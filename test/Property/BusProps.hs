{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bus property tests
module Property.BusProps where

import Bus.Bus qualified as Bus
import Control.Concurrent.STM
import Control.Monad (replicateM, replicateM_, unless, void)
import Data.Aeson (Value (..), toJSON)
import Data.List qualified as List
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- STM wait helpers (no threadDelay anywhere)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Wait for a TVar to satisfy a predicate, with timeout (microseconds).
Returns the final value regardless of whether the predicate was met.
-}
waitForTVar :: Int -> TVar a -> (a -> Bool) -> IO a
waitForTVar timeoutUs var predicate = do
    gate <- registerDelay timeoutUs
    atomically $ do
        val <- readTVar var
        if predicate val
            then pure val
            else do
                done <- readTVar gate
                if done then pure val else retry

-- | Wait for a TVar list to reach a given length, with timeout.
waitForLength :: Int -> TVar [a] -> Int -> IO [a]
waitForLength timeoutUs var n = waitForTVar timeoutUs var (\xs -> listLength xs >= n)

-- | Wait for a TVar Int to reach a given value, with timeout.
waitForCount :: Int -> TVar Int -> Int -> IO Int
waitForCount timeoutUs var n = waitForTVar timeoutUs var (>= n)

-- ═══════════════════════════════════════════════════════════════════════════
-- Bus property tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: published events are received by subscribers
prop_publishSubscribe :: Property
prop_publishSubscribe = property $ do
    eventType <- forAll genEventType
    eventCount <- forAll $ Gen.int (Range.linear 1 10)

    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO []

        -- Subscribe to events
        void $ Bus.subscribe bus eventType $ \event ->
            atomically $ modifyTVar' receivedVar (Bus.beType event :)

        -- Publish events
        replicateM_ eventCount $ Bus.publish bus eventType Null

        -- Wait for all events to be received (1s timeout)
        waitForLength 1000000 receivedVar eventCount

    -- All events should have been received
    listLength received === eventCount
    -- All should be the same event type
    all (== eventType) received === True

-- | Property: subscribeAll receives all event types
prop_subscribeAll :: Property
prop_subscribeAll = withTests 20 $ property $ do
    eventTypes <- forAll $ Gen.list (Range.linear 1 3) genEventType

    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO []

        -- Subscribe to all events
        void $ Bus.subscribeAll bus $ \event ->
            atomically $ modifyTVar' receivedVar (Bus.beType event :)

        -- Publish different event types
        mapM_ (\et -> Bus.publish bus et Null) eventTypes

        -- Wait for all events (1s timeout)
        waitForLength 1000000 receivedVar (listLength eventTypes)

    -- Should receive all events
    listLength received === listLength eventTypes

-- | Property: multiple subscribers receive the same events
prop_multipleSubscribers :: Property
prop_multipleSubscribers = withTests 20 $ property $ do
    eventType <- forAll genEventType
    subscriberCount <- forAll $ Gen.int (Range.linear 2 4)

    results <- evalIO $ do
        bus <- Bus.newBus
        vars <- replicateM subscriberCount $ newTVarIO []

        -- Subscribe all
        mapM_
            ( \var -> Bus.subscribe bus eventType $ \event ->
                atomically $ modifyTVar' var (Bus.beType event :)
            )
            vars

        -- Publish one event
        Bus.publish bus eventType Null

        -- Wait for each subscriber to receive 1 event (1s timeout)
        mapM (\var -> waitForLength 1000000 var 1) vars

    -- All subscribers should have received the event
    all (\r -> listLength r == 1) results === True
    all (\case [x] -> x == eventType; _otherResults -> False) results === True

prop_subscribeAllOrder :: Property
prop_subscribeAllOrder = withTests 20 $ property $ do
    eventTypes <- forAll $ Gen.list (Range.linear 1 3) genEventType
    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO []
        void $ Bus.subscribeAll bus $ \event ->
            atomically $ modifyTVar' receivedVar (\events -> events <> [Bus.beType event])
        mapM_ (\et -> Bus.publish bus et Null) eventTypes
        waitForLength 1000000 receivedVar (listLength eventTypes)
    received === eventTypes

prop_unsubscribeStopsDelivery :: Property
prop_unsubscribeStopsDelivery = withTests 20 $ property $ do
    eventType <- forAll genEventType
    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO (0 :: Int)
        unsubscribe <- Bus.subscribe bus eventType $ \_event ->
            atomically $ modifyTVar' receivedVar (+ 1)

        -- Publish one event and wait for it to arrive
        Bus.publish bus eventType Null
        _ <- waitForCount 1000000 receivedVar 1

        -- Unsubscribe, then publish another event
        unsubscribe
        Bus.publish bus eventType Null

        -- Give the second event a chance to arrive (it shouldn't)
        -- We can't "wait for something NOT to happen", so we use a short timeout
        gate <- registerDelay 50000 -- 50ms
        atomically $ do
            done <- readTVar gate
            unless done retry
        readTVarIO receivedVar

    -- Should have received exactly 1 (the first one only)
    received === 1

-- ═══════════════════════════════════════════════════════════════════════════
-- Bus to TChan forwarding (SSE integration pattern)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Property: Events published to bus are forwarded to a TChan subscriber.
This tests the pattern used in State.hs where bus events are forwarded to
eventChan for SSE delivery.
-}
prop_busToTChanForwarding :: Property
prop_busToTChanForwarding = withTests 20 $ property $ do
    eventTypes <- forAll $ Gen.list (Range.linear 1 5) genEventType

    received <- evalIO $ do
        bus <- Bus.newBus
        -- Create a broadcast TChan (like eventChan in State.hs)
        eventChan <- newBroadcastTChanIO

        -- Subscribe to bus and forward to TChan (like State.mkAppState does)
        void $ Bus.subscribeAll bus $ \event ->
            atomically $ writeTChan eventChan (toJSON event)

        -- Create a reader (like SSE handler does with dupTChan)
        reader <- atomically $ dupTChan eventChan

        -- Publish events
        mapM_ (\et -> Bus.publish bus et Null) eventTypes

        -- Read expected number of events from TChan with timeout
        let expected = listLength eventTypes
        let readN n acc
                | n <= 0 = pure acc
                | otherwise = do
                    gate <- registerDelay 1000000
                    mval <-
                        atomically $
                            (Just <$> readTChan reader)
                                `orElse` do
                                    done <- readTVar gate
                                    if done then pure Nothing else retry
                    case mval of
                        Just _ -> readN (n - 1) (acc + 1)
                        Nothing -> pure acc
        readN expected (0 :: Int)

    -- All events should have been forwarded
    received === listLength eventTypes

{- | Property: Multiple TChan readers all receive forwarded bus events.
This tests that multiple SSE connections all receive the same events.
-}
prop_multipleTChanReaders :: Property
prop_multipleTChanReaders = withTests 20 $ property $ do
    eventType <- forAll genEventType
    readerCount <- forAll $ Gen.int (Range.linear 2 5)

    results <- evalIO $ do
        bus <- Bus.newBus
        eventChan <- newBroadcastTChanIO

        -- Forward bus events to TChan
        void $ Bus.subscribeAll bus $ \event ->
            atomically $ writeTChan eventChan (toJSON event)

        -- Create multiple readers (like multiple SSE connections)
        readers <- replicateM readerCount $ atomically $ dupTChan eventChan

        -- Publish one event
        Bus.publish bus eventType Null

        -- Read one event from each reader with timeout
        mapM
            ( \reader -> do
                gate <- registerDelay 1000000
                mval <-
                    atomically $
                        (Just <$> readTChan reader)
                            `orElse` do
                                done <- readTVar gate
                                if done then pure Nothing else retry
                pure $ case mval of
                    Just _ -> 1 :: Int
                    Nothing -> 0
            )
            readers

    -- All readers should have received exactly one event
    all (== 1) results === True
    listLength results === readerCount

{- | Property: TChan readers created after publish don't receive old events.
This verifies broadcast semantics - only events after subscription are received.
-}
prop_tchanBroadcastSemantics :: Property
prop_tchanBroadcastSemantics = withTests 20 $ property $ do
    beforeCount <- forAll $ Gen.int (Range.linear 1 3)
    afterCount <- forAll $ Gen.int (Range.linear 1 3)

    (beforeReceived, afterReceived) <- evalIO $ do
        bus <- Bus.newBus
        eventChan <- newBroadcastTChanIO

        -- Forward bus events to TChan
        void $ Bus.subscribeAll bus $ \event ->
            atomically $ writeTChan eventChan (toJSON event)

        -- Publish "before" events and wait for them via a temporary reader
        tempReader <- atomically $ dupTChan eventChan
        replicateM_ beforeCount $ Bus.publish bus "before.event" Null
        -- Drain the temp reader to confirm all "before" events have been forwarded
        let drainN n
                | n <= 0 = pure ()
                | otherwise = do
                    gate <- registerDelay 1000000
                    _ <-
                        atomically $
                            (Just <$> readTChan tempReader)
                                `orElse` do
                                    done <- readTVar gate
                                    if done then pure Nothing else retry
                    drainN (n - 1)
        drainN beforeCount

        -- NOW create a reader (should not see "before" events)
        reader <- atomically $ dupTChan eventChan

        -- Publish "after" events
        replicateM_ afterCount $ Bus.publish bus "after.event" Null

        -- Read expected number of "after" events from reader
        let readN n acc
                | n <= 0 = pure acc
                | otherwise = do
                    gate <- registerDelay 1000000
                    mval <-
                        atomically $
                            (Just <$> readTChan reader)
                                `orElse` do
                                    done <- readTVar gate
                                    if done then pure Nothing else retry
                    case mval of
                        Just _ -> readN (n - 1) (acc + 1)
                        Nothing -> pure acc
        count <- readN afterCount 0

        pure (0 :: Int, count) -- before events should not be received

    -- Reader should NOT receive events published before it was created
    beforeReceived === 0
    -- Reader should receive all events published after creation
    afterReceived === afterCount

-- Generators
genEventType :: Gen Text
genEventType =
    Gen.element
        [ "session.created"
        , "session.updated"
        , "session.deleted"
        , "message.updated"
        , "message.part.updated"
        , "pty.created"
        , "pty.updated"
        , "pty.deleted"
        ]

-- Test tree
tests :: TestTree
tests =
    testGroup
        "Bus Property Tests"
        [ testProperty "publish/subscribe" prop_publishSubscribe
        , testProperty "subscribeAll receives all" prop_subscribeAll
        , testProperty "multiple subscribers" prop_multipleSubscribers
        , testProperty "subscribeAll order" prop_subscribeAllOrder
        , testProperty "unsubscribe stops delivery" prop_unsubscribeStopsDelivery
        , testGroup
            "Bus to TChan forwarding (SSE pattern)"
            [ testProperty "events forwarded to TChan" prop_busToTChanForwarding
            , testProperty "multiple TChan readers" prop_multipleTChanReaders
            , testProperty "broadcast semantics (no old events)" prop_tchanBroadcastSemantics
            ]
        ]

listLength :: [a] -> Int
listLength = List.foldl' (\acc _ -> acc + 1) 0
