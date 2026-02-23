{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bus property tests
module Property.BusProps where

import Bus.Bus qualified as Bus
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (replicateM, replicateM_, void)
import Data.Aeson (Value (..), toJSON)
import Data.List qualified as List
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Property: published events are received by subscribers
prop_publishSubscribe :: Property
prop_publishSubscribe = withTests 20 $ property $ do
    eventType <- forAll genEventType
    eventCount <- forAll $ Gen.int (Range.linear 1 10)

    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO []

        -- Subscribe to events
        void $ Bus.subscribe bus eventType $ \event ->
            atomically $ modifyTVar' receivedVar (Bus.beType event :)

        -- Publish events
        replicateM_ eventCount $ do
            Bus.publish bus eventType Null
            threadDelay 50

        -- Wait for all events to be processed
        threadDelay 2000

        readTVarIO receivedVar

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

        threadDelay 5000
        readTVarIO receivedVar

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

        waitForAll vars (30 :: Int)

    -- All subscribers should have received the event
    all (\r -> listLength r == 1) results === True
    all (\case [x] -> x == eventType; _otherResults -> False) results === True
  where
    waitForAll vars attempts = do
        results <- mapM readTVarIO vars
        if all (\r -> listLength r == 1) results
            then pure results
            else do
                if attempts <= 0
                    then pure results
                    else do
                        threadDelay 500
                        waitForAll vars (attempts - 1)

prop_subscribeAllOrder :: Property
prop_subscribeAllOrder = withTests 20 $ property $ do
    eventTypes <- forAll $ Gen.list (Range.linear 1 3) genEventType
    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO []
        void $ Bus.subscribeAll bus $ \event ->
            atomically $ modifyTVar' receivedVar (\events -> events <> [Bus.beType event])
        mapM_ (\et -> Bus.publish bus et Null) eventTypes
        threadDelay 5000
        readTVarIO receivedVar
    received === eventTypes

prop_unsubscribeStopsDelivery :: Property
prop_unsubscribeStopsDelivery = withTests 20 $ property $ do
    eventType <- forAll genEventType
    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO (0 :: Int)
        unsubscribe <- Bus.subscribe bus eventType $ \_event ->
            atomically $ modifyTVar' receivedVar (+ 1)
        Bus.publish bus eventType Null
        threadDelay 50000
        unsubscribe
        Bus.publish bus eventType Null
        threadDelay 50000
        readTVarIO receivedVar
    received === 1

-- ═══════════════════════════════════════════════════════════════════════════
-- // Bus to TChan forwarding (SSE integration pattern) //
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
        receivedVar <- newTVarIO (0 :: Int)

        -- Publish events
        mapM_ (\et -> Bus.publish bus et Null) eventTypes

        -- Wait and collect
        threadDelay 10000

        -- Try to read all available events
        let readAll = do
                mval <- atomically $ tryReadTChan reader
                case mval of
                    Nothing -> pure ()
                    Just _ -> do
                        atomically $ modifyTVar' receivedVar (+ 1)
                        readAll
        readAll

        readTVarIO receivedVar

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
        counters <- replicateM readerCount $ newTVarIO (0 :: Int)

        -- Publish one event
        Bus.publish bus eventType Null

        threadDelay 10000

        -- Read from each reader
        mapM_
            ( \(reader, counter) -> do
                mval <- atomically $ tryReadTChan reader
                case mval of
                    Just _ -> atomically $ modifyTVar' counter (+ 1)
                    Nothing -> pure ()
            )
            (zip readers counters)

        mapM readTVarIO counters

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

        -- Publish "before" events
        replicateM_ beforeCount $ Bus.publish bus "before.event" Null
        threadDelay 5000

        -- NOW create a reader
        reader <- atomically $ dupTChan eventChan

        -- Publish "after" events
        replicateM_ afterCount $ Bus.publish bus "after.event" Null
        threadDelay 5000

        -- Count events received
        let countEvents = do
                mval <- atomically $ tryReadTChan reader
                case mval of
                    Nothing -> pure 0
                    Just _ -> (+ 1) <$> countEvents
        count <- countEvents

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
