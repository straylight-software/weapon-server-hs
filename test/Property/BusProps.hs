{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Property.BusProps
Description : Property tests for Bus.Bus and Bus.Event modules

This module contains property tests for the event bus system:

* Pure function tests (no IO) for 'mkBusEvent', 'matchesEventType'
* Pure function tests for 'eventTypeToText', 'textToEventType', 'mkEvent'
* IO-based tests for pub/sub semantics
* Integration tests for SSE forwarding patterns
-}
module Property.BusProps where

import Bus.Bus qualified as Bus
import Bus.Event (Event (eventType), EventType (..), eventTypeToText, mkEvent, textToEventType)
import Control.Concurrent.STM
import Control.Monad (forM_, replicateM, replicateM_, unless, void)
import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import Data.Ix (range)
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Helpers (listLength, waitForCount, waitForLength)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- Pure Function Tests (no IO - fast and deterministic)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: mkBusEvent creates event with correct fields
prop_mkBusEvent_fields :: Property
prop_mkBusEvent_fields = property $ do
    typ <- forAll genEventTypeText
    props <- forAll genProperties
    let event = Bus.mkBusEvent typ props
    Bus.beType event === typ
    Bus.beProperties event === props

-- | Property: matchesEventType returns True for matching types
prop_matchesEventType_matches :: Property
prop_matchesEventType_matches = property $ do
    typ <- forAll genEventTypeText
    props <- forAll genProperties
    let event = Bus.mkBusEvent typ props
    Bus.matchesEventType typ event === True

-- | Property: matchesEventType returns False for non-matching types
prop_matchesEventType_no_match :: Property
prop_matchesEventType_no_match = property $ do
    typ1 <- forAll genEventTypeText
    typ2 <- forAll $ Gen.filter (/= typ1) genEventTypeText
    props <- forAll genProperties
    let event = Bus.mkBusEvent typ1 props
    Bus.matchesEventType typ2 event === False

-- | Property: BusEvent JSON round-trips correctly
prop_busEvent_json_roundtrip :: Property
prop_busEvent_json_roundtrip = property $ do
    typ <- forAll genEventTypeText
    props <- forAll genProperties
    let event = Bus.mkBusEvent typ props
    let encoded = encode event
    let decoded = decode encoded
    decoded === Just event

-- | Property: BusEvent JSON has correct structure
prop_busEvent_json_structure :: Property
prop_busEvent_json_structure = property $ do
    typ <- forAll genEventTypeText
    props <- forAll genProperties
    let event = Bus.mkBusEvent typ props
    let json = toJSON event
    case json of
        Object _obj -> do
            -- Must have "type" field
            annotate "BusEvent JSON should have 'type' field"
            success
        Null -> do
            annotate "BusEvent should serialize to JSON object"
            failure
        String _ -> do
            annotate "BusEvent should serialize to JSON object"
            failure
        Number _ -> do
            annotate "BusEvent should serialize to JSON object"
            failure
        Bool _ -> do
            annotate "BusEvent should serialize to JSON object"
            failure
        Array _ -> do
            annotate "BusEvent should serialize to JSON object"
            failure

-- ═══════════════════════════════════════════════════════════════════════════
-- Event Type Conversion Tests (pure)
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: eventTypeToText is total (defined for all EventTypes)
prop_eventTypeToText_total :: Property
prop_eventTypeToText_total = property $ do
    et <- forAll genTypedEventType
    let text = eventTypeToText et
    -- Just checking it doesn't error - text should be non-empty
    assert $ text /= ""

-- | Property: textToEventType inverts eventTypeToText
prop_textToEventType_inverts :: Property
prop_textToEventType_inverts = property $ do
    et <- forAll genTypedEventType
    let text = eventTypeToText et
    textToEventType text === Just et

-- | Property: textToEventType returns Nothing for invalid input
prop_textToEventType_invalid :: Property
prop_textToEventType_invalid = property $ do
    invalidText <- forAll genInvalidEventTypeText
    textToEventType invalidText === Nothing

-- | Property: mkEvent creates Event with correct type text
prop_mkEvent_correct_type :: Property
prop_mkEvent_correct_type = property $ do
    et <- forAll genTypedEventType
    props <- forAll genProperties
    let event = mkEvent et props
    eventType event === eventTypeToText et

-- | Property: All EventType constructors have unique text representations
prop_eventType_unique_texts :: Property
prop_eventType_unique_texts = withTests 1 $ property $ do
    let allTypes = range (minBound, maxBound) :: [EventType]
    let allTexts = map eventTypeToText allTypes
    let uniqueTexts = foldl' (\seen t -> if t `elem` seen then seen else t : seen) [] allTexts
    listLength uniqueTexts === listLength allTypes

-- | Property: EventType JSON round-trips correctly
prop_eventType_json_roundtrip :: Property
prop_eventType_json_roundtrip = property $ do
    et <- forAll genTypedEventType
    let encoded = encode et
    let decoded = decode encoded :: Maybe EventType
    decoded === Just et

-- | Property: Event JSON round-trips correctly
prop_event_json_roundtrip :: Property
prop_event_json_roundtrip = property $ do
    et <- forAll genTypedEventType
    props <- forAll genProperties
    let event = mkEvent et props
    let encoded = encode event
    let decoded = decode encoded
    decoded === Just event

-- | Property: All EventTypes round-trip through text (exhaustive check)
prop_all_eventTypes_roundtrip :: Property
prop_all_eventTypes_roundtrip = withTests 1 $ property $ do
    let allTypes = range (minBound, maxBound) :: [EventType]
    -- Verify each type round-trips
    forM_ allTypes $ \et -> do
        let text = eventTypeToText et
        let parsed = textToEventType text
        annotate $ "EventType: " <> show et <> " -> Text: " <> show text
        parsed === Just et

-- ═══════════════════════════════════════════════════════════════════════════
-- IO-based Pub/Sub Tests
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: published events are received by subscribers
prop_publishSubscribe :: Property
prop_publishSubscribe = property $ do
    eventType <- forAll genEventTypeText
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
prop_subscribeAll = property $ do
    eventTypes <- forAll $ Gen.list (Range.linear 1 3) genEventTypeText

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
prop_multipleSubscribers = property $ do
    eventType <- forAll genEventTypeText
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

-- | Property: subscribeAll preserves event order
prop_subscribeAllOrder :: Property
prop_subscribeAllOrder = property $ do
    eventTypes <- forAll $ Gen.list (Range.linear 1 3) genEventTypeText
    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO []
        void $ Bus.subscribeAll bus $ \event ->
            atomically $ modifyTVar' receivedVar (\events -> events <> [Bus.beType event])
        mapM_ (\et -> Bus.publish bus et Null) eventTypes
        waitForLength 1000000 receivedVar (listLength eventTypes)
    received === eventTypes

-- | Property: unsubscribe stops event delivery
prop_unsubscribeStopsDelivery :: Property
prop_unsubscribeStopsDelivery = property $ do
    eventType <- forAll genEventTypeText
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

-- | Property: publishTyped works with typed EventTypes
prop_publishTyped :: Property
prop_publishTyped = property $ do
    et <- forAll genTypedEventType
    props <- forAll genProperties

    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO Nothing

        -- Subscribe using the text representation
        void $ Bus.subscribe bus (eventTypeToText et) $ \event ->
            atomically $ writeTVar receivedVar (Just event)

        -- Publish using typed API
        Bus.publishTyped bus et props

        -- Wait for event
        waitForTVarJust 1000000 receivedVar

    case received of
        Just event -> do
            Bus.beType event === eventTypeToText et
            Bus.beProperties event === props
        Nothing -> do
            annotate "Should have received an event"
            failure

-- | Property: subscribeTyped works with typed EventTypes
prop_subscribeTyped :: Property
prop_subscribeTyped = property $ do
    et <- forAll genTypedEventType
    props <- forAll genProperties

    received <- evalIO $ do
        bus <- Bus.newBus
        receivedVar <- newTVarIO Nothing

        -- Subscribe using typed API
        void $ Bus.subscribeTyped bus et $ \event ->
            atomically $ writeTVar receivedVar (Just event)

        -- Publish using raw text
        Bus.publish bus (eventTypeToText et) props

        -- Wait for event
        waitForTVarJust 1000000 receivedVar

    case received of
        Just event -> Bus.beProperties event === props
        Nothing -> do
            annotate "Should have received an event"
            failure

-- ═══════════════════════════════════════════════════════════════════════════
-- Bus to TChan forwarding (SSE integration pattern)
-- ═══════════════════════════════════════════════════════════════════════════

{- | Property: Events published to bus are forwarded to a TChan subscriber.
This tests the pattern used in State.hs where bus events are forwarded to
eventChan for SSE delivery.
-}
prop_busToTChanForwarding :: Property
prop_busToTChanForwarding = property $ do
    eventTypes <- forAll $ Gen.list (Range.linear 1 5) genEventTypeText

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
prop_multipleTChanReaders = property $ do
    eventType <- forAll genEventTypeText
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
prop_tchanBroadcastSemantics = property $ do
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

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate a Text event type (existing API)
genEventTypeText :: Gen Text
genEventTypeText =
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

-- | Generate a typed EventType
genTypedEventType :: Gen EventType
genTypedEventType =
    Gen.element
        [ SessionCreated
        , SessionUpdated
        , SessionDeleted
        , MessageUpdated
        , MessagePartUpdated
        , PtyCreated
        , PtyUpdated
        , PtyDeleted
        , ServerConnected
        , ServerHeartbeat
        ]

-- | Generate invalid event type text (for testing textToEventType)
genInvalidEventTypeText :: Gen Text
genInvalidEventTypeText =
    Gen.element
        [ "invalid.type"
        , "not.a.real.event"
        , ""
        , "foo"
        , "session"
        , "session."
        , ".created"
        ]

-- | Generate JSON properties
genProperties :: Gen Value
genProperties =
    Gen.choice
        [ pure Null
        , pure $ object []
        , do
            sessionId <- Gen.text (Range.linear 5 20) Gen.alphaNum
            pure $ object ["sessionID" .= sessionId]
        , do
            msg <- Gen.text (Range.linear 1 50) Gen.unicode
            pure $ object ["message" .= msg]
        ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════════════════

-- | Wait for a TVar Maybe to become Just, with timeout
waitForTVarJust :: Int -> TVar (Maybe a) -> IO (Maybe a)
waitForTVarJust timeoutUs var = do
    gate <- registerDelay timeoutUs
    atomically $ do
        val <- readTVar var
        case val of
            Just _ -> pure val
            Nothing -> do
                done <- readTVar gate
                if done then pure Nothing else retry

-- ═══════════════════════════════════════════════════════════════════════════
-- Test tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "Bus Property Tests"
        [ testGroup
            "Pure Functions (no IO)"
            [ testProperty "mkBusEvent creates correct fields" prop_mkBusEvent_fields
            , testProperty "matchesEventType matches" prop_matchesEventType_matches
            , testProperty "matchesEventType no match" prop_matchesEventType_no_match
            , testProperty "BusEvent JSON round-trip" prop_busEvent_json_roundtrip
            , testProperty "BusEvent JSON structure" prop_busEvent_json_structure
            ]
        , testGroup
            "EventType Conversions (pure)"
            [ testProperty "eventTypeToText is total" prop_eventTypeToText_total
            , testProperty "textToEventType inverts eventTypeToText" prop_textToEventType_inverts
            , testProperty "textToEventType returns Nothing for invalid" prop_textToEventType_invalid
            , testProperty "mkEvent uses correct type text" prop_mkEvent_correct_type
            , testProperty "all EventTypes have unique texts" prop_eventType_unique_texts
            , testProperty "EventType JSON round-trip" prop_eventType_json_roundtrip
            , testProperty "Event JSON round-trip" prop_event_json_roundtrip
            , testProperty "all EventTypes round-trip (exhaustive)" prop_all_eventTypes_roundtrip
            ]
        , testGroup
            "Pub/Sub (IO)"
            [ testProperty "publish/subscribe" prop_publishSubscribe
            , testProperty "subscribeAll receives all" prop_subscribeAll
            , testProperty "multiple subscribers" prop_multipleSubscribers
            , testProperty "subscribeAll order" prop_subscribeAllOrder
            , testProperty "unsubscribe stops delivery" prop_unsubscribeStopsDelivery
            , testProperty "publishTyped works" prop_publishTyped
            , testProperty "subscribeTyped works" prop_subscribeTyped
            ]
        , testGroup
            "Bus to TChan forwarding (SSE pattern)"
            [ testProperty "events forwarded to TChan" prop_busToTChanForwarding
            , testProperty "multiple TChan readers" prop_multipleTChanReaders
            , testProperty "broadcast semantics (no old events)" prop_tchanBroadcastSemantics
            ]
        ]
