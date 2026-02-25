{-# LANGUAGE OverloadedStrings #-}

{- | Property-based tests for the Server.ErrorFormatters module.

These tests verify the core invariants of error formatting:

* Error bodies have the correct JSON structure
* Error names are consistent with error types
* JSON encoding is valid and decodable
* Messages are preserved exactly
* Edge cases (empty strings, unicode, special chars) are handled correctly
-}
module Property.ErrorFormattersProps where

import Control.Monad (when)
import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Server.ErrorFormatters
import Test.Helpers (genNonEmptyText, genText)
import Test.Tasty
import Test.Tasty.Hedgehog

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Helpers for JSON value matching
-- ═══════════════════════════════════════════════════════════════════════════

-- | Helper to annotate and fail for unexpected Value types
unexpectedValueType :: (MonadTest m) => String -> Value -> m ()
unexpectedValueType context val = do
    annotate $ context <> ": got " <> valueTypeName val
    failure

-- | Get a human-readable name for a Value type
valueTypeName :: Value -> String
valueTypeName (Object _) = "Object"
valueTypeName (Array _) = "Array"
valueTypeName (String _) = "String"
valueTypeName (Number _) = "Number"
valueTypeName (Bool _) = "Bool"
valueTypeName Null = "Null"

-- ═══════════════════════════════════════════════════════════════════════════
-- Generators
-- ═══════════════════════════════════════════════════════════════════════════

-- | Generate an error name (alphanumeric with common suffixes)
genErrorName :: Gen Text
genErrorName = do
    base <- Gen.element ["Validation", "Auth", "NotFound", "Internal", "Rate", "Timeout"]
    suffix <- Gen.element ["Error", "Exception", "Failure", ""]
    pure $ base <> suffix

-- | Generate a typical error message
genErrorMessage :: Gen Text
genErrorMessage =
    Gen.choice
        [ pure "Something went wrong"
        , pure "Resource not found"
        , pure "Invalid input"
        , pure "Unauthorized access"
        , pure "Rate limit exceeded"
        , genNonEmptyText
        ]

-- | Generate edge-case error messages
genEdgeCaseMessage :: Gen Text
genEdgeCaseMessage =
    Gen.element
        [ ""
        , " "
        , "   \t\n  "
        , "Error with \"quotes\""
        , "Error with 'quotes'"
        , "Error with <html> tags"
        , "Error: special chars !@#$%^&*()"
        , "Error with\nnewlines\nand\ttabs"
        , "Very " <> T.replicate 100 "long " <> "message"
        ]

-- | Generate unicode error messages
genUnicodeMessage :: Gen Text
genUnicodeMessage =
    Gen.element
        [ "Ошибка сервера" -- Russian
        , "服务器错误" -- Chinese
        , "サーバーエラー" -- Japanese
        , "Erro do servidor" -- Portuguese
        , "Error del servidor" -- Spanish
        , "Erreur de serveur" -- French
        , "Fehler 🔥 im Server" -- Emoji
        ]

-- ═══════════════════════════════════════════════════════════════════════════
-- Core Properties: Error Body Structure
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: errorBody produces valid JSON with correct structure
prop_errorBodyStructure :: Property
prop_errorBodyStructure = property $ do
    name <- forAll genErrorName
    msg <- forAll genErrorMessage
    let body = errorBody name msg
    case decode body :: Maybe Value of
        Nothing -> do
            annotate "Failed to decode JSON"
            failure
        Just (Object obj) -> do
            -- Must have "name" field
            case KM.lookup "name" obj of
                Just (String n) -> n === name
                Just other -> unexpectedValueType "'name' field" other
                Nothing -> do
                    annotate "Missing 'name' field"
                    failure
            -- Must have "data.message" field
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === msg
                        Just other -> unexpectedValueType "'data.message' field" other
                        Nothing -> do
                            annotate "Missing 'data.message' field"
                            failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> do
                    annotate "Missing 'data' field"
                    failure
        Just (Array _) -> do
            annotate "Expected JSON object at top level, got Array"
            failure
        Just (String _) -> do
            annotate "Expected JSON object at top level, got String"
            failure
        Just (Number _) -> do
            annotate "Expected JSON object at top level, got Number"
            failure
        Just (Bool _) -> do
            annotate "Expected JSON object at top level, got Bool"
            failure
        Just Null -> do
            annotate "Expected JSON object at top level, got Null"
            failure

-- | Property: errorBody preserves message exactly
prop_errorBodyPreservesMessage :: Property
prop_errorBodyPreservesMessage = property $ do
    msg <- forAll genText
    let body = errorBody "TestError" msg
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) ->
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === msg
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: errorBody preserves name exactly
prop_errorBodyPreservesName :: Property
prop_errorBodyPreservesName = property $ do
    name <- forAll genText
    let body = errorBody name "Test message"
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) ->
            case KM.lookup "name" obj of
                Just (String n) -> n === name
                Just other -> unexpectedValueType "'name' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- ═══════════════════════════════════════════════════════════════════════════
-- Core Properties: Specific Error Bodies
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: notFoundBody has correct name
prop_notFoundBodyName :: Property
prop_notFoundBodyName = property $ do
    let body = notFoundBody
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) ->
            case KM.lookup "name" obj of
                Just (String n) -> n === "NotFoundError"
                Just other -> unexpectedValueType "'name' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: notFoundBody has correct message
prop_notFoundBodyMessage :: Property
prop_notFoundBodyMessage = property $ do
    let body = notFoundBody
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) ->
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === "Not found"
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: badRequestBody has correct name and preserves message
prop_badRequestBody :: Property
prop_badRequestBody = property $ do
    msg <- forAll genErrorMessage
    let body = badRequestBody msg
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) -> do
            case KM.lookup "name" obj of
                Just (String n) -> n === "BadRequestError"
                Just other -> unexpectedValueType "'name' field" other
                Nothing -> failure
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === msg
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: unauthorizedBody has correct name and preserves message
prop_unauthorizedBody :: Property
prop_unauthorizedBody = property $ do
    msg <- forAll genErrorMessage
    let body = unauthorizedBody msg
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) -> do
            case KM.lookup "name" obj of
                Just (String n) -> n === "UnauthorizedError"
                Just other -> unexpectedValueType "'name' field" other
                Nothing -> failure
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === msg
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: forbiddenBody has correct name and preserves message
prop_forbiddenBody :: Property
prop_forbiddenBody = property $ do
    msg <- forAll genErrorMessage
    let body = forbiddenBody msg
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) -> do
            case KM.lookup "name" obj of
                Just (String n) -> n === "ForbiddenError"
                Just other -> unexpectedValueType "'name' field" other
                Nothing -> failure
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === msg
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: internalErrorBody has correct name and preserves message
prop_internalErrorBody :: Property
prop_internalErrorBody = property $ do
    msg <- forAll genErrorMessage
    let body = internalErrorBody msg
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) -> do
            case KM.lookup "name" obj of
                Just (String n) -> n === "InternalServerError"
                Just other -> unexpectedValueType "'name' field" other
                Nothing -> failure
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === msg
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- ═══════════════════════════════════════════════════════════════════════════
-- Core Properties: errorResponse helper
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: errorResponse produces JSON with "error" field
prop_errorResponseStructure :: Property
prop_errorResponseStructure = property $ do
    msg <- forAll genErrorMessage
    let resp = errorResponse msg
    case resp of
        Object obj ->
            case KM.lookup "error" obj of
                Just (String m) -> m === msg
                Just other -> unexpectedValueType "'error' field" other
                Nothing -> failure
        other -> unexpectedValueType "top level" other

-- | Property: errorResponse preserves message exactly
prop_errorResponsePreservesMessage :: Property
prop_errorResponsePreservesMessage = property $ do
    msg <- forAll genText
    let resp = errorResponse msg
    case resp of
        Object obj ->
            case KM.lookup "error" obj of
                Just (String m) -> m === msg
                Just other -> unexpectedValueType "'error' field" other
                Nothing -> failure
        other -> unexpectedValueType "top level" other

-- ═══════════════════════════════════════════════════════════════════════════
-- Edge Case Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: Empty message is handled correctly
prop_emptyMessage :: Property
prop_emptyMessage = property $ do
    let body = errorBody "TestError" ""
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) ->
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === ""
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: Empty error name is handled correctly
prop_emptyName :: Property
prop_emptyName = property $ do
    let body = errorBody "" "Some message"
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) ->
            case KM.lookup "name" obj of
                Just (String n) -> n === ""
                Just other -> unexpectedValueType "'name' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: Edge case messages are preserved
prop_edgeCaseMessages :: Property
prop_edgeCaseMessages = property $ do
    msg <- forAll genEdgeCaseMessage
    let body = errorBody "TestError" msg
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) ->
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === msg
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: Unicode messages are preserved through JSON encoding
prop_unicodeMessages :: Property
prop_unicodeMessages = property $ do
    msg <- forAll genUnicodeMessage
    let body = errorBody "UnicodeError" msg
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) ->
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === msg
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- | Property: Very long messages are handled correctly
prop_longMessages :: Property
prop_longMessages = property $ do
    msg <- forAll $ Gen.text (Range.linear 1000 10000) Gen.alphaNum
    let body = errorBody "LongError" msg
    case decode body :: Maybe Value of
        Nothing -> failure
        Just (Object obj) ->
            case KM.lookup "data" obj of
                Just (Object dataObj) ->
                    case KM.lookup "message" dataObj of
                        Just (String m) -> m === msg
                        Just other -> unexpectedValueType "'message' field" other
                        Nothing -> failure
                Just other -> unexpectedValueType "'data' field" other
                Nothing -> failure
        Just other -> unexpectedValueType "top level" other

-- ═══════════════════════════════════════════════════════════════════════════
-- Consistency Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: All specific error bodies are consistent with errorBody
prop_specificBodiesConsistent :: Property
prop_specificBodiesConsistent = property $ do
    -- notFoundBody should equal errorBody "NotFoundError" "Not found"
    notFoundBody === errorBody "NotFoundError" "Not found"

-- | Property: JSON output is deterministic (same input -> same output)
prop_deterministic :: Property
prop_deterministic = property $ do
    name <- forAll genErrorName
    msg <- forAll genErrorMessage
    let body1 = errorBody name msg
        body2 = errorBody name msg
    body1 === body2

-- | Property: Different inputs produce different outputs
prop_differentInputsDifferentOutputs :: Property
prop_differentInputsDifferentOutputs = property $ do
    name1 <- forAll genErrorName
    name2 <- forAll genErrorName
    msg <- forAll genErrorMessage
    -- Different names should produce different bodies
    when (name1 /= name2) $ do
        let body1 = errorBody name1 msg
            body2 = errorBody name2 msg
        assert $ body1 /= body2

-- ═══════════════════════════════════════════════════════════════════════════
-- JSON Validity Properties
-- ═══════════════════════════════════════════════════════════════════════════

-- | Property: All error bodies produce valid JSON
prop_validJson :: Property
prop_validJson = property $ do
    name <- forAll genText
    msg <- forAll genText
    let body = errorBody name msg
    -- Must be parseable as JSON
    case decode body :: Maybe Value of
        Nothing -> failure
        Just _ -> success

-- | Property: errorResponse produces valid JSON (round-trip)
prop_errorResponseRoundTrip :: Property
prop_errorResponseRoundTrip = property $ do
    msg <- forAll genText
    let resp = errorResponse msg
    case decode (encode resp) :: Maybe Value of
        Nothing -> failure
        Just decoded -> decoded === resp

-- ═══════════════════════════════════════════════════════════════════════════
-- Test Tree
-- ═══════════════════════════════════════════════════════════════════════════

tests :: TestTree
tests =
    testGroup
        "ErrorFormatters Property Tests"
        [ testGroup
            "Error Body Structure"
            [ testProperty "errorBody produces valid JSON structure" prop_errorBodyStructure
            , testProperty "errorBody preserves message" prop_errorBodyPreservesMessage
            , testProperty "errorBody preserves name" prop_errorBodyPreservesName
            ]
        , testGroup
            "Specific Error Bodies"
            [ testProperty "notFoundBody has correct name" prop_notFoundBodyName
            , testProperty "notFoundBody has correct message" prop_notFoundBodyMessage
            , testProperty "badRequestBody correct structure" prop_badRequestBody
            , testProperty "unauthorizedBody correct structure" prop_unauthorizedBody
            , testProperty "forbiddenBody correct structure" prop_forbiddenBody
            , testProperty "internalErrorBody correct structure" prop_internalErrorBody
            ]
        , testGroup
            "errorResponse Helper"
            [ testProperty "errorResponse has correct structure" prop_errorResponseStructure
            , testProperty "errorResponse preserves message" prop_errorResponsePreservesMessage
            , testProperty "errorResponse round-trip" prop_errorResponseRoundTrip
            ]
        , testGroup
            "Edge Cases"
            [ testProperty "empty message handled" prop_emptyMessage
            , testProperty "empty name handled" prop_emptyName
            , testProperty "edge case messages preserved" prop_edgeCaseMessages
            , testProperty "unicode messages preserved" prop_unicodeMessages
            , testProperty "very long messages handled" prop_longMessages
            ]
        , testGroup
            "Consistency"
            [ testProperty "specific bodies consistent with errorBody" prop_specificBodiesConsistent
            , testProperty "output is deterministic" prop_deterministic
            , testProperty "different inputs produce different outputs" prop_differentInputsDifferentOutputs
            ]
        , testGroup
            "JSON Validity"
            [ testProperty "all bodies produce valid JSON" prop_validJson
            ]
        ]
