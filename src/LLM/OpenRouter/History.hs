{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LLM.OpenRouter.History (
    messageToOpenRouterWith,
) where

import Api (Message, messageInfoRole, msgInfo, msgParts)
import Data.Aeson (Value (..), object)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import LLM.OpenRouter qualified as OpenRouter
import Message.Parts (extractUserText)

messageToOpenRouterWith :: (Text -> Text) -> Message -> [OpenRouter.ChatMessage]
messageToOpenRouterWith truncateOutput msg =
    case messageInfoRole (msgInfo msg) of
        "assistant" -> assistantMessages
        "system" -> maybe [] (pure . OpenRouter.messageWithContent OpenRouter.System) (userContentFromParts parts)
        "user" -> maybe [] (pure . OpenRouter.messageWithContent OpenRouter.User) (userContentFromParts parts)
        _otherRole -> maybe [] (pure . OpenRouter.messageWithContent OpenRouter.User) (userContentFromParts parts)
  where
    parts = msgParts msg
    content = extractUserText parts
    assistantMessages =
        let contentMaybe = if T.null content then Nothing else Just content
            toolCalls = extractToolCalls parts
            toolResults = extractToolResults truncateOutput parts
            assistantMsg =
                if null toolCalls
                    then case contentMaybe of
                        Just txt -> [OpenRouter.simpleMessage OpenRouter.Assistant txt]
                        Nothing -> []
                    else [OpenRouter.assistantMessageWithTools contentMaybe toolCalls]
         in assistantMsg ++ toolResults

userContentFromParts :: [Value] -> Maybe OpenRouter.Content
userContentFromParts parts =
    let contentParts = mapMaybe contentPartFromValue parts
        textOnly = [t | OpenRouter.ContentPartText t <- contentParts]
     in if null contentParts
            then Nothing
            else
                if length textOnly == length contentParts
                    then Just (OpenRouter.ContentText (T.intercalate "\n" textOnly))
                    else Just (OpenRouter.ContentParts contentParts)

contentPartFromValue :: Value -> Maybe OpenRouter.ContentPart
contentPartFromValue (Object obj) = case KM.lookup "type" obj of
    Just (String "text") -> do
        txt <- lookupText "text" obj
        pure (OpenRouter.ContentPartText txt)
    Just (String "file") -> do
        url <- lookupText "url" obj
        mime <- lookupText "mime" obj
        let filename = lookupText "filename" obj
        if "image/" `T.isPrefixOf` mime
            then Just (OpenRouter.ContentPartImageUrl url)
            else Just (OpenRouter.ContentPartFile url mime filename)
    _other -> Nothing
contentPartFromValue _other = Nothing

extractToolCalls :: [Value] -> [OpenRouter.ToolCall]
extractToolCalls = mapMaybe toolCallFromPart . toolPartObjects

extractToolResults :: (Text -> Text) -> [Value] -> [OpenRouter.ChatMessage]
extractToolResults truncateOutput = mapMaybe (toolResultFromPart truncateOutput) . toolPartObjects

toolPartObjects :: [Value] -> [KM.KeyMap Value]
toolPartObjects =
    mapMaybe $ \case
        Object obj -> case KM.lookup "type" obj of
            Just (String "tool") -> Just obj
            _otherType -> Nothing
        _other -> Nothing

toolCallFromPart :: KM.KeyMap Value -> Maybe OpenRouter.ToolCall
toolCallFromPart obj = do
    callId <- lookupTextNonEmpty "callID" obj
    toolName <- lookupTextNonEmpty "tool" obj
    let inputVal = fromMaybe (object []) (lookupStateValue "input" obj)
    let argsText = encodeValueText inputVal
    pure $
        OpenRouter.ToolCall
            { OpenRouter.tcId = callId
            , OpenRouter.tcType = "function"
            , OpenRouter.tcFunction =
                OpenRouter.ToolCallFunction
                    { OpenRouter.tcfName = toolName
                    , OpenRouter.tcfArguments = argsText
                    }
            }

toolResultFromPart :: (Text -> Text) -> KM.KeyMap Value -> Maybe OpenRouter.ChatMessage
toolResultFromPart truncateOutput obj = do
    callId <- lookupTextNonEmpty "callID" obj
    status <- lookupStateText "status" obj
    case status of
        "completed" -> do
            output <- lookupStateValue "output" obj
            pure $ OpenRouter.toolResultChatMessage callId (truncateOutput (valueText output))
        "error" -> do
            err <- lookupStateValue "error" obj
            pure $ OpenRouter.toolResultChatMessage callId (truncateOutput (valueText err))
        _otherStatus -> Nothing

lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText key obj = case KM.lookup (K.fromText key) obj of
    Just (String s) -> Just s
    _other -> Nothing

lookupTextNonEmpty :: Text -> KM.KeyMap Value -> Maybe Text
lookupTextNonEmpty key obj = do
    txt <- lookupText key obj
    if T.null txt then Nothing else Just txt

lookupStateObject :: KM.KeyMap Value -> Maybe (KM.KeyMap Value)
lookupStateObject obj = case KM.lookup "state" obj of
    Just (Object stateObj) -> Just stateObj
    _other -> Nothing

lookupStateValue :: Text -> KM.KeyMap Value -> Maybe Value
lookupStateValue key obj = do
    stateObj <- lookupStateObject obj
    KM.lookup (K.fromText key) stateObj

lookupStateText :: Text -> KM.KeyMap Value -> Maybe Text
lookupStateText key obj = do
    stateObj <- lookupStateObject obj
    lookupText key stateObj

encodeValueText :: Value -> Text
encodeValueText = TE.decodeUtf8 . BSL.toStrict . Aeson.encode

valueText :: Value -> Text
valueText (String s) = s
valueText v = encodeValueText v
