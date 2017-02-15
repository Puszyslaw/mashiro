{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import ClassyPrelude

import Control.Concurrent (forkIO)
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.WebSockets
import Wuss
import Text.Regex.TDFA ((=~))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Text.Show.Pretty (pPrint, ppShow)

import Messages

botToken :: Text
botToken = "<token goes hier>"

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    runSecureClient "gateway.discord.gg" 443 "/?encoding=json&v=6" (sessionStart botToken manager)

receiveMessage :: Connection -> IO ServerMessage
receiveMessage conn =
    (decode <$> receiveData conn) >>= \case
        Just serverMessage -> return serverMessage
        Nothing            -> fail "Invalid payload received"

sendMessage :: Connection -> ClientMessage -> IO ()
sendMessage conn message = sendTextData conn (encode message)

createMessage :: ChannelId -> Text -> AuthToken -> Manager -> IO ()
createMessage channelId content token manager = do
    let initReq = parseRequest_ . unpack $ "https://discordapp.com/api/channels/" ++ channelId ++ "/messages"
    let req = urlEncodedBody [("content", encodeUtf8 content)] initReq
            { method         = "POST"
            , requestHeaders = [("Authorization", "Bot " ++ encodeUtf8 token)]
            }

    void $ httpLbs req manager

sessionStart :: AuthToken -> Manager -> ClientApp ()
sessionStart token manager conn = do
    -- Get the heartbeat interval from initial Hello message
    HelloMessage
        { _heartbeatInterval = hbInt
        } <- receiveMessage conn

    -- Send the authentication payload
    sendMessage conn ConnectMessage
            { _token    = token
            , _os       = "Linux"
            , _browser  = "Mashiro"
            , _device   = "Mashiro"
            , _compress = False
            }

    -- Receive the READY event dispatch and fork a heartbeat thread
    DispatchMessage seqNum
        ReadyEvent
            { _sessionId = sessionId
            , _user      = user
            , _guildIds  = guildIds
            } <- receiveMessage conn

    pPrint user
    pPrint guildIds

    tvSeqNum <- newTVarIO seqNum

    forkIO . forever $ do
        threadDelay $ hbInt * 1000
        seqNum <- atomically $ readTVar tvSeqNum
        sendMessage conn $ HeartbeatMessage seqNum

    -- Run a worker with initial state
    let state = DiscordState
            { _token       = token
            , _sessionId   = sessionId
            , _user        = user
            , _tvSeqNum    = tvSeqNum
            , _reqManager  = manager
            , _voiceStates = M.empty
            }

    evalStateT (sessionWorker conn) state

sessionWorker :: Connection -> StateT DiscordState IO ()
sessionWorker conn = do
    message <- liftIO $ receiveMessage conn
    liftIO $ pPrint message
    case message of
        DispatchMessage seqNum event -> do
            DiscordState { _tvSeqNum = tvSeqNum } <- get
            atomically $ writeTVar tvSeqNum seqNum
            case event of
                MessageCreateEvent{..} -> handleMessageEvent conn event
                _ -> return ()
        _ -> return ()
    sessionWorker conn

mentions :: Text -> User -> Bool
mentions text user = not . null $ match
    where User { _id = userId } = user
          regex = "^<@!?" ++ userId ++ ">"
          match = unpack text =~ unpack regex :: String

handleMessageEvent :: Connection -> ServerEvent -> StateT DiscordState IO ()
handleMessageEvent conn MessageCreateEvent{..} = do
    state@DiscordState { _user = me, _token = token, _reqManager = manager } <- get
    let User { _id = authorId } = _author

    when (_content `mentions` me) $ do
        let content = T.strip . T.tail . dropWhile (/= '>') $ _content :: Text
        let lowContent = T.toLower content
        let mention = "<@!" ++ authorId ++ ">"
        liftIO $ createMessage _channelId (mention ++ replyTo lowContent) token manager

handleMessageEvent conn _ = return ()


replyTo :: Text -> Text
replyTo "ping"        = ", pong!"
replyTo "dobranoc"    = ", お休み!"
replyTo "araara"      = ", アラアラ!"
replyTo _             = ", ?"
replyTo "meow"        = ", meow!"
replyTo "dzień dobry" = ", おはよう"
