{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Messages where

import ClassyPrelude

import Data.Aeson.Types
import Network.HTTP.Client
import Text.Show.Pretty (pPrint, ppShow)


type AuthToken   = Text
type ChannelType = Text
type GuildId     = Text
type ChannelId   = Text
type UserId      = Text
type SessionId   = Text
type SeqNum      = Int
type VoiceStates = HashMap UserId ChannelId


data DiscordState = DiscordState
    { _token       :: AuthToken
    , _sessionId   :: SessionId
    , _tvSeqNum    :: TVar SeqNum
    , _reqManager  :: Manager
    , _user        :: User
    , _voiceStates :: VoiceStates
    }

data ClientMessage =
      ConnectMessage
          { _token    :: AuthToken
          , _os       :: Text
          , _browser  :: Text
          , _device   :: Text
          , _compress :: Bool
          }
    | ResumeMessage
          { _token     :: AuthToken
          , _sessionId :: SessionId
          , _seqNum    :: SeqNum
          }
    | HeartbeatMessage
          { _seqNum :: SeqNum
          }
      deriving Show

data ServerMessage =
      DispatchMessage
          { _seqNum :: SeqNum
          , _event  :: ServerEvent
          }
    | HelloMessage
          { _heartbeatInterval :: Int
          }
    | HeartbeatAckMessage
    | UnknownMessage Int Value
      deriving Show

data ServerEvent =
      ReadyEvent
          { _protocol  :: Int
          , _sessionId :: SessionId
          , _guildIds  :: [GuildId]
          , _user      :: User
          }
    | MessageCreateEvent
          { _author    :: User
          , _channelId :: ChannelId
          , _content   :: Text
          }
    | UnknownEvent Text
      deriving Show

data User = User
    { _id            :: UserId
    , _username      :: Text
    , _discriminator :: Text
    }

instance ToJSON ClientMessage where
    toJSON = buildMessage

instance FromJSON ServerMessage where
    parseJSON = parseMessage

instance Show User where
    show User{..} = unpack $ _username ++ "#" ++ _discriminator


parseMessage :: Value -> Parser ServerMessage
parseMessage = withObject "payload" $ \obj -> do
    opcode    <- obj .: "op"
    eventData <- obj .: "d"
    case (opcode :: Int) of
        0  -> do
            seqNum    <- obj .: "s"
            eventName <- obj .: "t"
            DispatchMessage seqNum <$> parseEvent eventName eventData
        10 -> flip (withObject "event data") eventData $ \d -> do
            _heartbeatInterval <- d .: "heartbeat_interval"
            return HelloMessage{..}
        11 -> return HeartbeatAckMessage
        _  -> return $ UnknownMessage opcode eventData


parseEvent :: Text -> Value -> Parser ServerEvent
parseEvent "READY"          = withObject "event data" $ \obj -> do
--    traceM $ ppShow obj
    _protocol  <- obj .: "v"
    _sessionId <- obj .: "session_id"
    _user      <- obj .: "user"   >>= parseUser
    _guildIds  <- obj .: "guilds" >>=
        withArray "guild ids" (\arr -> forM (toList arr) $
            withObject "guild id" $ flip (.:) "id")
    return ReadyEvent{..}
parseEvent "GUILD_CREATE"   = withObject "event data" $ \obj -> do
    traceM $ ppShow obj
    return $ UnknownEvent "GUILD_CREATE"
parseEvent "MESSAGE_CREATE" = withObject "event data" $ \obj -> do
    _author    <- obj .: "author" >>= parseUser
    _channelId <- obj .: "channel_id"
    _content   <- obj .: "content"
    return MessageCreateEvent{..}
parseEvent eventName = \event -> do
    traceM $ ppShow event
    return $ UnknownEvent eventName


parseUser :: Value -> Parser User
parseUser = withObject "user" $ \obj -> do
    _id            <- obj .: "id"
    _username      <- obj .: "username"
    _discriminator <- obj .: "discriminator"
    return User{..}


buildMessage :: ClientMessage -> Value
buildMessage HeartbeatMessage{..} = object
    [ "op" .= (1 :: Int)
    , "d"  .= _seqNum
    ]
buildMessage ConnectMessage{..} = object
    [ "op" .= (2 :: Int)
    , "d"  .= object
        [ "token"      .= _token
        , "compress"   .= _compress
        , "properties" .= object
            [ "$os"               .= _os
            , "$browser"          .= _browser
            , "$device"           .= _device
            , "$referrer"         .= ("" :: Text)
            , "$referring_domain" .= ("" :: Text)
            ]
        ]
    ]
buildMessage ResumeMessage{..} = object
    [ "op" .= (6 :: Int)
    , "d"  .= object
        [ "token"      .= _token
        , "session_id" .= _sessionId
        , "seq"        .= _seqNum
        ]
    ]
