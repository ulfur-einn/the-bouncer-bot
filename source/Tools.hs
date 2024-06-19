{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tools (Reply (..), Group (..), GroupInstancesReply (..), Instance (..), World (..),
    sendModChannelNameResp, vrcSendRequestJSON, sendResponse, registerCommand, postToBotChannel, worldName, addRole, id2FilePath
    ) where

import qualified Discord.Types as DT
import Data.Text
import Discord hiding (Request)
import Discord.Interactions
import qualified Discord.Requests as R
import GHC.Generics
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import Control.Monad
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Control.Monad.IO.Class
import Config

sendResponse :: DT.InteractionId -> DT.InteractionToken -> Text -> DiscordHandler ()
sendResponse interactionId interactionToken message = do
    let response = interactionResponseBasic message
    let interResp = R.CreateInteractionResponse interactionId interactionToken response
    response <- restCall interResp
    either (fail.show) return response

sendModChannelNameResp :: DT.ChannelId -> Text -> DiscordHandler ()
sendModChannelNameResp id text = do
    let opts = def { R.modifyChannelName = Just text }
    let request = R.ModifyChannel id opts
    response <- restCall request
    either (fail.show) (void.return) response

registerCommand :: DT.ApplicationId -> CreateApplicationCommand -> DiscordHandler ()
registerCommand appid command = do
    response <- restCall $ R.CreateGlobalApplicationCommand appid command
    either (fail.show) (void.return) response

postToBotChannel :: DT.ChannelId -> T.Text -> DiscordHandler ()
postToBotChannel channelId message = do
    let request = R.CreateMessage channelId  message
    response <- restCall request
    either (fail.show) (void.return) response

vrcSendRequestJSON :: (MonadIO m, FromJSON a) => Config -> Request -> Cookie -> m (Response a)
vrcSendRequestJSON config request auth = do
    let cookieReq = request {cookieJar = Just $ createCookieJar [auth]}
    let agentReq = addRequestHeader "User-Agent" (encodeUtf8 $ userAgent config) cookieReq
    httpJSON agentReq

addRole :: DT.GuildId -> DT.UserId -> DT.RoleId -> DiscordHandler ()
addRole guildId userId roleId = do
    response <- restCall $ R.AddGuildMemberRole guildId userId roleId
    either (fail.show) return response

id2FilePath :: Show a => a -> String
id2FilePath guildId = "Guilds/" <> show guildId <>".json"

options :: Data.Aeson.Options
options = defaultOptions {
    sumEncoding = ObjectWithSingleField,
    constructorTagModifier = camelTo2 '_',
    fieldLabelModifier = camelTo2 '_'
    }

data Reply =
    Error {statusCode :: Int, message :: T.Text} |
    Success {statusCode :: Int, message :: T.Text}
    deriving (Generic, Show)

instance FromJSON Reply where
    parseJSON = genericParseJSON options
instance ToJSON Reply where
    toJSON = genericToJSON options

data Group = Group {memberCount :: Int, onlineMemberCount :: Int}
    deriving (Generic, Show, FromJSON, ToJSON)

data Instance = Instance {name :: Text, userCount :: Int, world :: World}
    deriving (Generic, Show, FromJSON, ToJSON)

data GroupInstancesReply = GroupInstancesReply {instances :: [Instance]}
    deriving (Generic, Show, FromJSON, ToJSON)

data World = World {capacity :: Int, name :: Text}
    deriving (Generic, Show, FromJSON, ToJSON)

worldName :: World -> T.Text
worldName (World _ a) = a
