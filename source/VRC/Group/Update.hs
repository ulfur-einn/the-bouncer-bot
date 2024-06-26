{-# LANGUAGE ScopedTypeVariables #-}

module VRC.Group.Update (updateMembers, updateOnlineMembers, updateAll, updateChannels, forceUpdate, forceUpdateRespond, aSyncUpdate) where

import VRC.Group.Group
import Config
import Network.HTTP.Client.Conduit
import Tools
import qualified Data.Text as T
import VRC.Login (refreshCookies)
import Data.IORef (readIORef, writeIORef, IORef)
import Discord
import Control.Monad.IO.Class
import Channels
import qualified Discord.Types as DT
import Data.Foldable
import Discord.Interactions
import qualified Data.Map as Map
import Control.Concurrent
import Network.HTTP.Simple
import UnliftIO.Exception

updateAll :: Config -> IORef (Cookie, Cookie) -> DiscordHandler ()
updateAll config cookies = do
    (oldAuth,oldTwoFactorAuth) <- liftIO $ readIORef cookies
    (auth,twoFactorAuth) <- liftIO $ refreshCookies config (oldAuth,oldTwoFactorAuth)
    liftIO $ writeIORef cookies (auth,twoFactorAuth)
    let allServers = Map.assocs (servers config)
    traverse_ (updateMembers config auth) allServers
    traverse_ (updateOnlineMembers config auth) allServers
    traverse_ (updateChannels auth config) allServers

updateMembers :: Config -> Cookie -> (DT.GuildId, Server) -> DiscordHandler ()
updateMembers config auth (_, server) = do
    group <- liftIO $ getGroup config auth (vrcGroupId server)
    sendModChannelNameResp (displayMembersChannel server) ("Members: " <> T.pack (show (memberCount group)))

updateOnlineMembers :: Config -> Cookie -> (DT.GuildId, Server) -> DiscordHandler ()
updateOnlineMembers config auth (_, server) = do
    group <- liftIO $ getGroup config auth (vrcGroupId server)
    sendModChannelNameResp (displayOnlineMembersChannel server) ("Online: " <> T.pack (show (onlineMemberCount group)))

updateChannels :: Cookie -> Config -> (DT.GuildId, Server) -> DiscordHandler ()
updateChannels auth config (guildId, server) = do
    insts <- liftIO $ getGroupInstances guildId config auth
    channels <- getChannels guildId
    let
        f :: DT.Channel -> Bool
        f (DT.ChannelVoice {DT.channelParentId = prntId}) = prntId == Just (instancesCategory server)
        f _  = False
    let targetChannels = filter f channels
    traverse_ (deleteChannel . DT.channelId) targetChannels
    traverse_ (inst2Channel guildId (instancesCategory server)) insts

inst2Channel :: DT.GuildId -> DT.ParentId -> Instance -> DiscordHandler ()
inst2Channel guildId instCategory inst = makeChannel guildId instCategory text where
    text = worldName (world inst) <> " " <> T.pack (show (userCount inst)) <> "/" <> T.pack (show $ capacity (world inst))

forceUpdate :: CreateApplicationCommand
forceUpdate = CreateApplicationCommandChatInput {
    createName = "update",
    createLocalizedName = Nothing,
    createDescription = "Update Statistics and Instances",
    createLocalizedDescription = Nothing,
    createDefaultMemberPermissions = Nothing,
    createDMPermission = Just False,
    createOptions = Nothing
    }

forceUpdateRespond :: Config -> DT.GuildId -> IORef (Cookie,Cookie) -> T.Text ->
    DT.InteractionId -> DT.InteractionToken -> DiscordHandler ()
forceUpdateRespond config guildId cookies usrname interId interToken = do
    sendResponse interId interToken "Will attempt an update."
    updateAll config cookies
    let maybeserver = Map.lookup guildId (servers config)
    server <- maybe (fail "No server in eventhandle for InvGroupResp.") return maybeserver
    postToBotChannel (botOutputChannel server) (usrname <> " has called for an update.")

aSyncUpdate :: Config -> IORef (Cookie, Cookie) -> DiscordHandler ()
aSyncUpdate config cookies = do
    catches (updateAll config cookies)
        [Handler $ \ (ex :: IOException) -> liftIO $ print ex,
        Handler $ \ (ex :: JSONException) -> liftIO $ print ex,
        Handler $ \ (ex :: HttpException) -> liftIO $ print ex]
    liftIO $ threadDelay $ 1000000 * 60 * 30  -- 1 second * 60 seconds * 30 minutes
    aSyncUpdate config cookies
