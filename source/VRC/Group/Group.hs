{-# LANGUAGE ScopedTypeVariables #-}

module VRC.Group.Group (getGroup, getGroupInstances, removeFromGroup) where

import Network.HTTP.Simple
import qualified Data.Text as T
import Data.List
import Network.HTTP.Conduit
import Tools
import Config
import Discord
import qualified Discord.Types as DT
import qualified Data.Map as Map
import Control.Monad.IO.Class
import User
import Data.Aeson hiding (Success,Error)

getGroup :: Config -> Cookie -> T.Text -> IO Group
getGroup config auth grpid = do
    request <- parseRequest ("GET https://api.vrchat.cloud/api/1/groups/" <> T.unpack grpid)
    response <- vrcSendRequestJSON config request auth
    return (responseBody response)

getGroupInstances :: DT.GuildId -> Config -> Cookie -> IO [Instance]
getGroupInstances guildId config auth = do
    let maybeserver = Map.lookup guildId (servers config)
    server <- maybe (fail "No server in getGroupInstance.") return maybeserver
    request <- parseRequest ("GET https://api.vrchat.cloud/api/1/users/" <> T.unpack (usrId config) <> "/instances/groups/" <> T.unpack (vrcGroupId server))
    response :: Response GroupInstancesReply <- vrcSendRequestJSON config request auth
    return (instances (responseBody response))

removeFromGroup :: Config -> Server -> DT.GuildId -> DT.UserId -> Cookie -> DiscordHandler ()
removeFromGroup config server guildId targetUserId auth = do
    result <- liftIO $ eitherDecodeFileStrict (id2FilePath guildId)
    oldUsers <- case result of
            Left msg -> fail ("Failure reading JSON in removeFromGroup with response of " <> msg)
            Right a -> return (a :: [User])
    let f (User {userId = uid}) = targetUserId == uid
    case find f oldUsers of
        Just user -> do
            let newUser = user {isInDiscord = False}
            let remUsers = filter (not.f) oldUsers
            let newUsers = remUsers ++ [newUser]
            liftIO (encodeFile (id2FilePath guildId) newUsers)
            case vrcUUId user of
                Just vrcId -> do
                    request <- parseRequest ("DELETE https://api.vrchat.cloud/api/1/groups/" <> T.unpack (vrcGroupId server) <> "/members/" <> T.unpack vrcId)
                    response <- vrcSendRequestJSON config request auth
                    case responseBody response of
                        Error _ msg -> do
                            postToBotChannel (botOutputChannel server) ("Tried to remove a user with userId: " <> T.pack (show targetUserId) <> " from the group, but encountered a response of " <> msg)
                        Success _ msg -> do
                            postToBotChannel (botOutputChannel server) ("VRChat successfully removed userId: " <> T.pack (show targetUserId) <> " with a reply of " <> msg)
                Nothing -> return ()
        Nothing -> postToBotChannel (botOutputChannel server) ("An unregistered user with userId of " <> T.pack (show targetUserId) <> " has left.")
