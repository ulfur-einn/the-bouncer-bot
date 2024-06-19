{-# LANGUAGE ScopedTypeVariables #-}
module VRC.Group.GetInstance where

import Network.HTTP.Conduit
import Config
import qualified Data.Text as T
import Tools
import qualified Discord.Types as DT
import qualified Data.Map as Map

getGroupInstances :: DT.GuildId -> Config -> Cookie -> IO [Instance]
getGroupInstances guildId config auth = do
    let maybeserver = Map.lookup guildId (servers config)
    server <- maybe (fail "No server in getGroupInstance.") return maybeserver
    request <- parseRequest ("GET https://api.vrchat.cloud/api/1/users/" <> T.unpack (usrId config) <> "/instances/groups/" <> T.unpack (vrcGroupId server))
    response :: Response GroupInstancesReply <- vrcSendRequestJSON config request auth
    return (instances (responseBody response))
