{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config (Config (..), Server(..)) where
import Data.Aeson
import Data.Text
import qualified Discord.Types as DT
import GHC.Generics
import Data.Map

data Config = Config {
    --Discord
    authToken :: Text,
    applicationId :: DT.ApplicationId,
    --VRChat
    userAgent :: Text,
    username :: Text,
    password :: Text,
    authSecret :: Text,
    usrId :: Text,
    servers :: Map DT.GuildId Server
    } deriving (Generic, FromJSON)
    
data Server = Server{    
    displayMembersChannel :: DT.ChannelId,
    displayOnlineMembersChannel :: DT.ChannelId,
    botOutputChannel :: DT.ChannelId,
    instancesCategory :: DT.ParentId,
    vrcGroupId :: Text,
    verifiedRole :: DT.RoleId,
    memberRole :: DT.RoleId
    } deriving (Generic, FromJSON)
