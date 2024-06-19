{-# LANGUAGE DeriveGeneric #-}

module User (User(..)) where
import Data.Aeson
import GHC.Generics
import Data.Time
import Data.Text
import qualified Discord.Types as DT

data User = User {
  userId :: DT.UserId,
  userName :: Text,
  userGlobalName :: Maybe Text,
  memberNick :: Maybe Text,
  memberJoinedAt :: UTCTime,
  dateOfBirth :: Day,
  dateOfRegistry :: UTCTime,
  vrcUUId :: Maybe Text,
  isInDiscord :: Bool
} deriving Generic

instance FromJSON User
instance ToJSON User
