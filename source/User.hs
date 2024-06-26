{-# LANGUAGE DeriveGeneric #-}

module User (User(..), readUser, readUserRespond, removeUser, removeUserRespond) where

import Data.Aeson
import GHC.Generics
import Control.Monad.IO.Class
import Data.Time
import qualified Discord.Types as DT
import Discord
import Discord.Interactions
import Data.List
import Tools hiding (message)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)


data User = User {
  userId :: DT.UserId,
  userName :: T.Text,
  userGlobalName :: Maybe T.Text,
  memberNick :: Maybe T.Text,
  memberJoinedAt :: UTCTime,
  dateOfBirth :: Day,
  dateOfRegistry :: UTCTime,
  vrcUUId :: Maybe T.Text,
  isInDiscord :: Bool
} deriving Generic
instance FromJSON User
instance ToJSON User


readUser :: CreateApplicationCommand
readUser = CreateApplicationCommandChatInput
    {
        createName = "read",
        createLocalizedName = Nothing,
        createDescription = "Retrieve a user record.",
        createLocalizedDescription = Nothing,
        createDefaultMemberPermissions = Nothing,
        createDMPermission = Just False,
        createOptions = Just $ OptionsValues [
            OptionValueUser
            {
                optionValueName = "user",
                optionValueLocalizedName = Nothing,
                optionValueDescription = "Enter the user here.",
                optionValueLocalizedDescription = Nothing,
                optionValueRequired = True
            }
        ]
    }

readUserRespond :: DT.GuildId -> DT.UserId -> DT.InteractionId -> InteractionToken -> DiscordHandler ()
readUserRespond guildId targetUserId interId interToken = do
    result <- liftIO $ eitherDecodeFileStrict (id2FilePath guildId)
    oldUsers <- case result of
            Left msg -> fail ("Failure reading JSON in readUserRespond with response of " <> msg)
            Right a -> return (a :: [User])
    let f (User {userId = uid}) = targetUserId == uid
    case find f oldUsers of
        Just targetUser -> sendResponse interId interToken (
            "User has a record of \n" 
            <> "userID: " <> T.pack (show $ userId targetUser) <> "\n"
            <> "userName: " <> userName targetUser <> "\n"
            <> "userGlobalName: " <> fromMaybe "Global Name is not set." (userGlobalName targetUser) <> "\n"
            <> "memberNick: " <> fromMaybe "Member Name is not set." (memberNick targetUser) <> "\n"
            <> "memberJoinedAt: " <> "<t:" <> T.pack (show (floor (utcTimeToPOSIXSeconds (memberJoinedAt targetUser)) :: Int)) <> ":f>" <> "\n"
            <> "dateOfBirth: " <> T.pack (show $ dateOfBirth targetUser) <> "\n"
            <> "dateOfRegistry:" <> "<t:" <> T.pack (show (floor (utcTimeToPOSIXSeconds (dateOfRegistry targetUser)) :: Int)) <> ":f>" <> "\n"
            <> "vrcUUId: " <> fromMaybe "vrcUUId is not set." (vrcUUId targetUser)
            )
        Nothing -> sendResponse interId interToken "User was not found."

removeUser :: CreateApplicationCommand
removeUser = CreateApplicationCommandChatInput {
    createName = "remove",
    createLocalizedName = Nothing,
    createDescription = "Remove a users entry.",
    createLocalizedDescription = Nothing,
    createDefaultMemberPermissions = Nothing,
    createDMPermission = Just False,
    createOptions = Just $ OptionsValues [
        OptionValueUser {
            optionValueName = "user",
            optionValueLocalizedName = Nothing,
            optionValueDescription = "Enter the user here.",
            optionValueLocalizedDescription = Nothing,
            optionValueRequired = True
        }]
}

removeUserRespond :: DT.GuildId -> DT.UserId -> DT.InteractionId -> InteractionToken -> DiscordHandler ()
removeUserRespond guildId targetUserId interId interToken = do
    result <- liftIO $ eitherDecodeFileStrict (id2FilePath guildId)
    oldUsers <- case result of
            Left msg -> fail ("Failure reading JSON in removeUserRespond with response of " <> msg)
            Right a -> return a
    let f (User {userId = uid}) = targetUserId == uid
    case find f oldUsers of 
        Just targetUser -> do
            let prunedUsers = filter (not . f) oldUsers
            liftIO $ encodeFile (id2FilePath guildId) prunedUsers
            sendResponse interId interToken ("User " <> userName targetUser <> " has been removed.")
        Nothing -> sendResponse interId interToken "User was not found."
