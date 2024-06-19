module RemoveUser (removeUser, removeUserRespond) where

import Discord.Interactions
import qualified Discord.Types as DT
import Discord
import Data.Aeson
import Control.Monad.IO.Class
import User
import Tools
import Data.List

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
