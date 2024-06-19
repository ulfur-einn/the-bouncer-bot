{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Fix (fix, fixRespond) where

import Discord.Interactions
import qualified Discord.Types as DT
import Discord
import User
import Tools
import Data.Aeson
import Control.Monad.IO.Class
import Data.List
import Data.Foldable
import Data.Time
import VRC.Group.InviteToGroup (inviteToGroupRespond)
import Config
import Network.HTTP.Conduit


fix :: CreateApplicationCommand
fix = CreateApplicationCommandChatInput
    {
        createName = "fix",
        createLocalizedName = Nothing,
        createDescription = "Fix a users record.",
        createLocalizedDescription = Nothing,
        createDefaultMemberPermissions = Nothing,
        createDMPermission = Just False,
        createOptions = Just $ OptionsValues 
            [
                OptionValueUser
                    {
                        optionValueName = "user",
                        optionValueLocalizedName = Nothing,
                        optionValueDescription = "Enter the user here.",
                        optionValueLocalizedDescription = Nothing,
                        optionValueRequired = True
                    },
                OptionValueString
                    {
                        optionValueName = "usrid",
                        optionValueLocalizedName = Nothing,
                        optionValueDescription = "Please include the UUID of the user, found on VRChat's website.",
                        optionValueLocalizedDescription = Nothing,
                        optionValueRequired = False,
                        optionValueStringChoices = Left False,
                        optionValueStringMinLen = Just 40,
                        optionValueStringMaxLen = Just 40
                    },
                OptionValueInteger 
                    {
                        optionValueName = "month",
                        optionValueDescription = "Enter the month.",
                        optionValueLocalizedName = Nothing,
                        optionValueLocalizedDescription = Nothing,
                        optionValueRequired = False,
                        optionValueIntegerChoices = Right 
                            [
                                Choice "January" Nothing 1,
                                Choice "February" Nothing 2,
                                Choice "March" Nothing 3,
                                Choice "April" Nothing 4,
                                Choice "May" Nothing 5,
                                Choice "June" Nothing 6,
                                Choice "July" Nothing 7,
                                Choice "August" Nothing 8,
                                Choice "September" Nothing 9,
                                Choice "October" Nothing 10,
                                Choice "November" Nothing 11,
                                Choice "December" Nothing 12
                            ],
                        optionValueIntegerMinVal = Nothing,
                        optionValueIntegerMaxVal = Nothing
                    },
                OptionValueInteger 
                    {
                        optionValueName = "day",
                        optionValueDescription = "Enter the day.",
                        optionValueLocalizedName = Nothing,
                        optionValueLocalizedDescription = Nothing,
                        optionValueRequired = False,
                        optionValueIntegerChoices = Left False,
                        optionValueIntegerMinVal = Just 1,
                        optionValueIntegerMaxVal = Just 31
                    },
                OptionValueInteger 
                    {
                        optionValueName = "year",
                        optionValueDescription = "Enter the year.",
                        optionValueLocalizedName = Nothing,
                        optionValueLocalizedDescription = Nothing,
                        optionValueRequired = False,
                        optionValueIntegerChoices = Left False,
                        optionValueIntegerMinVal = Just 1920,
                        optionValueIntegerMaxVal = Nothing
                    }
            ]
    }


fixRespond :: Config -> DT.GuildId -> Server -> Cookie -> [OptionDataValue] -> DT.InteractionId -> InteractionToken -> DiscordHandler ()
fixRespond config guildId server auth values interId interToken = do
    let
        user (OptionDataValueUser "user" usrId) = Just usrId
        user _ = Nothing
    let usrMatch = asum $ map user values
    let
        day (OptionDataValueInteger "day" (Right day)) = Just day
        day _ = Nothing
    let dayMatch = asum $ map day values
    let
        month (OptionDataValueInteger "month" (Right month)) = Just month
        month _ = Nothing
    let monthMatch = asum $ map month values
    let
        year (OptionDataValueInteger "year" (Right year)) = Just year
        year _ = Nothing
    let yearMatch = asum $ map year values
    let
        vrcUUId (OptionDataValueString "usrid" vrcUUId) = Just vrcUUId
        vrcUUId _ = Nothing
    let vrcUUIdMatch = asum $ map vrcUUId values
    usrId <- case usrMatch of
            Just a -> return a
            Nothing -> fail "Unable to find a valid UserId in fix command."       
    result <- liftIO $ eitherDecodeFileStrict (id2FilePath guildId)
    oldUsers <- case result of
            Left msg -> fail ("Failure reading JSON in FixRespond with response of " <> msg)
            Right a -> return (a :: [User])
    let f (User {userId = uid}) = usrId == uid
    case find f oldUsers of
        Just user -> case (dayMatch, monthMatch, yearMatch, vrcUUIdMatch) of
            (Nothing, Nothing, Nothing, Nothing) -> sendResponse interId interToken "You have not entered any information to update."
            (Nothing, Nothing, Nothing, Just vrcID) -> case vrcID of
                Left _ -> sendResponse interId interToken "Recieved unexpected value from Discord for VRCUUID in fix command. Please try again."
                Right vrcID-> do
                    let newUser = user {vrcUUId = Nothing}
                    let prunedUsers = filter (not.f) oldUsers
                    let newUsers = prunedUsers ++ [newUser]
                    liftIO (encodeFile (id2FilePath guildId) newUsers)
                    inviteToGroupRespond config vrcID guildId server usrId auth interId interToken
            (Just day, Just month, Just year, Nothing) -> do
                let dob = fromGregorianValid year (fromIntegral month) (fromIntegral day)
                case dob of
                    Just dob -> do
                        currentTime <- liftIO getCurrentTime
                        let today = utctDay currentTime
                        let plusDOB = addGregorianYearsClip 18 dob
                        if today > plusDOB
                            then do 
                                let newUser = user {dateOfBirth = dob}
                                let prunedUsers = filter (not.f) oldUsers
                                let newUsers = prunedUsers ++ [newUser]
                                liftIO (encodeFile (id2FilePath guildId) newUsers)
                                sendResponse interId interToken "DOB successfully updated."
                            else do
                                sendResponse interId interToken "Entered DOB is less than 18 years of age."
                    Nothing -> sendResponse interId interToken "Entered DOB is an invalid date."
            (Just day, Just month, Just year, Just vrcID) -> do
                let dob = fromGregorianValid year (fromIntegral month) (fromIntegral day)
                case dob of
                    Just dob -> do
                        currentTime <- liftIO getCurrentTime
                        let today = utctDay currentTime
                        let plusDOB = addGregorianYearsClip 18 dob
                        if today > plusDOB
                            then do 
                                case vrcID of
                                    Left _ -> sendResponse interId interToken "Recieved unexpected value from Discord for VRCUUID in fix command. Please try again."
                                    Right vrcID-> do
                                        let newUser = user {dateOfBirth = dob, vrcUUId = Nothing}
                                        let prunedUsers = filter (not.f) oldUsers
                                        let newUsers = prunedUsers ++ [newUser]
                                        liftIO (encodeFile (id2FilePath guildId) newUsers)
                                        inviteToGroupRespond config vrcID guildId server usrId auth interId interToken
                            else do
                                sendResponse interId interToken "Entered DOB is less than 18 years of age."
                    Nothing -> sendResponse interId interToken "Entered DOB is an invalid date."
            _ -> sendResponse interId interToken "Recieved unexpected values for the fix command. Exiting Interaction. Please try again."
        Nothing -> sendResponse interId interToken "User was not found."
