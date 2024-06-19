{-# OPTIONS_GHC -Wno-name-shadowing #-}

module VerifyDOB (verifyDOBCreate, verifyDOBRespond) where
    
import Discord.Interactions
import User
import Tools
import Data.Time
import Control.Monad.IO.Class
import Data.Aeson
import qualified Discord.Types as DT
import qualified Data.Text as T
import Discord
import Data.List
import Config

verifyDOBCreate :: CreateApplicationCommand
verifyDOBCreate = CreateApplicationCommandChatInput {
    createName = "verify",
    createLocalizedName = Nothing,
    createDescription = "Enter and verify your date of birth.",
    createLocalizedDescription = Nothing,
    createOptions = Just $ OptionsValues [
        OptionValueInteger {
        optionValueName = "month",
        optionValueDescription = "Enter the month.",
        optionValueLocalizedName = Nothing,
        optionValueLocalizedDescription = Nothing,
        optionValueRequired = True,
        optionValueIntegerChoices = Right [
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
        OptionValueInteger {
        optionValueName = "day",
        optionValueDescription = "Enter the day.",
        optionValueLocalizedName = Nothing,
        optionValueLocalizedDescription = Nothing,
        optionValueRequired = True,
        optionValueIntegerChoices = Left False,
        optionValueIntegerMinVal = Just 1,
        optionValueIntegerMaxVal = Just 31
        },
        OptionValueInteger {
        optionValueName = "year",
        optionValueDescription = "Enter the year.",
        optionValueLocalizedName = Nothing,
        optionValueLocalizedDescription = Nothing,
        optionValueRequired = True,
        optionValueIntegerChoices = Left False,
        optionValueIntegerMinVal = Just 1920,
        optionValueIntegerMaxVal = Nothing
        }
    ],
    createDefaultMemberPermissions = Nothing,
    createDMPermission = Just False
 }

verifyDOBRespond ::
    Integer -> Integer -> Integer ->
    Server -> DT.GuildId -> DT.UserId -> T.Text -> Maybe T.Text ->
    Maybe T.Text -> UTCTime ->
    DT.InteractionId -> InteractionToken ->
    DiscordHandler ()
verifyDOBRespond
    year month day
    server guildId userId usrName userGlobalName
    memberNick memberJoinedAt
    interactionId interactionToken = do
        result <- liftIO $ eitherDecodeFileStrict (id2FilePath guildId)
        oldUsers <- case result of
                Left msg -> fail ("Failure reading JSON in FixRespond with response of " <> msg)
                Right a -> return a
        let dob = fromGregorianValid year (fromIntegral month) (fromIntegral day)
        let f (User {userId = uid}) = userId == uid
        let isAlreadyUser = find f oldUsers
        case isAlreadyUser of
            Just user -> do
                case dob of
                    Just dob -> do
                        (if dob == dateOfBirth user
                            then do
                                let newUser = user {isInDiscord = True}
                                let prunedUsers = filter (not . f) oldUsers
                                let newUsers = prunedUsers ++ [newUser]
                                liftIO $ encodeFile (id2FilePath guildId) newUsers
                                addRole guildId userId (verifiedRole server)
                                sendResponse interactionId interactionToken ("Re-Verification Successful. Welcome Back " <> userName user)
                            else sendResponse interactionId interactionToken "You have entered a date of birth that is different from your previous record. Please try again. If you believe this to be a mistake, please contact a moderator.")
                    Nothing -> sendResponse interactionId interactionToken "Your date of birth is invalid."
            Nothing -> do
                case dob of
                    Just dob -> do
                        currentTime <- liftIO getCurrentTime
                        let today = utctDay currentTime
                        let plusDOB = addGregorianYearsClip 18 dob
                        if today > plusDOB
                            then do
                                let newUser = User {
                                    userId = userId,
                                    userName = usrName,
                                    userGlobalName = userGlobalName,
                                    memberNick = memberNick,
                                    memberJoinedAt = memberJoinedAt,
                                    dateOfBirth = dob,
                                    dateOfRegistry = currentTime,
                                    vrcUUId = Nothing,
                                    isInDiscord = True}
                                let
                                    f :: User -> Bool
                                    f (User {userId = uid}) = userId == uid
                                if not $ any f oldUsers
                                    then do
                                        let newUsers = oldUsers ++ [newUser]
                                        liftIO (encodeFile (id2FilePath guildId) newUsers)
                                        addRole guildId userId (verifiedRole server)
                                        sendResponse interactionId interactionToken "Your date of birth has been successfully recorded!"
                                    else sendResponse interactionId interactionToken "You have already entered your date of birth! Contact a moderator to have it changed."
                            else sendResponse interactionId interactionToken "Your date of birth is less than 18 years old."
                    Nothing -> sendResponse interactionId interactionToken "Your date of birth is invalid."
