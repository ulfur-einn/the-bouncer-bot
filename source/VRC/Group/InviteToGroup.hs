{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module VRC.Group.InviteToGroup (inviteToGroup, inviteToGroupReg, inviteToGroupRespond, reinvite, reinviteRespond) where

import Network.HTTP.Client.Conduit
import Network.HTTP.Simple
import Data.Aeson hiding (Error, Success)
import qualified Data.Text as T
import Discord.Interactions
import User
import Tools
import qualified Discord.Types as DT
import Discord
import Control.Monad.IO.Class
import Data.List
import Config
import Data.Maybe
import Data.Text.Encoding

inviteToGroup :: Config -> Cookie -> T.Text -> T.Text -> IO Reply
inviteToGroup config auth usrID grpID = do
    request <- parseRequest ("POST https://api.vrchat.cloud/api/1/groups/" <> T.unpack grpID <> "/invites")
    let bodyReq = setRequestBodyJSON (object [("userId", String usrID),("confirmOverrideBlock","false")]) request
    let cookieReq = bodyReq {cookieJar = Just $ createCookieJar [auth]}
    let agentReq = addRequestHeader "User-Agent" (encodeUtf8 $ userAgent config) cookieReq
    response <- httpJSON agentReq
    return (responseBody response)

inviteToGroupReg :: CreateApplicationCommand
inviteToGroupReg = CreateApplicationCommandChatInput {
    createName = "invite",
    createLocalizedName = Nothing,
    createDescription = "Have the bot invite a user.",
    createLocalizedDescription = Nothing,
    createDefaultMemberPermissions = Nothing,
    createDMPermission = Just False,
    createOptions = Just $ OptionsValues [
        OptionValueString
        {
            optionValueName = "usrid",
            optionValueLocalizedName = Nothing,
            optionValueDescription = "Please include the UUID of the user, found on VRChat's website.",
            optionValueLocalizedDescription = Nothing,
            optionValueRequired = True,
            optionValueStringChoices = Left False,
            optionValueStringMinLen = Just 40,
            optionValueStringMaxLen = Just 40
        }
    ]
    }

inviteToGroupRespond :: Config -> T.Text -> DT.GuildId -> Server -> DT.UserId -> Cookie -> DT.InteractionId -> InteractionToken -> DiscordHandler ()
inviteToGroupRespond config vrcUUId guildId server userId auth interId interToken  = do
    result <- liftIO $ eitherDecodeFileStrict (id2FilePath guildId)
    oldUsers <- case result of
            Left msg -> fail ("Failure reading JSON in inviteToGroupRespond with response of " <> msg)
            Right a -> return (a :: [User])
    let f (User {userId = uid, vrcUUId = vrciD}) = userId == uid && isNothing vrciD
    case find f oldUsers of
        Just user -> do
            let addVRCID = do
                let newUser = user {vrcUUId = Just vrcUUId}
                let remUsers = filter (not.f) oldUsers
                let newUsers = remUsers ++ [newUser]
                liftIO (encodeFile (id2FilePath guildId) newUsers)
                addRole guildId userId (memberRole server)
            result <- liftIO $ inviteToGroup config auth vrcUUId (vrcGroupId server)
            case result of
                Error 400 msg -> do
                    addVRCID
                    sendResponse interId interToken ("VRChat replied with an error of: " <> msg <> " However the VRC ID was still successfully registered.")
                Error 404 msg -> do
                    sendResponse interId interToken ("VRChat replied with an error of: " <> msg)
                Error _ msg -> do
                    sendResponse interId interToken "VRChat replied with an unexpected error. Please ping a mod in a separate reply to this message."
                    fail ("Failure in attempt to invite with response from VRC of " <> T.unpack msg)
                Success _ msg -> do
                    addVRCID
                    sendResponse interId interToken msg
        Nothing -> sendResponse interId interToken "User was either not found in the Discord or has already been registered. Contact a moderator if you need any help."


reinvite :: CreateApplicationCommand
reinvite = CreateApplicationCommandChatInput
    {
        createName = "reinvite",
        createLocalizedName = Nothing,
        createDescription = "Have the bot send an invite to a registered VRC UUID.",
        createLocalizedDescription = Nothing,
        createDefaultMemberPermissions = Nothing,
        createDMPermission = Just False,
        createOptions = Nothing
    }

reinviteRespond :: Config -> DT.UserId -> DT.GuildId -> Server -> Cookie -> DT.InteractionId -> InteractionToken -> DiscordHandler ()
reinviteRespond config usrId guildId server auth interId interToken = do
    result <- liftIO $ eitherDecodeFileStrict (id2FilePath guildId)
    oldUsers <- case result of
            Left msg -> fail ("Failure reading JSON in reinviteRespond with response of " <> msg)
            Right a -> return (a :: [User])
    let f (User {userId = uid}) = usrId == uid
    case find f oldUsers of
        Just user -> do
            let vrcID = vrcUUId user
            case vrcID of
                Just vrcId -> do
                    result <- liftIO $ inviteToGroup config auth vrcId (vrcGroupId server)
                    case result of
                        Error 400 msg -> do
                            sendResponse interId interToken ("VRChat replied with an error of: " <> msg)
                        Error 404 msg -> do
                            sendResponse interId interToken ("VRChat replied with an error of: " <> msg)
                        Error _ msg -> do
                            sendResponse interId interToken "VRChat replied with an unexpected error. Please ping a mod in a separate reply to this message."
                            fail ("Failure in attempt to invite with response from VRC of " <> T.unpack msg)
                        Success _ msg -> do
                            sendResponse interId interToken msg
                Nothing -> sendResponse interId interToken "You have no recorded VRC UUID on record. Please use /invite to register one."
        Nothing -> sendResponse interId interToken "User was either not found in the Discord or has already been registered. Contact a moderator if you need any help."
