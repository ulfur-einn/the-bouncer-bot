{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Types as DT
import Data.Aeson
import VerifyDOB
import Tools
import User
import EventHandle
import Config
import VRC.Group.InviteToGroup
import VRC.Login
import Data.IORef
import VRC.Group.Update
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import Control.Monad
import Data.Aeson.Types 
import Data.Foldable
import Fix
import Control.Concurrent.Async
import Control.Monad.Reader hiding (fix)

main :: IO ()
main = do
    result <- eitherDecodeFileStrict "configGlobal.json"
    config <- either fail return result
    let allServers = Map.keys (servers config)
    traverse_ checkFile allServers
    initialCookies <- loginInitial config --should only need to pass auth. 2fa is for login
    cookies <- newIORef initialCookies
    let opts = def {
        discordToken = authToken config,
        discordOnStart = do
            let appId = applicationId config
            registerCommand appId verifyDOBCreate
            registerCommand appId removeUser
            registerCommand appId readUser
            registerCommand appId inviteToGroupReg
            registerCommand appId forceUpdate
            registerCommand appId fix
            registerCommand appId reinvite
            handle <- ask
            let a = runReaderT (aSyncUpdate config cookies) handle
            liftIO $ void $ async a,
        discordOnEvent = eventHandle config cookies,
        discordOnLog = TIO.putStrLn,
        discordForkThreadForEvents = False,
        discordGatewayIntent = def {DT.gatewayIntentMessageContent = False, DT.gatewayIntentMembers = True},
        discordOnEnd = do
            oldCookies <- readIORef cookies
            result <- logout config oldCookies
            print result
    }

    err <- runDiscord opts

    TIO.putStrLn err

checkFile :: DT.GuildId -> IO ()
checkFile guildId = do
    check <- doesFileExist (id2FilePath guildId)
    unless check $ encodeFile (id2FilePath guildId) emptyArray
