{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module VRC.Login (loginInitial, refreshCookies) where

import Network.HTTP.Simple
import Network.HTTP.Types
import Data.ByteString.Base64
import Crypto.OTP
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS
import Data.ByteArray.Encoding
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.Aeson.Types
import qualified Data.Text as T
import Config
import Data.Text.Encoding
import Network.HTTP.Conduit
import Data.Time
import Data.List
import Text.Printf

loginBasic :: Config -> Maybe Cookie -> IO Cookie
loginBasic config cookie = case cookie of
    Nothing -> do
        let auth = encode (urlEncode True (encodeUtf8 (username config)) <> ":" <> urlEncode True (encodeUtf8 (password config)))
        request <- parseRequest "GET https://api.vrchat.cloud/api/1/auth/user"
        let authReq = addRequestHeader "Authorization" ("Basic " <> auth) request
        let agentReq = addRequestHeader "User-Agent" (encodeUtf8 $ userAgent config) authReq
        response :: Response Value <- httpJSON agentReq
        case getResponseStatusCode response of
            200 -> do
                let newCookieJar = responseCookieJar response
                let newCookies = destroyCookieJar newCookieJar
                case newCookies of
                    [a] -> case find isAuth [a] of
                        Nothing -> fail $ "auth token was not found in response to loginBasic in Nothing case with input of " <> show a
                        Just a -> return a
                    b -> fail ("Unexpected Cookies from loginBasic in Nothing case, with input of " <> show b)
            a -> fail $ "Unexpected Statuscode from loginBasic in Nothing case, with code of " <> show a
    Just cookie -> do
        let auth = encode (urlEncode True (encodeUtf8 (username config)) <> ":" <> urlEncode True (encodeUtf8 (password config)))
        request <- parseRequest "GET https://api.vrchat.cloud/api/1/auth/user"
        let cookieReq = request {cookieJar = Just $ createCookieJar [cookie]}
        let authReq = addRequestHeader "Authorization" ("Basic " <> auth) cookieReq
        let agentReq = addRequestHeader "User-Agent" (encodeUtf8 $ userAgent config) authReq
        response :: Response Value <- httpJSON agentReq
        case getResponseStatusCode response of
            200 -> do
                let newCookieJar = responseCookieJar response
                let newCookies = destroyCookieJar newCookieJar
                case find isAuth newCookies of
                    Nothing -> fail $ "auth token was not found in response to loginBasic in Just case with input of " <> show newCookies
                    Just a -> return a
            a -> fail $ "Unexpected Statuscode from loginBasic in Just case, with code of " <> show a

tfa :: T.Text -> IO OTP
tfa secret = do
    let key = BC.map toUpper $ BC.filter (/= ' ') (encodeUtf8 secret)
    decodedkey <- case convertFromBase Base32 key of
        Left msg -> fail ("TFA responded with a error of " <> msg)
        Right a -> return (a :: BS.ByteString)
    t <- getPOSIXTime
    let otpTime = floor t :: OTPTime
    let x = totp defaultTOTPParams decodedkey otpTime
    return x

verifyWithTFA :: Config -> Cookie -> IO Cookie
verifyWithTFA config cookie = do
    request <- parseRequest "POST https://api.vrchat.cloud/api/1/auth/twofactorauth/totp/verify"
    let cookieReq = request {cookieJar = Just $ createCookieJar [cookie]}
    x <- tfa (authSecret config)
    let codeReq = setRequestBodyJSON (object [("code", String $ T.pack $ printf "%06u" x)]) cookieReq
    let agentReq = addRequestHeader "User-Agent" (encodeUtf8 $ userAgent config) codeReq
    response :: Response Value <- httpJSON agentReq
    case getResponseStatusCode response of
            200 -> do
                let newCookieJar = responseCookieJar response
                let newCookies = destroyCookieJar newCookieJar
                case newCookies of
                    [a,b] -> case find isTwoFactorAuth [a,b] of
                        Nothing -> fail $ "TwoFactorAuth token was not found in response to verifyWithTFA with input of " <> show [a,b]
                        Just a -> return a
                    c -> fail ("Unexpected Cookies from verifyWithTFA of " <> show c)
            _ -> fail $ "Unexpected Statuscode from verifyWithTFA, with a response of " <> show response

refreshCookies :: Config -> (Cookie,Cookie) -> IO (Cookie,Cookie)
refreshCookies config (authCookie,twoFactorAuth) = do
    time <- getCurrentTime
    case (isValidCookie time authCookie, isValidCookie time twoFactorAuth) of
        (False,False) -> loginInitial config
        (True, True) -> return (authCookie,twoFactorAuth)
        (True, False) -> do
            newCookie <- verifyWithTFA config authCookie
            return (authCookie,newCookie)
        (False, True) -> do
            newCookie <- loginBasic config (Just twoFactorAuth)
            return (newCookie,twoFactorAuth)

isValidCookie :: UTCTime -> Cookie -> Bool
isValidCookie time cookie = addUTCTime (60 * 60 * 24) time < cookie_expiry_time cookie

loginInitial :: Config -> IO (Cookie,Cookie)
loginInitial config = do
    newCookie <- loginBasic config Nothing
    newSuperCookie <- verifyWithTFA config newCookie
    return (newCookie, newSuperCookie)

isTwoFactorAuth :: Cookie -> Bool
isTwoFactorAuth cookie = cookie_name cookie == "twoFactorAuth"

isAuth :: Cookie -> Bool
isAuth cookie = cookie_name cookie == "auth"
