module VRC.Logout (logout) where

import Network.HTTP.Conduit
import Network.HTTP.Simple
import Tools
import Config
import Data.Text.Encoding

logout :: Config -> (Cookie, Cookie) -> IO Reply
logout config (auth, twoFactorAuth)= do
    request <- parseRequest "PUT https://api.vrchat.cloud/api/1/logout"
    let agentReq = addRequestHeader "User-Agent" (encodeUtf8 $ userAgent config) request
    let cookieReq = agentReq {cookieJar = Just $ createCookieJar [auth,twoFactorAuth]}
    response <- httpJSON cookieReq
    return (responseBody response)
