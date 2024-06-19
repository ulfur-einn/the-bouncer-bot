{-# LANGUAGE ScopedTypeVariables #-}

module VRC.Group.GetGroup (getGroup) where

import Network.HTTP.Simple
import qualified Data.Text as T
import Network.HTTP.Conduit
import Tools
import Config

getGroup :: Config -> Cookie -> T.Text -> IO Group
getGroup config auth grpid = do
    request <- parseRequest ("GET https://api.vrchat.cloud/api/1/groups/" <> T.unpack grpid)
    response <- vrcSendRequestJSON config request auth
    return (responseBody response)
