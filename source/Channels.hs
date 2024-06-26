module Channels (getChannels, makeChannel, deleteChannel) where

import Discord.Requests
import Discord
import qualified Discord.Types as DT
import Control.Monad
import qualified Data.Text as T
import Data.Coerce (coerce)

getChannels :: DT.GuildId -> DiscordHandler [DT.Channel]
getChannels guildId = do
    let request = GetGuildChannels guildId
    response <- restCall request
    either (fail.show) return response

makeChannel :: DT.GuildId -> DT.ParentId -> T.Text -> DiscordHandler ()
makeChannel guildId instCategory text = do
    let opts = CreateGuildChannelOptsVoice {
        createGuildChannelOptsMaxUsers = Nothing,
        createGuildChannelOptsCategoryId = Just (coerce instCategory),
        createGuildChannelOptsBitrate = Nothing}
    let request = CreateGuildChannel guildId text [] opts
    response <- restCall request
    either (fail.show) (void.return) response

deleteChannel :: DT.ChannelId -> DiscordHandler ()
deleteChannel channelId = do
    let request = DeleteChannel channelId
    response <- restCall request
    either (fail.show) (void.return) response
