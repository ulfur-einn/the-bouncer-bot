{-# OPTIONS_GHC -Wno-name-shadowing #-}

module EventHandle (eventHandle) where
import Discord.Interactions
import qualified Discord.Types as DT
import Discord
import VerifyDOB
import User
import VRC.Group.InviteToGroup
import Network.HTTP.Conduit
import Config
import Data.IORef
import Control.Monad.IO.Class
import VRC.Group.Update
import qualified Data.Map as Map
import VRC.Group.Group
import Fix



eventHandle :: Config -> IORef (Cookie, Cookie) -> DT.Event -> DiscordHandler ()
eventHandle config _ (DT.InteractionCreate (InteractionApplicationCommand {
    interactionId = interactionId,
    interactionToken = interactionToken,
    applicationCommandData = ApplicationCommandDataChatInput {
        applicationCommandDataName = "verify",
        optionsData = Just (OptionsDataValues [
            OptionDataValueInteger "month" (Right month),
            OptionDataValueInteger "day" (Right day),
            OptionDataValueInteger "year" (Right year)])
            },
    interactionGuildId = Just guildId,
    interactionUser = MemberOrUser (Left DT.GuildMember {
        DT.memberUser = Just DT.User {
        DT.userId = userId,
        DT.userName = userName,
        DT.userGlobalName = userGlobalName
        },
        DT.memberNick = memberNick,
        DT.memberJoinedAt = memberJoinedAt})
  })) = do
    let maybeserver = Map.lookup guildId (servers config)
    server <- maybe (fail "No server in eventhandle for InvGroupResp.") return maybeserver
    verifyDOBRespond year month day server guildId userId userName userGlobalName memberNick memberJoinedAt interactionId interactionToken

eventHandle _ _ (DT.InteractionCreate (InteractionApplicationCommand{
    interactionId = interactionId,
    interactionToken = interactionToken,
    applicationCommandData =
        ApplicationCommandDataChatInput
        {
        applicationCommandDataName = "remove",
        optionsData = Just (OptionsDataValues [
            OptionDataValueUser "user" targetUserId
        ])
        },
    interactionGuildId = Just guildId,
    interactionUser = _userId
    })) = removeUserRespond guildId targetUserId interactionId interactionToken

eventHandle _ _ (DT.InteractionCreate (InteractionApplicationCommand{
    interactionId = interactionId,
    interactionToken = interactionToken,
    applicationCommandData =
        ApplicationCommandDataChatInput
        {
        applicationCommandDataName = "read",
        optionsData = Just (OptionsDataValues [
            OptionDataValueUser "user" targetUserId
        ])
        },
    interactionGuildId = Just guildId,
    interactionUser = _userId
    })) = readUserRespond guildId targetUserId interactionId interactionToken

eventHandle config cookies (DT.InteractionCreate (InteractionApplicationCommand{
    interactionId = interactionId,
    interactionToken = interactionToken,
    applicationCommandData =
        ApplicationCommandDataChatInput
        {
        applicationCommandDataName = "invite",
        optionsData = Just (OptionsDataValues [
            OptionDataValueString "usrid" (Right vrcUUID)
        ])
        },
    interactionGuildId = Just guildId,
    interactionUser = MemberOrUser (
        Left DT.GuildMember {
        DT.memberUser = Just DT.User {
        DT.userId = userId
        }}
    )
})) = do
    let maybeserver = Map.lookup guildId (servers config)
    server <- maybe (fail "No server in eventhandle for InvGroupResp.") return maybeserver
    (auth,_) <- liftIO (readIORef cookies)
    inviteToGroupRespond config vrcUUID guildId server userId auth interactionId interactionToken

eventHandle config cookies (DT.InteractionCreate (InteractionApplicationCommand
    {
    interactionId = interactionId,
    interactionToken = interactionToken,
    applicationCommandData =
        ApplicationCommandDataChatInput
        {
        applicationCommandDataName = "update"
        },
    interactionGuildId = Just guildId,
    interactionUser = MemberOrUser
    (
        Left DT.GuildMember
        {
        DT.memberUser = Just DT.User {
            DT.userName = name
        }
        }
    )
    }
  )) = forceUpdateRespond config guildId cookies name interactionId interactionToken

eventHandle config cookies (DT.InteractionCreate 
    (InteractionApplicationCommand
        {
            interactionId = interactionId,
            interactionToken = interactionToken,
            applicationCommandData = ApplicationCommandDataChatInput 
                {
                    applicationCommandDataName = "fix",
                    optionsData = Just 
                        (
                            OptionsDataValues values
                        )
                },
            interactionGuildId = Just guildId
        })) = do
            let maybeserver = Map.lookup guildId (servers config)
            server <- maybe (fail "No server in eventhandle for InvGroupResp.") return maybeserver
            (auth,_) <- liftIO (readIORef cookies)
            fixRespond config guildId server auth values interactionId interactionToken

eventHandle config cookies (DT.InteractionCreate (InteractionApplicationCommand{
    interactionId = interId,
    interactionToken = interToken,
    applicationCommandData = ApplicationCommandDataChatInput {applicationCommandDataName = "reinvite"},
    interactionUser = MemberOrUser (Left DT.GuildMember {DT.memberUser = Just DT.User {DT.userId = usrId}}),
    interactionGuildId = Just guildId
    })) = do
        (auth,_) <- liftIO (readIORef cookies)
        let maybeserver = Map.lookup guildId (servers config)
        server <- maybe (fail "No server in eventhandle for removeFromGroup.") return maybeserver
        reinviteRespond config usrId guildId server auth interId interToken

eventHandle config cookies (DT.GuildMemberRemove guildId (DT.User{DT.userId=userId})) = do
    (auth,_) <- liftIO (readIORef cookies)
    let maybeserver = Map.lookup guildId (servers config)
    server <- maybe (fail "No server in eventhandle for removeFromGroup.") return maybeserver
    removeFromGroup config server guildId userId auth


eventHandle _ _ _ = return ()
