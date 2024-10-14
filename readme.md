# The Bouncer Bot
*Disclaimer: I am a newbie Programmer and this is my first solo project. Expect imperfect code! Any tips/advice/suggestions are greatly appreaciated! 
## About
The Bouncer Bot is a Discord bot that interfaces with VRChat's API to provide access to Group Managent Tools through Discord's Slash Commands.

The Bot is intended for use in 18+ communities with its ability to perform simple age verification and grant roles upon verification. 
The bot also provides a way to manage VRChat Group Invites and Kicks through Discord.


## Features

### Age Verification
Each user can verify their age by providing their date of birth. 
The Bot will then check if the user is 18+ and grant them a designated role if they are. 
The Bot will also record the verification in a JSON file, for the purpose of resolving potential future issues/disputes.

### Group Management
The Bot enables users to invite themselves to a VRChat group by providing their VRChat UUID, which can be found on the VRChat website. 
The Bot will then send a Group Invite to the user in VRChat.

The Bot will also record which UUID was requested to be invited, and by whom, in the JSON file. 
This is to make it so each Discord user can only invite one VRChat account to the group.

The Bot also will ensure that users stay in the Discord server by kicking users from the VRChat group if they leave the Discord server. 
This was intentional so that users can't just join the Discord server, invite themselves to the VRChat group, and then leave the server, making them unreachable should any issues on the VRChat side arise.

### Group Stats/Instances Display
The Bot can create a display using voice channels indicating the total number of Group members as well as those currently active in the VRChat Group. 
The Bot will also display any open Group Instances and the number of users in each instance. 
The Bot is also set to update the display every 30 minutes, and there is a command to manually update the display.


## Commands

* `/verify` - Verify your age by providing your date of birth.
* `/invite` - Invite yourself to the VRChat group by providing your VRChat UUID.
* `/reinvite` - Reinvite the UUID associated with the users Discord to the VRChat group if they have left the group/discord.
* `/fix` - Allows correction of the VRC UUID or DOB of a provided user.
* `/read` - Outputs the JSON entry for a provided user.
* `/remove` - Removes the JSON entry for a provided user.
* `/update` - Manually update the Group Stats/Instances display.


## Requirements
* Haskell (Developed on GHC 9.4.8)
* Discord API Token and Application ID
* VRChat Account
* Discord Server
* VRChat Group

## Setup
* Windows
    1. Download the latest release from the releases page.
    2. Set up the configGlobal.json file. (See below)
    3. Run the executable.
* Linux
    1. Clone the repository.
    2. Set up the configGlobal.json file. (See below)
    3. Run `cabal build` in the project directory.

### configGlobal.example

The configGlobal.example file is a json file used to store the Bot's Token, Application ID, and the IDs of the Discord Servers the Bot will be operating in.
There should be the configGlobal.example file in the top level of the project directory if you are building from source, or in the same directory as the executable if you are using a release.

The file should look like this:
```json
{
    "authToken": "",
    "applicationId": "",
    "userAgent": "",
    "username": "",
    "password": "",
    "authSecret": "",
    "usrId": "",
    "servers": {
        "55555GUILD-ID-HERE5555": {
            "guildName": "",
            "displayMembersChannel": "",
            "displayOnlineMembersChannel": "",
            "botOutputChannel": "",
            "instancesCategory": "",
            "vrcGroupId": "",
            "verifiedRole": "",
            "memberRole": ""
        }
    }
}   
```

They should for the most part be self-explanatory, but here is a brief explanation of what should go in each field (inside each of the ""):

* `authToken` - The Bot's Discord API Token.
* `applicationId` - The Bot's Discord Application ID.
* `userAgent` - The Bot's User Agent. (BotName/Version email-address-of-bot-owner@email.com)
* `username` - The Bot's VRChat Username.
* `password` - The Bot's VRChat Password.
* `authSecret` - The Bot's VRChat Auth Secret for 2FA.
* `usrId` - The Bot's VRChat UUID. (Can be found on the VRChat website) 
* `servers` - A list of Discord Servers the Bot will be operating in. (Each server should have its own entry, with the Guild ID as the key)
    * `guildName` - The name of the Discord Server. (This is just for reference)
    * `displayMembersChannel` - The ID of the Voice Channel where the total number of Group Members will be displayed.
    * `displayOnlineMembersChannel` - The ID of the Voice Channel where the number of Group Members currently online will be displayed.
    * `botOutputChannel` - The ID of the Text Channel where the Bot will output messages.
    * `instancesCategory` - The ID of the Category where the Group Instances will be displayed.
    * `vrcGroupId` - The ID of the VRChat Group.
    * `verifiedRole` - The ID of the Role that will be granted to users who use the bot to verify their age.
    * `memberRole` - The ID of the Role that will be granted to users who use the bot to invite themselves to the VRChat Group.
 
And after you fill all of those in, save your changes and rename the file from configGlobal.example to configGlobal.json. Next create a folder called "Guilds" next to "source", which is where the data of each user that registers with the bot in each server will be stored.
