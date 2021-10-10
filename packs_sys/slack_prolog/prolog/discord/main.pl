:- if(current_prolog_flag(xref,true);(prolog_load_context(file,F),prolog_load_context(source,F))).
:- module(discord_main,[]).
:- endif.

discord_grouping(messages).
discord_grouping(channels).
discord_grouping(roles).
discord_grouping(members).
discord_grouping(presences).
discord_grouping(guilds).


% ===============================================
% How this module might find your token:
% ===============================================
% 0th - if the next line is uncommented and replaced by a real token 
% tmp:discord_token('xoxb-01234567890-xxxxxxxxxxxxxxxx').
% 1st - Checks for a local declaration 
find_token:- tmp:discord_token(_),!.
% 2nd - Checks for a local file called ".discord_auth.pl" for tmp:discord_token/1 as above
find_token:- exists_file('.discord_auth.pl'), consult('.discord_auth.pl'), !.
% 3rd - Checks env for DISCORD_API_TOKEN %  ( defined by# export DISCORD_API_TOKEN=xoxb-01234567890-xxxxxxxxxxxxxxxx )
find_token:- getenv('DISCORD_API_TOKEN',Was), asserta(tmp:discord_token(Was)), !.
% 4th - Checks users config directory for file called ".discord_auth.pl"  tmp:discord_token/1 as above
find_token:- expand_file_name('~/.discord_auth.pl',[X]), exists_file(X), consult(X), !.
find_token:- throw(missing(tmp:discord_token(_))).

:- find_token.


discord_update(me):-  discord_http(users/'@me').
discord_update(guilds):- discord_update(me), discord_http(users/'@me'/guilds).
discord_update(channels):- discord_update(guilds),
 forall(discord_ddd(GuildID,instanceOf,guilds),
  (discord_http(guilds/GuildID),
   discord_http(guilds/GuildID/channels),
   %discord_http(guilds/{guilds-id}/members),
   %discord_http(guilds/{guilds-id}/roles),
   true
   )).


/*
request_members:- 
discord_send({
  "op": 8,
  d: {msg

    "guild_id": $guild_id,
    "query": "",
    "limit": 0
  }
}).
*/

% Poll for any existing DMs
rtrv_dm_handles:-  forall(discord_ddd(UserID,instanceOf,members), rtrv_dm_handle(UserID)).

rtrv_dm_handle(UserID):-
 %discord_ddd(UserID,instanceOf,members),
 notrace(( 
   ignore((
    \+ discord_ddd(UserID,bot,true),
    % check that we are missing the info
    \+ (discord_ddd(ID,recipient,UserID),
        discord_ddd(ID,recipient_name,_Name),
        discord_ddd(ID,type,1),
        discord_ddd(ID,instanceOf,channels)),
 % actually retrieve it
 rtrv_dm_handle_now(UserID))))).

rtrv_dm_handle_now(UserID):- integer(UserID), !, 
  (discord_http(users/'@me'/channels,[post(json(_{recipient_id:UserID}))]) -> true ;
   show_discord_info_raw(UserID)).
rtrv_dm_handle_now(Name):- maybe_into_string(Name,SName), !, rtrv_dm_handle_now(SName).
rtrv_dm_handle_now(Name):- discord_name_id_type(Name,ID,members),integer(ID),!,rtrv_dm_handle_now(ID).
rtrv_dm_handle_now(Name):- from_string(Name,ID),integer(ID),!,rtrv_dm_handle_now(ID).


discord_me(Self):-discord_dd('@me', id, Self).

get_emojis:- discord_http(guilds/{guilds-id}/emojis).

default_guild(GuildID):- discord_ddd(GuildID,instanceOf,guilds).
default_guild(748871194572226661).
default_guild("GUILD_CREATE").


%:- autoload_all.
discord_restore_1:-
 without_ddbg((
  bot_token_string(TokenHeaderB), discord_add(bot_token,TokenHeaderB),
  discord_token_string(TokenHeader), discord_add(token,TokenHeader),
  get_time(Time), ITime is integer(Time), discord_add(time,ITime))).

:- if( \+ prolog_load_context(reloading, true)).
:- discord_restore_1.
:- endif.
:- initialization(discord_restore_1).

discord_restore_2:- discord_connect.
:- initialization(discord_restore_2).

:- if( \+ prolog_load_context(reloading, true)).
:- prolog_load_context(file,File), forall((source_file(PP,File),strip_module(PP,M,P),functor(P,F,0)),add_history(M:F)).
:- endif.


% start discord gateway in a thread
:- initialization(discord_start_gateway).
% dequee discord events in a thread
:- deque_discord_events.
% start discord pinger in a thread
%:- ping_discord.
% start discord message checker in a thread 
:- initialization(discord_proc_tasks).




end_of_file.


https://discord.com/api/oauth2/authorize?client_id=772113231757574185&scope=bot&permissions=1

Name=logicmoo-app
USERNAME=PrologMUD#4124
PERMISSION=536536415425
Public=63920eac424255a5a57fc6b48a7c958c46621ab18505506b1d3fa32f3708e638
Client=772113231757574185
Channel=892809238710222930
Token=


curl -v -H "Authorization: Bot $AUTH_TOK" -H "User-Agent: curl" -H "Content-Type: application/json" -XGET https://discord.com/api/v9/channels/892809238710222930/messages
curl -v -H "Authorization: $AUTH_TOKEN" -H "User-Agent: DiscordBot" -H "Content-Type: application/json" -H "Content-Length: 0" -X GET https://discord.com/api/channels/892809238710222930/messages

{
  "token_type": "Bearer",
  "access_token": "GNaVzEtATqdh173tNHEXY9ZYAuhiYxvy",
  "scope": "webhook.incoming",
  "expires_in": 604800,
  "refresh_token": "PvPL7ELyMDc1836457XCDh1Y8jPbRm",
  "webhook": {
    "application_id": "310954232226357250",
    "name": "testwebhook",
    "url": "https://discord.com/api/webhooks/347114750880120863/kKDdjXa1g9tKNs0-_yOwLyALC9gydEWP6gr9sHabuK1vuofjhQDDnlOclJeRIvYK-pj_",
    "channel_id": "345626669224982402",
    "token": "kKDdjXa1g9tKNs0-_yOwLyALC9gydEWP6gr9sHabuK1vuofjhQDDnlOclJeRIvYK-pj_",
    "type": 1,
    "avatar": null,
    "guild_id": "290926792226357250",
    "id": "347114750880120863"
  }
}

curl -v -H "Authorization: XXX" -H "User-Agent: curl" -H "Content-Type: application/json" -H "Content-Length: 0" -X GET https://discord.com/api/v9/channels/892809238710222930/messages


From this object, you


https://discord.com/api/oauth2/authorize?response_type=code&client_id=772113231757574185&scope=webhook.incoming&state=15773059ghq9183habn&redirect_uri=https%3A%2F%2Fnicememe.website



 {
    "application": {
        "id": "772113231757574185",
        "name": "AIRHORN SOLUTIONS",
        "icon": "f03590d3eb764081d154a66340ea7d6d",
        "description": "",
        "summary": "",
        "hook": true,
        "bot_public": true,
        "bot_require_code_grant": false,
        "verify_key": "c8cde6a3c8c6e49d86af3191287b3ce255872be1fff6dc285bdb420c06a2c3c8"
    },
    "scopes": [
        "guilds.join",
        "identify"
    ],
    "expires": "2021-01-23T02:33:17.017000+00:00",
    "user": {
        "id": "268473310986240001",
        "username": "Discord",
        "avatar": "f749bb0cbeeb26ef21eca719337d20f1",
        "discriminator": "0001",
        "public_flags": 131072
    }
}



// This is a message
{
    "content": "Mason is looking for new arena partners. What classes do you play?",
    "components": [
        {
            "type": 1,
            "components": [
                {
                    "type": 3,
                    "custom_id": "class_select_1",
                    "options":[
                        {
                            "label": "Rogue",
                            "value": "rogue",
                            "description": "Sneak n stab",
                            "emoji": {
                                "name": "rogue",
                                "id": "625891304148303894"
                            }
                        },
                        {
                            "label": "Mage",
                            "value": "mage",
                            "description": "Turn 'em into a sheep",
                            "emoji": {
                                "name": "mage",
                                "id": "625891304081063986"
                            }
                        },
                        {
                            "label": "Priest",
                            "value": "priest",
                            "description": "You get heals when I'm done doing damage",
                            "emoji": {
                                "name": "priest",
                                "id": "625891303795982337"
                            }
                        }
                    ],
                    "placeholder": "Choose a class",
                    "min_values": 1,
                    "max_values": 3
                }
            ]
        }
    ]
}

