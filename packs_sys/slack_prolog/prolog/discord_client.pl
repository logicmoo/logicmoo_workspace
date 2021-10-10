:- module(discord_client, [
        discord_start_gateway/0,
        discord_say/2,
        discord_send/1,
        ping_discord/0,
        discord_get_websocket/1,
        is_thread_running/1,        
        discord_restore_0/0,
        discord_chat_override/1,
        any_to_id/2]).

/** <module> discord_client - Provides a websocket API to write discord clients

*/
:- use_module(library(logicmoo_utils)).

discord_restore_0:- tmp:discord_debug(_),!.
discord_restore_0:- 
  stream_property(X,file_no(2)),asserta(tmp:discord_debug(X)),
  notrace_catch(mutex_create(last_done_mutex)),
  notrace_catch(mutex_create(connection_mutex)).

:- initialization(discord_restore_0).

disable_gateway:- false.  % true for when websocket code is borken

:- dynamic(tmp:discord_token/1).

:- include(discord/util).
:- include(discord/task).
:- include(discord/find).
:- include(discord/data).
:- include(discord/conn).
:- include(discord/chat).
:- fixup_exports.
:- include(discord/main).


