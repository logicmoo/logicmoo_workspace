:- if(current_prolog_flag(xref,true);(prolog_load_context(file,F),prolog_load_context(source,F))).
:- module(discord_objs,[]).
:- endif.

:- if(exists_source(library(udt))).
    :- use_module(library(udt)).

:- oo_class_begin(discord_client).

% url	A WebSocket Message Server URL.
:- oo_class_field(url).

% '@me'	The authenticated bot user.
:- oo_inner_class_begin(clients).

discord_client:clients:new(Ref):- throw(clients:new(Ref)).

:- oo_inner_class_end(clients).



% '@me'	The authenticated bot user.
:- oo_inner_class_begin('@me').
:- oo_inner_class_end('@me').

% guild	Details on the authenticated user's guild.
:- oo_inner_class_begin(guild).
:- oo_inner_class_end(guild).

% users	A hash of user objects by user ID.
:- oo_inner_class_begin(users).
:- oo_inner_class_end(users).


% channels	A hash of channel objects, one for every channel visible to the authenticated user.
:- oo_inner_class_begin(channels).
:- oo_inner_class_end(channels).

% roles	A hash of role objects, one for every role the authenticated user is in.
:- oo_inner_class_begin('@me').
:- oo_inner_class_end('@me').

% ims	A hash of IM objects, one for every direct message channel visible to the authenticated user.
:- oo_inner_class_begin(chats).
:- oo_inner_class_end(chats).

% integrations	Details of the integrations set up on this guild.
:- oo_inner_class_begin(integrations).
:- oo_inner_class_end(integrations).

% text	textual utils.
:- oo_inner_class_begin(text).
:- oo_inner_class_end(text).

% debug	Debugger fidling.
:- oo_inner_class_begin(debug).
:- oo_inner_class_end(debug).

% events	Registered callbacks.
:- oo_inner_class_begin(events).
:- oo_inner_class_end(events).

% files	registered storage.
:- oo_inner_class_begin(files).
:- oo_inner_class_end(files).

:- oo_class_end(discord_client).

:- endif.


