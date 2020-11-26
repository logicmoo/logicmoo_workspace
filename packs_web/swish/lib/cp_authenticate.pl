/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2017, VU University Amsterdam
			      CWI, Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(cp_swish_authenticate,
	  [
	  ]).
:- use_module(library(pengines), []).
:- use_module(library(broadcast)).
:- use_module(user(user_db)).

/** <module> SWISH login management

This module provides basic login and  password management facilities for
SWISH.  You can create an authenticated SWISH server by

  1. Loading this library
  2. Add one or more users to the passwd file using swish_add_user/3

     ==
     ?- swish_add_user("Bob", "Bob's secret", []).
     ==

As a result, trying to create the  first pengine (e.g., using _|Run!|_),
the server will challenge the user.  The   logged  in  user is available
through pengine_user/1.
*/

:- multifile
    swish_config:reply_logged_in/1,
    swish_config:reply_logged_out/1,
    swish_config:authenticate/2,
    swish_config:user_info/3.

swish_config:authenticate(_Request, User) :-
    logged_on(User).

swish_config:user_info(_Request, local, Info) :-
    logged_on(User),
    cp_user_info(User, Info).

cp_user_info(User, Info) :-
    findall(Name-Value, cp_user_property(User, Name, Value), Pairs),
    dict_pairs(Info, u, Pairs).

cp_user_property(User, user, User).
cp_user_property(User, name, RealName) :-
    user_property(User, realname(RealName)).
cp_user_property(User, email, Email) :-
    user_property(User, email(Email)).
cp_user_property(User, group, Group) :-
    user_property(User, allow(Allow)),
    (   memberchk(admin(_), Allow)
    ->  Group = admin
    ;   memberchk(write(_,_), Allow)
    ->  Group = writer
    ;   Group = reader
    ).

:- listen(identity_property(Identity, Property),
          cp_identity_property(Identity, Property)).

cp_identity_property(Identity, Property) :-
    _{user:User, identity_provider:local} :< Identity,
    Property =.. [Name,Value],
    cp_user_property(User, Name, Value).


		 /*******************************
		 * LINK LOGIN/LOGOUT TO PROFILE *
		 *******************************/

:- listen(cliopatria(login(User, _Session)),
          cp_logged_in(User)).
:- listen(cliopatria(logout(User)),
          cp_logged_out(User)).

cp_logged_in(User) :-
    cp_user_info(User, Info),
    IdInfo = Info.put(_{identity_provider:local, external_identity:User}),
    swish_config:reply_logged_in([user_info(IdInfo), reply(none)]).

cp_logged_out(_User) :-
    swish_config:reply_logged_out([reply(none)]).


:- if((false ,exists_source(cliopatria(hooks)))).

:- use_module(cliopatria(hooks)).

/** <module> Add Prolog interaction to ClioPatria
*/

:- multifile
        user:file_search_path/2,
        swish_config:config/2.

% tell SWISH where to find its parts.   The last clause allows adding an
% =examples=  directory  in  the  main   directory  holding  application
% specific examples.

user:file_search_path(swish_web, web(.)).
user:file_search_path(example,   cpacks(swish/examples)).
user:file_search_path(example,   examples).
user:file_search_path(library,   cpacks(.)).

% Load swish.  You need this.
:- use_module(applications(swish)).
% Load the authentication hook. When loaded, ClioPatria users with admin
% rights can use SWISH without sandboxing security
%:- use_module(swish('lib/cp_authenticate')).
% Enable user profile management
:- use_module(swish(lib/plugin/profile)).
% Enable notifications
:- use_module(swish(lib/plugin/notify)).
% Enable logging of SWISH queries and sources if HTTP logging is enabled
:- use_module(swish(lib/logging)).

% Make side-effect-free RDF predicates safe
:- if(exists_source(library(semweb/rdf_sandbox))).
:- use_module(library(semweb/rdf_sandbox)).
:- endif.
% Make the R interface available.
% make sure Rserve runs in a good sandbox or only allow for
% authenticated access.  See https://github.com/JanWielemaker/rserve-sandbox
:- if(exists_source(library(r/r_call))).
:- use_module(user:swish(lib/r_swish)).
:- use_module(library(r/r_sandbox)).
:- endif.

% Allows users to extend the Examples menu by ticking the Example
% checkbox.
swish_config:config(community_examples, true).

% Uncomment this to make Captitalised words _atoms_.  Variables must
% be written as _var.  Requires SWI-Prolog 7.3.27.
%:- set_prolog_flag(swish:var_prefix, true).

%%      cliopatria:menu_item(-Item, -Label) is nondet.
%
%       Add SWISH to the Query menu.

cliopatria:menu_item(300=query/swish, 'SWISH Prolog shell').

:- endif.

