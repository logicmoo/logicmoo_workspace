/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(user_profile,
	  [ profile_open_db/1,		% +Options

	    profile_create/2,		% ?ProfileId, +Attributes
	    current_profile/1,		% ?ProfileId
	    current_profile/2,		% ?ProfileId, -Attributes
	    profile_property/2,		% ?ProfileId, ?Attribute
	    set_profile/2,		% +ProfileId, +Property
	    set_profile/3,		% +ProfileId, +Property, -Modified
	    profile_remove/2,		% +ProfileId, +Property
	    profile_remove/1,		% +ProfileId

	    profile_add_session/3,	% +ProfileId, +SessionID, +Options
	    profile_remove_session/2,	% +ProfileId, +SessionID
	    profile_session/2,		% ?ProfileId, ?SessionID
	    profile_refresh_session/2,	% +ProfileId, +SessionID

	    profile_canonical_value/3	% +Attribute, +Value0, -Value
	  ]).
:- use_module(library(uuid)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(uri)).
:- use_module(library(lists)).

/** <module> User Profile Management

This module implements  user  profile   management,  in  particular  for
managing authentication and authorization for   HTTP  servers. It mainly
defines the interface that can be used within an HTTP application.

The  actual  storage  is  left  to    a  plugin  providing  the  backend
implementation. Backend choices may  depend   on  integration needs with
other services, scale of the site  (number of users), distribution, ease
of installation.

The typical setup sequence is

```
:- use_module(library(http/user_profile)).
:- use_module(library(http/impl/profile_prolog)).
:- set_setting(user_profile:backend, impl_profile_prolog).

:- multifile
	user_profile:attribute/3.

user_profile:attribute_type(name, string, []).
...

```
*/

:- multifile
	attribute/3.			% ?Attribute, ?Type, ?Options

:- setting(backend, atom, user_profile_prolog,
	   "Backend to use (name of the module").
:- setting(session_timeout, number, 900,
	   "Default timeout for session based logins").
:- setting(session_persistency, boolean, false,
	   "Default session persistency handling").


		 /*******************************
		 *	      DATABASE		*
		 *******************************/

%%	profile_open_db(+Options) is det.
%
%	Open the profile database. Must  be   called  before  any of the
%	other  profile  API  predicates.  Options  depend  on  the  used
%	backend.

profile_open_db(Options) :-
	setting(backend, Backend),
	Backend:impl_profile_open_db(Options).


		 /*******************************
		 *	       CREATE		*
		 *******************************/

%%	profile_create(?ProfileID, +Attributes) is det.
%
%	Create a new user profile with the given initial attributes.
%
%	@arg	Attributes is a list of Name(Value) terms.

profile_create(ProfileID, Attributes) :-
	instantiate_profile_id(ProfileID),
	maplist(typecheck_attribute, Attributes, CanAttributes),
	(   current_profile(ProfileID)
	->  permission_error(redefine, user_profile, ProfileID)
	;   true
	),
	setting(backend, Backend),
	Backend:impl_profile_create(ProfileID, CanAttributes).

instantiate_profile_id(ProfileID) :-
	var(ProfileID), !,
	uuid(ProfileID).
instantiate_profile_id(ProfileID) :-
	must_be(atom, ProfileID).

typecheck_attribute(Term, Canonical) :-
	attribute_nv(Term, Name, Value0),
	profile_canonical_value(Name, Value0, Value),
	Canonical =.. [Name,Value].

%!	profile_canonical_value(+Attribute, +ValueIn, -Value) is det.
%
%	True when Value is  the  canonical   value  for  Attribute  that
%	satisfies the type constraint for Attribute.
%
%	@error type_error(Type, ValueIn) if the type is wrong
%	@error existence_error(profile_attribute, Attribute) if the
%	       attribute is unknown.

profile_canonical_value(Name, Value0, Value) :-
	(   attribute(Name, Type, _)
	->  must_be(ground, Type),
	    (   convert_attribute_value(Type, Value0, Value)
	    ->	true
	    ;	Value = Value0,
		must_be(Type, Value)
	    )
	;   existence_error(profile_attribute, Name)
	).

%!	convert_attribute_value(+Type, +Input, -Value)
%
%	True when Value is the result of converting Input to Type.

convert_attribute_value(Type, Text, String) :-
	string_value(Type),
	text(Text), !,
	atom_string(Text, String).
convert_attribute_value(float, Int, Float) :-
	integer(Int),
	Float is float(Int).
convert_attribute_value(string, ip(A,B,C,D), String) :-
	format(string(String), '~w.~w.~w.~w', [A,B,C,D]).
convert_attribute_value(oneof(Values), Text, Value) :-
	member(Value, Values),
	string_value(Text, Value), !.

string_value(string).
string_value(url).
string_value(url(_Scheme)).
string_value(email).

string_value(Value, Value) :- !.
string_value(String, Value) :-
	atom(Value),
	atom_string(Value, String), !.
string_value(String, Value) :-
	number(Value),
	number_string(String, Value1),
	Value1 =:= Value.

text(T) :- atom(T), !.
text(T) :- string(T), !.

attribute_nv(Term, _Name, _Value) :-
	var(Term), !,
	instantiation_error(Term).
attribute_nv(Term, Name, Value) :-
	compound(Term),
	compound_name_arguments(Term, Name, [Value]), !.
attribute_nv(Name = Value, Name, Value) :- !,
	must_be(atom, Name).
attribute_nv(Name - Value, Name, Value) :- !,
	must_be(atom, Name).
attribute_nv(Term, _Name, _Value) :-
	type_error(name_value, Term).


		 /*******************************
		 *	       QUERY		*
		 *******************************/

%%	current_profile(?ProfileID) is nondet.
%
%	True when ProfileID is a currently known user profile.

current_profile(ProfileID) :-
	setting(backend, Backend),
	Backend:impl_current_profile(ProfileID).

%%	current_profile(?ProfileID, -Attributes:dict) is nondet.
%
%	True when ProfileID is a currently   known user profile with the
%	given attributes.

current_profile(ProfileID, Attributes) :-
	setting(backend, Backend),
	Backend:impl_current_profile(ProfileID, Attributes0),
	add_defaults(Attributes0, Attributes).

add_defaults(Attributes0, Attributes) :-
	findall(Name-Value, default_attribute(Name, Value), Pairs),
	Pairs \== [], !,
	dict_pairs(Defaults, user_profile, Pairs),
	Attributes = Defaults.put(Attributes0).
add_defaults(Attributes, Attributes).

default_attribute(Name, Value) :-
	attribute(Name, _Type, Options),
	memberchk(default(Value), Options).


%%	profile_property(?ProfileID, ?Property:compound) is nondet.
%
%	True when the user with ProfileID   has  Property. Property is a
%	term Name(Value).

profile_property(ProfileID, Property) :-
	nonvar(ProfileID),
	nonvar(Property), !,
	attribute_nv(Property, Name, Value),
	setting(backend, Backend),
	(   VarP =.. [Name,Value0],
	    Backend:impl_profile_property(ProfileID, VarP)
	->  Value = Value0
	;   default_attribute(Name, Value)
	).
profile_property(ProfileID, Property) :-
	setting(backend, Backend),
	Backend:impl_profile_property(ProfileID, Property).


		 /*******************************
		 *	       UPDATE		*
		 *******************************/

%%	set_profile(+ProfileID, +Attribute) is det.
%%	set_profile(+ProfileID, +Attribute, -Modified) is det.
%
%	Set an attribute of the profile.
%
%	@arg Attribute is a term Name(Value)
%	@arg Modified is unified with a boolean, indicating whether
%	     or not the value was modified.

set_profile(ProfileID, Attribute) :-
	set_profile(ProfileID, Attribute, _).

set_profile(ProfileID, Attribute, Modified) :-
	must_be(atom, ProfileID),
	typecheck_attribute(Attribute, CanAttribute),
	setting(backend, Backend),
	Backend:impl_set_profile(ProfileID, CanAttribute, Modified).

%%	profile_remove(+ProfileID) is det.
%
%	Completely destroy a profile.

profile_remove(ProfileID) :-
	must_be(atom, ProfileID),
	setting(backend, Backend),
	Backend:impl_profile_remove(ProfileID).

%%	profile_remove(+ProfileID, +Attribute) is det.
%
%	Remove an attribute from a profile.

profile_remove(ProfileID, Attribute) :-
	must_be(atom, ProfileID),
	must_be(atom, Attribute),
	setting(backend, Backend),
	Backend:impl_profile_remove(ProfileID, Attribute).


		 /*******************************
		 *	SESSION MANAGEMENT	*
		 *******************************/

%%	profile_add_session(+ProfileID, +SessionID, +Options) is det.
%
%	Associate a profile with a session (login). Options defined are:
%
%	  - timeout(+Seconds)
%	  Max idle time for the session.
%	  - persistent(+Boolean)
%	  If `true`, store the session association persistently, such
%	  that a server restart maintains the login.

profile_add_session(ProfileID, SessionID, Options) :-
	must_be(atom, ProfileID),
	must_be(atom, SessionID),
	setting(session_timeout, DefTimeOut),
	setting(session_persistency, DefPresistency),
	option(timeout(TimeOut), Options, DefTimeOut),
	option(persistent(Persistent), Options, DefPresistency),
	local_add_session(ProfileID, SessionID,
			  [ timeout(TimeOut),
			    persistent(Persistent)
			  ]).

%%	profile_refresh_session(+ProfileID, +SessionID) is det.
%
%	Update the last access time for the indicated session.

profile_refresh_session(ProfileID, SessionID) :-
	must_be(atom, ProfileID),
	must_be(atom, SessionID),
	local_refresh_session(ProfileID, SessionID).

%%	profile_remove_session(+ProfileID, +SessionID) is det.
%
%	Remove the association of a profile with a session (logout).

profile_remove_session(ProfileID, SessionID) :-
	must_be(atom, ProfileID),
	must_be(atom, SessionID),
	local_remove_session(ProfileID, SessionID).

%%	profile_session(?ProfileID, ?SessionID) is nondet.
%
%	True when ProfileID is associated (logged in) with SessionID.

profile_session(ProfileID, SessionID) :-
	local_session(ProfileID, SessionID).


		 /*******************************
		 *	  LOCAL SESSIONS	*
		 *******************************/

:- dynamic
	tmp_session/3,			% ProfileID, SessionID, DeadLine
	session_last_usage/2.		% SessionID, Time
:- volatile
	tmp_session/3,
	session_last_usage/2.

local_add_session(ProfileID, SessionID, Options) :-
	option(persistent(false), Options), !,
	option(timeout(Timeout), Options),
	get_time(Now),
	asserta(tmp_session(ProfileID, SessionID, Timeout)),
	asserta(session_last_usage(SessionID, Now)).
local_add_session(ProfileID, SessionID, Options) :-
	setting(backend, Backend),
	Backend:impl_profile_add_session(ProfileID, SessionID, Options).

local_refresh_session(ProfileID, SessionID) :-
	tmp_session(ProfileID, SessionID, _Timeout), !,
	get_time(Now),
	retractall(session_last_usage(SessionID, _)),
	asserta(session_last_usage(SessionID, Now)).
local_refresh_session(ProfileID, SessionID) :-
	setting(backend, Backend),
	Backend:impl_profile_refresh_session(ProfileID, SessionID).

local_remove_session(ProfileID, SessionID) :-
	retract(tmp_session(ProfileID, SessionID, _)), !.
local_remove_session(ProfileID, SessionID) :-
	setting(backend, Backend),
	Backend:impl_profile_remove_session(ProfileID, SessionID).

local_session(ProfileID, SessionID) :-
	var(ProfileID), var(SessionID), !,
	(   tmp_session(_, SessionID, _),
	    local_session(ProfileID, SessionID)
	;   setting(backend, Backend),
	    Backend:impl_profile_session(ProfileID, SessionID)
	).
local_session(ProfileID, SessionID) :-
	tmp_session(ProfileID, SessionID, TimeOut), !,
	session_last_usage(SessionID, LastUsage),
	get_time(Now),
	(   LastUsage+TimeOut < Now
	->  true
	;   retractall(tmp_session(ProfileID, SessionID, _)),
	    retractall(session_last_usage(SessionID, _)),
	    fail
	).
local_session(ProfileID, SessionID) :-
	setting(backend, Backend),
	Backend:impl_profile_session(ProfileID, SessionID).


		 /*******************************
		 *	      TYPES		*
		 *******************************/

:- multifile error:has_type/2.

%!	error:has_type(+Type, +Value) is semidet.
%
%	True if Value satisfies Type.   This  implementation extends the
%	type logic defined  in  library(error)   with  some  types  that
%	commonly apply to user profiles.
%
%	@tbd: extend with e.g., zip, country, phone, date

error:has_type(url(http), URI) :-
	string(URI),
	uri_components(URI, Components),
	valid_http_scheme(Components),
	valid_authority(Components).
error:has_type(email, Email) :-
	string(Email),
	split_string(Email, "@", "", [_,_]).
error:has_type(time_stamp(_Format), Stamp) :-
	number(Stamp).

valid_http_scheme(Components) :-
	uri_data(scheme, Components, Scheme),
	nonvar(Scheme),
	http_scheme(Scheme).

http_scheme(http).
http_scheme(https).

valid_authority(Components) :-
	uri_data(authority, Components, Authority),
	nonvar(Authority).


		 /*******************************
		 *	      HOOKS		*
		 *******************************/

%%	attribute(?Attribute, ?Type, ?Options) is nondet.
%
%	Multifile hook that defines that the profile attribute Attribute
%	must have the type Type. Type are  types as defined by must_be/2
%	from library(error).  Options defined are:
%
%	  - access(+Access)
%	  Defines whether or not the user can update the attribute
%	  value. Access is one of `rw` (default) or `ro`.
%	  - hidden(+Boolean)
%	  If `true`, the attribute is not displayed in the user
%	  profile.
%	  - default(+Value)
%	  Assumed default if the value is unknown.
