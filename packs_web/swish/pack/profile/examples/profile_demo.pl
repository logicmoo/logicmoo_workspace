:- use_module(library(settings)).
:- use_module(library(user_profile)).
:- use_module(library(profile/backend/profile_prolog)).

:- set_setting(user_profile:backend, impl_profile_prolog).

user_profile:attribute_type(name,        string).
user_profile:attribute_type(given_name,  string).
user_profile:attribute_type(family_name, string).
user_profile:attribute_type(email,       email).
user_profile:attribute_type(picture,     url(http)).
user_profile:attribute_type(home_page,   url(http)).

:- initialization
	profile_open_db([]).
