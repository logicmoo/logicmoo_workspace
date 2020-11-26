:- module(impl_profile_prolog, []).

/** <module> User Profile backend

This module is a backend for user_profile.pl, realising ... The
properties of this profile are:

  - ...
*/

:- public
	impl_profile_open_db/1,
	impl_profile_create/2,
	impl_current_profile/1,
	impl_current_profile/2,
	impl_profile_property/2,
	impl_set_profile/3,
	impl_profile_remove/1,
	impl_profile_remove/2,
	impl_profile_add_session/3,
	impl_profile_remove_session/2,
	impl_profile_session/2,
	impl_profile_refresh_session/2.

%%	impl_profile_open_db(+Options)
%%	impl_profile_create(+ProfileID, +CanAttributes)
%%	impl_current_profile(?ProfileID)
%%	impl_current_profile(?ProfileID, ?Attributes)
%%	impl_profile_property(?ProfileID, ?Attribute)
%%	impl_set_profile(+ProfileID, +CanAttribute, -Modified)
%%	impl_profile_remove(+ProfileID)
%%	impl_profile_remove(+ProfileID, +Attribute)
%%	impl_profile_add_session(+ProfileID, +SessionID, +Options)
%%	impl_profile_remove_session(+ProfileID, +SessionID)
%%	impl_profile_session(?ProfileID, ?SessionID)
%%	impl_profile_refresh_session(+ProfileID, +SessionID)
