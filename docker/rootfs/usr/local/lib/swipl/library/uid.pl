/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2013, VU University Amsterdam
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

:- module(uid,
          [ getuid/1,                   % -UID
            getgid/1,                   % -GID
            geteuid/1,                  % -UID
            getegid/1,                  % -GID
            getgroups/1,                % -GIDs
            user_info/2,                % +User, -UserInfo
            group_info/2,               % +Group, -GroupInfo
            user_data/3,                % +Field, +UserInfo, -Value
            group_data/3,               % +Field, +GroupInfo, -Value
            setuid/1,                   % +UID
            setgid/1,                   % +GID
            seteuid/1,                  % +UID
            setegid/1,                  % +GID

            set_user_and_group/1,       % +User
            set_user_and_group/2        % +User, +Group
          ]).

:- use_foreign_library(foreign(uid)).

:- if(predicate_property(initgroups(_,_), defined)).
:- export(initgroups/2).
:- else.
initgroups(_,_).
:- endif.

:- if(predicate_property(setgroups(_), defined)).
:- export(setgroups/1).
:- endif.

/** <module> User and group management on Unix systems

This module provides and interface  to   user  and  group information on
Posix systems. In addition, it allows for   changing user and group ids.
When changing user and group settings for   the calling process, bear in
mind that:

  - Changing user and groups of the calling process requires permission.
  - The functions setgroups() and initgroups() are not part of the
    POSIX standard and therefore the derived predicates may not be
    present.

@see    Please check the documentation of your OS for details on the
        semantics of this predicates.
*/

%!  getuid(-UID) is det.
%
%   UID is the real user ID of the calling process.

%!  getgid(-GID) is det.
%
%   GID is the real group ID of the calling process.

%!  geteuid(-UID) is det.
%
%   UID is the effective user ID of the calling process.

%!  getegid(-GID) is det.
%
%   GID is the effective group ID of the calling process.

%!  getgroups(-GroupsIDs:list(integer)) is det.
%
%   GroupsIDs is the set of supplementary   group IDs of the calling
%   process.  Note  that  these   are    numeric   identifiers.  Use
%   group_info/2  to  obtain  details   on    the   returned   group
%   identifiers.

%!  user_info(+User, -UserData) is det.
%
%   UserData represent the passwd  information   for  User.  User is
%   either a numeric UID or a   user name. The predicate user_data/3
%   can be used to extract information from UserData.

%!  user_data(?Field, ?UserData, ?Value)
%
%   Value is the value for Field in UserData.  Defined fields are:
%
%     * name
%     Name of the user
%     * password
%     Password hash of the user (or =x= if this is not accessible)
%     * uid
%     Numeric user id of the user
%     * gid
%     Numeric primary group id of the user
%     * comment
%     The _gecos_ field
%     * home
%     Home directory of the user
%     * shell
%     Default (login) shell of the user.

user_data(name,     user_info(Nam, _, _, _, _, _, _), Nam).
user_data(password, user_info(_, PWD, _, _, _, _, _), PWD).
user_data(uid,      user_info(_, _, UID, _, _, _, _), UID).
user_data(gid,      user_info(_, _, _, GID, _, _, _), GID).
user_data(comment,  user_info(_, _, _, _, GEC, _, _), GEC).
user_data(home,     user_info(_, _, _, _, _, HOM, _), HOM).
user_data(shell,    user_info(_, _, _, _, _, _, SHE), SHE).

%!  group_info(+Group, -GroupData) is det.
%
%   GroupData represent the group information   for  Group. Group is
%   either a numeric GID or a group name. The predicate group_data/3
%   can be used to extract information from GroupData.

%!  group_data(?Field, ?GroupData, ?Value)
%
%   Value is the value for Field  GroupData.  Defined fields are:
%
%     * name
%     Name of the user
%     * password
%     Password hash of the user (or =x= if this is not accessible)
%     * gid
%     Numeric group id of the group
%     * members
%     List of user-names that are member of this group.

group_data(name,     group_info(Nam, _, _, _), Nam).
group_data(password, group_info(_, PWD, _, _), PWD).
group_data(gid,      group_info(_, _, GID, _), GID).
group_data(members,  group_info(_, _, _, MBR), MBR).

                 /*******************************
                 *             SETTING          *
                 *******************************/

%!  setuid(+UID)
%
%   Set the user id of the calling process.

%!  seteuid(+UID)
%
%   Set the effective user id of the calling process.


%!  setgid(+GID)
%
%   Set the group id of the calling process.

%!  setegid(+GID)
%
%   Set the effective group id of the calling process.

%!  initgroups(+User, +Group) is det.
%
%   Initialise the group access list of   the calling process to the
%   registered groups for User and the   group Group. This predicate
%   is only available if the underlying OS provides it.

%!  setgroups(+Groups:list(integer)) is det.
%
%   Set the group access list of the caling process to the indicated
%   groups. This predicate is only available   if  the underlying OS
%   provides it.

%!  set_user_and_group(+User) is det.
%!  set_user_and_group(+User, +Group) is det.
%
%   Set the UID and GID to the User. User  is either a UID or a user
%   name. If Group is not specified, the   primary  group of User is
%   used. If initgroups/2 is available,   the resulting group access
%   list of the calling process consists   of  the registered groups
%   for User and the specified Group.

set_user_and_group(User) :-
    user_info(User, Data),
    user_data(uid, Data, UID),
    user_data(gid, Data, GID),
    initgroups(User, GID),
    setgid(GID),
    setuid(UID).

set_user_and_group(User, Group) :-
    user_info(User, Data),
    group_info(Group, GData),
    user_data(uid, Data, UID),
    user_data(gid, Data, UGID),
    group_data(gid, GData, GID),
    initgroups(User, UGID),
    setgid(GID),
    setuid(UID).
