/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2015, University of Amsterdam
                              VU University Amsterdam
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

:- module(authenticate,
          [ http_authenticate/3,        % +Check, +Header, -User
            http_authorization_data/2,  % +AuthorizationText, -Data
            http_current_user/3,        % +File, ?User, ?Fields

            http_read_passwd_file/2,    % +File, -Data
            http_write_passwd_file/2    % +File, +Data
          ]).
:- use_module(library(base64)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(crypt)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(apply)).

/**     <module> Authenticate HTTP connections using 401 headers

This module provides the basics  to   validate  an  HTTP =Authorization=
header. User and password  information  are   read  from  a  Unix/Apache
compatible password file.

This  library  provides,  in  addition    to  the  HTTP  authentication,
predicates to read and write password files.
*/

%!  http_authenticate(+Type, +Request, -Fields)
%
%   True if Request contains the   information to continue according
%   to Type. Type identifies the required authentication technique:
%
%           * basic(+PasswordFile)
%           Use HTTP =Basic= authetication and verify the password
%           from PasswordFile. PasswordFile is a file holding
%           usernames and passwords in a format compatible to
%           Unix and Apache. Each line is record with =|:|=
%           separated fields. The first field is the username and
%           the second the password _hash_.  Password hashes are
%           validated using crypt/2.
%
%   Successful authorization is  cached  for   60  seconds  to avoid
%   overhead of decoding and lookup of the user and password data.
%
%   http_authenticate/3 just validates the  header. If authorization
%   is not provided the browser must   be challenged, in response to
%   which it normally opens a   user-password dialogue. Example code
%   realising this is below. The exception   causes the HTTP wrapper
%   code to generate an HTTP 401 reply.
%
%   ==
%   (   http_authenticate(basic(passwd), Request, Fields)
%   ->  true
%   ;   throw(http_reply(authorise(basic, Realm)))
%   ).
%   ==
%
%   @param  Fields is a list of fields from the password-file entry.
%           The first element is the user.  The hash is skipped.
%   @tbd    Should we also cache failures to reduce the risc of
%           DoS attacks?

http_authenticate(basic(File), Request, [User|Fields]) :-
    memberchk(authorization(Text), Request),
    debug(http_authenticate, 'Authorization: ~w', [Text]),
    (   cached_authenticated(Text, File, User, Fields)
    ->  true
    ;   http_authorization_data(Text, basic(User, Password)),
        debug(http_authenticate,
              'User: ~w, Password: ~s', [User, Password]),
        validate(File, User, Password, Fields),
        get_time(Now),
        assert(authenticated(Text, File, User, Now, Fields)),
        debug(http_authenticate, 'Authenticated ~w~n', [User])
    ).

%!  http_authorization_data(+AuthorizeText, ?Data) is semidet.
%
%   Decode the HTTP =Authorization= header.  Data is a term
%
%       Method(User, Password)
%
%   where Method is the (downcased)  authorization method (typically
%   =basic=), User is an atom holding the  user name and Password is
%   a list of codes holding the password

http_authorization_data(Text, Data) :-
    (   nonvar(Data)
    ->  functor(Data, Method, 2)    % make authorization//2 fail early
    ;   true
    ),
    atom_codes(Text, Codes),
    phrase(authorization(Method, Cookie), Codes),
    phrase(base64(UserPwd), Cookie),
    phrase(ident(UserCodes, Password), UserPwd),
    !,
    atom_codes(User, UserCodes),
    Data =.. [Method, User, Password].

authorization(Method, Cookie) -->
    nonblanks(MethodChars),
    { atom_codes(Method0, MethodChars),
      downcase_atom(Method0, Method)
    },
    blanks,
    nonblanks(Cookie),
    blanks.

ident(User, Password) -->
    string(User),
    ":",
    string(Password).

%!  cached_authenticated(+Authorization, +File, -User, -RestFields)
%
%   Validate using the cache. If the entry   is not in the cache, we
%   also remove all outdated entries from the cache.

:- dynamic
    authenticated/5.        % Authorization, File, User, Time, RestFields

cached_authenticated(Authorization, File, User, Fields) :-
    authenticated(Authorization, File, User, Time, Fields),
    get_time(Now),
    Now-Time =< 60,
    !.              % 60-second timeout
cached_authenticated(_, _, _, _) :-
    get_time(Now),
    (   clause(authenticated(_, _, _, Time, _), true, Ref),
        Now-Time > 60,
        erase(Ref),
        fail
    ).


%!  validate(+File, +User, +Passwd, -Fields)
%
%   True if User and Passwd combination   appears in File. File uses
%   the same format as .htaccess files  from Apache or Unix password
%   files. I.e. it consists  of  one   line  per  entry  with fields
%   separated by =|:|=. The  first  field   is  the  User field, The
%   second contains the Passwd in DES   or MD5 encrypted format. See
%   crypt/2 for details.

validate(File, User, Password, Fields) :-
    update_passwd(File, Path),
    passwd(User, Path, Hash, Fields),
    crypt(Password, Hash).

%!  http_current_user(+File, ?User, ?Fields) is nondet.
%
%   True when User is present in the htpasswd file File and Fields
%   provides the additional fields.
%
%   @arg    Fields are the fields from the password file File,
%           converted using name/2, which means that numeric values
%           are passed as numbers and other fields as atoms.  The
%           password hash is the first element of Fields and is
%           a string.

http_current_user(File, User, Fields) :-
    update_passwd(File, Path),
    passwd(User, Path, Hash, Fields0),
    Fields = [Hash|Fields0].

%!  update_passwd(+File, -Path) is det.
%
%   Update passwd/3 to reflect the correct  passwords for File. Path
%   is the absolute path for File.

:- dynamic
    passwd/4,                       % User, File, Encrypted, Fields
    last_modified/2.                % File, Stamp

update_passwd(File, Path) :-
    absolute_file_name(File, Path, [access(read)]),
    time_file(Path, Stamp),
    (   last_modified(Path, Stamp)
    ->  true
    ;   with_mutex(http_passwd, reload_passwd_file(Path, Stamp))
    ).

reload_passwd_file(Path, Stamp) :-
    last_modified(Path, Stamp),
    !.  % another thread did the work
reload_passwd_file(Path, Stamp) :-
    http_read_passwd_file(Path, Data),
    retractall(last_modified(Path, _)),
    retractall(passwd(_, Path, _, _)),
    forall(member(passwd(User, Hash, Fields), Data),
           assertz(passwd(User, Path, Hash, Fields))),
    assert(last_modified(Path, Stamp)).

%!  http_read_passwd_file(+Path, -Data) is det.
%
%   Read a password file. Data is  a   list  of  terms of the format
%   below, where User is an atom  identifying   the  user, Hash is a
%   string containing the salted password   hash  and Fields contain
%   additional fields. The string value of   each field is converted
%   using name/2 to either a number or an atom.
%
%     ==
%     passwd(User, Hash, Fields)
%     ==

http_read_passwd_file(Path, Data) :-
    setup_call_cleanup(
        open(Path, read, Fd),
        ( read_line_to_codes(Fd, Line),
          read_passwd_file(Line, Fd, Path, Data)
        ),
        close(Fd)).

read_passwd_file(end_of_file, _, _, []) :- !.
read_passwd_file(Line, Fd, Path, Data) :-
    (   phrase(password_line(User, Hash, Fields), Line, _)
    ->  Data = [passwd(User, Hash, Fields)|Tail]
    ;   Tail = Data                 % TBD: warning
    ),
    read_line_to_codes(Fd, Line2),
    read_passwd_file(Line2, Fd, Path, Tail).


password_line(User, Hash, Fields) -->
    string(UserCodes),
    ":",
    string(HashCodes),
    peek_eof,
    !,
    fields(Fields),
    { atom_codes(User, UserCodes),
      string_codes(Hash, HashCodes)
    }.

fields([Field|Fields]) -->
    field(Field),
    !,
    fields(Fields).
fields([]) --> [].

field(Value) -->
    ":",
    !,
    string(Codes),
    peek_eof,
    !,
    { name(Value, Codes)
    }.

peek_eof, ":" --> ":".
peek_eof --> eos.


%!  http_write_passwd_file(+File, +Data:list) is det.
%
%   Write password data Data to File. Data   is a list of entries as
%   below. See http_read_passwd_file/2 for details.
%
%     ==
%     passwd(User, Hash, Fields)
%     ==
%
%   @tbd    Write to a new file and atomically replace the old one.

http_write_passwd_file(File, Data) :-
    must_be(list, Data),
    maplist(valid_data, Data),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        maplist(write_data(Out), Data),
        close(Out)),
    retractall(last_modified(File, _)). % flush cache

valid_data(passwd(User, Hash, Fields)) :-
    !,
    valid_field(User),
    valid_field(Hash),
    must_be(list, Fields),
    maplist(valid_field, Fields).
valid_data(Data) :-
    type_error(passwd_entry, Data).

valid_field(Field) :-
    must_be(atomic, Field),
    (   number(Field)
    ->  true
    ;   sub_string(Field, _, _, _, ":")
    ->  representation_error(passwd_field)
    ;   true
    ).

write_data(Out, passwd(User, Hash, Fields)) :-
    atomics_to_string([User, Hash|Fields], ":", String),
    format(Out, '~s~n', [String]).


                 /*******************************
                 *   PLUGIN FOR HTTP_DISPATCH   *
                 *******************************/

:- multifile
    http:authenticate/3.

%!  http:authenticate(+AuthData, +Request, -Fields)
%
%   Plugin  for  library(http_dispatch)  to    perform   basic  HTTP
%   authentication.
%
%   This predicate throws http_reply(authorise(basic, Realm)).
%
%   @arg    AuthData must be a term basic(File, Realm)
%   @arg    Request is the HTTP request
%   @arg    Fields describes the authenticated user with the option
%           user(User) and with the option user_details(Fields) if
%           the password file contains additional fields after the
%           user and password.

http:authenticate(basic(File, Realm), Request,
                  [ user(User)
                  | Details
                  ]) :-
    (   http_authenticate(basic(File), Request, [User|Fields])
    ->  (   Fields == []
        ->  Details = []
        ;   Details = [user_details(Fields)]
        )
    ;   throw(http_reply(authorise(basic, Realm)))
    ).

