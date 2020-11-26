/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2016, VU University Amsterdam
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

:- module(http_digest,
          [ http_digest_challenge//2,      % +Realm, +Options
            http_digest_password_hash/4,   % +User, +Realm, +Passwd, -Hash
                                           % client support
            http_parse_digest_challenge/2, % +Challenge, -Fields
            http_digest_response/5         % +Fields, +User, +Password,
                                           % -Reply +Opts
          ]).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_stream)).
:- use_module(library(dcg/basics)).
:- use_module(library(md5)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(settings)).
:- use_module(library(base64)).
:- use_module(library(broadcast)).
:- use_module(library(uri)).
:- use_module(library(apply)).


/** <module> HTTP Digest authentication

This library implements HTTP  _Digest   Authentication_  as per RFC2617.
Unlike  _Basic  Authentication_,  digest  authentication   is  based  on
challenge-reponse and therefore does not need  to send the password over
the (insecure) connection. In addition, it   provides  a count mechanism
that ensure that old  credentials  cannot   be  reused,  which  prevents
attackers  from  using  old  credentials  with  a  new  request.  Digest
authentication have the following advantages and disadvantages:

  - Advantages
    - Authentication without exchanging the password
    - No re-use of authentication data
  - Disadvantages
    - An extra round trip is needed for the first authentication
    - Server-side storage of the password is the MD5 hash of the
      user, _realm_ and password.  As MD5 hashes are quick to
      compute, one needs strong passwords.  This fixed algorithm
      also allows for _rainbow table_ attacks, although their
      value is limited because you need to precompute the rainbow
      table for every server (_realm_) and user.
    - The connection is sensitive to man-in-the-middle attack,
      where the attacker can both change the request and response.
    - Both client and server need to keep an administration of
      issued _nonce_ values and associated _nonce count_ values.

And, of course, the connection  itself   remains  insecure. Digest based
authentication is a viable alternative if HTTPS is not a good option and
security of the data itself is not an issue.

This library acts as plugin   for library(http/http_dispatch), where the
registered handler (http_handler/3) can be  given   the  option below to
initiate digest authentication.

  - authentication(digest(PasswdFile, Realm))

Above, `PasswdFile` is a file containing lines  of the from below, where
PasswordHash is computed  using   http_digest_password_hash/4.  See also
library(http/http_authenticate),       http_read_passwd_file/2       and
http_write_passwd_file/2.

  ==
  User ":" PasswordHash (":" Extra)*
  ==

This library also  hooks  into   library(http/http_open)  if  the option
authorization(digest(User, Password)) is given.

@see https://tools.ietf.org/html/rfc2617
*/

:- setting(nonce_timeout, number, 3600,
           "Validity time for a server nonce").
:- setting(client_nonce_timeout, number, 3600,
           "Validity time for a client nonce").

                 /*******************************
                 *      TRACK CONNECTIONS       *
                 *******************************/

:- dynamic
    nonce_key/1,                    % Our nonce private key
    nonce/2,                        % Nonce, CreatedTime
    nonce_nc/3,                     % Nonce, NC, Time
    nonce_nc_first/2,               % Nonce, NC
    nonce_gc_time/1.                % Time of last nonce GC

%!  register_nonce(+Nonce, +Created) is det.
%
%   Register a nonce created by the  server.   We  need  to do so to
%   ensure the client uses our nonce  and that the connection should
%   not considered timed out.

register_nonce(Nonce64, Created) :-
    broadcast(http_digest(nonce(Nonce64, Created))),
    assertz(nonce(Nonce64, Created)),
    gc_nonce.

%!  nonce_ok(+Nonce, +NC, -Stale) is semidet.
%
%   True if Nonce at nonce-count NC   is  acceptable. That means the
%   nonce has not timed out and we   have not seen the same sequence
%   number  before.  Note  that  requests   may  be  concurrent  and
%   therefore NC values may not come in order.

nonce_ok(Nonce, NC, Stale) :-
    get_time(Now),
    nonce_not_timed_out(Nonce, Now, Stale),
    nonce_nc_ok(Nonce, NC, Now).

nonce_not_timed_out(Nonce, Now, Stale) :-
    (   nonce(Nonce, Created)
    ->  setting(nonce_timeout, TimeOut),
        (   Now - Created < TimeOut
        ->  Stale = false
        ;   forget_nonce(Nonce),
            debug(http(nonce), 'Nonce timed out: ~q', [Nonce]),
            Stale = true
        )
    ;   our_nonce(Nonce, _Stamp)
    ->  Stale = true
    ;   debug(http(nonce), 'Unknown nonce: ~q', [Nonce]),
        fail
    ).

nonce_nc_ok(Nonce, NC, _Now) :-
    (   nonce_nc(Nonce, NC, _)
    ;   nonce_nc_first(Nonce, First),
        NC @=< First
    ),
    !,
    debug(http(nonce), 'Nonce replay attempt: ~q@~q', [Nonce, NC]),
    fail.
nonce_nc_ok(Nonce, NC, Now) :-
    assertz(nonce_nc(Nonce, NC, Now)).

forget_nonce(Nonce) :-
    retractall(nonce(Nonce, _)),
    retractall(nonce_nc(Nonce, _, _)),
    retractall(nonce_nc_first(Nonce, _)).

%!  gc_nonce
%
%   Garbage collect server nonce.

gc_nonce :-
    nonce_gc_time(Last),
    get_time(Now),
    setting(nonce_timeout, TimeOut),
    Now-Last < TimeOut/4,
    !.
gc_nonce :-
    with_mutex(http_digest_gc_nonce,
               gc_nonce_sync).

gc_nonce_sync :-
    get_time(Now),
    asserta(nonce_gc_time(Now)),
    forall(( nonce_gc_time(T),
             T \== Now
           ),
           retractall(nonce_gc_time(T))),
    setting(nonce_timeout, TimeOut),
    Before is Now - TimeOut,
    forall(nonce_timed_out(Nonce, Before),
           forget_nonce(Nonce)),
    NCBefore is Now - 60,
    forall(nonce(Nonce, _Created),
           gc_nonce_nc(Nonce, NCBefore)).

nonce_timed_out(Nonce, Before) :-
    nonce(Nonce, Created),
    Created < Before.

gc_nonce_nc(Nonce, Before) :-
    findall(NC, gc_nonce_nc(Nonce, Before, NC), List),
    sort(0, @>, List, [Max|_]),
    !,
    asserta(nonce_nc_first(Nonce, Max)),
    forall(( nonce_nc_first(Nonce, NC),
             NC \== Max
           ),
           retractall(nonce_nc_first(Nonce, NC))).
gc_nonce_nc(_, _).

gc_nonce_nc(Nonce, Before, NC) :-
    nonce_nc(Nonce, NC, Time),
    Time < Before,
    retractall(nonce_nc(Nonce, NC, Time)).



%!  private_key(-PrivateKey) is det.
%
%   Return our private key.

private_key(PrivateKey) :-
    nonce_key(PrivateKey),
    !.
private_key(PrivateKey) :-
    with_mutex(http_digest,
               private_key_sync(PrivateKey)).

private_key_sync(PrivateKey) :-
    nonce_key(PrivateKey),
    !.
private_key_sync(PrivateKey) :-
    PrivateKey is random(1<<63-1),
    assertz(nonce_key(PrivateKey)).

%!  our_nonce(+Nonce, -Stamp:string) is semidet.
%
%   True if we created Nonce at time Stamp.
%
%   @arg  Stamp  is  the  stamp  as  created  by  nonce//1:  a  time
%   stamp*1000+sequence number.

our_nonce(Nonce64, Stamp) :-
    base64(Nonce, Nonce64),
    split_string(Nonce, ":", "", [Stamp,HNonceContent]),
    private_key(PrivateKey),
    atomics_to_string([Stamp,PrivateKey], ":", NonceContent),
    hash(NonceContent, HNonceContent).


                 /*******************************
                 *            GRAMMAR           *
                 *******************************/

%!  http_digest_challenge(+Realm, +Options)//
%
%   Generate the content for  a   401  =|WWW-Authenticate:  Digest|=
%   header field.

http_digest_challenge(Realm, Options) -->
    %       "Digest ",
            realm(Realm),
            domain(Options),
            nonce(Options),
            option_value(opaque, Options),
            stale(Options),
    %       algorithm(Options),
            qop_options(Options).
%       auth_param(Options).

realm(Realm) -->
    { no_dquote(realm, Realm) },
    "realm=\"", atom(Realm), "\"".

domain(Options) -->
    { option(domain(Domain), Options) },
    !,
    sep, "domain=\"", uris(Domain), "\"".
domain(_) --> "".

uris(Domain) -->
    { atomic(Domain) },
    !,
    uri(Domain).
uris(Domains) -->
    { must_be(list(atomic), Domains)
    },
    uri_list(Domains).

uri_list([]) --> "".
uri_list([H|T]) -->
    uri(H),
    (   {T \== []}
    ->  " ", uri_list(T)
    ;   ""
    ).

uri(URI) -->
    { no_dquote(uri, URI) },
    atom(URI).

%!  nonce(+Options)
%
%   Compute the server _nonce_ value.  Note   that  we  should never
%   generate the same nonce twice for   the  same client. The client
%   _may_ issue multiple requests without   an  authorization header
%   for resources appearing on a page. As long as we return distinct
%   nonce values, this is ok. If we do not, the server will reuse NC
%   counters on the same nonce, which will break the authentication.

nonce(Options) -->
    { get_time(Now),
      flag(http_digest_nonce_seq, Seq, Seq+1),
      Stamp is floor(Now)*1000+(Seq mod 1000),
      private_key(PrivateKey),
      atomics_to_string([Stamp,PrivateKey], ":", NonceContent),
      hash(NonceContent, HNonceContent),
      atomics_to_string([Stamp,HNonceContent], ":", NonceText),
      base64(NonceText, Nonce),
      option(nonce(Nonce-Now), Options, _),
      debug(http(authenticate), 'Server nonce: ~q', [Nonce])
    },
    sep, "nonce=\"", atom(Nonce), "\"".

stale(Options) -->
    { option(stale(true), Options), !
    },
    sep, "stale=true".
stale(_) --> "".

qop_options(_Options) -->
    sep, "qop=\"auth,auth-int\"".

option_value(Key, Options) -->
    { Opt =.. [Key,Value],
      option(Opt, Options), !
    },
    key_qvalue(Key, Value).
option_value(_, _) --> "".

key_value(Key, Value)  -->
    atom(Key), "=", atom(Value).
key_qvalue(Key, Value) -->
    { no_dquote(Key, Value) },
    atom(Key), "=\"", atom(Value), "\"".

no_dquote(Key, Value) :-
    nonvar(Value),
    sub_atom(Value, _, _, _, '"'),
    !,
    domain_error(Key, value).
no_dquote(_, _).

sep --> ", ".

hash(Text, Hash) :-
    md5_hash(Text, Hash, []).

%!  http_digest_authenticate(+Request, -User, -UserFields, +Options)
%
%   Validate the client reponse from the Request header. On success,
%   User is the validated user and  UserFields are additional fields
%   from the password file. Options include:
%
%     - passwd_file(+File)
%     Validate passwords agains the given password file.  The
%     file is read using http_current_user/3 from
%     library(http/http_authenticate).
%     - stale(-Stale)
%     The request may succeed on a timed-out server nonce.  In
%     that case, Stale is unified with `true`.

http_digest_authenticate(Request, [User|Fields], Options) :-
    memberchk(authorization(Authorization), Request),
    debug(http(authenticate), 'Authorization: ~w', [Authorization]),
    digest_authenticate(Authorization, User, Fields, Options).

digest_authenticate(Authorization, User, Fields, Options) :-
    string_codes(Authorization, AuthorizationCodes),
    phrase(parse_digest_reponse(AuthValues), AuthorizationCodes),
    memberchk(username(User), AuthValues),
    memberchk(realm(Realm), AuthValues),
    memberchk(nonce(ServerNonce), AuthValues),
    memberchk(uri(Path), AuthValues),
    memberchk(qop(QOP), AuthValues),
    memberchk(nc(NC), AuthValues),
    memberchk(cnonce(ClientNonce), AuthValues),
    memberchk(response(Response), AuthValues),
    user_ha1_details(User, Realm, HA1, Fields, Options),
    option(method(Method), Options, get),
    ha2(Method, Path, HA2),
    atomics_to_string([ HA1,
                        ServerNonce,
                        NC,
                        ClientNonce,
                        QOP,
                        HA2
                      ], ":", ResponseText),
    debug(http(authenticate), 'ResponseText: ~w', [ResponseText]),
    hash(ResponseText, ResponseExpected),
    (   Response == ResponseExpected
    ->  debug(http(authenticate), 'We have a match!', [])
    ;   debug(http(authenticate),
              '~q \\== ~q', [Response, ResponseExpected]),
        fail
    ),
    nonce_ok(ServerNonce, NC, Stale),
    (   option(stale(Stale), Options)
    ->  true
    ;   Stale == false
    ).

user_ha1_details(User, _Realm, HA1, Fields, Options) :-
    option(passwd_file(File), Options),
    http_current_user(File, User, [HA1|Fields]).

%!  parse_digest_request(-Fields)//
%
%   Parse a digest request into a list of Name(Value) terms.

parse_digest_request(Fields) -->
    "Digest", whites,
    digest_values(Fields).

%!  parse_digest_reponse(-ResponseValues)//

parse_digest_reponse(ResponseValues) -->
    "Digest", whites,
    digest_values(ResponseValues).


digest_values([H|T]) -->
    digest_value(H),
    !,
    whites,
    (   ","
    ->  whites,
        digest_values(T)
    ;   {T = []}
    ).

digest_value(V) -->
    string_without(`=`, NameCodes), "=",
    { atom_codes(Name, NameCodes) },
    digest_value(Name, V).

digest_value(Name, V) -->
    "\"",
    !,
    string_without(`"`, ValueCodes), "\"",
    { parse_value(Name, ValueCodes, Value),
      V =.. [Name,Value]
    }.
digest_value(stale, stale(V)) -->
    !,
    boolean(V).
digest_value(Name, V) -->
    string_without(`, `, ValueCodes),
    { parse_value(Name, ValueCodes, Value),
      V =.. [Name,Value]
    }.


parse_value(domain, Codes, Domain) :-
    !,
    string_codes(String, Codes),
    atomic_list_concat(Domain, ' ', String).
parse_value(Name, Codes, Value) :-
    atom_value(Name),
    atom_codes(Value, Codes).
parse_value(_Name, Codes, Value) :-
    string_codes(Value, Codes).

atom_value(realm).
atom_value(username).
atom_value(response).
atom_value(nonce).
atom_value(stale).              % for misbehaving servers that quote stale

boolean(true) --> "true".
boolean(false) --> "false".


                 /*******************************
                 *           CLIENT             *
                 *******************************/

%!  http_parse_digest_challenge(+Challenge, -Fields) is det.
%
%   Parse the value of an HTTP =|WWW-Authenticate|= header into
%   a list of Name(Value) terms.

http_parse_digest_challenge(Challenge, Fields) :-
    string_codes(Challenge, ReqCodes),
    phrase(parse_digest_request(Fields), ReqCodes).

%!  http_digest_response(+Challenge, +User, +Password, -Reply, +Options)
%
%   Formulate a reply to a digest authentication request.  Options:
%
%     - path(+Path)
%     The request URI send along with the authentication.  Defaults
%     to `/`
%     - method(+Method)
%     The HTTP method.  Defaults to `'GET'`
%     - nc(+Integer)
%     The nonce-count as an integer.  This is formatted as an
%     8 hex-digit string.
%
%   @arg    Challenge is a list Name(Value), normally from
%           http_parse_digest_challenge/2.  Must contain
%           `realm` and  `nonce`.  Optionally contains
%           `opaque`.
%   @arg    User is the user we want to authenticated
%   @arg    Password is the user's password
%   @arg    Options provides additional options

http_digest_response(Fields, User, Password, Reply, Options) :-
    phrase(http_digest_response(Fields, User, Password, Options), Codes),
    string_codes(Reply, Codes).

http_digest_response(Fields, User, Password, Options) -->
    { memberchk(nonce(ServerNonce), Fields),
      memberchk(realm(Realm), Fields),
      client_nonce(ClientNonce),
      http_digest_password_hash(User, Realm, Password, HA1),
      QOP = 'auth',
      option(path(Path), Options, /),
      option(method(Method), Options, 'GET'),
      option(nc(NC), Options, 1),
      format(string(NCS), '~`0t~16r~8+', [NC]),
      ha2(Method, Path, HA2),
      atomics_to_string([ HA1,
                          ServerNonce,
                          NCS,
                          ClientNonce,
                          QOP,
                          HA2
                        ], ":", ResponseText),
      hash(ResponseText, Response)
    },
    "Digest ",
    key_qvalue(username, User),
    sep, key_qvalue(realm, Realm),
    sep, key_qvalue(nonce, ServerNonce),
    sep, key_qvalue(uri, Path),
    sep, key_value(qop, QOP),
    sep, key_value(nc, NCS),
    sep, key_qvalue(cnonce, ClientNonce),
    sep, key_qvalue(response, Response),
    (   { memberchk(opaque(Opaque), Fields) }
    ->  sep, key_qvalue(opaque, Opaque)
    ;   ""
    ).

client_nonce(Nonce) :-
    V is random(1<<32),
    format(string(Nonce), '~`0t~16r~8|', [V]).

ha2(Method, Path, HA2) :-
    string_upper(Method, UMethod),
    atomics_to_string([UMethod,Path], ":", A2),
    hash(A2, HA2).

%!  http_digest_password_hash(+User, +Realm, +Password, -Hash) is det.
%
%   Compute the password hash for the HTTP password file.  Note that
%   the HTTP digest mechanism does allow us to use a seeded expensive
%   arbitrary hash function.  Instead, the hash is defined as the MD5
%   of the following components:
%
%     ==
%     <user>:<realm>:<password>.
%     ==
%
%   The inexpensive MD5 algorithm makes the hash sensitive to brute
%   force attacks while the lack of seeding make the hashes sensitive
%   for _rainbow table_ attacks, although the value is somewhat limited
%   because the _realm_ and _user_ are part of the hash.

http_digest_password_hash(User, Realm, Password, HA1) :-
    atomics_to_string([User,Realm,Password], ":", A1),
    hash(A1, HA1).


                 /*******************************
                 *   PLUGIN FOR HTTP_DISPATCH   *
                 *******************************/

:- multifile
    http:authenticate/3.

%!  http:authenticate(+Digest, +Request, -Fields)
%
%   Plugin  for  library(http_dispatch)  to    perform   basic  HTTP
%   authentication.  Note that we keep the authentication details
%   cached to avoid a `nonce-replay' error in the case that the
%   application tries to verify multiple times.
%
%   This predicate throws http_reply(authorise(digest(Digest)))
%
%   @arg    Digest is a term digest(File, Realm, Options)
%   @arg    Request is the HTTP request
%   @arg    Fields describes the authenticated user with the option
%           user(User) and with the option user_details(Fields) if
%           the password file contains additional fields after the
%           user and password.

http:authenticate(digest(File, Realm), Request, Details) :-
    http:authenticate(digest(File, Realm, []), Request, Details).
http:authenticate(digest(File, Realm, Options), Request, Details) :-
    current_output(CGI),
    cgi_property(CGI, id(Id)),
    (   nb_current('$http_digest_user', Id-Details)
    ->  true
    ;   authenticate(digest(File, Realm, Options), Request, Details),
        nb_setval('$http_digest_user', Id-Details)
    ).

authenticate(digest(File, Realm, Options), Request,
             [ user(User)
             | Details
             ]) :-
    (   option(method(Method), Request, get),
        http_digest_authenticate(Request, [User|Fields],
                                 [ passwd_file(File),
                                   stale(Stale),
                                   method(Method)
                                 ])
    ->  (   Stale == false
        ->  (   Fields == []
            ->  Details = []
            ;   Details = [user_details(Fields)]
            ),
            Ok = true
        ;   true
        )
    ;   true
    ),
    (   Ok == true
    ->  true
    ;   add_option(nonce(Nonce-Created), Options, Options1),
        add_stale(Stale, Options1, Options2),
        phrase(http_digest_challenge(Realm, Options2), DigestCodes),
        string_codes(Digest, DigestCodes),
        register_nonce(Nonce, Created),
        throw(http_reply(authorise(digest(Digest))))
    ).

add_option(Option, Options0, _) :-
    option(Option, Options0),
    !.
add_option(Option, Options0, [Option|Options0]).

add_stale(Stale, Options0, Options) :-
    Stale == true,
    !,
    Options = [stale(true)|Options0].
add_stale(_, Options, Options).


                 /*******************************
                 *     PLUGIN FOT HTTP_OPEN     *
                 *******************************/

:- multifile
    http:authenticate_client/2.
:- dynamic
    client_nonce/4,                 % Authority, Domains, Keep, Time
    client_nonce_nc/3,              % Nonce, Count, Time
    client_nonce_gc_time/1.         % Time

%!  http:authenticate_client(+URL, +Action) is semidet.
%
%   This hooks is called by http_open/3 with the following Action
%   value:
%
%     - send_auth_header(+AuthData, +Out, +Options)
%     Called when sending the initial request.  AuthData contains
%     the value for the http_open/3 option authorization(AuthData)
%     and Out is a stream on which to write additional HTTP headers.
%     - auth_reponse(+Headers, +OptionsIn, -Options)
%     Called if the server replies with a 401 code, challenging the
%     client.  Our implementation adds a
%     request_header(authorization=Digest) header to Options, causing
%     http_open/3 to retry the request with the additional option.

http:authenticate_client(URL, auth_reponse(Headers, OptionsIn, Options)) :-
    debug(http(authenticate), "Got 401 with ~p", [Headers]),
    memberchk(www_authenticate(Authenticate), Headers),
    http_parse_digest_challenge(Authenticate, Fields),
    user_password(OptionsIn, User, Password),
    !,
    uri_components(URL, Components),
    uri_data(path, Components, Path),
    http_digest_response(Fields, User, Password, Digest,
                             [ path(Path)
                             | OptionsIn
                             ]),
    merge_options([ request_header(authorization=Digest)
                  ],
                  OptionsIn, Options),
    keep_digest_credentials(URL, Fields).
http:authenticate_client(URL, send_auth_header(Auth, Out, Options)) :-
    authorization_data(Auth, User, Password),
    uri_components(URL, Components),
    uri_data(authority, Components, Authority),
    uri_data(path, Components, Path),
    digest_credentials(Authority, Path, Nonce, Fields),
    !,
    next_nonce_count(Nonce, NC),
    debug(http(authenticate), "Continue ~p nc=~q", [URL, NC]),
    http_digest_response(Fields, User, Password, Digest,
                         [ nc(NC),
                           path(Path)
                         | Options
                         ]),
    format(Out, 'Authorization: ~w\r\n', [Digest]).
http:authenticate_client(URL, send_auth_header(Auth, _Out, _Options)) :-
    debug(http(authenticate), "Failed ~p", [URL]),
    authorization_data(Auth, _User, _Password).


user_password(Options, User, Password) :-
    option(authorization(Auth), Options),
    authorization_data(Auth, User, Password).

authorization_data(digest(User, Password), User, Password).

%!  digest_credentials(+Authority, +Path, -Nonce, -Fields) is semidet.
%
%   True if we have digest credentials for Authority on Path with the
%   server _nonce_ Nonce and additional Fields.

digest_credentials(Authority, Path, Nonce, Fields) :-
    client_nonce(Authority, Domains, Fields, _Created),
    in_domain(Path, Domains),
    memberchk(nonce(Nonce), Fields),
    !.

in_domain(Path, Domains) :-
    member(Domain, Domains),
    sub_atom(Path, 0, _, _, Domain),
    !.

next_nonce_count(Nonce, NC) :-
    with_mutex(http_digest_client,
               next_nonce_count_sync(Nonce, NC)).

next_nonce_count_sync(Nonce, NC) :-
    retract(client_nonce_nc(Nonce, NC0, _)),
    !,
    NC1 is NC0+1,
    get_time(Now),
    assert(client_nonce_nc(Nonce, NC1, Now)),
    NC = NC1.
next_nonce_count_sync(Nonce, 2) :-
    get_time(Now),
    assert(client_nonce_nc(Nonce, 2, Now)).

%!  keep_digest_credentials(+URL, +Fields)
%
%   Keep the digest credentials for subsequent connections.

keep_digest_credentials(URL, Fields) :-
    get_time(Now),
    uri_components(URL, Components),
    uri_data(authority, Components, Authority),
    include(keep_field, Fields, Keep),
    (   memberchk(domain(Domains), Fields)
    ->  true
    ;   Domains = [/]
    ),
    assertz(client_nonce(Authority, Domains, Keep, Now)),
    gc_client_nonce.

keep_field(realm(_)).
keep_field(nonce(_)).
keep_field(opaque(_)).

gc_client_nonce :-
    client_nonce_gc_time(Last),
    get_time(Now),
    setting(client_nonce_timeout, TimeOut),
    Now-Last < TimeOut/4,
    !.
gc_client_nonce :-
    get_time(Now),
    retractall(client_nonce_gc_time(_)),
    asserta(client_nonce_gc_time(Now)),
    setting(client_nonce_timeout, TimeOut),
    Before is Now-TimeOut,
    forall(client_nonce_expired(Nonce, Before),
           forget_client_nonce(Nonce)).

client_nonce_expired(Nonce, Before) :-
    client_nonce(_Authority, _Domains, Fields, Created),
    Created < Before,
    memberchk(nonce(Nonce), Fields),
    \+ ( client_nonce_nc(Nonce, _, Last),
         Last < Before
       ).

forget_client_nonce(Nonce) :-
    client_nonce(_, _, Fields, Created),
    memberchk(nonce(Nonce), Fields),
    !,
    retractall(client_nonce(_, _, Fields, Created)),
    retractall(client_nonce_nc(Nonce, _, _)).
