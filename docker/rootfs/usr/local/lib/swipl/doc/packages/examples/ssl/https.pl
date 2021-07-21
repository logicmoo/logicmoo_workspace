/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org

    This demo code is released in the _public domain_, which implies
    you may copy, modify and reuse this code without restriction.
*/

:- module(https,
          [ http_download/2,                    % +URL, -String

            http_server/1,                      % ?Port
            https_server/1,                     % ?Port
            https_server_with_client_cert/1,    % ?Port

            http_client/2,                      % +Port, +Page
            https_client/2,                     % +Port, +Page
            https_client_with_client_cert/2     % +Port, +Page
          ]).

% Include  our  dependencies.  The  conditions  are  to  accomodate  for
% different locations of some of  the  files   in  the  source  tree and
% examples tree.

:- prolog_load_context(directory, D),
   atom_concat(D, '/..', DD),
   asserta(user:file_search_path(library, DD)).
:- if(exists_source(library(http/examples/demo_body))).
:- use_module(library(http/examples/demo_body)).    % location in source tree
:- elif(exists_source(swi(doc/packages/examples/http/demo_body))).
:- use_module(swi(doc/packages/examples/http/demo_body)).
:- else.
:- use_module(library(http/demo_body)).             % location in demo tree
:- endif.
:- use_module(library(http/thread_httpd)).
:- if(exists_source(library(http/http_ssl_plugin))).
:- use_module(library(http/http_ssl_plugin)).
:- else.
:- use_module(http_ssl_plugin).
:- endif.
:- use_module(library(debug)).

/** <module> Demo HTTPS server

This demo illustrates how to use the SSL library for dealing with HTTPS.
It provides examples for simple download of HTTPS documents from the web
and provides demo code  for  several   HTTPS  servers  and corresponding
client code using  (self-signed)  certificates.   The  certificates  are
provided in the  `etc`  directory.  These   certificates  are  the  same
certificates that are used for testing the SSL library. You may use them
for testing purposes, but you should not  use them for your own services
because the private key is not so private.

This demo implements three different servers:

  - http_server/1 implements a typical HTTP server.
  - https_server/1 implements a typical HTTPS server.
  - https_server_with_client_cert/1 implements a server that asks

The startup sequence to run the  server   at  port  1443 is shown below.
Directing your browser to the indicated URL should cause your browser to
warn you about an untrusted certificate. After accepting you can use the
simple demo through HTTPS.

  ==
  $ swipl https.pl
  ...
  ?- https_server(1443).
  % Started server at https://localhost:1443/
  true.
  ==

The demo is primarily intended to  access   from  a  browser, but at the
bottom of this file are predicates to access the server from Prolog. The
first argument is the port and must be the same as the value returned by
or given to the *_server predicate. The   second  is the location on the
server, e.g., `/`, `/quit`, `/env`, etc.

  - http_client/2 accesses http_server/1.
  - https_client/2 accesses https_server/1.
  - https_client_with_client_cert/2 accesses https_server_with_client_cert/1.

An example session is given  below.  Now   that  you  can either run the
client calls from the Prolog window in   which you started the server or
load this file into another Prolog.

  ==
  ?- https_client(1443, '/quit').
  Bye Bye
  true.
  ==
*/

%!  http_download(+URL, -Data) is det.
%
%   Download data from an HTTP or HTTPS   server.  If HTTPS is used,
%   the certificate is verified  by   the  system's root certificate
%   store.  This  means  you  cannot  download  from  servers  using
%   self-signed certificates, but  you  can   download  from  proper
%   public websites with official authorized keys.  For example:
%
%     ==
%     ?- http_download('https://raw.githubusercontent.com\c
%                      /SWI-Prolog/packages-ssl/master/README.md',
%                      String).
%     ==

http_download(URL, String) :-
    http_open(URL, In, []),
    call_cleanup(
        read_string(In, _, String),
        close(In)).

%!  http_server(?Port) is det.
%
%   Our baseline is a plain HTTP server.  No HTTPS involved here. We
%   give it for the case you want to do timing and other experiments
%   comparing the HTTP with HTTPS.

http_server(Port) :-
    http_server(reply,
                [ port(Port)
                ]).


%!  https_server(?Port) is det.
%
%   Start an HTTPS demo server at Port.   Compared  to a normal HTTP
%   server, this requires two additional SSL components:
%
%     1. The server certificate.  This is basically a public
%     key, so there is no need to keep this secret.
%     2. The server private key.  If someone manages to grab
%     this key, he can setup a server that claims to be you.
%     There are two ways to protect it.  One is to make sure
%     the file cannot be obtained and the other is to protect
%     it using a password and make sure that the password is
%     kept secret.  Our server uses a password, but it is not
%     very secret.  See also the `pem_password_hook` option
%     of ssl_context/3.
%
%   Note that anyone can access this  server. You can implement HTTP
%   authentication or cookie based password login in the application
%   to realise a safe login procedure  where attackers cannot easily
%   steal the HTTP authentication token or cookie.

https_server(Port) :-
    http_server(reply,
                [ port(Port),
                  ssl([ certificate_file('etc/server/server-cert.pem'),
                        key_file('etc/server/server-key.pem'),
                        password("apenoot1")
                      ])
                ]).


%!  https_server_with_client_cert(?Port)
%
%   Our second server  is  a  setup   that  is  typically  used  for
%   administrative tasks where users  are   handed  a certificate to
%   login. In our example, we use the client certificate and private
%   key that can be found in etc/client.   First  of all, we need to
%   combine these into a `.p12` (PKCS12)   file.  This is done using
%   `openssl`  as  below.  We  provide  the    .p12  file  for  your
%   convenience.
%
%     ==
%     $ openssl pkcs12 -export \
%           -inkey client-key.pem -in client-cert.pem \
%           -name jan -out client-cert.p12
%     Enter pass phrase for client-key.pem: apenoot2
%     Enter Export Password: secret
%     ==
%
%   Next, import `client-cert.p12` into your   browser. For firefox,
%   this is in   Edit/Preference/Advanced/View  Certificates/Import.
%   When requested for the password, enter "secret".

https_server_with_client_cert(Port) :-
    http_server(reply,
                [ port(Port),
                  ssl([ certificate_file('etc/server/server-cert.pem'),
                        key_file('etc/server/server-key.pem'),
                        password("apenoot1"),
                        peer_cert(true),
                        cacert_file('etc/demoCA/cacert.pem'),
                        cert_verify_hook(client_cert_verify)
                      ])
                ]).

%!  client_cert_verify(+SSL,
%!                     +ProblemCert, +AllCerts, +FirstCer,
%!                     +Error) is semidet.
%
%   This hook is called for  validating   the  peer certificate. The
%   certificate has already been validated   against the known _root
%   certificates_ and Error either contains   `verified` to indicate
%   that this part of the  validation   was  successful or a message
%   from OpenSSL indicating why it did   not verify. The certificate
%   is accepted if this hook  succeeds,   regardless  of the initial
%   verification. If this hook  is   not  defined, certificates that
%   verify against the root certificates are accepted and others are
%   rejected, i.e., the default implementation  behaves as `Error ==
%   verified`.

% :- debug(cert_verify_hook).

:- public
    client_cert_verify/5.

client_cert_verify(_SSL, _Problem, _AllCerts, First, Error) :-
    debug(cert_verify_hook,
          'Handling client certificate verification~n\c
              Certificate: ~p, error: ~w~n', [First, Error]).


                 /*******************************
                 *            CLIENTS           *
                 *******************************/

:- use_module(library(http/http_open)).

%!  http_client(+Port, +Page) is det.
%
%   Access the server created with http_server/1. Note that the only
%   significant difference to  https_client/2  is   the  URL  scheme
%   (`http` vs. `https`.

http_client(Port, Page) :-
    format(atom(URL), 'http://localhost:~d~w', [Port, Page]),
    http_open(URL, In,
              [
                  ]),
    copy_stream_data(In, current_output),
    close(In).

%!  https_client(+Port, +Page) is det.
%
%   Access the server created with https_server/1.  Note that, as we
%   are using a self-signed  certificate,  we   pass  our  own  root
%   certificate instead of using the system one.

https_client(Port, Page) :-
    format(atom(URL), 'https://localhost:~d~w', [Port, Page]),
    http_open(URL, In,
              [ cacert_file('etc/demoCA/cacert.pem')
              ]),
    copy_stream_data(In, current_output),
    close(In).

%!  https_client_with_client_cert(+Port, +Page) is det.
%
%   Access the server created  with https_server_with_client_cert/1,
%   providing our client certificate.

https_client_with_client_cert(Port, Page) :-
    format(atom(URL), 'https://localhost:~d~w', [Port, Page]),
    http_open(URL, In,
              [ cacert_file('etc/demoCA/cacert.pem'),
                certificate_file('etc/client/client-cert.pem'),
                key_file('etc/client/client-key.pem'),
                password('apenoot2')
              ]),
    copy_stream_data(In, current_output),
    close(In).
