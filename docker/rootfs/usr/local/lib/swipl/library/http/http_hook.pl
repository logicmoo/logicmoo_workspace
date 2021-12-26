/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
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

:- module(http_hook,
          []).

/** <module> HTTP library hooks

Get the declarations of the HTTP package using

    ==
    :- use_module(library(http/http_hook)).
    ==

@tbd    This should be using include, but then it cannot be a module
        and this would cause more overhead in SWI-Prolog
@tbd    Complete this and document the hooks.
*/

                 /*******************************
                 *           HTTP-PATH          *
                 *******************************/

:- multifile http:location/3.
:- dynamic   http:location/3.


                 /*******************************
                 *           HTML-WRITE         *
                 *******************************/

:- multifile
    html_write:expand//1,
    html_write:expand_attribute_value//1,
    html_write:html_head_expansion/2,
    html_write:layout/3.


                 /*******************************
                 *         HTTP-DISPATCH        *
                 *******************************/

:- multifile
    http:authenticate/3.


                 /*******************************
                 *       HTTP-PARAMETERS        *
                 *******************************/

%!  http:convert_parameter(+Type, +ValueIn, -ValueOut) is semidet.
%
%   Hook to execute a step in the HTTP parameter conversion process.
%
%   @see http_parameters:check_type/4.

:- multifile
    http:convert_parameter/3.


                 /*******************************
                 *            PROXIES           *
                 *******************************/

%!  http:http_connection_over_proxy(+Proxy, +URLParts, +Endpoint,
%!                                  -StreamPair, +Options, -NewOptions).
%
%   Try to connect to the host Endpoint   via Proxy for the purposes
%   of retrieving the resource  identified   by  URLParts. Different
%   options can be returned in NewOptions,  which may be required if
%   you   have   defined   a   non-standard     proxy    method   in
%   socket:proxy_for_url/3 (such as one requiring authentication)

:- multifile
    http:http_connection_over_proxy/6.
