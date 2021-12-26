/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013, VU University Amsterdam
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

:- module(http_ax,
          [ http_ax_attributes/2,       % +Spec, -AttributeList
            ax_form_attributes/2        % +Form, -Values
          ]).
:- use_module(library(error)).


/** <module> Attribute Exchange library

This library can be used to create   HTTP request parameters and analyse
form-data for _attribute exchange_. Attribute exchange   (AX) is used by
OpenID and OAuth to fetch attributes  for   accounts,  such  as the real
username or e-mail address.
*/

%!  http_ax_attributes(+Spec, -HTTPAttributes) is det.
%
%   True when HTTPAttributes is a list  of Name=Value pairs that can
%   be used with an HTTP request to   query for the attributes Spec.
%   Spec is a list of elements =|Alias(Value[, Options])|=.  Options
%   include:
%
%     - required
%     The attribute is required.  This is mutually exclusive
%     with =if_available=.
%     - if_available
%     Only provide the attribute if it is available. This is
%     mutually exclusive with =required=.  This is the default.
%     - url(+URL)
%     Can be used to ovcerrule or extend the ax_alias/2.
%     - count(+Count)
%     Maximum number of values to provide
%
%   For example:
%
%       ==
%       ?- http_ax_attributes([ nickname(Nick),
%                               email(Email, [required])
%                             ], Params).
%       Params = [ 'openid.ax.mode'          = fetch_request,
%                  'openid.ax.type.nickname' = 'http://axschema.org/namePerson/friendly',
%                  'openid.ax.type.email'    = 'http://axschema.org/contact/email',
%                  'openid.ax.required'      = email,
%                  'openid.ax.if_available'  = nickname
%                ].
%       ==

http_ax_attributes(Spec, [ 'openid.ns.ax'   = 'http://openid.net/srv/ax/1.0',
                           'openid.ax.mode' = fetch_request
                         | AllAttr
                         ]) :-
    maplist(type_alias, Spec, AliasAttrs),
    partition(required, Spec, Required, Optional),
    alias_list(Required, 'openid.ax.required', RequiredAttr),
    alias_list(Optional, 'openid.ax.if_available', IfAvailableAttr),
    count_attr(Spec, CountAttr),
    append([AliasAttrs, RequiredAttr, IfAvailableAttr, CountAttr], AllAttr).

type_alias(Spec, Attr=URL) :-
    functor(Spec, Alias, Arity),
    (   Arity > 1,
        arg(2, Spec, Options),
        memberchk(url(URL), Options)
    ->  true
    ;   ax_alias(Alias, URL)
    ->  true
    ;   existence_error(ax_alias, Alias)
    ),
    atom_concat('openid.ax.type.', Alias, Attr).

required(Spec) :-
    functor(Spec, _, 2),
    arg(2, Spec, Options),
    memberchk(required, Options).

alias_list([], _, []).
alias_list(Specs, A, [A=V]) :-
    maplist(alias_name, Specs, Aliases),
    atomic_list_concat(Aliases, ',', V).

alias_name(Spec, Alias) :-
    functor(Spec, Alias, _).

count_attr([], []).
count_attr([Spec|T0], [A=Count|T]) :-
    functor(Spec, Alias, 2),
    arg(2, Spec, Options),
    memberchk(count(Count), Options),
    !,
    atomic_list_concat('openid.ax.count.', Alias, A),
    count_attr(T0, T).
count_attr([_|T0], T) :-
    count_attr(T0, T).


%!  ax_alias(?Alias, ?URL) is nondet.
%
%   True when Alias is an alias  name   for  the AX schema URL. This
%   predicate is defined as _multifile_.
%
%   Note  that  Google  federated  login    only  supports  =email=,
%   =country=, =language=, =firstname= and =lastname=.

:- multifile
    ax_alias/2.

ax_alias(nickname,  'http://axschema.org/namePerson/friendly').
ax_alias(email,     'http://axschema.org/contact/email').
ax_alias(fullname,  'http://axschema.org/namePerson').
ax_alias(dob,       'http://axschema.org/birthDate').
ax_alias(gender,    'http://axschema.org/person/gender').
ax_alias(postcode,  'http://axschema.org/contact/postalCode/home').
ax_alias(country,   'http://axschema.org/contact/country/home').
ax_alias(language,  'http://axschema.org/pref/language').
ax_alias(timezone,  'http://axschema.org/pref/timezone').
ax_alias(prefix,    'http://axschema.org/namePerson/prefix').
ax_alias(firstname, 'http://axschema.org/namePerson/first').
ax_alias(lastname,  'http://axschema.org/namePerson/last').
ax_alias(suffix,    'http://axschema.org/namePerson/suffix').


                 /*******************************
                 *            RESPONSE          *
                 *******************************/

%!  ax_form_attributes(+Form, -Values) is det.
%
%   True if Values  is  a  list   Alias(Value)  for  each  exchanged
%   attribute.
%
%   Note that we assume we get the same   alias names as we used for
%   requesting the data. Not sure whether this is true.
%
%   @arg    Form is an HTTP form as returned using the form(Form)
%           option of http_parameters/3.

ax_form_attributes(Form, Values) :-
    (   memberchk('openid.ax.mode'=fetch_response, Form)
    ->  Ext = ax
    ;   memberchk(ExtNS='http://openid.net/srv/ax/1.0', Form),
        atomic_list_concat([openid,ns,Ext], '.', ExtNS)
    ->  true
    ),
    ax_attributes(Form, Ext, Values).
ax_form_attributes(_, []).

ax_attributes([], _, []).
ax_attributes([Name=Value|T0], Ext, AXs) :-
    atomic_list_concat([openid, Ext, value, Alias|_Num], '.', Name),
    !,
    AX =.. [Alias,Value],
    AXs = [AX|AXT],
    ax_attributes(T0, Ext, AXT).
ax_attributes([_|T0], Ext, AXs) :-
    ax_attributes(T0, Ext, AXs).
