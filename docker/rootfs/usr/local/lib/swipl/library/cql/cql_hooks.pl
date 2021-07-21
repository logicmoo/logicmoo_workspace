/*  Part of SWI-Prolog

    Author:        Mike Elston
                   Matt Lilley
    E-mail:        matt.s.lilley@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, Mike Elston, Matt Lilley
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

/*  PostgreSQL is a trademark of the PostgreSQL Global Development Group.
    Microsoft, SQL Server, and Windows are either registered trademarks or
    trademarks of Microsoft Corporation in the United States and/or other
    countries. SQLite is a registered trademark of Hipp, Wyrick & Company,
    Inc in the United States. All other trademarks or registered trademarks
    are the property of their respective owners.
*/

:-module(cql_hooks, [application_value_to_odbc_value_hook/7,
                     odbc_value_to_application_value_hook/7,
                     cql_update_history_hook/16
                     ]).

:-multifile(odbc_value_to_application_value_hook/7).
:-multifile(application_value_to_odbc_value_hook/7).


%%      cql_update_history_hook(+Schema,
%%                              +TableName,
%%                              +AttributeName,
%%                              +PrimaryKeyAttributeName,
%%                              +PrimaryKeyValue,
%%                              +ApplicationValueBefore,
%%                              +ApplicationValueAfter,
%%                              +AccessToken,
%%                              +UserId,
%%                              +UserIpAddress,
%%                              +TransactionId,
%%                              +TransactionTimestamp,
%%                              +ThreadId,
%%                              +Spid,
%%                              +Connection,
%%                              +Goal).
%
%       Use this hook predicate to actually record database attribute value changes.
%
%       You are free to let this predicate fail or raise an exception - the
%       database layer will ignore both of these eventualities.
%
%       @param Schema <atom>
%       @param TableName <atom> (lower case)
%       @param AttributeName <atom> (lower case)
%       @param PrimaryKeyAttributeName <atom> (lower case)
%       @param PrimaryKeyValue <int>
%       @param ApplicationValueBefore <domain dependent>
%       @param ApplicationValueAfter <domain dependent>
%       @param AccessToken <atom>
%       @param UserId <atom>
%       @param UserIpAddress <atom>
%       @param TransactionId <atom>
%       @param TransactionTimestamp <t7/7>
%       @param ThreadId <atom>
%       @param Spid <int>
%       @param Connection <opaque>
%       @param Goal <goal term> The goal passed to pri_db_trans
:-multifile(cql_update_history_hook/16).
