/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2015, University of Amsterdam
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

:- module(xsdp_type,
          [ xsdp_type/1,                % ?Type
            xsdp_uri_type/2,            % ?URI, ?Type
            xsdp_numeric_uri/2,         % ?URI, ?Primary
            xsdp_subtype_of/2,          % ?Type, ?Super
            xsdp_convert/3              % +Type, +Content, -Value
          ]).

/** <module> XML-Schema primitive types

This  modules  provides  support  for  the  primitive  XML-Schema  (XSD)
datatypes. It defines the type hierarchy which allows for reasoning over
types as well as xsdp_convert/3 to  convert   XML  content  to a natural
Prolog representation of the XSD type.

Based on the W3C definitions at

        * http://www.w3.org/TR/xmlschema-2/#built-in-datatypes

The current implementation is incomplete and only  there to test the API
and its integration with rdf:dataType=Type handling in the RDF parser.

The extra 'p' in the module  prefix  (xsdp_*)   is  used  to allow for a
module xsd_*, providing full  user-defined  XSD   types  on  top of this
module.
*/

ns('http://www.w3.org/2001/XMLSchema#').


                 /*******************************
                 *    PRIMITIVE TYPE HIERARCHY  *
                 *******************************/

%!  xsdp_type(?Type)
%
%   Test/generate the names for the XML schema primitive types

xsdp_type(Type) :-
    subtype_of(Type, _).

%!  xsdp_uri_type(?URI, ?Type)
%
%   True if URI is the URI for the the XML-Schema primitive Type.

xsdp_uri_type(URI, Type) :-
    xsd_local_id(URI, Type),
    subtype_of(Type, _).

%!  xsdp_subtype_of(?Type, ?Super)
%
%   True if Type is a (transitive) subtype of Super.

xsdp_subtype_of(Type, Type) :-
    subtype_of(Type, _).
xsdp_subtype_of(Type, Super) :-
    (   nonvar(Type)
    ->  subtype_of(Type, Super0),
        Super0 \== (-),
        xsdp_subtype_of(Super0, Super)
    ;   nonvar(Super)
    ->  subtype_of(Sub0, Super),
        xsdp_subtype_of(Type, Sub0)
    ;   subtype_of(Type, _),
        xsdp_subtype_of(Type, Super)
    ).

subtype_of(anyType,            -).
subtype_of(anySimpleType,      anyType).
                                        % string hierarchy
subtype_of(string,             anySimpleType).
subtype_of(normalizedString,   string).
subtype_of(token,              normalizedString).
subtype_of(language,           token).
subtype_of('Name',             token).
subtype_of('NCName',           'Name').
subtype_of('NMTOKEN',          token).
subtype_of('NMTOKENS',         'NMTOKEN').
subtype_of('ID',               'NCName').
subtype_of('IDREF',            'NCName').
subtype_of('IDREFS',           'IDREF').
subtype_of('ENTITY',           'NCName').
subtype_of('ENTITIES',         'ENTITY').
                                        % numeric hierarchy
subtype_of(decimal,            anySimpleType).
subtype_of(integer,            decimal).
subtype_of(nonPositiveInteger, integer).
subtype_of(negativeInteger,    nonPositiveInteger).
subtype_of(long,               integer).
subtype_of(int,                long).
subtype_of(short,              int).
subtype_of(byte,               short).
subtype_of(nonNegativeInteger, integer).
subtype_of(unsignedLong,       nonNegativeInteger).
subtype_of(positiveInteger,    nonNegativeInteger).
subtype_of(unsignedInt,        unsignedLong).
subtype_of(unsignedShort,      unsignedInt).
subtype_of(unsignedByte,       unsignedShort).
                                        % other simple types
subtype_of(duration,           anySimpleType).
subtype_of(dateTime,           anySimpleType).
subtype_of(time,               anySimpleType).
subtype_of(date,               anySimpleType).
subtype_of(gYearMonth,         anySimpleType).
subtype_of(gYear,              anySimpleType).
subtype_of(gMonthDay,          anySimpleType).
subtype_of(gDay,               anySimpleType).
subtype_of(gMonth,             anySimpleType).
subtype_of(boolean,            anySimpleType).
subtype_of(base64Binary,       anySimpleType).
subtype_of(hexBinary,          anySimpleType).
subtype_of(float,              anySimpleType).
subtype_of(double,             anySimpleType).
subtype_of(anyURI,             anySimpleType).
subtype_of('QName',            anySimpleType).
subtype_of('NOTATION',         anySimpleType).

%!  xsdp_numeric_uri(?URI, -PromoteURI) is nondet.
%
%   Table mapping all XML-Schema numeric  URIs   into  the type they
%   promote to. Types are promoted   to =integer=, =float=, =double=
%   and =decimal=.

term_expansion(integer_types, Clauses) :-
    findall(integer_type(Type), xsdp_subtype_of(Type, integer), Clauses).
term_expansion(xsd_local_ids, Clauses) :-
    ns(NS),
    findall(xsd_local_id(URI, Type),
            (   xsdp_type(Type),
                atom_concat(NS, Type, URI)
            ),
            Clauses).
term_expansion(numeric_uirs, Clauses) :-
    findall(xsdp_numeric_uri(URI, PrimaryURI),
            (   (   integer_type(Type),     Primary = integer
                ;   Type = float,           Primary = float
                ;   Type = double,          Primary = double
                ;   Type = decimal,         Primary = decimal
                ),
                xsd_local_id(URI, Type),
                xsd_local_id(PrimaryURI, Primary)
            ),
            Clauses).

integer_types.
xsd_local_ids.

numeric_uirs.

%!  xsdp_convert(+Type, +Content, -Value)
%
%   Convert the content model Content to an  object of the given XSD
%   type and return the Prolog value in Value.

xsdp_convert(URI, Content, Value) :-
    (   xsd_local_id(URI, Type)
    ->  convert(Type, Content, Value)
    ;   convert(URI, Content, Value)
    ).

convert(anyType, Term, Term) :- !.
convert(anySimpleType, [Simple], Simple) :- !.
                                        % strings
convert(string, [String], String) :- !.
                                        % numbers
convert(IntType, [Text], Integer) :-
    integer_type(IntType),
    !,
    atom_number(Text, Integer),
    (   integer(Integer),
        validate_int_domain(IntType, Integer)
    ->  true
    ;   throw(error(domain_error(Text, IntType), _))
    ).
convert(float, [Text], Float) :-
    !,
    atom_number(Text, Number),
    Float is float(Number).
convert(double, [Text], Float) :-
    !,
    atom_number(Text, Number),
    Float is float(Number).
convert(_Any, [X], X) :- !.             % TBD: provide for more types
convert(_Any, X, X).

validate_int_domain(integer, _).
validate_int_domain(int, _).
validate_int_domain(long, _).
validate_int_domain(nonPositiveInteger, I) :- \+ I > 0.
validate_int_domain(negativeInteger, I) :-    I < 0.
validate_int_domain(short, I) :-              between(-32768, 32767, I).
validate_int_domain(byte,  I) :-              between(-128, 127, I).
validate_int_domain(nonNegativeInteger, I) :- \+ I < 0.
validate_int_domain(unsignedLong,       I) :- I >= 0.
validate_int_domain(positiveInteger,    I) :- I > 0.
validate_int_domain(unsignedInt,        I) :- I >= 0.
validate_int_domain(unsignedShort,      I) :- between(0, 65535, I).
validate_int_domain(unsignedByte,       I) :- between(0, 255, I).
