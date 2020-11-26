%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- module(pfcsyntax, [
    op(500,fx,'~'),
    op(1050,xfx,('==>')),
    op(1050,xfx,'<==>'),
    op(1050,xfx,('==>')),
    op(1050,xfx,'==>'),
    op(1050,xfx,('<-')),
    op(1050,xfx,('<-')),
    op(1100,fx,('==>')),
    op(1150,xfx,('::::'))]).
:- use_module(library(pfc_pack_xform)).

:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

/*
:- multifile('term_expansion'/2).

term_expansion((P==>Q),(:- add((P==>Q)))).
%term_expansion((P==>Q),(:- add(('<-'(Q,P))))).  % speed-up attempt
term_expansion(('<-'(P,Q)),(:- add(('<-'(P,Q))))).
term_expansion((P<==>Q),(:- add((P<==>Q)))).
term_expansion((RuleName :::: Rule),(:- add((RuleName :::: Rule)))).
term_expansion((==>P),(:- add(P))).
*/


