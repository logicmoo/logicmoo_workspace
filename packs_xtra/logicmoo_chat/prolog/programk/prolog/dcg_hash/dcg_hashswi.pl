/*
	This Module is a modified version of SWI module '$dcg'.
	
	Optimisations and caching are disabled. 
	Is used by dcg_term_expansion to expand grammar bodies. 
*/
/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2016, University of Amsterdam
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

:- module(dcg_hashswi,
          [ dcg_translate_rule_/2       % +Rule, -Clause
          ]).

:-  absolute_file_name(swi(boot/dcg),DcgFile), 
    ensure_loaded(DcgFile).

                /********************************
                *        GRAMMAR RULES          *
                *********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The DCG compiler. The original code was copied from C-Prolog and written
by Fernando Pereira, EDCAAD, Edinburgh,  1984.   Since  then many people
have modified and extended this code. It's a nice mess now and it should
be redone from scratch. I won't be doing   this  before I get a complete
spec explaining all an implementor needs to   know  about DCG. I'm a too
basic user of this facility myself (though   I  learned some tricks from
people reporting bugs :-)

The original version contained '$t_tidy'/2  to   convert  ((a,b),  c) to
(a,(b,c)), but as the  SWI-Prolog  compiler   doesn't  really  care (the
resulting code is simply the same), I've removed that.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
	DCG Term Expansion
	Modified DCG Expansion
	
	dcg_translate_rule_/2
	'$dcg':dcg_translate_rule/2 without optimisation
	Original can be found here:
	http://www.swi-prolog.org/pldoc/doc/_SWI_/boot/dcg.pl?show=src#dcg_translate_rule/2
*/
dcg_translate_rule_(Rule, Clause) :-
	dcg_translate_rule_(Rule, _, Clause, _).
dcg_translate_rule_(((LP,MNT)-->RP), Pos0, (H:-B), Pos) :-
	!,
	dcg_hashswi:f2_pos(Pos0, PosH0, PosRP0, Pos, PosH, PosRP),
	dcg_hashswi:f2_pos(PosH0, PosLP0, PosMNT0, PosH, PosLP, PosMNT),
	'$current_source_module'(M),
	Qualify = q(M,M,_),
	dcg_extend_(LP, PosLP0, S0, SR, H, PosLP),
	dcg_body_(RP, PosRP0, Qualify, S0, S1, B0, PosRP),
	dcg_body_(MNT, PosMNT0, Qualify, SR, S1, B1, PosMNT),
	B = (B0, B1).
	%dcg_optimise((B0,B1),B2,S0),
	%dcg_optimise(B2,B,SR).
dcg_translate_rule_((LP-->RP), Pos0, (H:-B), Pos) :-
	dcg_hashswi:f2_pos(Pos0, PosLP0, PosRP0, Pos, PosLP, PosRP),
	dcg_extend_(LP, PosLP0, S0, S, H, PosLP),
	'$current_source_module'(M),
	Qualify = q(M,M,_),
	dcg_body_(RP, PosRP0, Qualify, S0, S, B, PosRP).
	%dcg_optimise(B0,B,S0).

%!  dcg_optimise(+BodyIn, -Body, +S0) is det.
%
%   Performs the following translations:
%
%     - a(H,T) :- H = [a,b|T], b(T), ... into
%       a([a,b|T0]) :- b(T0, T).
%     - a(H,T) :- H = [a,b|T] into
%       a([a,b|T0])
%
%   @arg S0 is the initial input list of the rule.
/*
dcg_optimise((S00=X,B), B, S0) :-
    S00 == S0,
    !,
    S0 = X.
dcg_optimise(S00=X, B, S0) :-
    S00 == S0,
    !,
    S0 = X,
    B = true.
dcg_optimise(B, B, _).
*/

%!  dcg_body(:DCG, ?Pos0, +Qualify, ?List, ?Tail, -Goal, -Pos) is det.
%
%   Translate DCG body term.

/*
	dcg_body_/7
	
	'$dcg':dcg_body/7 without caching
	http://www.swi-prolog.org/pldoc/doc/_SWI_/boot/dcg.pl?show=src#dcg_body/7
*/
dcg_body_(Var, P0, Q, S, SR, phrase(QVar, S, SR), P) :-
    var(Var),
    !,
    dcg_hashswi:qualify(Q, Var, P0, QVar, P).
dcg_body_(M:X, Pos0, q(_,C,_), S, SR, Ct, Pos) :-
    !,
    dcg_hashswi:f2_pos(Pos0, _, XP0, _, _, _),
    dcg_body_(X, XP0, q(M,C,Pos0), S, SR, Ct, Pos).
dcg_body_([], P0, _, S, SR, S=SR, P) :-         % Terminals
    !,
    dcg_hashswi:dcg_terminal_pos(P0, P).
dcg_body_(List, P0, _, S, SR, C, P) :-
    (   List = [_|_]
    ->  !,
        (   is_list(List)
        ->  '$append'(List, SR, OL),        % open the list
            C = (S = OL)
        ;   '$skip_list'(_, List, Tail),
            var(Tail)
        ->  C = '$append'(List, SR, S)      % TBD: Can be optimized
        ;   '$type_error'(list_or_partial_list, List)
        )
    ;   string(List)                        % double_quotes = string
    ->  !,
        string_codes(List, Codes),
        '$append'(Codes, SR, OL),
        C = (S = OL)
    ),
    dcg_hashswi:dcg_terminal_pos(P0, P).
dcg_body_(!, P0, _, S, SR, (!, SR = S), P) :-
    !,
    dcg_hashswi:dcg_cut_pos(P0, P).
dcg_body_({}, P, _, S, S, true, P) :- !.
dcg_body_({T}, P0, Q, S, SR, (QT, SR = S), P) :-
    !,
    dcg_hashswi:dcg_bt_pos(P0, P1),
    dcg_hashswi:qualify(Q, T, P1, QT, P).
dcg_body_((T,R), P0, Q, S, SR, (Tt, Rt), P) :-
    !,
    dcg_hashswi:f2_pos(P0, PA0, PB0, P, PA, PB),
    dcg_body_(T, PA0, Q, S, SR1, Tt, PA),
    dcg_body_(R, PB0, Q, SR1, SR, Rt, PB).
dcg_body_((T;R), P0, Q, S, SR, (Tt;Rt), P) :-
    !,
    dcg_hashswi:f2_pos(P0, PA0, PB0, P, PA, PB),
    dcg_body_(T, PA0, Q, S, S1, T1, PA), dcg_hashswi:or_delay_bind(S, SR, S1, T1, Tt),
    dcg_body_(R, PB0, Q, S, S2, R1, PB), dcg_hashswi:or_delay_bind(S, SR, S2, R1, Rt).
dcg_body_((T|R), P0, Q, S, SR, (Tt;Rt), P) :-
    !,
    dcg_hashswi:f2_pos(P0, PA0, PB0, P, PA, PB),
    dcg_body_(T, PA0, Q, S, S1, T1, PA), dcg_hashswi:or_delay_bind(S, SR, S1, T1, Tt),
    dcg_body_(R, PB0, Q, S, S2, R1, PB), dcg_hashswi:or_delay_bind(S, SR, S2, R1, Rt).
dcg_body_((C->T), P0, Q, S, SR, (Ct->Tt), P) :-
    !,
    dcg_hashswi:f2_pos(P0, PA0, PB0, P, PA, PB),
    dcg_body_(C, PA0, Q, S, SR1, Ct, PA),
    dcg_body_(T, PB0, Q, SR1, SR, Tt, PB).
dcg_body_((C*->T), P0, Q, S, SR, (Ct*->Tt), P) :-
    !,
    dcg_hashswi:f2_pos(P0, PA0, PB0, P, PA, PB),
    dcg_body_(C, PA0, Q, S, SR1, Ct, PA),
    dcg_body_(T, PB0, Q, SR1, SR, Tt, PB).
dcg_body_((\+ C), P0, Q, S, SR, (\+ Ct, SR = S), P) :-
    !,
    dcg_hashswi:f1_pos(P0, PA0, P, PA),
    dcg_body_(C, PA0, Q, S, _, Ct, PA).
dcg_body_(T, P0, Q, S, SR, QTt, P) :-
    dcg_extend_(T, P0, S, SR, Tt, P1),
    dcg_hashswi:qualify(Q, Tt, P1, QTt, P).

or_delay_bind(S, SR, S1, T, (T, SR=S)) :-
    S1 == S,
    !.
or_delay_bind(_S, SR, SR, T, T).

%!  qualify(+QualifyInfo, +Goal, +Pos0, -QGoal, -Pos) is det.
%
%   @arg QualifyInfo is a term   q(Module,Context,Pos), where Module
%   is the module in which Goal must   be  called and Context is the
%   current source module.

qualify(q(M,C,_), X0, Pos0, X, Pos) :-
    M == C,
    !,
    X = X0,
    Pos = Pos0.
qualify(q(M,_,MP), X, Pos0, M:X, Pos) :-
    dcg_qualify_pos(Pos0, MP, Pos).


%!  dcg_extend(+Head, +Extra1, +Extra2, -NewHead)
%
%   Extend Head with two more arguments (on behalf DCG compilation).
%   The solution below is one option. Using   =..  and append is the
%   alternative. In the current version (5.3.2), the =.. is actually
%   slightly faster, but it creates less garbage.

%:- dynamic  dcg_extend_cache/4.
%:- volatile dcg_extend_cache/4.

dcg_no_extend([]).
dcg_no_extend([_|_]).
dcg_no_extend({_}).
dcg_no_extend({}).
dcg_no_extend(!).
dcg_no_extend((\+_)).
dcg_no_extend((_,_)).
dcg_no_extend((_;_)).
dcg_no_extend((_|_)).
dcg_no_extend((_->_)).
dcg_no_extend((_*->_)).
dcg_no_extend((_-->_)).

%!  dcg_extend(:Rule, ?Pos0, ?List, ?Tail, -Head, -Pos) is det.
%
%   Extend a non-terminal with the   DCG  difference list List\Tail.
%   The position term is extended as well   to reflect the layout of
%   the created term. The additional variables   are  located at the
%   end of the Rule.

/*
	'$dcg':dcg_extend/6 without caching
	
	Original: http://www.swi-prolog.org/pldoc/doc/_SWI_/boot/dcg.pl?show=src#dcg_extend/4
	
	The built-in caching stores extended bodies in 'dcg_extend_cache/4' facts. 
	Was disabled since it caused the expansion inside dcg_tracer:phrase_mi/4 to behave faulty. 
*/
dcg_extend_(V, _, _, _, _, _) :-
	var(V),
	!,
	throw(error(instantiation_error,_)).
dcg_extend_(M:OldT, Pos0, A1, A2, M:NewT, Pos) :-
	!,
	dcg_hashswi:f2_pos(Pos0, MPos, P0, Pos, MPos, P),
	dcg_extend_(OldT, P0, A1, A2, NewT, P).
%dcg_extend(OldT, P0, A1, A2, NewT, P) :-
%    dcg_extend_cache(OldT, A1, A2, NewT),
%    !,
%    extended_pos(P0, P).
dcg_extend_(OldT, P0, A1, A2, NewT, P) :-
	(   callable(OldT)
	->  true
	;   throw(error(type_error(callable,OldT),_))
	),
	(   dcg_hashswi:dcg_no_extend(OldT)
	->  throw(error(permission_error(define,dcg_nonterminal,OldT),_))
	;   true
	),
	(   compound(OldT)
	->  compound_name_arity(OldT, Name, Arity),
		compound_name_arity(CopT, Name, Arity)
	;   CopT = OldT,
		Name = OldT,
		Arity = 0
	),
	NewArity is Arity+2,
	functor(NewT, Name, NewArity),
	dcg_hashswi:copy_args(1, Arity, CopT, NewT),
	A1Pos is Arity+1,
	A2Pos is Arity+2,
	arg(A1Pos, NewT, A1C),
	arg(A2Pos, NewT, A2C),
	%assert(dcg_extend_cache(CopT, A1C, A2C, NewT)),
	OldT = CopT,
	A1C = A1,
	A2C = A2,
	dcg_hashswi:extended_pos(P0, P).

copy_args(I, Arity, Old, New) :-
    I =< Arity,
    !,
    arg(I, Old, A),
    arg(I, New, A),
    I2 is I + 1,
    copy_args(I2, Arity, Old, New).
copy_args(_, _, _, _).


                 /*******************************
                 *        POSITION LOGIC        *
                 *******************************/

extended_pos(Pos0, Pos) :-
    '$expand':extended_pos(Pos0, 2, Pos).
f2_pos(Pos0, A0, B0, Pos, A, B) :- '$expand':f2_pos(Pos0, A0, B0, Pos, A, B).
f1_pos(Pos0, A0, Pos, A) :- '$expand':f1_pos(Pos0, A0, Pos, A).

%!  dcg_bt_pos(?BraceTermPos, -Pos) is det.
%
%   Position transformation for mapping of {G} to (G, S=SR).

dcg_bt_pos(Var, Var) :-
    var(Var),
    !.
dcg_bt_pos(brace_term_position(F,T,P0),
           term_position(F,T,F,F,
                         [ P0,
                           term_position(T,T,T,T,_)
                         ])) :- !.
dcg_bt_pos(Pos, _) :-
    expected_layout(brace_term, Pos).

dcg_cut_pos(Var, Var) :-
    var(Var),
    !.
dcg_cut_pos(F-T, term_position(F,T,F,T,
                               [ F-T,
                                 term_position(T,T,T,T,_)
                               ])).
dcg_cut_pos(Pos, _) :-
    expected_layout(atomic, Pos).

%!  dcg_terminal_pos(+ListPos, -TermPos)

dcg_terminal_pos(Pos, _) :-
    var(Pos),
    !.
dcg_terminal_pos(list_position(F,T,_Elms,_Tail),
                 term_position(F,T,_,_,_)).
dcg_terminal_pos(F-T,
                 term_position(F,T,_,_,_)).
dcg_terminal_pos(Pos, _) :-
    expected_layout(terminal, Pos).

%!  dcg_qualify_pos(?TermPos0, ?ModuleCreatingPos, -TermPos)

dcg_qualify_pos(Var, _, _) :-
    var(Var),
    !.
dcg_qualify_pos(Pos,
                term_position(F,T,FF,FT,[MP,_]),
                term_position(F,T,FF,FT,[MP,Pos])) :- !.
dcg_qualify_pos(_, Pos, _) :-
    expected_layout(f2, Pos).

expected_layout(Expected, Found) :-
    '$expand':expected_layout(Expected, Found).

