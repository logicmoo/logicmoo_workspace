%% fuzzy ALC reasoning
%% using resolution as inference operator
%%
%% Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 6-2-2007
%% Copyright (C) 2007 Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along
%% with this program; if not, write to the Free Software Foundation, Inc.,
%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


:- module( resolution, [prove/5,
			yadlr_concept/2, yadlr_relation/2, yadlr_instance/2,
			yadlr_concept_name/2, yadlr_relation_name/2,
			yadlr_instance_name/2,
			yadlr_assert/3, yadlr_init/1,
                        retractKB/1,
			yadlr_retrieve_concept/2, yadlr_retrieve_instance/2,
			set_debug_msgs/1, set_depth_limit/1,
			set_proof_tree_log/1, unset_proof_tree_log/0] ).

:- user:use_algebra( AlgModule ), use_module( AlgModule ).


%%
%% SETTINGS
%%

:- dynamic use_debug_msgs/1,
	use_proof_tree_log/1, use_depth_limit/1.

use_debug_msgs( no ).

set_debug_msgs( yes ) :-
	retractall( use_debug_msgs(_) ),
	assert_if_new( use_debug_msgs(yes) ).
set_debug_msgs( no ) :-
	retractall( use_debug_msgs(_) ),
	assert_if_new( use_debug_msgs(no) ).

msg_debug( Msg ) :-
	use_debug_msgs( yes ),
	fmt( '~q~n', Msg ),
	!.
msg_debug( _ ) :-
	use_debug_msgs( no ),
	!.


use_proof_tree_log( no ).

set_proof_tree_log( no ) :-
	!,
	retractall( use_proof_tree_log(_) ),
	assert_if_new( use_proof_tree_log(no) ).
set_proof_tree_log( yes ) :-
	!,
	retractall( use_proof_tree_log(_) ),
	assert_if_new( use_proof_tree_log(yes) ),
	open( 'resolution.log', write,Stream, [type(text)] ), % ,alias(yadlr_logfile)
        asserta(yadlr_logfile(Stream)).

set_proof_tree_log( File ) :-
	retractall( use_proof_tree_log(_) ),
	assert_if_new( use_proof_tree_log(yes) ),        
	open( File, write, Stream, [type(text)] ), % ,alias(yadlr_logfile)
        asserta(yadlr_logfile(Stream)).

unset_proof_tree_log :-
	set_proof_tree_log( no ),
        yadlr_logfile(Stream),
	close( Stream ).

msg_proof_tree( Depth, StepType, OpenList, RestrVars ) :-
	use_proof_tree_log( yes ),
	yadlr_format_clauses( OpenList, Clauses ),
	fmt_range( RestrVars, Restr ),
	fmt( yadlr_logfile,
		'derivation(~d, ~q, ~q, ~q).~n',
		[Depth, StepType, Restr, Clauses] ),
	!.
msg_proof_tree( _, _, _, _ ) :-
	use_proof_tree_log( no ),
	!.


use_depth_limit( no ).

set_depth_limit( N ) :-
	integer( N ),
	retractall( use_depth_limit(_) ),
	assert_if_new( use_depth_limit(N) ).
set_depth_limit( no ) :-
	retractall( use_depth_limit(_) ),
	assert_if_new( use_depth_limit(no) ).
	


%% 
%% REPRESENTATION
%% 

% Literals are ylit(Polarity,Symbol) terms

yadlr_is_literal( ylit(pos, _) ).
yadlr_is_literal( ylit(neg, _) ).

yadlr_mk_literal( ylit(P,S), P, S ).

yadlr_mk_literals( [], _, [] ).
yadlr_mk_literals( [ylit(P,Head)|Rest1], P, [Head|Rest2] ) :-
	yadlr_mk_literals( Rest1, P, Rest2 ).


yadlr_lit_complement( ylit(pos,L), ylit(neg,L) ).
yadlr_lit_complement( ylit(neg,L), ylit(pos,L) ).

yadlr_lit_list_complement( [], [] ).
yadlr_lit_list_complement( [H|Rest], [NotH|NotRest] ) :-
	yadlr_lit_complement( H, NotH ),
	yadlr_lit_list_complement( Rest, NotRest ).


% Clauses are (Pos,Neg,Degree) tuples.
% Pos and Neg are sets of literals
% The abstract clause is the disjunction of union(Pos,Neg)
% Degree is the fuzzy degree

yadlr_mk_clause( [], Range, yclause([],[],Range) ).
yadlr_mk_clause( [ylit(pos,L)|Rest], Range,
	         yclause([ylit(pos,L)|RestPos], Neg, Range) ) :-
	yadlr_mk_clause( Rest, Range, yclause(RestPos,Neg,Range) ).
yadlr_mk_clause( [ylit(neg,L)|Rest], Range,
                 yclause(Pos, [ylit(neg,L)|RestNeg], Range) ) :-
	yadlr_mk_clause( Rest, Range, yclause(Pos,RestNeg,Range) ).

yadlr_clause_pos( yclause(Pos,_,_), Pos ).
yadlr_clause_neg( yclause(_,Neg,_), Neg ).
yadlr_clause_degree( yclause(_,_,Degree), Degree ).

yadlr_format_clauses( [], [] ).
yadlr_format_clauses( [yclause(Pos,Neg,Deg)|RestIn],
		      [yclause(Pos,Neg,FmtDeg)|RestOut] ) :-
	fmt_range( [Deg], FmtDeg ),
	yadlr_format_clauses( RestIn, RestOut ).

% The KB is yadlr_kb( Clause ) terms.
% TODO1: yadlr_kb( head, clause ) terms. head is redundant, but
% helps the prolog engine's index mechanism.
% TODO2: store the original abstract form, for log presentation


% init kb: cleans KB and asserts built-in relations
yadlr_init( KB ) :- 
	retractKB( KB ),
	% declare reserved symbols
	yadlr_concept( KB, dlalldifferent ).

% place grounds facts at the top
yadlr_assert_one( KB, Clause ) :-
	yadlr_clause_neg( Clause, [] ),
	!,
	db_recorda( KB, Clause, _ ).
yadlr_assert_one( KB, Clause ) :-
	db_recordz( KB, Clause, _ ).

yadlr_assert_many( _, [] ).
yadlr_assert_many( KB, [Head|Rest] ) :-
	yadlr_assert_one( KB, Head ),
	yadlr_assert_many( KB, Rest ).

% TODO: check if it already exists, and fail
yadlr_concept( KB, ConceptName ) :-
	db_recordz( KB, yconcept(ConceptName), _ ).

yadlr_relation( KB, RelationName ) :-
	db_recordz( KB, yrelation(RelationName), _ ).

yadlr_instance( KB, InstanceName ) :-
	db_recordz( KB, yinstance(InstanceName), _ ).

yadlr_concept_name( KB, ConceptName ) :-
	db_recorded( KB, yconcept(ConceptName), _ ).

yadlr_relation_name( KB, RelationName ) :-
	db_recorded( KB, yrelation(RelationName), _ ).

yadlr_instance_name( KB, InstanceName ) :-
	db_recorded( KB, yinstance(InstanceName), _ ).

yadlr_assert( KB, Formula, FuzzyDegree ) :-
	clausify_abstract( Formula, FuzzyDegree, Clauses ),
	yadlr_assert_many( KB, Clauses ).



% SWI Prolog does not have eraseall/1
retractKB( KB ) :-
	prolog_engine(swi),
	db_recorded( KB, _, Ref ),
	erase( Ref ),
	fail.
retractKB( _ ) :-
	prolog_engine(swi),
	!.
% SWI Prolog does not have eraseall/1
% retractKB( KB ) :- eraseall( KB ).

yadlr_retrieve_concept( KB, ConceptName ) :-
	db_recorded( KB, yconcept(ConceptName), _ ).

yadlr_retrieve_relation( KB, RelationName ) :-
	db_recorded( KB, yrelation(RelationName), _ ).

yadlr_retrieve_instance( KB, InstanceName ) :-
	db_recorded( KB, yinstance(InstanceName), _ ).

yadlr_retrieve_clause( KB, yclause(Pos,Neg,Deg) ) :-
	db_recorded( KB, yclause(Pos,Neg,Deg), _ ).
	

%%
%% UTILITIES
%%

:- use_module( [library(lists),nfsdl] ).


% SWI Prolog does not have remove_duplicates/2
% also consider file_search_path(swi, _) as a test,
% as the Prolog interpreter might have been renamed
/*
:- ( prolog_engine(swi) ->
     assert_if_new( remove_duplicates([], []) ),
     assert_if_new( (remove_duplicates([Elem|L], [Elem|NL]) :-
	     delete(L, Elem, Temp), remove_duplicates(Temp, NL)) )
   ;
     true
   ).
*/

complement_member( L1, [L2|_]) :-
	yadlr_lit_complement( L1, L2 ).
complement_member(Element, [_|Rest]) :-
        complement_member(Element, Rest).

% complement_intersection/5 (+List1, +List2, ?Intersection1, -Rest1, -Rest2)

complement_intersection( [],    _,  [],    _, _ ).
complement_intersection( [H|T], L2, [H|I], T, Rest2 ) :-
        complement_member( H, L2 ),
	!,
	yadlr_lit_complement( H, NotH ),
        complement_intersection( T, L2, I, T, R2 ),
	select( NotH, R2, Rest2 ).
complement_intersection( [H|T], L2, I, [H|R1], R2 ) :-
        complement_intersection( T, L2, I, R1, R2 ).

% straight_intersection/5 (+List1, +List2, ?Intersection, -Rest1, -Rest2)

straight_intersection( [],    L2, [],   [], L2 ).
straight_intersection( [H|T], L2, [H|I], T, Rest2 ) :-
        member( H, L2 ),
	!,
        straight_intersection( T, L2, I, T, R2 ),
	select( H, R2, Rest2 ).
straight_intersection( [H|T], L2, I, [H|R1], R2 ) :-
        straight_intersection( T, L2, I, R1, R2 ).

% C1 is a subset of C2, and C1 is more certain

fuzzy_subsumes( Clause1, Clause2, Residue ) :-
	yadlr_clause_degree( Clause1, D1 ),
	yadlr_clause_degree( Clause2, D2 ),
	less_fuzzy( D1, D2 ),
	yadlr_clause_pos( Clause1, Pos1 ),
	yadlr_clause_pos( Clause2, Pos2 ),
	permutation( Pos1, P1 ),
	append( P1, PosRes, Pos2 ),
	yadlr_clause_neg( Clause1, Neg1 ),
	yadlr_clause_neg( Clause2, Neg2 ),
	permutation( Neg1, N1 ),
	append( N1, NegRes, Neg2 ),
	append( PosRes, NegRes, Residue ).


% clausify_literals( ?[Literals], ?Degree, ?[Clauses] )
%
% turns a list of literals into a list of single-literal clauses
% all clauses have fuzzy degree Degree

clausify_literals( [], _, []).
clausify_literals( [L|RestL], Degree, [C|RestC] ) :-
	yadlr_mk_clause( [L], Degree, C ),
	clausify_literals( RestL, Degree, RestC ).


% clausify_abstract( +Formula, +FuzzyDegree, -[YClauses] )
%
% turns an abstract formula into a list of yclauses

clausify_abstract( Formula, FuzzyDegree, Clauses ) :-
	is_fuzzy_degree( FuzzyDegree ),
	nnf( Formula, NNF ), pnf( NNF, PNF ), cf( PNF, CF ),
	cf_to_yclauses( CF, FuzzyDegree, Clauses ).

cf_to_yclauses( [], _, [] ).
cf_to_yclauses( [CF|T1], FuzzyDegree, [Clause|T2] ) :-
	cf_to_yclause( CF, FuzzyDegree, Clause ),
	cf_to_yclauses( T1, FuzzyDegree, T2 ).

cf_to_yclause( cl(Pos,Neg), FuzzyDegree, Clause ) :-
	yadlr_mk_literals( PosLits, pos, Pos ),
	yadlr_mk_literals( NegLits, neg, Neg ),
	append( PosLits, NegLits, Lits ),
	yadlr_mk_clause( Lits, FuzzyDegree, Clause ).


%%
%% RESOLUTION
%%

% Clause1-Clause2 are instantiated and complement-intersect
%
% NOTE: implementation guarantees that Clause1 is the one providing
% the pos literals. This is needed for fuzzy degree checking,
% do not attempt neg(Clause1)-pos(Clause2).

compatible( Clause1, Clause2, Inters, Residue ) :-
	var( Inters ),
	!,
	yadlr_clause_pos( Clause1, Pos ),
	yadlr_clause_neg( Clause2, Neg ),
	complement_intersection( Pos, Neg, Inters, Rest1, Rest2 ),
	append( Rest1, Rest2, Residue ).

% Clause1-Inters are instantiated and Inters is a subset of Clause1
% TODO: do not require Inters subset Clause1, to get non-definite resolution

compatible( Clause1, Clause2, Inters, Residue ) :-
	var( Clause2 ),
	!,
	yadlr_mk_clause( LitList1, _, Clause1 ),
	yadlr_mk_clause( LitListI, _, Inters ),
	straight_intersection( LitList1, LitListI, LitListI, R, [] ),
	yadlr_lit_list_complement( R, NotR ),
	% union(LitList2,Residue) must be NotR
	permutation( NotR, A ),
	append( LitList2, Residue, A ),
	% make a half-baked Clause2
	yadlr_mk_clause( LitList2, _, Clause2 ).

resolve( Clause1, Clause2, Res, RestrVarsIn, RestrVarsOut ) :-
	nonvar( Clause1 ), nonvar( Clause2 ), var( Res ),
	!,
	% look for the complement-intersection of Clause1
	% and Clause2.
	compatible( Clause1, Clause2, _Inters, Residue ),
	% check degrees
	yadlr_clause_degree( Clause1, Deg1 ),
	yadlr_clause_degree( Clause2, Deg2 ),
	less_fuzzy( Deg2, Deg1 ),
	% calculate degree range for Res clause
	tnorm( mp, Deg1, Deg2, DegR ),
	yadlr_mk_clause( Residue, DegR, Res ),
	(var(Deg1) -> append([Deg1], RestrVarsIn, RestrVars1) ; RestrVars1 =  RestrVarsIn),
	(var(Deg2) -> append([Deg2], RestrVars1, RestrVarsOut) ; RestrVarsOut =  RestrVars1).

resolve( Clause1, Clause2, Res, RestrVarsIn, RestrVarsOut ) :-	
	nonvar( Clause1 ), var( Clause2 ), nonvar( Res ),
	!,
	% Theorize about Clause2: Residue gets unified with
	% what needs to be proven to prove Res.
	% Clause2 gets unified with the perfect clause that
	% would do the job without any residue.
	compatible( Clause1, Clause2, Res, [] ),
	% check degrees
	yadlr_clause_degree( Clause1, Deg1 ),
	yadlr_clause_degree( Res, DegR ),
	less_fuzzy( Deg1, DegR ),
	tnorm( mp, Deg1, Deg2, DegR ),
	yadlr_clause_degree( Clause2, Deg2 ),
	(var(Deg1) ->
	 append([Deg1], RestrVarsIn, RestrVarsOut)
	;
	 RestrVarsIn = RestrVarsOut
	).


% resolution_step( +KB, +Clause, -OpenList )
% perform a single resolution step

resolution_step( KB, Clause, ResidualClauses, RestrIn, RestrOut ) :-
	yadlr_retrieve_clause( KB, Clause1 ),
	resolve( Clause1, Residue, Clause, RestrIn, RestrOut ),
	% Residue is a pos-only clause that remains to be
	% proven; turn into a list of atomic clauses
	yadlr_mk_clause( Literals, Degree, Residue ),
	% The fuzzy degree of the clause will be the
	% fuzzy degree of each clausified literal
	clausify_literals( Literals, Degree, ResidualClauses ).

tautology_check( [Clause|RestIn], RestOut ) :-
	% if a literal appears as both pos and neg,
	% then the clause is a tautology
	yadlr_clause_pos( Clause, Pos ),
	yadlr_clause_neg( Clause, Neg ),
	\+ complement_intersection( Pos, Neg, [], _, _ ),
	!,
	tautology_check( RestIn, RestOut ).
tautology_check( [Clause|RestIn], [Clause|RestOut] ) :-
	tautology_check( RestIn, RestOut ).


%%
%% makealldiff/1 ( +List )
%% ensures that all elements of are ununifiable
%%

checkalldiffhelper( _, [] ).
checkalldiffhelper( A, [B] ) :-	A \= B.
checkalldiffhelper( A, [H|R] ) :-
	A \= H,
	checkalldiffhelper( A, R ).

checkalldiff( [] ).
checkalldiff( [_] ).
checkalldiff( [H|R] ) :-
	checkalldiffhelper( H, R ),
	checkalldiff( R ).

has_vars( [H] ) :- var( H ).
has_vars( [H|R] ) :- var( H ) ; has_vars( R ).


%%
%% lr_pass/7 ( +KB, +Depth, +Clauses, +OpenListIn, -OpenListOut, +RestrVarsIn, -RestrVarsOut )
%%


% if the restrictions narrow Degree down to zero, the branch
% is done for
lr_pass( _, Depth, _, Open, Open, RestrVars, RestrVars ) :-
	check_clause_degree( RestrVars, 0.0 ),
	NewDepth is Depth + 1,
	msg_proof_tree( NewDepth, f, Open, _RestrVarsIn ).
% nothing left to prove, success
lr_pass( _, Depth, [], Open, Open, RestrVars, RestrVars ) :-
	NewDepth is Depth + 1,
	msg_proof_tree( NewDepth, t, Open, RestrVars ).
% complement
lr_pass( KB, Depth, [H|Rest], OpenIn, OpenOut, RestrVarsIn, RestrVarsOut ) :-
	yadlr_clause_neg( H, [NegLit] ),
	NewDepth is Depth + 1,
	yadlr_mk_literal( NegLit, neg, Lit ),
	yadlr_clause_degree( H, OldDegree ),
	tnorm( complement, OldDegree, NewDegree ),
	yadlr_mk_literal( PosLit, pos, Lit ),
	yadlr_mk_clause( [PosLit], NewDegree, Clause ),
	once( msg_proof_tree(NewDepth,compl,[Clause|Rest],RestrVarsIn) ),
	lr_pass( KB, NewDepth, [Clause|Rest], OpenIn, OpenOut, RestrVarsIn, RestrVarsOut ).
% defer uninstantiated dlalldifferent/1 literals
lr_pass( KB, Depth, [H|Rest], OpenIn, OpenOut, RestrVarsIn, RestrVarsOut ) :-
	yadlr_clause_pos( H, [Lit] ),
	yadlr_mk_literal( Lit, pos, dlalldifferent(DiffList) ),
	has_vars( DiffList ),
	NewDepth is Depth + 1,
	append( Rest, [H], NewRest ), 
	once( msg_proof_tree(NewDepth, sp, NewRest, RestrVarsIn) ),
	lr_pass( KB, NewDepth, NewRest, OpenIn, OpenOut, RestrVarsIn, RestrVarsOut ).
% check instantiated dlalldifferent/1 literals
lr_pass( KB, Depth, [H|Rest], OpenIn, OpenOut, RestrVarsIn, RestrVarsOut ) :-
	yadlr_clause_pos( H, [Lit] ),
	yadlr_mk_literal( Lit, pos, dlalldifferent(DiffList) ),
	\+ has_vars( DiffList ),
	NewDepth is Depth + 1,
	once( msg_proof_tree(NewDepth, sp, [H|Rest], RestrVarsIn) ),
	checkalldiff( DiffList ),
	lr_pass( KB, NewDepth, Rest, OpenIn, OpenOut, RestrVarsIn, RestrVarsOut ).
% apply resolution step
lr_pass( KB, Depth, [H|Rest], OpenIn, OpenOut, RestrVarsIn, RestrVarsOut ) :-
	NewDepth is Depth + 1,
	resolution_step( KB, H, ResidualClauses, RestrVarsIn, RestrVars1 ),
	append( Rest, ResidualClauses, NewRest1 ),
	remove_duplicates( NewRest1, NewRest ),
	once( msg_proof_tree(NewDepth, mp, NewRest, RestrVars1) ),
	lr_pass( KB, NewDepth, NewRest, OpenIn, OpenOut, RestrVars1, RestrVarsOut ),
	append( [H|Rest], OpenIn, _ToDo ).
	%msg_proof_tree( NewDepth, mp, ToDo ).
lr_pass( KB, Depth, [H|Rest], OpenIn, [H|OpenList], RestrVarsIn, RestrVarsOut ) :-
	NewDepth is Depth + 1,
	lr_pass( KB, NewDepth, Rest, [H|OpenIn], OpenList, RestrVarsIn, RestrVarsOut ),
	append( Rest, [H|OpenIn], ToDo ),
	msg_proof_tree( NewDepth, s, ToDo, RestrVarsOut ).

prove_clauses( KB, Depth, Clauses, Open, RestrVarsIn, RestrVarsOut ) :-
	msg_proof_tree( 0, i, Clauses, RestrVarsIn ),
	lr_pass( KB, Depth, Clauses, [], Open, RestrVarsIn, RestrVarsOut ).


%% prove/5 ( +KB, +Query, ?FuzzyDegree, -Open, -ExtraRestr )

prove( KB, Query, FuzzyDegree, Open, Restr ) :-
	var(FuzzyDegree),
	!,
	clausify_abstract( Query, FuzzyDegree, Clauses ),
	prove_clauses( KB, 0, Clauses, Open, [FuzzyDegree], RestrVars ),
	fmt_range( RestrVars, Restr ),
	% if FuzzyDegree has not been instantiated,
	% set to max allowed by restrictions
	% this is only possible if all inputs are instantiated
	% (ie, there only one element, d, in RestrVars)
	(var(FuzzyDegree), length( RestrVars, 1 ) ->
	 nth0( 0, RestrVars, Deg ),
	 get_max(Deg,FuzzyDegree)
	;
	 true
	).
prove( KB, Query, FuzzyDegree, Open, Restr ) :-
	float(FuzzyDegree),
	!,
	clausify_abstract( Query, Degree, Clauses ),
	less_fuzzy( Degree, FuzzyDegree ),
	prove_clauses( KB, 0, Clauses, Open, [Degree], RestrVars ),
	fmt_range( RestrVars, Restr ).

