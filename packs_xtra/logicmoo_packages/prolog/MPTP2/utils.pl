%%- -*-Mode: Prolog;-*--------------------------------------------------
%%
%% $Revision: 1.151 $
%%
%% File  : utils.pl
%%
%% Author: Josef Urban
%%
%%  MPTP2 Prolog utilities, tested only with SWI Prolog 5.2 now.
%%  The top-level predicate is now mk_nonnumeric/0
%%  or mk_first100/0, see below. You also need to set the mml_dir/1
%%  location here appropriately.
%%  The top-level predicate for createing MPTPChallenge problems is e.g.
%%  :- mk_problems_from_file('mptp_chall_problems').
%%------------------------------------------------------------------------

%% TODO: These two are only needed for formula abstraction 0 currently an AI experiment.
%%       Can be safely removed if ausing problems.
:- use_module(library(assoc)).
:- use_module(library(ordsets)).


%%%%%%%%%%%%%%%%%%%% Settings %%%%%%%%%%%%%%%%%%%%

%% set this to the location of Prolog files created from MML
%% ('pl' directory in the distro).
%mml_dir("/home/urban/miztmp/distro/pl/").
%mml_dir("/home/urban/mptp/pl/").
mml_dir("/home/urban/mizwrk/7.13.01_4.181.1147/MPTP2/pl/").
%mml_dir("/home/urban/rsrch/MPTP2/pl/").
%mml_dir("/big/urban/miztmp/mml3/tmp/").
mml_dir_atom(A):- mml_dir(S), string_to_atom(S,A).

%% version of MML needed for requirements (encoding of numbers)
%mml_version([4,48,930]).
mml_version([4,181,1147]).
%mml_version([4,100,1011]).
%% switch to fail for debug
optimize_fraenkel. % :- fail.

%% appends __Article to local constants, lemmas, etc. - use it for
%% mixing local stuff from different articles consistently.
%% (all atoms starting with c[0-9]+_
absolute_locals. %%:- fail.

%% special articles not in mml.lar - changed in 1191; we omit hidden
mml_added_spc([tarski]):- mml_version([_,_,X]), X < 1191,!.
mml_added_spc([tarski_0, tarski_a]).


%% debugging, Flags can be: [dbg_FRAENKELS,dbg_CLUSTERS,dbg_REQS,dbg_FIXPOINT,dbg_LEVEL_REFS]
dbg_flags([]).
dbg(Flag, Goal):-
	dbg_flags(Flags), member(Flag, Flags), !, Goal.
dbg(_,_).

%%%%%%%%%%%%%%%%%%%% End of settings %%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%% Options %%%%%%%%%%%%%%%%%%%%
opt_available([opt_REM_SCH_CONSTS,	%% generalize local constants in scheme instances
	       opt_LEMMA_GLOBAL_GEN,	%% MoMM-like export of proof-local lemmas by generelizing
	       opt_MK_TPTP_INF,		%% better tptp inference slot and no mptp_info
	       opt_TPTP_SHORT,	        %% short tptp format (parsable by vampire too)
	       opt_NO_EX_BG_FLAS,       %% do not include existential background in fixpoint
	       opt_FXP_CHECK_CNSDR,     %% add rclusters for suspisous consider_justifications
	       opt_ADDED_NON_MML,       %% unary functor passing a list of nonMML article names
	       opt_NON_MML_DIR,         %% unary functor passing a nonstandard directory for the article
	       opt_LOAD_MIZ_ERRORS,     %% import err2 file with Mizar's errors
	       opt_PROB_PRINT_FUNC,	%% unary functor passing a special printing func
	       opt_SORT_TRANS_FUNC,     %% unary functor passing a special sort transformation func
	       opt_USE_ORIG_SCHEMES,    %% use the original schemes instead of their instances in the problem
	       opt_PRINT_PROB_PROGRESS, %% print processed problems to stdout
	       opt_ARTICLE_AS_TPTP_AXS, %% print the whole article as tptp axioms
	       opt_PP_SMALLER_INCLUDES, %% unary functor passing a list of possible includes,
	                                %% used with print_refs_as_tptp_includes
	       opt_TPTPFIX_ND_ROLE,	%% make the role acceptable to GDV
	       opt_ADD_INTEREST,	%% add interestingness to useful info
	       opt_LINE_COL_NMS,        %% problem are named LINE_COL instead
	       opt_CONJECTURE_NMS,      %% problem are named as its conjecture
	       opt_LEVEL_REF_INFO,      %% .refspec file with immediate references is printed
	       opt_ALLOWED_REF_INFO,    %% .allowed_local file with accessible references is printed
	       opt_PROVED_BY_INFO,      %% .proved_by0 file with orig refs (no bg) printed
	       opt_DBG_LEVS_POS,        %% print a debugging info for levels and positions
	       opt_DBG_ART_POS,         %% print a debugging info for article positions
	       opt_NO_FRAENKEL_CONST_GEN, %% do not generalize local consts when abstracting fraenkels
	                                 %% (useful for fast translation, when consts are not loaded)
	       opt_LEARN_EDGE,           %% print fromula as labeled graph edges for learning
	       opt_LEARN_STDFILES,	 %% print the symnr and refnr files without the prolog syntax
	       opt_LEARN_SYMS_SMALL      %% symbols are numbered starting from 0 instead of references
	      ]).

%%%%%%%%%%%%%%%%%%%% End of options %%%%%%%%%%%%%%%%%%%%


zip([],[],[]).
zip([H|T],[H1|T1],[[H,H1]|T2]):- zip(T,T1,T2).

zip_s(_,[],[],[]).
zip_s(S,[H|T],[H1|T1],[Trm|T2]):-
	Trm =.. [S,H,H1],
	zip_s(S,T,T1,T2).

% when the second is list of lists
zip1([],[],[]).
zip1([H|T],[H1|T1],[[H|H1]|T2]):- zip1(T,T1,T2).

append_l(In,Out):- reverse(In,In1),append_l1(In1,[],Out).
append_l1([],P,P).
append_l1([H|T],Done,Res):-
	append(H,Done,Res1),
	append_l1(T,Res1,Res).

%% nonempty_intersection(Set1, Set2) (not used for anything now)
nonempty_intersection([H|_],Set2):- memberchk(H,Set2),!.
nonempty_intersection([_|T],Set2):- nonempty_intersection(T,Set2),!.

%% succeed once or throw ecxeption
ensure(Goal, Exception):- Goal,!; throw(Exception).

%% equivalence classes under binary predicate P
%% eqclasses(EquivalencePredicate, InputList, EqClasses)
eqclasses(_,[],[]).
eqclasses(P,[H|T],[E_h|O_t]):-
	eqcl1(P,H,[H|T],E_h,N_h),
	eqclasses(P,N_h,O_t).

% eqcl1(EquivalencePredicate, Member, InputList, Equivalent, NonEquivalent)
eqcl1(_,_,[],[],[]).
eqcl1(P,H1,[H2|I],[H2|O],R):-
	apply(P,[H1,H2]) -> eqcl1(P,H1,I,O,R),!.
eqcl1(P,H1,[H2|I],O,[H2|R]):-
	eqcl1(P,H1,I,O,R).

% insert into P-eqclasses (keeping copies)
% eqc_insert(EquivalencePredicate, Member, InputEqClasses, OutputEqClasses)
eqc_insert(_,M,[],[[M]]).
eqc_insert(P,M,[[M1|H]|T],[[M,M1|H]|T]):-
	apply(P,[M,M1]),!.
eqc_insert(P,M,[H|T],[H|T1]):-
	eqc_insert(P,M,T,T1).

% stolen from tptp2X.main
%----Runtime version of operators
declare_TPTP_operators:-
    op(99,fx,'$'),
    op(100,fx,++),
    op(100,fx,--),
    op(100,xf,'!'),
    op(400,fx,'^'),
    op(405,xfx,'='),
    op(405,xfx,'~='),
    op(450,fy,~),
    op(501,yfx,'@'),
    op(502,xfy,'|'),
    op(502,xfy,'~|'),
    op(503,xfy,&),
    op(503,xfy,~&),
    op(504,xfy,=>),
    op(504,xfy,<=),
    op(505,xfy,<=>),
    op(505,xfy,<~>),
%----! and ? are of higher precedence than : so !X:p(X) is :(!(X),p(X))
%----Otherwise !X:!Y:p(X,Y) cannot be parsed.
    op(400,fx,!),
    op(400,fx,?),
%----Need : stronger than + for equality and otter in tptp2X
%----Need : weaker than quantifiers for !X : ~p
    op(450,xfy,:),
%---- .. used for range in tptp2X. Needs to be stronger than :
    op(400,xfx,'..').


% untested - for newer swi
declare_TPTP_operators1:-
    op(99,fx,'$'),
    op(100,fx,++),
    op(100,fx,--),
    op(100,xf,'!'),
    op(400,fx,'^'),
    op(405,xfx,'='),
    op(405,xfx,'~='),
    op(450,fy,~),
    op(501,yfx,'@'),
    (system_mode(true),op(502,xfy,'|'),system_mode(false)),
    op(502,xfy,'~|'),
    op(503,xfy,&),
    op(503,xfy,~&),
    op(504,xfy,=>),
    op(504,xfy,<=),
    op(505,xfy,<=>),
    op(505,xfy,<~>),
%----! and ? are of higher precedence than : so !X:p(X) is :(!(X),p(X))
%----Otherwise !X:!Y:p(X,Y) cannot be parsed.
    op(400,fx,!),
    op(400,fx,?),
%----Need : stronger than + for equality and otter in tptp2X
%----Need : weaker than quantifiers for !X : ~p
    op(450,xfy,:),
%---- .. used for range in tptp2X. Needs to be stronger than :
    op(400,xfx,'..').




:- declare_TPTP_operators.
logic_syms([++,--,'@','$',~,'|','~|',true,&,~&,=>,<=,<=>,<~>,!,?,:,'..',sort,all,'.',[]]).

%% uncomment this for E prover versions earlier than 0.9
% portray(A = B):- format(' equal(~p,~p) ',[A,B]).
portray(A @ B):- format(' (~p @ ~p) ',[A,B]).
portray(~A):- write(' ~ ('), print(A), write(') ').
portray(A & B):- format(' (~p & ~p) ',[A,B]).
portray(A | B):- format(' (~p | ~p) ',[A,B]).
portray(A => B):- format(' (~p => ~p) ',[A,B]).
portray(A <=> B):- format(' (~p <=> ~p) ',[A,B]).
portray(A : B):- var(A), format(' ( ~p : ~p) ',[A,B]).
portray(! A : B):- format(' (! ~p : ~p) ',[A,B]).
portray(? A : B):- format(' (? ~p : ~p) ',[A,B]).
portray('^' A : B):- format(' (^ ~p : ~p) ',[A,B]).

portray(A):- atom(A), constr_name1(A,_,_,S,_), write(S).

portray(A):- atom(A), constr_name(A,Name,Quote),
	((Quote==0, write(Name));
	    (Quote==1, write(''''),write(Name),write(''''))).
portray(A):- atom(A), abs_name(A,Name), write(Name).
portray(A):- compound(A), A =.. [F|L], constr_name(F,Name,Quote),
	((Quote==0, write(Name));
	    (Quote==1, write(''''),write(Name),write(''''))),
	write('('), print_many(L), write(')').


% this creates the constr_name1 predicate for pretty-printing (see mk_constr_names1.pl for 00constrnamesarity):
% urban@zen:~/gr/MPTP2$ ./mk_constr_names1.pl  00constrnamesarity > foo1.pl
% [foo1].

portray(A):-
	compound(A), A =.. [F|L], constr_name1(F,LA,RA,S,RS),
	length(L,NL),
	(RA=circumfix ->
	 FirstPos is NL - LA,
	 write(S), print_many_from(FirstPos,L), write(RS)
	;
	 TA is LA + RA,
	 FirstPos is NL - TA,
	 (LA=0 -> Rest = L
	 ;
	  write('('), print_many_from_times(FirstPos,LA,L,Rest), write(')')
	 ),
	 write(S),
	 (Rest = [] -> true
	 ;
	  write('('), print_many(Rest), write(')')
	 )
	).

print_many([]).
print_many([X]):- print(X),!.
print_many([X|Y]):- print(X), write(','), print_many(Y).

print_many_from(_,[]):-!.
print_many_from(0,L):-!, print_many(L).
print_many_from(N,[_|T]):- N1 is N - 1,  print_many_from(N1, T).


print_many_times(_,[],[]):-!.
print_many_times(0,L,L):- !.
print_many_times(1,[X|Y],Y):- !, print(X).
print_many_times(N,[X|Y],Res):- !, N1 is N - 1, print(X), write(','), print_many_times(N1,Y,Res).

% print from the From position Times elements, return Res
print_many_from_times(_,_,[],[]):-!.
print_many_from_times(_,0,L,L):-!.
print_many_from_times(0,Times,L,Res):-!, print_many_times(Times,L,Res).
print_many_from_times(N,Times,[_|T],Res):- N1 is N - 1,  print_many_from_times(N1, Times, T,  Res).



% explain tstp parsing
d2l(X,X):- atomic(X);var(X).
d2l([],[]).
d2l(X,Y):- X =.. Z, d2ls(Z,Y).
d2ls([],[]).
d2ls([H|T],[H1|T1]):- d2l(H,H1), d2ls(T,T1).

term2list(X,X):- (atomic(X);var(X)),!.
term2list(X,[H|T1]):- X =.. [H|T], maplist(term2list,T,T1).

declare_mptp_predicates:-
 abolish(fof/4),
 abolish(fof/5),
 abolish(theory/2),
 abolish(constr_name/3),
 abolish(constr_name/5),
 multifile(fof/4),
 multifile(fof/5),
 dynamic(fof/5),
 dynamic(constr_name/3),
 abolish(constr_name1/5),
 multifile(constr_name/3),
 multifile(constr_name1/5),
 multifile(theory/2),
 abolish(fof_name/2),
 abolish(fof_file/2),
 abolish(fof_eq_def/2),
 abolish(fof_sort_def/2),
 abolish(fof_pred_def/2),
 abolish(fof_section/2),
 abolish(fof_level/2),
 abolish(fof_toplevel/2),
 abolish(fof_newlevel/3),
 dynamic(fof_newlevel/3),
 abolish(fof_parentlevel/2),
 dynamic(fof_parentlevel/2),
 abolish(fof_cluster/3),
 dynamic(fof_cluster/3),
 abolish(fof_identifyexp/3),
 dynamic(fof_identifyexp/3),
 abolish(rc_syms_for_consider/7),
 abolish(fof_req/3),
 dynamic(fof_req/3),
 abolish(fof_syms/2),
 dynamic(fof_syms/2),
 abolish(sym_ref_graph/2),
 dynamic(sym_ref_graph/2),
 abolish(fof_ante_sym_cnt/4),
 dynamic(fof_ante_sym_cnt/4),
 abolish(abs_name/2),
 dynamic(abs_name/2),
 abolish(miz_errors/1),
 dynamic(miz_errors/1),
 abolish(fof_redefines/4),
 dynamic(fof_redefines/4),
 abolish(fraenkel_cached/3),
 dynamic(fraenkel_cached/3),
 abolish(fraenkels_loaded/1),
 dynamic(fraenkels_loaded/1),
 abolish(sch_orig_copy/2),
 dynamic(sch_orig_copy/2),
 abolish(articles_numbered/1),
 dynamic(articles_numbered/1),
 abolish(article_position/2),
 dynamic(article_position/2),
 abolish(fxp_refsin_/1),
 dynamic(fxp_refsin_/1),
 abolish(fxp_allsyms_/1),
 dynamic(fxp_allsyms_/1),
 abolish(zerotyp/2),
 multifile(zerotyp/2),
 dynamic(zerotyp/2),
 abolish(nonzerotyp/2),
 multifile(nonzerotyp/2),
 dynamic(nonzerotyp/2),
 abolish(rec_sch_inst_name/2),
 dynamic(rec_sch_inst_name/2).
% index(fof(1,1,0,1,1)),
% index(fof(1,1,0,1)).

%% this makes things very slow, do not use it
% index(fof_name(1,1)).

%% collect subterms satisfying P with count, test by equality
collect_with_count_top(Pred,Term,OutList,OutCounts):-
	collect_with_count(Pred,Term,[],[],OutList,OutCounts),!.
collect_with_count(P,X,In,InC,Out,OutC):-
	(var(X);atomic(X)), !, collect_with_count1(P,X,In,InC,Out,OutC).
%% neglects the head functor!
collect_with_count(P,X,In,InC,Out,OutC):-
	collect_with_count1(P,X,In,InC,Out1,OutC1),
	X =.. [_|T1],
	collect_with_countl(P,T1,Out1,OutC1,Out,OutC).
collect_with_countl(_,[],In,InC,In,InC).
collect_with_countl(P,[H|T],In,InC,Out,OutC):-
	collect_with_count(P,H,In,InC,Out1,OutC1),
	collect_with_countl(P,T,Out1,OutC1,Out,OutC).

%% do the collecting
collect_with_count1(P,X,In,InC,Out,OutC):- apply(P,[X]),!,
	(enth1(N,In,X) ->
	    (Out = In, remove_at(C_X,InC,N,Tmp),
		succ(C_X,C1_X), insert_at(C1_X,Tmp,N,OutC));
	    (Out = [X|In], OutC = [1|InC])).
collect_with_count1(_,_,In,InC,In,InC).

%% exact select
eselect(A, [C|B], B):- A == C,!.
eselect(A, [B|C], [B|D]):- eselect(A, C, D).
%% exact nth1
enth1(A, B, C):- var(A), !, enth_gen(B, C, 1, A).
enth_gen([A|_], A1, C, C):- A == A1.
enth_gen([_|B], C, D, E) :-
        succ(D, F),
        enth_gen(B, C, F, E).

%% remove K-th element (1-based)
% remove_at(X,L,K,R) :- X is the K'th element of the list L; R is the
%    list that remains when the K'th element is removed from L.
%    (element,list,integer,list) (?,?,+,?)
remove_at(X,[X|Xs],1,Xs).
remove_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1,
   K1 is K - 1, remove_at(X,Xs,K1,Ys).

% Insert an element at a given position into a list (1-based)
% insert_at(X,L,K,R) :- X is inserted into the list L such that it
%    occupies position K. The result is the list R.
%    (element,list,integer,list) (?,?,+,?)
insert_at(X,L,K,R) :- remove_at(X,R,K,L).

% Split a list into two parts
% split(L,N,L1,L2) :- the list L1 contains the first N elements
%    of the list L, the list L2 contains the remaining elements.
%    (list,integer,list,list) (?,+,?,?)
split(L,0,[],L).
split([X|Xs],N,[X|Ys],Zs) :- N > 0, N1 is N - 1, split(Xs,N1,Ys,Zs).

%% Digit is a digit character
%% digit(Digit):- member(Digit, ['0','1','2','3','4','5','6','7','8','9']).

digit('0'). digit('1'). digit('2'). digit('3'). digit('4').
digit('5'). digit('6'). digit('7'). digit('8'). digit('9').

%% mptp_func with args
mptp_func(X):- X =..[H|_],mptp_func_sym(H).
ground_mptp_func(X):- ground(X),mptp_func(X).
sch_symbol(X):- atom_chars(X,[F|_]),member(F,[f,p]).
mptp_local_const(X):- atom_chars(X,[c|_]).
mptp_func_sym(X):- atom_chars(X,[F|_]),member(F,[k,g,u,'0','1','2','3','4','5','6','7','8','9']).
mptp_attr_sym(X):- atom_chars(X,[v|_]).
mptp_mode_sym(X):- atom_chars(X,[F|_]),member(F,[m,l]).
mptp_attr_or_mode_sym(X):- atom_chars(X,[F|_]),member(F,[v,m,l]).
mptp_req_name(X):- atom_chars(X,[r,q|_]).
mptp_fraenkel_sym(X):- atom_chars(X,[a|_]).
mptp_choice_sym(X):- atom_chars(X,[o|_]).
mptp_sch_func_sym(X):- atom_chars(X,[f|_]).
mptp_sch_pred_sym(X):- atom_chars(X,[p|_]).

% X/_ is an MPTP object translated as tptp functor
mptp_tptp_func_arity(X/_):- atom_chars(X,[F|_]),member(F,[k,g,u,c,f,a,o,'0','1','2','3','4','5','6','7','8','9']).

% avoids "f" and "p" - left to the scheme variant below
mptp_tptp_func_arity_thf(X/_):- atom_chars(X,[F|_]),member(F,[k,g,u,c,a,o,'0','1','2','3','4','5','6','7','8','9']).
mptp_tptp_pred_arity_thf(X/_):- atom_chars(X,[F|_]),member(F,[r,v,m,l]).

mptp_scheme_arity_thf(X/_):- atom_chars(X,[F|_]),member(F,[f,p]).

%% collect ground counts into buk/3, stack failure otherwise
%% then print and run perl -e 'while(<>) { /.([0-9]+), (.*)./; $h{$2}+=$1;} foreach $k (sort {$h{$b} <=> $h{$a}} (keys %h)) {print "$h{$k}:$k\n";}'
%% on the result
get_ground_info:-
	fof(Ref,_,Fla,file(_,_), [mptp_info(_,_,theorem,_,_)|_]),
	collect_with_count_top(ground_mptp_func,Fla,Out,OutC),
	not(buk(Ref,_,_)),assert(buk(Ref,Out,OutC)),
	fail.
print_ground_info:-
	tell('00ground'),
	findall(e,(buk(_,B,C),zip(C,B,S),findall(d,(member(X,S),print(X),nl),_)),_),
	told.


%% like collect_symbols, but with arity
collect_symbols_arity_top(X,L):-
	collect_symbols_arity(X,L1),!,
	flatten(L1,L2),
	sort(L2,L).
collect_symbols_arity(X,[]):- var(X),!.
collect_symbols_arity(X,[X/0]):- atomic(X),!.
collect_symbols_arity(X1,T2):-
	X1 =.. [H1|T1],
	length(T1,A1),
	maplist(collect_symbols_arity,T1,T3),
	flatten(T3,T4),
	sort([H1/A1|T4],T2).

% like logic_syms, but with a fake arity - is dangerous to use and hackish
logic_syms_arity([(=)/_,'++'/_,'--'/_,'@'/_,'$'/_,(~)/_,'|'/_,'~|'/_,true/_,(&)/_,(~&)/_,(=>)/_,(<=)/_,(<=>)/_,(<~>)/_,(!)/_,(?)/_,(:)/_,'..'/_,sort/_,the/_,all/_,'.'/_,([])/_]).

%% collect funcs and preds with arity from a formula
get_func_pred_syms(X,F,P):-
	collect_symbols_arity_top(X,L),
	logic_syms_arity(LA),
	subtract(L,LA,L1),
	sublist(mptp_tptp_func_arity,L1,F),
	subtract(L1,F,P).

%% collect funcs and preds with arity from a formula, avoid "f" and "p" - schemes
get_func_pred_syms_thf(X,F,P):-
	collect_symbols_arity_top(X,L),
	logic_syms_arity(LA),
	subtract(L,LA,L1),
	sublist(mptp_tptp_func_arity_thf,L1,F),
	subtract(L1,F,L2),
	sublist(mptp_tptp_pred_arity_thf,L2,P).



%% collect nonvar symbols from term
collect_symbols_top(X,L):-
	collect_symbols(X,L1),!,
	flatten(L1,L2),
	sort(L2,L).
collect_symbols(X,[]):- var(X),!.
collect_symbols(X,[X]):- atomic(X),!.
collect_symbols(X1,T2):-
	X1 =.. [H1|T1],
	maplist(collect_symbols,T1,T3),
	flatten(T3,T4),
	sort([H1|T4],T2).

union1([],In,In).
union1([H|T],In,Out):-
	union(H,In,R1),
	union1(T,R1,Out).


%% check_if_symbol(+Term,+Symbol)
%%
%% faster check for a symbol than collecting
%% e.g. check_if_symbol(Fla,all) checks for a fraenkel term
check_if_symbol(X,_):- var(X),!,fail.
check_if_symbol(X, S):- atomic(X),!, X == S.
check_if_symbol(X1,S):-
	X1 =.. [H1|T1],
	(
	  H1 = S,!
	;
	  check_if_symbol_l(T1,S)
	).

check_if_symbol_l([H|_],S):- check_if_symbol(H,S),!.
check_if_symbol_l([_|T],S):- check_if_symbol_l(T,S),!.

%%%%%%%%%%%%%%%%%%%% Propositional Abstractor %%%%%%%%%%%%%%%%%%%%

% to forbid backtracking
propabstr_transform_top(X,Y):- 	sort_transform_top(X,X1),!, propabstr_transform(X1,Y), !.
% end of traversal
propabstr_transform(! _ : X, Y):- !, propabstr_transform(X, Y).
propabstr_transform(? _ : X, Y):- !, propabstr_transform(X, Y).
propabstr_transform($true,$true):- !.
propabstr_transform($false,$false):- !.

propabstr_transform(X & Y,X1 & Y1):- !,
	propabstr_transform_l([X,Y],[X1,Y1]).

propabstr_transform(X,TermOut):-
	X =.. [F|Args],!,
	(
	 memberchk(F,[&,'|',~,=>,<=,<=>]) ->
	 propabstr_transform_l(Args,Args1),
	 TermOut =.. [F|Args1]
	;
	 F == '=' ->
	 TermOut = eq
	;
	 TermOut = F
	).

propabstr_transform_l([],[]).
propabstr_transform_l([H|T],[H1|T1]):-
	propabstr_transform(H,H1),
	propabstr_transform_l(T,T1).

%%%%%%%%%%%%%%%%%%%% Abstractor %%%%%%%%%%%%%%%%%%%%

%% ##TEST: :- declare_mptp_predicates,load_mml,install_index,all_articles(AA),checklist(abstract_fraenkels_if, AA),!,member(A,[card_1]),time(mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma, sublemma] ],[opt_LOAD_MIZ_ERRORS,opt_ARTICLE_AS_TPTP_AXS,opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_LINE_COL_NMS,opt_PRINT_PROB_PROGRESS,opt_ALLOWED_REF_INFO,opt_PROVED_BY_INFO,opt_SORT_TRANS_FUNC(set_of_abstr_trm_top)])),fail.

% to forbid backtracking
abstr_transform_top(X,Y):- abstr_transform(X,Y,[],_), !.
% end of traversal
abstr_transform(X,X,SubstIn,SubstIn):- var(X),!.

abstr_transform(X & Y,X1 & Y1,SubstIn,SubstOut):-
	abstr_transform_l([X,Y],[X1,Y1],SubstIn,SubstOut).


abstr_transform(X,TermOut,SubstIn,SubstOut):-
	X =.. [F|Args],!,
	(
	 memberchk(F,[&,'|',~,=>,<=,!,?,:,<=>,'..','.','$',true,[]]) ->
	 abstr_transform_l(Args,Args1,SubstIn,SubstOut),
	 TermOut =.. [F|Args1]
	;
	 (
	  memberchk((F/NewVar),SubstIn) -> Subst1 = SubstIn
	 ;
	  Subst1 = [(F/NewVar)|SubstIn]
	 ),
	 abstr_transform_l(Args,Args1,Subst1,SubstOut),
	 TermOut =.. [ap,NewVar|Args1]
	). 

abstr_transform_l([],[],SubstIn,SubstIn).
abstr_transform_l([H|T],[H1|T1],SubstIn,SubstOut):-
	abstr_transform(H,H1,SubstIn,Subst1),
	abstr_transform_l(T,T1,Subst1,SubstOut).

% Does not work when here - moved to the top
% :- use_module(library(ordsets)).

set_of_terms_top(X,Res) :- !,
	copy_term(X,X1),
	numbervars(X1,0,_),
	set_of_terms(X1,[],Res).

set_of_terms_l([],In,In).
set_of_terms_l([H|T],In,Out):-
	set_of_terms(H,In,S1),
	set_of_terms_l(T,S1,Out).

set_of_terms(X,In,Out):-
	ord_add_element(In, X, S1),
	X =.. [F|Args],
	list_to_ord_set([F|Args], S2),
	ord_union(S1, S2, S3),
	set_of_terms_l(Args, S3, Out).

set_of_abstr_trm_top(X,Out):-
	sort_transform_top(X,X1),
	abstr_transform_top(X1,Y),
	set_of_terms_top(Y,Out1),!,
	findall(K,(member(T,Out1),with_output_to(string(K), print(T))),Out).
		   %%format(string(K),'~w',T)),Out).

%	maplist(format('~w'),Out1,Out).
%	repeat,(member(K,Out),write('"'),write(K),write('",'),fail; nl).

%%%%%%%%% the version that add the counts of occurences

%% ##TEST: :- declare_mptp_predicates,load_mml,install_index,all_articles(AA),checklist(abstract_fraenkels_if, AA),!,member(A,[card_1]),time(mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma, sublemma] ],[opt_LOAD_MIZ_ERRORS,opt_ARTICLE_AS_TPTP_AXS,opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_LINE_COL_NMS,opt_PRINT_PROB_PROGRESS,opt_ALLOWED_REF_INFO,opt_PROVED_BY_INFO,opt_SORT_TRANS_FUNC(bag_of_abstr_trm_top)])),fail.


% Does not work when here - moved to the top
:- use_module(library(assoc)).

bag_insert(X,In,Out):-
	(get_assoc(X, In, V) ->
	 V1 is V + 1
	;
	 V1 is 1
	 ),
	put_assoc(X, In, V1, Out).

bag_insert_l([],In,In).
bag_insert_l([H|T],In,Out):-
	bag_insert(H,In,Out1),
	bag_insert_l(T,Out1,Out).
	
bag_of_terms_top(X,Res) :- !,
	copy_term(X,X1),
	numbervars(X1,0,_),
	empty_assoc(Assoc),
	bag_of_terms(X1,Assoc,Res).

bag_of_terms_l([],In,In).
bag_of_terms_l([H|T],In,Out):-
	bag_of_terms(H,In,S1),
	bag_of_terms_l(T,S1,Out).

bag_of_terms($true,In,In):- !.
bag_of_terms(X,In,Out):-
	X =.. [F|Args],
	bag_insert_l([X,F],In,In1),
	bag_of_terms_l(Args, In1, Out).

bag_of_abstr_trm_top(X,Out):-
	sort_transform_top(X,X1),
	abstr_transform_top(X1,Y),
	bag_of_terms_top(Y,Out0),!,
	assoc_to_list(Out0,Out1),
	findall(K,(member(T,Out1),with_output_to(string(K), print(T))),Out).


%%%%%%%%% the version that distinguishes funcs and preds

mptp_pred_tptp(X):- atom_chars(X,[F|_]), member(F,[v,r,m,l,p]).

% to forbid backtracking
abstr1_transform_top(X,Y):- abstr1_transform(X,Y,[],[],_,_), !.
% end of traversal
abstr1_transform(X,X,SubstPIn,SubstFIn,SubstPIn,SubstFIn):- var(X),!.

abstr1_transform(X & Y,X1 & Y1,SubstPIn,SubstFIn,SubstPOut,SubstFOut):-
	abstr1_transform_l([X,Y],[X1,Y1],SubstPIn,SubstFIn,SubstPOut,SubstFOut).

abstr1_transform(X,TermOut,SubstPIn,SubstFIn,SubstPOut,SubstFOut):-
	X =.. [F|Args],!,
	(
	 memberchk(F,[&,'|',~,=>,<=,!,?,:,<=>,'..','.','$',true,[]]) ->
	 abstr1_transform_l(Args,Args1,SubstPIn,SubstFIn,SubstPOut,SubstFOut),
	 TermOut =.. [F|Args1]
	;
	 (
	  mptp_pred_tptp(F) ->
	  (
	   memberchk((F/NewVar),SubstPIn) ->
	   Subst1P = SubstPIn
	  ;
	   Subst1P = [(F/NewVar)|SubstPIn]
	  ),
	  abstr1_transform_l(Args,Args1,Subst1P,SubstFIn,SubstPOut,SubstFOut),
	  TermOut =.. [p,NewVar|Args1]	  
	 ;
	  (
	   memberchk((F/NewVar),SubstFIn) ->
	   Subst1F = SubstFIn
	  ;
	   Subst1F = [(F/NewVar)|SubstFIn]
	  ),
	  abstr1_transform_l(Args,Args1,SubstPIn,Subst1F,SubstPOut,SubstFOut),
	  TermOut =.. [f,NewVar|Args1]
	 )
	). 

abstr1_transform_l([],[],SubstPIn,SubstFIn,SubstPIn,SubstFIn).
abstr1_transform_l([H|T],[H1|T1],SubstPIn,SubstFIn,SubstPOut,SubstFOut):-
	abstr1_transform(H,H1,SubstPIn,SubstFIn,Subst1P,Subst1F),
	abstr1_transform_l(T,T1,Subst1P,Subst1F,SubstPOut,SubstFOut).


%% ##TEST: :- declare_mptp_predicates,load_mml,install_index,all_articles(AA),checklist(abstract_fraenkels_if, AA),!,member(A,[card_1]),time(mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma, sublemma] ],[opt_LOAD_MIZ_ERRORS,opt_ARTICLE_AS_TPTP_AXS,opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_LINE_COL_NMS,opt_PRINT_PROB_PROGRESS,opt_ALLOWED_REF_INFO,opt_PROVED_BY_INFO,opt_SORT_TRANS_FUNC(bag_of_abstr1_trm_top)])),fail.

bag_of_abstr1_trm_top(X,Out):-
	sort_transform_top(X,X1),
	abstr1_transform_top(X1,Y),
	bag_of_terms_top(Y,Out0),!,
	assoc_to_list(Out0,Out1),
	findall(K,(member(T,Out1),with_output_to(string(K), print(T))),Out).


%%%%%%%%%%%%%%%%%%%% Putting variable types into terms for learning  %%%%%%%%%%%%%%%%%%%%

%% not used yet because of various explosions

tvar_transforml(NewVar,Y1 & Y2,List):- !,
	tvar_transforml(NewVar,Y1,L1),
	tvar_transforml(NewVar,Y2,L2),
	append(L1,L2,List).

tvar_transforml(_, $true, []):- !.

tvar_transforml(NewVar, ~S, [~S1]):- !,
	S =.. [F|Args],
	S1 =.. [F|[NewVar|Args]].

tvar_transforml(NewVar, S, [S1]):- !,	
	S =.. [F|Args],
	S1 =.. [F|[NewVar|Args]].

% to forbid backtracking
tvar_transform_top(X,Y):- tvar_transform(X,Y), !.
% end of traversal
tvar_transform(X,X):- atomic(X); var(X).

tvar_transform(! [(X:S)|T] : Y, Result):-
	(
	 S == $true -> X = NewVar
	;
	 tvar_transforml(NewVar,S,List),
	 (X = List,!; print(zzzfail(X,List)))
	 %ensure(X = List, tvar_transforml(X,List))
	),
	(
	 T = [] -> tvar_transform(Y, Result)
	;
	 tvar_transform(! T : Y, Result)
	), !.

tvar_transform(? [(X:S)|T] : Y, Result):- !,
	tvar_transform(! [(X:S)|T] : Y, Result).

tvar_transform(sort(_,$true),$true).
tvar_transform(sort(_,$false),$false).

%% a list of predicates applied to a list
tvar_transform(sort(X,S),List):- !,
	tvar_transforml(X, S, List).

%% removal of $true
tvar_transform((A | B), Result):-
	maplist(tvar_transform,[A,B],[A1,B1]),!,
	(
	  (A1 == $true;B1 == $true) -> Result = $true;
	  Result = (A1 | B1)
	).
tvar_transform(A & B, Result):-
	maplist(tvar_transform,[A,B],[A1,B1]),!,
	(
	  A1== $true -> Result = B1;
	  (
	    B1== $true -> Result = A1;
	    Result = (A1 & B1)
	  )
	).
tvar_transform(A => B, Result):-
	maplist(tvar_transform,[A,B],[A1,B1]),!,
	(
	  A1== $true -> Result = B1;
	  (
	    B1== $true -> Result = $true;
	    Result = (A1 => B1)
	  )
	).
tvar_transform(A <=> B, Result):-
	maplist(tvar_transform,[A,B],[A1,B1]),!,
	(
	  A1== $true -> Result = B1;
	  (
	    B1== $true -> Result = A1;
	    Result = (A1 <=> B1)
	  )
	).
% functor traversal
tvar_transform(X1,X2):-
	X1 =.. [H1|T1],
	maplist(tvar_transform,T1,T2),
	X2 =.. [H1|T2].



%%%%%%%%%%%%%%%%%%%% Sort relativization %%%%%%%%%%%%%%%%%%%%

% return the list of quantified variables and conjunction of predicates
sort_transform_qlist([],[],$true).  % needed only for fraenkel
sort_transform_qlist([X:S],[X],S1):- !,
	sort_transform(sort(X,S),S1).
sort_transform_qlist([(X:S)|T],[X|Qvars1],SortPreds1):-
	sort_transform(sort(X,S),S1),
	sort_transform_qlist(T,Qvars1,Preds1), !,
	(
	  S1 == $true -> SortPreds1 = Preds1;
	  (
	    Preds1 == $true -> SortPreds1 = S1;
	    SortPreds1 = (S1 & Preds1)
	  )
	).


% to forbid backtracking
sort_transform_top(X,Y):- sort_transform(X,Y), !.
% end of traversal
sort_transform(X,X):- atomic(X); var(X).

% do sort relativization, and simple removal of $true
sort_transform(! Svars : Y, Result):-
	sort_transform_qlist(Svars,Qvars,Preds),
	sort_transform(Y,Y1), !,
	(
	  Y1 == $true -> Result = $true;
	  Result = (! Qvars : UnivRelat),
	  (
	    Preds == $true -> UnivRelat = Y1;
	    UnivRelat = (Preds => Y1)
	  )
	).
sort_transform(? Svars : Y, Result):-
	sort_transform_qlist(Svars,Qvars,Preds),
	sort_transform(Y,Y1), !,
	(
	  Preds == $true ->
	  (
	    Y1 == $true -> Result = $true;
	    Result = (? Qvars : Y1)
	  )
	;
	  (
	    Y1 == $true -> Result = (? Qvars : Preds);
	    Result = (? Qvars : (Preds & Y1))
	  )
	).
% This clause is redundant now, sort trafo can be done only after 'all' removal
sort_transform(all(Svars,Trm,Frm),all(Qvars,Trm1,RelatFrm1)):-
	sort_transform_qlist(Svars,Qvars,Preds),
	sort_transform(Trm,Trm1),
	sort_transform(Frm,Frm1), !,
	(
	  Preds == $true -> RelatFrm1 = Frm1;
	  RelatFrm1 = (Preds & Frm1)
	).
sort_transform(sort(X,Y1 & Y2),SortPreds):-
	sort_transform(sort(X,Y1),Z1),
	sort_transform(sort(X,Y2),Z2), !,
	(
	  Z1 == $true -> SortPreds = Z2;
	  (
	    Z2 == $true -> SortPreds = Z1;
	    SortPreds = (Z1 & Z2)
	  )
	).
sort_transform(sort(X,~Y),~Z):-
	sort_transform(sort(X,Y),Z).
sort_transform(sort(_,$true),$true).
sort_transform(sort(_,$false),$false).
sort_transform(sort(X,Y),Z):-
	Y =.. [F|Args],
	maplist(sort_transform,[X|Args],Args1),
	Z =.. [F|Args1].
% we should not get here
sort_transform(sort(_,_),_):- throw(sort).
% removal of $true
sort_transform((A | B), Result):-
	maplist(sort_transform,[A,B],[A1,B1]),!,
	(
	  (A1 == $true;B1 == $true) -> Result = $true;
	  Result = (A1 | B1)
	).
sort_transform(A & B, Result):-
	maplist(sort_transform,[A,B],[A1,B1]),!,
	(
	  A1== $true -> Result = B1;
	  (
	    B1== $true -> Result = A1;
	    Result = (A1 & B1)
	  )
	).
sort_transform(A => B, Result):-
	maplist(sort_transform,[A,B],[A1,B1]),!,
	(
	  A1== $true -> Result = B1;
	  (
	    B1== $true -> Result = $true;
	    Result = (A1 => B1)
	  )
	).
sort_transform(A <=> B, Result):-
	maplist(sort_transform,[A,B],[A1,B1]),!,
	(
	  A1== $true -> Result = B1;
	  (
	    B1== $true -> Result = A1;
	    Result = (A1 <=> B1)
	  )
	).
% functor traversal
sort_transform(X1,X2):-
	X1 =.. [H1|T1],
	maplist(sort_transform,T1,T2),
	X2 =.. [H1|T2].

%%%%%%%%%%%%%%%%%%%% THF translation %%%%%%%%%%%%%%%%%%%%

% return the list of quantified variables and conjunction of predicates
sort_transform_thf_qlist(S1,S1,[],[],$true).  % needed only for fraenkel
sort_transform_thf_qlist(SubstsIn,SubstsOut,[X:S],[X : $i],S1):- !,
	sort_transform_thf(SubstsIn,SubstsOut,sort(X,S),S1).
sort_transform_thf_qlist(SubstsIn,SubstsOut,[(X:S)|T],[X : $i|Qvars1],SortPreds1):-
	sort_transform_thf(SubstsIn,SubstsOut1,sort(X,S),S1),
	sort_transform_thf_qlist(SubstsOut1,SubstsOut,T,Qvars1,Preds1), !,
	(
	  S1 == $true -> SortPreds1 = Preds1;
	  (
	    Preds1 == $true -> SortPreds1 = S1;
	    SortPreds1 = (S1 & Preds1)
	  )
	).

add_lambda_l(_,[],[]).
add_lambda_l(X, [H|T], [Z | T1]) :-
	Z = ('^' [ X : $i] : H),
	add_lambda_l(X,T,T1).



sort_transform_thf_qlist_fr(S1,S1,[],[],[]).  % needed only for fraenkel
sort_transform_thf_qlist_fr(SubstsIn,SubstsOut,[(X:S)|T],[X : $i|Qvars1],Preds2):-
	sort_transform_thf(SubstsIn,SubstsOut1,sort(X,S),S1),
	sort_transform_thf_qlist_fr(SubstsOut1,SubstsOut,T,Qvars1,Preds1),
	add_lambda_l(X,[S1|Preds1],Preds2), !.



add_univ_context1([],Fla,Fla).
add_univ_context1([[_,Var,Decl]|T], FlaIn, ( ! [Var:Decl] : ( FlaOut) )):-
	add_univ_context1(T,FlaIn,FlaOut).

apply_univ_context1([],_).
apply_univ_context1([[Sym,Var,_]|T], FlaIn):-
	Var = Sym,
	apply_univ_context1(T,FlaIn).



% sort_transform_thf_top(+NoGen,+X,-Y):-
%
% If NoGen == 1, we do not generalize the scheme symbols like "f" and
% "p" into variables. This means that we just apply the substitutions
% collected.
sort_transform_thf_top(NoGen,X,Y):-
	sort_transform_thf([],SubstsOut,X,Y1), !,
	(
	 NoGen == 1 -> apply_univ_context1(SubstsOut,Y1), Y = Y1
	;
	 add_univ_context1(SubstsOut,Y1,Y)
	).


% sort_transform_thf(+SubstsIn, -SubstsOut, +In , -Out)
%
% transform to THF and collect the substitutions (context) that may
% later be applied in sort_transform_thf_top .
%
% Scheme functors and preds get replaced by a new
% variable and a declaration is created for them; the replacements get
% collected so we can easily undo them by instatiation later
	
% end of traversal
sort_transform_thf(S1,S1,X,X):- atomic(X), not(sch_symbol(X)); var(X).

% do sort relativization, and simple removal of $true
sort_transform_thf(SubstsIn,SubstsOut,! Svars : Y, Result):-
	sort_transform_thf_qlist(SubstsIn,SubstsOut1,Svars,Qvars,Preds),
	sort_transform_thf(SubstsOut1,SubstsOut,Y,Y1), !,
	(
	  Y1 == $true -> Result = $true;
	  Result = (! Qvars : UnivRelat),
	  (
	    Preds == $true -> UnivRelat = Y1;
	    UnivRelat = (Preds => Y1)
	  )
	).
sort_transform_thf(SubstsIn,SubstsOut,? Svars : Y, Result):-
	sort_transform_thf_qlist(SubstsIn,SubstsOut1,Svars,Qvars,Preds),
	sort_transform_thf(SubstsOut1,SubstsOut,Y,Y1), !,
	(
	  Preds == $true ->
	  (
	    Y1 == $true -> Result = $true;
	    Result = (? Qvars : Y1)
	  )
	;
	  (
	    Y1 == $true -> Result = (? Qvars : Preds);
	    Result = (? Qvars : (Preds & Y1))
	  )
	).


%% instantiate fraenkels with their defs, create the defs
%% {f x | x in b, p x} ... all([X:b],f(X),p(X))
% {F x|M x,P x}
% 	replSepC1@(M)@(^[X:$i]:p(X))@(^[X:$i]:f(X))
%
% 	all([B3: m1_subset_1(u1_struct_0(B1))],B3,r2_hidden(B2,k2_abcmiz_0(B1,B3))) 
%
% 	(replSepC1 @ (m1_subset_1 @ (u1_struct_0 @ B1))
% 	                  @ (^[B3:$i]: r2_hidden @ B2 @ (k2_abcmiz_0 @ B1 @B3))
% 	                  @ (^[B3:$i]: B3) )
%
% all([B2: ( v8_abcmiz_1(k27_abcmiz_1) & m1_abcmiz_1(k27_abcmiz_1,k13_abcmiz_1(k27_abcmiz_1)) )],k9_abcmiz_a(k27_abcmiz_1
% ,B2),r2_hidden(B2,k41_abcmiz_1(k27_abcmiz_1,B1)) & B2 = B2)

% (replSepC1 @ (^[B2:$i] ( (v8_abcmiz_1 @ k27_abcmiz_1 @ B2)
% 		                     & ( m1_abcmiz_1 @ k27_abcmiz_1 @ ( k13_abcmiz_1 @ k27_abcmiz_1) @ B2 ) )  )
%                  @(^[B2:$i]: ((r2_hidden @ B2 @ ( k41_abcmiz_1 @ k27_abcmiz_1 @ B1)) & (B2 = B2)))
% 		 @(^[B2:$i]:  ( k9_abcmiz_a @ k27_abcmiz_1 @ B2) )
% 		 )

% the(( v1_relat_1 & v1_funct_1 & v5_ordinal1 & v1_ordinal2 ))
%(eps @ (^[NewVar:$i]: ((v1_relat_1 @ NewVar) & (v1_funct_1 @ NewVar) & (v5_ordinal1 @ NewVar) & (v1_ordinal2 @ NewVar))))

% the(m2_qc_lang1(B1))
% (eps @ (^[NewVar:$i]: ((m2_qc_lang1 @ B1 @ NewVar))))
% or
% (eps @ (m2_qc_lang1 @ B1))
%
% replSepC2 is an operator corresponding to (unary) Fraenkel operators
% replSepC2@M1@M2@P@F means {F x y|M1 x, M2 x y, P x y} when sethood@M1 and sethood@(M2@X) (for all X with M1@X) hold;
% if sethood@M1 or sethood@M2@X for some M1@X fail, replSepC2@M1@M2@P@F is left as some unspecified subset of {F x y|M1 x,M2 x y,P x y} (which may be a class)
% thf(replSepC2,type,(replSepC2: ($i>$o)>($i>$i>$o)>($i>$i>$o)>($i>$i>$i)>$i)).
% thf(replSepC2E,axiom, (! [M1:$i>$o] : (! [M2:$i>$i>$o] : (! [P:$i>$i>$o] : (! [F:$i>$i>$i] : (! [Y:$i] : ((in@Y@(replSepC2@M1@M2@P@F)) => (? [X1:$i] : (? [X2:$i] : ((M1@X1) & (M2@X1@X2) & (P@X1@X2) & (Y = (F@X1@X2)))))))))))).
% thf(replSepC2I,axiom, (! [M1:$i>$o] : (! [M2:$i>$i>$o] : (! [P:$i>$i>$o] : (! [F:$i>$i>$i] : ((sethood@M1) => ((! [X1:$i] : ((M1@X1) => (sethood@(M2@X1)))) => (! [X1:$i] : (! [X2:$i] : ((M1@X1) => ((M2@X1@X2) => ((P@X1@X2) => (in@(F@X1@X2)@(replSepC2@M1@M2@P@F)))))))))))))).

% all([B3: m1_subset_1(B1),B4: m1_trees_1(k9_xtuple_0(B3))],k5_trees_2(B3,B4),~ ( r2_hidden(B4,k3_trees_1(k9_xtuple_0(B3))) & ~ r2_hidden(k1_funct_1(B3,B4),B2) ))

% (replSepC2 @ (m1_subset_1 @ B1)
%                   @  (^[B3:$i]: (m1_trees_1 @ (k9_xtuple_0 @ B3)))
%  	          @ (^[B3,B4:$i]: ((~ ( r2_hidden @ B4 @ (k3_trees_1 @ (k9_xtuple_0 @ B3)))) & (~ ( r2_hidden @ (k1_funct_1 @ B3 @ B4) @ B2) )))
%                   @ (^[B3,B4:$i]: ( k5_trees_2 @ B3 @ B4)))

% thf(eps_ax,axiom,(! [P:$i>$o] : (! [X:$i] : ((P@X) => (P@(eps@P)))))).


sort_transform_thf(SubstsIn,SubstsOut,all(Svars,Trm,Frm),Result):-
	sort_transform_thf_qlist_fr(SubstsIn,SubstsOut1,Svars,Qvars,Preds),
	sort_transform_thf(SubstsOut1,SubstsOut2,Trm,Trm1),
	sort_transform_thf(SubstsOut2,SubstsOut,Frm,Frm1), !,
	Trm2 = ('^' Qvars : Trm1),
	Frm2 = ('^' Qvars : Frm1),
	length(Qvars, Arity),
	atom_concat(replSep, Arity, ReplSep),
	append(Preds,[Trm2,Frm2],Args1),
	foldl(@, ReplSep, Args1, Result).

sort_transform_thf(SubstsIn,SubstsOut,the(Type),Result):-
	sort_transform_thf(SubstsIn,SubstsOut,sort(NewVar,Type),Z1),
	Result = (eps @ ('^' [NewVar : $i] : Z1)).




% This clause is redundant now, sort trafo can be done only after 'all' removal
% sort_transform_thf(SubstsIn,SubstsOut,all(Svars,Trm,Frm),all(Qvars,Trm1,RelatFrm1)):-
% 	sort_transform_qlist(SubstsIn,SubstsOut1,Svars,Qvars,Preds),
% 	sort_transform_thf(SubstsOut1,SubstsOut2,Trm,Trm1),
% 	sort_transform_thf(SubstsOut2,SubstsOut,Frm,Frm1), !,
% 	(
% 	  Preds == $true -> RelatFrm1 = Frm1;
% 	  RelatFrm1 = (Preds & Frm1)
% 	).



sort_transform_thf(SubstsIn,SubstsOut,sort(X,Y1 & Y2),SortPreds):-
	sort_transform_thf(SubstsIn,SubstsOut1,sort(X,Y1),Z1),
	sort_transform_thf(SubstsOut1,SubstsOut,sort(X,Y2),Z2), !,
	(
	  Z1 == $true -> SortPreds = Z2;
	  (
	    Z2 == $true -> SortPreds = Z1;
	    SortPreds = (Z1 & Z2)
	  )
	).
sort_transform_thf(SubstsIn,SubstsOut,sort(X,~Y),~Z):-
	sort_transform_thf(SubstsIn,SubstsOut,sort(X,Y),Z).
sort_transform_thf(S1,S1,sort(_,$true),$true).
sort_transform_thf(S1,S1,sort(_,$false),$false).
sort_transform_thf(SubstsIn,SubstsOut,sort(X,Y),Z):-
	Y =.. [F|Args],
	sort_transform_thf_l(SubstsIn,SubstsOut,[X|Args],Args1),
	foldl(@, F, Args1, Z).

% we should not get here
sort_transform_thf(_SubstsIn,_SubstsOut,sort(_,_),_):- throw(sort).
% removal of $true
sort_transform_thf(SubstsIn,SubstsOut,(A | B), Result):-
	sort_transform_thf_l(SubstsIn,SubstsOut,[A,B],[A1,B1]),!,
	(
	  (A1 == $true;B1 == $true) -> Result = $true;
	  Result = (A1 | B1)
	).
sort_transform_thf(SubstsIn,SubstsOut,A & B, Result):-
	sort_transform_thf_l(SubstsIn,SubstsOut,[A,B],[A1,B1]),!,
	(
	  A1== $true -> Result = B1;
	  (
	    B1== $true -> Result = A1;
	    Result = (A1 & B1)
	  )
	).
sort_transform_thf(SubstsIn,SubstsOut,A => B, Result):-
	sort_transform_thf_l(SubstsIn,SubstsOut,[A,B],[A1,B1]),!,
	(
	  A1== $true -> Result = B1;
	  (
	    B1== $true -> Result = $true;
	    Result = (A1 => B1)
	  )
	).
sort_transform_thf(SubstsIn,SubstsOut,A <=> B, Result):-
	sort_transform_thf_l(SubstsIn,SubstsOut,[A,B],[A1,B1]),!,
	(
	  A1== $true -> Result = B1;
	  (
	    B1== $true -> Result = A1;
	    Result = (A1 <=> B1)
	  )
	).

sort_transform_thf(S1,S1,$true,$true).
sort_transform_thf(S1,S1,$false,$false).

sort_transform_thf(SubstsIn,SubstsOut,~X1,~X2):-
	sort_transform_thf(SubstsIn,SubstsOut,X1,X2).

% functor traversal; scheme functors and preds get replaced by a new
% variable and a declaration is created for them; the replacements get
% collected so we can easily undo them by instatiation later
sort_transform_thf(SubstsIn,SubstsOut,X1,X2):-
	X1 =.. [H1|T1],
	sort_transform_thf_l(SubstsIn,SubstsOut1,T1,T2),
	(
	 mptp_sch_func_sym(H1) ->
	 (
	  member([H1,H2|_],SubstsOut1) ->
	  SubstsOut = SubstsOut1
	 ;
	  length(T1,A),
	  A1 is A + 1,
	  fill('$i',A1,List),
	  concat_atom(List, ' > ', Decl),
	  SubstsOut = [[H1,H2,Decl]|SubstsOut1]
	 )
	;
	 (
	  mptp_sch_pred_sym(H1) ->
	  (
	   member([H1,H2|_],SubstsOut1) ->
	   SubstsOut = SubstsOut1
	  ;
	   length(T1,A),
	   fill('$i',A,List),
	   append(List,['$o'],List1),
	   concat_atom(List1, ' > ', Decl),
	   SubstsOut = [[H1,H2,Decl]|SubstsOut1]
	 )
	 ;
	  H2 = H1, SubstsOut = SubstsOut1
	 )
	),
	foldl(@, H2, T2, X2).

sort_transform_thf_l(SubstsIn,SubstsIn,[],[]).
sort_transform_thf_l(SubstsIn,SubstsOut,[H|T],[H1|T1]):-
	sort_transform_thf(SubstsIn,SubstsOut1,H,H1),
	sort_transform_thf_l(SubstsOut1,SubstsOut,T,T1).





%%%%%%%%%%%%%%%%%%%% Fraenkel de-anonymization %%%%%%%%%%%%%%%%%%%%

%% First we replace fraenkels by placeholder variables which we remember, and collect
%% the fraenkels (with their context) into a list corresponding to the variables.
%% Then we find optimal 'skolem' definitions corresponding to the fraenkels,
%% instantiate such functors for each frankel with its context, and
%% put them into the formulas by unifying them with the placeholder variables.
%% GroundCopy is kept for exact comparisons envolving context


%% context has to be updated by the current variable before descent to the rest of variables
all_collect_qlist([],[],C,C,[]).
all_collect_qlist([(X:S)|T],[(X:S1)|T1],Context,NewContext,Info):-
	all_collect(S,S1,Context,Info_s),
	append(Context,[(X:S1)],C1),
	all_collect_qlist(T,T1,C1,NewContext,Info_t),
	append(Info_s,Info_t,Info).

fr_vars(FrInfo, FrVars):- maplist(nth1(1), FrInfo, FrVars).
split_svars(Vars,Sorts,Svars):- zip_s(':', Vars, Sorts, Svars).


%% all_collect_top(+In,-Out,-Infos)
%%
%% Replace fraenkels in In by variables in Out, putting the needed
%% info (for creating fraenkel defs) in Infos.
%% Now also treats choice terms together with freankels.
all_collect_top(In,Out,Info):- all_collect(In,Out,[],Info),!.


%% all_collect(+InTerm,-OutTerm,+Context=[(Var:Sort1)|RestV],
%%             -Info=[[NewVar,Context,all(Svars1,Trm1,Frm1),GroundCopy]|RestI])
%%
%% First we replace fraenkels by NewVarplaceholder variables which we remember, and collect
%% the fraenkels (with their context) into a list corresponding to the variables.
%% Then we find optimal 'skolem' definitions corresponding to the fraenkels,
%% instantiate such functors for each frankel with its context, and
%% put them into the formulas by unifying them with the placeholder variables.
%% GroundCopy is kept for exact comparisons envolving context.
%% In a similar way the choice terms (the) are treated: replaced by placeholders,
%% and collected with contexts to a list:
%% -Info=[[NewVar,Context,the(Type1),GroundCopy]|RestI]

%% end of traversal
all_collect(X,X,_,[]):- atomic(X); var(X).

all_collect(! Svars : Y, ! Svars1 : Y1, Context, Info_r):-
	all_collect_qlist(Svars,Svars1,Context,NewContext,Info_s),
	all_collect(Y,Y1,NewContext,Info_y),
	append(Info_s,Info_y,Info_r).
all_collect(? Svars : Y, ? Svars1 : Y1, Context, Info_r):-
	all_collect_qlist(Svars,Svars1,Context,NewContext,Info_s),
	all_collect(Y,Y1,NewContext,Info_y),
	append(Info_s,Info_y,Info_r).
% fix context!!
all_collect(all(Svars,Trm,Frm),NewVar,Context,
	    [[NewVar,RContext,all(Svars1,Trm1,Frm1),GroundCopy]|Info_r]):-
	(optimize_fraenkel ->
	    (free_variables(all(Svars,Trm,Frm), FreeVars),
		real_context(FreeVars, Context, RContext));
	    RContext = Context),
	copy_term([RContext,all(Svars,Trm,Frm)],GroundCopy),
	numbervars(GroundCopy,0,_),
	all_collect_qlist(Svars,Svars1,Context,NewContext,Info),
	all_collect_l([Trm,Frm],[Trm1,Frm1],NewContext,Info_l),
	append(Info,Info_l,Info_r).

% unlike above, there is no qlist  here
all_collect(the(Type),NewVar,Context,
	    [[NewVar,RContext,the(Type1),GroundCopy]|Info_r]):-
	(optimize_fraenkel ->
	    (free_variables(the(Type), FreeVars),
		real_context(FreeVars, Context, RContext));
	    RContext = Context),
	copy_term([RContext,the(Type)],GroundCopy),
	numbervars(GroundCopy,0,_),
	all_collect(Type,Type1,Context,Info_r).

% this can only be used if contexts of arguments are independent!
all_collect(X1,X2,Context,Info):-
	X1 =.. [H1|T1],
	all_collect_l(T1,T2,Context,Info),
	X2 =.. [H1|T2].

% this can only be used if contexts of arguments are independent!
all_collect_l([],[],_,[]).
all_collect_l([H1|T1],[H2|T2],Context,Info_r):-
	all_collect(H1,H2,Context,Info),
	all_collect_l(T1,T2,Context,Info_l),
	append(Info,Info_l,Info_r).

%% only select context necessary for InVars from BigC
%% done by copy & number to avoid freevars
is_numvar(X):- ground(X), X=..['$VAR'|_].


%% add variables recursively needed for New (i.e. in their sorts)
%% AllVars and AllSorts must be aligned
add_real_numvars(_,_,Old,[],Old).
add_real_numvars(AllVars,AllSorts,Old,New,Result):-
	append(Old,New,Old1),
	findall(S, (member(V,New),nth1(N,AllVars,V),nth1(N,AllSorts,S)), Srts0),
	collect_with_count_top(is_numvar,Srts0,New0,_),
	intersection(AllVars,New0,New1),
	subtract(New1,Old1,New2),
	add_real_numvars(AllVars,AllSorts,Old1,New2,Result).

%% real_context(+InVars, +BigC, -RealC)
%% puts the "really needed" variables from BigContext into RealContext
%% BigC and RealC are lists of Var:Sort
%% bagof needed in the end instead of findall! (not to spoil freevars);
%% hence the alternative - bagof fails with empty list unlike findall
%% N1^ is needed for bagof not to bind N1
real_context(InVars, BigC, RealC):-
	split_svars(Vars,Sorts,BigC),
	copy_term([InVars,Vars,Sorts],[InVars1,Vars1,Sorts1]),
	numbervars([InVars1,Vars1,Sorts1],0,_),
	intersection(Vars1,InVars1,Added),
	add_real_numvars(Vars1,Sorts1,[],Added,AllAdded),
	(AllAdded = [] -> RealC = [];
	    (findall(N, (member(V,AllAdded),nth1(N,Vars1,V)), Nrs),
		sort(Nrs,Nrs1),
		bagof(C, (N1^member(N1,Nrs1),nth1(N1,BigC,C)), RealC))).


% ##todo: this now assumes that length of Context is number of
%       preceding variables - fix for other quantification formats
% create freankel (skolem) definition:
% all([X1,X2:integer], plus(X1,X2), (X1 < X2)) generates:
% ![X]:( in(X,fraenkel_skolem_functor_1) <=>
%         ?[X1,X2:integer]:( X = plus(X1,X2) & X1 < X2))
% Take care with variables, context is shared with formulas,
% newvars are used as placeholders for other fraenkels.
% +Info=[[NewVar,Context,all(Svars1,Trm1,Frm1),GroundCopy]|RestI])
% where GroundCopy is cretaed in all_collect by following:
% copy_term([RContext,all(Svars,Trm,Frm)],GroundCopy),
% numbervars(GroundCopy,0,_),
% which means that two fraenkels with the same GroundCopy are
% the same (because the RContext is complete, and all symbol names
% are absolute inside one article)
% +FrInfo, -NewFrInfo ... unordered list of
% lists of Fraenkel symbols starting with their arity, e.g.:
% [[0|FrSymsOfArity_0],[2|FrSymsOfArity_2],[1|FrSymsOfArity_1]],


%% new_fr_sym(+File, +Arity, +FrInfo, -NewFrInfo, -NewSym)
%%
%% Note that the fraenkel and choice symbols are now treated together,
%% even though fraenkels start with 'a' and choices with 'o'. So the
%% numbering is contiguous across both now.
new_fr_sym(File, Arity, FrInfo, NewFrInfo, NewSym):-
	% find the sublist of symbols with Arity
	( select([Arity|FrSyms], FrInfo, TmpInfo);
	    ( FrSyms = [], TmpInfo = FrInfo )),
	length(FrSyms, Nr),
	concat_atom([a,Arity,Nr,File], '_', NewSym),
	% update the sublist of symbols with Arity
	select([Arity,NewSym|FrSyms], NewFrInfo, TmpInfo), ! .

new_the_sym(File, Arity, FrInfo, NewFrInfo, NewSym):-
        % find the sublist of symbols with Arity
	( select([Arity|FrSyms], FrInfo, TmpInfo);
	    ( FrSyms = [], TmpInfo = FrInfo )),
	length(FrSyms, Nr),
	concat_atom([o,Arity,Nr,File], '_', NewSym),
	% update the sublist of symbols with Arity
	select([Arity,NewSym|FrSyms], NewFrInfo, TmpInfo), ! .

%% now also allows re-creating the def from a cached symbol name,
%% if NewSym is not a variable
mk_fraenkel_def(File, Var, Context, all(Svars1,Trm1,Frm1),
		FrInfo, NewFrInfo, NewSym, Def) :-
	split_svars(Vars, _, Context),
	length(Vars, Arity),
	(var(NewSym) ->
	    new_fr_sym(File, Arity, FrInfo, NewFrInfo, NewSym)
	;
	    NewFrInfo = FrInfo
	),
	FrTrm =.. [NewSym|Vars],
	InPred =.. [r2_hidden, X, FrTrm],
	ExFla = ( ? Svars1 : ( ( X = Trm1 ) & Frm1)),
	Def = ( ! [(X : $true)|Context] : ( InPred <=> ExFla ) ),
	Var = FrTrm.

%% version for choiceterms (keeping the name of predicate for simplicity)
%% now also allows re-creating the def from a cached symbol name,
%% if NewSym is not a variable
mk_fraenkel_def(File, Var, Context, the(Type1),
		FrInfo, NewFrInfo, NewSym, Def) :-
	split_svars(Vars, _, Context),
	length(Vars, Arity),
	(var(NewSym) ->
	    new_the_sym(File, Arity, FrInfo, NewFrInfo, NewSym)
	;
	    NewFrInfo = FrInfo
	),
	ChoiceTrm =.. [NewSym|Vars],
	SrtFla = sort(ChoiceTrm, Type1),
	(Arity = 0 ->
	 Def = SrtFla
	;
	 Def = ( ! Context: SrtFla )
	),
	Var = ChoiceTrm.

%% mk_fraenkel_defs_top(+File, +Infos, -NewFrSyms, -NewDefs)
%%
%% Using Infos created by all_collect_top, fraenkels are defined
%% and instantiated in the formulas (sharing vars with infos).
%% The NewSym and GroundCopy of already created defs are passed in the loop,
%% and the GroundCopy is checked first for being in the list when
%% creating a new fraenkel def. If it already exists, the old symbol is
%% used. This can hopefully be used to keep the naming of
%% fraenkels stable across various invocations (e.g. when working only
%% with article's exported items vs. when working with the full article) -
%% remembering of these pairs across such invocations seems to be enough.
%% NewDefs is a list of pairs [DefinedSymbol, Def] now

/* old noncaching version to be removed after checks
mk_fraenkel_defs_top(File, Infos, NewFrSyms, NewDefs):-
	mk_fraenkel_defs(File, Infos, [], [], NewFrSyms, NewDefs),!.

mk_fraenkel_defs(_, [], _, FrSyms, FrSyms, []).
mk_fraenkel_defs(File, [[V,C,_,GrC]|T], GrCopies, FrSyms, NewFrSyms, Defs):-
	member([FoundSym,GrC], GrCopies), !,
	split_svars(Vars, _, C),
	V =.. [FoundSym|Vars],
	mk_fraenkel_defs(File, T, GrCopies, FrSyms, NewFrSyms, Defs).

mk_fraenkel_defs(File, [[V,C,Trm,GrC]|T], GrCopies, FrSyms,
		 NewFrSyms, [[NewSym,D]|Defs]):-
	mk_fraenkel_def(File, V, C, Trm, FrSyms, FrSyms1, NewSym, D),
	mk_fraenkel_defs(File, T, [[NewSym,GrC]|GrCopies],
			 FrSyms1, NewFrSyms, Defs).
*/



%% updates the fraenkel_cached predicate
%% note that NewFrSyms is the new cached version, and does not
%% necessarily correspond to NewDefs (i.e. some defs might
%% not be created)
mk_fraenkel_defs_top(File, Infos, NewFrSyms, NewDefs):-
	(fraenkel_cached(File, CachedGround0, CachedFrSyms0) ->
	    CachedGround = CachedGround0,
	    CachedFrSyms = CachedFrSyms0
	;
	    CachedGround = [],
	    CachedFrSyms = []
	),
	%% GrCopiesOut and CachedGround will generally overlap after this,
	%% doing union is OK
	mk_fraenkel_defs(File, Infos, [], GrCopiesOut, CachedFrSyms, CachedGround, NewFrSyms, NewDefs),!,
	union(CachedGround, GrCopiesOut, NewCachedGround),
	retractall(fraenkel_cached(File, _, _)),
	assert(fraenkel_cached(File, NewCachedGround, NewFrSyms)).

mk_fraenkel_defs(_, [], GrCopiesIn, GrCopiesIn, FrSyms, _, FrSyms, []).

%% first check for being "totally processed"
mk_fraenkel_defs(File, [[V,C,_,GrC]|T], GrCopiesIn, GrCopiesOut, FrSyms, CachedGround, NewFrSyms, Defs):-
	member([FoundSym,GrC], GrCopiesIn), !,
	split_svars(Vars, _, C),
	% this is where the variable gets instantiated to the fraenkel functor applied to context
	V =.. [FoundSym|Vars],
	mk_fraenkel_defs(File, T, GrCopiesIn, GrCopiesOut, FrSyms, CachedGround, NewFrSyms, Defs).

%% if cached, create the def and put the GroundCopy into normal GrCopies,
%% so that the def is not created again (it will get caught by the previous clause).
%% FrSyms are unchanged, because they contain CachedSym already
mk_fraenkel_defs(File, [[V,C,Trm,GrC]|T], GrCopiesIn, GrCopiesOut, FrSyms, CachedGround,
		 NewFrSyms, [[CachedSym,D]|Defs]):-
	member([CachedSym,GrC], CachedGround), !,
	mk_fraenkel_def(File, V, C, Trm, FrSyms, FrSyms, CachedSym, D),
	mk_fraenkel_defs(File, T, [[CachedSym,GrC]|GrCopiesIn],
			 GrCopiesOut, FrSyms, CachedGround, NewFrSyms, Defs).

%% otherwise create the new def, update both GrCopies and FrSyms
mk_fraenkel_defs(File, [[V,C,Trm,GrC]|T], GrCopiesIn, GrCopiesOut, FrSyms, CachedGround,
		 NewFrSyms, [[NewSym,D]|Defs]):-
	mk_fraenkel_def(File, V, C, Trm, FrSyms, FrSyms1, NewSym, D),
	mk_fraenkel_defs(File, T, [[NewSym,GrC]|GrCopiesIn], GrCopiesOut,
			 FrSyms1, CachedGround, NewFrSyms, Defs).




%%%%%%%%%%%%%%%%%%%% FOF accessors %%%%%%%%%%%%%%%%%%%%

% should be unique for Ref
get_ref_fla(Ref,Fla):- fof_name(Ref,Id),!,clause(fof(Ref,_,Fla,_,_),_,Id),!.
get_ref_file(Ref,File):- fof_name(Ref,Id),!,clause(fof(Ref,_,_,file(File,_),_),_,Id),!.
get_ref_fof(Ref,fof(Ref,R1,R2,R3,R4)):-
	fof_name(Ref,Id),!,
	clause(fof(Ref,R1,R2,R3,R4),_,Id),!.

% dynamic caching for speed
get_ref_syms(Ref,Syms):- fof_syms(Ref,Syms),!.
get_ref_syms(Ref,Syms):-
	get_ref_fla(Ref,Fla),
	collect_symbols_top(Fla,Syms),
	assert(fof_syms(Ref,Syms)),!.

%% ref_mpropkind(?MPropKind, +Ref)
%%
%% Tell the MPropKind of Refs. The arguments are in the opposite order
%% to make sublist(ref_mpropkind(Kind), RefsIn, RefsOut) calls possible.
ref_mpropkind(MPropKind, Ref):-
	get_ref_fof(Ref,fof(Ref,_,_,_,[mptp_info(_,_,MPropKind,_,_)|_])).

ref_is_rcluster(Ref):- ref_mpropkind(rcluster, Ref).

%% get_sec_info_refs(+RefsIn, +Secs, +Info, -NewRefs)
%%
%% Find all fofs with fof_section in Secs, and Info slot as prescribed.
%% Those not in RefsIn return in NewRefs.
%% Is not unique for Sec and Info.
get_sec_info_refs(RefsIn, Secs, Info, NewRefs):- !,
	findall(Ref1, (member(Sec1,Secs), fof_section(Sec1,Id),
			  clause(fof(Ref1,_,_,file(_,Sec1), Info),_,Id)), Refs1),
	subtract(Refs1, RefsIn, NewRefs).

%% this was very slow for many clauses, even with maximum prolog indexing
%% so the previous clause is used instead with homemade indexing
get_sec_info_refs_old(RefsIn, Secs, Info, NewRefs):-
	findall(Ref1, (member(Sec1,Secs), fof(Ref1,_,_,file(_,Sec1), Info)), Refs1),
	subtract(Refs1, RefsIn, NewRefs).

get_consider_empty_justif_types(RefsIn, ConsEmptyJustifTypes):-
	findall(Type,
		(
		  member(Ref, RefsIn),
		  get_ref_fof(Ref,fof(Ref,_,_,_,
				      [mptp_info(_,_,_,_,
						 [_,mizar_item(consider_justification),
						  considered_constants(Consts)|_]),
				       inference(mizar_by,_,[])|_])),
		  member(Const,Consts),
		  fof_section(Const,Id),
		  clause(fof(_,_,sort(Const,Type),file(_,Const),
			     [mptp_info(_,_,constant,_,[consider,type|_])|_]),_,Id)

		),
		ConsEmptyJustifTypes).


%%%%%%%%%%%%%%%%%%%% Background computation %%%%%%%%%%%%%%%%%%%%

%% add properties for SymsIn
get_properties(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,_,_,[property(_)])|_], AddedRefs).

get_existence(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,_,_,[existence])|_], AddedRefs).

get_redefinitions(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,_,_,[redefinition(_,_,_,_)])|_],
			  AddedRefs).

get_fraenkel_defs(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,fraenkel,_,_)|_], Refs1),
	(Refs1 = [] -> AddedRefs = [];
	    (memberchk(t2_tarski, RefsIn) -> AddedRefs = Refs1;
		AddedRefs = [t2_tarski|Refs1])).

get_types(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,_,_,[ctype])|_], Refs1),
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,constant,_,[_,type|_])|_], Refs2),
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,functor,_,[scheme,type|_])|_], Refs3),
	flatten([Refs1, Refs2, Refs3], AddedRefs).

get_equalities(RefsIn,SymsIn,AddedRefs):-
	get_sec_info_refs(RefsIn, SymsIn,
			  [mptp_info(_,_,constant,_,[_,equality|_])], AddedRefs).

%% get functor definitions by 'equals' but only for articles
%% mentioned in the 'definitions' env. declaration
get_eq_defs(Files,RefsIn,SymsIn,AddedRefs):-
	findall(Ref1, (member(Sym,SymsIn), fof_eq_def(Sym, Id),
			  clause(fof(Ref1,_,_,file(F,_),_),_,Id),
			  member(F,Files)), Refs1),
	subtract(Refs1, RefsIn, AddedRefs).


func_eq_directive(Defs,Theory):-
	mml_version([_,_,V]),
	(V < 1172 ->
	 member(definitions(Defs),Theory)
	;
	 member(equalities(Defs),Theory)
	).

pred_expand_directive(Expands,Theory):-
	mml_version([_,_,V]),
	(V < 1172 ->
	 Expands = []
	;
	 member(expansions(Expands),Theory)
	).

%% ##TODO: add also fof_sort_def in the findall - can be a bit
%% expensive if done also for cluster symbols however, so we might do
%% it more cautiously - omitting for now since attributive expansions
%% are hopefully not so common
get_pred_expansions(_F,_Theory,_RefsIn,_NewSyms,[]):- mml_version([_,_,V]), V < 1172, !.
get_pred_expansions(F,Theory,RefsIn,NewSyms,RefsOut):-	 
	member(expansions(Expands),Theory),
	findall(Ref1, (member(Sym,NewSyms), fof_pred_def(Sym, Id),
		       clause(fof(Ref1,_,_,file(G,_),_),_,Id),
		       member(G,[F|Expands])), Refs1),
	subtract(Refs1, RefsIn, RefsOut).


%% version for mizar_by and mizar_proof; mizar_proof should be
%% enhanced a bit probably
%% OldSyms are used only for clusters and requirements
one_pass(F,Pos,Top,InfKind,RefsIn,OldSyms,NewSyms,ZeroedRefs,AddedRefs):-
	member(InfKind,[mizar_by,mizar_proof]),
	theory(F, Theory),
	member(registrations(Regs),Theory),
	member(requirements(Reqs),Theory),
	func_eq_directive(Defs,Theory),
	get_properties(RefsIn,NewSyms,Refs0),
	get_existence(RefsIn,NewSyms,Refs1),
	get_redefinitions(RefsIn,NewSyms,Refs2),
	get_types(RefsIn,NewSyms,Refs3),
	get_equalities(RefsIn,NewSyms,Refs4),
%% Refs5=[],
	get_clusters([F|Regs],Pos,RefsIn,OldSyms,NewSyms,Refs5),
	get_identifyexp([F|Regs],Pos,RefsIn,OldSyms,NewSyms,Refs5i),
	get_requirements(Reqs,RefsIn,OldSyms,NewSyms,ZeroedRefs,Refs6),
	get_fraenkel_defs(RefsIn,NewSyms,Refs7),
	get_eq_defs([F|Defs],RefsIn,NewSyms,Refs8),
	get_nr_types(Reqs,RefsIn,NewSyms,Refs9),
	(Top = top ->
	 get_pred_expansions(F,Theory,RefsIn,NewSyms,Refs10)
	;
	 Refs10 = []
	),
	flatten([Refs0,Refs1,Refs2,Refs3,Refs4,Refs5,Refs5i,Refs6,Refs7,Refs8,Refs9,Refs10],
		AddedRefs).

%% version for mizar_by and mizar_proof pruning
%%  existence properties and clusters, and requirement flas.
%%  -- just heuristical, quite often incomplete.
one_pass(F,Pos,Top,mizar_no_existence,RefsIn,OldSyms,NewSyms,_ZeroedRefs,AddedRefs):-
	theory(F, Theory),
	member(registrations(Regs),Theory),
	member(requirements(Reqs),Theory),
	func_eq_directive(Defs,Theory),
	get_properties(RefsIn,NewSyms,Refs0),
	get_redefinitions(RefsIn,NewSyms,Refs2),
	get_types(RefsIn,NewSyms,Refs3),
	get_equalities(RefsIn,NewSyms,Refs4),
%% Refs5=[],
	get_fc_clusters([F|Regs],Pos,RefsIn,OldSyms,NewSyms,Refs5),
	get_identifyexp([F|Regs],Pos,RefsIn,OldSyms,NewSyms,Refs5i),
	get_fraenkel_defs(RefsIn,NewSyms,Refs7),
	get_eq_defs([F|Defs],RefsIn,NewSyms,Refs8),
	get_nr_types(Reqs,RefsIn,NewSyms,Refs9),
	(Top = top ->
	 get_pred_expansions(F,Theory,RefsIn,NewSyms,Refs10)
	;
	 Refs10 = []
	),
	flatten([Refs0,Refs2,Refs3,Refs4,Refs5,Refs5i,Refs7,Refs8,Refs9,Refs10],
		AddedRefs).


%% version for mizar_from
%% OldSyms are used only for clusters and requirements,
%% fraenkel defs should not be needed --
%% NO: unfortunately, fraenkel defs are needed, see e1_46__relset_2
%% ###TODO: also another possible problem there: fraenkel definitions don't seem
%%          to be instantiated properly in scheme instances
one_pass(F,Pos,_Top,mizar_from,RefsIn,OldSyms,NewSyms,_ZeroedRefs,AddedRefs):-
	theory(F, Theory),
	member(registrations(Regs),Theory),
	member(requirements(Reqs),Theory),
	get_properties(RefsIn,NewSyms,Refs0),
	get_redefinitions(RefsIn,NewSyms,Refs2),
	get_types(RefsIn,NewSyms,Refs3),
	get_clusters([F|Regs],Pos,RefsIn,OldSyms,NewSyms,Refs5),
	get_identifyexp([F|Regs],Pos,RefsIn,OldSyms,NewSyms,Refs5i),
	get_nr_types(Reqs,RefsIn,NewSyms,Refs6),
	get_fraenkel_defs(RefsIn,NewSyms,Refs7),
	flatten([Refs0,Refs2,Refs3,Refs5,Refs5i,Refs6,Refs7], AddedRefs).


%% fixpoint(+F,+Pos,+InfKind,+RefsIn,+OldSyms,+NewSyms,-RefsOut)
%%
%% Add symbol references until nothing added, OldSyms=[] when called first,
%% it only serves during the recursion.
%% fxp_refsin_/1 is an updated list of needed refs computed by the algo
%% fxp_allsyms_/1 is an updated list of included symbols computed by the algo
%% We assume that NewSyms comes sorted.
%% ###TODO: keep the refs of different kinds in separate lists,
%%          so that the lookup using member/2 is not so expensive
%%          (or just use the recorded db
fixpoint(F,Pos,InfKind,RefsIn,[],NewSyms,RefsOut):-
	retractall(fxp_refsin_(_)),
	retractall(fxp_allsyms_(_)),
	findall(d,(member(R,RefsIn),assert(fxp_refsin_(R))),_),
	findall(d,(member(S,NewSyms),assert(fxp_allsyms_(S))),_),
	init_fxp_sym_flags,
	fixpoint_(F,Pos,top,InfKind,RefsIn,[],NewSyms,RefsOut).

fixpoint_(F,Pos,Top,InfKind,RefsIn,OldSyms,NewSyms,RefsOut):-
	decrease_fxp_sym_flags(NewSyms, ZeroedRefs),
	dbg(dbg_FIXPOINT,
	    (print(nEWSYMS(NewSyms, ZeroedRefs)),nl)),
	one_pass(F, Pos, Top, InfKind, RefsIn, OldSyms, NewSyms, ZeroedRefs, Refs1), !,
	(
	  Refs1 = [] ->
	  (
	    (
%%	      member(opt_FXP_CHECK_CNSDR, Options),
	      member(InfKind,[mizar_by,mizar_proof]),
	      get_consider_empty_justif_types(RefsIn, ConsEmptyJustifTypes),
	      ConsEmptyJustifTypes = [_ | _]
	    ) ->
	    findall(ConsInfo,
		    (
		      member(ConsType, ConsEmptyJustifTypes),
		      create_type_info_for_consider(ConsType, ConsInfo)
		    ),
		    ConsInfos0
		  ),
	    sort(ConsInfos0, ConsInfos),
	    ord_union(OldSyms, NewSyms, AllSyms),
	    add_rclusters_for_unhandled(F, Pos, RefsIn, AllSyms, ConsInfos, NewRClusters),
	    union(RefsIn, NewRClusters, RefsOut)
	  ;
	    RefsOut = RefsIn
	  )
	;
	  union(Refs1, RefsIn, Refs2),
	  findall(d,(member(R,Refs1),assert(fxp_refsin_(R))),_),
	  ord_union(OldSyms, NewSyms, OldSyms1),
	  maplist(get_ref_syms, Refs1, SymsL1),
	  ord_union(SymsL1,Syms1),
	  ord_subtract(Syms1, OldSyms1, NewSyms1),
	  findall(d,(member(S,NewSyms1),assert(fxp_allsyms_(S))),_),
	  fixpoint_(F, Pos, nontop, InfKind, Refs2, OldSyms1, NewSyms1, RefsOut)
	).


%% create_type_info_for_consider(ConsType, -ConsInfo)
%%
create_type_info_for_consider(ConsType, ConsInfo):-
	constr2list('&', _Last, AndList, ConsType),!,
	process_attrsmode_list_for_cons(AndList, ConsAttrs0, Radix),
	create_cons_info(ConsAttrs0, Radix, ConsInfo).

create_cons_info(ConsAttrs0, Radix, ConsInfo):-
	(
	  Radix = $true ->
	  ConsMode = $true,
	  ConsFuncs = []
	;
	  Radix =.. [ConsMode | Args],
	  collect_symbols_top(Args, RadixArgSyms),
	  sublist(mptp_func_sym, RadixArgSyms, ConsFuncs0),
	  sort(ConsFuncs0, ConsFuncs)
	),
	length(ConsFuncs, ConsFuncsNr),
	sort(ConsAttrs0, ConsAttrs),
	length(ConsAttrs, ConsAttrsNr),
	ConsInfo = [ConsMode, ConsFuncsNr, ConsAttrsNr, ConsFuncs, ConsAttrs],!.

%% process_attrsmode_list_for_cons(+AndList, -ConsAttrs0, -Radix),
%%
%% Expects a list possibly ending with a radix. Converts the negated atributes
%% to 'w'-symbols, and forgest their arguments. If there is no radix, it
%% pretends to be $true
process_attrsmode_list_for_cons([AttrTermOrRadix | Rest], [ AttrSym | ConsAttrsRest], Radix):-
	process_attrterm_for_cons(AttrTermOrRadix, AttrSym), !,
	process_attrsmode_list_for_cons(Rest, ConsAttrsRest, Radix).

process_attrsmode_list_for_cons([Radix], [], Radix).
process_attrsmode_list_for_cons([], [], $true).


%% process_attrterm_for_cons(+AttrTerm, -AttrSym)
process_attrterm_for_cons(~(AttrTerm), NegAttrSym):- !,
	AttrTerm =.. [PosAttrSym | _],
	ensure(atom_chars(PosAttrSym, [v | Rest]), process_attrterm_for_cons),
	atom_chars(NegAttrSym, [ w | Rest]), !.

process_attrterm_for_cons(AttrTerm, PosAttrSym):-
	AttrTerm =.. [PosAttrSym | _],
	atom_chars(PosAttrSym, [v | _]).


%% get_current_rclusters(+RefsIn, ?Rclusters)
%%
get_current_rclusters(RefsIn, Rclusters):-
	sublist(ref_is_rcluster, RefsIn, Rclusters).

%% rcluster_covers_consider(+RCl, [+ConsMode, +ConsFuncsNr, +ConsAttrsNr, +ConsFuncs, +ConsAttrs])
%%
%% Ensure that all attributes in the consider are also in the rcluster,
%% that the rcluster's mode is same as in consider, and that all
%% functors in the rcluster are also in the consider.
%% Note if we know all this, the additional condition for the consider to be
%% fully covered by the rcluster is just that ConsFuncsNr = FuncsNr.
%% ###TODO: if this is still too weak, take care also of the context vars/consts on
%%          which the rcluster/consider depends.
rcluster_covers_consider(RCl, ConsInfo):-
	ConsInfo = [ConsMode, ConsFuncsNr, ConsAttrsNr, ConsFuncs, ConsAttrs],
	rc_syms_for_consider(RCl, _File, ConsMode, FuncsNr, AttrsNr, FuncSyms, AttrSyms),
	FuncsNr =< ConsFuncsNr,
	ConsAttrsNr =< AttrsNr,
	subset(FuncSyms, ConsFuncs),
	subset(ConsAttrs, AttrSyms),!.


%% get_covered_consider(+Rclusters, +ConsInfos, -Covered)
%%
%% Select the ConsInfos covered by some of Rclusters into Covered.
%% We don't have to check the file and position here - it has already been checked
%% at this point.
%% Note that we use setof, to only check Rclusters until we find the first covering
%% for a particul ConsInfo.
get_covered_consider(Rclusters, ConsInfos, Covered):-
	setof(ConsInfo,
		(
		  member(ConsInfo, ConsInfos),
		  member(RCl, Rclusters),
		  rcluster_covers_consider(RCl, ConsInfo)
		),
	      Covered),!.

%% to fix the failure above when the setof is empty
get_covered_consider(_Rclusters, _ConsInfos, []).


%% get_first_rcluster_cover(+Rclusters, +ConsInfo, -RCl):-
get_first_rcluster_cover(Rclusters, ConsInfo, RCl):-
	member(RCl, Rclusters),
	rcluster_covers_consider(RCl, ConsInfo), !.

%% as a first hack, no optimization here;
%% ##TODO: optimize this better, using ALGO above
cover_remaining_consider(Rclusters, ConsInfos, Covered, Covering):-
	findall([ConsInfo,RCl],
		(
		  member(ConsInfo, ConsInfos),
		  get_first_rcluster_cover(Rclusters, ConsInfo, RCl)
		),
		Res),
	zip(Covered, Covering, Res).

%% get_eligible_rclusters(+File, +Pos, +CurrentRclusters, -Eligible)
%%
%% Get rclusters usable for File and Pos, and not included in CurrentRclusters.
get_eligible_rclusters(F, Pos, CurrentRclusters, Eligible):-
	theory(F, Theory),
	member(registrations(Regs),Theory),
	findall(Ref1,
		(
		  member(F1,[F|Regs]),
		  fof_cluster(F1,Ref1,_),
		  atom_chars(Ref1,[r|_]),
		  check_cluster_position(F,Pos,F1,Ref1),
		  not(member(Ref1, CurrentRclusters))
		  %% this is faster but a bit hackier way at this point:
%%		  not(fxp_refsin_(Ref1))
		),
		Eligible).

%% add_rclusters_for_unhandled(+F, +Pos, +RefsIn, +AllSyms, +ConsInfos, -NewRClusters)
%%
%% Finish the fixpoint algo by finding NewRClusters needed for considers with
%% empty justifications (passed already preprocessed as ConsInfos).
%% ##TODO: add some debug info, when not all ConsInfos are covered.
%%
%% Longer & older rant:
%%
%% Add needed rclusters for ConsInfos coming from consider justifications.
%% Each	ConsInfo has the form [ConsMode, ConsFuncsNr, ConsAttrsNr, ConsFuncs, ConsAttrs],
%% and we require thet the ConsMode and the ConsAttrs be covered by som rcluster
%% If an appropriate rcluster is in RefsIn, we use it, otherwise we try to add
%% rcluster constrained by F's theory and position Pos.
%% We are intentionally not looking for additional mode existence fofs, because they
%% should already be there.
%% We need the following info about rclusters for this:
%% - which symbols certainly have to be present in the consider statement:
%%         the mode symbol, and the functor symbols in the rcluster
%% - which symbols form the upper bound, and we only require that the
%%         consider statement does not contain more than them:
%%         the attributes forming the proper cluster, could be also
%%         with functors, but the functors will be present in the mode
%%         anyway, so we don't need them; we however need the polarity
%%         (could be done by using  the 'w' version of the symbol for negation)
%% Indexing requirements: we have to ensure that all attributes in the consider are
%%         also in the rcluster; but typically, we'll have many rclusters, and just a few
%%         considers to deal with; so we'll just prune the rclusters by the mode & functor
%%         syms, and then check each rcluster's attrs for being superset of the consider's
%%         attrs; finally, we should also collect the optional info whether all consider's functor
%%         syms are covered by the rcluster
%% following indexing is used:
%%     rc_syms_for_consider(+RCl, -File, -ConsMode, -FuncsNr, -AttrsNr, -FuncSyms, -AttrSyms)
%% ##PROBLEM: redefinitions could mess things, e.g. the FuncSyms might differ; ignore now.
%% ##ALGO: each SymPair which is not covered by RefsIn increases the "cover count" for
%%         all eligible rclusters which cover it, and is added to their "cover list".
%%         Then we greedily choose the rclusters with largest "cover count", until
%%         all SymPairs are covered. (not done yet: This is attempted first in the "Func-mode",
%%         i.e. trying to use only rclusters that cover the SymPair also with its
%%         FuncSyms, and only with the remaining clusters we try in the AttrOrModeSyms
%%         only.)
add_rclusters_for_unhandled(F, Pos, RefsIn, _AllSyms, ConsInfos, NewRClusters):-
	get_current_rclusters(RefsIn, CurrentRclusters),
	get_covered_consider(CurrentRclusters, ConsInfos, Covered),
	subtract(ConsInfos, Covered, Uncovered),
	(
	  Uncovered = [] ->
	  NewRClusters = []
	;
	  get_eligible_rclusters(F, Pos, CurrentRclusters, Eligible),
	  cover_remaining_consider(Eligible, Uncovered, _NewlyCovered, NewRClusters)
	).


fixpoint_old(F,Pos,InfKind,RefsIn,OldSyms,NewSyms,RefsOut):-
	decrease_fxp_sym_flags(NewSyms, ZeroedRefs),
	one_pass(F, Pos, InfKind, RefsIn, OldSyms, NewSyms, ZeroedRefs, Refs1), !,
	(
	  Refs1 = [] ->
	  RefsOut = RefsIn
	;
	  union(Refs1, RefsIn, Refs2),
	  union(OldSyms, NewSyms, OldSyms1),
	  maplist(get_ref_fla, Refs1, Flas1),
	  collect_symbols_top(Flas1, Syms1),
	  subtract(Syms1, OldSyms1, NewSyms1),
	  fixpoint_old(F, Pos, InfKind, Refs2, OldSyms1, NewSyms1, RefsOut)
	).

%% antecedent symbols needed for fcluster or ccluster
cl_needed_syms_top(Fla,Syms):-
	cl_needed_syms(Fla,Syms1),
	logic_syms(Syms2),
	subtract(Syms1, Syms2, Syms), !.
cl_needed_syms( ! Svars : Fla, AnteSyms):- !,
	collect_symbols_top(Svars, Syms1),
	cl_needed_syms(Fla, Syms2),
	union(Syms1, Syms2, AnteSyms).
cl_needed_syms( sort(_,Ante) => sort(_,_), AnteSyms):- !, collect_symbols_top(Ante, AnteSyms).
cl_needed_syms( sort(Trm,_), AnteSyms):- !, collect_symbols_top(Trm, AnteSyms).
%% should not get here
cl_needed_syms(_,_):- throw(cluster).

%% return the level on which cluster must not be used
%% used also for identifyexp
get_cluster_proof_level(Ref,Lev):-
	fof_name(Ref, Id),!,
	clause(fof(Ref,_,_,_,[Info|_]), _, Id),
	Info = mptp_info(_,[],_,_,[proof_level(Lev)|_]), ! .

get_cluster_proof_level(Ref,_):-
	throw(get_cluster_proof_level(Ref)).

%% Check that cluster is applicable to [Pos1,Lev1] .
%% Only relevant if from the same file,
%% The calling code has to ensure correctness for different files.
%% Used also for identifyexp.
check_cluster_position(F,[Pos1,Lev1],F,Ref2):- !,
	ensure(article_position(Ref2,Pos2), throw(article_position2(F,Ref2))),
	dbg(dbg_CLUSTERS,
	    format('Considering cluster: ~w,~w, position: ~w with [~w,~w] ~n',
		   [Ref2,F,Pos2,Pos1,Lev1])),
	Pos2 < Pos1,
	get_cluster_proof_level(Ref2,Lev2),
	dbg(dbg_CLUSTERS, format('Cluster proof level: ~w', [Lev2])),
	not(sublevel(Lev1, Lev2)),
	dbg(dbg_CLUSTERS, format('cluster succeeded ~n')).

check_cluster_position(F,P,F,_):- throw(check_cluster_position(F,P)).
check_cluster_position(_,_,_,_).

%% fof_cluster contains precomputed info
%% assumes that F is the current article
get_clusters([F|Regs],Pos,_RefsIn,_OldSyms,_NewSyms,AddedRefs):-
	findall(Ref1, (member(F1,[F|Regs]),
			  fof_cluster(F1,Ref1,AnteSyms),
			  check_cluster_position(F,Pos,F1,Ref1),
			  not(fxp_refsin_(Ref1)),
			  checklist(fxp_allsyms_,AnteSyms)),
		AddedRefs1),
	sort(AddedRefs1,AddedRefs).

%% as get_clusters, but only conditional and functor clusters
%% (i.e. avoids existential ones)
get_fc_clusters([F|Regs],Pos,_RefsIn,_OldSyms,_NewSyms,AddedRefs):-
	findall(Ref1, (member(F1,[F|Regs]),
			  fof_cluster(F1,Ref1,AnteSyms),
			  atom_chars(Ref1,[Char|_]),
			  member(Char,[c,f]),
			  check_cluster_position(F,Pos,F1,Ref1),
			  not(fxp_refsin_(Ref1)),
			  checklist(fxp_allsyms_,AnteSyms)),
		AddedRefs1),
	sort(AddedRefs1,AddedRefs).

%% old slow versions
get_clusters_old([F|Regs],Pos,RefsIn,OldSyms,NewSyms,AddedRefs):-
	union(OldSyms, NewSyms, AllSyms),
	findall(Ref1, (member(F1,[F|Regs]),
			  fof_cluster(F1,Ref1,AnteSyms),
			  check_cluster_position(F,Pos,F1,Ref1),
			  not(member(Ref1, RefsIn)),
			  subset(AnteSyms, AllSyms)),
		AddedRefs).

get_fc_clusters_old([F|Regs],Pos,RefsIn,OldSyms,NewSyms,AddedRefs):-
	union(OldSyms, NewSyms, AllSyms),
	findall(Ref1, (member(F1,[F|Regs]),
			  fof_cluster(F1,Ref1,AnteSyms),
			  atom_chars(Ref1,[Char|_]),
			  member(Char,[c,f]),
			  check_cluster_position(F,Pos,F1,Ref1),
			  not(member(Ref1, RefsIn)),
			  subset(AnteSyms, AllSyms)),
		AddedRefs).

%% fof_identifyexp contains precomputed info
%% assumes that F is the current article
get_identifyexp([F|Regs],Pos,_RefsIn,_OldSyms,_NewSyms,AddedRefs):-
	findall(Ref1, (member(F1,[F|Regs]),
			  fof_identifyexp(F1,Ref1,AnteSyms),
			  check_cluster_position(F,Pos,F1,Ref1),
			  not(fxp_refsin_(Ref1)),
			  checklist(fxp_allsyms_,AnteSyms)),
		AddedRefs1),
	sort(AddedRefs1,AddedRefs).

%% there are now 7469 requirements arithm (MML 930)
get_requirements(Files,_RefsIn,_OldSyms,_NewSyms,ZeroedRefs,AddedRefs):-
	findall(Ref1, (
			member(Ref1, ZeroedRefs),
			fof_ante_sym_cnt(Ref1,F1,req,_),
			member(F1,Files),
			not(fxp_refsin_(Ref1)),
			%% check that all symbols of Ref1 are indeed
			%% in fxp_allsyms_, complain if not
			dbg(dbg_REQS,
			    (fof_req(F1,Ref1,Syms1),
				(checklist(fxp_allsyms_,Syms1) -> true;
				    findall(S,(member(S,Syms1),not(fxp_allsyms_(S))),Failed),
				    print(sUCKERS(Ref1,Failed)),nl)))
		      ),
		AddedRefs1),
	findall(Ref2, (
			member(F2,Files),
			hard_wired_req(F2,Ref2,Syms),
			not(fxp_refsin_(Ref2)),
			checklist(fxp_allsyms_,Syms)
		      ),
		AddedRefs2),
	%% need to remove multiples introduced by hard_wired_req
	append(AddedRefs1, AddedRefs2, AddedRefs3),
	sort(AddedRefs3, AddedRefs).

get_requirements_old1(Files,_RefsIn,_OldSyms,_NewSyms,_ZeroedRefs,AddedRefs):-
	findall(Ref1, (
			member(F1,Files),
			(fof_req(F1,Ref1,Syms); hard_wired_req(F1,Ref1,Syms)),
			not(fxp_refsin_(Ref1)),
			checklist(fxp_allsyms_,Syms)
		      ),
		AddedRefs1),
	%% need to remove multiples introduced by hard_wired_req
	sort(AddedRefs1, AddedRefs).

%% old slow version
get_requirements_old(Files,RefsIn,OldSyms,NewSyms,_ZeroedRefs,AddedRefs):-
	union(OldSyms, NewSyms, AllSyms),
	findall(Ref1, (member(F1,Files),
			  (fof_req(F1,Ref1,Syms);
			      hard_wired_req(F1,Ref1,Syms)
			  ),
			  not(member(Ref1, RefsIn)),
			  subset(Syms, AllSyms)),
		AddedRefs1),
	%% need to remove multiples introduced by hard_wired_req
	sort(AddedRefs1, AddedRefs).


%% get references for types of numbers
get_nr_types(Files,RefsIn,NewSyms,AddedRefs):-
	findall(Ref1, (member(F1,Files),
			  member(N,NewSyms),
			  integer(N),
			  get_nr_type(F1,N,Ref1),
			  not(member(Ref1, RefsIn))),
		AddedRefs).

%% get the type formula for numbers added by a requirement File
%% possibly create and index the fof, if not existing yet
get_nr_type(File,N,Name):-
	member(File, [boole,numerals]),
	integer(N),
	concat_atom([spc,N,'_',File],Name),
	(fof_name(Name,_),!
	;
	    get_nr_fof(File,N,Res),
	    Res = fof(Name,_,_,_,_),
	    assert(Res,Id),
	    assert(fof_name(Name,Id)),
	    assert(fof_section(Name,Id))
	).


%% the (non)zerotyp added by requirements numeral
spec_num_type(0,[4,_,_],(sort(0,(m2_subset_1(k1_numbers, k5_numbers)))
		 & sort(0,(m1_subset_1(k5_numbers)))
		 & sort(0,(m1_subset_1(k1_numbers))))) :- !.

spec_num_type(0,[5,_,_],sort(0,(m1_subset_1(k4_ordinal1)))):- !.

spec_num_type(N,[4|MmlVersion4],(sort(N,(IsPositive & m2_subset_1(k1_numbers, k5_numbers)))
		 & sort(N,(m1_subset_1(k5_numbers)))
		 & sort(N,(m1_subset_1(k1_numbers))))):- !, req_Positive([4|MmlVersion4],IsPositive).

spec_num_type(N,[5|MmlVersion5],sort(N,(IsPositive & m1_subset_1(k4_ordinal1)))):-
	     !, req_Positive([5|MmlVersion5],IsPositive).


%% ###TODO: change this system to the one used for assert_arith_evals

get_nr_fof(boole,0,fof(spc0_boole,theorem, sort(0, v1_xboole_0),
	       file(boole,spc0_boole),
	       [mptp_info(0,[],theorem,position(0,0),[0])])).
get_nr_fof(boole,N,Res):-
	integer(N), N > 0, concat_atom([spc,N,'_boole'],Name),
	Res= fof(Name,theorem, sort(N, ~(v1_xboole_0)),
		 file(boole,Name),[mptp_info(0,[],theorem,position(0,0),[0])]).

%% positive Element of omega; for 0, Element of omega is ensured by NUMERALS:1
%% ##TODO: it seems that this can become the redefined Element of NAT (widening to REAL)
%%         see iocorrel:Load_EnvConstructors
%% that may be needed for adding the 'complex' attribute, using
%% cluster   -> complex Element of REAL ;
%%
%% ##NOTE: the .zrt files will not be used; if the (non)zerotyp/2 predicates are
%%         needed, they will be created by looking at requirements directives of the article
%% The reason for not using custom (non)zerotyp (with upper cluster) is that the naming of
%% such formulas becomes either inconsistent (different flas with the same name), or
%% unstable (different name for the same fla).
%% Following test should fail; if it does not fail, we should fix the default type.
%% ##TEST: :- zerotyp(AR,K),AR \= numbers, constr2list(A, X, L,K),X\=m2_subset_1(k1_numbers, k5_numbers),theory(AR,T),member(requirements(R),T),member(numerals,R).

%% done below now
%% get_nr_fof(numerals,0,fof(spc0_numerals,theorem,sort(0,m2_subset_1(k1_numbers, k5_numbers)),
%% 			  file(numerals,spc0_numerals),
%% 			  [mptp_info(0,[],theorem,position(0,0),[0])])).

%% ##NOTE: we have to add the original type explicitly here, and also the supertype,
%%    because the redefinition subset_1.html#M2 requires nonemptiness of NAT,
%%    and e.e. in square_1 this is not known. Mizar uses the redefined version anyway,
%%    which seems to automatically make also the original version available.
%% ###TODO: have a closer look at this redefinition issue; it ptobably caused also
%%          the incomplete bg in one MPTP Challenge problem.
get_nr_fof(numerals,N,Res):-
	integer(N),
	mml_version(MmlVersion),
	spec_num_type(N,MmlVersion,NumType),
%%	N > 0,
	concat_atom([spc,N,'_numerals'],Name),
	Res= fof(Name,theorem,NumType,
		 file(numerals,Name),
		 [mptp_info(0,[],theorem,position(0,0),[0])]).

%% these requirements need to be hard-wired, because not all syms
%% are needed to fire; therefore they do not need to bee asserted
%% during the normal processing (but are now)

%% t6_boole: b1 is empty implies b1 = {}
hard_wired_req(boole,t6_boole,[v1_xboole_0]).
hard_wired_req(boole,t6_boole,[k1_xboole_0]).

%% t7_boole: not ( b1 in b2 & b2 is empty )
hard_wired_req(boole,t7_boole,[v1_xboole_0]).
hard_wired_req(boole,t7_boole,[r2_hidden]).

%% t1_subset: b1 in b2 implies b1 is Element of b2
hard_wired_req(subset,t1_subset,[r2_hidden]).

%% t2_subset: b1 is Element of b2 implies (b2 is empty or b1 in b2)
hard_wired_req(subset,t2_subset,[m1_subset_1,v1_xboole_0]).

%% t3_subset: b1 is Subset of b2 iff b1 c= b2
hard_wired_req(subset,t3_subset,[r1_tarski]).
hard_wired_req(subset,t3_subset,[m1_subset_1,k1_zfmisc_1]).

%% other requirements:
%% - 2 distinct numerals are not equal
%% - functor rqImaginaryUnit (k1_xcmplx_0) equals to i (what is i? - I'll ignore
%%   "complex numerals" in the first version probably
%% - +,*,-/1, 1/x, -/2,/ evaluation (rqRealAdd, rqRealMult, rqRealNeg, rqRealInv,
%%                 rqRealDiff, rqRealDiv -
%%         - k2_xcmplx_0, k3_xcmplx_0, k4_xcmplx_0, k5_xcmplx_0, k6_xcmplx_0, k7_xcmplx_0)
%%   special cases for 0 and 1 mentioned in arithm.miz


%%%%%%%%%%%%%%%%%%%% Problem creation for many articles %%%%%%%%%%%%%%%%%%%%

first100([
	  xboole_0,boole,xboole_1,enumset1,zfmisc_1,subset_1,subset,relat_1,
	  funct_1,grfunc_1,relat_2,ordinal1,wellord1,setfam_1,relset_1,partfun1,
	  mcart_1,wellord2,funct_2,funct_3,domain_1,binop_1,funcop_1,funct_4,
	  ordinal2,ordinal3,arytm_3,arytm_2,arytm_1,finset_1,finsub_1,setwiseo,
	  fraenkel,
	  numbers,arytm_0,numerals,xcmplx_0,arithm,xreal_0,real,xcmplx_1,
	  xreal_1,axioms,real_1,square_1,nat_1,int_1,rat_1,binop_2,membered,
	  complex1,absvalue,card_1,finseq_1,zf_lang,zf_model,zf_colla,orders_1,
	  eqrel_1,funct_5,card_2,trees_1,finseq_2,recdef_1,classes1,card_3,
	  classes2,ordinal4,finseq_3,zfmodel1,zf_lang1,zf_refle,zfrefle1,qc_lang1,
	  qc_lang2,qc_lang3,cqc_lang,pboole,seq_1,seq_2,prob_1,wellset1,seqm_3,
	  seq_4,real_2,margrel1,prob_2,rcomp_1,multop_1,mcart_2,mcart_3,mcart_4,
	  mcart_5,mcart_6,finseq_4,finseqop,finsop_1,setwop_2]).

%% articles in which at least one scheme is proved
scheme_articles([
		 abcmiz_0, afinsq_1, algseq_1, altcat_1, altcat_2,
		 ami_3, ami_4, armstrng, arytm_3, asympt_0, bhsp_4, binarith, binom,
		 binop_1, binop_2, bintree1, bintree2, birkhoff, borsuk_2, borsuk_6,
		 bvfunc_1, card_1, card_3, card_4, card_fil, cat_3, cat_5, catalg_1,
		 cfcont_1, chain_1, circcmb2, circcmb3, circcomb, circtrm1, clopban4,
		 closure1, clvect_3, cohsp_1, complsp1, comptrig, comput_1, comseq_1,
		 cqc_lang, cqc_sim1, dickson, domain_1, dtconstr, eqrel_1, facirc_1,
		 fdiff_2, fib_num2, fib_num, filter_1, finseq_1, finseq_2, finseq_5,
		 finset_1, fin_topo, fraenkel, frechet2, funct_1, funct_2, funct_3,
		 funct_5, funct_7, functor0, glib_000, goboard1, goboard2, graph_1,
		 graph_2, graph_5, grcat_1, group_4, group_5, hallmar1, heyting3,
		 hilbert2, index_1, instalg1, int_1, int_2, irrat_1, jct_misc,
		 jgraph_2, jgraph_3, jgraph_4, jordan1a, knaster, kurato_2, lattice3,
		 lattice5, lattice7, lfuzzy_0, lmod_7, lopban_4, margrel1, matrix_1,
		 measure1, measure5, membered, metric_3, midsp_3, modal_1, monoid_0,
		 monoid_1, msafree1, msaterm, mssubfam, msualg_6, msualg_8, msualg_9,
		 multop_1, nat_1, nat_2, nattra_1, necklace, orders_1, orders_3,
		 ordinal1, ordinal2, ordinal4, partfun1, partfun2, pboole, pcomps_1,
		 pcomps_2, pencil_2, pnproc_1, polynom2, polynom3, pre_circ, prob_4,
		 prvect_1, pscomp_1, pua2mss1, qc_lang1, qc_lang3, qc_lang4, quantal1,
		 rcomp_1, real_1, recdef_1, recdef_2, relat_1, relset_1, rewrite1,
		 rlvect_4, scheme1, schems_1, scmfsa6a, scmfsa_7, scmfsa9a, scmfsa_9,
		 scmpds_4, scmpds_8, scpinvar, scpisort, seq_1, seqfunc, setwiseo,
		 sgraph1, sin_cos, sppol_1, square_1, stirl2_1, sublemma, subset_1,
		 substut1, substut2, supinf_1, tarski, tex_2, toler_1, topgen_3,
		 topgen_4, topgen_5, topreal1, toprns_1, transgeo, trees_1, trees_2,
		 trees_4, trees_9, triang_1, uniroots, uproots, valuat_1, waybel_0,
		 waybel10, waybel11, waybel17, waybel19, waybel24, waybel_2, waybel30,
		 waybel31, waybel_4, waybel_6, wellfnd1, wellord2, wellset1, xboole_0,
		 yellow_0, yellow15, yellow16, yellow17, yellow18, yellow20, yellow21,
		 yellow_3, yellow_9, zf_lang1, zf_lang, zf_model, zfrefle1, zf_refle
		]).


nonnumeric(Article):-
	theory(Article,T),
	member(requirements(Req),T),
	subset(Req,[hidden, boole, subset]).

all_articles(List):-
	mml_dir(Dir),
	sformat(AList, '~s../mml.lar', [Dir]),
	open(AList,read,S),
	read_lines(S,List),
	close(S).

read_lines(S,Res):-
	read_line_to_codes(S,C),
	(C= end_of_file,Res=[];
	    read_lines(S,Res1),
	    string_to_atom(C,C1),
	    Res = [C1|Res1]).

read_file_lines(File, List):-
	open(File,read,S),
	read_lines(S,List),
	close(S),!.


%% Top level predicate for creating problems for
%% first 100 MML articles.
mk_first100:-
	declare_mptp_predicates,load_mml,first100(L),!,
	member(A,L),mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem]],[opt_REM_SCH_CONSTS]),fail.

%% Create problems for nonumeric articles (about 300)
mk_nonnumeric:-
	declare_mptp_predicates,load_mml,all_articles(L),!,
	member(A,L),nonnumeric(A),
	mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem]],[opt_REM_SCH_CONSTS]),fail.

%% Create scheme problems for nonumeric articles
mk_nonnumeric_schemes:-
	declare_mptp_predicates,load_mml,scheme_articles(L),!,
	member(A,L),nonnumeric(A),
	mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[scheme]],[opt_REM_SCH_CONSTS,opt_MK_TPTP_INF]),fail.

%% Create cluster problems for nonumeric articles
mk_nonnumeric_clusters:-
	declare_mptp_predicates,load_mml,all_articles(L),!,
	member(A,L),nonnumeric(A),
	mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[cluster]],[opt_REM_SCH_CONSTS,opt_MK_TPTP_INF]),fail.


%% for creating by-explanations, use this:
%% L=[xboole_0, boole, xboole_1, enumset1, zfmisc_1, subset_1, subset, relat_1, funct_1, grfunc_1, relat_2, ordinal1, wellord1],
%% member(A,L),mk_article_problems(A,[[mizar_by],[theorem, top_level_lemma, sublemma]],
%% [opt_REM_SCH_CONSTS,opt_MK_TPTP_INF,opt_LINE_COL_NMS]),fail.

%% print names of theorem problems in their order into SpecFile
%% now nonnumeric only;
%% TODO: check why t13_aff_1 was not created by SNoW
mk_ordered_th_problem_list(SpecFile):-
	declare_mptp_predicates,
	load_theorems,
	load_environs, % needed for nonnumeric/1
	install_index,
	all_articles(Articles),
	tell(SpecFile),
	(
	  member(A,Articles),
	  nonnumeric(A),
	  fof(Name,theorem,Fla,file(A,Name),_),
	  Fla \= $true,
	  format('~w/~w__~w~n',[A,A,Name]),
	  fail
	;
	  told
	).

%% Create problems from the recommendations given by SNoW
%% in SpecFile (it contains snow_spec(Conjecture, Refs) clauses.
%% Now limited to nonnumeric articles.
mk_nonnumeric_snow(SpecFile):-
	abolish(snow_spec/2), consult(SpecFile),
	declare_mptp_predicates,load_mml,all_articles(L),!,
	member(A,L),nonnumeric(A),
	mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem],snow_spec],[opt_REM_SCH_CONSTS]),fail.

%% Create (sub)problems whose names are in the list - should have option handling.
%% Names should have the form xboole_1__t40_xboole_1 (i.e.: Article__Problem)
mk_sub_problems_from_list(List):-
	declare_mptp_predicates,load_mml,
	findall([Article, Problem],
		( member(Name,List), concat_atom([Article,Problem], '__', Name)),
		Pairs), !,
	maplist(nth1(1),Pairs,Articles),
	sort(Articles, L),!,
	member(A,L),
	findall(P, member([A,P], Pairs), AList),
	mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],
			       [theorem, top_level_lemma, sublemma],
			       subproblem_list(AList)],
			    [opt_REM_SCH_CONSTS,opt_MK_TPTP_INF,opt_LEVEL_REF_INFO]),fail.

%% Create problems whose names are in the list.
%% Note that the fraenkels will not be uniformly abstracted when calling this
%% multiple times for different articles - use mk_problems_from_articlelist for that.
%% Names should have the form xboole_1__t40_xboole_1 (i.e.: Article__Problem),
%% or list of article positions of the form card_1__pos(980,19)
mk_problems_from_list(List,AddOptions):-
	declare_mptp_predicates,load_mml,
	findall([Article, Problem],
		( member(Name,List), concat_atom([Article,Problem], '__', Name)),
		Pairs), !,
	maplist(nth1(1),Pairs,Articles),
	sort(Articles, L),!,
	member(A,L),
	findall(P,
		(
		 member([A,P0], Pairs),
		 (
		  atom_concat(pos, _, P0) ->
		  term_to_atom(P,P0)
		 ;
		  P = P0
		 )
		),
		AList),
	union([opt_REM_SCH_CONSTS,opt_MK_TPTP_INF], AddOptions, Options),
	mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],
			       [theorem, top_level_lemma],
			       problem_list(AList)], Options),fail.

%% ##TEST: :- mk_problems_from_file('mptp_chall_problems').
mk_problems_from_file(File):-
	open(File,read,S),
	read_lines(S,List),
	close(S),!,
	mk_problems_from_list(List).

%% ##TEST: :- mk_problems_from_file('mptp_chall_problems',[opt_PROB_PRINT_FUNC(print_refs_as_one_fla)]).
%% ##TEST: :- mk_problems_from_file('rootsnonnum246',[opt_TPTP_SHORT]).
%% ##TEST: :- mk_problems_from_file('rootsnonnum246',[opt_TPTP_SHORT,opt_PROB_PRINT_FUNC(print_refs_as_tptp_includes)]).
%% ##TEST: :- read_file_lines('00includes_with_problem_articles',AllIncludes),mk_problems_from_file('rootsnonnum246',[opt_TPTP_SHORT,opt_PROB_PRINT_FUNC(print_refs_as_tptp_includes),opt_PP_SMALLER_INCLUDES(AllIncludes)]).
%% ##TEST: :- all_articles(AllIncludes),mk_problems_from_file('rootsnonnum246',[opt_TPTP_SHORT,opt_PROB_PRINT_FUNC(print_refs_as_tptp_includes),opt_PP_SMALLER_INCLUDES([hidden,tarski|AllIncludes])]).
mk_problems_from_file(File, AddOptions):-
	open(File,read,S),
	read_lines(S,List),
	close(S),!,
	mk_problems_from_list(List, AddOptions).


%% Use this for generating all problems for a set of articles with the
%% .allowed_local info for creating problems containing all previous
%% references.

%% ##TEST: :- declare_mptp_predicates,load_mml,install_index,all_articles(AA),checklist(abstract_fraenkels_if, AA),!,member(A,[tarski,xboole_0,xcmplx_0]),time(mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma, sublemma] ],[opt_LOAD_MIZ_ERRORS,opt_ARTICLE_AS_TPTP_AXS,opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_LINE_COL_NMS,opt_PRINT_PROB_PROGRESS,opt_ALLOWED_REF_INFO,opt_PROVED_BY_INFO])),fail.

%% These two can be used for generating normal and chainy problems for
%% all MML. The chainy version gives uses the whole theory of the
%% current article. A super-chainy version can be done by replacing
%% article's includes with the preceding articles in mml.lar .

%% ##TEST: :- declare_mptp_predicates,load_mml,install_index,all_articles(AA),checklist(abstract_fraenkels_if, AA),mml_dir(Dir), sformat(MmlLar, '~s../mml.lar', [Dir]), mk_problems_from_articlelist(MmlLar,[[mizar_by,mizar_from,mizar_proof], [theorem, top_level_lemma]], [opt_TPTP_SHORT,opt_PROB_PRINT_FUNC(print_refs_as_tptp_includes)]).
%% ##TEST: :- declare_mptp_predicates,load_mml,install_index,all_articles(AA),checklist(abstract_fraenkels_if, AA),mml_dir(Dir), sformat(MmlLar, '~s../mml.lar', [Dir]), mk_problems_from_articlelist(MmlLar,[[mizar_by,mizar_from,mizar_proof], [theorem, top_level_lemma]], [opt_TPTP_SHORT]).
mk_problems_from_articlelist(File, Kinds, AddOptions):-
        read_file_lines(File,AList),
	union([opt_REM_SCH_CONSTS,opt_MK_TPTP_INF], AddOptions, Options),!,
	member(A,AList),
	mk_article_problems(A,Kinds, Options),
	fail.


%% this takes time, better do the simple versions for all
%% ##TEST: :- test_refs_first100.
test_refs_first100:-
	declare_mptp_predicates,load_mml,first100(L),!,
	member(A,L),test_refs(A),fail.

%% ##TEST: :- test_refs_simple_first100.
test_refs_simple_first100:-
	declare_mptp_predicates,first100(L),!,
	member(A,L),test_refs_simple(A),fail.

%% ##TEST: :- test_refs_simple_all.
test_refs_simple_all:-
	declare_mptp_predicates,all_articles(L),!,
	member(A,L),test_refs_simple(A),fail.

%% print all theorems in standard TPTP, so that it can be cnf-ed by E
print_thms_for_cnf:-
	all_articles(L),
	declare_mptp_predicates,
	load_mml,
	install_index,
	(member(A2,L), abstract_fraenkels(A2, [], _, _),fail; true), !,
	install_index,
	tell('TheoremsInTPTP'),!,
	(
	  member(A1,L),
	  fof_file(A1,I),
	  clause(fof(A,theorem,B,C,[mptp_info(_,_,theorem,_,_)|_]),_,I),
	  sort_transform_top(B,B1),
	  numbervars(B1,0,_),
	  print(fof(A,axiom,B1,C)),write('.'),nl,fail;
	  told
	).

%% print defs and thms, with only top-level references
print_thms_and_defs_for_learning:-
	declare_mptp_predicates,
	load_theorems,
	install_index,
	tell('Proof_learning'),
	%% print definitions - they have no proof
	((fof(Name,definition,A1,A2,A3),
	  numbervars(A1,0,_),
	  print(fof(Name,definition,A1,A2,A3)),
	  write('.'),nl,
	  fail);
	    true),
	%% print theorems, with only thm and def references
	((fof(Name,theorem,A1,A2,[A3,inference(A4,A5,Refs)]),
	  findall(Ref,(member(Ref,Refs),atom_chars(Ref,[C|_]),
		       member(C,[t,d])), Refs1),
	  numbervars(A1,0,_),
	  print(fof(Name,theorem,A1,A2,[A3,inference(A4,A5,Refs1)])),
	  write('.'),nl,
	  fail);
	    told).

%% number all available fofs in their order in the prolog database
%% storing the numbering in fof_pcl_id (yes, this is used for the pcl protocol)
number_fofs:-
	abolish(fof_pcl_id/2),
	dynamic(fof_pcl_id/2),
	findall(NRef1,fof(NRef1,_,_,_,_),Refs),!,
	repeat,
	(
	  nth1(N,Refs,Ref),
	  assert(fof_pcl_id(Ref, N)),
	  fail
	;
	  true
	).


%% mptp2tptp(+InFile,+Options,+OutFile)
%%
%% Read InFile with theorems in MPTP syntax, translate it to
%% TPTP syntax in OutFile.
%% The files can be given both as atoms and strings.
%% This outputs only the required flas, no background is added.
%% Options is normally just [], unless local constants are present (e.g. in top-level lemmas).
%% In that case use opt_NO_FRAENKEL_CONST_GEN, otherwise abstract_fraenkels
%% breaks.
%% Option opt_PRUNE_TREE_FOR_FILE(+File) causes to include all fofs coming from File,
%% and only those other fofs which are needed for the inference parents of those fofs.
%% This can be used to make the OutFile's size significantly smaller for apps like AgInt.
%% Option opt_PCL prints the inference infos in the PCL format, and therefore
%% also numbers the formulas, assuming that in the input file, parents come before children.
%%
%% ##TEST: :- mptp2tptp("matrix11.agin",[opt_PCL,opt_PRUNE_TREE_FOR_FILE(matrix11),opt_NO_FRAENKEL_CONST_GEN],"matrix11.agin13").
%% ##TEST: :- mptp2tptp("matrix11.agin",[opt_PRUNE_TREE_FOR_FILE(matrix11),opt_NO_FRAENKEL_CONST_GEN],"matrix11.agin13").
mptp2tptp(InFile,Options,OutFile):-
	(atom(InFile) ->
	    InFile1 = InFile
	;
	    string_to_atom(InFile, InFile1)
	),
	(atom(OutFile) ->
	    OutFile1 = OutFile
	;
	    string_to_atom(OutFile, OutFile1)
	),
	declare_mptp_predicates,
	consult(InFile1),
	(member(opt_PCL, Options) ->
	    number_fofs
	;
	    true
	),
	install_index,
	%% find all the article names for our flas -
	%% abstract_fraenkels/4 requires it now
	findall(AName,fof_file(AName,_),ANames1),
	sort(ANames1, ArticleNames),
	findall(FraenkelName,
		(
		  member(Article, ArticleNames),
		  once(abstract_fraenkels(Article, Options, _, ArticleFrNames)),
		  member(FraenkelName, ArticleFrNames)
		),
		AddedFraenkelNames), !,
	install_index,!,

	RefCodes = [t,d,s,l],
	(member(opt_PRUNE_TREE_FOR_FILE(PFile), Options) ->
	    findall(ARef2,
		    (
		      fof_file(PFile,AId),
		      clause(fof(ARef1,_,_,file(_,_),AInfo),_,AId),
		      AInfo = [mptp_info(_,_,_,_,_), inference(_,_,ARefs0) |_],
		      member(ARef0,[ARef1|ARefs0]),
		      atom_chars(ARef0,[C1|Cs1]),
		      member(C1,RefCodes),
		      fix_sch_ref(ARef0,[C1|Cs1],ARef2)
		    ),
		    PrintedNames1),
	    sort(PrintedNames1, PrintedNames)
	;
	    PrintedNames = []  %% just a suitable value
	),

	tell(OutFile1),
	(
	  fof(Name,Role,Fla,file(A,Sec),Info),
	  (
	    PrintedNames \= [],
	    member(Name,PrintedNames)
	  ;
	    PrintedNames = []
	  ),
	  not(member(Name, AddedFraenkelNames)),
	  (Info = [mptp_info(_,_,_,_,_), inference(_,_,Refs) | _] ->
	      findall(Ref,(member(Ref0,Refs),atom_chars(Ref0,[C|Cs]),
			   member(C,RefCodes),fix_sch_ref(Ref0,[C|Cs],Ref)), Refs2),
	      sort_transform_top(Fla,Fla1),
	      numbervars(Fla1,0,_),
	      %% ###TODO: remove this when Geoff allows inferences without parents
	      (Refs2 = [] ->
		  (member(opt_PCL, Options) ->
		      fof_pcl_id(Name,NId),
		      write(NId), write(' : : '),
		      print(Fla1), write(' : '),
		      string_to_atom(A1,A),
		      print(initial(A1,Name))
		  ;
		      print(fof(Name,theorem,Fla1,file(A,Sec))),
		      write('.')
		  )
	      ;
		  (member(opt_PCL, Options) ->
		      fof_pcl_id(Name,NId),
		      write(NId), write(' : : '),
		      print(Fla1), write(' : '),
		      maplist(fof_pcl_id,Refs2,Ids2),
		      Info1 =.. [foreign_gen|Ids2],
		      print(Info1), write(' : '), print(Name)

		  ;
		      Info1 = inference(mizar_proof,[status(thm)],Refs2),
		      print(fof(Name,theorem,Fla1,Info1,[file(A,Sec)])),
		      write('.')
		  )
	      )
	  ;
	      sort_transform_top(Fla,Fla1),
	      numbervars(Fla1,0,_),
	      (member(opt_PCL, Options) ->
		  fof_pcl_id(Name,NId),
		  write(NId), write(' : : '),
		  print(Fla1), write(' : '),
		  string_to_atom(A1,A),
		  print(initial(A1,Name))
	      ;
		  print(fof(Name,Role,Fla1,file(A,Sec))),
		  write('.')
	      )
	  ),
	  nl,
	  fail
	;
	  told
	).

%% thms2tptp(+OutDirectory)
%%
%% translate all mml theorems and top-level lemmas to TPTP,
%% each into file Article.(lem|the|sch)3 in OutDirectory
%% ##TEST: :- thms2tptp('/home/urban/mptp0.2/00tmp3/').
thms2tptp(OutDirectory):-
	mml_dir_atom(MMLDir),
	all_articles(List),
	mml_added_spc(Spc),
	append(Spc,List,NList),
	member(A,NList),
	member([Kind|Options],[[lem, opt_NO_FRAENKEL_CONST_GEN],[the],[sch]]),
	concat_atom([MMLDir, A, '.', Kind, '2'], InFile),
	concat_atom([OutDirectory, A, '.', Kind, '3'], OutFile),
	once(mptp2tptp(InFile,Options,OutFile)),
	fail.

%% mml2tptp(+OutDirectory)
%%
%% translate all of mml (theorems, top-level lemmas, constructor info and clusters)
%% each into file Article.(lem|the|dco|dcl)3 in OutDirectory
%% ##TEST: :- mml2tptp('/home/urban/mptp0.2/00tmp4/').
mml2tptp(OutDirectory):-
	mml_dir_atom(MMLDir),
	all_articles(List),
	mml_added_spc(Spc),
	append(Spc,List,NList),
	member(A,[hidden|NList]),
	member([Kind|Options],[[lem, opt_NO_FRAENKEL_CONST_GEN],[the],[dco],[dcl]]),
	concat_atom([MMLDir, A, '.', Kind, '2'], InFile),
	exists_file(InFile),
	concat_atom([OutDirectory, A, '.', Kind, '3'], OutFile),
	once(mptp2tptp(InFile,Options,OutFile)),
	fail.

%% article2tptp_include(+OutDirectory, +A)
%%
%% Generate include file ( .ax) in TPTP format from MML article A,
%% and place it into OutDirectory.
article2tptp_include(OutDirectory,A):-
	concat_atom([OutDirectory, A, '.', ax], OutFile),
	tell(OutFile),
	repeat,
	(
	  fof_file(A,AId),
	  clause(fof(Name,_Role,Fla,file(A,_Sec),_Info),_,AId),
	  sort_transform_top(Fla,Fla1),
	  numbervars(Fla1,0,_),
	  print(fof(Name,axiom,Fla1)),
	  write('.'),nl,
	  fail
	;
	  told
	).

%% Generate include files in TPTP format from each MML article.
%% Fraenkels are generated, but scheme instances and special
%% on-demand requirement formulas are not included (they rather
%% belong to particular problems). Lemmas are not included.
%% ##TEST: :- mml2tptp_includes('00tmp6/').
mml2tptp_includes(OutDirectory):-
	declare_mptp_predicates,
	load_mml,
	install_index,
	all_articles(ArticleNames),
	checklist(abstract_fraenkels_if, ArticleNames),
	install_index,
	mml_added_spc(Spc),
	append(Spc,ArticleNames,NList),
	checklist(article2tptp_include(OutDirectory), [hidden | NList]).

%% For all articles create the inference dag of its top-level theorems
%% (and lemmas and schemes), including needed stuff from other articles
%% as axioms. No background is added, each article is written to
%% problems/Article.top, and is suitable for running AGInTRater on it.
%% ##TEST: :- create_top_infers.
create_top_infers:-
	declare_mptp_predicates,
	load_mml,
	load_lemmas,
	all_articles(ArticleNames),
	install_index,
	findall(FraenkelName,
		(
		  member(Article, ArticleNames),
		  once(abstract_fraenkels(Article, [opt_NO_FRAENKEL_CONST_GEN], _, ArticleFrNames)),
		  member(FraenkelName, ArticleFrNames)
		),
		_AddedFraenkelNames), !,
	install_index,!,

	RefCodes = [t,d,s,l],
	member(PFile, ArticleNames),
	findall(ARef2,
		(
		  fof_file(PFile,AId),
		  clause(fof(ARef1,_,_,file(_,_),AInfo),_,AId),
		  AInfo = [mptp_info(_,_,_,_,_), inference(_,_,ARefs0) |_],
		  member(ARef0,[ARef1|ARefs0]),
		  atom_chars(ARef0,[C1|Cs1]),
		  member(C1,RefCodes),
		  fix_sch_ref(ARef0,[C1|Cs1],ARef2)
		),
		PrintedNames1),
	sort(PrintedNames1, PrintedNames),
	concat_atom([problems,'/',PFile,'.top'], OutFile1),
	tell(OutFile1),
	(

	  member(Name,PrintedNames),
	  get_ref_fof(Name, fof(Name,Role,Fla,file(A,Name),Info)),
	  ((A = PFile, Info = [mptp_info(_,_,_,_,_), inference(_,_,Refs) | _]) ->
	      findall(Ref,(member(Ref0,Refs),atom_chars(Ref0,[C|Cs]),
			   member(C,RefCodes),fix_sch_ref(Ref0,[C|Cs],Ref)), Refs2),
	      sort_transform_top(Fla,Fla1),
	      numbervars(Fla1,0,_),
	      %% ###TODO: remove this when Geoff allows inferences without parents
	      (Refs2 = [] ->
		  print(fof(Name,theorem,Fla1,file(A,Name))),
		  write('.')
	      ;
		  Info1 = inference(mizar_proof,[status(thm)],Refs2),
		  print(fof(Name,theorem,Fla1,Info1,[file(A,Name)])),
		  write('.')
	      )
	  ;
	      sort_transform_top(Fla,Fla1),
	      numbervars(Fla1,0,_),
	      print(fof(Name,Role,Fla1,file(A,Name))),
	      write('.')
	  ),
	  nl,
	  fail
	;
	  told
	),
	fail.



%% reads ProvedFile and prints comparison of the proofs there
%% with the MML proofs; sorted by the difference between the
%% numbers of explicit references
compare_proved_by_refsnr(ProvedFile):-
	declare_mptp_predicates,
	load_mml,
	install_index,
	consult(ProvedFile),
	findall([Diff,R1,Refs0,Refs1,BG],
		(
		  proved(R1,_F,Refs0,BG,_I),
		  get_ref_fof(R1,fof(R1,_,_,_,[_,inference(_,_,I3)|_])),
		  findall(Ref, (member(Ref,I3), atom_chars(Ref,[C|_]),
				   member(C,[t,d])),
			  Refs11),
		  sort(Refs11,Refs1),
		  length(Refs0,N1), length(Refs1,N2),
		  Diff is N1 - N2),
		Tuples),
	sort(Tuples,T1),
	checklist(print_nl,T1).

%% fix_sch_ref(+SchemeRefIn,+SchChars,-SchemeRefOut)
%%
%% If SchChars contain '__', then they are scheme instance like s1_ordinal1__e8_6__wellord2.
%% In that case return only the part before '__', i.e. s1_ordinal1.
%% Otherwise return SchemeRefIn.
fix_sch_ref(Ref0,[s|Cs],Ref1):- !,
	%% find the part before "__"
	(append(S1,['_','_'|_],[s|Cs]) ->
	    atom_chars(Ref1,S1);
	    Ref1 = Ref0
	).
fix_sch_ref(Ref0,_,Ref0).

%% get_rec_uses - not for defs and clusters yet, just theorems, schemes and top-level lemmas
get_rec_uses(Name,_,NN,LRec1,LRecNN1,CL1,CLNN1,Last1,LastNN1,CLast1,CLastNN1,AllRecTmpS1,AllRecTmpNNS1):-
	atom(Name),
	th_rec_uses(Name,NN,LRec1,LRecNN1,CL1,CLNN1,Last1,LastNN1,CLast1,CLastNN1,AllRecTmpS1,AllRecTmpNNS1),!.

get_rec_uses(Name,RefCodes,NN,LRec1,LRecNN1,CL1,CLNN1,Last1,LastNN1,CLast1,CLastNN1,AllRecTmpS1,AllRecTmpNNS1):-
	fof(Name,Role,_,file(A,Name),Info),!,
	(nonnumeric(A) -> NN = t; NN = f),
	Info = [mptp_info(_,_,_,_,_), inference(_,_,Refs) |_],
	once((
	  findall(Ref,(member(Ref0,Refs),atom_chars(Ref0,[C|Cs]),
		       member(C,RefCodes),fix_sch_ref(Ref0,[C|Cs],Ref)), Refs1),

	  %% find recursive dependencies
	  findall(Reff1,
		  (member(Ref1,Refs1),get_rec_uses(Ref1,RefCodes,_,_,_,_,_,_,_,_,_,RefsAll1,_),
		      member(Reff1,[Ref1|RefsAll1])),
		  AllRecTmp1),
	  sort(AllRecTmp1,AllRecTmpS1),

	  %% find largest direct dependence (or f is none)
	  findall([L_Dref1,DRef1],(member(DRef1,Refs1),get_rec_uses(DRef1,RefCodes,_,L_Dref1,_,_,_,_,_,_,_,_,_)),Lengths1),
	  sort(Lengths1,SLengths1),
	  (last(SLengths1,Last1) -> true; Last1 = f),

	  %% find longest chain dependence (or f is none), update my chain length
	  findall([LC_Dref1,C_DRef1],(member(C_DRef1,Refs1),get_rec_uses(C_DRef1,RefCodes,_,_,_,LC_Dref1,_,_,_,_,_,_,_)),C_Lengths1),
	  sort(C_Lengths1,SCLengths1),
	  (last(SCLengths1,CLast1) -> (CLast1 = [CLL1,_], CL1 is CLL1 + 1); (CLast1 = f, CL1 = 1)),

	  %% find recursive nonnumeric dependencies
	  findall(ReffNN1,
		  (member(RefNN1,Refs1),get_rec_uses(RefNN1,RefCodes,t,_,_,_,_,_,_,_,_,_,RefsAllNN1),
		      member(ReffNN1,[RefNN1|RefsAllNN1])),
		  AllRecTmpNN1),
	  sort(AllRecTmpNN1,AllRecTmpNNS1),

	  %% find largest direct nonnumeric dependence (or f is none)
	  findall([L_DRefNN1,DRefNN1],(member(DRefNN1,Refs1),get_rec_uses(DRefNN1,RefCodes,_,_,L_DRefNN1,_,_,_,_,_,_,_,_)),LengthsNN1),
	  sort(LengthsNN1,SLengthsNN1),
	  (last(SLengthsNN1,LastNN1) -> true; LastNN1 = f),

	  %% find longest nonnumeric chain dependence (or f is none), update my nonnumeric chain length
	  findall([LC_DRefNN1,C_DRefNN1],(member(C_DRefNN1,Refs1),get_rec_uses(C_DRefNN1,RefCodes,t,_,_,_,LC_DRefNN1,_,_,_,_,_,_)),C_LengthsNN1),
	  sort(C_LengthsNN1,SCLengthsNN1),
	  (last(SCLengthsNN1,CLastNN1) -> CLastNN1 = [CLLNNTmp1,_];  (CLastNN1 = f, CLLNNTmp1 = 0)),
	  (NN = t -> CLNN1 is CLLNNTmp1 + 1; CLNN1 = CLLNNTmp1),

		true,
	  maplist(length,[AllRecTmpS1,AllRecTmpNNS1],[LRec1,LRecNN1]),
	  assert(th_rec_uses(Name,NN,LRec1,LRecNN1,CL1,CLNN1,Last1,LastNN1,CLast1,CLastNN1,AllRecTmpS1,AllRecTmpNNS1))
	     )), !.




%% This is used for selecting problems for MPTP challenege:
%% find recursive dependencies of theorems, theorems chains, and all also
%% nonnumerical; print it on-the-fly, because for all articles it now crashes
%% on jgraph_6; results are assrted into th_rec_uses/12
%% th_rec_uses(Theorem,Nonnumerical,LengthAllRec,LengthAllRecNN,AllChainLength,
%%             AllNNChainLength,LargestParent,LargestParentNN,LongestParent,
%%             LongestParentNN,AllRecUses,AllRecNonNumUses)
do_th_stats(File):-
	do_th_stats(File,[]),!.

do_th_stats(File,Options):-
	declare_mptp_predicates,
	load_theorems,
	load_environs,
	(member(o_ths_TL_LEMMAS, Options) -> (RefCodes1= [l], load_lemmas); RefCodes1=[]),
	(member(o_ths_SCHEMES, Options) -> (RefCodes=[t,s|RefCodes1], load_schemes); RefCodes=[t|RefCodes1]),
	install_index,
	all_articles(Articles),
	mml_added_spc(Spc),
	append(Spc,Articles,NList),
%	Articles = [xboole_0,boole,xboole_1,enumset1],
%	Articles = [jgraph_6],
%	first100(Articles),
	abolish(th_rec_uses/12),
	abolish(th_rec_usedby/12),
	dynamic(th_rec_uses/12),
	dynamic(th_rec_usedby/12),!,
%	tell(TrainFile),
	%% this no longer assumes mml order of Articles
	(
	  member(A,NList),		write(A),nl,
%	  (nonnumeric(A) -> NN = t; NN = f),
	  fof(Name,theorem,_,file(A,Name),Info),
	  get_rec_uses(Name,RefCodes,_NN,_LRec1,_LRecNN1,_CL1,_CLNN1,_Last1,_LastNN1,_CLast1,_CLastNN1,_AllRecTmpS1,_AllRecTmpNNS1),
	  fail
	;
	  true
	).

%% expand_sch_refs(+InRefs, +RefCodes, -OutRefs)
%%
%% All scheme references in InRefs are recursively expanded.
%% The result is sorted into OutRefs. Only references starting with
%% RefCodes are used for recursive expansion.
expand_sch_refs([], _, []).

expand_sch_refs([Ref|Refs], RefCodes, OutRefs):-
	atom_chars(Ref,[C|_]),
	( C = s ->
	    fof(Ref,_,_,file(_,Ref),Info),
	    Info = [mptp_info(_,_,_,_,_), inference(_,_,Refs1) |_],
	    findall(Ref1,(member(Ref0,Refs1),atom_chars(Ref0,[C0|Cs0]),
			  member(C0,RefCodes),fix_sch_ref(Ref0,[C0|Cs0],Ref1)), Refs2),
	    expand_sch_refs(Refs2, RefCodes, OutRefs2),
	    expand_sch_refs(Refs, RefCodes, OutRefs1),
	    append(OutRefs2, OutRefs1, OutRefs)
	;
	    expand_sch_refs(Refs, RefCodes, OutRefs1),
	    ( member(C,RefCodes) ->
		OutRefs = [Ref|OutRefs1]
	    ;
		OutRefs = OutRefs1
	    )
	).

%% prints the nonumerical chain (actually tree) ending with LastTh,
%% needs to run do_th_stats/1 first
%% Option o_ths_EXPAND_SCHREFS causes to recursively replace all references to schemes
%% with their references.
%% the graph for mptp challenge was generated this way:
%% ##TEST:
%% :- do_th_stats(_,[o_ths_SCHEMES,o_ths_TL_LEMMAS]).
%% :- print_nn_chain(l37_yellow19,[o_ths_SCHEMES,o_ths_TL_LEMMAS,o_ths_EXPAND_SCHREFS]).
print_nn_chain(LastTh):- print_nn_chain(LastTh,[]).
print_nn_chain(LastTh,Options):-
	all_articles(L),
	declare_mptp_predicates,
	load_mml,
	install_index,!,
	%% abstracting will probably fail if lemmas are loaded without top-level constant types!
	%% therefore the lemmas are loaded after abstracting;
	%% if some fraenkel is in them, bad luck (and manual work now)
	(member(A2,L), abstract_fraenkels(A2, [], _, _),fail; true), !,
	(member(o_ths_TL_LEMMAS, Options) -> (RefCodes2= [l], load_lemmas); RefCodes2=[]),
	(member(o_ths_DEFS, Options) -> RefCodes1= [d|RefCodes2]; RefCodes1=RefCodes2),
	(member(o_ths_SCHEMES, Options) -> RefCodes=[t,s|RefCodes1]; RefCodes=[t|RefCodes1]),
	install_index,!,
	th_rec_uses(LastTh,_,_,_,_,_,_,_,_,_,U,_),
	%% definitions are not in th_rec_uses now, so find them
	findall(DefRef,
		(
		  member(Name,[LastTh|U]),
		  fof(Name,_,_,_,Info),
		  Info = [mptp_info(_,_,_,_,_), inference(_,_,Refs) |_],
		  member(DefRef,Refs),atom_chars(DefRef,[d|_])
		),
		DefRefs1),!,
	sort(DefRefs1, DefRefs),!,
	(
	  member(Name,[LastTh|U]),
	  fof(Name,_,Fla,file(A,Name),Info),
	  Info = [mptp_info(_,_,_,_,_), inference(_,_,Refs) |_],
	  findall(Ref,(member(Ref0,Refs),atom_chars(Ref0,[C|Cs]),
		       member(C,RefCodes),fix_sch_ref(Ref0,[C|Cs],Ref)), Refs1),
	  (member(o_ths_EXPAND_SCHREFS, Options) ->
	      expand_sch_refs(Refs1, RefCodes, Refs2)
	  ;
	      Refs2 = Refs1
	  ),
	  Info1 = inference(mizar_proof,[status(thm)],Refs2),
	  sort_transform_top(Fla,Fla1),
	  numbervars(Fla1,0,_),
	  print(fof(Name,theorem,Fla1,Info1,[file(A,Name)])),
	  write('.'),nl,
	  fail
	;
	  member(Name,DefRefs),
	  fof(Name,_,Fla,file(A,Name),_),
	  sort_transform_top(Fla,Fla1),
	  numbervars(Fla1,0,_),
	  print(fof(Name,definition,Fla1,file(A,Name))),
	  write('.'),nl,
	  fail
	).


cmp_in_mml_order(Delta, Name1, Name2):-
	atom(Name1),
	atom(Name2),
	get_ref_fof(Name1, fof(Name1,_,_,file(Article1,_),
			       [mptp_info(ItemNr1, _, ItemKind1, position(Line1,Col1), _)|_])),
	get_ref_fof(Name2, fof(Name2,_,_,file(Article2,_),
			       [mptp_info(ItemNr2, _, ItemKind2, position(Line2,Col2), _)|_])),
	article_nr(Article1,ANr1),
	article_nr(Article2,ANr2),!,
	compare(ADelta, ANr1, ANr2),
	(ADelta == '=' ->
	    (ItemKind1 == ItemKind2 ->
		compare(Delta, ItemNr1, ItemNr2)
	    ;
		%% different kinds - have to compare by position
		compare(Delta, [Line1,Col1], [Line2,Col2])
	    )
	;
	    Delta = ADelta
	).

%% debugging
cmp_in_mml_order(_,Name1,Name2):- throw(cmp_in_mml_order(Name1,Name2)).


%% for articles only
cmp_articles_in_mml_order(Delta, Article1, Article2):-
	atom(Article1),
	atom(Article2),
	article_nr(Article1,ANr1),
	article_nr(Article2,ANr2),!,
	compare(Delta, ANr1, ANr2).

%% debugging
cmp_articles_in_mml_order(_,Name1,Name2):- throw(cmp_articles_in_mml_order(Name1,Name2)).


%% number MML articles if needed
%% takes a list of added articles (not in mml), that will be appended in their order
%% to mml articles
number_articles(Added):-  articles_numbered(Added),!.
number_articles(Added):-
	all_articles(L1),
	mml_added_spc(Spc),
	append(Spc,L1,NList),	
	append([hidden|NList], Added, L),
	abolish(article_nr/2),
	dynamic(article_nr/2),!,
	findall(foo, ( nth1(N,L,A), assert(article_nr(A,N))), _),
	assert(articles_numbered(Added)).



%% sort fof names, using the article ordering, item numbers, and  positions
sort_in_mml_order(U,U1):-
	number_articles([]),
	predsort(cmp_in_mml_order,U,U1).

%% sort_articles_in_mml_order(+ArticlesIn, +AddedNonMML, -ArticlesOut)
%%
%% Sort ArticlesIn using the article ordering in mml.lar .
%% AddedNonMML (usually empty list) can be used to pass extra nonMML
%% articles that will be appended to MML in that order for numbering.
sort_articles_in_mml_order(ArticlesIn, AddedNonMML, ArticlesOut):-
	number_articles(AddedNonMML),
	predsort(cmp_articles_in_mml_order,ArticlesIn,ArticlesOut).


%% prints the graph in a .dot format
print_nn_chain_for_dot(LastTh):- print_nn_chain_for_dot(LastTh,[]).
print_nn_chain_for_dot(LastTh,Options):-
	declare_mptp_predicates,
	load_mml,
	(member(o_ths_TL_LEMMAS, Options) -> (RefCodes2= [l], load_lemmas); RefCodes2=[]),
	(member(o_ths_DEFS, Options) -> RefCodes1= [d|RefCodes2]; RefCodes1=RefCodes2),
	(member(o_ths_SCHEMES, Options) -> RefCodes=[t,s|RefCodes1]; RefCodes=[t|RefCodes1]),
	install_index,
	format("strict digraph ~w {",[LastTh]), !,
	th_rec_uses(LastTh,_,_,_,_,_,_,_,_,_,U,_),
	sort_in_mml_order([LastTh|U],U1), !,
	nth1(Nr,U1, Name),
% 	(
% 	  Nr = 1
% 	;

% 	  Nr > 1,
% 	  PrevNr is Nr - 1,
% 	  nth1(PrevNr,U1, PrevName),
% 	  write(PrevName),format(" -> "),write(Name),format("[color = red];"),nl
% 	),
	fof(Name,_,_,file(_,Name),Info),
	Info = [mptp_info(_,_,_,_,_), inference(_,_,Refs) |_],
	findall(Ref,(member(Ref0,Refs),atom_chars(Ref0,[C|Cs]),
		     member(C,RefCodes),fix_sch_ref(Ref0,[C|Cs],Ref)), Refs1),
	once(findall(d,(member(R1,Refs1),write(R1),format(" -> "),write(Name),format(";"),nl),_)),
	fail.

%% select only references from article
filter_article_refs(Article,RefsIn,RefsOut):-
	findall(Ref,
		(
		  member(Ref, RefsIn),
		  fof_name(Ref, Id),
		  clause(fof(Ref,_,_,file(Article,_),_),_,Id)
		),
		RefsOut
	       ).

%% find article root theorems (i.e. those not referenced by other theorems in the article)
find_article_roots(A,Roots):-
	findall([Ref,Refs1],
		(
		  fof_file(A,I),
		  clause(fof(Ref,theorem,_,file(A,_),[mptp_info(_,_,_,_,_), inference(_,_,[R1|Refs])| _]),_,I),
		  filter_article_refs(A,[R1|Refs],Refs1)
		),
		Pairs
	       ),
	zip(Ths, Refs2, Pairs),
	flatten(Refs2, Refs3),
	sort(Refs3, Refs4),
	subtract(Ths, Refs4, Roots).

%% ###TODO: pepin.lem2 is broken because of very large integer evaluations there
%% Print the nr. of roots of articles asserted in predicate good/1, print the roots
%% of each such article, and count how many roots are in all good articles
%% ##TEST: :- assert(good(aff_3)),declare_mptp_predicates,load_mml,install_index,
%%            findall(N,(good(X),find_article_roots(X,Roots),length(Roots,N),print(roots(X,N)),nl,print(Roots),nl),L),
%%            sumlist(L,M),length(L,L1),print(L).

%%%%%%%%%%%%%%%%%%%% Create initial proved_by table for malarea  %%%%%%%%%%%%%%%%%%%%

%% print thms and top lemmas with top-level references, and including itself;
%% all other top-level guys (defs, clusters, types) will be taken care of by malarea
mk_proved_by_for_malarea(File):-
	declare_mptp_predicates,
	all_articles(Articles),
	mml_added_spc(Spc),
	append(Spc,Articles,NList),
	load_theorems,
	load_lemmas,
	tell(File),
	%% ###TODO: this might be a bit fragile
	(
	  member(A,NList),
	  fof(Name,Kind,_Fla,file(A,Name),Info),
	  member(Kind,[theorem,lemma_conjecture]),
	  Info = [mptp_info(_,_,_,_,_), inference(_,_,Refs) |_],
	  Refs = [_ | _],
	  findall(Ref,(member(Ref,Refs),atom_chars(Ref,[C|_]),
		       member(C,[t,d,s,l])), Refs1),
	  write(proved_by(Name,[Name | Refs1])),
	  write('.'), nl,
	  fail
	;
	  told
	).

%%%%%%%%%%%%%%%%%%% Get the used_by info %%%%%%%%%%%%%%%%%%%%
create_used_by(_Options):-
	declare_mptp_predicates,
	load_theorems,
	install_index,
	all_articles(Articles),
	mml_added_spc(Spc),
	append(Spc,Articles,NList),
	abolish(used_by/2),
	dynamic(used_by/2),
	(
	 member(A,NList),
	 fof(Name,Kind,_,file(A,Name),Info),
	 Kind = theorem,
	 Info = [mptp_info(_,[],theorem,_,_), inference(_,_,Refs) |_],
	 findall(Ref,(member(Ref,Refs),atom_chars(Ref,[C|_]),
		     member(C,[t,d]), assert(used_by(Ref,Name))), _),
	 fail
	;
	 true
	).

% should be preceded by create_used_by
print_used_by(File, _Options):-
	all_articles(Articles),
	mml_added_spc(Spc),
	append(Spc,Articles,NList),
	tell(File),
	(
	 member(A,NList),
	 fof(Name,_,_,file(A,Name),_),
	 findall(Ref, used_by(Name, Ref), Refs),
	 concat_atom(Refs,',',ToPrint),
	 write(Name), write(':'), write(ToPrint), nl,
	 fail
	;
	 told
	).


%%%%%%%%%%%%%%%%%%%% Create training data for SNoW %%%%%%%%%%%%%%%%%%%%


%% symbol and reference numbering for snow
%% symbols start at 100000
%% old code that numbers from 1
get_snow_refnr_old(Ref,Nr):- snow_refnr(Ref,Nr),!.
get_snow_refnr_old(Ref,Nr):-
	flag(snow_refnr,N,N+1), Nr is N+1,
	assert(snow_refnr(Ref,Nr)),!.

get_snow_symnr_old(Ref,Nr):- snow_symnr(Ref,Nr),!.
get_snow_symnr_old(Ref,Nr):- flag(snow_symnr,N,N+1), Nr is N+1,
	assert(snow_symnr(Ref,Nr)),!.

%% new code that numbers from 0
get_snow_refnr(Ref,Nr):- snow_refnr(Ref,Nr),!.
get_snow_refnr(Ref,Nr):-
	flag(snow_refnr,Nr,Nr+1),
	assert(snow_refnr(Ref,Nr)),!.

get_snow_symnr(Ref,Nr):- snow_symnr(Ref,Nr),!.
get_snow_symnr(Ref,Nr):-
	flag(snow_symnr,Nr,Nr+1),
	assert(snow_symnr(Ref,Nr)),!.




%% print defs and thms, with only top-level references
%% now prints in the article order, and also respects
%% the order of defs and theorems in the article -
%% needed for incremental learning
%% ##TEST: :- mk_snow_input_for_learning(snow1,[]).
%%
%% With the following options the formula is printed as a graph, and with symbol
%% numbering starting at 0 (instead of reference numbering):
%% ##TEST: :- mk_snow_input_for_learning(snow3,[opt_LEARN_EDGE, opt_LEARN_SYMS_SMALL]).
%%
%% Print the refnr and symnr files without the prolog notation
%% ##TEST: :- mk_snow_input_for_learning(snow3,[opt_LEARN_STDFILES]).

mk_snow_input_for_learning(File,Options):-
	declare_mptp_predicates,
	load_theorems,
	install_index,
	all_articles(Articles),
	mml_added_spc(Spc),
	append(Spc,Articles,NList),
	abolish(snow_symnr/2),
	abolish(snow_refnr/2),
	dynamic(snow_symnr/2),
	dynamic(snow_refnr/2),
	(
	 member(opt_LEARN_SYMS_SMALL, Options) ->
	 flag(snow_refnr,_,100000),
	 flag(snow_symnr,_,0)
	;
	 flag(snow_refnr,_,0),
	 flag(snow_symnr,_,100000)
	),
	logic_syms(LogicSyms),
	concat_atom([File,'.train'], TrainFile),
	concat_atom([File,'.refnr'], RefFile),
	concat_atom([File,'.symnr'], SymFile),
	tell(TrainFile),
	%% print definitions - they have no proof
	%% ###TODO: this might be a bit fragile
	(
	  member(A,NList),
	  fof(Name,Kind,Fla,file(A,Name),Info),
	  (
	    Kind = definition,
	    Refs1 = []
	  ;
	    Kind = theorem,
	    Info = [mptp_info(_,[],theorem,_,_), inference(_,_,Refs) |_],
	    findall(Ref,(member(Ref,Refs),atom_chars(Ref,[C|_]),
			 member(C,[t,d])), Refs1)
	  ),
	 (
	  member(opt_LEARN_EDGE,Options) ->
	  sort_transform_top(Fla,Fla1),
	  numbervars(Fla1,0,_),
	  term2list(Fla1,LL1),
	  flatten(LL1,LL2),
	  length(LL2,Length1),
	  write(Length1),
	  write(';'),
	  print_as_numbered_tree(Fla1),
	  maplist(get_snow_refnr,[Name|Refs1],RefNrs),
	  concat_atom(RefNrs,',',ToPrint),
	  write(ToPrint), write(':'), nl
	 ;
	  collect_symbols_top(Fla, AllSyms),
	  subtract(AllSyms,LogicSyms,Syms),
	  Syms = [_|_],
	  maplist(get_snow_symnr,Syms,SymNrs),
	  maplist(get_snow_refnr,[Name|Refs1],RefNrs),
	  append(SymNrs,RefNrs,AllNrs),
	  concat_atom(AllNrs,',',ToPrint),
	  write(ToPrint), write(':'), nl
	 ),
	 fail
	;
	  told
	),
	tell(SymFile),
	(
	 member(opt_LEARN_STDFILES,Options) ->
	 (
	  snow_symnr(SymName,_), print_nl(SymName), fail ; true
	 )
	 ;
	 listing(snow_symnr)
	),
	told,
	tell(RefFile),
	(
	 member(opt_LEARN_STDFILES,Options) ->
	 (
	  snow_refnr(RefName,_), print_nl(RefName), fail ; true
	 )
	 ;
	 listing(snow_refnr)
	),
	told.




%% print_as_numbered_tree(+Term)
%%
%% prints term edges in the format 11:23,45:3,56:78
%% numbering the vertices uniquely
print_as_numbered_tree(Term) :- p2t(Term),!.

p2t(X):- (atomic(X);var(X);is_numvar(X)),!.
p2t(X):-
	X =.. [H|T],
	get_snow_symnr(H,HNr),
	print_edges_to(HNr,T),
	checklist(p2t, T).

%% print_edges_to(+From,+ToTerms)
print_edges_to(_,[]).
print_edges_to(From,[H|T]):-
	write(From),
	write(':'),
	(
	 (atomic(H);is_numvar(H)) -> H1 = H
	;
	 H=.. [H1|_]
	),
	get_snow_symnr(H1,HNr),
	write(HNr),
	write(','),
	print_edges_to(From,T).

%% translate numerical spec into Refs and assert them
%% not used any more, implementing the translation in perl
%% with an array is much faster
snow_nr_spec_translate(Nr,Nrs):-
	maplist(snow_refnr, [Ref | Refs], [Nr | Nrs]),
	assert(snow_spec( Ref, Refs)), !,
	((0 =:= Nr mod 1000) -> write('.'),nl; true).

% snow_nr_spec_translate(R,Rs):- throw(snow_nr_spec_translate(R,Rs)).


%% parse snow specification
%% not used any more, the prolog translation was very slow
%% use just consult(Specfile) instead now!!
parse_snow_specs(File):-
	throw(do_not_use_this),
	abolish(snow_symnr/2),
	abolish(snow_refnr/2),
	abolish(snow_spec/2),
	abolish(snow_nr_spec/2),
%	dynamic(snow_symnr/2),
%	dynamic(snow_refnr/2),
	dynamic(snow_spec/2),
%	dynamic(snow_nr_spec/2),
	index(snow_symnr(1,1)),
	index(snow_refnr(1,1)),
	concat_atom([File,'.spec'], SpecFile),
	concat_atom([File,'.refnr'], RefFile),
	concat_atom([File,'.symnr'], SymFile),
 	load_files([RefFile, SymFile]),
	index(snow_refnr(1,1)),
	compile_predicates([snow_refnr/2]),
	see(SpecFile),
	repeat,
	read(Spec),
	( Spec = end_of_file -> seen;
	    Spec = snow_nr_spec(Nr,Nrs),
%	    assert(snow_nr_spec(Nr,Nrs)),
	    snow_nr_spec_translate(Nr, Nrs),
	    fail).
% 	load_files([SpecFile, RefFile, SymFile]),
% 	(
% 	  snow_nr_spec(Nr, Nrs),
% 	  snow_nr_spec_translate(Nr, Nrs),
% 	  fail
% 	;
% 	  true
% 	).

%%%%%%%%%%%%%%% requirements switched on by directives %%%%%

%% arithm.dre MML 930
% req_RealAdd,             //12
% req_RealMult,            //13
% req_RealNeg,             //21
% req_RealInv,             //22
% req_RealDiff,            //23
% req_RealDiv,             //24
% req_ImaginaryUnit,       //29
% req_Complex,             //30

% %% boole.dre
% req_Empty,               //4
% req_EmptySet,            //5
% req_Union,               //16
% req_Intersection,        //17
% req_Subtraction,         //18
% req_SymmetricDifference, //19
% req_Meets,               //20

% %% hidden.dre
% req_Any,                 //1
% req_EqualsTo,            //2
% req_BelongsTo,           //3


% %% numerals.dre
% req_Succ,                //15
% req_Omega                //31


% %% real.dre
% req_LessOrEqual,         //14
% req_Positive,            //26
% req_Negative,            //27

% %% subset.dre
% req_Element,             //6
% req_PowerSet,            //7
% req_Inclusion,           //8
% req_SubDomElem,          //9




%%%%%%%%%%%%%%% generating numerical formulas %%%%%%%%%%%%%%%%%%%%

%% ## TEST: :- declare_mptp_predicates,load_mml,
%%             findall(L,(current_atom(L), atom_concat(rq,L1,L), concat_atom([_,_,_|_],'__',L1)), Names),length(Names,N).
%% There are 7456 different evaluations in MML 940 4.48
%% (measured by previous, which may omit some evaluations in proofs of e.g. def. correctness)

%% ###TODO: rqPositive, rqNegative
%% ##TODO: represent the operators with TPTP language
%% ##TODO: arithmetical evaluations take place in Mizar also during
%%         term parsing (identify.pas:CollectInferConst), precisely:
%%   - numerals and rqImaginaryUnit get a numvalue
%%   - rqRealAdd, rqRealMult, rqRealDiff, rqRealDiv, rqRealNeg, rqRealInv
%%     of arguments with numvalue get a numvalue
%%  Parsing (CollectInferConst) can happen in other places than just
%%  By justifications, so we probably have to do these evaluations
%%  automatically, once we encounter such terms and the requirements
%%  tell us to do so.

%% axioms needed for any arithmetical problem:
%% linearity of odering of reals (if rqLessOrEqual appeared)
%% X+0 = X   ARITHM:1 fof(t1_arithm,theorem,![B1: v1_xcmplx_0]: ( k2_xcmplx_0(B1,0) = B1 ),file(arithm,t1_arithm),
%% X + (-Y) = X - Y  fof(spc1_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]: ( k2_xcmplx_0(B1,k4_xcmplx_0(B2)) =
%%                       k6_xcmplx_0(B1,B2) ),file(arithm,spc1_arithm),
%% -(-X) = X  fof(involutiveness_k4_xcmplx_0
%% X*0 = 0  ARITHM:2
%% X*1 = X  ARITHM:3
%% X*(-1) = -X fof(spc2_arithm,theorem,![B1: v1_xcmplx_0]: ( k3_xcmplx_0(B1,k4_xcmplx_0(1)) =  k4_xcmplx_0(B1)),
%%                       file(arithm,spc2_arithm),
%% (X")" = X   (what about 0? - holds) fof(involutiveness_k5_xcmplx_0
%% 1/X = X"  as special case of XCMPLX_0:def 9 (X/Y = X*Y"), let's have an spc3_arithm:
%%              fof(spc3_arithm,theorem,![B1: v1_xcmplx_0]: ( k7_xcmplx_0(1,B1) =  k5_xcmplx_0(B1)),
%%                       file(arithm,spc3_arithm),
%% X/1 = X  ARITHM:6
%% X*(Y/Z) = (X*Y)/Z  fof(spc4_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0),(B3: v1_xcmplx_0) ]:
%%                         ( k3_xcmplx_0(B1,k7_xcmplx_0(B2,B3)) = k7_xcmplx_0(k3_xcmplx_0(B1,B2),B3)),
%%                       file(arithm,spc4_arithm),
%% succ(X) = X+1 ( only if both succ and + present in a problem)
%% (X+Y)*Z= X*Z + Y*Z fof(spc5_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0),(B3: v1_xcmplx_0) ]:
%%                         ( k3_xcmplx_0(k2_xcmplx_0(B1,B2),B3) = k2_xcmplx_0(k3_xcmplx_0(B1,B3),k3_xcmplx_0(B2,B3))),
%%                       file(arithm,spc5_arithm),
%% (X+Y)+Z = X+(Y+Z) fof(spc6_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0),(B3: v1_xcmplx_0) ]:
%%                         ( k2_xcmplx_0(k2_xcmplx_0(B1,B2),B3) = k2_xcmplx_0(B1,k2_xcmplx_0(B2,B3))),
%%                       file(arithm,spc6_arithm),
%% (X*Y)*Z = X*(Y*Z) fof(spc7_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0),(B3: v1_xcmplx_0) ]:
%%                         ( k3_xcmplx_0(k3_xcmplx_0(B1,B2),B3) = k3_xcmplx_0(B1,k3_xcmplx_0(B2,B3))),
%%                       file(arithm,spc6_arithm),
%% version of spc1 catching cases without rqRealDiff (was redundant for square_1, but needed by xreal_1)
%% (-X) + (-Y) = -(X + Y)  fof(spc8_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]:
%%                           ( k2_xcmplx_0(k4_xcmplx_0(B1),k4_xcmplx_0(B2)) =
%%                       k4_xcmplx_0(k2_xcmplx_0(B1,B2)) ),file(arithm,spc8_arithm),
%% another strange phrasing of spc1, when + is not there
%% (-X) - (-Y) = Y - X    fof(spc9_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]:
%%                        ( k6_xcmplx_0(k4_xcmplx_0(B1),k4_xcmplx_0(B2))  =
%%                          k6_xcmplx_0(B2,B1) ),file(arithm,spc9_arithm),
%% (X") * (Y") = (X*Y)"   fof(spc10_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]:
%%                           ( k3_xcmplx_0(k5_xcmplx_0(B1),k5_xcmplx_0(B2)) =
%%                       k5_xcmplx_0(k3_xcmplx_0(B1,B2)) ),file(arithm,spc10_arithm),
%% (X") / (Y") = Y/X     fof(spc11_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]:
%%                        ( k7_xcmplx_0(k5_xcmplx_0(B1),k5_xcmplx_0(B2))  =
%%                          k7_xcmplx_0(B2,B1) ),file(arithm,spc11_arithm),
%% X*(Y") = X/Y      fof(spc12_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]: ( k3_xcmplx_0(B1,k5_xcmplx_0(B2)) =
%%                       k7_xcmplx_0(B1,B2) ),file(arithm,spc12_arithm),
%% -(X - Y) = Y - X
%%
%%


%% (-X) - Y = -(X + Y)
%% X <= X  reflexivity


%% this could become  MML-version specific (it is now)
req_LessOrEqual([4,48,_],r1_xreal_0):-!.
req_LessOrEqual([4,_,_],r1_xxreal_0):-!.
req_LessOrEqual([5,_,_],r1_xxreal_0):-!.
req_ImaginaryUnit(_,k1_xcmplx_0).
req_RealAdd(_,k2_xcmplx_0).
req_RealDiff(_,k6_xcmplx_0).
req_RealDiv(_,k7_xcmplx_0).
req_RealInv(_,k5_xcmplx_0).
req_RealMult(_,k3_xcmplx_0).
req_RealNeg(_,k4_xcmplx_0).
req_Positive([4,48,_],v2_xreal_0):-!.
req_Positive([4,_,_],v2_xxreal_0):-!.
req_Positive([5,_,_],v2_xxreal_0).



%% decode_pn_number(+Atom, [-MizExpr, -CmplxNr])
%%
%% Decode the Polish notation used for encoding numbers in MPTP
%% formula names. The result is an MPTP term, encoded using
%% the corresponding Mizar functors (see the req_Real... above),
%% and a canonical prolog term c(Im,Re) for proper complex numbers, and
%% r(NumInt,DenInt) (e.g. r(-1,3)) for those whose imaginary part is 0,
%% usable for prolog evaluation and comparison.
decode_pn_number(MmlVersion, Atom, [MizExpr, CmplxNr]):-
	ensure(atom_chars(Atom, Chars), decode_pn_number(Atom)),
	ensure(decd_cmplx_nr(Chars, MmlVersion, MizExpr, CmplxNr, []), decd_cmplx_nr(Chars)).

decd_cmplx_nr([c|L1],MmlVersion,MizRes,c(Im,Re),Hole):- !,
	decd_rat_nr(L1, MmlVersion, MizIm1, Im, [r|Rest]),
	decd_rat_nr([r|Rest], MmlVersion, MizRe1, Re, Hole),
	req_ImaginaryUnit(MmlVersion,ImU),
	(
	  MizIm1 = 1 ->
	  MizIm = ImU
	;
	  req_RealMult(MmlVersion,Mult),
	  MizIm =.. [Mult, MizIm1, ImU]
	),
	(
	  MizRe1 = 0 ->
	  MizRes = MizIm
	;
	  req_RealAdd(MmlVersion,Add),
	  MizRes =.. [Add, MizIm, MizRe1]
	).

decd_cmplx_nr([r|L1],MmlVersion,MizRes,RatNr,Hole):- decd_rat_nr([r|L1], MmlVersion, MizRes, RatNr, Hole).

decd_rat_nr([r,n|L1],MmlVersion,MizRes,r(SNum,SDen),Hole):- !,
	decd_signed_nr(L1, MmlVersion, MizNum, SNum1, [d|Rest]),
	decd_signed_nr(Rest, MmlVersion, MizDen, SDen1, Hole),
	ratnr_normalize_sgn(r(SNum1,SDen1), r(SNum,SDen)),
	req_RealDiv(MmlVersion, Div),
	MizRes =.. [Div, MizNum, MizDen].

decd_rat_nr([r|L1],MmlVersion,MizRes,r(SNum,1),Hole):- decd_signed_nr(L1,MmlVersion,MizRes,SNum,Hole).

decd_signed_nr([m|L1],MmlVersion,MizRes,SNum,Hole):- !,
	decd_nat_nr(L1, Res1, Hole),
	SNum is 0 - Res1,
	req_RealNeg(MmlVersion, Neg),
	MizRes =.. [Neg, Res1].

decd_signed_nr(L1,_,Res,Res,Hole):- decd_nat_nr(L1,Res,Hole).

decd_nat_nr([H|T],Res,Hole):-
	digit(H),
	decd_digits(T,Digs,Hole),
	number_chars(Res, [H | Digs]).

decd_digits([D|L],[D|Res1],Hole):-
	digit(D),!,
	decd_digits(L, Res1, Hole).

decd_digits(Hole,[],Hole).

%% Normalize signum of a fraction so that there is at most
%% one, and at the numerator (safety, Mizar ensures this).
ratnr_normalize_sgn(r(SNum1,SDen1), r(SNum,SDen)):-
	  SDen1 < 0, !,
	  SDen is 0 - SDen1,
	  SNum is 0 - SNum1.

ratnr_normalize_sgn(r(SNum,SDen), r(SNum,SDen)).

%% compare two fractions in the standard numerical ordering
ratnr_cmp(Order,r(A,B),r(C,D)):-
	AD is A*D,
	BC is B*C,
	compare(Order, AD, BC).

%% decode_eval_name(+Name,-Req,-Constructor,-MizNumbers,-Numbers)
%%
%% Name is a reference like: rqRealMult__k3_xcmplx_0__rnm1d2_rm2.
%% Returns the requirement (rqRealMult), constructor (k3_xcmplx_0),
%% list of Mizar numbers like:
%% [k7_xcmplx_0(k4_xcmplx_0(1), 2), k4_xcmplx_0(2)], and
%% a list of corresponding prolog-friendly rational numbers like:
%% [r(-1, 2), r(-2, 1)].
decode_eval_name(Name,Req,Constructor,MizNumbers,RatNrs):-
	concat_atom([Req,Constructor,ArgsAtom], '__', Name),
	concat_atom(ArgsAtoms,'_',ArgsAtom),
	mml_version(MmlVersion),
	maplist(decode_pn_number(MmlVersion), ArgsAtoms, Numbers),
	zip(MizNumbers, RatNrs, Numbers).


%% create_eval_fla(Req, Constructor, MizNumbers, RatNrs, -Fla)
create_eval_fla(rqLessOrEqual, Constructor, [MizNr1,MizNr2], [RatNr1,RatNr2], Fla):- !,
	Fla1 =.. [Constructor, MizNr1, MizNr2],
	(
	  ratnr_cmp('>', RatNr1, RatNr2) ->
	  Fla = (~ Fla1)
	;
	  Fla = Fla1
	).

%% ###TODO: check the Mizar result in prolog here
create_eval_fla(_Req, Constructor, MizNumbers, _RatNrs, FuncTerm = MizRes):-
	append(MizArgs, [MizRes], MizNumbers),
	FuncTerm =.. [Constructor | MizArgs].

%% gen_eval_fof(+Name,-Fof,+Options)
%%
%% ##TODO: add a third debugging clause (ensure) if it's ok for callers
gen_eval_fof(Name,_Fof,_Options):- fof(Name,_,_,_,_),!,fail.
gen_eval_fof(Name,Fof,_Options):-
	decode_eval_name(Name, Req, Constructor, MizNumbers, RatNrs),
	create_eval_fla(Req, Constructor, MizNumbers, RatNrs, Fla),
	Fof = fof(Name,theorem, Fla, file(arithm,Name),
		  [mptp_info(0,[],theorem,position(0,0),[0])]).

%% assert_arith_evals(-Names)
%%
%% Asserts arithmetical formulas for evaluations referenced
%% anywhere, and returns their fof names.
%% Cheats by going through the symbol table instead of
%% traversing all formulas, a bit fragile as a result,
%% it calls gen_eval_fof on all symbols starting with rq,
%% and containing at least two '__' substrings. This check could be
%% stronger if needed, and redundant correct eval fofs don't hurt.
assert_arith_evals(Names):-
	findall(L,(
		   current_atom(L),
		   atom_concat(rq,L1,L),
		   concat_atom([_,_,_|_],'__',L1),
		   gen_eval_fof(L,Fof,[]),
		   assert(Fof)
		  ),
		Names).


assert_fof_if_not(fof(Name,_,_,_,_)):- fof(Name,_,_,_,_),!.
assert_fof_if_not(fof(A,B,C,D,E)):- assert(fof(A,B,C,D,E)).
%% assert_arith_reqs(Names)
%%
assert_arith_reqs([spc1_arithm,spc2_arithm,spc3_arithm,spc4_arithm,spc5_arithm,spc6_arithm,
		   spc7_arithm,spc8_arithm,spc9_arithm,spc10_arithm,spc11_arithm,spc12_arithm]):-
	assert_fof_if_not(fof(spc1_arithm,theorem,![(B11: v1_xcmplx_0),(B12: v1_xcmplx_0) ]:
			     ( k2_xcmplx_0(B11,k4_xcmplx_0(B12)) = k6_xcmplx_0(B11,B12) ),
			      file(arithm,spc1_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc2_arithm,theorem,![B21: v1_xcmplx_0]:
			     ( k3_xcmplx_0(B21,k4_xcmplx_0(1)) =  k4_xcmplx_0(B21)),
			      file(arithm,spc2_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc3_arithm,theorem,![B31: v1_xcmplx_0]:
			     ( k7_xcmplx_0(1,B31) =  k5_xcmplx_0(B31)),
			      file(arithm,spc3_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc4_arithm,theorem,![(B41: v1_xcmplx_0),(B42: v1_xcmplx_0),(B43: v1_xcmplx_0) ]:
			     ( k3_xcmplx_0(B41,k7_xcmplx_0(B42,B43)) = k7_xcmplx_0(k3_xcmplx_0(B41,B42),B43)),
			      file(arithm,spc4_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc5_arithm,theorem,![(B51: v1_xcmplx_0),(B52: v1_xcmplx_0),(B53: v1_xcmplx_0) ]:
			     ( k3_xcmplx_0(k2_xcmplx_0(B51,B52),B53) = k2_xcmplx_0(k3_xcmplx_0(B51,B53),k3_xcmplx_0(B52,B53))),
			      file(arithm,spc5_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc6_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0),(B3: v1_xcmplx_0) ]:
			     ( k2_xcmplx_0(k2_xcmplx_0(B1,B2),B3) = k2_xcmplx_0(B1,k2_xcmplx_0(B2,B3))),
			      file(arithm,spc6_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc7_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0),(B3: v1_xcmplx_0) ]:
			     ( k3_xcmplx_0(k3_xcmplx_0(B1,B2),B3) = k3_xcmplx_0(B1,k3_xcmplx_0(B2,B3))),
			      file(arithm,spc6_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc8_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]:
			     ( k2_xcmplx_0(k4_xcmplx_0(B1),k4_xcmplx_0(B2)) = k4_xcmplx_0(k2_xcmplx_0(B1,B2)) ),
			      file(arithm,spc8_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc9_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]:
			     ( k6_xcmplx_0(k4_xcmplx_0(B1),k4_xcmplx_0(B2))  = k6_xcmplx_0(B2,B1) ),
			      file(arithm,spc9_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc10_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]:
			     ( k3_xcmplx_0(k5_xcmplx_0(B1),k5_xcmplx_0(B2)) = k5_xcmplx_0(k3_xcmplx_0(B1,B2)) ),
			      file(arithm,spc10_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc11_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]:
			     ( k7_xcmplx_0(k5_xcmplx_0(B1),k5_xcmplx_0(B2))  = k7_xcmplx_0(B2,B1) ),
			      file(arithm,spc11_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])),
	assert_fof_if_not(fof(spc12_arithm,theorem,![(B1: v1_xcmplx_0),(B2: v1_xcmplx_0) ]:
			     ( k3_xcmplx_0(B1,k5_xcmplx_0(B2)) = k7_xcmplx_0(B1,B2) ),
			      file(arithm,spc12_arithm), [mptp_info(0,[],theorem,position(0,0),[0])])).

%%%%%%%%%%%% export arithmetical formulas for testing with E %%%%%%%%%%%%

%% test integer stuff only, numerals only
%% req_RealAdd(k2_xcmplx_0).
%% req_RealMult(k3_xcmplx_0).
%% req_RealNeg(k4_xcmplx_0).
%% req_RealInv(k5_xcmplx_0).
%% req_RealDiff(k6_xcmplx_0).
%% req_RealDiv(k7_xcmplx_0).
%% req_LessOrEqual(r1_xreal_0).

retract_tptp_int_names(MmlVersion):-
	req_RealAdd(MmlVersion, RealAdd),
	req_RealMult(MmlVersion, RealMult),
	req_RealNeg(MmlVersion, RealNeg),
	req_RealDiff(MmlVersion, RealDiff),
	req_RealDiv(MmlVersion, RealDiv),
	req_LessOrEqual(MmlVersion, LessOrEqual),

	retractall(constr_name(RealAdd,_,_)),
	retractall(constr_name(RealMult,_,_)),
	retractall(constr_name(RealNeg,_,_)),
	retractall(constr_name(RealDiff,_,_)),
	retractall(constr_name(RealDiv,_,_)),
	retractall(constr_name(LessOrEqual,_,_)).

assert_tptp_int_names(MmlVersion):-
	req_RealAdd(MmlVersion, RealAdd),
	req_RealMult(MmlVersion, RealMult),
	req_RealNeg(MmlVersion, RealNeg),
	req_RealDiff(MmlVersion, RealDiff),
	req_RealDiv(MmlVersion, RealDiv),
	req_LessOrEqual(MmlVersion, LessOrEqual),

	assert(constr_name(RealAdd,$plus_int,0)),
	assert(constr_name(RealMult,$times_int,0)),
	assert(constr_name(RealNeg,$uminus_int,0)),
	assert(constr_name(RealDiff,$minus_int,0)),
	assert(constr_name(RealDiv,$divide_int,0)),
	assert(constr_name(LessOrEqual,$lesseq_int,0)).

%% argument is a rational number incoding an integer
is_int_rat(r(_,1)).

%% print_int_evals(Names)
%%
%% Print arithmetical evaluations involving only integers to stdout,
%% using the suggested TPTP syntax.
%%
%% ##TEST: :- declare_mptp_predicates,load_mml,print_int_evals(_).
print_int_evals(Names):-
	mml_version(MmlVersion),
	retract_tptp_int_names(MmlVersion),
	assert_tptp_int_names(MmlVersion),
	findall(L,(
		   current_atom(L),
		   atom_concat(rq,L1,L),
		   concat_atom([_,_,_|_],'__',L1),
		   decode_eval_name(L, Req, Constructor, MizNumbers, RatNrs),
		   Req \= rqRealInv,
		   Req \= rqSucc,
		   checklist(is_int_rat,RatNrs),
		   create_eval_fla(Req, Constructor, MizNumbers, RatNrs, Fla),
		   print(fof(L,conjecture, Fla)),
		   write('.'),nl
		  ),
		Names).

%%%%%%%%%%%%%%% generating scheme instances  %%%%%%%%%%%%%%%%%%%%%%%

%% get_sym_subst(+SchemeSymbol,+SubstitutionList,-Substitution)
%%
%% find the corresponding subst for X, throw exception if not;
%% X is assumed to be a scheme functor or predicate
%% note that copy_term is used here to always refresh the
%% possible "unification" variables, which will later get
%% instantiated to different arguments
get_sym_subst(X,[],_):- !, throw(sch_subst(X)).
get_sym_subst(X,[Z/Y|_],Subst):- X == Z, !, copy_term(X/Y, Subst).
get_sym_subst(X,[_|T],Subst):- get_sym_subst(X,T,Subst).

%% apply_sch_subst0(+SchemeSymbol,+Substitution,-Value)
%%
%% Apply one substitution to SchemeSymbol, yielding Value.
%%
%% SchemeSymbol is assumed to be a scheme functor or predicate,
%% without any arguments.
apply_sch_subst0(Sym,Sym/([]:Val),Val):- !.
apply_sch_subst0(Sym,Sym/([H|T]:Val),Val):- !,throw(sch_subst(Sym,[H|T]:Val)).
apply_sch_subst0(Sym,Sym/Val,Val):-!.
apply_sch_subst0(Sym,Subst,_):- throw(sch_subst(Sym,Subst)).

%% apply_sch_subst1([+SchemeSymbol|+Args],+Substitution,-Value)
%%
%% Apply  one Substitution to a SchemeSymbol with Args, yielding
%% Value. Args must be unifiable with the parameter list of
%% Substitution if given. If the parameter list is not given, we
%% assume that the SchemeSymbol corresponds directly to other
%% constructor, and apply the constructor directly to the Args.
apply_sch_subst1([Sym|Args],Sym/(Vars:Val),Val):- !,
	(Vars = Args,!; throw(sch_subst(Sym,Vars:Val))).

apply_sch_subst1([Sym|Args],Sym/Val,Val1):- Val1 =.. [Val|Args],!.
apply_sch_subst1(Sym,Subst,_):- throw(sch_subst(Sym,Subst)).


%% apply_sch_substs(+Substs,+Fla,-Fla1)
%%
%% applies scheme substitutions to Fla, throwing exception
%% if no substitution for any scheme symbol in Fla does not exist

apply_sch_substs_top(Substs,Fla,Fla1):-
	apply_sch_substs(Substs,Fla,Fla1), !.

%% end of traversal
apply_sch_substs(_,X,X):- var(X),!.
apply_sch_substs(Substs,X1,X2):- atomic(X1), sch_symbol(X1), !,
	get_sym_subst(X1,Substs,Subst),
	apply_sch_subst0(X1,Subst,X2).

apply_sch_substs(_,X,X):- atomic(X),!.

apply_sch_substs(Substs,X1,X2):- X1 =.. [H1|T1], sch_symbol(H1), !,
	maplist(apply_sch_substs(Substs),T1,T2),
	get_sym_subst(H1,Substs,Subst),
	apply_sch_subst1([H1|T2],Subst,X2).

apply_sch_substs(Substs,X1,X2):-
	X1 =.. [H1|T1],
	maplist(apply_sch_substs(Substs),T1,T2),
	X2 =.. [H1|T2].

%% add_univ_context(+VarDecls,+FlaIn,-FlaOut)
%%
add_univ_context([],Fla,Fla).
add_univ_context([H|T], Fla, ( ! [H|T] : ( Fla) )).

%% gen_sch_instance(?SchInstName,?File,-Res,+Options)
%%
%% Generate a scheme instance SchInstNam as fof with the same
%% level (that is []) as the original scheme.
%% If opt_REM_SCH_CONSTS is passed in Options, all local
%% constants inside the instance are generalized to universally
%% quantified variables.
%% Why does generalizing of local constants yield a valid theorem
%% in this case:
%% - the constants never appear in the original scheme, and what we
%%   have is its instance (which involves only type checking, no
%%   special knowledge about the constant); therefore any object
%%   (with the same type) can be used at the place of the constant.
%%
%% ##NOTE: fraenkels in schemes are already deanonymized here, therefore
%%       we have to check sch_orig_copy/2 for instantiation: incase the
%%       original scheme contained a fraenkel, we'll use the original.
%%       This assumes that fraenkels will be abstracted after this (which is true).
gen_sch_instance(SI_Name,F,Res,Options):-
	fof(Ref,_,_,file(F,_),
	    [MPTPInfo,inference(mizar_from,InferInfo,_Refs)|_]),
	member(scheme_instance(SI_Name,S_Name,Ref,_,Substs), InferInfo),
	MPTPInfo= mptp_info(_Nr,Lev,_Kind,Pos,_Args),
	once((sch_orig_copy(S_Name,Fla); fof(S_Name,theorem,Fla,_,_))),
	copy_term(Fla,Tmp),
	apply_sch_substs_top(Substs,Tmp,Fla0),
	(member(opt_REM_SCH_CONSTS,Options) ->
	    generalize_consts(Fla0, Tmp2, UnivContext, _ ),
	    add_univ_context(UnivContext, Tmp2, Fla1),
	    Lev1 = []
	;
	    Fla1 = Fla0, Lev1 = Lev
	),
	Res = fof(SI_Name,theorem, Fla1, file(F,SI_Name),
		  [mptp_info(0,Lev1,scheme_instance,Pos,[]),
		   inference(mizar_sch_instance,InferInfo,[S_Name])]).

%% How to generate a proof of the scheme instance from the original scheme
%% proof:
%% method I:
%% 1. generate the instance without generalization
%% 2. replace the scheme symbols everywhere in the original proof by their
%%    instances (the same process as for the instance)
%% 3. generalize the instance - this yields a constant-to-var substitution,
%%    and a universal context
%% 4. apply the substitution (apply_const_substs) to all proof references
%%    (actually first collect all other local consts from the references,
%%     and enrich the substitution by identity on them, otherwise
%%     get_const_subst will throw exception on them),
%% 5. add the universal context to all the proof references (can be optimized,
%%    to add only the needed subcontext)
%%
%% method II.
%% 1. generate the instance without generalization
%% 2. generalize it, yielding the constant substitution, and the univ context
%% 3. compose the scheme instance subst with the constant subst
%% 4. apply just this composed subst (i.e. pass only once), and also add the context
%%
%% Note: tricky business, be sure to do copy_term on the substitution each time
%%       before using it
%%
%% Technicalities:
%% - the nasmespace for local constants from different articles is now not
%%   separated - this makes loading of the "scheme article" on demand impossible,
%%   while the "scheme instance article" is loaded;
%% - solution: when creating article problems ...
%% - are there problems with computing background?:
%%   - yes, if we only loaded the
%%   "scheme article's BG, then we cannot first instantiate,
%%   and then compute BG, because the instantiation possibly contains constructors
%%   which are not in the BG; however, we always load full MML; this leaves
%%   out BG info only for the "scheme instance article" (if local consts are
%%   generalized);
%%   - if we first compute BG, it may be smaller, since the instances symbols will
%%   not interact with the set of symbols involved in the scheme's proof;
%%   however, we'll need to remove the facts about the original scheme functors and
%%   preds, and add typing BG for the instances symbols instead

%% the algo for creating the high-level (not ND) reproving problems is
%% actually simple
%% - generate the reproving problem for the original scheme;
%%   that problem contains top-level references (top lemmas,
%%   theorems, defs, scheme instances), possibly fraenkel defs,
%%   typing info about the scheme functors and predicates, and
%%   other top-level background (even if a top-level constant
%%   typing is there, we should be OK, since this is done for the
%%   article with the original scheme)
%% - if another scheme instance is used in the proof, apply the
%%   substitutions to the scheme functors
%% - but in addition to the standard filtering of sublevel references,
%%   filter also all the typing info for the scheme functors
%% - compute background for the generalized scheme instance; it needs
%%   to be done in the context of the instance article

%% - add to the top-level symbol set the symbols from the scheme instance
%%   (actually those from the generalized-consts version should be enough)
%% - rerun


%% get_top_scheme_instances_for(+Problem, -SchInstances)
%%
%% Generates the chains of scheme instances for the Problem.
%% Allowed problems must be top-level. The chains can be later re-used for
%% constructing proper proofs of the instances.
%% Following generates the chains of scheme instances for the MPTP Challenge:
%% ##TEST: :- get_top_scheme_instances_for(l37_yellow19, SchInstances), write(SchInstances),length(SchInstances,N).
%% Follwing writes only the top-level scheme instances (useful as input for
%% mk_sch_instance_problem_from_th_top_l)
%% ##TEST: :- get_top_scheme_instances_for(l37_yellow19, SchInstances),
%%            findall(S,(member(SL,SchInstances),length(SL,N),N1 is N - 1, nth1(N1,SL,S)),Tops1),
%%	      sort(Tops1,Tops), write(Tops),length(Tops,L1).
get_top_scheme_instances_for(Problem, SchInstances):-
	declare_mptp_predicates,load_mml,load_lemmas,
	install_index,
	get_top_scheme_instances_for1(Problem, [], [], SchInstances, _).
get_top_scheme_instances_for1(Problem, _SchAncestors, DoneThsIn, [], DoneThsIn):-
	member(Problem, DoneThsIn),!.
get_top_scheme_instances_for1(Problem1, SchAncestors, DoneThsIn, SchInstances, DoneThsOut):-
	(
	  Problem1 = [Problem, ProbSchInst],!
	;
	  Problem = Problem1
	),
	get_ref_fof(Problem, fof(_,_,_,_,Info)),
	Info = [_, inference(_,_,ARefs0) |_ ],
	findall(ARef0,
		(
		  member(ARef0,ARefs0),
		  atom_chars(ARef0,[C1|Cs1]),
		  member(C1,[t,l])
		),
		TopThRefs1
	       ),
	sort(TopThRefs1, TopThRefs2),
	subtract(TopThRefs2, DoneThsIn, TopThRefs),
	findall([ARef2, ARef0],
		(
		  member(ARef0,ARefs0),
		  atom_chars(ARef0,[s|Cs1]),
		  fix_sch_ref(ARef0,[s|Cs1],ARef2)
		),
		TopSchPairs
	       ),
	(
	  Problem1 = [Problem, ProbSchInst] ->
	  NewDone = DoneThsIn,
	  NewSchAncestors = [ ProbSchInst | SchAncestors]
	;
	  NewDone = [Problem|DoneThsIn],
	  NewSchAncestors = [ Problem ]
	),
	zip(_TopSchRefs, TopSchInsts, TopSchPairs),
	get_top_scheme_instances_for1_l(TopThRefs, [], NewDone, SchInsts1, DoneThsOut1),
	get_top_scheme_instances_for1_l(TopSchPairs, NewSchAncestors, DoneThsOut1, SchSchInsts1, DoneThsOut),
	findall([SchInst | NewSchAncestors], member(SchInst, TopSchInsts), TopSchInsts1),
	append(TopSchInsts1, SchInsts1, SchInsts2),
	append(SchInsts2, SchSchInsts1, SchInstances).

get_top_scheme_instances_for1_l([], _, DoneThsIn, [], DoneThsIn).
get_top_scheme_instances_for1_l([H|T], SchAncestors, DoneThsIn, SchInstances, DoneThsOut):-
	get_top_scheme_instances_for1(H, SchAncestors, DoneThsIn, SchInstances1, DoneThsOut1),
	get_top_scheme_instances_for1_l(T, SchAncestors, DoneThsOut1, SchInstances_l, DoneThsOut),
	append(SchInstances1, SchInstances_l, SchInstances).


% mk_rec_sch_instance_problem_from_th(SI_Name,Options, SubstStackIn, NewSubstStack, SchInstsToDo):-
%	mk_sch_instance_problem_from_th(SI_Name,Options, SubstStackIn, NewSubstStack, SchInstsToDo).

mk_sch_instance_problem_from_th_l(_Ancestors, [], _Options, _SubstStackIn).
mk_sch_instance_problem_from_th_l(Ancestors, [SI_Name |T], Options, SubstStackIn):-
	mk_sch_instance_problem_from_th(Ancestors,SI_Name,Options, SubstStackIn, _NewSubstStack,
					_NewAncestors, _SchInstsToDo),
	mk_sch_instance_problem_from_th_l(Ancestors, T, Options, SubstStackIn).

mk_sch_instance_problem_from_th_top(SI_Name,Options):-
	abolish(rec_sch_inst_name/2),
	dynamic(rec_sch_inst_name/2),
	mk_sch_instance_problem_from_th([],SI_Name,Options, [],
					_NewSubstStack, _NewAncestors, _SchInstsToDo).

%% Following creates all scheme instance problems for MPTP Challenge (note that it also
%% creates instances of tarski, which are axioms (not provable) - those problems
%% should be removed
%% ##TEST: :- declare_mptp_predicates,load_mml,install_index,
%%      A=[s1_ordinal1__e8_6__wellord2,s1_ordinal2__e18_27__finset_1,s1_relat_1__e6_21__wellord2,
%%	s1_wellord2__e6_39_3__yellow19,s1_xboole_0__e4_27_3_1__finset_1,s1_xboole_0__e6_22__wellord2,
%%	s1_xboole_0__e6_27__finset_1,s2_finset_1__e11_2_1__waybel_0,s2_funct_1__e10_24__wellord2,
%%	s2_funct_1__e4_7_1__tops_2,s2_funct_1__e4_7_2__tops_2,s3_funct_1__e16_22__wellord2,
%%	s3_subset_1__e1_40__pre_topc,s3_subset_1__e2_37_1_1__pre_topc],
%%       mk_sch_instance_problem_from_th_top_l(A,[opt_MK_TPTP_INF, opt_REM_SCH_CONSTS]).
mk_sch_instance_problem_from_th_top_l(SI_Names,Options):-
	abolish(rec_sch_inst_name/2),
	dynamic(rec_sch_inst_name/2),
	mk_sch_instance_problem_from_th_l([],SI_Names,Options, []).

%% The main procedure for creating problems from layered scheme instances.
%% The NewSubstStack, NewAncestors, and SchInstsToDo are now unused as output
%% parameters ( could be changed in the future).
%% ##TODO: Should be (much) improved in terms of efficiency (minimize the number of
%% full article reloadings for a set of schemes!!).
%% ##TODO: enhance to create an ND version for GDV (possible way: use new dummy
%%         scheme functors/predicates for outputing the main scheme ND proof, then
%%         just add equivalence to their instances and verify the instances' typing)
mk_sch_instance_problem_from_th(Ancestors,SI_Name,Options, SubstStackIn, NewSubstStack, NewAncestors, SchInstsToDo):-
	ensure(concat_atom([S_Name, Problem1, InstArticle], '__', SI_Name), schinst_name(SI_Name)),
	get_sch_inst_name(Ancestors, SI_Name, SchInstName),
	print([SI_Name, SchInstName, Ancestors]), nl,
	get_ref_fof(S_Name, fof(S_Name,_,Fla,file(SchArticle,_),SchInfo)),
	SchInfo = [mptp_info(_,_,_,_,_), inference(_,_,SchRefs) | _],
	findall(SchInst1,
		(
		  member(SchInst1, SchRefs),
		  atom_concat('s', _, SchInst1)
		),
		SchInstsToDo
	       ),
	%% print the background and conjecture created from the InstArticle,
	%% also create the substitutions and the UnivContext
	load_proper_article(InstArticle, Options, PostLoadFiles),
	fof(Problem1,_,_,file(InstArticle,_),
	    [MPTPInfo,inference(mizar_from,InferInfo,_Refs)|_]),
	MPTPInfo = mptp_info(_,_,_,position(InstLine0, InstCol0),_),
	member(scheme_instance(SI_Name,S_Name,_Ref,_,Substs), InferInfo),
	(
	    member(position(InstLine,InstCol), InferInfo) -> true
	;
	    InstLine = InstLine0,
	    InstCol = InstCol0
	),
	zip_s('/',SchFuncsAndPreds,_SchInsts,Substs),
	copy_term(Fla,Tmp),
	apply_sch_substs_top(Substs,Tmp,Fla0),
	(member(opt_REM_SCH_CONSTS,Options) ->
	    generalize_consts(Fla0, Tmp2, UnivContext, NewConstSubst),
	    add_univ_context(UnivContext, Tmp2, Fla1_1)
	;
	    ensure(fail, bad_sch_inst_options(Options))
	),
	(
	  Ancestors = [] -> BGArticle = InstArticle, Problem = Problem1, BGPostLoadFiles = PostLoadFiles
	;
	  last(Ancestors, [Problem, BGArticle]),
	  retractall(fof(_,_,_,file(InstArticle,_),_)),
	  load_files(PostLoadFiles,[silent(true)]),
	  load_proper_article(BGArticle, Options, BGPostLoadFiles)
	),
	print([SubstStackIn, Fla1_1]),
	instantiate_and_gen_sch_inst_many(SubstStackIn, Fla1_1, Fla1), write(hehe),nl,
	print(Fla1),nl,

	collect_symbols_top( Fla1, Syms1),
	NewSubstStack = [[UnivContext, NewConstSubst, Substs] | SubstStackIn],
	NewAncestors = [[Problem1,InstArticle] | Ancestors],
%	zip(AncProblems,AncArticles,NewAncestors),
%	concat_atom(AncProblems,'__',OutName1),
	OutName1 = SchInstName,
	ensure(article_position(Problem, Pos1), throw(article_position(Problem))), write(haha),
	once(fixpoint(BGArticle, [Pos1, []], mizar_from, [], [], Syms1, AllRefs)),
	concat_atom([problems,'/',dummy_schinst,'_',OutName1],Outfile),
	tell(Outfile),
	format('% Mizar problem: ~w,~w,~w,~w,~w ~n', [SI_Name,InstArticle,InstLine,InstCol,Ancestors]),
	write('% scheme substs: '),
	print(Substs), nl,
	write('% constant substs: '),
	print(NewConstSubst), nl,
	write('% subst stack: '),
	print(SubstStackIn), nl,
	delete(AllRefs, SI_Name, ProperRefs1),

	copy_term(Fla1, Fla11),
	sort_transform_top(Fla11,SR2),
	numbervars(SR2,0,_),
	print(fof(SchInstName,conjecture,SR2,file(BGArticle,SchInstName),[])),
	write('.'), nl,

%	print_ref_as_conjecture(Options, [], SI_Name),
	checklist(print_ref_as_axiom(Options), ProperRefs1),
	write('%%% end of instance refs'), nl,
	told,
	retractall(fof(_,_,_,file(BGArticle,_),_)),
	load_files(BGPostLoadFiles,[silent(true)]),
	%% now print the scheme proof
	%% the scheme functor axioms are filtered away (replaced by type inference
	%% on their instances),
	load_proper_article(SchArticle, Options, SchPostLoadFiles),
	mk_problem_data(_,SchArticle,dummy,[[mizar_proof],
					     [scheme], problem_list([S_Name])],
				 Options,_Outfile,_Line,_Col,SchAllRefs),
	get_sec_info_refs([], SchFuncsAndPreds,
			  [mptp_info(_,_,functor,_,[scheme,type|_])|_], SchFuncTyps),
	delete(SchAllRefs, S_Name, SchProperRefs1),
	subtract(SchProperRefs1, ProperRefs1, SchNewProperRefs0),
	subtract(SchNewProperRefs0, SchFuncTyps, SchNewProperRefs2),
	subtract(SchNewProperRefs2, SchInstsToDo, SchNewProperRefs1),
	maplist(mk_sch_inst_name(NewAncestors), SchInstsToDo, _NewSchInstsNames),
	append(Outfile),
	checklist(rec_instantiate_and_gen_sch_inst(NewSubstStack, Options, NewAncestors), SchInstsToDo),
	checklist(print_ref_as_axiom(Options), SchNewProperRefs1),
	retractall(fof(_,_,_,file(SchArticle,_),_)),
	load_files(SchPostLoadFiles,[silent(true)]),
	told,
	mk_sch_instance_problem_from_th_l(NewAncestors,SchInstsToDo,Options,NewSubstStack).


%% get_sch_inst_name(+Ancestors, +SchInst, -SchInstName)
%%
%% assumes that we are inside mk_sch_instance_problem_from_th,
%% and that nontrivial (i.e. layered scheme instance) names were
%% already created by call to mk_sch_inst_name, and are available
%% in rec_sch_inst_name/2
get_sch_inst_name([], SchInst, SchInst).
get_sch_inst_name([H | T], SchInst, SchInstName):-
	last([H | T], [Problem, BGArticle]),
	concat_atom([S_Name, _Problem1, _InstArticle], '__', SchInst),
	concat_atom([S_Name, Problem, BGArticle], '__', SchInst1),
	rec_sch_inst_name(SchInst1,N),
	concat_atom([SchInst1, '__', N], SchInstName).

%% mk_sch_inst_name(+Ancestors, +SchInst, -SchInstName)
%%
%% create a name for a layered scheme instance, store it
%% using rec_sch_inst_name/2
%%
%% mk_sch_inst_name([], SchInst, SchInst).
mk_sch_inst_name( [H | T], SchInst, SchInstName):-
	last([H | T], [Problem, BGArticle]),
	concat_atom([S_Name, _Problem1, _InstArticle], '__', SchInst),
	concat_atom([S_Name, Problem, BGArticle], '__', SchInst1),
	(
	  retract(rec_sch_inst_name(SchInst1,N)) ->
	  N1 is N + 1
	;
	  N1 = 1
	),
	assert(rec_sch_inst_name(SchInst1,N1)),
	concat_atom([SchInst1, '__', N1], SchInstName).


%% rec_instantiate_and_gen_sch_inst(+SubstStack, +Options, +Ancestors, +SchInst)
%%
%% Instantiate a scheme SchInst using the stack of substitutions SubstStack.
%% The Options and Ancestors serve only for printing info.
rec_instantiate_and_gen_sch_inst(SubstStack, Options, Ancestors, SchInst):-
	get_ref_fof(SchInst, fof(SchInst,_,Fla,Q3,Q4)),
	ensure(get_sch_inst_name(Ancestors, SchInst, SchInstName), get_sch_inst_name(SchInst)),
	instantiate_and_gen_sch_inst_many(SubstStack, Fla, FlaOut),
	sort_transform_top(FlaOut,SR2),
	numbervars([SR2,Q3,Q4],0,_),
	Status = axiom, QQ3 = Q3, QQ4= [],
	(member(opt_MK_TPTP_INF,Options) ->
	    print(fof(SchInstName,Status,SR2,QQ3,QQ4))
	;
	    print(fof(SchInstName,Status,SR2,Q3,Q4))
	),
	write('.'),
	nl.


%% perform %% instantiate_and_gen_sch_inst/5 multiple times
instantiate_and_gen_sch_inst_many([], FlaIn, FlaIn).
instantiate_and_gen_sch_inst_many([[UnivContext, NewConstSubst, Substs] | OldSubstStack], FlaIn, FlaOut):-
	copy_term(FlaIn,Tmp),
	copy_term([UnivContext,NewConstSubst], [FreshUnivContext, FreshConstSubst]),
	apply_sch_substs_top(Substs,Tmp,Fla0),
	apply_const_substs(FreshConstSubst, Fla0, Fla1),
	add_univ_context(FreshUnivContext, Fla1, FlaOut1),
	instantiate_and_gen_sch_inst_many(OldSubstStack, FlaOut1, FlaOut).

%% instantiate_and_gen_sch_inst(+UnivContext, +NewConstSubst, +Substs, +FlaIn, -FlaOut)
%%
%% Apply a scheme substitution Substs to FlaIn, then apply constant substitutions
%% in NewConstSubst to the result, and add the UnivContext to it.
instantiate_and_gen_sch_inst(UnivContext, NewConstSubst, Substs, FlaIn, FlaOut):-
	copy_term(FlaIn,Tmp),
	copy_term([UnivContext,NewConstSubst], [FreshUnivContext, FreshConstSubst]),
	apply_sch_substs_top(Substs,Tmp,Fla0),
	apply_const_substs(FreshConstSubst, Fla0, Fla1),
	add_univ_context(FreshUnivContext, Fla1, FlaOut).

%% get_problem_sch_insts_from_article(+Article, +ProblemPairs, +Options, -SI_Names, topRefs)
%%
%% Get scheme instances from problems (generated with Options) in ProblemPairs
%% corresponding to Article.
%% Not used now, but an example of using mk_problem_data.
get_problem_sch_insts_from_article(A, Pairs, Options, SI_Names, topRefs):-
	findall(P, member([A,P], Pairs), AList),
	load_proper_article(A, Options, PostLoadFiles),
	findall(SI_Name,
		(
		  mk_problem_data(_,A,dummy,[[mizar_by,mizar_from,mizar_proof],
					     [theorem, top_level_lemma], problem_list(AList)],
				 Options,_Outfile,_Line,_Col,AllRefs),
		  member(SI_Name, AllRefs),
		  atom_chars(SI_Name, [s, Digit | _]),
		  digit(Digit)
		),
		SI_Names
	       ),
	%% retract current file but return mml parts,
	retractall(fof(_,_,_,file(A,_),_)),
	load_files(PostLoadFiles,[silent(true)]).

%% create and assert all scheme instances for a given article
assert_sch_instances(File,Options):-
	repeat,
	( gen_sch_instance(_,File,Res,Options), assert(Res), fail; true).


%%%%%%%%%%%% Creating henkin axioms %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% create and assert all henkin axioms for a given article
assert_henkin_axioms(File,_Options):-
	repeat,
	(
	  fof_file(File,Id),
	  clause(fof(Ref,_,_,file(File,Ref),[Mptp_info|Rest]),_,Id),
	  (member(mizar_nd(inference(let,_,Refs0)), Rest) ->
	      append(Defs,[T],Refs0),
	      %% remove possible thesisexpansions
	      findall(Hax,(member(Hax,Defs),atom_prefix(Hax,'dh_')),Henkin_Refs),
	      create_henkin_axioms_let_top(Henkin_Refs,T,HaFofs)
	  ;
	      Mptp_info = mptp_info(_,_,_,_,[_,_,considered_constants(HC)|_]),
	      maplist(atom_concat('dh_'),HC,Henkin_Refs),
	      create_henkin_axioms_consider_top(Henkin_Refs,Ref,HaFofs)
	  ),
	  checklist(assert,HaFofs),
	  fail
	;
	  true
	).
/*
   example:
   fof(t2_abcmiz_0,theorem,![B1: l1_abcmiz_0]: ![B2: l1_abcmiz_0]: ( ( ( u1_abcmiz_0(B1) = u1_abcmiz_0(B2) ) & sort(B1,v4_abcmiz_0) ) => ( sort(B2,v4_abcmiz_0) ) ),file(abcmiz_0,t2_abcmiz_0),[mptp_info(2,[],theorem,position(257,37),[0,mizar_item(justifiedtheorem)]),inference(mizar_proof,[proof_level([14])],[d4_abcmiz_0,e2_14,e1_14,d4_abcmiz_0]),mizar_nd(inference(let,[],[dh_c1_14,dh_c2_14,i1_14]))]).
 fof(dt_c1_14,assumption,sort(c1_14,l1_abcmiz_0),file(abcmiz_0,c1_14),[mptp_info(1,[14],constant,position(0,0),[let,type])]).
 fof(dt_c2_14,assumption,sort(c2_14,l1_abcmiz_0),file(abcmiz_0,c2_14),[mptp_info(2,[14],constant,position(0,0),[let,type])]).
 fof(i1_14,thesis,( ( ( u1_abcmiz_0(c1_14) = u1_abcmiz_0(c2_14) ) & sort(c1_14,v4_abcmiz_0) ) => ( sort(c2_14,v4_abcmiz_0) ) ),file(abcmiz_0,i1_14),[mptp_info(1,[14],thesis,position(0,0),[]),mizar_nd(inference(discharge_asm,[discharged([e1_14])],[i2_14]))]).

 algo for "let":
 1. get the ND inference references, i.e. [dh_c1_14,dh_c2_14,i1_14]
 2. look up the used referred thesis, i.e. i1_14
    (cannot use the orig. thesis, i.e. t2_abcmiz_0, because of thesis expansions)
 3. look up the sort declaration, i.e.  dt_c1_14,dt_c2_14
 4. create henkin axiom for the last constant, i.e.
 4a. replace the last (n-th) constant everywhere with a new variable V_n (V2),
     yielding GenThes_, i.e.:
     GenThes_n = ( ( u1_abcmiz_0(c1_14) = u1_abcmiz_0(V2) ) & sort(c1_14,v4_abcmiz_0) )
     => ( sort(V2,v4_abcmiz_0) )
 4b. add the universal quantifier, with the n-th constant's sort,
     yielding UnivGenThes_n,i.e.:
     UnivGenThes_ = ![V2:  l1_abcmiz_0] : GenThes1
 4c. create the implication (Sort => Thesis) => UnivGenThes_n, i.e.:
     (sort(c1_14,l1_abcmiz_0)
      =>
      ( ( ( u1_abcmiz_0(c1_14) = u1_abcmiz_0(c2_14) ) & sort(c1_14,v4_abcmiz_0) )
         => sort(c2_14,v4_abcmiz_0) )
     )
     => ![V2:  l1_abcmiz_0] : GenThes_n
 4d. the result ((Sort => Thesis) => UnivGenThes_n) is the henkin axiom for the last (n-th) constant,
     here for c2_14, i.e dh_c2_14
 5.  iterate for the rest of the constants, taking UnivGenThes_{n+1} instead of
     thesis for n-th constant


 fof(dt_c1_25_1_1,lemma_conjecture,sort(c1_25_1_1,( ~ v3_struct_0 & v3_realset2 & v2_orders_2 & l1_orders_2 )),file(abcmiz_0,c1_25_1_1),[mptp_info(1,[25,1,1],constant,position(0,0),[consider,type]),mizar_nd(inference(consider,[],[dh_c1_25_1_1,e1_25_1_1]))]).
 fof(dt_c2_25_1_1,lemma_conjecture,sort(c2_25_1_1,( ~ v4_abcmiz_0 & v5_abcmiz_0 & v6_abcmiz_0 & l1_abcmiz_0 )),file(abcmiz_0,c2_25_1_1),[mptp_info(2,[25,1,1],constant,position(0,0),[consider,type]),mizar_nd(inference(consider,[],[dh_c1_25_1_1,dh_c2_25_1_1,e1_25_1_1]))]).
 fof(dt_c3_25_1_1,lemma_conjecture,sort(c3_25_1_1,( v1_funct_1 & v1_funct_2(u1_struct_0(c1_25_1_1),k5_finsub_1(u1_abcmiz_0(c2_25_1_1))) & m2_relset_1(u1_struct_0(c1_25_1_1),k5_finsub_1(u1_abcmiz_0(c2_25_1_1)))
 )),file(abcmiz_0,c3_25_1_1),[mptp_info(3,[25,1,1],constant,position(0,0),[consider,type]),mizar_nd(inference(consider,[],[dh_c1_25_1_1,dh_c2_25_1_1,dh_c3_25_1_1,e1_25_1_1]))]).

 fof(e1_25_1_1,lemma_conjecture,?[B1: ( ~ v3_struct_0 & v3_realset2 & v2_orders_2 & l1_orders_2 )]: ?[B2: ( ~ v4_abcmiz_0 & v5_abcmiz_0 & v6_abcmiz_0 & l1_abcmiz_0 )]: ?[B3: ( v1_funct_1 & v1_funct_2(u1_struct_0(B1),k5_finsub_1(u1_abcmiz_0(B2))) & m2_relset_1(u1_struct_0(B1),k5_finsub_1(u1_abcmiz_0(B2))) )]: $true,file(abcmiz_0,e1_25_1_1),[mptp_info(1,[25,1,1],proposition,position(408,68),[0,mizar_item(consider_justification),considered_constants([c1_25_1_1,c2_25_1_1,c3_25_1_1])]),inference(mizar_by,[],[])]).


 algo for "consider":
 1. get the ND inference references, i.e. [dh_c1_25_1_1,e1_25_1_1]
 2. complete the constant list, using the considered_constants slot of the last reference (Just)
     (i.e. e1_25_1_1) to the full list, i.e. c1_25_1_1,c2_25_1_1,c3_25_1_1
 3. look up the sort declaration, i.e.   dt_c1_25_1_1,dt_c2_25_1_1,dt_c3_25_1_1
 4. create henkin axiom for the first constant, i.e.
 4a. instantiate the first variable everywhere with the first constant,
     yielding ExRes_1, i.e.:
     ExRes_1 = ?[c1_25_1_1: ( ~ v3_struct_0 & v3_realset2 & v2_orders_2 & l1_orders_2 )]:
               ?[B2: ( ~ v4_abcmiz_0 & v5_abcmiz_0 & v6_abcmiz_0 & l1_abcmiz_0 )]:
 	      ?[B3: ( v1_funct_1 & v1_funct_2(u1_struct_0(c1_25_1_1),k5_finsub_1(u1_abcmiz_0(B2)))
 		    & m2_relset_1(u1_struct_0(c1_25_1_1),k5_finsub_1(u1_abcmiz_0(B2))) )]: $true
 4b. strip the exist. quantifier, yielding the 1st constant's sort (Srt_1 - note that we do
     not copy it from the sort declaration, to be independent for verification), and Res_1:
     Srt1 = sort(c1_25_1_1: ( ~ v3_struct_0 & v3_realset2 & v2_orders_2 & l1_orders_2 ))
     Res_1 = ?[B2: ( ~ v4_abcmiz_0 & v5_abcmiz_0 & v6_abcmiz_0 & l1_abcmiz_0 )]:
             ?[B3: ( v1_funct_1 & v1_funct_2(u1_struct_0(c1_25_1_1),k5_finsub_1(u1_abcmiz_0(B2)))
                   & m2_relset_1(u1_struct_0(c1_25_1_1),k5_finsub_1(u1_abcmiz_0(B2))) )]: $true

 4c. create the implication Just => (Srt_1 & Res1), i.e.:
     ( ?[B1: ( ~ v3_struct_0 & v3_realset2 & v2_orders_2 & l1_orders_2 )]:
       ?[B2: ( ~ v4_abcmiz_0 & v5_abcmiz_0 & v6_abcmiz_0 & l1_abcmiz_0 )]:
       ?[B3: ( v1_funct_1 & v1_funct_2(u1_struct_0(B1),k5_finsub_1(u1_abcmiz_0(B2)))
             & m2_relset_1(u1_struct_0(B1),k5_finsub_1(u1_abcmiz_0(B2))) )]: $true )
     => sort(c1_25_1_1: ( ~ v3_struct_0 & v3_realset2 & v2_orders_2 & l1_orders_2 ))
        & ( ?[B2: ( ~ v4_abcmiz_0 & v5_abcmiz_0 & v6_abcmiz_0 & l1_abcmiz_0 )]:
            ?[B3: ( v1_funct_1 & v1_funct_2(u1_struct_0(c1_25_1_1),k5_finsub_1(u1_abcmiz_0(B2)))
                  & m2_relset_1(u1_struct_0(c1_25_1_1),k5_finsub_1(u1_abcmiz_0(B2))) )]: $true)

 4d. the result (Just => (Srt_1 & Res1)) is the henkin axiom for the first constant,
     here for c1_25_1_1, i.e dh_c1_25_1_1
 5.  iterate for the rest of the constants, taking Res_{n-1} instead of
     Just for n-th constant

*/

%% apply_const_subst(+(Const/Var),+Fla,-Fla1)
%%
%% applies local constant substitution to Fla

%% end of traversal
apply_const_subst(_,X,X):- var(X),!.
apply_const_subst((X1/Var1),X1,Var1):- atomic(X1), mptp_local_const(X1), !.
apply_const_subst(_,X,X):- atomic(X),!.

apply_const_subst(Subst,X1,X2):-
	X1 =.. [H1|T1],
	maplist(apply_const_subst(Subst),T1,T2),
	X2 =.. [H1|T2].

%% make a new variable for the constant
make_const_var_subst(Const, (Const/_NewVar)).


%% create_henkin_axioms_let_top(+Henkin_Refs,+Thesis_Ref,-Henkin_Fofs)
%%
%% this assumes that the Henkin_Refs come in the right order, i.e.
%% from first constant to the last (if not they need to be sorted)
create_henkin_axioms_let_top(Henkin_Refs0,Thesis_Ref,Henkin_Fofs0):-
	reverse(Henkin_Refs0, Henkin_Refs),
	ensure(get_ref_fla(Thesis_Ref,Thesis), henkin(Thesis_Ref)),
	ensure(maplist(atom_concat('dh_'),Consts,Henkin_Refs),henkin_bad_refs(Henkin_Refs0)),
	maplist(atom_concat('dt_'),Consts,Sort_Refs),
	create_henkin_axioms_let(Henkin_Refs,Consts,Sort_Refs,Thesis,_,Henkin_Fofs),
	reverse(Henkin_Fofs, Henkin_Fofs0), !.


%% create_henkin_axioms_let(+Henkin_Refs,+Constants,+Sort_Refs,+Instance,-UnivInst,-Henkin_Fofs)
%%
%% the loop - needed for passing the generalized instance along
create_henkin_axioms_let([],[],[],Inst,Inst,[]).
create_henkin_axioms_let([HRef|HRefs],[Const|Consts],[SRef|SRefs],Instance,UnivInst,[HFof|HFofs]):-
	create_henkin_axiom_let(HRef,Const,SRef,Instance,UnivGenInst,HFof),
	create_henkin_axioms_let(HRefs,Consts,SRefs,UnivGenInst,UnivInst,HFofs).


%% create_henkin_axiom_let(+Henkin_Ref,+Const,+Sort_Ref,+Instance,-UnivGenInst,-Henkin_Fof)
%%
%% Given the desired name of the new axiom (Henkin_Ref), the new constant (Const),
%% its sort declaration (Sort_Ref with formula Sort_Fla sort(Const,Sort)), and the instance formula (Instance),
%% containing the Const, replace Const in Instance with NewVar, and add quantification (! [NewVar : Sort])
%% yielding UnivGenInst, and
%% create the Henkin_Fof, saying that
%% (sort(Const,Sort) => Instance) => UnivGenInst
%%
%% note that the processing order of constants guarantees
%% that only the right constants are present in the new quantification
create_henkin_axiom_let(Henkin_Ref,Const,Sort_Ref,Instance,UnivGenInst,Henkin_Fof):-
	copy_term(Instance, Tmp0),
	make_const_var_subst(Const, (Const/NewVar)),
	apply_const_subst((Const/NewVar), Tmp0, Tmp),
	ensure(get_ref_fof(Sort_Ref, fof(_,_,Sort_Fla,FileInfo,[mptp_info(Nr,Lev,constant,Pos,[Kind,type|_])|_])),
	       henkin_noref(Sort_Ref)),
	ensure( (Sort_Fla = sort(Const, Sort)), henkin_bad_sort(Sort_Fla)),
	copy_term(Sort, Sort1),
	UnivGenInst = ( ! [NewVar : Sort1] : ( Tmp) ),
	copy_term((Sort_Fla => Instance), Tmp1),
	Henkin_Ax = ( Tmp1 => UnivGenInst ),
	Henkin_Fof = fof(Henkin_Ref, axiom, Henkin_Ax, FileInfo,
			 [mptp_info(Nr,Lev,constant,Pos,[Kind,henkin_axiom])]),!.


%% create_henkin_axioms_consider_top(+Henkin_Refs,+Thesis_Ref,-Henkin_Fofs)
%%
%% this assumes that the Henkin_Refs come in the right order, i.e.
%% from first constant to the last (if not they need to be sorted)
create_henkin_axioms_consider_top(Henkin_Refs,Thesis_Ref,Henkin_Fofs):-
	ensure(get_ref_fla(Thesis_Ref,Thesis), henkin(Thesis_Ref)),
	ensure(maplist(atom_concat('dh_'),Consts,Henkin_Refs),henkin(Thesis_Ref)),
	maplist(atom_concat('dt_'),Consts,Sort_Refs),
	create_henkin_axioms_consider(Henkin_Refs,Consts,Sort_Refs,Thesis,_,Henkin_Fofs),!.


%% create_henkin_axioms_consider(+Henkin_Refs,+Constants,+Sort_Refs,+ExFla,-ExInstance,-Henkin_Fofs)
%%
%% the loop - needed for passing the instance along
create_henkin_axioms_consider([],[],[],Inst,Inst,[]).
create_henkin_axioms_consider([HRef|HRefs],[Const|Consts],[SRef|SRefs],ExFla,ExInstance,[HFof|HFofs]):-
	create_henkin_axiom_consider(HRef,Const,SRef,ExFla,ExInst1,HFof),
	create_henkin_axioms_consider(HRefs,Consts,SRefs,ExInst1,ExInstance,HFofs).

%% create_henkin_axiom_consider(+Henkin_Ref,+Const,+Sort_Ref,+ExFla,-ExInstance,-Henkin_Fof)
%%
%% Given the desired name of the new axiom (Henkin_Ref), the new constant (Const),
%% its sort declaration (Sort_Ref - this is only used for useful_info of
%% the new axiom), and the existential formula (ExFla, in the form ? [(ExVar : Sort)] : Body ),
%% create the Henkin_Fof, saying that
%% ExFla => (sort(Const,Sort) & Body),
%% and also return ExInstance, which is Body with ExVar instantiated to Const.
%%
%% Note that the processing order of constants guarantees
%% that only the right constants are present in the new quantification.
%% Also note that the Sort_Ref is only used for getting the  mptp_info
%% when creating the new fof, not for copying the sort from its formula.
create_henkin_axiom_consider(Henkin_Ref,Const,Sort_Ref,ExFla,ExInstance,Henkin_Fof):-
	copy_term(ExFla, Tmp0),
	%% Sort is without external variables at this point
	%% (there still can be some inside fraenkels though -
	%% that's why we do copy_term below)
	ensure(strip_exist(Tmp0, ExVar, Sort, Body), henkin(Const)),
	ExVar = Const,
	copy_term(Sort, Sort1),
	copy_term((ExFla => (sort(Const,Sort1) & Body)), Henkin_Ax),
	copy_term(Body, ExInstance),
	ensure(get_ref_fof(Sort_Ref, fof(_,_,sort(Const,_),FileInfo,
					 [mptp_info(Nr,Lev,constant,Pos,[Kind,type|_])|_])),
	       henkin(Sort_Ref)),
	Henkin_Fof = fof(Henkin_Ref, axiom, Henkin_Ax, FileInfo,
			 [mptp_info(Nr,Lev,constant,Pos,[Kind,henkin_axiom])]),!.


%% strip_exist(+ExFla, -ExVar, -Sort, -Body)
%%
%% Strip existential quantifier, and return the parts.
%% Handle also negated universal formulas, and top double negations.
%% Throw exception if failure.
strip_exist( ? [(ExVar : Sort) | Srts ] : Fla, ExVar, Sort, Body):- !,
	(Srts = [] -> Body = Fla ; Body = ( ? Srts : Fla )).
strip_exist( ~(~( Fla)), ExVar, Sort, Body):- !, strip_exist( Fla, ExVar, Sort, Body).
strip_exist( ~( ! Q : ( ~( Fla))), ExVar, Sort, Body):- !, strip_exist( ? Q : Fla, ExVar, Sort, Body).
strip_exist( ~( ! Q : Fla), ExVar, Sort, Body):- !, strip_exist( ? Q : ( ~( Fla)), ExVar, Sort, Body).
strip_exist(Fla, _, _, _) :- throw(strip_exist(Fla)).



%%%%%%%%%%%% Constant generalization (abstraction) %%%%%%%%%%%%%%%%%%%%

%% get_consts_and_refs(+RefsIn, +ConstsIn, +AddedConsts, -RefsOut, -ConstsOut)
%%
%% Collect all constants together with their type definitions.
%% Start with a list of references (type defs) and known constants,
%% and recursively add all references and constants mentioned in the
%% type defs.
get_consts_and_refs(RefsIn, ConstsIn, AddedConsts, RefsOut, ConstsOut):-
	get_sec_info_refs(RefsIn, AddedConsts,
			  [mptp_info(_,_,constant,_,[_,type|_])|_], NewRefs),
	([] = NewRefs ->
	    (RefsOut = RefsIn, ConstsOut = ConstsIn)
	;
	    maplist(get_ref_fla, NewRefs, Sorts0),
	    collect_symbols_top(Sorts0, Syms0),
	    sublist(mptp_local_const, Syms0, Consts0),
	    subtract( Consts0, ConstsIn, Consts1),
	    union(ConstsIn, Consts1, AllConsts1),
	    union(RefsIn, NewRefs, AllRefs1),
	    get_consts_and_refs(AllRefs1, AllConsts1, Consts1, RefsOut, ConstsOut)
	).

%% cmp_const_info(-Res, +Fof1, +Fof2)
%%
%% Compare two Fofs (##ASSUME: containining type defs of consts,
%% with proper (and different) info). Const in sublevel is always
%% larger than const on superlevel (##TODO: not consistent with positions!),
%% on the same level, compare just by const number.
%% This ensures that the infos are comparable and different
cmp_const_info(Res, fof(_,_,_,_,[mptp_info(Nr1,Lev1,_,_,_)|_]),
	       fof(_,_,_,_,[mptp_info(Nr2,Lev2,_,_,_)|_])):- !,
	(Lev1 = Lev2 ->
	    (compare(Res,Nr1,Nr2),
		ensure(Nr1 \= Nr2, cmp_const_info(Nr1,Lev1)))
	;
	    (sublevel(Lev1, Lev2) ->
		Res = '>'
	    ;
		(ensure(sublevel(Lev2, Lev1), cmp_const_info(Lev1,Lev2)),
		    Res = '<'))).

%% must not be reached
cmp_const_info(_,I1,I2):- ensure( fail, cmp_const_info(I1,I2)).

insrt_by_info(X, [], [X]).
insrt_by_info(F1, [F2|T], Sorted):-
	cmp_const_info(Res, F1, F2),
	(Res = '<' ->
	    Sorted = [F1,F2|T]
	;
	    (insrt_by_info(F1, T, Sorted1),
		Sorted = [F2|Sorted1])
	).

%% This is an insert sort by mptp_info.
%% All consts must be comparable, and different.
%% Could be done using article_position/2 .
sort_by_const_names([], Sorted, Sorted).
sort_by_const_names([H|T], Sorted, Res):-
	insrt_by_info(H, Sorted, Sorted1),
	sort_by_const_names(T, Sorted1, Res).



%% find the corresponding subst for X, throw exception if not
%% X is assumed to be a local constant
get_const_subst(X,[],_):- !, throw(const_subst(X)).
get_const_subst(X,[Z/Y|_],Z/Y):- X == Z, !.
get_const_subst(X,[_|T],Subst):- get_const_subst(X,T,Subst).

%% apply_const_substs(+Substs,+Fla,-Fla1)
%%
%% applies local constant substitutions to Fla, throwing exception
%% if no substitution for any local const exists

%% end of traversal
apply_const_substs(_,X,X):- var(X),!.

apply_const_substs(Substs,X1,Var1):- atomic(X1), mptp_local_const(X1), !,
	get_const_subst(X1,Substs,X1/Var1).

apply_const_substs(_,X,X):- atomic(X),!.
apply_const_substs(Substs,X1,X2):-
	X1 =.. [H1|T1],
	maplist(apply_const_substs(Substs),T1,T2),
	X2 =.. [H1|T2].

%% make_const_var_substs(+Consts, -Substs).
%%
%% Make new variable substitutions for the constants.
make_const_var_substs([], []).
make_const_var_substs([H|T], [(H/_NewVar)|NewSubsts]):-
	make_const_var_substs(T, NewSubsts).

%% generalize_consts(+In, -Out, -UnivContext, -Subst)
%%
%% Generalize local constants into free (later universally bound) variables.
%% All local consts in +In are replaced with new vars in Out, these vars
%% are collected in UnivContext (with their sorts),
%% -Subst is the substitution which turns -Out into +In again
%% (actually vice versa - it has format: [Constant1/Var1,...])
generalize_consts(In, Out, UnivContext, NewConstSubst):-
	collect_symbols_top(In, Syms0),
	sublist(mptp_local_const, Syms0, Consts0),
	get_consts_and_refs([], Consts0, Consts0, SortRefs, AllConsts),
	length(AllConsts, N),
	ensure(length(SortRefs, N), gen_consts(In,AllConsts,SortRefs)),
	maplist(get_ref_fof, SortRefs, SortFofs),
	sort_by_const_names(SortFofs, [], SortFofs1),
	maplist(arg(3), SortFofs1, SortFlas1),
	ensure( zip_s(sort, SortedConsts,Sorts,SortFlas1),gen_consts),
	zip_s(':', SortedConsts, Sorts, Context1),
	once(make_const_var_substs(SortedConsts, NewConstSubst)),
	apply_const_substs(NewConstSubst, [In,Context1], [Out,UnivContext]).

%%%%%%%%%%%% End of constant generalization %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%% Installation of fraenkel definitions %%%%%%%%%%%%%%%%%%%%%

%% inst_univ_fof([+Fof,+Substs], -Res)
%% apply all the substitutioons in Substs to Fof, assuming that
%% the length of Substs equals the first quantif. prefix, and
%% the prefic contains the insntiated vars; strip this quantif. prefix
inst_univ_fof([X,[]],X):- !.
inst_univ_fof([fof(R,R1,(! [_] : Out),R3,R4),[Cnst/Var]], Res):- !,
	Var = Cnst,
	Res = fof(R,R1,Out,R3,R4).
inst_univ_fof([fof(R,R1,(! [_|Vs] : Out),R3,R4),[Cnst/Var|T]], Res):- !,
	Var = Cnst,
	inst_univ_fof([fof(R,R1,(! Vs : Out),R3,R4), T], Res).

inst_univ_fof(_,_):- throw(inst_univ_fof).

%% assert_fraenkel_def(+File,[+NewSym, +Def], -NewName)
%%
%% create the fof for a given pair [NewSym, Def], and assert.
%% Now handles also choice terms - with different naming.
assert_fraenkel_def(File,[NewSym, Def], NewName):-
	(mptp_fraenkel_sym(NewSym) ->
	 concat_atom(['fraenkel_', NewSym], NewName),
	 Res= fof(NewName, definition, Def, file(File,NewSym),
		  [mptp_info(0,[],fraenkel,position(0,0),[])])
	;
	 ensure(mptp_choice_sym(NewSym), assert_fraenkel_def(File,[NewSym,Def])),
	 concat_atom(['dt_', NewSym], NewName),
	 Res= fof(NewName, axiom, Def, file(File,NewSym),
		  [mptp_info(0,[],o,position(0,0),[ctype])])
	),
	assert(Res),!.

print_clause_id(Id):- clause(X,_,Id), print(X), nl, !.
print_nl(X):- print(X), nl, !.

%% abstract_fraenkels(+Article, +Options, -NewFrSyms, -NewFlaNames)
%%
%% Create definitions for all fraenkel and choice terms in Article,
%% in which the possible local consts are generalized-out;
%% assert these definitions as new fofs, erase the original clauses
%% with fraenkel terms and assert instead of them their versions
%% with fraenkel terms replaced by the new fraenkel functors.
%% Schemes containing fraenkels are saved for possible instantiations
%% in sch_orig_copy/2 .
%% For choice terms, only the type fla is created, there is no definition.
abstract_fraenkels(_Article, Options, _NewFrSyms, _NewFlaNames):- member(opt_USE_ORIG_SCHEMES,Options),!.
abstract_fraenkels(Article, Options, NewFrSyms, NewFlaNames):-
	findall(Id,(fof_file(Article,Id),
		    clause(fof(_,_,Fla,file(_,_),_),_,Id),
		    once(check_if_symbol(Fla, all);check_if_symbol(Fla, the))),Ids), !,
	%% keep original form of schemes with fraenkel in sch_orig_copy/2 -
	%% needed for correct scheme instantiation
	findall([SchRef, SchFla],
		(
		  member(Id,Ids),
		  clause(fof(SchRef,theorem,SchFla,file(_,_),[mptp_info(_,_,scheme,_,_)|_]),_,Id),
		  retractall(sch_orig_copy(SchRef,_)),
		  assert(sch_orig_copy(SchRef, SchFla))
		),
		_),
	%% collect fraenkel and choice infos and put variables into fraenkel and choice flas
	findall([[fof(R,R1,Out,R3,R4),Subst],Info],
		(member(Id,Ids), clause(fof(R,R1,R2,R3,R4),_,Id),
		    (
		      member(opt_NO_FRAENKEL_CONST_GEN, Options),
		      Subst = [], Out2 = R2
		    ;
		      not(member(opt_NO_FRAENKEL_CONST_GEN, Options)),
		      once(generalize_consts(R2, Out1, UnivContext, Subst)),
		      add_univ_context(UnivContext, Out1, Out2)
		    ),
		    all_collect_top(Out2,Out,Info)),
		S1),
	zip(FofSubsts, Infos1, S1),
	append_l(Infos1,Infos),
	%% instantiate fraenkels with their defs, create the defs
	mk_fraenkel_defs_top(Article, Infos, NewFrSyms, Defs),
	%% Defs generally share context vars with the original flas,
	%% and with each other (if multiple fraenkels in one fla);
	%% so we have to give them fresh vars, since the vars in original
	%% flas can be e.g. instantiated back to constants
	maplist(copy_term,Defs,Defs1),
	dbg(dbg_FRAENKELS, checklist(print_nl, Defs1)),
	%% now we can safely instantiate the added vars to constants again
	maplist(inst_univ_fof, FofSubsts, Fofs),
	checklist(erase, Ids),
	checklist(assert, Fofs),
	dbg(dbg_FRAENKELS, checklist(print_nl, Fofs)),
	length(Defs1, NDefs1), length(Ids, NIds),
	format('~w: ~p flas with fraenkel terms, ~p defs created ~n',
	       [Article,NIds,NDefs1]),
	maplist(assert_fraenkel_def(Article), Defs1, NewFlaNames).

abstract_fraenkels_if(Article):- fraenkels_loaded(Article),!.
abstract_fraenkels_if(Article):-
	abstract_fraenkels(Article, [], _, _),
	assert(fraenkels_loaded(Article)),!.


%%%%%%%%%%%% End of Installation of fraenkel definitions %%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%% MoMM export %%%%%%%%%%%%%%%%%%%%%

%% We want the following format for MoMM type files (.typ):
%%
%% for functors:
%% type(Functor(Vars),TypeConstr(Functor(Vars),AdditionalArgs)).
%%
%% for types:
%% type(TypeConstr(Vars),SuperTypeConstr(ArgsOfVars)).
%%
%%
%% type(g1_struct_0(A1), l1_struct_0(g1_struct_0(A1))).
%% type(l2_struct_0(A1), l1_struct_0(A1)).
%% type(g2_struct_0(A1,A2), l2_struct_0(g2_struct_0(A1,A2))).
%% type(u2_struct_0(A1), m1_subset_1(u2_struct_0(A1),u1_struct_0(A1))).
%% type(k1_struct_0(A1,A2,A3), m1_subset_1(k1_struct_0(A1,A2,A3),k1_zfmisc_1(u1_struct_0(A1)))).
%% type(k2_struct_0(A1,A2,A3), m1_subset_1(k2_struct_0(A1,A2,A3),k1_zfmisc_1(u1_struct_0(A1)))).
%% type(k3_struct_0(A1,A2,A3), m1_subset_1(k3_struct_0(A1,A2,A3),k1_zfmisc_1(u1_struct_0(A1)))).
%% type(k4_struct_0(A1,A2,A3), m1_subset_1(k4_struct_0(A1,A2,A3),k1_zfmisc_1(u1_struct_0(A1)))).
%% type(k5_struct_0(A1,A2), m1_subset_1(k5_struct_0(A1,A2),k1_zfmisc_1(u1_struct_0(A1)))).
%% type(k6_struct_0(A1,A2,A3), m1_subset_1(k6_struct_0(A1,A2,A3),k1_zfmisc_1(u1_struct_0(A1)))).
%% type(m1_struct_0(A3,A1,A2), m1_subset_1(A3,u1_struct_0(A1))).
%% type(k7_struct_0(A1,A2,A3), m2_setfam_1(k7_struct_0(A1,A2,A3),u1_struct_0(A1))).
%% type(k8_struct_0(A1,A2,A3), m2_setfam_1(k8_struct_0(A1,A2,A3),u1_struct_0(A1))).
%% type(k9_struct_0(A1,A2,A3), m2_setfam_1(k9_struct_0(A1,A2,A3),u1_struct_0(A1))).

%% mk_momm_typ_files(+OutDirectory)
%%
%% Create MoMM typ files for all MML articles in directory OutDirectory.
%% ##TEST: :- mk_momm_typ_files('00mommtypes/').
mk_momm_typ_files(OutDirectory):-
	declare_mptp_predicates,
	load_mml,
	install_index,
	all_articles(ArticleNames),
	mml_added_spc(Spc),
	append(Spc,ArticleNames,NList),
	checklist(abstract_fraenkels_if, ArticleNames),
	install_index,
	checklist(article_momm_typ_file(OutDirectory), [hidden | NList]).

%% article_momm_typ_file(+OutDirectory,+A)
%%
%% Create MoMM typ file for article A in directory OutDirectory.
article_momm_typ_file(OutDirectory,A):-
	concat_atom([OutDirectory, A, '.', typ], OutFile),
	tell(OutFile),
	repeat,
	(
	 fof_toplevel(A,Id),
	 clause(fof(_Ref,_Role,MptpFla,file(A,_Constr),[MptpInfo|_]),_,Id),
	 MptpInfo = mptp_info(_RelNr,[],ConstrKind,_Pos,[ctype]),
	 constr_type_fla2momm(ConstrKind,MptpFla,MommFla),
	 numbervars(MommFla, 0, _),
	 print(MommFla),
	 write('.'), nl,
	 fail
	;
	 told
	).

%% constr_type_fla2momm(+ConstrKind,+MptpFla,-MommFla)
%%
%% This fails for predicates.
constr_type_fla2momm(ConstrKind,MptpFla,MommFla):-
	(
	 member(ConstrKind,[k,g,u]) ->
	 func_type_fla2momm(MptpFla, MommFla)
	;
	 member(ConstrKind,[m,l]),
	 mode_type_fla2momm(MptpFla, MommFla)
	).

%% func_type_fla2momm(+MptpFla, -MommFla)
%%
%% Create a MoMM functor type fla from MPTP functor type fla.
%% This can fail if the sort is trivial or only attributes.
%% sort(u2_struct_0(_G250), m1_subset_1(u1_struct_0(_G250)))
%% becomes
%% type(u2_struct_0(A1), m1_subset_1(u2_struct_0(A1),u1_struct_0(A1))).
func_type_fla2momm(MptpFla, MommFla):-
	ensure(strip_univ_quant(MptpFla,sort(Term, AttrsAndRadix),_), func_type_fla2momm(MptpFla)),
	constr2list('&', Radix, _ , AttrsAndRadix),
	Radix =.. [ Mode | Args],
	mptp_mode_sym(Mode),
	NewRadix =.. [ Mode, Term | Args],
	MommFla = type(Term, NewRadix).

%% mode_type_fla2momm(+MptpFla, -MommFla)
%%
%% Create a MoMM mode type fla from MPTP mode type fla.
%% This can fail if the supersort is trivial or only attributes.
%% (sort(_G250, l2_struct_0)=>sort(_G250, l1_struct_0))
%% becomes
%% type(l2_struct_0(A1), l1_struct_0(A1)).
%% and
%% (sort(_G278, m2_subset_1(_G250, _G258))=>sort(_G278, m1_subset_1(_G250)))
%% becomes
%% type(m2_subset_1(A3,A1,A2), m1_subset_1(A3,A1)).
mode_type_fla2momm(MptpFla, MommFla):-
	ensure(strip_univ_quant(MptpFla,sort(Var, DefSort) => Sorts,_), mode_type_fla2momm(MptpFla)),
	constr2list('&', sort(Var1, Radix), _ , Sorts),
	ensure((Var == Var1), mode_type_fla2momm(MptpFla)),
	Radix =.. [ Mode | Args],
	mptp_mode_sym(Mode),
	DefSort =.. [ DefMode | DefVars],
	NewDefSort =.. [ DefMode, Var | DefVars],
	NewRadix =.. [ Mode, Var | Args],
	MommFla = type(NewDefSort, NewRadix).


%%%%%%%%%%%% end of MoMM export %%%%%%%%%%%%%%%%%%%%%

%% test uniqueness of references inside Article,
%% creating also scheme instances and abstracting fraenkels
test_refs(Article):-
	mml_dir_atom(MMLDir),
	concat_atom([MMLDir, Article, '.xml2'],File),
	concat_atom([MMLDir, Article, '.dcl2'],DCL),
	concat_atom([MMLDir, Article, '.dco2'],DCO),
	concat_atom([MMLDir, Article, '.the2'],THE),
	concat_atom([MMLDir, Article, '.sch2'],SCH),
	%% remove the ovelapping mml parts first
	retractall(fof(_,_,_,file(Article,_),_)),
	sublist(exists_file,[DCO],ToLoad1),
	load_files(ToLoad1,[silent(true)]),
	consult(File),
	install_index,
	once(assert_sch_instances(Article,Options)),
	install_index,
	abstract_fraenkels(Article, [], _, _),
	install_index,!,

	findall([Fof1,Fof2], (fof_file(Article, Id1),fof_name(X,Id1),fof_name(X,Id2),
				 Id1 < Id2,
				 clause(Fof1,_,Id1),
				 clause(Fof2,_,Id2)),S),
	print(S),nl,length(S,N),print(N),
	retractall(fof(_,_,_,file(Article,_),_)),
	sublist(exists_file,[DCL,DCO,THE,SCH],ToLoad),
	load_files(ToLoad,[silent(true)]),!.


%% test uniqueness of references inside Article (simple version)
test_refs_simple(Article):-
	mml_dir_atom(MMLDir),
	concat_atom([MMLDir, Article,'.xml2'],File),
	consult(File),
	install_index,
	findall([Fof1,Fof2], (fof_name(X,Id1),fof_name(X,Id2),
				 Id1 < Id2, clause(Fof1,_,Id1),
				 clause(Fof2,_,Id2)),S),
	print(S),nl,length(S,N),print(N),
	retractall(fof(_,_,_,file(Article,_),_)),!.

%% Shorter is an ancestor level or equal to Longer
sublevel(Longer,Shorter) :- append(Shorter,_,Longer).

%% Shorter is a (strict) ancestor level of Longer
strict_sublevel(Longer,Shorter) :- append(Shorter,[_|_],Longer).

%% filter out refs with more special level
filter_level_refs(Lev,RefsIn,RefsOut):-
	sort(RefsIn,Refs1),
	findall(Ref,(member(Ref,Refs1),
		     get_ref_fof(Ref,fof(Ref,_,_,_,Info)),
		     Info = [mptp_info(_,Lev1,_,_,_)|_],
		     sublevel(Lev,Lev1)), RefsOut).
%	findall(Ref,(member(Ref,RefsIn),atom_chars(Ref,[C|_]),
%		     member(C,[t,d,l])), Refs1),

%% filter_for_proved_by(+F, +P, +Lev, +MPropKind, +InfKind, +Refs0, -ProvedByRefs)
%%
%% Return refs used for initialising proved_by relation.
filter_for_proved_by(_F, _P, Lev, MPropKind, InfKind, Refs0, ProvedByRefs):-
	(
	  InfKind == mizar_proof ->
	  filter_level_refs(Lev, Refs0, Refs1)
	;
	  (
	    member(MPropKind,[fcluster,ccluster,rcluster,identifyexp]) ->
	    filter_level_refs(Lev, Refs0, Refs1)
	  ;
	    Refs1 = Refs0
	  )
	),
	findall(R, (member(R,Refs1), not(mptp_req_name(R))), ProvedByRefs).


%% filter out refs with this or more special level
get_superlevel_refs(Lev,RefsIn,RefsOut):-
	sort(RefsIn,Refs1),
	findall(Ref,(member(Ref,Refs1),
		     get_ref_fof(Ref,fof(Ref,_,_,_,Info)),
		     Info = [mptp_info(_,Lev1,_,_,_)|_],
		     strict_sublevel(Lev,Lev1)), RefsOut).


%% childern and descendants of a level expressed as atom (for speed)
%% ###TODO: seems that for percases blocks only the level of the percases
%% justification saves this recursion - we should have thesis for
%% each percases block, and all subblocks, with proper levels
at_level_children(At1,Childs):- findall(C, fof_parentlevel(At1,C), Childs).
at_level_descendents(At1,Descs):-
	at_level_children(At1,Childs),
	( Childs = [], Descs = [];
	    Childs \= [],
	    maplist(at_level_descendents,Childs,Ds1),
	    flatten([Childs|Ds1], Descs)).

%% all fof names on this level (block) and below, uses fof_level/2 and
%% fof_parentlevel/2 for speed
get_sublevel_names(Lev,Names):-
	level_atom(Lev, At1),
	at_level_descendents(At1, Descs),
	findall(Name,( member(L1, [At1|Descs]),
		      fof_level(L1, Id),
		      clause(fof(Name,_,_,_,_),_,Id)), Names).

get_sublevel_proved_names(Lev,Names):-
	level_atom(Lev, At1),
	at_level_descendents(At1, Descs),
	findall(Name,( member(L1, [At1|Descs]),
		      fof_level(L1, Id),
		      clause(fof(Name,_,_,_,[_,inference(_,_,_)|_]),_,Id)),
		Names).


%% all fof names on this level (block); uses fof_level/2 and
%% fof_parentlevel/2 for speed
get_thislevel_names(Lev,Names):-
	level_atom(Lev, At1),
	at_level_children(At1, Descs),
	findall(Name,( member(L1, [At1|Descs]),
		      fof_level(L1, Id),
		      clause(fof(Name,_,_,_,_),_,Id)), Names).

%% get only this-level references for Lev from RefsIn
%% the top-level included in RefsIn, which are not references of
%% anything below are included too (should be useful e.g. fro definitional
%% expansions done in this level) - note that some background adding might
%% be needed after that.

%% simplified version - we cannot rely on RefsIn, since
%% lemmas with subproofs are not there
get_thislevel_refs_simple(Lev, _, ThisLevelRefs, []) :-
	get_thislevel_names(Lev, ThisLevelNames),
	findall(Ref,
		(
		  member(Ref,ThisLevelNames),
		  get_ref_fof(Ref,fof(Ref,_,_,_,Info)),
		  Info = [mptp_info(_,Lev,_,_,_),inference(_,_,_) |_]
		),
	       ThisLevelRefs).

%% better (standard) version - takes ThesisExpansions into account
get_thislevel_refs(Lev, RefsIn, ThisLevelRefs, SuperLevelRefs):-
	get_thislevel_names(Lev, ThisLevelNames),
	%% select only justified, collect their references
	findall([Ref,Refs],
		(
		  member(Ref,ThisLevelNames),
		  get_ref_fof(Ref,fof(Ref,_,_,_,Info)),
		  Info = [mptp_info(_,Lev,_,_,_),inference(_,_,Refs)|_]
		),
	       RefRefs),
	zip(ThisLevelRefs,ThisRefRefsLists,RefRefs),
	flatten(ThisRefRefsLists,ThisRefRefs1),
	sort(ThisRefRefs1,ThisRefRefs),
	get_superlevel_refs(Lev,RefsIn,SuperLevelRefs1),
	%% remove all superlevel refs used inside ThisLevelRefs - a bit heuristical
	subtract(SuperLevelRefs1,ThisRefRefs,SuperLevelRefs).



%% get all (also proof-local) symbols used in this proof, and all
%% formula names in the proof. the proof
%% is described by the explicit references and the proof level.
%% Symbols from the references and from all formulas on this level
%% and below are collected.
%% used as input for the fixpoint algorithm
%% NOTE: proof-local symbols cannot be filtered-out here. E.g.
%%   if we filtered-out a locally reconsidered constant, whose
%%   original is outside this proof, we would never reach the original
%%   in the fixpoint algorithm. So we have to pass all symbols to
%%   the fixpoint algorithm, and filter the proof-local flas afterwards.
get_proof_syms_and_flas(RefsIn, PLevel, PSyms, PRefs):-
	get_sublevel_names(PLevel, Names),
	append(RefsIn, Names, AllNames),
	sort(AllNames, PRefs),!,
	maplist(get_ref_syms, PRefs, PSymsL),
	ord_union(PSymsL, PSyms).

%% Create the abs_name table for all local references and constants
%% by checking if their name ends with _Article or not.
%% If not, __Article is appended.
%% Local constants are done by their type declaration - so
%% ##REQUIRE: Local constants always have their type declaration (even if trivial).
%% ##TEST: generate problems for some articles with absolute_locals:- fail., then
%%         generate with absolute_locals, then check with low timelimit that the results
%%         of E prover are the same.
absolutize_locals(Article):-
	abolish(abs_name/2),
	dynamic(abs_name/2),
	atom_concat('_',Article,ArticleSuffix),
	atom_concat('__',Article,ArticleSuffix1),!,
	repeat,
	(
	  fof_file(Article,Id),
	  %% ##NOTE: indexing fof_name on second param makes things very slow,
	  %% hence use clause/3 instead of fof_name/2 here
	  clause( fof(Ref,_,_,_,_),_,Id),
	  (
	    atom_concat(_,ArticleSuffix,Ref) -> true
	  ;
	    atom_concat(Ref,ArticleSuffix1,AbsName),
	    assert(abs_name(Ref,AbsName)),
	    %% register local constants from their types
	    (
	      atom_concat('dt_c',Rest,Ref) ->
	      atom_concat(c,Rest,Constant),
	      atom_concat(Constant,ArticleSuffix1,ConstantAbsName),
	      assert(abs_name(Constant,ConstantAbsName))
	    ;
	      true
	    )
	  ),
	  fail
	;
	  true
	).

%%%%%%%%%%%%%%%%%%%% Problem creation %%%%%%%%%%%%%%%%%%%%
/*
  Master plan for the TSTP export and ATP cross-verification
  of large proofs:

 Example:
  for X1 being T1 st P1(X1) holds
  for X2 being T2 of X1 ex X3 being T3 of X1 st P3(X3,X2)
proof
  let c1 be T1;
  assume A1: P1(c1);
  let c2 be T2 of c1;
  A2: Q1(c1,c2) by Th1;
  reconsider c3 = F1(c1,c2) as T4 of c1 by A2;
  consider c4 being T5 of c3 by Def1;
  A3: Q2(c4) by Th2;
  A4: Q3(c4) by A1,A3, Th3;
  reconsider c5 = c4 as T3 of c1 by A4;
  take c5;
  A5: for X4 being T6 of c1,c2 holds Q4(X4,c5)
  proof
    let c6 be T6 of c1,c2;
    A6: Q5(c6, c5) by Th4;
    thus A7: Q4(c6,c5) by A4,A6, Th5;
  end;
  thus A8: P3(c5,c2) by A5,Th6;
end;

translation:
proof
  fof(dc_c1,henkin_ax1, (sort(c1,t1) => (p1(c1) => ![X2:t2(c1)]:(?[X3:t3(c1)]:p3(X3,X2))) =>
                        (![X1:t1]:(p1(X1) => ![X2:t2(X1)]:(?[X3:t3(X1)]:p3(X3,X2))))).

  fof(dt_c1,assumption, sort(t1,c1)).
  fof(a1, assumption, p1(c1)).
  for(dc_c2,henkin_ax2, (ex X3 being T3 of c1 st P3(X3,c2)) =>
                        (![X2:t2(c1)]:(?[X3:t3(c1)]:p3(X3,X2)))).
  fof(a2,lemma, q1(c1,c2), th1).
  fof(e1,lemma, sort(f1(c1,c2),t4(c1)), a2).
  fof(dc_c3,def, c3 = f1(c1,c2)).
  fof(e2,lemma, ?([X], sort(X,t5(c3))), def1).
  for(dc_c4,henkin_ax4, ?([X], sort(X,t5(c3))) => sort(c4,t5(c3))).
  fof(dt_c4,lemma,sort(c4,t5(c3)),e2,dc_c4).
  fof(a3,lemma, q2(c4), th2).
  fof(a4,lemma, q3(c4), a1,a3,th3).
  fof(e3,lemma, sort(c4,t3(c1)), a4).
  fof(dc_c5,def, c5 = c4).
  take ... just changing thesis
  fof(a5, (![X4:t6(c1,c2)]: q4(X4,c5)), proof(1)).
  proof
    fof(dc_c6,henkin_ax6, q4(c6,c5) => (![X4:t6(c1,c2)]: q4(X4,c5))).
    fof(a6,lemma, q5(c6,c5), th4).
    fof(a7,lemma, q4(c6,c5), a4,a6).
    ... now eliminating the const:
    fof(a5, (![X4:t6(c1,c2)]: q4(X4,c5)), dc_c6,a7).
  end;
  fof(a8, p3(c5,c2), a5,th6).
  ... now justifing the subsequent theses bottom-up:
  fof(r_take, ?[X3:t3(c1)]:p3(X3,c2), e3,a8).
  fof(r_dc_c2, ![X2:t2(c1)]:(?[X3:t3(c1)]:p3(X3,X2)), r_take, dc_c2).
  fof(r_a1, p1(c1) => ![X2:t2(c1)]:(?[X3:t3(c1)]:p3(X3,X2)), discharge(a1), r_dc_c2).
  fof(r_dt_c1, sort(c1,t1) => (p1(c1) => ![X2:t2(c1)]:(?[X3:t3(c1)]:p3(X3,X2))), discharge(dt_c1), r_a1).
  fof(r_dc_c1, ![X1:t1]:(p1(X1) => ![X2:t2(X1)]:(?[X3:t3(X1)]:p3(X3,X2))), r_dt_c1, dc_c1).
end;

inference DAG:
  axioms:    th2 th3 th4 dc_c6 th6  dc_c2   dc_c1 def1 dc_c4 dc_c3 dc_c5 dc_c6
              |   |   /   /     /      /      /    |     /
      ass:a1  a3 /  a6   /     /      /      /     e2   /
            \ | /  /    /     /      /      /        \ /
              a4  /    /     /      /      /        dt_c4
              | \/    /     /      /      /
             e3 a7   /     /      /      /
              \   \ /     /      /      /
               \  a5     /      /      /
                \   \   /      /      /
                 \   a8       /      /
                  \   |      /      /
                   r_take   /      /
                         \ /      /
                       r_dc_c2   /
                          |     /
              disch(a1): r_a1  /
                          |   /
                        r_dc_c1

*/

%% get_smaller_includes(+Article, +AllIncludes, -SmallerIncludes)
%%
%% Select into SmallerIncludes all articles from AllIncludes
%% smaller than Article, sort them by MML order.
get_smaller_includes(Article, AllIncludes, SmallerIncludes):-
	sublist(cmp_articles_in_mml_order(>, Article), AllIncludes, SmallerIncludes0),
	sort_articles_in_mml_order(SmallerIncludes0, [], SmallerIncludes).

%% needed_environ(+Article, +AddedNonMML, -Articles)
%%
%% AddedNonMML (usually empty list) can be used to pass extra nonMML
%% articles that will be appended to MML in that order for numbering.
needed_environ(Article, AddedNonMML, Articles):-
	theory(Article, Theory),
	member(constructors(Constrs),Theory),
	member(registrations(Regs),Theory),
	member(requirements(Reqs),Theory),
	member(definitions(Defs),Theory),
	member(theorems(Thms),Theory),
	member(schemes(Schms),Theory),
	mml_version([_,_,V]),	
	(V < 1172 ->
	 Equals = [],
	 Expands = []
	;
	 member(equalities(Equals),Theory),
	 member(expansions(Expands),Theory)
	),	 
	union1([Constrs,Regs,Reqs,Defs,Thms,Schms,Equals,Expands],[],Articles0),
	sort_articles_in_mml_order(Articles0, AddedNonMML, Articles).

%% ##TEST: :- first100(L),print_env_deps(mml1,L).
%% ##TEST: :- print_env_deps(mmldeps).
%%
print_env_deps(File):-
	all_articles(L),!,
	print_env_deps(File,L).

% print_env_deps(+File,+Articles)
%
% Print the environmental dependencies of Atricles into a File in the dot notation
print_env_deps(File,Articles):-
	declare_mptp_predicates,
	load_mml,
	install_index,
	tell(File),
	write('strict digraph mml {'),
	nl,!,
	(
	 member(A,Articles),
	 needed_environ(A,[],As),
	 once(findall(d,(member(R1,As),write(R1),format(" -> "),write(A),format(";"),nl),_)),
	 fail
	;
	 write('}'),
	 nl,
	 told
	).



%% ensure that fraenkels are abstracted in all prerequisities
%% of Article (changes the fraenkels_loaded/1 predicate).
%% calling install_index after this is a good idea
abstract_prereq_fraenkels(Article, Options):- member(opt_USE_ORIG_SCHEMES,Options),!.
abstract_prereq_fraenkels(Article, Options):-
	(member(opt_ADDED_NON_MML(AddedNonMML),Options) ->
	    Added = AddedNonMML
	;
	    Added = []
	),
	needed_environ(Article, Added, Prereqs),
	checklist(abstract_fraenkels_if, Prereqs).

%% should be used only right after loading a full article File
%% current usage is for determining clusters' area of validity in fixpoint
install_article_positions(File):-
	abolish(article_position/2),
	dynamic(article_position/2),
	flag(a_pos,_,0),
	repeat,
	(
	  fof(Ref1,_,_,file(File,_),_),
	  flag(a_pos, APos, APos + 1),
	  assert(article_position(Ref1, APos)),
	  fail
	;
	  true
	),!.

%% load_proper_article(+Article,+Options,-PostLoadFiles)
%%
%% Load all article reasoning parts, create the MPTP-added stuff
%% like fraenkel defs, etc., prepare the problem directory and return
%% it, and return list of files that will have to be reloaded,
%% after retractall(fof(_,_,_,file(Article,_),_)), is run after
%% the main processing.
load_proper_article(Article,Options,PostLoadFiles):-
	(
	  member(opt_NON_MML_DIR(NonMMLDir),Options) ->
	  Dir = NonMMLDir
	;
	  mml_dir_atom(Dir)
	),
	concat_atom([Dir, Article, '.xml2'],File),
	concat_atom([Dir, Article, '.dcl2'],DCL),
	concat_atom([Dir, Article, '.dco2'],DCO),
	concat_atom([Dir, Article, '.the2'],THE),
	concat_atom([Dir, Article, '.sch2'],SCH),
	concat_atom([Dir, Article, '.err2'],ERR),
	retractall(miz_errors(_)),
	(
	  member(opt_LOAD_MIZ_ERRORS,Options) ->
	  sublist(exists_file,[ERR],ERR1),
	  load_files(ERR1,[silent(true)])
	;
	  true
	),
	%% remove the ovelapping mml parts first
	retractall(fof(_,_,_,file(Article,_),_)),
	retractall(fraenkels_loaded(Article)),
	%% ###TODO: this willbe no longer needed as the dco is in xml
	%% commented now - works with MML 1103 and could caues trouble with 1011
	%% uncomment for old versions
	% sublist(exists_file,[DCO],ToLoad1),
	% load_files(ToLoad1,[silent(true)]),
	consult(File),
	assert_arith_evals(_),
	assert_arith_reqs(_),
	install_article_positions(Article),
	install_index,
	once(assert_sch_instances(Article,Options)),
	install_index,
	abstract_fraenkels(Article, Options, _, _),
	abstract_prereq_fraenkels(Article, Options),
	install_index,
	(
	  member(opt_DBG_ART_POS,Options) ->
	  findall(bla,
		  (
		    fof(RefL1,_,_,_,_),
		    article_position(RefL1,PosR1),
		    print_nl([RefL1,PosR1])
		  ),
		  _Foo)
	;
	  true
	),
	(
	  member(opt_DBG_LEVS_POS,Options) ->
	  findall([FL1,RefL1,PosR1],
		  (
		    fof_level(FL1,IdL1),
		    clause(fof(RefL1,_,_,_,_),_,IdL1),
		    article_position(RefL1,PosR1)
		  ),
		  FLevs),
	  print(FLevs)
	;
	  true
	),
	sublist(exists_file,[DCL,DCO,THE,SCH],PostLoadFiles).

%% prepare_for_article(+Article,+Options,-Dir,-PostLoadFiles)
%%
%% Load all article reasoning parts, create the MPTP-added stuff
%% like fraenkel defs, etc., prepare the problem directory and return
%% it, and return list of files that will have to be reloaded,
%% after retractall(fof(_,_,_,file(Article,_),_)), is run after
%% the main processing.
prepare_for_article(Article,Options,Dir,PostLoadFiles):-
	load_proper_article(Article,Options,PostLoadFiles),
	(
	  member(opt_NON_MML_DIR(NonMMLDir),Options) ->
	  concat_atom([NonMMLDir,'/problems/',Article,'/'],Dir)
	;
	  concat_atom(['problems/',Article,'/'],Dir)
	),
	(exists_directory(Dir) -> (string_concat('rm -r -f ', Dir, Command),
				      shell(Command)); true),
	make_directory(Dir).

%% make nd problems for an Article
%% generating problems for all MPTP Challenge articles:
%% ##TEST: :- declare_mptp_predicates,load_mml,install_index,
%%            L=[ compts_1, connsp_2, enumset1, filter_1, finset_1, funct_1, funct_2,
%%		lattice3, lattices, mcart_1, orders_2, ordinal1, pre_topc, relat_1,
%%		relset_1, setfam_1, subset_1, tex_2, tmap_1, tops_1, tops_2, waybel_0,
%%		waybel_7, waybel_9, wellord1, wellord2, xboole_0, xboole_1, yellow_0,
%%		yellow_1, yellow19, yellow_6, zfmisc_1], member(A,L),
%%            mk_article_nd_problems(A,_,[opt_REM_SCH_CONSTS,opt_MK_TPTP_INF, opt_ADD_INTEREST]),fail.
%%
%% generate GDV derivations for 20 articles for CASC batch division:
%% (first abstract all fraenkels, to keep them consistent):
%% ##TEST:  declare_mptp_predicates, load_mml, install_index, all_articles(ArticleNames),
%%          checklist(abstract_fraenkels_if, ArticleNames), install_index, !,
%%          L=[autgroup,closure3,conlat_2,filter_2,isocat_2,latsubgr,latsum_1,lopclset,
%%	       relset_2,rmod_5,tsp_2,waybel22,waybel33,waybel34,yellow17,yoneda_1], member(A,L),
%%	    mk_article_nd_problems(A,_,[opt_REM_SCH_CONSTS,opt_MK_TPTP_INF, opt_ADD_INTEREST]),fail.
%%
%% generate GDV derivations for all articles:
%% ##TEST: :- declare_mptp_predicates,load_mml,install_index,all_articles(L),!, member(A,L),
%%            mk_article_nd_problems(A,_,[opt_REM_SCH_CONSTS,opt_MK_TPTP_INF, opt_ADD_INTEREST]),fail.
mk_article_nd_problems(Article,_Kinds,Options):-
	prepare_for_article(Article,Options,Dir,PostLoadFiles),
	assert_henkin_axioms(Article,[]),
	install_index,
	%% create the table of local-to-global names if absolute_locals
	(absolute_locals -> absolutize_locals(Article); true),

	repeat,(mk_nd_problem(_,Article,Dir,Options),fail; !,true),
	retractall(fof(_,_,_,file(Article,_),_)),
	load_files(PostLoadFiles,[silent(true)]).


%% Kinds is a list [InferenceKinds, PropositionKinds | Rest]
%% possible InferenceKinds are now [mizar_by, mizar_from, mizar_proof]
%% possible PropositionKinds are now [theorem, scheme,cluster,fcluster,ccluster,rcluster,identifyexp,top_level_lemma, sublemma]
%% Rest is a list now only possibly containing snow_spec, problem_list and subproblem_list.
%%
%% The fraenkeldef creation for the loaded MML is done on demand,
%% by looking at article's theory, and remembering which fraenkels
%% are done at a special predicate fraenkels_loaded/1.
mk_article_problems(Article,Kinds,Options):-
	prepare_for_article(Article,Options,Dir,PostLoadFiles),
	%% create the table of local-to-global names if absolute_locals
	(absolute_locals -> absolutize_locals(Article); true),
	(member(opt_ARTICLE_AS_TPTP_AXS,Options) ->
	 article2tptp_include(Dir,Article)
	;
	 true
	),

	%% now take care of possible subproblem_list option -
	%% create a problem_list from all local mizar_proof references which have the inference slot;
	%% to get all lemmas, we cannot rely on the inference slot, since it only
	%% contains those proved by simple justification!
	(
	  member(subproblem_list(PList), Kinds) ->
	  findall(Ref,
		  (
		    member(R1,PList),
		    get_ref_fof(R1,fof(R1,_,_,_,[_,inference(mizar_proof,InferInfo,_) |_])),
		    member(proof_level(Lev), InferInfo),
		    get_sublevel_proved_names(Lev,LRefs),
		    member(Ref, LRefs)
		  ),
		  AllRefs),
	  append(PList, AllRefs, AllProbs1),
	  sort(AllProbs1, AllProbs),
	  append(Kinds,[problem_list(AllProbs)],Kinds1)
	;
	  Kinds1 = Kinds
	),

	%% expand cluster into fcluster,ccluster,rcluster,identifyexp
	Kinds1 = [InferenceKinds,PropositionKinds|Rest],
	(member(cluster,PropositionKinds) ->
	    delete(PropositionKinds,cluster,PKinds1),
	    union([fcluster,ccluster,rcluster,identifyexp],PKinds1,PKinds2),
	    Kinds2 = [InferenceKinds,PKinds2|Rest]
	;
	    Kinds2 = Kinds1
	),

	repeat,(mk_problem(_,Article,Dir,Kinds2,Options),fail; !,true),

	%% retract current file but return mml parts,
	retractall(fof(_,_,_,file(Article,_),_)),
	load_files(PostLoadFiles,[silent(true)]).
	%% abstracting here would not work, since the index is broken
	%% at this moment
%	abstract_fraenkels(Article, [], _, _),
%	assert(fraenkels_loaded(Article)).


%% create proving problem for a given proposition,
%% (P does not have to be instantiated), store it in file Prefix.P
%% propositions only from the current article can be loaded -
%% - otherwise change their naming
%% mk_problem(?P,+F,+Prefix,+Kinds,+Options)
%% possible InferenceKinds are now [mizar_by, mizar_proof, mizar_from]
%% possible PropositionKinds are now: [theorem,scheme,fcluster,ccluster,rcluster,identifyexp,
%% top_level_lemma, sublemma] (not that just cluster is not allowed here - has to be expanded above).
%% Rest is now checked for containing snow_refs and problem_list([...]).
%% The snow_spec of P have to be available in predicate
%% snow_spec(P,Refs) and they are used instead of the Mizar refs,
%% and InfKind is set to mizar_by. The problem_list([...]) is used for limiting
%% the problems generated (P must be member of it).
%% see opt_available/1 for Options.
%% ###TODO: currently does not handle reconsidered const's type proof, since
%%          the item_kind is not proposition (but constant)
%%          also note that fixpoint is hardly needed for that inference
mk_problem(P,F,Prefix,[InferenceKinds,PropositionKinds|Rest],Options):-
	mk_problem_data(P,F,Prefix,[InferenceKinds,PropositionKinds|Rest],
			Options, Outfile,Line,Col,AllRefs),
	print_problem(P,F,[InferenceKinds,PropositionKinds|Rest],
		      Options, Outfile,Line,Col,AllRefs).

%% mk_problem_data(?P,+F,+Prefix,+[InferenceKinds,PropositionKinds|Rest],+Options,
%%		   -Outfile,-Line,-Col,-AllRefs)
%%
%% internal version for mk_problem, useful if the explicit reference list is needed
mk_problem_data(P,F,Prefix,[InferenceKinds,PropositionKinds|Rest],Options,
		Outfile,Line,Col,AllRefs):-
	theory(F, _Theory),
	member(InfKind0,InferenceKinds),
	member(PropKind,PropositionKinds),
	(
	  member(PropKind,[theorem,scheme,fcluster,ccluster,rcluster,identifyexp]),
	  MPropKind = PropKind
	;
	  not(member(PropKind,[theorem,scheme,fcluster,ccluster,rcluster,identifyexp])),
	  MPropKind = proposition,
	  (
	    PropKind = top_level_lemma,
	    Lev = []
	  ;
	    PropKind \= top_level_lemma
	  )
	),
	fof(P,_,_Fla,file(F,_),[mptp_info(_Nr,Lev,MPropKind,position(Line0,Col0),Item_Info)
			       |Rest_of_info]),

	(member(MPropKind,[fcluster,ccluster,rcluster,identifyexp]) ->
	    ensure(Item_Info = [proof_level(_), Justification |_], cluster(Item_Info)),
	    (Justification = correctness_conditions([Correctness_Proposition1|_]) ->
		Correctness_Proposition1 =.. [_Correctness_Condition_Name1, Corr_Proposition_Ref1],
		fof(Corr_Proposition_Ref1,_,_CPFla,file(F,Corr_Proposition_Ref1),
		    [_CP_Mptp_info, Inference_info|_])
	    ;
		%% forged inference for "strict" rcluster justified by the aggregate type
		ensure(Justification = inference(mizar_by,_,_), cluster(Justification)),
		Inference_info = Justification
	    )
	;
	    Rest_of_info = [Inference_info|_]
	),


	(
	  member(snow_spec, Rest) ->
	  snow_spec(P, Refs0),
	  InfKind = mizar_by,
	  Line = Line0,
	  Col = Col0
	;
	  Inference_info = inference(InfKind0,InfInfo,Refs0),
	  InfKind = InfKind0,
	  %% inference's position can differ from proposition's
	  (
	    member(position(Line,Col), InfInfo) -> true
	  ;
	    Line = Line0,
	    Col = Col0
	  )
	),

	%% filter if problem_list given (problem names or positions like pos(78,22))
	(
	  member(problem_list(PList), Rest) ->
	  (
	    member(P,PList) -> true
	  ;
	    member(pos(Line,Col), PList)
	  )
	;
	  true
	),

	(
	  member(opt_LINE_COL_NMS, Options) ->
	  concat_atom([Prefix,F,'__',Line,'_',Col],Outfile)
	;
	  (abs_name(P,AbsP) -> true; AbsP = P),
	  (
	    member(opt_CONJECTURE_NMS, Options) ->
	    concat_atom([Prefix,AbsP],Outfile)
	  ;
	    concat_atom([Prefix,F,'__',P],Outfile)
	  )
	),
	% fof_name(P, Id1),
	%% this is used to limit clusters to preceding clauses from the nitial file;
	%% note that using the ordering of clauses in the initial file
	%% can be quite fragile (e.g. adding of frankels and scheme instances,...)
	%% OK, this is done safely now with article_position, nth_clause/3 was very slow
	ensure(article_position(P,Pos1), throw(article_position1(P))),
	(
	  member(opt_NO_EX_BG_FLAS, Options) ->
	  InfKind1 = mizar_no_existence
	;
	  InfKind1 = InfKind
	),
%	filter_level_refs(Lev,Refs0,Refs),

	(
	  member(opt_ALLOWED_REF_INFO, Options) ->
	  get_preceding_article_refs(F, P, AllowedLocalRefs),
	  concat_atom([Outfile,'.allowed_local'], AllowLocalSpecFile),
	  tell(AllowLocalSpecFile),
	  print(allowed_local(P,AllowedLocalRefs)),
	  write('.'),nl,
	  told
	;
	  true
	),

	(
	  member(opt_PROVED_BY_INFO, Options) ->
	  filter_for_proved_by(F, P, Lev, MPropKind, InfKind, Refs0, ProvedByRefs),
	  concat_atom([Outfile,'.proved_by0'], ProvedByFile),
	  tell(ProvedByFile),
	  print(proved_by(P,ProvedByRefs)),
	  write('.'),nl,
	  told
	;
	  true
	),
	

	%% Compute references into AllRefs.
	%% This gets a bit tricky for cluster registrations - we need the original
	%% formula (P), but use the PLevel for collecting the refs, and finally filter
	%% using the original cluster's level (Lev).
	(
	  InfKind == mizar_proof,
	  ensure(member(proof_level(PLevel), InfInfo), inf_info(InfInfo,PLevel)),
	  (member(opt_LEVEL_REF_INFO, Options) ->
	      get_thislevel_refs(PLevel, Refs0, ThisLevelRefs, SuperLevelRefs),
	      concat_atom([Outfile,'.refspec'], RefSpecFile),
	      tell(RefSpecFile),
	      print(refspec(P,ThisLevelRefs,SuperLevelRefs)),
	      write('.'),nl,
	      told
	  ;
	      true
	  ),
	  get_proof_syms_and_flas([P|Refs0], PLevel, PSyms, PRefs),
	  dbg(dbg_LEVEL_REFS, format('Refs for ~w bef. filtering: ~w~n', [P,PRefs])),
	  Syms1 = PSyms,
	  once(fixpoint(F, [Pos1, Lev], InfKind1, PRefs, [], Syms1, AllRefs0)),
	  dbg(dbg_LEVEL_REFS, format('Refs for ~w after fixpoint: ~w~n', [P,AllRefs0])),
	  %% incorrect, but needs handling of fraenkels and sch_insts
	  filter_level_refs(Lev,AllRefs0,AllRefs),
	  dbg(dbg_LEVEL_REFS, format('Refs for ~w after level filtering: ~w~n', [P,AllRefs]))
	;
	  InfKind \== mizar_proof,
	  Refs = Refs0,
	  maplist(get_ref_syms, [P|Refs], SymsL0),
	  ord_union(SymsL0,Syms0),
	  Syms1 = Syms0,
	  %% ###TODO: for reconsidered type, following is enough instead of fixpoint
%	  AllRefs1 = [P|Refs]
	  once(fixpoint(F, [Pos1, Lev], InfKind1, [P|Refs], [], Syms1, AllRefs1)),
	  %% if we used the correctness proposition for computing references of cluster
	  %% registrations, we have to filter using the cluster's level
	  (member(MPropKind,[fcluster,ccluster,rcluster,identifyexp]) ->
	      filter_level_refs(Lev,AllRefs1,AllRefs)
	  ;
	      AllRefs = AllRefs1
	  )
	).


%% print_problem(+ProblemName,+Article,[+InferenceKinds,+PropositionKinds|+Rest],+Options,
%%               +Outfile,+Line,+Column,+AllRefs)
%%
%% Print problem ProblemName into OutFile, doing sort transformation.
print_problem(P,F,[_InferenceKinds,_PropositionKinds|Rest],Options,
	      Outfile,Line,Col,AllRefs):-
	(member(opt_PRINT_PROB_PROGRESS,Options) ->
	    format('% Mizar problem: ~w,~w,~w,~w ~n', [P,F,Line,Col])
	;
	    true
	),
	tell(Outfile),
	format('% Mizar problem: ~w,~w,~w,~w ~n', [P,F,Line,Col]),
	(member(snow_spec, Rest) ->
	    format('% Using references advised by SNoW ~n', [])
	;
	    true
	),
	(member(opt_PROB_PRINT_FUNC(Print_Func),Options) ->
	    call(Print_Func, P, AllRefs, Options)
	;
	    print_refs(P, AllRefs, Options)
	),
	told.


print_ref_as_axiom(Options, Q):-
	get_ref_fof(Q, fof(Q,_Q1,Q2,Q3,Q4)),
	(
	 member(opt_SORT_TRANS_FUNC(Transf_Func),Options) ->
	 call(Transf_Func, Q2, SR2)
	;
	 sort_transform_top(Q2,SR2)
	),
	numbervars([SR2,Q3,Q4],0,_),
	Status = axiom, QQ3 = Q3, QQ4= [],
	(member(opt_TPTP_SHORT,Options) ->
	    print(fof(Q,Status,SR2))
	;
	    (member(opt_MK_TPTP_INF,Options) ->
		print(fof(Q,Status,SR2,QQ3,QQ4))
	    ;
		print(fof(Q,Status,SR2,Q3,Q4))
	    )
	),
	write('.'),
	nl.

print_ref_as_conjecture(Options, ProperRefs1, Q):-
	get_ref_fof(Q, fof(Q,_Q1,Q2,Q3,Q4)),
	(
	 member(opt_SORT_TRANS_FUNC(Transf_Func),Options) ->
	 call(Transf_Func, Q2, SR2)
	;
	 sort_transform_top(Q2,SR2)
	),
	numbervars([SR2,Q3,Q4],0,_),
	Status = conjecture,
	QQ3 = inference(mizar_bg_added,[status(thm)],ProperRefs1),
	QQ4 = [Q3],
	(member(opt_TPTP_SHORT,Options) ->
	    print(fof(Q,Status,SR2))
	;
	    (member(opt_MK_TPTP_INF,Options) ->
		print(fof(Q,Status,SR2,QQ3,QQ4))
	    ;
		print(fof(Q,Status,SR2,Q3,Q4))
	    )
	),
	write('.'),
	nl.

print_refs(Conjecture, AllRefs, Options):-
	delete(AllRefs, Conjecture, ProperRefs1),
	print_ref_as_conjecture(Options, ProperRefs1, Conjecture),
	checklist(print_ref_as_axiom(Options), ProperRefs1).

get_transformed_fla(Q,Fla):-
	get_ref_fof(Q, fof(Q,_Q1,Q2,Q3,Q4)),
	sort_transform_top(Q2,Fla),
	numbervars([Fla,Q3,Q4],0,_).

get_transformed_fla_thf(Q,Fla):-
	get_ref_fof(Q, fof(Q,_Q1,Q2,Q3,Q4)),
	sort_transform_thf_top(Q2,Fla),
	numbervars([Fla,Q3,Q4],0,_).

%% fold(+BinConstr, +Top, +List , -Term)
%%
%% Replace cons and nil with BinConstr and Top (initially the top functor), creating Term.
foldl(_BinConstr, Top, [], Top).
foldl(BinConstr, Top, [H|T],Res):-
	Tmp =.. [BinConstr, Top, H],
	foldl(BinConstr, Tmp, T, Res).


fill(_,0,[]):- !.
fill(X,N,[X|L1]):- !, N1 is N - 1, fill(X,N1,L1).


%% list2constr(+BinConstr, +Nil, +List , -Term)
%%
%% Replace cons and nil with BinConstr and Nil, creating Term.
list2constr(_BinConstr, Nil, [], Nil).
list2constr(BinConstr, Nil, [H|T],Res):-
	list2constr(BinConstr, Nil, T, Tmp),
	Res =.. [BinConstr, H, Tmp].

%% constr2list(+BinConstr, -Last, -List , +Term)
%%
%% While BinConstr, replace with cons creating List
%% (having Last element).
%% Order of params is compatible with list2constr.
constr2list(BinConstr, Last, [H,H1|T],Res):-
	Res =.. [BinConstr, H, Tmp],!,
	constr2list(BinConstr, Last, [H1|T], Tmp).
constr2list(_BinConstr, T, [T], T):- !.

% destruct also heads
constr2list2(BinConstr, Last, List,Res):-
	Res =.. [BinConstr, H, Tmp],!,
	constr2list2(BinConstr, _, H0, H),
	constr2list2(BinConstr, Last, List2, Tmp),
	append(H0,List2,List).
constr2list2(_BinConstr, T, [T], T):- !.


print_refs_as_one_fla(Conjecture, AllRefs, _Options):-
	delete(AllRefs, Conjecture, ProperRefs1),
	maplist(get_transformed_fla, ProperRefs1, ProperRefs2),
	list2constr(&, $true, ProperRefs2, AxiomsConjunction),
	Q = Conjecture,
	get_ref_fof(Q, fof(Q,_Q1,Q2,Q3,Q4)),
	sort_transform_top(Q2,SR2),
	numbervars([SR2,Q3,Q4],0,_),
	print(fof(Q,conjecture,(AxiomsConjunction => SR2),Q3)),
	write('.'),
	nl.


%%%%%%%%%%%%%%%%%%%% THF translation %%%%%%%%%%%%%%%%%%%%

% print_func_decl_thf(+X/A):-
%
% declare function types as $i > ... > $i
% numbers get the "n" prefix
print_func_decl_thf(X/A):-
	(number(X) -> concat_atom([n,X],X1); X1 = X),
	concat_atom([X1,'_type'],Name),
	A1 is A + 1,
	fill('$i',A1,List),
	concat_atom(List, ' > ', Decl),
	print(thf(Name,type,( X : Decl))),
	write('.'),
	nl.

% print_pred_decl_thf(+X/A):-
%
% declare predicate types as $i > ... > $i > $o
% attributes and modes get one more arg
% we do not print r2_hidden because it's included and declared in our THF axioms
print_pred_decl_thf(r2_hidden/_):- !.
print_pred_decl_thf(X/A):-
	concat_atom([X,'_type'],Name),
	(mptp_attr_or_mode_sym(X) -> A1 is A + 1; A1 = A),
	fill('$i',A1,List),
	append(List,['$o'],List1),
	concat_atom(List1, ' > ', Decl),
	print(thf(Name,type,( X : Decl))),
	write('.'),
	nl.




%% ##TEST: declare_mptp_predicates,load_mml,install_index,all_articles(AA),checklist(abstract_fraenkels_if, AA),!,member(A,[xboole_0]),time(mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma, sublemma] ],[opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_LINE_COL_NMS,opt_PROB_PRINT_FUNC(print_refs_in_thf)])),fail.

%% top level function for printing problems in thf - still needs to split the axioms and conjecture
print_refs_in_thf(Conjecture, AllRefs, _Options):-
	delete(AllRefs, Conjecture, ProperRefs01),
	replace_sch_instances(ProperRefs01,SchRefs0,ProperRefs1),
	sort(SchRefs0, SchRefs),
%	sort([s13_fraenkel,s24_fraenkel|SchRefs0], SchRefs),
	maplist(get_ref_fla, SchRefs, SchFofs),
	maplist(get_ref_fla, [Conjecture|ProperRefs1], [ConjFof|ProperFofs]),

%	maplist(get_ref_fla, AllRefs, AllFofs),
%	get_func_pred_syms_thf(AllFofs,F,P),

	%% avoids "f" and "p" in scheme refs
	get_func_pred_syms_thf(SchFofs,F0,P0),
	%% does not avoid "f" and "p" in conjectures - here it is skolemized
	get_func_pred_syms([ConjFof|ProperFofs],F1,P1),
	union(F0,F1,F),
	union(P0,P1,P),
	print_include_directive('SET010^0'),
	checklist(print_func_decl_thf,F),
	checklist(print_pred_decl_thf,P),
	maplist(print_ref_in_thf(1,axiom),ProperRefs1),
	%% the schemes are printed with quantification over "f" and "p"
	maplist(print_ref_in_thf(0,axiom),SchRefs),
	print_ref_in_thf(1,conjecture,Conjecture).


%% probably old and unused
%% top level function for printing problems in thf - still needs to split the axioms and conjecture
print_refs_in_thf_old(Conjecture, AllRefs, _Options):-
	delete(AllRefs, Conjecture, ProperRefs1),
	maplist(get_ref_fla, AllRefs, AllFofs),
	get_func_pred_syms(AllFofs,F,P),
	checklist(print_func_decl_thf,F),
	checklist(print_pred_decl_thf,P),
	maplist(get_transformed_fla_thf, ProperRefs1, ProperRefs2),
	list2constr(&, $true, ProperRefs2, AxiomsConjunction),
	Q = Conjecture,
	get_ref_fof(Q, fof(Q,_Q1,Q2,Q3,Q4)),
	sort_transform_thf_top(0,Q2,SR2),
	numbervars([SR2,Q3,Q4],0,_),
	print(thf(Q,conjecture,(AxiomsConjunction => SR2),Q3)),
	write('.'),
	nl.

% scheme_instance_parent(+SchInstName,-SchName)
%
% return the parent scheme of a scheme instance
scheme_instance_parent(SchInstName,SchName):-
	atom_chars(SchInstName,C),
	once(append([[s],Num,['_'|_]],C)),
	name(N,Num),
	number(N),
	once(append(C1,['_','_'|_],C)),
	atom_chars(SchName,C1).
 
replace_sch_instances([],[]).
replace_sch_instances([H|T],[H1|T1]):- scheme_instance_parent(H,H1),!,replace_sch_instances(T,T1).
replace_sch_instances([H|T],[H|T1]):- replace_sch_instances(T,T1).


% replace_sch_instances(+OrigNames,-ReplacedNames,-Kept).
replace_sch_instances([],[],[]).
replace_sch_instances([H|T],[H1|T1],T2):- scheme_instance_parent(H,H1),!,replace_sch_instances(T,T1,T2).
replace_sch_instances([H|T],T1,[H|T2]):- replace_sch_instances(T,T1,T2).



	
% probably unused now
print_refs_in_as_one_fla_thf(Conjecture, AllRefs, _Options):-
	delete(AllRefs, Conjecture, ProperRefs1),
%	replace_sch_instances(ProperRefs01,ProperRefs1),
	maplist(get_ref_fla, AllRefs, AllFofs),
	get_func_pred_syms_thf(AllFofs,F,P),
	checklist(print_func_decl_thf,F),
	checklist(print_pred_decl_thf,P),
	maplist(get_transformed_fla_thf, ProperRefs1, ProperRefs2),
	list2constr(&, $true, ProperRefs2, AxiomsConjunction),
	Q = Conjecture,
	get_ref_fof(Q, fof(Q,_Q1,Q2,Q3,Q4)),
	sort_transform_thf_top(0,Q2,SR2),
	numbervars([SR2,Q3,Q4],0,_),
	print(thf(Q,conjecture,(AxiomsConjunction => SR2),Q3)),
	write('.'),
	nl.


add_sch_sorts_antecedents(SchRef,SchFla,SortedNewFla):-
	get_ref_syms(SchRef,Syms),
	sublist(mptp_sch_func_sym,Syms,FSyms),
	findall(SortFla, (member(Sec1,FSyms), fof_section(Sec1,Id),
			  clause(fof(_,_,SortFla,file(_,Sec1), _),_,Id)), SortFlas),
	( SortFlas = [] -> SortedNewFla = SchFla
	;
	  list2constr(&,$true,SortFlas,Antecedent),
	  SortedNewFla = (Antecedent => SchFla)
	).


% print_ref_in_thf(+NoGen,+Role,+Q)
%
% NoGen is 0 or 1. 0 is used for printing scheme refs.
print_ref_in_thf(NoGen,Role,Q):-
	get_ref_fof(Q, fof(Q,_Q1,Q2,Q3,Q4)),
	(NoGen == 0 ->
	 add_sch_sorts_antecedents(Q,Q2,NewQ2)
	;
	 NewQ2 = Q2
	),
	sort_transform_thf_top(NoGen,NewQ2,SR2),
	numbervars([SR2,Q3,Q4],0,_),
	print(thf(Q,Role,SR2,Q3)),
	write('.'),
	nl.
	


%%%%%%%%%%%%%%%%%%%% End of THF translation %%%%%%%%%%%%%%%%%%%%


%% get_include_refs(+Refs, -IncludeRefs, -NonIncludeRefs, -IncludeFiles)
%%
%% Divide Refs into the standard ones contained in include files (see
%% mml2tptp_includes/1 for them), and the rest. Make a list of
%% include files for the includable ones.
%% ##ASSUMES: that fraenkels are ok, and the only special formulas
%%    are either scheme instances (starting with sNr) or requirements,
%%    starting with spcNr.
get_include_refs(Refs, IncludeRefs, NonIncludeRefs, IncludeArticles):-
	findall(Ref,
		(
		  member(Ref, Refs),
		  concat_atom([H|_],'_',Ref),
		  (
		    atom_concat(spc, Nr, H) ->
		    atom_chars(Nr, NrChars),
		    checklist(digit,NrChars)
		  ;
		    atom_concat(s, Nr, H),
		    atom_chars(Nr, NrChars),
		    checklist(digit,NrChars),
		    get_ref_fof(Ref,_,_,_,[mptp_info(_,_,scheme_instance,_,_)|_])
		  )
		),
		NonIncludeRefs
	       ),
	subtract(Refs, NonIncludeRefs, IncludeRefs),
	maplist(get_ref_file, IncludeRefs, IncludeArticles1),
	sort(IncludeArticles1, IncludeArticles).

print_include_directive(Article):-
	concat_atom(['Axioms/', Article, '.ax'], IncludeFile),
	writeq(include(IncludeFile)),
	write('.'),
	nl.

%% get_preceding_article_refs(+Article, +Ref, -Refs)
%%
%% This is a tested version used for inclusion of theorems into TPTP,
%% which only gets refernces with proof level [] .
%% So it cannot be used for sublemmas.
%% ##TODO: reimplement & test using fof_toplevel/2
get_preceding_article_refs_old(Article, Ref, Refs):-
	ensure(article_position(Ref,Pos), throw(article_position(Article,Ref))),
	findall(Ref1,
		(
		  article_position(Ref1,Pos1),
		  Pos1 < Pos,
		  fof_name(Ref1, Id),
		  clause(fof(Ref1,_,_,file(Article,_), [mptp_info(_,[],_,_,_)|_]),_,Id)
		),
		Refs
	       ).

%% This is the new version, used for generating the .allowed_local files .
%% It used to behaved badly for sublemmas, because of a bug in assert_level.
%% Note that for sublemms we have to avoid the theses, that is: those that are
%% marked as thesis, and those that are currently being proved.
get_preceding_article_refs(Article, Ref, Refs):-
	ensure(article_position(Ref,Pos), throw(article_position(Article,Ref))),
	get_ref_fof(Ref,fof(Ref,_,_,file(Article,_),[mptp_info(_,ProofLevel,_,_,_)|_])),
	findall(LevAtom, (sublevel(ProofLevel,Lev), level_atom(Lev, LevAtom)), LevAtoms),
	findall(Id1, (member(Lev1, LevAtoms), fof_newlevel(Lev1,Article,Id1)), BadIds),
	findall(Ref0,
		(
		  fof_toplevel(Article, Id0),
		  clause(fof(Ref0,_,_,file(Article,_), [mptp_info(_,[],_,_,_)|_]),_,Id0),
		  article_position(Ref0,Pos0),
		  Pos0 < Pos,
		  not(member(Id0, BadIds))
		),
		Refs0
	       ),
	findall(Ref1,
		(
		  member(LA1, LevAtoms),
		  fof_level(LA1, Id),
		  clause(fof(Ref1,Role1,_,file(Article,_), [mptp_info(_,_,_,_,_)|_]),_,Id),
		  article_position(Ref1,Pos1),
		  Pos1 < Pos,
		  Role1 \= thesis,
		  not(member(Id, BadIds))
		),
		Refs1
	       ),
	union(Refs0, Refs1, Refs).



%% All standard axioms (coming from other articles than the conjecture)
%% which were printed to include files
%% by mml2tptp_includes/1 are omitted, and only the include
%% directive is generated for them. The conjecture, all formulas from
%% its article, and other special formulas not printed by mml2tptp_includes/1
%% are printed explicitly.
print_refs_as_tptp_includes(Conjecture, AllRefs, Options):-
	delete(AllRefs, Conjecture, ProperRefs1),
	ensure(get_ref_file(Conjecture, Article), bad_conjecture(Conjecture)),
	findall(ARef,
		(
		  member(ARef, ProperRefs1),
		  get_ref_fof(ARef,fof(ARef,_,_,file(Article,_),_))
		),
		ArticleRefs
	       ),
	subtract(ProperRefs1, ArticleRefs, NonArticleRefs),
	get_include_refs(NonArticleRefs, _IncludeRefs, NonIncludeRefs, _IncludeArticles0),
	get_preceding_article_refs(Article, Conjecture, PrecedingRefs),
	union(ArticleRefs, PrecedingRefs, AllArticleRefs),
	(
	  member(opt_PP_SMALLER_INCLUDES(AllIncludes), Options) ->
	  get_smaller_includes(Article, AllIncludes, IncludeArticles)
	;
	  needed_environ(Article, [], IncludeArticles)
	),
	checklist(print_include_directive, IncludeArticles),
	checklist(print_ref_as_axiom(Options), NonIncludeRefs),
	checklist(print_ref_as_axiom(Options), AllArticleRefs),
	print_ref_as_conjecture(Options, ProperRefs1, Conjecture).


%%%%%%%%%%%%%%%%%%% ND problem creation %%%%%%%%%%%%%%%%%%%%%%%%%

%% Assumption control (these two seem sufficient):
%% - each assumption in the inference tree is exactly once discharged
%% - each assumption is only used in the subtree rooted at the point of its discharge

%% mk_nd_tree(F,_Prefix,_Options,P)
%%
%% Print all prerequisities for P and then P.
%% Prerequisities are limited to what is inside a proof of a theorem
%% top-level lemmas are not expanded.
%% That means:
%% get references of P, call mk_nd_tree on them, then print P.
%% the recursion stops for things outside of P's prooflevel,
%% and for assumptions.
%% This is a tree traversal, and we want to print every node only once
%% and at the earliest position. So we proceed depth-first, keep a list
%% of things that were requested to be printed, and a list of things
%% that already were printed.

%% now works also for top-level propositions proved by mizar_by or mizar_from
%% (to make the article tree complete)
%% the level is in that case forged to [0], to prevent recursion into other
%% top-level propositions
mk_nd_problem(P,F,Prefix,Options):-
	(var(P) -> fof_file(F,Id); true),
	clause(fof(P,Role1,Fla,file(F,P),[mptp_info(_,[],MPropKind,position(Line0,Col0),_),
					  inference(InfKind,InferInfo,_)|_]),_,Id),
	(
	  Role1 = theorem,
	  %% to avoid canceled theorems
	  Fla \== $true,
	  %% to avoid scheme instances on top-level (s2_recdef_1__e21__pre_ff)
	  MPropKind \== scheme_instance
	;
	  Role1 = lemma_conjecture),
	(InfKind = mizar_proof ->
	    member(proof_level(Lev), InferInfo)
	;
	    Lev = [0]
	),
	(
	  member(position(Line,Col), InferInfo) -> true
	;
	  Line = Line0,
	  Col = Col0
	),
	(member(opt_LINE_COL_NMS, Options) ->
	    concat_atom([Prefix,F,'__',Line,'_',Col],Outfile)
	;
	    concat_atom([Prefix,F,'__',P],Outfile)
	),
	tell(Outfile),
	format('% Mizar ND problem: ~w,~w,~w,~w ~n', [P,F,Line,Col]),
	mk_nd_tree1(F,Lev,[],_,P,Assumptions,Options),
	ensure((Assumptions == []), mk_nd_problem(nonempty_assumptions(P,Assumptions))),
	told.


%% the end of recursion

mk_nd_tree(_,_,PrintedIn,PrintedIn,P,Assums,_):- memberchk([P,Assums],PrintedIn), !.

mk_nd_tree(F,Lev,PrintedIn,[[P,Assums]|PrintedIn],P,Assums,Options):-
	get_ref_fof(P,fof(P,Role,_,file(F1,_),[ mptp_info(_Nr,Lev1,MPropKind,_,Spc)|_Info])),
	(
	  F \= F1, Assums = [], !
	;
	  Role = assumption, Assums = [P], !
	;
	  Role = axiom, Assums = [], ! %% for henkins
	;
	  %% ###TODO: shouldn't the level of sch. insts. be changed (they do not contain consts anyway now)?
	  MPropKind = scheme_instance, Assums = [], ! %% hack - their Lev1 is not [] now
	;
	  strict_sublevel(Lev,Lev1), Assums = [], !
	;
	  MPropKind = constant,
	  Role = definition,
	  Spc = [EqKind, equality|_],
	  member(EqKind,[reconsider,takeasvar]),
	  Assums = [], !
	), !,
	print_for_nd(P,axiom,[],[],Options).


mk_nd_tree(F,Lev,PrintedIn,PrintedOut,P,Assums,Options):- mk_nd_tree1(F,Lev,PrintedIn,PrintedOut,P,Assums,Options),!.
mk_nd_tree(F,Lev,PrintedIn,PrintedOut,P,Assums,Options):-
	throw(mk_nd_tree(unhandled,F,Lev,PrintedIn,PrintedOut,P,Assums,Options)).

%% renamed to have an easy start for the initial clause in mk_nd_problem,
%% which would otherwise be caught by the second clause of mk_nd_tree
mk_nd_tree1(F,Lev,PrintedIn,PrintedOut,P,Assums,Options):-
	get_ref_fof(P,fof(P,_,_,file(F,_),[mptp_info(_,_,_,_,_)
					  |Rest_of_info])),
	member( mizar_nd( inference(NDKind,NDOpts,NDRefs)), Rest_of_info), !,
	(
	  NDKind = discharge_asm,!,
	  ensure( member( discharged(Discharged), NDOpts), mk_nd_tree(Rest_of_info)),
	  append(Discharged, NDRefs, AllRefs2),
	  %% if ThesExps, then prevthesis follows from (Discharged => P) & ThesExps,
	  %% hence it is correct to add ThesExps to the fixpoint algo
	  (
	    member( thesis_expansions(ThesExps), NDOpts),!,
	    get_mizar_inf_refs(F, P, AllRefs2, mizar_from, AllRefs1),
	    delete(AllRefs1, P, AllRefs),
	    subtract(NDRefs, ThesExps, PureTheses),
	    subtract(AllRefs, AllRefs2, AllRefs3),
	    append(ThesExps, AllRefs3, AllRefs4),
	    RefSlot = discharge(Discharged, PureTheses, AllRefs4)
	  ;
	    check_redefs(P,AllRefs2),!,
	    get_mizar_inf_refs(F, P, AllRefs2, mizar_from, AllRefs1),
	    delete(AllRefs1, P, AllRefs),
	    subtract(AllRefs, AllRefs2, AllRefs3),
	    RefSlot = discharge(Discharged, NDRefs, AllRefs3)
	  ;
	    append(Discharged,NDRefs,AllRefs),
	    RefSlot = discharge(Discharged, NDRefs)
	  )
	;
	  NDKind = take,!,
	  %% this takes care of the possible expansions automatically
	  get_mizar_inf_refs(F, P, NDRefs, mizar_from, AllRefs1),
	  delete(AllRefs1, P, AllRefs),
	  RefSlot = AllRefs,
	  Discharged = []
	;
	  NDKind = let,!,
	  %% we have to conduct additional discharging step
	  %% for types of the local consts, i.e. ![X:t(X)]:p(X) is justified
	  %% by p(c) discharged with assumption t(c) (this is the place where t(c)
	  %% gets discharged), and dh_c (i.e. the inference slot is:
	  %% infer(foo,[],[infer(discharge_asm,[discharged([t(c)])],[p(c)]),dh_c])
	  %% if ThesExps occur - e.g. X c= Y expanded to ![X:t(X)]:p(X) by d1,
	  %% then they and the bg are added to dh_c (i.e. the outer list of refs)
	  findall(Hax,(member(Hax,NDRefs),atom_prefix(Hax,'dh_')),Henkin_Refs),
	  maplist(atom_concat('dh_'),HC,Henkin_Refs),
	  maplist(atom_concat('dt_'),HC,Sort_Refs),
	  Discharged = Sort_Refs,
	  subtract(NDRefs, Henkin_Refs, PureTheses1),
	  (member( thesis_expansions(ThesExps), NDOpts) ->
	      get_mizar_inf_refs(F, P, NDRefs, mizar_from, AllRefs1),
	      delete(AllRefs1, P, AllRefs),
	      subtract(PureTheses1, ThesExps, PureTheses),
	      append(PureTheses, Sort_Refs, AvoidInOuterInf),
	      subtract(AllRefs, AvoidInOuterInf, OuterInfRefs),
	      RefSlot = let(Sort_Refs, PureTheses, OuterInfRefs)
	  ;
	      AllRefs = NDRefs,
	      RefSlot = let(Sort_Refs, PureTheses1, Henkin_Refs)

	  )
	;
	  member(NDKind, [conclusion, consider, iterative_eq, trivial, percases]),!,
	  Discharged = [],
	  ((member( thesis_expansions(ThesExps), NDOpts); check_redefs(P,NDRefs)) ->
	      get_mizar_inf_refs(F, P, NDRefs, mizar_from, AllRefs1),
	      delete(AllRefs1, P, AllRefs)
	  ;
	      AllRefs = NDRefs
	  ),
	  RefSlot = AllRefs
	),
	subtract_with_asms(AllRefs, PrintedIn, NewRefs, PrintedAssums),
	mk_nd_tree_l(F, Lev, PrintedIn, PrintedTmp, NewRefs, NewAssums, Options),
	union(PrintedAssums, NewAssums, Assums1),
	subtract(Assums1, Discharged, Assums),
	print_for_nd(P, NDKind, RefSlot, Assums, Options),
	PrintedOut = [[P, Assums] | PrintedTmp].

mk_nd_tree1(F,Lev,PrintedIn,PrintedOut,P,Assums,Options):-
	get_ref_fof(P,fof(P,_,_,file(F,_),[mptp_info(_,_,_,_,_)
					  |Rest_of_info])), !,
	member( inference(InfKind,_InferInfo,InfRefs), Rest_of_info),
	ensure( member(InfKind, [mizar_by, mizar_from]), mk_nd_tree(P,InfKind)),
	get_mizar_inf_refs(F, P, InfRefs, InfKind, AllRefs1),
	delete(AllRefs1, P, AllRefs),
	subtract_with_asms(AllRefs, PrintedIn, NewRefs, PrintedAssums),
	mk_nd_tree_l(F,Lev,PrintedIn,PrintedTmp,NewRefs, NewAssums, Options),
	union(PrintedAssums, NewAssums, Assums),
	print_for_nd(P, InfKind,  AllRefs, Assums, Options),
	PrintedOut = [[P, Assums] | PrintedTmp].


%% like subtract/3, but operates on lists of pairs instead,
%% assuming that on the second place in each pair is a list (of its assumptions),
%% and returns the union of all the subtracted guys' assumptions
subtract_with_asms([], _, [], []):- !.
subtract_with_asms([[A, AsmsA]|B], C, D, Asms) :-
	memberchk([A, AsmsA], C), !,
	subtract_with_asms(B, C, D, AsmsB),
	union(AsmsA, AsmsB, Asms).
subtract_with_asms([A|B], C, [A|D], Asms) :-
	subtract_with_asms(B, C, D, Asms).


mk_nd_tree_l(_,_,PrintedIn,PrintedIn,[],[],_).
mk_nd_tree_l(F,Lev,PrintedIn,PrintedOut,[H|T],Assums,Options):-
	mk_nd_tree(F,Lev,PrintedIn,PrintedTmp,H,AssumsH,Options),
	mk_nd_tree_l(F,Lev,PrintedTmp,PrintedOut,T,AssumsT,Options),
	union(AssumsH, AssumsT, Assums).

%% check if P and any of Refs have different redefinition variants
%% of the same original symbol; if so, typing needs to be added
%% to any reasoning involving EqFrm or EqTrm in Mizar, i.e. the ND
%% reasonings; the stronger version would be to check if the variants
%% are present in symbols from all formulas
check_redefs(P,Refs):-
	maplist(get_ref_fla, [P| Refs], [Fla|Flas1]),
	maplist(collect_symbols_top, [Fla,[Flas1]], [AllFlaSyms,AllRefSyms]),
	logic_syms(LogicSyms),
	subtract(AllFlaSyms, LogicSyms, FlaSyms),
	subtract(AllRefSyms, LogicSyms, RefSyms),
	findall([Redefs,Redefd],
		(
		  member(Redefs, FlaSyms),
		  fof_redefines(Redefs,Redefd,_,_)
		),
		FlaRedefPairs),
	findall([Redefs1,Redefd1],
		(
		  member(Redefs1,RefSyms),
		  fof_redefines(Redefs1,Redefd1,_,_)
		),
		RefRedefPairs),!,
	check_redefs_int(FlaRedefPairs,RefRedefPairs,FlaSyms,RefSyms).

%% check_redefs_int(FlaRedefPairs,RefRedefPairs,FlaSyms,RefSyms)
check_redefs_int([],[[_,Y]|_],FlaSyms,_):- member(Y, FlaSyms),!.
check_redefs_int([],[_|T],FlaSyms,_):- check_redefs_int([],T,FlaSyms,_).
check_redefs_int([[X,Y]|T],RefRedefPairs,FlaSyms,RefSyms):-
	(
	  member([Z,Y], RefRedefPairs),
	  X \== Z,!
	;
	  member(Y, RefSyms),!
	;
	  check_redefs_int(T,RefRedefPairs,FlaSyms,RefSyms)
	).




get_mizar_inf_refs(File, P, Refs, InfKind, AllRefs):-
	get_ref_fof(P, fof(P,_,Fla,_,[mptp_info(_,Lev,_,_,_)|_])),
	maplist(get_ref_syms, [P | Refs], SymsL1),
	ord_union(SymsL1,Syms1),
	ensure(article_position(P,Pos1), throw(article_position(P))),
	%% ###TODO: for reconsidered type, following is enough instead of fixpoint
	%% AllRefs1 = [P|Refs]
	once(fixpoint(File, [Pos1, Lev], InfKind, [P|Refs], [], Syms1, AllRefs)).

%% compute_interest(InfKind,Lev,Intrst)
%% now uses linear scaling
compute_interest(axiom,[],0.9):- !.
compute_interest(_,[],1):- !.
compute_interest(_,Lev,Intrst):- !,
	length(Lev,L),
	%% So L=1 gets 0.8, L=2 gets 0.65,...,L=6 gets 0.05, and L>6 gets 0.02
	Intrst is max(0.8 - (0.15 * (L - 1)), 0.02).


%% mk_conj_from_refs(Refs, ResultFla)
mk_conj_from_refs([], $true).
mk_conj_from_refs([H], ResultFla):- !, get_ref_fla(H, ResultFla).
mk_conj_from_refs([H|T], (HFla & TFla)):- !,
	get_ref_fla(H, HFla),
	mk_conj_from_refs(T, TFla).

%% mk_impl_from_refs(Antecedents, Consequents, ResultFla)
mk_impl_from_refs(Antecedents, Consequents, (AntecFla => ConseqFla)):-
	mk_conj_from_refs(Antecedents, AntecFla),
	mk_conj_from_refs(Consequents, ConseqFla).


%% for parsing with TPTP tools, use
%% Options = [opt_TPTPFIX_ND_ROLE, opt_MK_TPTP_INF],
%% for more info, use Options = []
%% Refs are either just a list of references, or a
%% term with top functor discharge or let (see below)
%% for more complicated inferences
print_for_nd(Q,InfKind,Refs,Assums,Options):-
	get_ref_fof(Q, fof(Q,Q_1,Q2,Q3,Q4)),
	Q4 = [mptp_info(_,Lev,_,_,MpInfoRest)|_],
	compute_interest(InfKind,Lev,Intrst),
	(
	  (Q_1 == lemma_conjecture; Q_1 == thesis),!,
	  Q1 = plain
	;
	  %% fix the old lemma-derived format
	  %% ###TODO: regenerate the constructor files, then remove this
	  %%          it is dangerous, and relies on having lemma-derived only in constr. files
	  (Q_1 == sort; Q_1 == lemma-derived),!,
	  Q1 = axiom
	;
	  Q_1 == axiom, MpInfoRest = [_,henkin_axiom|_],!,
	  %% this is not totally correct, the def is only partial
	  Q1 = definition %% plain

	%% these are defs I think, so just remove the commented code after a while
% 	;
% 	  Q_1 == definition,
% 	  MpInfoRest = [EqKind, equality|_],
% 	  member(EqKind,[reconsider,takeasvar]),!,
% 	  Q1 = plain
	;
	    Q1 = Q_1
	),
	sort_transform_top(Q2,SR2),
	numbervars(SR2,0,_),
	(member( opt_MK_TPTP_INF, Options) ->
	    UI0 = [Q3]
	;
	    UI0 = Q4
	),
	%% ###TODO: when the TPTP (and IDV) parser does not
	%%          complain about empty list of parents,
	%%          remove the triviality check
	Status = status(thm),
	(
	  Q1 = assumption,!,
	  S1 = introduced(assumption,[Q3]),
	  UI1 = [InfKind|UI0]
	;
	  InfKind = axiom,!,
	  (
	    (
	      MpInfoRest = [_,henkin_axiom|_],!
	    ;
	      MpInfoRest = [EqKind, equality|_],
	      member(EqKind,[reconsider,takeasvar])
	    ),!,
	    Q3 = file(_,NewConstSym),
	    S1 = introduced(definition,[new_symbol(NewConstSym),Q3])
	  ;
	    S1 = Q3
	  ),
	  UI1 = [InfKind|UI0]
	;
	  InfKind = trivial,!,
	  S1 = introduced(tautology,[Q3]),
	  UI1 = [InfKind|UI0]
	;
	    (
	      InfKind = discharge_asm,!,
	      (
		Refs = discharge(Disch, Thes),!,
		subtract(Assums, Disch, NewAssums),
		%% Thes are the proper refs, but Geoff wants also the
		%% assumptions in the parent slot
		union(Disch, Thes, UnionRefs),
		S1 = inference(discharge_asm,[Status, assumptions(NewAssums),
					      discharge_asm(discharge,Disch)],UnionRefs)
	      ;
		Refs = discharge(Disch, Thes, DefsAndBG),
		subtract(Assums, Disch, NewAssums),
		%% Thes are the proper refs of the inner inference,
		%% but Geoff wants also the assumptions in the parent slot
		union(Disch, Thes, UnionRefs),
		DischInfer = inference(discharge_asm,[Status, assumptions(NewAssums),
						      discharge_asm(discharge,Disch)],UnionRefs),
		%% have to add the article suffix - absolutize locals will not know about this
		Q3 = file(File,_),
		Thes = [Thes1|_],
		concat_atom([Thes1, '_tmp__',File], TmpName),
		mk_impl_from_refs(Disch, Thes, TmpImpl1),
		sort_transform_top(TmpImpl1,TmpImpl),
		numbervars(TmpImpl,0,_),
		print(fof(TmpName, plain, TmpImpl, DischInfer, [interesting(Intrst),Q])),
		write('.'), nl,
		S1 = inference(mizar_def_expansion, [Status, assumptions(NewAssums)],
			       [TmpName | DefsAndBG])
		% old version with nested inferences
%		S1 = inference(mizar_def_expansion, [Status, assumptions(NewAssums)],
%			       [DischInfer | DefsAndBG])
	      )
	    ;
	      InfKind = let,!,
	      Refs = let(Sort_Refs, PureTheses, OuterInfRefs),
	      subtract(Assums, Sort_Refs, NewAssums),
	      %% PureTheses are the proper refs of the inner inference,
	      %% but Geoff wants also the assumptions in the parent slot
	      union(Sort_Refs, PureTheses, UnionRefs),
	      DischInfer = inference(discharge_asm,[Status, assumptions(NewAssums),
						    discharge_asm(discharge,Sort_Refs)],UnionRefs),
	      %% have to add the article suffix - absolutize locals will not know about this
	      Q3 = file(File,_),
	      PureTheses = [Thes1|_],
	      concat_atom([Thes1, '_tmp__',File], TmpName),
	      mk_impl_from_refs(Sort_Refs, PureTheses, TmpImpl1),
	      sort_transform_top(TmpImpl1,TmpImpl),
	      numbervars(TmpImpl,0,_),
	      print(fof(TmpName, plain, TmpImpl, DischInfer, [interesting(Intrst),Q])),
	      write('.'), nl,
	      S1 = inference(let, [Status, assumptions(NewAssums)],
			     [TmpName | OuterInfRefs])
%	      S1 = inference(let,[Status, assumptions(NewAssums)], [DischInfer|OuterInfRefs])
	    ;
	      S1 = inference(InfKind,[Status, assumptions(Assums)],Refs)
	    ),
	    UI1 = [Q3,UI0]
	),

	(member( opt_TPTPFIX_ND_ROLE, Options) ->
	    %% ###TODO: when the TPTP (and IDV) parser does not
	    %%          complain about empty list of parents, or when
	    %%          AGint is fixed to consider plain as axiom,
	    %%          remove the triviality check
	    ((InfKind = axiom; InfKind = trivial) ->
%		(Q1= assumption -> Role = Q1; Role = axiom)
		Role = axiom
	    ;
		Role = plain
	    ),
	    UI = [Q1 | UI1]
	;
	    Role = Q1,
	    UI = UI1
	),!,
	(member( opt_ADD_INTEREST, Options) ->
	    NewUI = [interesting(Intrst) | UI]
	;
	    NewUI = UI
	),
	print(fof(Q,Role,SR2,S1,NewUI)),
	write('.'), nl.

print_for_nd(Q,InfKind,Refs,Assums,Options):- throw(print_for_nd(Q,InfKind,Refs,Assums,Options)).

%%%%%%%%%%%%%%%%%%%% MML loading and indexing %%%%%%%%%%%%%%%%%%%%

%% allowed file extensions for theory files
theory_exts([dcl,dco,evl,sch,the,lem,zrt]).

%% Kind must be in theory_exts
load_theory_files(Kind):-
	atom(Kind),
	theory_exts(Exts),
	member(Kind, Exts),
	mml_dir_atom(MMLDir),
	concat_atom([MMLDir, '*.', Kind, '2'], Anames),
	string_to_atom(WildCard, Anames),
	expand_file_name(WildCard, Names),
	load_files(Names,[silent(true)]).

%% get_absolute_path_for(+Kind, +MMLDir, +NonMMLDir, +Article, -Path)
%%
%% tries with MMLDir, if not found, succeeds with NonMMLDir
get_absolute_path_for(Kind, MMLDir, NonMMLDir, Article, Path):-
	concat_atom([MMLDir, Article, '.', Kind, '2' ], MMLPath),
	(
	  exists_file(MMLPath) ->
	  Path = MMLPath
	;
	  concat_atom([NonMMLDir, Article, '.', Kind, '2' ], Path)
	).
%% load_theory_files_for(+NonMMLDir, +Articles, +Kind)
load_theory_files_for(NonMMLDir, Articles, Kind):-
	atom(Kind),
	theory_exts(Exts),
	member(Kind, Exts),
	mml_dir_atom(MMLDir),
	maplist(get_absolute_path_for(Kind, MMLDir, NonMMLDir), Articles, Paths1),
	sublist(exists_file,Paths1,ToLoad1),
	load_files(ToLoad1,[silent(true)]).


load_clusters:- load_theory_files(dcl).
load_constructors:- load_theory_files(dco).
load_environs:- load_theory_files(evl).
load_schemes:- load_theory_files(sch).
load_theorems:- load_theory_files(the).
load_lemmas:- load_theory_files(lem).
load_numtypes:- load_theory_files(zrt).

%% do not load top-level lemmas by default;
%% if you do, take care of their local constants and similar stuff
load_mml:-
	load_clusters,load_theorems,load_schemes,
	load_constructors,load_environs,load_numtypes.

%% load_mml_for_article(+Article, +ArticleDir, +AddedNonMML)
%%
%% The ArticleDir is assumed to be the location of the .evl2 file,
%% and also the location of all nonMML articles (those in the AddedNonMML list).
load_mml_for_article(Article, ArticleDir, AddedNonMML):-
	concat_atom([ArticleDir, '/', Article, '.evl2'], EvlFile),
	consult(EvlFile),
	ensure(needed_environ(Article, AddedNonMML, Articles), needed_environ(Article)),
	Extensions = [dcl,dco,evl,sch,the,zrt], % lemmas not needed from others
	checklist(load_theory_files_for(ArticleDir, Articles), Extensions).

% should fail - load with theorems and propositions first
%% ##NOTE: this can fail depending on wjther preprocessing stages
%%         (scheme instances, henkin axioms, ...) have been performed
check_refs:-
	fof(_,_,_,_,[mptp_info(_,_,_,_,_),inference(mizar_by,_,I) |_]),
	member(P,I),
	not(fof(P,_,_,_,_)).

% should fail
check_consts:-
	File=abcmiz_0,
	findall(L1,(fof(_,_,F,file(File,_),_),collect_symbols_top(F,L1)),L2),
	union1(L2,[],L),!,
	member(Const,L),
	atom_prefix(Const,'c'),
	not(fof(_,_,_,file(File,Const),_)).

%% encode and decode between [1,2,3] and '1_2_3'
level_atom(List,Atom):- atom(Atom), !, concat_atom(V1,'_',Atom),
	maplist(atom_number,V1,List).
level_atom([H|T],Atom):- ground([H|T]),
	maplist(atom_number,V1,[H|T]),
	concat_atom(V1,'_',Atom).

%% strip_univ_quant(+FlaIn,-StrippedFla,-UnivVars)
strip_univ_quant((! Vars : X ),Y,[Vars | VarsX]):- !,strip_univ_quant(X,Y,VarsX).
strip_univ_quant(X,X,[]).

%% strip_possible_antecedent(+FlaIn,-StrippedFla)
%%
%% strip at most one antecedent if any exists
strip_possible_antecedent(( _ => B), B):- !.
strip_possible_antecedent(X,X).

%% installs the indeces for fast lookup of fof's;
%% should be called only after addition of custom fof's like
%% scheme instance, e.g.:
%% declare_mptp_predicates,assert_sch_instances(File), install_index
%% ##NOTE: article_positions are not installed here, that is done
%%         only once after loading a full article, since that can change
%%         later by doing e.g. the fraenkel abstraction
install_index:-
	abolish(fof_name/2),
	dynamic(fof_name/2),
	abolish(fof_file/2),
	dynamic(fof_file/2),
	abolish(fof_eq_def/2),
	dynamic(fof_eq_def/2),
	abolish(fof_sort_def/2),
	dynamic(fof_sort_def/2),
	abolish(fof_pred_def/2),
	dynamic(fof_pred_def/2),
	abolish(fof_section/2),
	dynamic(fof_section/2),
	abolish(fof_level/2),
	dynamic(fof_level/2),
	abolish(fof_newlevel/3),
	dynamic(fof_newlevel/3),
	abolish(fof_toplevel/2),
	dynamic(fof_toplevel/2),
	abolish(fof_parentlevel/2),
	dynamic(fof_parentlevel/2),
	abolish(fof_cluster/3),
	dynamic(fof_cluster/3),
	abolish(fof_identifyexp/3),
	dynamic(fof_identifyexp/3),
	abolish(rc_syms_for_consider/7),
	abolish(fof_req/3),
	dynamic(fof_req/3),
	abolish(fof_syms/2),
	dynamic(fof_syms/2),
	abolish(sym_ref_graph/2),
	dynamic(sym_ref_graph/2),
	abolish(fof_ante_sym_cnt/4),
	dynamic(fof_ante_sym_cnt/4),
	abolish(fof_redefines/4),
	dynamic(fof_redefines/4),
%	index(fof_name(1,1)),
%	add_hidden,
	logic_syms(LogicSyms),
	repeat,
	(
	  clause(fof(Ref1,Role1,Fla1,file(File1,Sec1), [mptp_info(_,L_1,MKind,_,Spc)|RestInfo]),_,Id),
	  assert(fof_name(Ref1, Id)),
	  assert(fof_file(File1, Id)),
	  assert(fof_section(Sec1, Id)),
	  assert_level(L_1, File1, RestInfo, Id),
	  assert_syms(MKind,Ref1,Role1,L_1,Fla1,File1,Id,LogicSyms,Sec1,Spc),
	  fail
	;
	  true
	),
	findall(Llev1, fof_level(Llev1, _), Levs),
	sort(Levs, Levs1),!,
	repeat,
	(
	  member(Lev1,Levs1),
	  level_atom(L_l1,Lev1),
	  L_l1 = [_,_|_],
	  append(L2, [_], L_l1),
	  level_atom(L2, Lev2),
	  assert(fof_parentlevel(Lev2,Lev1)),
	  fail
	;
	  true
	),!.



assert_level([], Article, RestInfo, Id):- !,
	assert(fof_toplevel(Article, Id)),
	assert_newlevel(RestInfo, Article, Id).
assert_level([H|T],Article, RestInfo, Id):- !,
	level_atom([H|T],Lev1),
	assert(fof_level(Lev1, Id)),
	assert_newlevel(RestInfo, Article, Id).
assert_level(_,_,_,_).

%% this keeps the article, because toplevel theorem from MML
%% have nontrivial newlevels which we want to avoid.
assert_newlevel([inference(mizar_proof,[proof_level([H|T])|_],_)|_], Article, Id):- !,
	level_atom([H|T],Lev1),
	assert(fof_newlevel(Lev1, Article, Id)).
assert_newlevel(_,_,_).

%% this differs from assert_newlevel only for efficiency of indexing -
%% it is called at different place.
assert_cluster_newlevel([proof_level([H|T])|_],Article,Id):- !,
	level_atom([H|T],Lev1),
	assert(fof_newlevel(Lev1, Article, Id)).
assert_cluster_newlevel(_,_,_).


assert_rc_syms(RCl, Fla, File):-
	strip_univ_quant(Fla, (? [Var : Radix] : sort(Var, Attrs) ), _),
	constr2list('&', _Last, AndList, Attrs),!,
	process_attrsmode_list_for_cons(AndList, ConsAttrs0, _),
	create_cons_info(ConsAttrs0, Radix, [ConsMode, FuncsNr, AttrsNr, FuncSyms, AttrSyms]),
	assert(rc_syms_for_consider(RCl, File, ConsMode, FuncsNr,
					  AttrsNr, FuncSyms, AttrSyms)).

%% ###TODO: requiring all rc symbols seems to be too restrictive, and
%%   probably leads to incompleteness of e3_41__waybel33, due to
%%   lack of rc3_waybel19; the right condition is that all symbols
%%   involved in rc's universally quantified variables (i.e. those on which
%%   the last existantial var depends) have to be already present in the problem,
%%   and probably also the radix mode (with possible functors inside);
%%   this could however get quite wild, and we should probably watch the set
%%   of constants and variables in the problem, and only add to the fixpoint algo
%%   those attributes which appear in their types; also we shouldn't add an rc
%%   when it only takes care of a const/var type which is already taken care of
assert_syms(rcluster,Ref1,_,_,Fla1,File1,Id,LogicSyms,_,Spc):- !,
	    collect_symbols_top(Fla1,AllSyms),
	    assert(fof_syms(Ref1,AllSyms)),
	    subtract(AllSyms,LogicSyms,Syms),
	    assert(fof_cluster(File1,Ref1,Syms)),
	    assert_fxp_data(Ref1,File1,rcluster,Syms),
	    assert_rc_syms(Ref1, Fla1, File1),
	    assert_cluster_newlevel(Spc, File1, Id).

assert_syms(definition,_,definition,[],Fla1,_,Id,_,_,_):-
	strip_univ_quant(Fla1, ( KTerm = _), _),
	nonvar(KTerm),
	KTerm =..[KFun|_],
	atom_chars(KFun,[k|_]),
	assert(fof_eq_def(KFun, Id)),!.

assert_syms(definition,_,definition,[],Fla1,_,Id,_,_,_):-
	strip_univ_quant(Fla1, ( sort(Var,STerm) <=> _), _),
	var(Var),
	nonvar(STerm),
	STerm =..[SFun|_],
	atom_chars(SFun,[v|_]),
	assert(fof_sort_def(SFun, Id)),!.

assert_syms(definition,_,definition,[],Fla1,_,Id,_,_,_):-
	strip_univ_quant(Fla1, ( RAtom <=> _), _),
	nonvar(RAtom),
	RAtom =..[RPred|_],
	atom_chars(RPred,[r|_]),
	assert(fof_pred_def(RPred, Id)),!.

assert_syms(_,Ref1,definition,[],_,_,Id,_,Sec1,[redefinition(_,_,_,Sec2)|_]):-
	assert(fof_redefines(Sec1, Sec2, Ref1, Id)).

%% ###TODO: take the symbols from the UnivVars also for clusters (definitions?)
assert_syms(identifyexp,Ref1,_,_,Fla1,File1,Id,LogicSyms,_,Spc):- !,
	strip_univ_quant(Fla1, Fla2, UnivVars),
	strip_possible_antecedent(Fla2, ( KTerm = _)),
	collect_symbols_top([KTerm | UnivVars], AllSyms),
	subtract(AllSyms,LogicSyms,Syms),
	assert(fof_identifyexp(File1,Ref1,Syms)),
	assert_fxp_data(Ref1,File1,identifyexp,Syms),
	assert_cluster_newlevel(Spc, File1, Id).

assert_syms(fcluster,Ref1,_,_,Fla1,File1,Id,_,_,Spc):- !,
	cl_needed_syms_top(Fla1,AnteSyms),
	assert(fof_cluster(File1,Ref1,AnteSyms)),
	assert_fxp_data(Ref1,File1,fcluster,AnteSyms),
	assert_cluster_newlevel(Spc, File1, Id).

assert_syms(ccluster,Ref1,_,_,Fla1,File1,Id,_,_,Spc):- !,
	cl_needed_syms_top(Fla1,AnteSyms),
	assert(fof_cluster(File1,Ref1,AnteSyms)),
	assert_fxp_data(Ref1,File1,ccluster,AnteSyms),
	assert_cluster_newlevel(Spc, File1, Id).

assert_syms(theorem,Ref1,_,_,Fla1,File1,_,LogicSyms,_,_):-
	member(File1,[numerals, boole, subset, arithm, real]),!,
	collect_symbols_top(Fla1,AllSyms),
	subtract(AllSyms,LogicSyms,Syms),
	assert(fof_req(File1,Ref1,Syms)),!,
	once(assert_fxp_data(Ref1,File1,req,Syms)).

assert_syms(_,_,_,_,_,_,_,_,_,_) :- !.

%% Adds a link from a Symbol to Requirements containing
%% that Symbol. When the symbol becomes available in fixpoint,
%% all counts (implemented as flags) of Refs are decreased,
%% and if they reach zero, the Refs are activated.
%% sym_ref_graph(Symbol,Reqs)
assert_fxp_data(Ref,File,MKind,AnteSyms):-
	checklist(assert_sym_ref_link(Ref),AnteSyms),
	length(AnteSyms,SymsLength),
	assert(fof_ante_sym_cnt(Ref,File,MKind,SymsLength)).

assert_sym_ref_link(Ref,Sym):- assert(sym_ref_graph(Sym,Ref)).

%% Initialize the symbol counters for each references registered
%% by fof_ante_sym_cnt. Intended for fixpoint.
init_fxp_sym_flags:-
	repeat,
	(
	  fof_ante_sym_cnt(Ref,_File,_MKind,SymsLength),
	  flag(Ref,_,SymsLength),
	  fail
	;
	  true
	).

%% decrease_fxp_sym_flags(+NewSyms, -ZeroedRefs)
%%
%% Decrease the fxp symbol counts (flags) for references
%% containing NewSyms.
%% Return the references which were zeroed.
decrease_fxp_sym_flags(NewSyms, ZeroedRefs):-
	maplist(decrease_fxp_sym_flags1,NewSyms,Refs_l),
	append_l(Refs_l,ZeroedRefs).

decrease_fxp_sym_flags1(NewSym, ZeroedRefs):-
	findall(Ref,
		(
		  sym_ref_graph(NewSym,Ref),
		  flag(Ref,N,N-1),
		  N=1
		),
		ZeroedRefs).

fraenkel_ths(S):-
	findall(A,(fof(A,theorem,D,_,_),collect_symbols_top(D,L),
		   member(all,L)),S),
	length(S,N),write(N).

fraenkel_info(S1):-
	fraenkel_ths(S),
	findall([Out,Info],(member(A,S),fof(A,theorem,In,_,_),
			 all_collect_top(In,Out,Info)),S1).

expanded_franks(Flas,D):-
	fraenkel_info(S),
	zip(Flas,Infos1,S),
	append_l(Infos1,Infos),
	mk_fraenkel_defs_top(Infos,NewFrSyms,D),
	length(D,N1), maplist(length,NewFrSyms,Lengths),
	sumlist(Lengths,N2),
	maplist(collect_symbols_top,Flas,L),
	union1(L,[],L2),length(L2,N3),write([N1,N2,N3]).

dotimes(_,0).
dotimes(X,N) :- N1 is N-1, (call(X);true), dotimes(X,N1).


%%%%%%%%%%%%%%%%%%%% Prolog clauses for the type system %%%%%%%%%%%%%%%%%%%%

% ##TEST: :- declare_mptp_predicates,load_mml,install_index,!, fof_cluster(A,B,C),fof(B,D,E,F,[mptp_info(_,_,ccluster,_,_)|_]),not(once(ccluster_to_clauses(E,J))).

% ccluster_to_clauses(+CCl, -L)
%
% Print ccluster as Prolog clauses.
% Prune from Conseq the things present in Antec
% ![ArgTypes]: ![LastVar:Typ] : sort(LastVar,(AntecedentConjunct)) => sort(LastVar,ConsequentConjunct)
% ArgTypes non empty followed by loci


ccluster_to_clauses(! QVars: (![V:T]: (sort(V,Antec) => sort(V,Conseq))), L):- !,
	append(QVars, [V:T], QVars1),
	ccluster_to_clauses(! QVars1: (sort(V,Antec) => sort(V,Conseq)), L).

ccluster_to_clauses(! QVars: (sort(V,Antec) => sort(V,Conseq)), L):- !,
	sort_transform_qlist(QVars, Varlist, QConjunct),
	sort_transform_top(sort(V,Antec), AntecConjunct),
	sort_transform_top(sort(V,Conseq), ConseqConjunct),
	Body = (QConjunct & AntecConjunct),
	pair_conj_to_clauses(Body, ConseqConjunct).


% ##TEST: :- declare_mptp_predicates,load_mml,install_index,!, fof_cluster(A,B,C),fof(B,D,E,F,[mptp_info(_,_,fcluster,_,_)|_]),not(once(fcluster_to_clauses(E,J))).

fcluster_to_clauses( sort(T,Conseq), L):- !,
	fcluster_to_clauses(! []: sort(T,Conseq), L).

fcluster_to_clauses( ! QVars: sort(T,Conseq), L):- !,
	sort_transform_qlist(QVars, Varlist, Body),
	sort_transform_top(sort(T,Conseq), ConseqConjunct),
	pair_conj_to_clauses(Body, ConseqConjunct).

pair_conj_to_clauses(Body, ConseqConjunct):-
	constr2list2(&,_,BodyList,Body),
	constr2list2(&,_,HeadList,ConseqConjunct),
	numbervars([HeadList,BodyList],0,_),
	subtract(HeadList,BodyList,NewHeadList),
	findall(dummy,(member(H,NewHeadList), write(H), write(' :- '), print_many(BodyList),write('.'),nl),_).


% "fof("; absk(#el=`.`,#kind="fc"); ",theorem,";
%     apply[ArgTypes];
%     $succ = { `count(Cluster[1]/*)`; }
%     $srt_s; "("; apply[*[$pres + 2]];  ",";
%     if [$succ = 0] { "$true"; } else {
%     if [$succ = 1] { apply[Cluster[1]]; }
%     else { "( "; apply[Cluster[1]]; " )"; } }
%     ")"; ",file("; lc(#s=`@aid`); ",";
%     absk(#el=`.`,#kind="fc"); "),[mptp_info("; `@nr`;
%     ",[],fcluster,position(0,0),[";



% "!["; ploci(#nr=$l); " : "; apply[Typ]; "]: (";
%     $ante = { `count(*[$pres + 2]/*)`; }
%     $succ = { `count(*[$pres + 4]/*)`; }
%     $srt_s; "("; ploci(#nr=$l); ",";
%     if [$ante = 0] { "$true"; } else {
%     if [$ante = 1] { apply[*[$pres + 2]]; }
%     else { "( "; apply[*[$pres + 2]]; " )"; } }
%     ")"; $imp_s; $srt_s; "("; ploci(#nr=$l); ",";
%     if [$succ = 0] { "$true"; } else {
%     if [$succ = 1] { apply[*[$pres + 4]]; }
%     else { "( "; apply[*[$pres + 4]]; " )"; } }
%     "))"; ",file("; lc(#s=`@aid`); ",";


