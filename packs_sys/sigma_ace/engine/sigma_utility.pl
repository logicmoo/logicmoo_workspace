% ===================================================================
% File 'sigma_utility.pl' 
% Author: Douglas Miles 
% Contact: dmiles@teknowledge.com; apease@teknowledge.com
% Version: 'sigma_utility.pl' 1.0.0 
% Created - 2000/10/25 dmiles@teknowledge.com
% ===================================================================


:-multifile(expireOptimizationsInKB/3).

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).
            
% ===================================================================
% This File is the bootstrap for the Sigma Infence engine one first calls "[inference_module]"
% So first is loads the proper files and then starts up the system
% There are no predicates defined in this file (it just uses other files' predicates)
% ===================================================================
% ===================================================================
% EXPORTS
% ===================================================================



retractallLogged(T):-
	format(':-retractall(~q).~n',[T]),
	retractall(T).

assertLogged(T):-
	format(':-assert(~q).~n',[T]),
	assert(T).



asserta_if_new(A):-A,!.
asserta_if_new(A):-asserta(A),!.


pvar_gen('$VAR'(N)):-idGen(N),!.


tn_link(Clause,ETracking,KB,ETracking):-!.

tn_link(Clause,ETracking,KB,kba(KB,Num)):-atomic(ETracking),atom_codes(ETracking,[84,45|Rest]),!,number_codes(Num,Rest).
tn_link(Clause,ETracking,KB,kbb(KB,ETracking)):-!.


ignore(X,Y):-ignore((X,Y)).


%   select(?Element, ?List, ?List2)
%   is true when the result of removing an occurrence of Element in List
%   is List2.
     /*
select(Element, [Element|Tail], Tail).
select(Element, [Head|Tail1], [Head|Tail2]) :-
        select(Element, Tail1, Tail2).
       */
	      
%======================================================================
% CLSID Generation
% idGen(+Var)
% Creates a new unique number   TODO
%
% Example:
% | ?- idGen(X).
% X = 2234
%======================================================================


idGen(X):-nonvar(X),!.
idGen('$VAR'(N)):-nonvar(N),!,sendNote(user,'SigmaKernel','Prolog Code Bug','Attempt to Gen CLSID on a frozen variable'),!,fail.
idGen(X):-retract(clsid(N)),X is N+1, assert(clsid(X)),!.
idGen(X):-get_time(T),convert_time(T,A,B,C,D,E,F,G),X  is 20000 + D *1000 + G * 1000 + F * 10 + E * 40,XX is X // 3 ,!,assert(clsid(X)).

getPrettyDateTime(String):-get_time(Time),convert_time(Time, String).

% ===================================================================
% has_singleton_vars(-Term).
% Checks for singleton variables
%
% Example:
%| ?- has_singleton_vars(a(X)).
% yes
%
%| ?- has_singleton_vars(a(X,X)).
% no
% ===================================================================


var_merge(L1,L2,O):-
	close_ll(L1,L11),
	close_ll(L2,L22),
	append(L1,L2,L4),
	catch(sort(L4,O),_,L4=O),!.
var_merge(L1,L2,L2):-!.

close_ll([],[]):-!.
close_ll(L1,[]):-isSlot(L1),!.
close_ll([H|T],[H|TT]):-close_ll(T,TT),!.


has_singleton_vars(Clause):-getPrologVars(Clause,_,[_|_],_).
has_singleton_vars(Clause):-getPrologVars(Clause,[_|_],_,[]).

% ===================================================================
% any_to_string(+Term,?String).
% converts any term to a list of prolog chars
%
% Example:
% | ?- any_to_string(X,Y).
% X = _h83
% Y = [95,104,56,51]
% | ?- any_to_string("X",Y).
% Y = [91,56,56,93]
% | ?- any_to_string("t(r)",Y).
% Y = [91,49,49,54,44,52,48,44,49,49,52,44,52,49,93]
% ===================================================================

any_to_string(Term,String) :- !,fmt_write_string(Atom,"%S",args(Term)),atom_codes(Atom,String).

% ===================================================================
% conv(?List,?Term) 
% conv_det(+List,-Term)
% term_to_list(?Term,?List)
% term_to_atomlist(?Term,?AtomList)

% Based on Conjuctions
% term_to_list/2 is the inverse of  conv(?List,?Term) and conv_det(+List,-Term)  

% term_to_atomlist/2 is a more agressive form of term_to_list
% Examples:
/*
| ?- conv(X,Y).
X = []
Y = true

| ?- conv([a,b],Y).
Y = (a  ','  b)

| ?- conv(Out,(a :- b)).
Out = [(a :- b)]


*/
% ===================================================================

conv([],true):-!.
conv([X|T],(X,R)) :- T \== [],!,conv(T,R).
conv([X],(X)).

conv_det([],true):-!.
conv_det(V1,_V2):-var(V1),!,fail.
conv_det([X|T],(X,R)) :- T \== [],!,conv_det(T,R).
conv_det([X],(X)).

term_to_list(Var,[]):-var(Var).
term_to_list(','(A,B,C,D,E,F),List):-!,term_to_list(((A,B),C),AL),term_to_list(((D,E),F),BL),append(AL,BL,List).
term_to_list(','(A,B,C,D,E),List):-!,term_to_list(((A,B),C),AL),term_to_list((D,E),BL),append(AL,BL,List).
term_to_list(','(A,B,C,D),List):-!,term_to_list(((A,B),C),AL),term_to_list((D),BL),append(AL,BL,List).
term_to_list(','(A,B,E),List):-!,term_to_list(((A,B)),AL),term_to_list((E),BL),append(AL,BL,List).
term_to_list((A,B),List):-!,term_to_list(A,AL),term_to_list(B,BL),append(AL,BL,List).
term_to_list(true,[]):-!.
term_to_list(List,List):-is_list(List).
term_to_list(Term,[Term]):-compound(Term).
term_to_list(Term,[Term]):-atomic(Term).

term_to_atomlist([Var],[Var]) :- ( var(Var) ; atomic(Var) ),!.
term_to_atomlist([TERM],[H|T]):-!,
         TERM=..[H|Args],
         term_to_atomlist(Args,T).
term_to_atomlist([H|T],List):-!,
         term_to_atomlist([H],HList),
         term_to_atomlist(T,TList),
         append(HList,TList,List).
term_to_atomlist(Term,AtomList):-!,
         conv(TermList,Term),
         term_to_atomlist(TermList,AtomList). 


% ===================================================================
% getSharedVariables(Term1,Term2).
% tests to see if Varables are shared between two Terms
% ===================================================================

getSharedVariables(Left,Right):-copy_term((Left,Right),(VLP,VRP)),numbervars((VLP,VRP)),
            term_to_atomlist(VLP,VLPP),term_to_atomlist(VRP,VRPP),
             member('$VAR'(N),VLPP),member('$VAR'(N),VRPP).
                                          
                                          
% ===================================================================
% delete_once/3.
/*
delete_once(+List1, ?Elem, ?List2)
Delete forall members of List1 that simultaneously equal with Elem and equal the result with List2. 
*/

% ===================================================================

delete_once([],X,X):-!.
delete_once(_,[],[]):-!.
delete_once([DeleteThisItem],[DeleteThisItem|ListOfMore],ListOfMore):-!.
delete_once([DeleteThisItem],[Skip|ListOfMore],[Skip|ListAfterDelete]):- !,
         delete_once([DeleteThisItem],ListOfMore,ListAfterDelete).
delete_once([DeleteThisItem|DeleteThese],SourceList,ResultList):-!,
         delete_once([DeleteThisItem],SourceList,DeleteThisItemResult),
         delete_once(DeleteThese,DeleteThisItemResult,ResultList).


% ===================================================================
% clean_true(+DirtyTerm,-CleanTerm).
% removes resundant true atoms from terms
% ===================================================================

clean_true(X,Y):-once((term_to_list(X,L))),once((delete(L,true,L2))),once((conv_det(L2,Y))).

% ===================================================================
% optional_bound(+Term1,+Term2).
% Prepairs an option binding and alway succeeds
% ===================================================================
optional_bound(Left,Right):-ignore(Left=Right).



% ===================================================================
% Global Variable Manipuitalion for Sigma
% ===================================================================

%global_set(Name,Value):-flag(Name,_,Value).

global_increment(Name):-flag(Name,N,N+1).

%global_get(Name,Value):- flag(Name,Value,Value).

copy_term(Term,TermCopy,CopyOfVars):-copy_term(Term,TermCopy),
               getPrologVars(TermCopy,CopyOfVars,_,_).


% ===================================================================
%    sigma_B_consult/1
% ===================================================================

sigma_B_consult(FileName):-
      [FileName],!.

sigma_B_consult(FileName):-real_prolog_file_name(FileName,AbsoluteFile),
      [AbsoluteFile].

% ===================================================================
%    sigma_B_load_dyn/1
% ===================================================================

sigma_B_load_dyn(FileName):-
               real_file_name(FileName,LocalFile),
               load_dyn(LocalFile).
/*
               file_open(LocalFile,'r',INPUT),
               repeat,
               file_read(INPUT,Term),
               assert(Term),
               Term=end_of_file,!,
               sigma_B_seen,!.
  */

assert_prolog(X,_) :- (var(X)),!.
assert_prolog([H|T],Vars) :-list_to_term([H|T],Term),!,assert_prolog(Term,Vars).
assert_prolog(PTERM_NATIVE,Vars):-PTERM_NATIVE=..[C,X,Y],'surface-instance'(C,'Junctive',_),!,
         assert_prolog(X,Vars),
         assert_prolog(Y,Vars).
assert_prolog(X,_Vars) :- predicate_property(X,built_in),!. 
assert_prolog(X,_Vars) :- ground(X),retract(X),fail.
assert_prolog(X,_Vars) :- /* not(exists_in_database(X)),!, */ assert(X). %, ua_out(modification,(X),Vars).
assert_prolog(_X,_Vars) :- !. %,not(exists_in_database(X)), assert_prolog(X). %ua_out(disp_modification,assert_prolog(X),Vars)

assert_prolog(Context_atom,WFF,Vars):-
         add_context(Context_atom,WFF,WFFAC),
         assert_prolog(WFFAC,Vars).



do_to_conjuncts(Var,G):- (var(Var);var(G)),!.
do_to_conjuncts((A,B),G):- !,
         ignore(once(do_to_conjuncts(A,G))),
         ignore(once(do_to_conjuncts(B,G))).
do_to_conjuncts(A,G):- !,
         ignore((C=..[G,A],once(C))).  

do_to_conjuncts(Var,Var2,G):- (var(Var);var(Var2   );var(G)),!.



do_to_conjuncts(V,(A,B),G):- !, %numbervars((A,B)),
         ignore(once(do_to_conjuncts(V,A,G))),
         ignore(once(do_to_conjuncts(V,B,G))).

do_to_conjuncts(V,A,G):- !,
         ignore((C=..[G,V,A],once(C))).  
                        


mretractall(TERM):-!, functor(TERM,F,A),predicate_property(TERM,Z),!,abolish(F/A),predicate_property(TERM,Q),!,dynamic(F/A),predicate_property(TERM,P),!. 


set_for_hilog_table(PrologForm):- 
            functor(PrologForm,OP,_A), 
            (predicate_property(PrologForm,(built_in)) ; 'surface-instance'(OP,'Connective',_)),!.

set_for_hilog_table(PrologForm):- telling_file,
            functor(PrologForm,OP,A), 
         	write_clause_to_file(assert(functsymbol(OP/A))), % Asserts to be known for Non
         	write_clause_to_file((table(OP/A))), % 
            write_clause_to_file(hilog(OP)).
set_for_hilog_table(_PrologForm).

exists_in_database((Y:-_)):-predicate_property(Y,built_in).
exists_in_database((X)):-predicate_property(X,built_in).
exists_in_database((Y:-X)):-!,not(predicate_property(Y,built_in)),clause(Y,X).
exists_in_database((Y)):-!,not(predicate_property(Y,built_in)),clause(Y,true).



clause_id_gen(CLID):-idGen(CLID).



% not assertzble objects
non_assertzble(Var):-var(Var),!.
non_assertzble([]):-!.
non_assertzble(end_of_file):-!.
non_assertzble('end-of-file'):-!.
non_assertzble(fail):-!.
non_assertzble(false):-!.
non_assertzble(true):-!.
non_assertzble(comment(_C)):-!.
non_assertzble(browser_only(_C)):-!.
non_assertzble(List):-is_list(List),length(List,X),X<8,!. 

once_ignore(X):-once(ignore(X)).

get_storage_file(PrivateName,RD):-
   'LOGIC_ENGINE_RT'(RealDir),
   atom_codes(RealDir,RealDirS),
   append(RealDirS,[47|PrivateName],RDS),
   atom_codes(RD,RDS).

get_storage_exists_file(PrivateName,RD):-
   'LOGIC_ENGINE_RT'(RealDir),
   atom_codes(RealDir,RealDirS),
   append(RealDirS,[47|PrivateName],RDS),
   atom_codes(RD,RDS),
   (not(exists_file(RD)) -> 
      (file_open(RD,'w',IOPort),fmt_write(IOPort,"end_of_file.\n",_),file_close(IOPort))  % Make the file
      ; true ).


/*
% SWI Builtin
union([X|L1],L2,L3) :-
	identical_member(X,L2),
	!,
	union(L1,L2,L3).
union([X|L1],L2,[X|L3]) :-
	union(L1,L2,L3).
union([],L,L).
*/
% ===================================================================
%  safe_kb_info_db(KB_Name,Can,WFS,PFile) Creates file paths
% ===================================================================


safe_kb_info_db(KB_Name,Can,WFS,PFile):-
                  kb_make_status_start(kb(KB_Name,Can)=_),!,
                  add_file_extension(".wfs",Can,WFS),
                  add_file_extension(".pl",Can,PFile),!.

safe_kb_info_db(KB_Name,error,error,error):- sendNote(error,ioSubsystem,'KB file not found',['the KB is like not correct ',KB_Name]),!,fail.

                  
actual_file_name(SourceFile,SourceFileLocal):-
            atom_codes(SourceFile,[99,58,47,47|Rest]),!,
            atom_codes(NewSourceFile,[99,58,47|Rest]),
            actual_file_name(NewSourceFile,SourceFileLocal).

actual_file_name(SourceFileLocal,SourceFileLocal):-!.


safe_file_open(SourceFile,MODE,HANDLE):-
                  actual_file_name(SourceFile,SourceFileLocal),
                  file_open(SourceFileLocal,MODE,TH),
                        (TH = (-1) ->
                                    (sendNote(error,ioSubsystem,'file not found',['the file or path ',SourceFile,' is not accessable ']),!,fail )
                                    ;
                                   HANDLE=TH) .





% ===================================================================
% split_key_list/4. (Adapted from Richard O'Keefe). 
% ===================================================================


split_key_list([], [], [], []).
split_key_list([V-_,W-_|Vs], Vars, Singletons, Multiples) :- W == V, !,
    Vars = [V|Vars1],
    Multiples = [V|Multiples1],
    split_key_list(Vs, V, Vs1),
    split_key_list(Vs1, Vars1, Singletons, Multiples1).
split_key_list([V-_|Vs], [V|Vars], [V|Singletons], Multiples) :-
    split_key_list(Vs, Vars, Singletons, Multiples).
split_key_list([W - _|Vs], V, Vs1) :- W == V, !,
    split_key_list(Vs, V, Vs1).
split_key_list(Vs1, _, Vs1).




close_list([]):-!.
close_list([_]):-!.
close_list([_,_]):-!.
close_list([_,_|CLOSE]):-!,close_list(CLOSE).
close_list([_,_,_|CLOSE]):-!,close_list(CLOSE).
close_list([_,_,_,_,_|CLOSE]):-!,close_list(CLOSE).
close_list([_,_,_,_,_,_,_|CLOSE]):-!,close_list(CLOSE).

open_list(A,B):-!,append(A,_,B).



unbind_numbers('$VAR'(P),'$VAR'(P)):-!.
unbind_numbers('AssignmentFn'(P,Q),'AssignmentFn'(P,Q)):-!.
unbind_numbers(P,_):-number(P),!.
unbind_numbers(P,P):-var(P),!.
unbind_numbers(P,Q):- !,P =.. [F|ARGS],!,
                   unbind_numbers_l(ARGS,UARGS),
                    Q=..[F|UARGS],!.

unbind_numbers_l([],[]).
unbind_numbers_l([A|RGS],[U|ARGS]):-!,
         unbind_numbers(A,U),
         unbind_numbers_l(RGS,ARGS),!.


consult_as_dynamic(FilenameLocal):- 
         open(FilenameLocal,'read',R),   
         repeat,
         read(R,TERM),
         ((TERM = end_of_file -> (true,!) ; 
               ((
                 assert(TERM),fail
                 ))
         )),!,
         close(R).

% ===================================================================
% Substitution based on ==
% ===================================================================

% Usage: subst(+Fml,+X,+Sk,?FmlSk)

subst(A,B,C,D):-notrace(nd_subst(A,B,C,D)),!.

nd_subst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_subst(        P, X,Sk,        P1 ) :- functor(P,_,N),nd_subst1( X, Sk, P, N, P1 ).

nd_subst1( _,  _, P, 0, P  ).

nd_subst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], nd_subst2( X, Sk, Args, ArgS ),
            nd_subst2( X, Sk, [F], [FS] ),
            P1 =.. [FS|ArgS].

nd_subst2( _,  _, [], [] ).
nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- nd_subst( A,X,Sk,Ap ),nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, L, L ).

weak_nd_subst(  Var, VarS,SUB,SUB ) :- Var=VarS,!.
weak_nd_subst(        P, X,Sk,        P1 ) :- functor(P,_,N),weak_nd_subst1( X, Sk, P, N, P1 ).

weak_nd_subst1( _,  _, P, 0, P  ).

weak_nd_subst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], weak_nd_subst2( X, Sk, Args, ArgS ),
            weak_nd_subst2( X, Sk, [F], [FS] ),
            P1 =.. [FS|ArgS].

weak_nd_subst2( _,  _, [], [] ).
weak_nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X = A, !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- weak_nd_subst( A,X,Sk,Ap ),weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, L, L ).


% ===================================================================
% Destructive Freezing/Melting
% ===================================================================

call_frozen(Goal):-
	copy_term(Goal,Copy),
	crossref_vars(Goal,Copy,CopyKey),
	call(Copy),
	setarg_vars(Copy,CopyKey,Goal).
	

freeze_vars(Fml,Frozen,MeltKey):-
	copy_term(Fml,Frozen),
	crossref_vars(Fml,Frozen,MeltKey),
	numbervars(Frozen),!.
	
melt_vars(Frozen,[]=[],Frozen):- !.
melt_vars(Frozen,[OV|OL]=[FV|VL],Thawed):-
	subst(Frozen,FV,OV,Thawing),
	melt_vars(Thawing,OL=VL,Thawed),!.

setarg_vars(Frozen,[]=[],Frozen):- !.
setarg_vars(Frozen,OL=NL,Thawed):-
	OT=..[getPrologVars|OL],
	NT=..[getPrologVars|NL],
	setarg_vars(1,Frozen,OT,OL,NT,NL,Thawed).
	
setarg_vars(_,Thawed,OT,OL,_,[],Thawed):-!.
setarg_vars(N,Frozen,OT,[OH|OL],NT,[NH|NL],Thawed):-
	setarg(N,OT,NH),
	NN is N +1,
	setarg_vars(NN,Frozen,OT,OL,NT,NL,Thawed),!.
	

crossref_vars(Fml,Frozen,FmlVars = FrozenVars):-
	free_variables(Fml,FmlVars),
	free_variables(Frozen,FrozenVars),!.


% ===================================================================
% Substitution based on =
% ===================================================================

% Usage: repl(+Fml,+X,+Sk,?FmlSk)

replc(Fml,X,Sk,FmlSk):-
	notrace(repl(Fml,X,Sk,FmlSk)),!.
/*
	copy_term(Fml,FmlX),
	numbervars(FmlX),
	repl(FmlX,X,Sk,FmlSk),!.
*/	
		

repl(  Var, VarS,SUB,SUB ) :- Var=VarS,!.
repl(        P, X,Sk,        P1 ) :- functor(P,_,N),repl1( X, Sk, P, N, P1 ).

repl1( _,  _, P, 0, P  ).

repl1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], repl2( X, Sk, Args, ArgS ),
            repl2( X, Sk, [F], [FS] ),
            P1 =.. [FS|ArgS].

repl2( _,  _, [], [] ).
repl2( X, Sk, [A|As], [Sk|AS] ) :- X = A, !, repl2( X, Sk, As, AS).
repl2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, repl2( X, Sk, As, AS).
repl2( X, Sk, [A|As], [Ap|AS] ) :- repl( A,X,Sk,Ap ),repl2( X, Sk, As, AS).
repl2( X, Sk, L, L ).

weak_repl(  Var, VarS,SUB,SUB ) :- Var=VarS,!.
weak_repl(        P, X,Sk,        P1 ) :- functor(P,_,N),weak_repl1( X, Sk, P, N, P1 ).

weak_repl1( _,  _, P, 0, P  ).

weak_repl1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], weak_repl2( X, Sk, Args, ArgS ),
            weak_repl2( X, Sk, [F], [FS] ),
            P1 =.. [FS|ArgS].

weak_repl2( _,  _, [], [] ).
weak_repl2( X, Sk, [A|As], [Sk|AS] ) :- X = A, !, weak_repl2( X, Sk, As, AS).
weak_repl2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, weak_repl2( X, Sk, As, AS).
weak_repl2( X, Sk, [A|As], [Ap|AS] ) :- weak_repl( A,X,Sk,Ap ),weak_repl2( X, Sk, As, AS).
weak_repl2( X, Sk, L, L ).


%Shared with XSB




file_newer(F1,F2):- time_file(F1,T1),time_file(F2,T2),!,T1>T2.


%:-getenv('LOGIC_ENGINE_RT',RealDir),assert('LOGIC_ENGINE_RT'(RealDir)) .

reconsult(F):-consult(F).

set_global_compiler_options(_Ignore).
load_dyn(X):-[X].
assert_n(X):-retractAllProlog(X),assert(X).

%%:-load_library(library(quintus)).


file_open(FileName,r,IOPort):- open(FileName,read,IOPort,[type(binary)]).
file_open(FileName,w,IOPort):- open(FileName,write,IOPort,[type(binary)]).
file_close(IOPort):-close(IOPort).

fmt_write_string(String,Format,Args):- catch(Args=..[_|FARGS],_,FARGS=[]),
      string_to_atom(Format,FMTSTR),
      fmtString(String,FMTSTR,FARGS).

fmt_write(Format,Args):- catch(Args=..[_|FARGS],_,FARGS=[]),
      string_to_atom(Format,FMTSTR),
      writeFmt(FMTSTR,FARGS).

fmt_write(OUTPUT,Format,Args):- catch(Args=..[_|FARGS],_,FARGS=[]),
      string_to_atom(Format,FMTSTR),
      writeFmt(OUTPUT,FMTSTR,FARGS).

real_file_name(X,X).

setPrologFlag(X,Y):-catch(current_prolog_flag(X,Y),_,fail).
setPrologFlag(_,' ').

%import(_ignore).
%export(_ignore).

real_kif_file_name(_FileName,AbsoluteFile):-
      once(add_file_extension(".kif",_FileName,SeeThisFile)),
      once(add_file_user_lib_directory(SeeThisFile,AbsoluteFile)).
                                                         
real_prolog_file_name(_FileName,AbsoluteFile):-
      once(add_file_extension(".pl",_FileName,SeeThisFile)),
      once(add_file_user_lib_directory(SeeThisFile,AbsoluteFile)).
                                                         

add_file_user_lib_directory(_LocalFile,AbsoluteFile):- 
         name(_LocalFile,FileNameString),
         once((('LOGIC_ENGINE_RT'(RTD),!,name(RTD,RTDString));((ua_out(cb_error,'SIGMA_XSB_RT Not Set in Environment',_),RTDString=[])))),
         once(append(RTDString,[47|FileNameString],LEPAth)),
         name(AbsoluteFile,LEPAth).

add_file_extension(EXT,FileName,LocalFile):-atom_codes(Extension,EXT),
            safe_file_name_extension(FileName, Extension, LocalFile).

safe_file_name_extension(SourceFile,Ext,SurfaceFile):-
            actual_file_name(SourceFile,File),
            file_name_extension(Base,_,File), 
            file_name_extension(Base,Ext,SurfaceFile). 


file_get0(IOPort,end_of_file):-  at_end_of_stream(IOPort),!.
file_get0(IOPort,Char):- get_code(IOPort,Char),ignore((Char<0)).
 

file_getbuf(IOPort,1,Char,1):-get_char(IOPort,Char).


max(X,Y,Max) :-
	X >= Y ->
		Max = X;
	%true ->
		Max = Y.

min(X,Y,Min) :-
	X =< Y ->
		Min = X;
	%true ->
		Min = Y.


nop.
nop(_).

conjoin(A,B,C) :-
	A == true ->
		C = B;
	B == true ->
		C = A;
	A == false ->
		C = false;
	B == false ->
		C = false;
	%true ->
		C = (A , B).

disjoin(A,B,C) :-
	A == true ->
		C = true;
	B == true ->
		C = true;
	A == false ->
		C = B;
	B == false ->
		C = A;
	%true ->
		C = (A ; B).




delete_ident(V,_,V):-isSlot(V),!.
delete_ident([],_,[]):-!.
delete_ident([Item|L1],ItemT,L2):-Item==ItemT, !,delete_ident(L1,ItemT,L2).
delete_ident([A|L1],ItemT,[A|L2]):-!,delete_ident(L1,ItemT,L2).


nth_identical_member(X,[],_):-!,fail.
nth_identical_member(X,[XX|_],1):-X==XX,!.
nth_identical_member(X,[_|XXs],NN):- nth_identical_member(X,XXs,N),NN is N + 1.


% ==========================================================
% TEMP DB
% ==========================================================

assert_conj(end_of_file,0).
assert_conj(end_of_file:_,0).
assert_conj(true,0).
assert_conj((X,Y):V,(XX,YY):V):-!,
      assert_conj(X,XX),
      assert_conj(Y,YY).
assert_conj((X,Y),(XX,YY)):-!,
      assert_conj(X,XX),
      assert_conj(Y,YY).

assert_conj(Y,0):- catch(in_pl_db(Y),E,fail),!.
assert_conj(Y,Ref):-!,unnumbervars(Y,Z),logOnFailure(assert(Z,Ref)).

in_pl_db((IH:-IB)):-!,
	renumbervars((IH:-IB),(H:-B)),!, 
	clause(H,B,CLDEX),
	clause(RH,RBody,CLDEX),
	numbervars((RH,RBody),'Test',0,_),numbervars((H,Body),'Test',0,_),
	(RH,RBody)==(H,Body),!.

in_pl_db(IH):-!,
	renumbervars((IH),(H)),!,
	clause(H,true,CLDEX),
	clause(RH,true,CLDEX),
	numbervars((RH),'Test',0,_),numbervars((H),'Test',0,_),
	(RH)==(H),!.
	
renumbervars(N,U):-
	unnumbervars(N,U),
	numbervars(U,'Test',0,_),!.

retract_ref(0):-!.
retract_ref((X,Y)):-!,
	retract_ref(X),
	retract_ref(Y).
retract_ref(X):-!,erase(X);true.


retract_conj(end_of_file,true).
retract_conj(end_of_file:_,true).
retract_conj(true,true).
retract_conj((X,Y):V,(XX,YY):V):-!,
      retract_conj(X,XX),
      retract_conj(Y,YY).
retract_conj((X,Y),(XX):V):-!,
      retract_conj(X,XX),
      retract_conj(Y,YY).

retract_conj((IH:-IB),(IH:-IB)):-!,
	renumbervars((IH:-IB),(H:-B)),!, 
	clause(H,B,CLDEX),
	clause(RH,RBody,CLDEX),
	numbervars((RH,RBody),'Test',0,_),numbervars((H,Body),'Test',0,_),
	(RH,RBody)==(H,Body),erase(CLDEX).

retract_conj((IH),(IH)):-!,
	renumbervars((IH),(H)),!, 
	clause(H,B,CLDEX),
	clause(RH,RBody,CLDEX),
	numbervars((RH),'Test',0,_),numbervars((H),'Test',0,_),
	(RH)==(H),erase(CLDEX).

retract_conj((IH),true):-!.



listing_template(T):-clause(T,N),
	format('~q:-~q.\n',[T,N]),
	fail.
	
listing_template(T).



assert_conj_count(A,C):-
         flag('Assert Conj',_,0),
         assert_conj_c(A),!,
         flag('Assert Conj',C,C),!.

assert_conj_c(end_of_file).
assert_conj_c(true).
assert_conj_c((X,Y)):-
      assert_conj_c(X),
      assert_conj_c(Y).
assert_conj_c(Y):-logOnFailure(assert(Y)),global_increment('Assert Conj').

retract_conj_count(A,C):-
         flag('Retract Conj',_,0),
         retract_conj_c(A),!,
         flag('Retract Conj',C,C),!.

retract_conj_c(end_of_file).
retract_conj_c(true).
retract_conj_c((X,Y)):-
      retract_conj_c(X),
      retract_conj_c(Y).
retract_conj_c(Y):-retractAllProlog(Y),global_increment('Retract Conj').

assert_conj_q(true).
assert_conj_q((X,Y)):-!,
      assert_conj_q(X),
      assert_conj_q(Y).
assert_conj_q(Y):-logOnFailure(assert(Y)). % ,post_each_can_form(Y).

retract_conj_q(true).
retract_conj_q((X,Y)):-!,
      retract_conj_q(X),
      retract_conj_q(Y).
retract_conj_q(Y):-retractAllProlog(Y),post_each_can_form(Y).



concat_shell(Shell,yes):-logOnFailure(concat_atom(Shell,ShellCmd)),ignore(shell(ShellCmd)).

sandbox(G):-catch(G,_,fail). % TODO .. add to ISO Standard :)


my_catch(X,E,G):-catch(X,E,G).

infer_by(_).

unifiable_member(X,[Y|_]) :-			% run-time predicate to
	unify_with_occurs_check(X,Y),!.				% find complementary ancestor
unifiable_member(X,[_|L]) :-
	unifiable_member(X,L).

identical_member(X,[Y|_])  :-			% run-time predicate to
	X == Y,					% find identical ancestor
	!.

identical_member(X,[_|L]) :-
	identical_member(X,L).
	
fidentical_member(X,[Y|_])  :-			% run-time predicate to
	X == Y -> ! ;identical_member(X,[_|L]).
	 		
	

nonidentical_member(X,[Y|L]) :-			% run-time predicate to
	X \== Y,				% find identical ancestor
	nonidentical_member(X,L).
nonidentical_member(_X,[]).

differing_member(X,[Y|L]) :-			% run-time predicate to
	dif(X,Y),				% constrain literal to not
	differing_member(X,L).			% be identical to ancestor
differing_member(_X,[]).


apply_to_conjuncts(Wff,P,Wff1) :-
	Wff = (A , B) ->
		apply_to_conjuncts(A,P,A1),
		apply_to_conjuncts(B,P,B1),
		conjoin(A1,B1,Wff1);
	%true ->
		P =.. G,
		append(G,[Wff,Wff1],G1),
		T1 =.. G1,
		call(T1).

apply_to_elements([X|L],P,Result) :-
	P =.. G,
	append(G,[X,X1],G1),
	T1 =.. G1,
	call(T1),
	apply_to_elements(L,P,L1),
	conjoin(X1,L1,Result).
apply_to_elements([],_,true).

apply_to_elements2([X|L],P,[X1|L1]) :-
	T1 =.. [P,X,X1],
	call(T1),
	apply_to_elements2(L,P,L1).
apply_to_elements2([],_,[]).


apply_to_list([],_,[]).
apply_to_list([Elem|List],P,[Elem1|List1]) :-
	T =.. [P,Elem,Elem1],
	call(T),
	apply_to_list(List,P,List1).


apply_to_list_flat([],_,[]).
apply_to_list_flat([Elem|List],P,ResList) :-
	T =.. [P,Elem,Result1],
	call(T),
	apply_to_list(List,P,List1),
	append(Result1,List1,ResList).




mreplc(F,F,R,R):-!.
mreplc(B,_,_,B):-not(compound(B)),!.
mreplc([H|Body],F,R,[NH|NBody]):-!,
	mreplc(H,F,R,NH),!,
	mreplc(Body,F,R,NBody),!.
mreplc(A,F,R,Rs):-!,
	A=..[H|Body],%  trace,
	mreplc(H,F,R,NH),
	mreplc(Body,F,R,NBody),
	Rs=..[NH|NBody].
mreplc(B,_,_,B).



getfunctor(not(Fact),A):-!,getfunctor(Fact,A).
getfunctor('~'(Fact),A):-!,getfunctor(Fact,A).
getfunctor(Fact,holds):-Fact=..[holds,A|_],isSlot(A),!.
getfunctor(Fact,A):-Fact=..[holds,A|_],!.
getfunctor(Fact,A):-functor(Fact,A,_).


% uses getPrologVars(Goal, Vars, Singletons, Multiples)
		


list_to_comma(Var,Var):-isSlot(Var),!.
list_to_comma([Var],Var):-isSlot(Var),!.
list_to_comma([],true).
list_to_comma([X],X).
list_to_comma([X|Y],(XX,YY)):-!,
	list_to_comma(X,XX),
	list_to_comma(Y,YY).
list_to_comma(and(X,Y),(XX,YY)):-!,
	list_to_comma(X,XX),
	list_to_comma(Y,YY).
list_to_comma(X,X).
	


conjunctsToList(Ante,Ante):-isSlot(Ante),!.
conjunctsToList([],[]).
conjunctsToList(and(A,B),List):-
	conjunctsToList(A,AL),		
	conjunctsToList(B,BL),
	append(AL,BL,List).		
conjunctsToList([A|B],List):-
	conjunctsToList(A,AL),		
	conjunctsToList(B,BL),
	append(AL,BL,List).		
conjunctsToList((A,B),List):-
	conjunctsToList(A,AL),		
	conjunctsToList(B,BL),
	append(AL,BL,List).		
conjunctsToList((Ante),[(Ante)]).




prologEach([],Item,_):-!.
prologEach([Item|Rest],Test,Goal):-notrace((
	not(not((Item=Test,Goal))),!,
	prologEach(Rest,Test,Goal),!)).


prologAtLeastOne([],Item,_):-!.
prologAtLeastOne([Item|Rest],Item,Goal):-once(Goal),!.
prologAtLeastOne([_|Rest],Item,Goal):-
	prologAtLeastOne(Rest,Item,Goal),!.

prologPartitionList(Pos,Item,Goal,Passed,Unshared):-
	(prologPartitionList1(Pos,Item,Goal,Passed,Unshared)).

prologPartitionList1([],_,_,[],[]):-!.
prologPartitionList1([Item|Rest],Test,Goal,[Item|Passed],Failures):-
	not(not((Item=Test,Goal))),!,
	prologPartitionList1(Rest,Test,Goal,Passed,Failures).
prologPartitionList1([Failed|Rest],Item,Goal,Passed,[Failed|Failures]):-!,
	prologPartitionList1(Rest,Item,Goal,Passed,Failures).


remove_numbers([],[]):-!.
remove_numbers([N|L],LL):-number(N),remove_numbers(L,LL),!.
remove_numbers([N|L],[N|LL]):-remove_numbers(L,LL),!.

%:-include('sigma_header.pl').
% This file changes Well Founded Semantic Prolog into a more normaized form of WFS-Prolog

/*
For example:

Special predicates...

		not('instance'('Fido','Cat')) to

		~'instance''('Fido','Cat'))

Holdable (Hilog) predicates...

		nextto('Fido','Cat')

		'Q'(nextto,'Fido','Cat')

Meta/Aggregate Predicates

Logical Connective Predicates are not re-literated but explored the equal way Aggregates are

Inline-Predicates (Implemented outside the scope of the Logic Engine)


All predicates have an additional property about how negation is preformed:

Eigther by failure or by explicit negative forms

Explicit negative forms are created internally by taking the root operator and appending '~' to it's name
Failure based predicates are wrapped with 'not/1' and wont be checked for explicit negation


possible(X) -> (X ; not(not(X))
consistent(X) -> (not(not(X))
forall(X) -> (X , not(not(X)) )
any(X) -> (X)
exists(X) -> (X ; not(not(X) )
never(X) -> not(X)
known(X) -> (X ; not(X))

matrix (X/not(X))  (and/or)  (not(X)/not(not(X)))


   X 
  ~X
  -X
 -~X
   X
  ~X
  -X
 -~X




X ; not(X)
not(X) ; not(X)
not(not(X)) ; not(X)
X ; not(X)
not(X) ; not(X)
not(not(X)) ; not(X)


*/


/*
:-module(
      sigma_krlog,
      [krlog_to_prolog/2,prolog_to_krlog/2,pterm_to_bt/3]
      ).
*/

% ===================================================================
% krlog_to_prolog(KRProlog,Prolog)  Converts And/Or/Implies terms to Proper Prolog
% ====================================================================

krlog_to_prolog(A,A):-isSlot(A),!.
krlog_to_prolog(<=>(A,B),<=>(AA,BB)):- !,
      krlog_to_prolog(A,AA),
      krlog_to_prolog(B,BB).
krlog_to_prolog(if(A,B),':-'(AA,BB)):- !,
      krlog_to_prolog(A,AA),
      krlog_to_prolog(B,BB).
krlog_to_prolog(entails(B,A),':-'(AA,BB)):- !,
      krlog_to_prolog(A,AA),
      krlog_to_prolog(B,BB).
krlog_to_prolog(LIST,OLIST):- is_list(LIST),!,
      krlog_to_prolog_l(LIST,OLIST).
krlog_to_prolog(=>(A,B),O):- !,
      krlog_to_prolog((B:-A),O).


krlog_to_prolog(consistent(A),consistent(O)):- !,           
      krlog_to_prolog(A,O).
krlog_to_prolog(inconsistent(A),consistent(O)):- !,           
      krlog_to_prolog(not(A),O).
krlog_to_prolog(known(A),known(O)):- !,           
      krlog_to_prolog((A),O).

krlog_to_prolog(and(A,B),O):- !,
      krlog_to_prolog((A,B),O).
krlog_to_prolog(and(A,B,C),O):- !,
      krlog_to_prolog((A,B,C),O).
krlog_to_prolog(and(A,B,C,D),O):- !,
      krlog_to_prolog((A,B,C,D),O).
krlog_to_prolog(and(A,B,C,D,E),O):- !,
      krlog_to_prolog((A,B,C,D,E),O).
krlog_to_prolog(and(A,B,C,D,E,F),O):- !,
      krlog_to_prolog((A,B,C,D,E,F),O).
krlog_to_prolog(or(A,B),O):- !,
      krlog_to_prolog((A;B),O).
krlog_to_prolog(or(A,B,C),O):- !,
      krlog_to_prolog((A;B;C),O).
krlog_to_prolog(or(A,B,C,D),O):- !,
      krlog_to_prolog((A;B;C;D),O).
krlog_to_prolog(or(A,B,C,D,E),O):- !,
      krlog_to_prolog((A;B;C;D;E),O).
krlog_to_prolog(or(A,B,C,D,E,F),O):- !,
      krlog_to_prolog((A;B;C;D;E;F),O).

krlog_to_prolog(not(A),not(AA)):- !,
      krlog_to_prolog(A,AA).
krlog_to_prolog('neg'(A),'neg'(AA)):- !,
      krlog_to_prolog(A,AA).
krlog_to_prolog('not'(A),'not'(AA)):- !,
      krlog_to_prolog(A,AA).
krlog_to_prolog('not'(A),'not'(AA)):- !,
      krlog_to_prolog(A,AA).

krlog_to_prolog(','(A,B),','(AA,BB)):- !,
      krlog_to_prolog(A,AA),
      krlog_to_prolog(B,BB).

krlog_to_prolog(';'(A,B),';'(AA,BB)):- !,
      krlog_to_prolog(A,AA),
      krlog_to_prolog(B,BB).

krlog_to_prolog(':-'((A1,A2),B), O):- isNonVar(A1),!,
      krlog_to_prolog((':-'(A1,B),':-'(A2,B)),O).

krlog_to_prolog(':-'((A1;A2),B), O):-  isNonVar(A1),!,
      krlog_to_prolog((':-'(A1,B),':-'(A2,B)),O).

krlog_to_prolog(':-'(A,B),':-'(AA,BB)):- !,
      krlog_to_prolog(A,AA),
      krlog_to_prolog(B,BB).

krlog_to_prolog(COMP,OCOMP):-compound(COMP),!,
      COMP=..[F|ARGS],
      krlog_to_prolog_l(ARGS,OARGS),
      OCOMP=..[F|OARGS].

krlog_to_prolog(A,A).


krlog_to_prolog_l([],[]):-!.
krlog_to_prolog_l([HK|TK],[HP|TP]):-!,
               krlog_to_prolog(HK,HP),
               krlog_to_prolog_l(TK,TP).


% ===================================================================
% prolog_to_krlog(KRProlog,Prolog)  Converts And/Or/Implies terms to Proper Prolog
% ====================================================================

prolog_to_krlog(A,A):-isSlot(A),!.

prolog_to_krlog(LIST,OLIST):- is_list(LIST),!,
      prolog_to_krlog_l(LIST,OLIST).
prolog_to_krlog(or(B,A),or(BB,AA)):- !,
      prolog_to_krlog(A,AA),
      prolog_to_krlog(B,BB),!.
prolog_to_krlog((B:-A),'=>'(BB,AA)):- !,
      prolog_to_krlog(A,AA),
      prolog_to_krlog(B,BB).
prolog_to_krlog((B:-true),BB):- !,
      prolog_to_krlog(B,BB).
prolog_to_krlog(<=>(B,A),<=>(BB,AA)):- !,
      prolog_to_krlog(A,AA),
      prolog_to_krlog(B,BB).
prolog_to_krlog(and(B,A),and(BB,AA)):- !,
      prolog_to_krlog(A,AA),
      prolog_to_krlog(B,BB).
prolog_to_krlog(=>(B,A),=>(BB,AA)):- !,
      prolog_to_krlog(A,AA),
      prolog_to_krlog(B,BB).
prolog_to_krlog(forall(B,A),forall(B,AA)):- !,
      prolog_to_krlog(A,AA).
prolog_to_krlog(exists(B,A),exists(B,AA)):- !,
      prolog_to_krlog(A,AA).
prolog_to_krlog((A,B),and(AA,BB)):- !,
      prolog_to_krlog(A,AA),
      prolog_to_krlog(B,BB).
prolog_to_krlog((A;B),or(AA,BB)):- !,
      prolog_to_krlog(A,AA),
      prolog_to_krlog(B,BB).
prolog_to_krlog(not(not(B)),consistent(BB)):- !,
      prolog_to_krlog(B,BB).
prolog_to_krlog(not(B),not(BB)):- !,
      prolog_to_krlog(B,BB),!.
prolog_to_krlog(call(B),(BB)):- !,
      prolog_to_krlog(B,BB).

prolog_to_krlog(COMP,OCOMP):-compound(COMP),
      COMP =.. [F,P|ARGS], memberchk(F,['M','N','O','P','Q','R','holds','holds']),
      prolog_to_krlog_l([P|ARGS],[PO|OARGS]),
      OCOMP =.. [holds,PO|OARGS].

prolog_to_krlog(COMP,not(OCOMP)):-compound(COMP),
      COMP=.. [F,P|ARGS], memberchk(F,['~M','~N','~O','~P','~Q','~R','~holds','~holds']),
      prolog_to_krlog_l([P|ARGS],[PO|OARGS]),
      OCOMP =.. [holds,PO|OARGS].


prolog_to_krlog(A,A).

prolog_to_krlog_l([],[]):-!.
prolog_to_krlog_l([HK|TK],[HP|TP]):-!,
               prolog_to_krlog(HK,HP),
               prolog_to_krlog_l(TK,TP).


conj_set(CNF,Set):-
	conjunctsToList(CNF,CNFList),
	disj_list(CNFList,Set).
	
disj_list([],[]).

disj_list([CNF|List],[Set|SetL]):-!,
	or_to_list(CNF,Set),
	disj_list(List,SetL).

or_to_list(or(A,B),CC):-
	or_to_list(A,AA),
	or_to_list(B,BB),
	append(AA,BB,CC).
	
or_to_list(CNF,[CNF]).


list_to_and([A],A):-!.
list_to_and([A|L],and(A,O)):-!,
		  list_to_and(L,O).




	             	 	
fdelete([],T,[]):-!.

fdelete([Replace|Rest],[H|T],Out):-
	functor(Replace,F,_),memberchk(F,[H|T]),!,
       fdelete(Rest,[H|T],Out),!.

fdelete([Replace|Rest],[H|T],[Replace|Out]):-!,
       fdelete(Rest,[H|T],Out),!.

fdelete([Replace|Rest],F,Out):-
	functor(Replace,F,_),!,%F=FF,
       fdelete(Rest,F,Out),!.

fdelete([Replace|Rest],F,[Replace|Out]):-
       fdelete(Rest,F,Out),!.


