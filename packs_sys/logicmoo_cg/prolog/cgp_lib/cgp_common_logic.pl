:- module(cgp_common_logic, 
  [%run_tests/0, 
   test_logicmoo_cg_clif/0,
   convert_clif_to_cg/2]).

:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo/dcg_meta)).
:- use_module(library(logicmoo/util_bb_frame)).
:- ensure_loaded(library(cgp_lib/cgp_swipl)).
:- use_module(library(logicmoo_clif)).
:- cgp_common_logic:import(dcg_basics:eol/2).

%:-ensure_loaded('cgp_common_logic.plt').

% ==========================================================================
% do_varaibles/5 replaces the VARS in things like (exists ...VARS... Stuff)
% ==========================================================================
do_varaibles(Mode, EoF, Var, Asserts, Fixes):- \+ is_list(Var), !, 
  do_varaibles(Mode, EoF, [Var], Asserts, Fixes).

do_varaibles(Mode, EoF, Vars, Asserts, Fixes):- is_list(Vars), !, 
   maplist(do_one_var(Mode, EoF), _, Vars, Assert, Fix), 
   flatten(Assert, Asserts), 
   flatten(Fix, Fixes), 
   !.
   

do_one_var(Mode, EoF, X, Var, Asserts, Fixes):- \+ is_list(Var), !, 
  do_one_var(Mode, EoF, X, [Var], Asserts, Fixes).

do_one_var(Mode, EoF, X, [VarName| Types], [Grok|Asserts], [Fix|Fixes]):-
  \+ number(VarName),
  var(X), cg_var_name(VarName, X, Fix), 
  add_mode(Mode, cg_quantz(EoF, X), Grok), 
  do_one_var(Mode, EoF, X, Types, Asserts, Fixes).
do_one_var(Mode, EoF, X, [Number| Types], [Grok|Asserts], Fixes):-
  number(Number),
  add_mode(Mode, cg_quantz_num(EoF,Number,'?'(X)), Grok),
  do_one_var(Mode, EoF, X, Types, Asserts, Fixes).
do_one_var(Mode, EoF, X, [Type| Types], [cg_type('?'(X),Type)|Asserts], Fixes):-
  do_one_var(Mode, EoF, X, Types, Asserts, Fixes).
do_one_var(_Mode, _EoF, _X, [], [], []).

cg_var_name('?'(X), X, []):-!.
cg_var_name(X, X, [X = '?'(X)]).

% ==========================================================================
% unchop/3 - unchops (joins) things into a list
% ==========================================================================
unchop(In1, In2, Out):- flatten([In1, In2], Out), !.
unchop(In1, In2, Out):-
  listify_h(In1, L1), 
  listify_h(In2, L2), 
  append(L1, L2, Out).

listify_h(L1, L2):- flatten([L1], L2), !.
listify_h(L1, L2):- listify(L1, L2), !.

% ==========================================================================
% chop_up_clif/2 - chops up and replaces CLIF into CG
% ==========================================================================

chop_up_clif(Stuff, Out):- chop_up_clif(Stuff,Stuff, Out).

chop_up_clif(OF, Stuff, Out):-
   chop_up_clif(OF, +, Stuff, Out).


is_var_with_name(X,N):- var(X),!,get_var_name(X,N).
is_var_with_name('$VAR'(Var),N):-!, is_var_with_name(Var,N).
is_var_with_name('?'(Var),N):-is_var_with_name(Var,N).
is_var_with_name(N,N).

% ==========================================================================
% chop_up_clif/3 - Like chop_up_clif/2 (chops up and replaces CLIF into CG) but takes a +/-
% ==========================================================================

chop_up_clif(_OF, _Mode, (Var), '$VAR'(Name)):- is_ftVar(Var), is_var_with_name(Var,Name),!.
chop_up_clif(_OF, _Mode, Var, Out):- var(Var),!, Out = Var.
chop_up_clif(_OF, _Mode, '$VAR'(Name), '$VAR'(Name)):-!.
chop_up_clif(OF, Mode, '?'(Var), Out):- !, chop_up_clif(OF, Mode, '$VAR'(Var), Out).
chop_up_clif(OF, Mode, [Var|Stuff], Out):- var(Var),!,chop_up_clif(OF, Mode, [holds,Var|Stuff], Out).
chop_up_clif(OF, Mode, [ExistsOrForall, VarList, Stuff], Out):- 
   nonvar(ExistsOrForall),
   member(ExistsOrForall, [exists, forall]), 
   do_varaibles(Mode, ExistsOrForall, VarList, Out1, NewVars), 
   subst_each(Stuff, NewVars, NewStuff), 
   chop_up_clif(OF, Mode, NewStuff, Out2), 
   unchop(Out1, Out2, Out).

chop_up_clif(OF, Mode, ['implies'|Stuff], Out) :- chop_up_clif(OF, Mode, ['=>'|Stuff], Out).
chop_up_clif(OF, Mode, ['if'|Stuff], Out) :- chop_up_clif(OF, Mode, ['=>'|Stuff], Out).

chop_up_clif(OF, Mode, ^(X,Y), Out):- !,
  chop_up_clif(OF, Mode, exists(X,Y), Out).

chop_up_clif(OF, Mode, object(_Frame,Var,Type,countable,na,eq,1), Out):- 
  Type \== '?',!,
  chop_up_clif(OF, Mode, isA(Var,Type), Out).

   
chop_up_clif(OF, Mode, intrans_pred(Type1,Type2,Pred,Arg1), Out):- 
  chop_up_clif(OF, Mode, (isA(Arg1,Type1),isA(Arg1,Type2),[Pred,Arg1]), Out).

chop_up_clif(OF, Mode, generic_pred(Type,Pred,Arg1,Arg2), Out):- 
  chop_up_clif(OF, Mode, (isA(Arg1,Type),[Pred,Arg1,Arg2]), Out).

chop_up_clif(OF, Mode, property(Var,Type,adj), Out):- 
  Type \== '?',!,
  chop_up_clif(OF, Mode, property(Var,Type), Out).

chop_up_clif(OF, Mode, object(_Frame,Var,Type,dom,na,na,na), Out):- 
  Type \== '?',!,
  chop_up_clif(OF, Mode, isA(Var,Type), Out).

chop_up_clif(OF, Mode, isa(Var,Type), Out):- 
   chop_up_clif(OF, Mode, isA(Var,Type), Out).

chop_up_clif(OF, Mode, ti(Type,Var), Out):- 
   chop_up_clif(OF, Mode, isA(Var,Type), Out).

chop_up_clif(OF, Mode, isA(Var,Type), Out):- 
  chop_up_clif(OF, Mode, cg_type(Var,Type), Out).

chop_up_clif(OF, Mode, [predicate,_Frame,_Exists_Be,Verb|Args], Out):- !,
  chop_up_clif(OF, Mode, [Verb|Args], Out).



chop_up_clif(OF, Mode, :-(X,Y), Out):- !,
  chop_up_clif(OF, Mode, if(Y,X), Out).

chop_up_clif(OF, Mode, relation(_Frame,X,of,Y), Out):- !,
  chop_up_clif(OF, Mode, of(X,Y), Out).

chop_up_clif(OF, Mode, (X,Y), Out):- !,  pred_juncts_to_list(',',(X,Y),List),
  chop_up_clif(OF, Mode,[and|List], Out).

chop_up_clif(OF, Mode, '&'(X,Y), Out):- !,  pred_juncts_to_list('&',(X,Y),List),
  chop_up_clif(OF, Mode,[and|List], Out).



chop_up_clif(_OF, _Mode, ['#'(quote), Mary], '#'(Mary)).
chop_up_clif(_OF, _Mode, '$STRING'(S), S).
chop_up_clif(_OF, _Mode, 'named'(S), S).


chop_up_clif(OF, +, [not, Stuff], Out) :- chop_up_clif(OF, -, Stuff, Out).
chop_up_clif(OF, -, [not, Stuff], Out) :- chop_up_clif(OF, +, Stuff, Out).

chop_up_clif(OF, Mode, [Type, Arg], Out) :- var(Arg), nonvar(Type), chop_up_clif(OF, Mode, ['Type', Arg, Type], Out).

chop_up_clif(OF, +, [and|Stuff],    Out )  :- chop_up_list(OF, +, Stuff, Out).
chop_up_clif(OF, -, [and|Stuff], or(Out))  :- chop_up_list(OF, -, Stuff, Out).
chop_up_clif(OF, +, [or|Stuff],  or(Out))  :- chop_up_list(OF, +, Stuff, Out).
chop_up_clif(OF, -, [or|Stuff],     Out )  :- chop_up_list(OF, -, Stuff, Out).


chop_up_clif(OF, Mode, ['=>', Arg1, Arg2], Out):-
  chop_up_clif(OF, Mode, Arg1, F1),flatten([F1],Out1),
  chop_up_clif(OF, Mode, Arg2, F2),flatten([F2],Out2),
  Out =.. ['cg_implies', Out1, Out2], !.


chop_up_clif(OF, Mode, [Name, Arg1, Arg2], Out):- is_cg_pred(Name, Pred), !, 
  chop_up_clif(OF, Mode, Arg1, Out1), 
  chop_up_clif(OF, Mode, Arg2, Out2),  
  Out =.. [Pred, Out1, Out2], !.

chop_up_clif(OF, Mode, [Pred|Args], Out):-  
  chop_up_list(OF, +, Args, ArgsO), 
  (HOLDS =.. [cg_holds, Pred|ArgsO]), 
  add_mode(Mode, HOLDS, Out).

chop_up_clif(OF, Mode, C, Out):- compound(C), \+ is_list(C), compound_name_arguments(C,F,A),
  chop_up_clif(OF, Mode, [F|A], Out).

chop_up_clif(_OF, _Mode, O, O).

  


is_cg_pred(Name, _):- \+ atom(Name), !, fail.
is_cg_pred('=>', 'cg_implies'):-!. 
is_cg_pred(Name, Pred):- downcase_atom(Name, NameDC), member(NameDC, [name, type]), atom_concat('cg_', NameDC, Pred), !.
is_cg_pred(Name, Pred):- downcase_atom(Name, Pred), atom_concat('cg_', _, Pred).

add_mode(-, - A, A).
add_mode(-, A, -A).
add_mode(_, A, A).

% ==========================================================================
% chop_up_list/3 is the maplist version of chop_up_clif/3
% ==========================================================================
chop_up_list(OF, Mode, Stuff, Out):- maplist(chop_up_clif(OF, Mode), Stuff, Out). 


% ==========================================================================
%% kif_to_term(+InS, -Clif)
% Converts InS string into Clif
% ==========================================================================
kif_to_term(InS, Clif):-
  locally(t_l:sreader_options(logicmoo_read_kif, true), 
      parse_sexpr(string(InS), Clif)), !.
 
 
% ==========================================================================
%% run_1_test(+String)
% Converts InS string into Clif
% ==========================================================================
run_1_test(String):-
   write('\n\n\n'), 
   dmsg("================================================="), 
  mpred_test(mort(cgp_common_logic:kif_to_term(String, Clif))),
  pprint_ecp(magenta, (?- run_1_test(String))), 
  pprint_ecp(yellow, clif=Clif), 
  mpred_test(mort(cgp_common_logic:convert_clif_to_cg(Clif, CG))),
   pprint_ecp(cyan, cg=(CG)), 
   ensure_fvars(CG,FVOut),
   nl,
   pprint_ecp(cyan, cgflat=(FVOut)), 
   dmsg("================================================="), !.

test_logicmoo_cg_clif:- notrace(update_changed_files),
  
  forall(cl_example(String), run_1_test(String)).

:- system:import(test_logicmoo_cg_clif/0).

:- public(test_logicmoo_cg_clif/0).

:- add_history(test_logicmoo_cg_clif).

% write_list(L):- maplist(write, L).
% ?- compound(a(b)).  %Yes   ?-atom(a(b)) % No
% ?- compound([a]).  %Yes
% ?- compound(a).  % No 
% ?- compound(1).  % No 


% Convert all  '?'(Name)  into  '$VAR'(UPPER)
qvar_to_vvar(I, O):- \+ compound(I), !, I=O.
qvar_to_vvar('?'(Name), '$VAR'(UPPER)):- atomic(Name), upcase_atom(Name, UPPER), !.
qvar_to_vvar(I, O):-
  compound_name_arguments(I, F, ARGS), 
  maplist(qvar_to_vvar, ARGS, ArgsO), 
  compound_name_arguments(O, F, ArgsO).

renumbervars_with_names_l(In0,In):- 
  guess_varnames(In0),
  term_variables(In0,Vs),
  logicmoo_util_terms:pred_subst(cgp_common_logic:var_q_var(Vs),In0,In).

var_q_var(_Vs,V,'$VAR'(Name)):- var(V),!,get_var_name(V,Name).
var_q_var(_Vs,'$VAR'(V),'$VAR'(V)):- !.
var_q_var(_Vs,'?'(V),'$VAR'(V)):- !.

var_k_var(Var):- get_var_name(Var,Name),(Var = ('?'(Name))). 

% ==========================================================================
%% convert_clif_to_cg(+Clif, -CG)
%  Redoes Clif forms into CG forms
% ==========================================================================
convert_clif_to_cg(In0, CG):-
  nl,
  guess_varnames(In0),
  renumbervars_with_names_l(In0,In),
  display(renumbervars_with_names(In0,In)),
  nl,
  chop_up_clif(In, Mid), 
  qvar_to_vvar(Mid, Mid2), 
  unnumbervars(Mid2, Out),!,  
  to_out_cg(Out,OutCG),
  cleanup_cg(OutCG,CG).

ensure_fvars(OutCG,FVOut):- \+ compound(OutCG),!,OutCG=FVOut.
ensure_fvars(OutCG,FVOut):- arg(1,OutCG,O),is_frmvar(O),!,OutCG=FVOut.
ensure_fvars(OutCG,FVOut):- is_cg_frame_var(OutCG,_),OutCG=FVOut.
ensure_fvars(OutCG,FVOut):- make_fv(FV), frame_to_db(FV,OutCG,FVOut).

is_frmvar(O):- is_ftVar(O),!.
%is_frmvar(O):- is_cg_frame(O),!.

make_fv(FV):-
  gensym('CGIF_',Sym),
  %debug_var(Sym,FV).
  %FV='$VAR'(Sym).
  FV='cgf'(Sym).

to_out_cg(Out,OutCG):- var(Out),!,OutCG = cg(Out).
to_out_cg(cg(Out),(Out)):-!.
to_out_cg((Out),(Out)):- nonvar(Out),!.

frame_to_db(FV,OutCG,FVOut):- frame_to_db(FV,0,OutCG,FVOut).

frame_to_db(_FV,_,P,P):- var(P).
frame_to_db(_,_,cgf(P),cgf(P)):-!.
frame_to_db(_,_,'$VAR'(P),'$VAR'(P)):-!.
frame_to_db(_,_,CGP,cg(FV,FVP)):- compound(CGP),CGP=cg(FV,FVP),!.
frame_to_db(FV,C,CGP,cg(FV,FVP)):- compound(CGP),CGP=cg(P),!, frame_to_db(FV,C,P,FVP).
%frame_to_db(FV,_,P,cg(FV,P)):- is_list(P),!.
%frame_to_db(FV,C,P,(FVP)):- is_list(P),!,maplist(frame_to_db(FV,C),P,FVP).
frame_to_db(FV,C,P,CJS):- is_list(P),!,maplist(frame_to_db(FV,C),P,FVP),list_to_conjuncts(',',FVP,CJS).
frame_to_db(FV,C,P,FVP):- number(C),C\==0,!,frame_to_db(FV,0,P,FVP).
frame_to_db(FV,:,P,FVP):- !, frame_to_db(FV,0,P,FVP).
frame_to_db(_,-,P,P):-!.
frame_to_db(_,+,P,P):-!.
frame_to_db(_,?,P,P):-!.
frame_to_db(FV,0,P,in_frame(FV,P)):- var(P).
frame_to_db(_, _,P,FVP):- \+ compound(P),!,FVP=P.
frame_to_db(_, _,P,FVP):- compound_name_arity(P,_,0),!,FVP=P.
frame_to_db(FV,C,-P,CJS):- !, frame_to_db(FV,C,P,CJS).
frame_to_db(FV,C,P,CJS):- P=..[F,E],frame_to_db(FV,C,E,M),!, CJS=..[F,M].
frame_to_db(FV,C,P,FVP):- compound(C),compound(P),compound_name_arity(C,_,A),compound_name_arity(P,F,A),!,
  compound_name_arguments(C,F,Ns),compound_name_arguments(P,F,As),
  maplist(frame_to_db(FV),Ns,As,FVPs), compound_name_arguments(FVP,F,FVPs).
frame_to_db(FV,_,P,FVP):- predicate_property(P,meta_predicate(Template)),!,frame_to_db(FV,Template,P,FVP).
frame_to_db(_, _,P,FVP):- predicate_property(P,builtin),!,FVP=P.
frame_to_db(FV,_,P,FVP):- contains_var(FV,P),!,FVP=P.

frame_to_db(FV,C,P,FVPO):- arg(_,P,E),is_cg_frame(E),nonvar(E),E\=cgf(_),
  once(is_cg_frame_var(E,F);make_fv(F)),into_cgvar(F,CGVAR),!,
  subst(P,E,CGVAR,M),frame_to_db(FV,C,M,FVP),
  frame_to_db(F,C,E,FVPE),
  conjoin(FVP,FVPE,FVPO).

frame_to_db(FV,_,P,FVP):- compound_name_arguments(P,F,FVPs), 
  maplist(frame_to_db(FV),FVPs,FVPsO),
  compound_name_arguments(FVP,F,[FV|FVPsO]),!.
frame_to_db(_, _,P,FVP):- FVP=P.

into_cgvar(F,CGVAR):- var(F),CGVAR=cgf(F).
into_cgvar(cgf(F),cgf(F)):-!.
is_cg_frame(E):- var(E),!,fail.
is_cg_frame(E):- is_list(E),!.
is_cg_frame(cg(_)):-!.
is_cg_frame(cgf(_)):-!.
is_cg_frame(cg(_,_)):-!.
is_cg_frame_var(E,_):- var(E),!,fail.
is_cg_frame_var(cgf(V),V):-!.
is_cg_frame_var(cg(V,_),V):-!.

% ==========================================================================
%% compound_name_arguments_sAfe(?Compound, ?Name ?Arguments)
%  Safely does compound_name_arguments/3 but with a special case
% ==========================================================================
compound_name_arguments_sAfe(F, F, []):- !. % special case
compound_name_arguments_sAfe(LpsM, F, ArgsO):- compound_name_arguments(LpsM, F, ArgsO).




/*
(documentation Hajj EnglishLanguage "The Pilgrimage to Mecca in Islam.  It is
the fifth obligatory Pillar of the Five Pillars of Islam for those who are
ablebodied and can afford to do pilgrimage to Mecca at least once in their
lifetime.  It takes place every year in the Islamic month of Dhu al-Hijjah.")
*/

cl_example("
(=>
  (and
    (attribute ?P Muslim)
    (capability Hajj agent ?P))

  (modalAttribute
    (exists (?H)
      (and
        (instance ?H Hajj)
        (agent ?H ?P)))
    Obligation))  ").
cl_example("
(exists (x y) (and (Red x) (not (Ball x)) (On x y) (not (and (Table y) (not (Blue y))))))").

cl_example('
(exists ((x Drive) (y Chevy) (z Old))
  (and (Person Bob) (City "St. Louis")
   (Agnt x Bob)(Dest x "St. Louis") (Thme x y) (Poss Bob y) (Attr y z) ))').

% If a cat is on a mat, then the cat is a happy pet.
cl_example("(not (exists ((x Cat) (y Mat)) (and (On x y)(not (exists z) (and (Pet x) (Happy z) (Attr x z))))))").

% For every cat x and every mat y and x is on y, then x is a happy pet.
cl_example("(forall ((x Cat) (y Mat))(if (On x y) (and (Pet x) (exists ((z Happy)) (Attr x z)))))").

cl_example("(exists ((r Relation)) (and (Familial r) (r Bob Sue)))").

cl_example("(exists ( ?y ) (implies (isa ?y Mat)  (Pred ?y ?z)))").

% a cat on a mat
cl_example("(exists ((?x Cat) (?y Mat)) (On ?x ?y))").

cl_example("(not (exists ((?x Cat)) (not (exists ((?y Mat)) (On ?x ?y)))))").

%all cats are on a mat
cl_example("(forall ((?x Cat)) (exists ((?y Mat)) (On ?x ?y)))").

%there are two cats on a mat.
cl_example("(exists ((?y Mat)(?x Cat)(?z Cat)) (and (On ?x ?y)(On ?z ?y)(different ?x ?z)))").

%john goes to Boston by bus... or something like that
cl_example("
(exists ((x Go) (y Bus))
      (and (Person John) (city Boston)
           (Agnt x John) (Dest x Boston) (Inst x y)))").

cl_example("
(exists ((?x Go) (?y Person) (?z City) (?w Bus))
        (and (Name ?y John) (Name ?z Boston)
             (Agnt ?x ?y) (Dest ?x ?z) (Inst ?x ?w)))").




%he believes that mary wants to marry a sailor
cl_example("
(exists ((?x1 person) (?x2 believe))
   (and (expr ?x2 ?x1)
        (thme ?x2
           (exists ((?x3 person) (?x4 want) (?x8 situation))
              (and (name ?x3 'Mary) (expr ?x4 ?x3) (thme ?x4 ?x8)
                   (dscr ?x8 (exists ((?x5 marry) (?x6 sailor))
                                (and (Agnt ?x5 ?x3) (Thme ?x5 ?x6)))))))))").
% cg_reader_tests


skip_cl_example("
(exists ((?x person) (?y rock) (?z place) (?w hard))
        (and (betw ?y ?z ?x) (attr ?z ?w)))").

skip_cl_example(  "
(For a number x, a number y is ((x+7) / sqrt(7)))").



