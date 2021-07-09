:- module(cgp_common_logic, 
  [%run_tests/0, 
   convert_clif_to_cg/2]).


:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo/dcg_meta)).
:- use_module(library(logicmoo/util_bb_frame)).
% :- ensure_loaded(library(cgp_lib/cgp_swipl)).
:- use_module(library(logicmoo_clif)).



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
  add_mode(Mode, cg_quantz(EoF, ?(X)), Grok),
  do_one_var(Mode, EoF, X, Types, Asserts, Fixes).
do_one_var(Mode, EoF, X, [Number| Types], [Grok|Asserts], Fixes):-
  number(Number),
  add_mode(Mode, cg_quantz_num(EoF,Number,?(X)), Grok),
  do_one_var(Mode, EoF, X, Types, Asserts, Fixes).
do_one_var(Mode, EoF, X, [Type| Types], [cg_type(?(X),Type)|Asserts], Fixes):-
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
% chop_up/2 - chops up and replaces CLIF into CG
% ==========================================================================
chop_up(Stuff, Out):-
   chop_up(+, Stuff, Out).



% ==========================================================================
% chop_up/3 - Like chop_up/2 (chops up and replaces CLIF into CG) but takes a +/-
% ==========================================================================
chop_up(Mode, [ExistsOrForall, VarList, Stuff], Out):-
   member(ExistsOrForall, [exists, forall]), 
   do_varaibles(Mode, ExistsOrForall, VarList, Out1, NewVars), 
   subst_each(Stuff, NewVars, NewStuff), 
   chop_up(Mode, NewStuff, Out2), 
   unchop(Out1, Out2, Out).

chop_up(Mode, ['implies'|Stuff], Out) :- chop_up(Mode, ['=>'|Stuff], Out).
chop_up(Mode, ['if'|Stuff], Out) :- chop_up(Mode, ['=>'|Stuff], Out).

chop_up(_Mode, ['#'(quote), Mary], '#'(Mary)).
chop_up(_Mode, '$STRING'(S), S).


chop_up(+, [not, Stuff], Out) :- chop_up(-, Stuff, Out).
chop_up(-, [not, Stuff], Out) :- chop_up(+, Stuff, Out).

chop_up(Mode, [Type, Arg], Out) :- chop_up(Mode, ['Type', Arg, Type], Out).

chop_up(+, [and|Stuff],    Out )  :- chop_up_list(+, Stuff, Out).
chop_up(-, [and|Stuff], or(Out))  :- chop_up_list(-, Stuff, Out).
chop_up(+, [or|Stuff],  or(Out))  :- chop_up_list(+, Stuff, Out).
chop_up(-, [or|Stuff],     Out )  :- chop_up_list(-, Stuff, Out).


chop_up(Mode, ['=>', Arg1, Arg2], Out):-
  chop_up(Mode, Arg1, F1),flatten([F1],Out1),
  chop_up(Mode, Arg2, F2),flatten([F2],Out2),
  Out =.. ['cg_implies', Out1, Out2], !.


chop_up(Mode, [Name, Arg1, Arg2], Out):- is_cg_pred(Name, Pred), !, 
  chop_up(Mode, Arg1, Out1), 
  chop_up(Mode, Arg2, Out2),  
  Out =.. [Pred, Out1, Out2], !.

chop_up(Mode, [Pred|Args], Out):-  
  chop_up_list(+, Args, ArgsO), 
  (HOLDS =.. [cg_holds, Pred|ArgsO]), 
  add_mode(Mode, HOLDS, Out).

chop_up(_Mode, O, O).


is_cg_pred(Name, _):- \+ atom(Name), !, fail.
is_cg_pred('=>', 'cg_implies'):-!. 
is_cg_pred(Name, Pred):- downcase_atom(Name, NameDC), member(NameDC, [name, type]), atom_concat('cg_', NameDC, Pred), !.
is_cg_pred(Name, Pred):- downcase_atom(Name, Pred), atom_concat('cg_', _, Pred).

add_mode(-, - A, A).
add_mode(-, A, -A).
add_mode(_, A, A).

% ==========================================================================
% chop_up_list/3 is the maplist version of chop_up/3
% ==========================================================================
chop_up_list(Mode, Stuff, Out):- maplist(chop_up(Mode), Stuff, Out). 


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
   kif_to_term(String, Clif), 
   pprint_ecp(magenta, (?- run_1_test(String))), 
   pprint_ecp(yellow, clif=Clif), 
   convert_clif_to_cg(Clif, CG), 
   pprint_ecp(cyan, cg(CG)), 
   dmsg("================================================="), !.

test_logicmoo_cg_clif:- % notrace(update_changed_files),
  forall(cl_example(String), run_1_test(String)).

:- public(test_logicmoo_cg_clif/0).

:- add_history(test_logicmoo_cg_clif).

% write_list(L):- maplist(write, L).
% ?- compound(a(b)).  %Yes   ?-atom(a(b)) % No
% ?- compound([a]).  %Yes
% ?- compound(a).  % No 
% ?- compound(1).  % No 


% Convert all  ?(Name)  into  '$VAR'(UPPER)
qvar_to_vvar(I, O):- \+ compound(I), !, I=O.
qvar_to_vvar('?'(Name), '$VAR'(UPPER)):- upcase_atom(Name, UPPER), !.
qvar_to_vvar(I, O):-
  compound_name_arguments(I, F, ARGS), 
  maplist(qvar_to_vvar, ARGS, ArgsO), 
  compound_name_arguments(O, F, ArgsO).


% ==========================================================================
%% convert_clif_to_cg(+Clif, -CG)
%  Redoes Clif forms into CG forms
% ==========================================================================
convert_clif_to_cg(In, Out):-
  chop_up(In, Mid), 
  qvar_to_vvar(Mid, Mid2), 
  unnumbervars(Mid2, Out).


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



