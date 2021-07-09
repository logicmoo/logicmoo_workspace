% %%%%%%%%%%%%%%%
% Our user:portray(Logic) English helpers
% %%%%%%%%%%%%%%%

:- '$set_source_module'(mu).

:- use_module(library(logicmoo/portray_vars)).

/*
set_our_portray_level(N):- flag('$adv_pp_level', _, N).
:- set_our_portray_level(0).
get_current_portray_level(Level) :- flag('$adv_pp_level', Level, Level), Level=Level.
:- export(get_current_portray_level/1).
*/

%adv_pretty_print(Term):- !, fmt90(Term).

% adv_pretty_print_goal(Term, Options).

% adv_pretty_print(Term):- format(current_output, '~w', [Term]).
adv_pretty_print(Term):-
   notrace((setup_call_cleanup(
        (flag('$adv_pp_level', Level, Level+1), push_operators([op(0, xfx, props), op(0, xfx, type)], Undo)),
           \+ \+ (portray_vars:pretty_numbervars(Term, Term2), adv_pretty_print(Level, Term2)),
          (pop_operators(Undo), flag('$adv_pp_level', _, Level))))), !.

adv_pretty_print(Level, Term):-
  (is_list(Term) ; (Level ==0)), !,
   prolog_pprint(Term, [ portray_goal(mu:adv_pretty_print_goal)]), !.
adv_pretty_print(_Level, Term):-
  is_logic(Term), logic2english(i, Term, Text),
  write(Text), !.
adv_pretty_print(Level, Term):- \+ is_list(Term), Level>0, Level < 4,
 flag_level_compare('english', >(0)),
 (flag_level_compare('english', >(1))
  -> our_portray_english('~s', Term);
   print_english_simple_only('~s', Term)), !.
adv_pretty_print(Level, Term):- compound(Term),
  setup_call_cleanup(
    flag('$adv_pp_level', Level, Level-1),
    prolog_pprint_tree(Term),
    flag('$adv_pp_level', Level, Level)), !.
adv_pretty_print(_Level, Term):- fmt90(Term), !.

adv_pretty_print_goal(Term, _Options):- adv_prolog_portray(Term), !.

our_portray_english(Fmt, Logic):-
 english_codes(Logic, Codes),
   format(Fmt, [Codes]), !.

print_english_simple_only(Fmt, Logic):-
 english_codes(Logic, Codes),
 was_simple_english_line(Codes),
 format(Fmt, [Codes]).

english_codes(Logic, Codes):-
 once(Agent = self ; mu_current_agent(Agent)),
 with_output_to(codes(SCodes), print_english(Agent, Logic)), !,
 trim_eols(SCodes, Codes), !.

trim_eols(String, Codes):- append(LString, [N], String), (N==13;N==10), !, trim_eols(LString, Codes).
trim_eols(Codes, Codes).

was_simple_english_line(O):- any_to_codelist(O, CL), length(CL, L), was_simple_english_line_0(L, CL), !.

was_simple_english_line_0(L, _String):- L < 3, !, fail.
was_simple_english_line_0(_, String):- last(String, N), member(N, `}`), !, fail.
was_simple_english_line_0(_, String):-
 freeze(C, member(C, `\n\r[{?`)),
 \+ member(C, String), !.

:- set_prolog_flag(expect_pfc_file, never).

% Called from portary and we _might_insert english also we _might_ fail if we are not supposed to do anything
adv_prolog_portray(Term):- var(Term), !, fail.
adv_prolog_portray(_   ):- flag('english', X, X), X < 1, !, fail.
adv_prolog_portray(Term):- string(Term), !, portray_string(string, Term).
adv_prolog_portray(Term):- \+ compound(Term), !, fail.
adv_prolog_portray(Term):- is_charlist(Term), !, portray_string(chars, Term).
adv_prolog_portray(Term):- is_codelist(Term), !, portray_string(codes, Term).
adv_prolog_portray(Term):- is_list(Term), !, fail, prolog_pprint(Term, [ portray_goal(mu:adv_pretty_print_goal)]).
%adv_prolog_portray(Term):- safe_functor(Term, i7_term, 2), !, display(Term), !.
adv_prolog_portray(Term):- safe_functor(Term, i7_term, 2), !, writeq(Term), !.
adv_prolog_portray( A=B):- (var(A);var(B)), !, fail.

adv_prolog_portray(Term):- ground(Term), \+ sub_term('$VAR'(_), Term), !, adv_prolog_portray_now(Term).

adv_prolog_portray_simple_only(Term):-
  is_type_functor(Type, Term), !,
  format(atom(Fmt), '{|i7||<~w> ~~s |}', [Type]),
  print_english_simple_only(Fmt, Term), !.

adv_prolog_portray_now(Term):- fail,
 \+ ( nb_current('$inprint_message', Messages), Messages\==[] ),
 \+ tracing, % fail,
 \+ \+ setup_call_cleanup(
      (flag('$adv_pp_level', Level, Level+2),
       flag('english', ELevel, ELevel-1)), % put a little less English on it
         (Level < 3, portray_vars:pretty_numbervars(Term, Term2), mu:adv_pretty_print(Level, Term2)),
        (flag('$adv_pp_level', _, Level),
         flag('english', _, ELevel))), !.

portray_string(Type, Term):- current_prolog_flag(back_quotes, Type), format('`~s`', [Term]).
portray_string(Type, Term):- current_prolog_flag(double_quotes, Type), format('"~s"', [Term]).


:- dynamic user:portray/1.
:- multifile user:portray/1.
:- module_transparent user:portray/1.

:- thread_local(t_l:no_english/0).

adv_prolog_portray_hook(Term) :- \+ ( nb_current('$inprint_message', Messages), Messages\==[] ), \+ tracing, \+ t_l:no_english, adv_prolog_portray(Term), !.
adv_prolog_portray_hook(Term) :- tracing, is_list(Term), member(E, Term), compound(E),
  ((E = inst(Some), format('[~w|...]', [inst(Some)]));(E = structure_label(Some), format('[~w|...]', [Some]))), !.

user:portray(Term):- notrace(adv_prolog_portray_hook(Term)), !.

no_memlists(Term):- fail, simplify_dbug(Term,Term1), map_tree_pred(simplify_memlists,Term1,Term2),!, Term\=@=Term2, writeq(Term2),!.

map_tree_pred(Pred,Arg1,Arg2):- call(Pred,Arg1,Arg2),!.
map_tree_pred(_ ,Arg1,Arg2):- \+ compound(Arg1), !, Arg2=Arg1.
map_tree_pred(Pred,Arg1,Arg2):- 
  compound_name_arguments(Arg1,F1,ArgS1),
  must_maplist(map_tree_pred(Pred),ArgS1,ArgS2),
  compound_name_arguments(Arg2,F1,ArgS2).

user:portray(Term):- notrace(no_memlists(Term)), !.

%:- set_prolog_flag(debugger_write_options,[quoted(true),portray(true),max_depth(5000),attributes(dots)]).

