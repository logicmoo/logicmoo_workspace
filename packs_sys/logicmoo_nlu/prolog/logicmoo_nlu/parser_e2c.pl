% ===================================================================
% File 'parser_e2c. pl'
% Purpose: English to KIF conversions from SWI-Prolog
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users. sourceforge. net ;
% Version: 'parser_e2c. pl' 1. 0. 0
% Revision:  $Revision: 1. 3 $
% Revised At:   $Date: 2012/06/06 15:43:15 $
% ===================================================================

:-module(parser_e2c, []).

:- set_module(class(library)).


/*
  Around 10% of the 1500 lines of code are from Bratko chapter 17 page 455.
   This comes from Pereira and Warren paper, AI journal, 1980
   To see original: https://github.com/logicmoo/logicmoo_nlu/blob/master/prolog/logicmoo_nlu/parser_bratko.pl

  What is fun and odd about Bratko's version was it goes straight from Text to Deep Logical-Form :)

   */
:- set_how_virtualize_file(false).

:-
 op(1199, fx, ('==>')),
 op(1190, xfx, ('::::')),
 op(1180, xfx, ('==>')),
 op(1170, xfx, '<==>'),
 op(1160, xfx, ('<-')),
 op(1150, xfx, '=>'),
 op(1140, xfx, '<='),
 op(1130, xfx, '<=>'),
 op(600, yfx, '&'),
 op(600, yfx, 'v'),
 op(350, xfx, 'xor'),
 op(300, fx, '~'),
 op(300, fx, '-').


:- discontiguous(test_e2c/2).
:- export(test_e2c/2).
:- multifile(test_e2c/2).
:- dynamic(test_e2c/2).
:- system:import(test_e2c/2).
:- parser_all:import(test_e2c/2).


:- discontiguous(test_e2c/1).
:- export(test_e2c/1).
:- multifile(test_e2c/1).
:- dynamic(test_e2c/1).
:- system:import(test_e2c/1).
:- parser_all:import(test_e2c/1).

:- op(50, xfx, +).
:- op(1200, xfx, -->).
:- op(100, fx, '`').

:- use_module(library(lists)).
%:- '$hide'(lists:append(_, _, _)).
/*
*/
:- use_module(library(check)).
%:- abolish(check:list_undefined, 0).
%:- asserta((check:list_undefined:-!)).
%:- parser_chat80:export(parser_chat80:theText1/3).
%:- import(parser_chat80:theText1/3).

%:- parser_chat80:export(parser_chat80:theText80/3).
%:- import(parser_chat80:theText80/3).
:- reexport(parser_chat80).
:- use_module(library(logicmoo_nlu/parser_tokenize)).
:- reexport(parser_tokenize).
:- reexport(parser_pldata).
%:- use_module(pldata(clex_iface)).
%:- use_module(parser_chat80, [plt/0, print_tree/1]).
%:- mu:export(mu:nop/3).
%:- import(mu:nop/3).
nop(_Goal)-->[].

make_dcg_test_stub(_, _, A):- A < 3.
make_dcg_test_stub(M, F, _):- current_predicate(M:F/1), !. % no place for it
make_dcg_test_stub(_, F, _):- current_predicate(F/1), !. % still no place for it?
make_dcg_test_stub(M, F, A):-
   functor(PrologHead, F, A),
   PrologHead =.. [F|PrologHeadArgs],
   append(_, [Out, W, E], PrologHeadArgs),
   TextHead =.. [F, S],
   (Assert = ( system:TextHead:- M:(make, to_wordlist_atoms(S, W), call_print_reply(Out, M:PrologHead), !, E=[]))),
   M:assert_if_new(Assert).

:- system:import(make_dcg_test_stub/3).

decl_is_dcg(MP):- strip_module(MP, M, P), (P=F/A;(P=F//A2,
  A is A2+2);(atom(P), F=P, A=2);(compound(P), compound_name_arity(P, F, A2), A is A2+2)), !,
  decl_is_dcg(M, F, A).
decl_is_dcg(M, F, A):- ain(baseKB:mpred_props(M, F, A, prologDcg)), ain(baseKB:mpred_props(M, F, A, prologOnly)), make_dcg_test_stub(M, F, A).

:- decl_is_dcg(optionalText1/3).
:- decl_is_dcg(theText1/3).
:- decl_is_dcg(phrase/3).

:- multifile(system:term_expansion/4).
:- dynamic(system:term_expansion/4).
%system:term_expansion(MH --> _, _, _, _):- notrace((strip_module(MH, M, H), decl_is_dcg(M:H), fail)).

% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% TESTING %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================

system:t33:- cls, make, t33s.
system:t33s:- cls, test_e2c([sanity]).
system:t33t:- cls, test_e2c([riddle]).
system:t33ts:- cls, test_e2c([]). % all

system:t33f:-
  mpred_trace_all,
  mpred_trace_exec,
  setup_call_cleanup(
     true,
     parser_e2c:forall(test_e2c(Sent, Type), show_failure(parser_fwd:add_nl_fwd(Sent, Type))),
     mpred_notrace_all),
  mpred_notrace_exec.


baseKB:sanity_test:- t33.

is_testing_e2c(_, _, []).
is_testing_e2c(_, Traits, + Include):- atom(Include), !, \+ \+ memberchk(Include, Traits).
is_testing_e2c(_, Traits, - Exclude):- atom(Exclude), !, \+ memberchk(Exclude, Traits).
is_testing_e2c(_, Traits, TestType):- atom(TestType), !, member(T, Traits), functor(T, TestType, _).
is_testing_e2c(S, _Traits, - CantHave):- string(CantHave), \+ atom_contains(S, CantHave).
is_testing_e2c(S, _Traits, + MustHave):- string(MustHave), atom_contains(S, MustHave).
% ANY or
is_testing_e2c(S, Traits, [Type|TestTypes]):- !,
  is_testing_e2c(S, Traits, Type);
  is_testing_e2c(S, Traits, TestTypes).
% AND
is_testing_e2c(S, Traits, Type1+Type2):- !,
  is_testing_e2c(S, Traits, Type1),
  is_testing_e2c(S, Traits, Type2).
% ALL BUT
is_testing_e2c(S, Traits, Type1-Type2):- !,
  is_testing_e2c(S, Traits, Type1),
  \+ is_testing_e2c(S, Traits, Type2).

run_e2c_test(S, _T):- e2c(S).

add_e2c(S):- add_e2c(S, sanity).
add_e2c(S, W):-  clause(test_e2c(S, W), true), !.
add_e2c(S, W):- listify(W, L), assertz(test_e2c(S, L)).


:- export(test_e2c/1).
:- export(test_e2c/2).
test_e2c(String) :- string(String), !, run_e2c_test(String, [requested]).
test_e2c(TestTypes) :-
  forall((test_e2c(S, T), is_testing_e2c(S, T, TestTypes)),
         (flatten([T, TestTypes], TestInfo), run_e2c_test(S, TestInfo))).



% ;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\ext\candc;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\ext\ape;W:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\prolog

% test_e2c(S, _T) :- \+ ground(S), !, fail.


% sanity = ran as a sanity test
% regression = ran as regression test
% feature = ran as feature test

test_e2c(S, TestInfo) :- nonvar(S), run_e2c_test(S, TestInfo).

test_e2c("His friends are liked by hers.", [bad_juju, sanity]).
test_e2c("Her friends are not liked by his.", [bad_juju, sanity]).
test_e2c("Do their friends like each other?", [bad_juju, feature]).

test_e2c("If ?X is rearing ?Y then ?X has ?Y.", [riddle(_), sanity]).
test_e2c("If ?X keeps ?Y then ?X has ?Y.", [riddle(_), sanity]).

test_e2c("There are 5 houses with five different colors.", [riddle(_), sanity]).
  test_e2c("There are 5 houses", [riddle_prep, sanity]).
  test_e2c("Each house has a different color", [riddle_prep, sanity]).
test_e2c("In each house lives a person with a different nationality.", [riddle(_), sanity]). % FAILS

test_e2c("These five owners drink a certain type of beverage, smoke a certain brand of cigar and keep a certain pet.", [riddle(3), sanity]). % FAILS
%vs
test_e2c("These five owners each drink a certain type of beverage.", [riddle(1), sanity]).
test_e2c("These five owners each smoke a certain brand of cigar.", [riddle(1), regression]). % FAILS
test_e2c("These five owners each keep a certain pet.", [riddle(1), sanity, regression]). % FAILS

test_e2c("No owners have the same pet, smoke the same brand of cigar or drink the same beverage.", [riddle(3), sanity]). % FAILS
%vs
test_e2c("No owners have the same pet.", [riddle(1), sanity, regression]).  % PASS
test_e2c("No owners smoke the same brand of cigar", [riddle(1), sanity, regression]).  % FAILS
test_e2c("No two owners drink the same kind of beverage.", [riddle(1), sanity]).

test_e2c("No two owners have the same pet.", [riddle(1), sanity, regression]).  % RESULTs?


test_e2c("The brit lives in the red house.", [riddle(_), sanity]).
test_e2c("The swede keeps dogs as pets.", [riddle(_), sanity]).
test_e2c("A dane drinks tea.", [riddle(_), sanity]).
test_e2c("The green house is on the left of the white house.", [riddle(_), sanity]).
test_e2c("The green house's owner drinks coffee.", [riddle(_), sanity]).

test_e2c("The person who smokes Pall Mall rears birds.", [riddle(_), sanity, regression]). % FAILS

test_e2c("The owner of the yellow house smokes Dunhill.", [riddle(_), sanity]).
test_e2c("The man living in the center house drinks milk.", [riddle(_), sanity]).
test_e2c("The Norwegian lives in the first house .", [riddle(_), sanity]).
test_e2c("The man who smokes Blends lives next to the one who keeps cats   .", [riddle(_), sanity]).
test_e2c("The man who keeps horses lives next to the man who smokes Dunhill.", [riddle(_), sanity]).
test_e2c("The owner who smokes BlueMaster drinks beer.", [riddle(_), sanity]).
test_e2c("The German smokes Prince.", [riddle(_), sanity]).
test_e2c("The Norwegian lives next to the blue house.", [riddle(_), sanity]).
test_e2c("The man who smokes Blends has a neighbor who drinks water.", [riddle(_), sanity]).
test_e2c("Who owns the fish?", [riddle(_), sanity]).

:- include(library(logicmoo_nlu/parser_tests)).

% test_e2c(S, _T) :- \+ ground(S), !, fail.

baseKB:feature_test(nlu_riddle):- riddle.
riddle :-  riddle(_AnyLevel).
riddle(Level) :- forall(test_e2c(Text, riddle(Level)), e2c(Text)).

:- thread_local(t_l:into_form_code/0).
:- asserta(t_l:into_form_code).


% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================

system:myb :- e2c.

:-export(e2c/0).
e2c :- locally(tracing80,
             with_no_assertions(lmconf:use_cyc_database,
                  locally(t_l:usePlTalk, (told, repeat, prompt_read('E2FC> ', U),
                            to_wordlist_atoms(U, WL), (WL==[ bye];WL==[end, '_', of, '_', file];e2c(WL)))))).

:-export(e2c/1).
e2c(Sentence):-
  with_error_to_predicate(nop, make),
   setup_call_cleanup(
   must(notrace((to_wordlist_atoms(Sentence, Words), fmt('?-'(e2c(Sentence)))))),
   (call_residue_vars(must(e2c_0(Words)), Vs), del_e2c_attributes(Vs)),
   true), !.
:-system:import(e2c/1).

:-export(e2c_0/1).
e2c_0(Words):-
  %cls, % ignore(also_show_chat80(Words)), !,
  ignore_must(e2c_0(Words, Reply)), 
  del_e2c_attributes(Reply),
  print_reply_colored(Reply), !.


:-export(e2c/2).
e2c(Sentence, Options):- callable(Options), set_e2c_options(Options), !, e2c(Sentence).
e2c(Sentence, Reply):- e2c(Sentence, [], Reply).
:-export(e2c/3).
e2c(Sentence, Options, Reply):-
 quietly(to_wordlist_atoms(Sentence, WL)), !,
 set_e2c_options(Options),
 call_residue_vars(e2c_0(WL, Reply), Vs), !,
 del_e2c_attributes(Reply+Vs), !.
:-system:import(e2c/2).

set_e2c_options(Options):- nb_setval('$e2c_options', Options).

:-export(e2c_0/2).
e2c_0(Sentence, Reply) :-
   % must_or_rtrace
   % set_prolog_flag(debugger_write_options, [quoted(true), portray(false), max_depth(50), attributes(portray)]),
   e2c_parse0(Sentence, LF), % deepen_pos?
   e2c_clausify_and_reply(LF, Reply).

e2c_0(Sentence,
   error('FAILED!!!!! small bug'(Sentence))):- ansifmt(red, rtrace(e2c_0(Sentence))).


:- assert_if_new(baseKB:mpred_prop(parser_e2c, e2c_parse, 2, prologOnly)).

e2c_parse(Sentence, LF):- cwc,
  quietly(to_wordlist_atoms(Sentence, WL)), !,
  e2c_parse0(WL, LF),
  del_e2c_attributes(LF).

:- assert_if_new(baseKB:mpred_prop(parser_e2c, e2c_parse0, 2, prologOnly)).

e2c_parse0(WL, LF):-
  b_setval('$variable_names', []),
  retractall(t_l:usePlTalk),
  retractall(t_l:useAltPOS), !,
  e2c_parse2(WL, LF).

%e2c_parse2(WL, LF):- deepen_pos(utterance(_How, LF, WL, []))-> ! ; e2c_parse3(WL, LF).

e2c_parse2(WL, LF):- no_repeats(LF, deepen_pos(utterance(_How, LF, WL, []))).
/*
e2c_parse2(WL, LF):- fail, deepen_pos(e2c_parse3(WL, LF)).


e2c_parse3(Sentence, Reply):- notrace(into_text80(Sentence, U)), !,
  also_chat80(U, Res),
  once((rewrite_result(_SF, verb, _VF, Res, Reply))).
*/

:- assert_if_new(baseKB:mpred_prop(parser_e2c, e2c_reply, 2, prologOnly)).

% e2c_reply a question
e2c_reply((answer(Answer) :- Condition), Reply) :- fail, nonvar(Condition), !,
 term_variables(Condition, Vars),
 term_singletons(Answer+Vars, FreeVars),
 fmt(query(Answer, FreeVars^satisfy(Condition))),
((baseKB:setof(Answer, FreeVars^call(call, satisfy, Condition), Answers)
 -> Reply = answer(Answers)
 ; (Answer == yes -> Reply = answer([ no]) ; Reply = answer([ none])))), !.
% e2c_reply an assertion @TODO remove NOP
e2c_reply(A, asserted(Assertion)) :- add_quant(exists, A), expand_quants(A, Assertion), nop(baseKB:ain(kif(Assertion))), !.
e2c_reply(A, Reply) :- add_quant(all, A), expand_quants(A, Reply), !.
e2c_reply(_, error('unknown type')).


% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% CLAUSIFY %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================
:- include(e2c/e2c_clausify).

e2c_clausify_and_reply(LF, Reply) :-
   quietly((show_call(e2c_clausify(LF, Clause)),
   e2c_reply(Clause, Reply),
   del_e2c_attributes(Reply))), !.


% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% PHRASE-LEVEL %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================
:- include(e2c/e2c_sentence).
:- include(e2c/e2c_noun_phrase).
:- discontiguous(verb_phrase1/5).
:- discontiguous(talk_verb_lf/6).
:- discontiguous(talk_verb_lf/8).
:- include(e2c/e2c_verb_phrase).


% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% WORD-LEVEL %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================
:- include(e2c/e2c_parts_of_speech).
:- include(e2c/e2c_rephrasing).

% =================================================================
% %%%%%%%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%%
% =================================================================
:- include(e2c/e2c_utility).
:- include(e2c/e2c_commands).

:- retract(t_l:into_form_code).


create_tests_for_cmd(Cmd):-
  functor(Cmd,F,_),
  format("~n~n:- begin_tests(~q).~n~n",[F]),
  forall(test_e2c(English, Options),
     (arg(1, Cmd, English),
      arg(2, Cmd, Options),
      format(atom(TestName), "?- ~q", [Cmd]),
      format("~n~n~p.~n",
        [
         test(TestName, [true(compound(O)), nondet]):-
           call(Cmd, O)
          ]))),
  format("~n~n:- end_tests(~q).~n~n",[F]),!.

generate_all_e2c_tests_now:-
   create_tests_for_cmd(test_lex_info(_, _)),
   create_tests_for_cmd(chat80(_, _)),
   create_tests_for_cmd(curt80(_, _)),
   create_tests_for_cmd(e2c(_, _)), !.

parser_e2c_plt_file(File):- absolute_file_name(library('logicmoo_nlu/parser_e2c.plt'),File,[access(read),file_errors(fail)]).

generate_e2c_plt_file:-
 parser_e2c_plt_file(File),
 (access_file(File,write)-> generate_e2c_plt_file(File) ; dmsg(cant_generate_e2c_plt_file(File))) .

generate_e2c_plt_file(File):-
 setup_call_cleanup(
  open(File, write, Out),
  with_output_to(Out, generate_all_e2c_tests_now), close(Out)).

%:- generate_e2c_plt_file.

%:- break.

%:- forall(test_e2c(A,B),e2c(A,B)).

:- fixup_exports.

%:- include('parser_e2c.plt').

