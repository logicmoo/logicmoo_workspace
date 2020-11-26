% ===================================================================
% File 'parser_all.pl'
% Purpose: English to KIF conversions from SWI-Prolog
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_all.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

:- module(parser_all, []).

:- use_module(library(pfc_lib)).


%:- '$set_typein_module'(baseKB).
%:- '$set_source_module'(baseKB).

% end_of_file.
% :- ensure_loaded(library(logicmoo_nlu/nl_pipeline)).


/*
% From /usr/lib/swi-prolog/library/apply_macros.pl:389
:- must(system:retract(((goal_expansion(GoalIn, PosIn, GoalOut, PosOut) :-
    apply_macros:expand_apply(GoalIn, PosIn, GoalOut, PosOut))))).
% From /usr/lib/swi-prolog/library/apply_macros.pl:386
:- must(system:retract(((goal_expansion(GoalIn, GoalOut) :-
    apply_macros:(\+ current_prolog_flag(xref, true),
    expand_apply(GoalIn, GoalOut)))))).
*/
:- use_module(library(apply_macros)).
:- (abolish(apply_macros:expand_apply, 4), assert((apply_macros:expand_apply(_In, _, _, _):- fail))).
:- (abolish(apply_macros:expand_apply, 2), assert((apply_macros:expand_apply(_In, _):- fail))).


:- use_module(library(logicmoo_utils)).

%:- use_module(library(logicmoo_lib)).

:- use_module(parser_sharing).
:- use_module(parser_tokenize).

%:- use_module(library(logicmoo_nlu)).
%:- ensure_loaded(library(wamcl_runtime)).

% :- dynamic(baseKB:installed_converter/2).
%:- rtrace.
:- shared_parser_data(baseKB:installed_converter/2).
:- export(baseKB:installed_converter/2).

:- shared_parser_data(talkdb:talk_db/2).
:- shared_parser_data(talkdb:talk_db/3).
:- shared_parser_data(talkdb:talk_db/6).

% ==============================================================================

:- volatile(t_l:disable_px/0).
:- thread_local(t_l:disable_px/0).
:- retractall(t_l:disable_px).
:- asserta(t_l:disable_px).

:- shared_parser_data(baseKB:type_action_info/3).


:- shared_parser_data(baseKB:agent_call_command/2).
:- shared_parser_data(baseKB:mud_test/2).
:- multifile(baseKB:sanity_test/0).
:- shared_parser_data(baseKB:sanity_test/0).
:- multifile(baseKB:regression_test/0).
:- shared_parser_data(baseKB:regression_test/0).
:- multifile(baseKB:feature_test/0).
:- shared_parser_data(baseKB:feature_test/0).
:- shared_parser_data(baseKB:sanity_test/1).
:- shared_parser_data(baseKB:regression_test/1).
:- shared_parser_data(baseKB:feature_test/1).



% ==============================================================================
%
% APE: Converter Pipeline
%   acetext, sentences, syntaxTrees, drs, drs0, sdrs, fol, pnf, (tokens),
%        sentencesToParse, paraphrase
%
% CHAT80:  acetext, text_no_punct, pos_sents_pre, parsed80, qplan
%
%  needConverter(syntaxTree, parsed80).
%
% =============================================================================


%% install_converter(+FunctorArgList).
%
%  ?- install_converter(tokens_to_paragraphs(+(tokens), -sentences:set)).
%  ?- install_converter(call_parser(+sentences:list, +(startID, 1), -syntaxtrees, -(drs0, reversed))).
%
:-meta_predicate(install_converter(*)).
:-share_mp(install_converter/1).

install_converter(M:XY):- !, install_converter(M, XY).
install_converter(XY):- strip_module(XY, M, CNV), install_converter(M, CNV).

install_converter(M, XY):- pi_splits(XY, X, Y), !, install_converter(M, X), install_converter(M, Y).
install_converter(M, XY):- pi_p(XY, PI), !, install_converter(M, PI).
install_converter(M, CNV):-
  strip_module(CNV, _OM, CNVLST),
  functor(CNVLST, F, A),
  '@'(export(M:F/A), M),
  '@'(import(M:F/A), parser_all),
  '@'(import(M:F/A), baseKB),
  catch(system:import(M:F/A),_,true),
  nop(dmsg(installed_converter(M, CNVLST))),
  must(ainz(installed_converter(M, CNVLST))).
%install_converter(M, CNV):-strip_module(CNV, M, CNVLST), functor(CNVLST, F, A), '@'(export(M:F/A), M), must(assertz_new(installed_converter(M, CNVLST))).



:-thread_local(pipeline_pass_fail/3).

%% try_converter(+TID:key, +CNV:pred).
%
%  called by recusive code upon Transaction ID
%
try_converter(TID, CNV):-
 strip_module(CNV, M, CNVLST), CNVLST=..[F|Args], !,
  ignore(on_x_debug(((
     maplist(make_io_closure(TID), Args, IOArgs, CLOSURES),
     IOCNVLST=..[F|IOArgs], !,
     deepen_pos('@'(IOCNVLST, M)),
     must_maplist(must, CLOSURES), flag(TID, X, X+1))))).

%% make_io_closure(+TID:key, +NameSpec, ?Value, -Closure).
%
% Make in-out closure on Convertor arg
%
:- export(make_io_closure/4).
make_io_closure(TID, + Name:Type, Value    , true):-!, get_pipeline_value(TID, Name:Type, Value, error), !,
  (Value\=error->true;((fail, trace_or_throw(unknown_get_pipeline_type_value(TID, Name:Type, Value))))).

make_io_closure(TID, (+ Name):Type , Value, O):- !, make_io_closure(TID, + Name:Type , Value, O).
make_io_closure(TID, + (Name:Type) , Value, O):- !, make_io_closure(TID, + Name:Type , Value, O).

make_io_closure(TID, +(Name, Else), Value, true):-!, get_pipeline_value(TID, Name, Value, Else).
make_io_closure(TID, + Name, Value    , true):- get_pipeline_value(TID, Name, Value, error), !,
  (Value\=error->true;((fail, trace_or_throw(unknown_get_pipeline_value(TID, Name, Value))))).

make_io_closure(TID, -Name:Type , Value, set_pipeline_value(TID, Name:Type, Value)):-!.

make_io_closure(TID, (- Name):Type , Value, O):- !, make_io_closure(TID, - Name:Type , Value, O).
make_io_closure(TID, - (Name:Type) , Value, O):- !, make_io_closure(TID, - Name:Type , Value, O).


make_io_closure(TID, -Name , Value, set_pipeline_value(TID, Name, Value)):-!.

make_io_closure(TID, NameType , Value, O):- trace_or_throw(make_io_closure(TID, NameType, Value, O)).

:-thread_local(tl:pipeline_value/3).

%% get_pipeline_value(+TID:key, +Name:varspec, -Value:term, +Else:term ).
%
% Get a variable in the Transaction ID or else a default
%
get_pipeline_value(TID, Name, Value, Else):-var(Name), !, trace_or_throw(var_get_pipeline_value(TID, Name, Value, Else)).
get_pipeline_value(TID, Name, Value, Else):- get_pipeline_val(TID, Name, Value0, Else), unnumbervars(Value0, Value).

get_pipeline_val(TID, Name:list, ValueOut, Else):- findall(V, tl:pipeline_value(TID, Name, V), Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:set, ValueOut, Else):- findall(V, tl:pipeline_value(TID, Name, V), Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:unique, ValueOut, Else):- !, get_pipeline_val(TID, Name, ValueOut, Else).
get_pipeline_val(TID, Name:reversed, ValueOut, Else):- findall(V, tl:pipeline_value(TID, Name, V), RBinders), reverse(RBinders, Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:reversed_set, ValueOut, Else):- findall(V, tl:pipeline_value(TID, Name, V), RBinders), reverse(RBinders, Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:Other, Value, Else):-!, trace_or_throw(unk_get_pipeline_val(TID, Name:Other, Value, Else)).
get_pipeline_val(TID, Name, Value, _ ):- tl:pipeline_value(TID, Name, Value), !.
get_pipeline_val(TID, (N1;Name), ValueOut, Else):- get_pipeline_val(TID, N1, Value, missing),
   (Value==missing ->  get_pipeline_val(TID, Name, ValueOut, Else) ; ValueOut= Value), !.
get_pipeline_val(TID, Name, Value, Else):- tl:pipeline_value(TID, Name, Value) -> true;  Value=Else.
get_pipeline_val(TID, Name, Value, Else):- tl:pipeline_value(TID, '&'(Name , _), Value) -> true;  Value=Else.

is_word_atomic(Value):-atomic(Value), !.
is_word_atomic(Value):-functor(Value, w, 2).

is_single_value(Value):- \+ is_list(Value), !.
is_single_value(Value):- is_worldlist_list(Value), !.

is_worldlist_list([Value|_]):-!, is_word_atomic(Value), !.

%% set_pipeline_value(+TID:key, +Name:varspec, +Value:term ).
%
% Set a variable in the Transaction ID
%

set_pipeline_value(TID, Name, Value):- (var(Value) ; \+ ground(Name)), !, trace_or_throw(var_set_pipeline_value(TID, Name, Value)).
set_pipeline_value(TID, '&'(N1, Name), Value):-!, set_pipeline_value(TID, N1, Value), set_pipeline_value(TID, Name, Value).
set_pipeline_value(TID, Name:unique, V0):- !, set_unique_pipeline_value(TID, Name, V0).
set_pipeline_value(TID, Name:set, Value):- is_single_value(Value), !, must(set_unique_pipeline_value(TID, Name, Value)).
set_pipeline_value(TID, Name:set, Values):- must(( foreach(member_rev(V, Values), set_unique_pipeline_value(TID, Name, V)))).
set_pipeline_value(TID, Name:list, Value):- is_single_value(Value), !, must(set_pipeline_value(TID, Name, Value)).
set_pipeline_value(TID, Name:list, Values):- must(( foreach(member_rev(V, Values), set_pipeline_value(TID, Name, V)))).
set_pipeline_value(TID, Name:reversed_set, RBinders):- reverse(RBinders, Values), set_pipeline_value(TID, Name:set, Values).
set_pipeline_value(TID, Name:reversed, RBinders):- reverse(RBinders, Values), set_pipeline_value(TID, Name:list, Values).
set_pipeline_value(TID, Name:Other, Value):-!, trace_or_throw(unknown_set_pipeline_value(TID, Name:Other, Value)).
% set_pipeline_value(TID, Name, Values):- \+ is_single_value(Values), !, must(( foreach(member_rev(V, Values), set_unique_pipeline_value(TID, Name, V)))).
set_pipeline_value(TID, Name, V0):- set_unique_pipeline_value(TID, Name, V0).

member_rev(V, Values):- reverse(Values, Rev), member(V, Rev).

/*
  ((copy_term(V, CV), clause(tl:pipeline_value(TID, Name, V), true, Ref),
                    clause(tl:pipeline_value(TID, NameC, VC), true, Ref),
                      (NameC:VC)=@=(Name:CV))
*/

set_unique_pipeline_value(TID, Name, V0):- clause_asserted(tl:pipeline_value(TID, Name, V0)), !.
set_unique_pipeline_value(TID, Name, V0):- set_new_pipeline_value(TID, Name, V0).

set_new_pipeline_value(TID, Name, V0):-
   renumber_vars_from_0(Name, V0, V), %rename_vars
   maybe_new_value_op(TID, Name, V),
   show_call(asserta(tl:pipeline_value(TID, Name, V0))).

maybe_new_value_op(TID, Name, V):-
  \+ \+ ((copy_term(V, CV),
            clause(tl:pipeline_value(TID, Name, V), true, Ref),
            clause(tl:pipeline_value(TID, NameC, VC), true, Ref),
            (NameC:VC)=@=(Name:CV)) -> true; flag(TID, OPs, 1+OPs)).

renumber_vars_from_0(_, V, UV):- copy_term(V, UM, _), duplicate_term(UM, UV), !.
renumber_vars_from_0(aceKif(_), V, UV):-V=UV, !.
renumber_vars_from_0(_, V, UV):- unnumbervars(V, UV). % get_ape_results:rename_vars(UV, UV). %, ape_numbervars(UV, 0, _).


renumber_vars_from_1(_, V, UV):- unnumbervars(V, UV). % get_ape_results:rename_vars(UV, UV). %, ape_numbervars(UV, 0, _).

system:ape_numbervars(DRSCopy, Zero, N):- numbervars(DRSCopy, Zero, N, [attvar(skip)]).

%% clear_pipeline(+TID:key)
%
%  Clean out the Transaction ID
%
clear_pipeline(TID):-retractall(tl:pipeline_value(TID, _, _)), retractall(pipeline_pass_fail(TID, _, _)).


%% init_pipeline(+TID:key)
%
%  Intialize the Transaction ID with defaults
%
%  when we switch to dictionaries.. we'd prebuild the keys
%
init_pipeline(_ID).


:- export(set_pipeline_nvlist/2).
set_pipeline_nvlist(TID, StartingNameValues):-
   forall(member(Name=Value, StartingNameValues),
     show_call(set_pipeline_value(TID, Name, Value))).

:- export(get_pipeline_nvlist/2).
get_pipeline_nvlist(TID, AllNameValues):-
        findall(Name=Values,
          ((no_repeats(Name, tl:pipeline_value(TID, Name, _)),
                      findall(Value, tl:pipeline_value(TID, Name, Value), Values))), AllNameValues).


%% run_pipeline( +StartingNameValues:list, +WaitingOnNVs:list, -AllNameValues:list )
%
%  Run a pipeline to yeild NameValues list
%
run_pipeline(Text):- run_pipeline(Text, [aceKif(p)=_, lf=_, clause=_, qplan=_, results80=_], O), show_kvs(O).

pipeline_input(X=Text, [X=Text]):-!.
pipeline_input([X=Text|More], [X=Text|More]):-!.
pipeline_input(Text, [input=Text]):-!.

run_pipeline(StartingNameValues0, WaitingOnNVs0, RAllNameValuesOut):-
    setup_call_cleanup(
      notrace(
        (pipeline_input(StartingNameValues0, StartingNameValues),
         flatten([WaitingOnNVs0], WaitingOnNVs),
         gensym(iPipeline, TID), clear_pipeline(TID), init_pipeline(TID),
         set_pipeline_nvlist(TID, StartingNameValues),
         % show_pipeline(TID),
         dmsg(start(run_pipeline_id(TID, WaitingOnNVs))))),

      run_pipeline_id(TID, WaitingOnNVs, ExitWhy),

      notrace((dmsg(end(run_pipeline_id(TID, ExitWhy))),
         get_pipeline_nvlist(TID, AllNameValues),
         %show_pipeline(TID),
         reverse(AllNameValues, RAllNameValues),
         %show_kvs(RAllNameValues),
         mapnvs(WaitingOnNVs0, RAllNameValues, RAllNameValuesOut),
         clear_pipeline(TID)))), !.

mapnvs(WaitingOnNVs0, RAllNameValues, RAllNameValuesOut):-
   forall(member(NV, WaitingOnNVs0),
     ((NV=(N=V)), member(N=V, RAllNameValues),
      nb_setarg(2, NV, V))), !, RAllNameValuesOut=WaitingOnNVs0.
mapnvs(_, O, O).

show_kvs(O):- notrace(show_kvs0(O)), !.
show_kvs0(V):- var(V), !, show_kvs0(var:-V).
show_kvs0(K:-V):- !, portray_clause(current_output, K:-V).
show_kvs0([H|List]):- !, show_kvs0(H), show_kvs0(List).
show_kvs0(List):- is_list(List), !, maplist(show_kvs0, List).
show_kvs0(K=V):- !, show_kvs0(K:-V).
show_kvs0(K-V):- !, show_kvs0(K:-V).
show_kvs0(KV):- show_kvs0((kv:-KV)), !.


%% text_pipeline( +Text:acetext, +NameValues:list )
%
%  Runs Transaction ID with acetext
%
text_pipeline(AceText, AllNameValues):-
  run_pipeline([input=AceText], [untildone=_], AllNameValues).

%% run_pipeline_id( +TID:key, +WaitingOnNVs:list )
%
%  Runs Transaction ID until WaitingOnNVs is grounded
%
run_pipeline_id(TID, WaitingOnNVs, ExitWhy):-
  flag(TID, _, 1),
  run_pipeline_id(TID, WaitingOnNVs, ExitWhy, 0).

run_pipeline_id(_TID, [] , complete , _N):- !.
run_pipeline_id( TID, _WaitingOnNVs, error(Name, Err), _N):- tl:pipeline_value(TID, Name, error(Err)), !.
run_pipeline_id( TID, _WaitingOnNVs, no_new_ops, _N):- flag(TID, 0, 0), !.
run_pipeline_id(_TID, _WaitingOnNVs, overflow(N), N):- N> 20, !.
run_pipeline_id( TID, _WaitingOnNVs, Err, _N):- tl:pipeline_value(TID, error, Err), !.
run_pipeline_id( TID, WaitingOnNVs, ExitWhy, N):-
   partition(is_bound_value(TID), WaitingOnNVs, _Bound, Unbound),
   Unbound \== WaitingOnNVs, !,
   run_pipeline_id(TID, Unbound, ExitWhy, N).
run_pipeline_id( TID, WaitingOnNVs, ExitWhy, N):-
    flag(TID, _, 0),
    forall(choose_converter(TID, WaitingOnNVs, CNV), try_converter(TID, CNV)),
    N2 is N +1,
    run_pipeline_id(TID, WaitingOnNVs, ExitWhy, N2).

is_bound_value(TID, Name=Value):- var(Value), !, tl:pipeline_value(TID, Name, Value).
is_bound_value(_TID, _Name=Value):- !, assertion(nonvar(Value)).
is_bound_value(TID, Name):- tl:pipeline_value(TID, Name, _Value).


choose_converter(TID, WaitingOnNVs, M:CNV):- fail, installed_converter(M, CNV),
   \+ \+ ((sub_term(Sub, CNV), compound(Sub), member(N=_, WaitingOnNVs), (-N) ==Sub)),
   \+ \+ ((sub_term(Sub, CNV), tl:pipeline_value(TID, N, _), N==Sub)), !.

choose_converter(TID, WaitingOnNVs, M:CNV):- !, installed_converter(M, CNV),
   \+ \+ ((sub_term(Sub, CNV), atom(Sub), (tl:pipeline_value(TID, N, _);member(N=_, WaitingOnNVs)), N==Sub)).

choose_converter(_TID, _WaitingOnNVs, M:CNV):- installed_converter(M, CNV).

% show stat
show_pipeline(TID):-
  wdmsg(show_pipeline(TID)),
  forall(tl:pipeline_value(TID, Name, Value), wdmsg(tl:pipeline_value(TID, Name, Value))),
  forall(pipeline_pass_fail(TID, Name, Value), wdmsg(pipeline_pass_fail(TID, Name, Value))).

show_pipeline:-forall(installed_converter(M, CNV), wdmsg(installed_converter(M, CNV))).


maybe_display(G):- dmsg(call(writeq(G))).

:- ignore(( Z = ('/'), user:current_op(X, Y, Z), maybe_display(:-(op(X, Y, Z))), nl, fail)).
:- ignore((Z = (':'), user:current_op(X, Y, Z), maybe_display(:-(op(X, Y, Z))), nl, fail)).
:- ignore((Z = ('-'), user:current_op(X, Y, Z), maybe_display(:-(op(X, Y, Z))), nl, fail)).
:- dmsg(parser_all_start).


:- shared_parser_data(clex_iface:clex_noun/5).

:- export(load_parser_interface/1).
% load_parser_interface(File):- \+ exists_source(File), !, call(File:ensure_loaded_no_mpreds(logicmoo_nlu_ext(File))).
load_parser_interface(File):- call(File:ensure_loaded_no_mpreds(File)).
%:- parser_chat80:import(load_parser_interface/1).

% ================================================================================================
%:- include(parser_ape).
:- if(load_parser_interface(parser_ape)).
%:- pfc_lib:load_parser_interface('AceRules/engine/run_testcases').
% ================================================================================================
:- use_module(ape(parser/ace_to_drs)).
:- use_module(ape(get_ape_results)).
:- user:import(get_ape_results:ace_to_pkif/2).
:- system:import(get_ape_results:ace_to_pkif/2).


system:my_aceparagraph_to_drs(AceText, Sentences, SyntaxTrees, UnresolvedDrsCopy, Drs, Messages):-
   ace_to_drs:aceparagraph_to_drs(AceText, on, off, 1, Sentences, SyntaxTrees, UnresolvedDrsCopy, Drs, Messages, _).

%:- install_converter(parser_tokenize:into_acetext(+input, -acetext)).
:- install_converter(parser_tokenize:into_acetext(+text80, -acetext)).
:- install_converter(parser_tokenize:into_text80(+input, -text80)).
:- install_converter(parser_tokenize:into_text80(+acetext, -text80)).
%:- install_converter(parser_tokenize:tokens_to_acetext(+(tokens), -acetext)).
%:- install_converter(tokenizer:tokenize(+input, -(tokens))).
%:- install_converter(get_ape_results:ace_to_pkif(+acetext, -aceKif(p))).
%:- install_converter(ace_to_drs:call_tokenizer(+acetext, +(guess, on), -sentences:set, -sentencesToParse)).
%:- install_converter(ace_to_drs:paragraphs_to_drs(+sentences:list, +(guess, on), +(catch, off), +(startID, 1), -sentences, -syntaxTrees, -drs0, -messages, -time)).
%:- install_converter(ace_to_drs:call_parser(+sentences:list, +(startID, 1), -syntaxtrees, -drs0:reversed_set)).
:- install_converter(system:my_aceparagraph_to_drs(+acetext, -sentences_set, -syntaxTrees, -unresolvedDrs, -drs0, -messages)).
%:- install_converter(ace_to_drs:acetext_to_drs(+acetext, -sentences_set, -syntaxTrees, -drs0, -messages)).
%:- install_converter(tokens_to_sentences:tokens_to_sentences(+(tokens), -sentences:set)).
%:- install_converter(tokens_to_sentences:tokens_to_paragraphs(+(tokens), -sentences:set)).
:- install_converter(drs_fol_pnf:drs_pnf(+drs0, -fol)).
:- install_converter(drs_fol_pnf:drs_fol(+drs0, -pnf)).

:- install_converter(get_ape_results:fol_to_pkif(+pnf, -aceKif(p_kif))).
:- install_converter(get_ape_results:fol_to_pkif(+fol, -aceKif(f_kif))).
:- install_converter(get_ape_results:fol_to_pkif(+drs0, -aceKif(d_kif))).
:- install_converter(get_ape_results:fol_to_pkif(+sdrs, -aceKif(s_kif))).

:- install_converter(drs_to_ace:drs_to_ace(+drs0, -paraphrase_set)).
:- install_converter(drs_to_ace:drslist_to_ace(+drs_set, -paraphrase_set)).
:- install_converter(drs_to_drslist:drs_to_drslist(+drs0, -drs_set)).
:- install_converter(drs_to_sdrs:drs_to_sdrs(+drs, -sdrs)).
:- endif.


% ================================================================================================
% CHAT80:  acetext, text_no_punct, pos_sents_pre, parsed80, simplify80, qplan
:-  if(load_parser_interface(parser_chat80)).
% ================================================================================================

:- export(pa_domain/2).
pa_domain(Var, List):-freeze(Var, member(Var, List)).

:- export(was_punct/1).
was_punct(Remove):-
  pa_domain(WRemove, [(, ), (.), (?), (!)]),
   (pa_domain(Remove, [w(_, punc), w(WRemove, _)]);Remove=WRemove).

remove_punctuation(W2, NP):- is_list(W2), was_punct(Remove), delete(W2, Remove, W3), W2 \=@= W3, !, remove_punctuation(W3, NP).
remove_punctuation(W2, NP):- is_list(W2), !, maplist(remove_punctuation, W2, NP).
remove_punctuation(W2, NP):- atom(W2), member(P, [(, ), (.), (?), (!)]), (atom_concat(NP, P, W2);atom_concat(P, NP, W2)), !.
remove_punctuation(W2, NP):- string(W2), member(P, [(, ), (.), (?), (!)]), (string_concat(NP, P, W2);string_concat(P, NP, W2)), !.
remove_punctuation(W2, W2).

%:- install_converter(parser_all:remove_punctuation(+acetext, -acetext_no_punct)).

%:- install_converter(parser_chat80:words_to_w2(+acetext_no_punct, -pos_sents_pre)).
% :- install_converter(parser_chat80:into_text80(+(tokens), -text80)).
:- install_converter(parser_chat80:sent_to_parsed(+text80, -parsed80)).
:- install_converter(parser_chat80:i_sentence(+parsed80, -sent80)).
:- install_converter(parser_chat80:clausify80(+sent80, -clausify80)).
:- install_converter(parser_chat80:simplify80(+clausify80, -simplify80)).
:- install_converter(parser_chat80:qplan(+simplify80, -qplan80)).
:- install_converter(parser_chat80:results80(+qplan80, -results80)).

:-kb_global(partOfSpeech/3).
:-kb_global(determinerStrings/2).


:-asserta((type(SET):- call_u(tSet(SET)))).

:- endif.


% ================================================================================================
% English2CycL:
:-  if((fail, load_parser_interface(parser_e2c))). % TODO confirm CHAT80 runs without E2C
% ================================================================================================

%:- debug.

:- endif.


% ================================================================================================
:-  if(load_parser_interface(parser_candc)).
% ================================================================================================

%:- debug.
%:- break.

:- endif.


% ================================================================================================
%:-  load_parser_interface(parser_chart89).
% ================================================================================================

% ================================================================================================
:-  if((false, load_parser_interface(parser_e2c))).
% ================================================================================================

eng_to_bratko(Sentence, LF, Type, Clause, FreeVars) :-
   show_call(bratko_parse(Sentence, LF, Type)),
   show_call(bratko_clausify(LF, Clause, FreeVars)), !.


:- install_converter(parser_all, eng_to_bratko(+(tokens), -lf, -type, -clause, +(freevars))).
:- install_converter(parser_bratko, bratko_parse(+(tokens), -lf, -type)).
:- install_converter(parser_bratko, bratko_clausify(+lf, -clause, -(freevars))).
:- install_converter(parser_bratko, bratko_reply(+type, +(freevars), +clause, -reply)).

%:- debug.

:- endif.

% ================================================================================================
:-  if(load_parser_interface(parser_e2c)).
% ================================================================================================

:- install_converter(parser_e2c, e2c_parse(+chat80, -lf_b)).
:- install_converter(parser_e2c, e2c_clausify(+lf_b, -clause_b)).
:- install_converter(parser_e2c, e2c_reply(+clause_b, -reply_b)).

%:- debug.

:- endif.


% ================================================================================================
load_parser_stanford:-  load_parser_interface(parser_stanford).
% ================================================================================================
% :- get_pos_tagger(I), jpl_set(I, is_DEBUG, '@'(false)).

:- reexport(library('logicmoo/common_logic/common_logic_snark.pl')).


%% with_el_holds_enabled_4_nl( :Goal) is semidet.
%
% Using El Holds Enabled.
%
:- meta_predicate(with_el_holds_enabled_4_nl(0)).
with_el_holds_enabled_4_nl(Goal):-locally_hide(el_holds_DISABLED_KB, Goal).


:- dynamic is_cyckb_t_pred/2.
:- dynamic is_cyckb_t_pred_rename/2.

:- dmsg("Scanning el_assertions.pl for programatic definations (This may take 10-30 seconds)").
%:- ain(cyckb_t(A, _, _) ==> is_cyckb_t_pred(A, 2)).
:- with_el_holds_enabled_4_nl(gripe_time(10, forall(cyckb_t(A, _, _) , assert_if_new(is_cyckb_t_pred(A, 2))))).
%:- ain(cyckb_t(A, _, _, _ ) ==> is_cyckb_t_pred(A, 3)).
:- with_el_holds_enabled_4_nl(gripe_time(2, forall(cyckb_t(A, _, _, _) , assert_if_new(is_cyckb_t_pred(A, 3))))).
%:- ain(cyckb_t(A, _, _, _, _ ) ==> is_cyckb_t_pred(A, 4)).
:- with_el_holds_enabled_4_nl(gripe_time(2, forall(cyckb_t(A, _, _, _ , _ ) , assert_if_new(is_cyckb_t_pred(A, 4))))).
%:- ain(cyckb_t(A, _, _, _, _, _ ) ==> is_cyckb_t_pred(A, 5)).
:- with_el_holds_enabled_4_nl(gripe_time(2, forall(cyckb_t(A, _, _, _ , _, _ ) , assert_if_new(is_cyckb_t_pred(A, 5))))).

:- dmsg("Implementing programatic definations (This shoiuld take less than 2 seconds)").
% :- ain((is_cyckb_t_pred(F, A) ==> {functor(H, F, A), H=..[F|ARGS], KB=..[cyckb_t, F|ARGS], assert_if_new((H:-KB))})).
:- gripe_time(2, forall(is_cyckb_t_pred(F, A) , ignore((atom(F), functor(H, F, A), H=..[F|ARGS],
    KB=..[cyckb_t, F|ARGS],
       on_x_log_cont(assert_if_new((H:- \+ (t_l:el_holds_DISABLED_KB), KB))))))).

% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_lexical).
% ================================================================================================



% ================================================================================================
% TODO Not yet started
:-  nop(load_parser_interface(parser_CURT)).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_regulus).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_SUPPLE).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_jpaine).
% ================================================================================================

% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_SIRIDUS).
% ================================================================================================


% ================================================================================================
% TODO - grovel the API
:-  load_parser_interface(parser_ProNTo).
% ================================================================================================

:- dmsg("List of possible data transformations").


:- dmsg(call(show_pipeline)).

:- ensure_loaded(parser_pldata).

% ================================================================================================
%:-  load_parser_interface(parser_fwd).
% ================================================================================================

:- dmsg(parser_all_complete).


baseKB:sanity_test:- run_pipeline(input='A person who loves all animals is loved by someone.', [aceKif(p)=_], O), show_kvs(O).
baseKB:sanity_test:- run_pipeline(input='All persons are happy.', [aceKif(p)=_], O), wdmsg(O).
baseKB:regression_test:- run_pipeline('What are the oceans that border african countries and that border asian countries ?').
baseKB:regression_test:- run_pipeline('What is the ocean that border african countries and that border asian countries?', [qplan=_], O), wdmsg(O).
baseKB:regression_test:- run_pipeline(input='what countries are there in europe ?', [qplan=_], O), show_kvs(O).
baseKB:regression_test:- must_test_80(Tokens, _, _), run_pipeline([(tokens)=Tokens], [qplan=_], O), show_kvs(O).
baseKB:regression_test_TODO:- run_pipeline(input='A person who loves all animals is loved by someone.', [aceKif(p)=_], O), show_kvs(O).
animals_test:- must_det_l((ace_to_pkif('A person who loves all animals is loved by someone.', X), kif_to_boxlog(X, BOX), portray_clause(user_error, (fol:-BOX)))).
baseKB:regression_test:- animals_test.

:- import(get_ape_results:ace_to_pkif/2).
:- baseKB:import(get_ape_results:rename_vars/2).

% som3how this next directive changes  -/1 op?
% :- animals_test.
:- op(300, fx, (-)).


baseKB:regression_test:- gripe_time(5, test_chat80_sanity).


% :- must(retract(t_l:disable_px)).

:- ensure_loaded(library(apply_macros)).


% set  -/1 op
:- op(200, fy, (-)).
:- must((current_op(P, FXY, (-)), ((arg(_, v(fy, fx), FXY), P =< 300)))).

:- ignore((Z = ('`'), user:current_op(X, Y, Z), dmsg(call((writeq(:-(op(X, Y, Z))), nl, fail))))).
% :- halt(666).

baseKB:feature_test:- run_pipeline("what countries are there in europe ?").
baseKB:feature_test:- run_pipeline("What countries are there in europe ?").
baseKB:feature_test:- run_pipeline("What countries are there in north_america ?").
baseKB:feature_test:- run_pipeline("What countries are there in north america ?").
baseKB:feature_test(must_test_80):-
  forall(must_test_80(U, R, O),
    (ignore(\+ \+ process_run_diff(report, U, R, O)),
     ignore(\+ \+ (run_pipeline([input=U], [results80=_], OL), show_kvs(OL))))).

:- fixup_exports.

:- if((current_prolog_flag(runtime_debug, D), D>2)).
:- dmsg(call((
   listing(feature_test),
   listing(sanity_test),
   listing(regression_test),
   listing(chat80/3),
   listing(chat80/1),
   listing(chat80/2),
   listing(test_e2c/1),
   listing(test_e2c/2),
   threads))).
:- endif.

%:- must_test_80.
%:- test_chat80_regressions.



