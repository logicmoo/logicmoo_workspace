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

:- module(parser_pipeline, []).


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

:- use_module(library(logicmoo_utils)).

%:- use_module(library(logicmoo_lib)).


%:- use_module(library(logicmoo_nlu)).
%:- ensure_loaded(library(wamcl_runtime)).

%:- dynamic(baseKB:installed_converter/4).
%:- rtrace.
:- shared_parser_data(baseKB:installed_converter/4).
:- export(baseKB:installed_converter/4).


:- export(load_parser_interface/1).
% load_parser_interface(File):- \+ exists_source(File), !, call(File:ensure_loaded_no_mpreds(logicmoo_nlu_ext(File))).
load_parser_interface(File):- call(File:ensure_loaded_no_mpreds(File)).
%:- parser_chat80:import(load_parser_interface/1).


:- debug(pipeline).

while_tracing_pipeline(_):- catch( \+ thread_self(1),_,fail),!.
while_tracing_pipeline(_):- catch(pengines:pengine_self(_),_,fail),!.
while_tracing_pipeline(X):- debugging(pipeline), !, call(X).
while_tracing_pipeline(_).

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

ainz_installed_converter(M, CNVLST, Ins,Out):- ainz(installed_converter(M, CNVLST, Ins,Out)).

:-share_mp(install_converter/2).
install_converter(M, XY):- pi_splits(XY, X, Y), !, install_converter(M, X), install_converter(M, Y).
install_converter(M, XY):- pi_p(XY, PI), !, install_converter(M, PI).
install_converter(M, CNV):-
  strip_module(CNV, _OM, CNVLST),
  functor(CNVLST, F, A),  
  '@'(export(M:F/A), M),
  '@'(import(M:F/A), parser_all),
  '@'(import(M:F/A), baseKB),
  catch(system:import(M:F/A),_,true),
  %while_tracing_pipeline(dmsg(installed_converter(M, CNVLST))),
  get_in_outs(CNVLST,Ins,Outs),
  must_maplist(ainz_installed_converter(M, CNVLST, Ins),Outs).
%install_converter(M, CNV):-strip_module(CNV, M, CNVLST), functor(CNVLST, F, A), '@'(export(M:F/A), M), must(assertz_new(installed_converter(M, CNVLST,Ins,Outs))).

get_in_outs(CNVLST,Ins,Outs):-
  findall(T,(sub_term(C,CNVLST),compound(C),(C= +(T))),Ins),
  findall(T,(sub_term(C,CNVLST),compound(C),(C= -(T))),Outs),!.

:-thread_local(tl:pipeline_pass_fail/3).

%% try_converter(+TID:key, +CNV:pred).
%
%  called by recusive code upon Transaction ID
%
try_converter(TID, CNV):-
 strip_module(CNV, M, CNVLST), CNVLST=..[F|Args], !,
  (((((
     maplist(make_io_closure(TID), Args, IOArgs, CLOSURES),
     IOCNVLST=..[F|IOArgs], !,
     (Goal = M:IOCNVLST),
     warn_failure(deepen_pos(catch(Goal,E,(dmsg(Goal-->E),on_x_fail(rtrace(Goal)),fail)))),
     maplist(must_or_rtrace, CLOSURES), nop(flag(TID, X, X+1))))))).

warn_failure(X):- call(X)*-> true; (nop(dmsg(failed(X))),!,fail).
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
make_io_closure(_TID, NameType , Value, O):- atom(NameType),Value=NameType,O=true,!.
make_io_closure(TID, NameType , Value, O):- trace_or_throw(make_io_closure(TID, NameType, Value, O)).

:-thread_local(tl:pipeline_value/5).

%% get_pipeline_value(+TID:key, +Name:varspec, -Value:term, +Else:term ).
%
% Get a variable in the Transaction ID or else a default
%
get_pipeline_value(TID, Name, Value, Else):-var(Name), !, trace_or_throw(var_get_pipeline_value(TID, Name, Value, Else)).
get_pipeline_value(TID, Name, Value, Else):- get_pipeline_val(TID, Name, Value, Else).

get_pipeline_val(TID, Name:list, ValueOut, Else):- findall(V, local_pipeline_value(1,TID, Name, V), Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:set, ValueOut, Else):- findall(V, local_pipeline_value(1,TID, Name, V), Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:unique, ValueOut, Else):- !, get_pipeline_val(TID, Name, ValueOut, Else).
get_pipeline_val(TID, Name:reversed, ValueOut, Else):- findall(V, local_pipeline_value(1,TID, Name, V), RBinders), reverse(RBinders, Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:reversed_set, ValueOut, Else):- findall(V, local_pipeline_value(1,TID, Name, V), RBinders), reverse(RBinders, Values), !, (Values==[]-> ValueOut=Else, ValueOut = Values).
get_pipeline_val(TID, Name:Other, Value, Else):-!, trace_or_throw(unk_get_pipeline_val(TID, Name:Other, Value, Else)).
get_pipeline_val(TID, Name, Value, _ ):- local_pipeline_value(1,TID, Name, Value), !.
get_pipeline_val(TID, (N1;Name), ValueOut, Else):- get_pipeline_val(TID, N1, Value, missing),
   (Value==missing ->  get_pipeline_val(TID, Name, ValueOut, Else) ; ValueOut= Value), !.
get_pipeline_val(TID, Name, Value, Else):- local_pipeline_value(1,TID, Name, Value) -> true;  Value=Else.
get_pipeline_val(TID, Name, Value, Else):- local_pipeline_value(1,TID, '&'(Name , _), Value) -> true;  Value=Else.

is_word_atomic(Value):-atomic(Value), !.
is_word_atomic(Value):-functor(Value, w, 2).

is_single_value(Value):- \+ is_list(Value), !.
is_single_value(Value):- is_worldlist_list(Value), !.

is_worldlist_list([Value|_]):-!, is_word_atomic(Value), !.

%% set_pipeline_value(+TID:key, +Name:varspec, +Value:term ).
%
% Set a variable in the Transaction ID
%

set_pipeline_value(ID,Name,Value):-
 %notrace
 ( nortrace, set_1pipeline_value(ID,Name,Value)).

set_1pipeline_value(_TID, _Name, Value):- var(Value) , !,
  %set_prolog_flag(no_pretty,true),
  %nop((set_prolog_flag(no_pretty,true), rtrace)), 
  must(Value=failed),!.
%set_1pipeline_value(_TID, _Name, Value):- nortrace,fail.
%set_1pipeline_value(TID, Name, Value):- \+ ground(Name), !, trace_or_throw(var_set_pipeline_value(TID, Name, Value)).
set_1pipeline_value(TID, Name:unique, V0):- !, set_unique_pipeline_value(TID, Name, V0).
set_1pipeline_value(TID, Name:set, Value):- is_single_value(Value), !, must(set_unique_pipeline_value(TID, Name, Value)).
set_1pipeline_value(TID, Name:set, Values):- must(( foreach(member_rev(V, Values), set_unique_pipeline_value(TID, Name, V)))).
set_1pipeline_value(TID, Name:list, Value):- is_single_value(Value), !, must(set_1pipeline_value(TID, Name, Value)).
set_1pipeline_value(TID, Name:list, Values):- must(( foreach(member_rev(V, Values), set_1pipeline_value(TID, Name, V)))).
set_1pipeline_value(TID, Name:reversed_set, RBinders):- reverse(RBinders, Values), set_1pipeline_value(TID, Name:set, Values).
set_1pipeline_value(TID, Name:reversed, RBinders):- reverse(RBinders, Values), set_1pipeline_value(TID, Name:list, Values).
set_1pipeline_value(TID, Name:Other, Value):-!, trace_or_throw(unknown_set_pipeline_value(TID, Name:Other, Value)).
% set_pipeline_value(TID, Name, Values):- \+ is_single_value(Values), !, must(( foreach(member_rev(V, Values), set_unique_pipeline_value(TID, Name, V)))).

set_1pipeline_value(TID, '&'(N1, Name), Value):-!, set_1pipeline_value(TID, N1, Value), set_1pipeline_value(TID, Name, Value).
set_1pipeline_value(TID, Name, V):- set_unique_pipeline_value(TID, Name, V).

member_rev(V, Values):- reverse(Values, Rev), member(V, Rev).


renumber_vars_from_0(_, V, UV):- copy_term(V, UM, _), duplicate_term(UM, UV), !.
renumber_vars_from_0(aceKif(_), V, UV):-V=UV, !.
renumber_vars_from_0(_, V, UV):- unnumbervars(V, UV). % get_ape_results:rename_vars(UV, UV). %, ape_numbervars(UV, 0, _).

renumber_vars_from_1(_, V, UV):- unnumbervars(V, UV). % get_ape_results:rename_vars(UV, UV). %, ape_numbervars(UV, 0, _).

:- rb_new(Y),nb_setval('$pipe',Y).

pipe_key(X,Key):- atom(X),!, atom_concat('$pipe_',X,Key).
pipe_key(_,_Key):- dumpST,break.

new_pipe(X):- pipe_key(X,Key),!,rb_new(Y),nb_setval(Key,Y).
rem_pipe(X):- pipe_key(X,Key),!,nb_delete(Key).

is_pipe(X,Y):- was_pipe(X),!,X=Y.
is_pipe(X,Y):- pipe_key(X,Key),!,nb_current(Key,Y).

was_pipe(X):- atom(X),!,fail.
was_pipe(X):- is_rbtree(X),!.
was_pipe(X):- is_ootree(X),!.

set_unique_pipeline_value(TID, Name, V):- duplicate_term(V,VV),
  set_unique_pipeline_value0(TID, Name, VV).
 
set_unique_pipeline_value0(TID, Name, V):- 
    must_or_rtrace((pretty_numbervars(V, VarNames), renumber_vars_from_0(Name,V,V0))), 
    ignore((set_1unique_pipeline_value_or_fail(TID, Name, V, V0, VarNames)->flag(TID, OPs, 1+OPs))).

set_1unique_pipeline_value_or_fail(TID0, Name, V, V0, VarNames):-
  is_pipe(TID0,TID),!,
  (nb_rb_get_node(TID, Name, Node),(nb_rb_node_value(Node,Values))-> 
     ((member(v(_,V0,_),Values),!,fail);
      (nb_set_add1(Values,v(V, V0, VarNames))));
    nb_rb_insert(TID, Name, [v(V, V0, VarNames)])).

set_1unique_pipeline_value_or_fail(TID0, Name, V, V0, VarNames):-
  is_pipe(TID0,TID),!,
  (nb_get_value(TID, Name, Values)-> 
     ((member(v(_,V0,_),Values),!,fail);
      (nb_set_add1(Values,v(V, V0, VarNames))));
    nb_set_value(TID, Name, [v(V, V0, VarNames)])).

set_1unique_pipeline_value_or_fail(TID, Name, V, V0, VarNames):-
    \+ clause(tl:pipeline_value(TID, Name, _, V0, _), true), 
    asserta(tl:pipeline_value(TID, Name, V, V0, VarNames)).



system:ape_numbervars(DRSCopy, Zero, N):- numbervars(DRSCopy, Zero, N, [attvar(skip)]).

%% clear_pipeline(+TID:key)
%
%  Clean out the Transaction ID
%
clear_pipeline(TID0):- is_pipe(TID0,TID),!,nop(reset_oo(TID)),rem_pipe(TID0).
clear_pipeline(TID):- retractall(tl:pipeline_value(TID, _, _, _, _)), retractall(tl:pipeline_pass_fail(TID, _, _)).


%% init_pipeline(+TID:key)
%
%  Intialize the Transaction ID with defaults
%
%  when we switch to dictionaries.. we'd prebuild the keys
%
init_pipeline(TID):- new_pipe(TID), !.
init_pipeline(_ID).


:- export(set_pipeline_nvlist/2).
set_pipeline_nvlist(TID, Name=Value):- !, (nonvar(Value)-> set_pipeline_value(TID, Name, Value) ; true).
set_pipeline_nvlist(TID, Have):-
  maplist(set_pipeline_nvlist(TID),Have).

:- export(get_pipeline_nvlist/3).
get_pipeline_nvlist(N,TID, AllNameValues):-
        findall(Name, ((no_repeats(Name, local_pipeline_value(1,TID, Name, _)))), Names),
        findall(Name=Value, 
           (member(Name,Names),local_pipeline_value(N, TID, Name, Value)), AllNameValues).

local_pipeline_value(N,TID0, Name, Value):- is_pipe(TID0,TID),!, quietly((rb_in(Name, Values,TID),member(V3,Values),arg(N,V3,Value))).
local_pipeline_value(N,TID, Name, Value):- 
   tl:pipeline_value(TID, Name, V, V0, VarNames),arg(N,v(V, V0, VarNames),Value).


get_pipeline_value_or(Name,Pairs,Value,Else):- (member(N=V,Pairs),Name=N)->Value=V;Value=Else.

:- multifile(default_pipeline_opts/1).
:- dynamic(default_pipeline_opts/1).

%default_pipeline_opts([aceKif(p_kif)=_, lf=_, text80=_, acetext=_, clause=_, reply_e2c=_, combined_info=_, results80=_]). 
% aceKif(p_kif)=_,
%default_pipeline_opts([aceKif(p_kif)=_, lf=_, clause=_, combined_info=_,  simplify80=_, results80=_]).
%default_pipeline_opts([text80=_]).

%% run_pipeline( +Have:list, +NEEDs:list, -AllNameValues:list )
%
%  Run a pipeline to yeild NameValues list
%

%run_pipeline(Text):- text_to_best_tree(Text,format),!.
run_pipeline(Text):- 
  fmt('~N?- run_pipeline(~q).~N',[Text]),
  must_be(nonvar,Text),
  default_pipeline_opts(DefaultOpts),
  run_pipeline(Text, DefaultOpts, O),  !,
  show_kvs(O),!.

ensure_pipline_spec(_Default,X=Value, [X=Value]):- nonvar(Value), !.
ensure_pipline_spec(_Default,NVPairs,Flat):- is_list(NVPairs), flatten(NVPairs,Flat),member(_=Value,Flat),nonvar(Value),!.
ensure_pipline_spec(Default,Text, [Default=Text]):-!.

run_pipeline(Text, MO):- strip_module(MO,_,O), var(O),!,
   default_pipeline_opts(Opts),
   (O = pipeline_interp(Text, Out)),
   run_pipeline(Text, Opts, Out),!.
   
run_pipeline(Text, NEEDs):- nonvar(NEEDs),
  run_pipeline(Text, NEEDs, NEEDs),!.


run_pipeline(Have, NEEDs, Out):-
  (ensure_pipline_spec(input,Have, NewHave)-> Have\==NewHave),!,
  run_pipeline(NewHave, NEEDs, Out).

run_pipeline(Have, NEEDs, Out):- 
  (canonicalise_defaults(NEEDs, NewNEEDs)-> NEEDs\==NewNEEDs),!,
  run_pipeline(Have, NewNEEDs, Out).

run_pipeline(Have, NEEDs, Out):- select(Tid=TID, Have,Have2), Tid==tid,!,run_pipeline_now(TID,Have2, NEEDs, Out).
run_pipeline(Have, NEEDs, Out):- select(Tid=TID, NEEDs,NEEDs2), Tid==tid,!,run_pipeline_now(TID,Have, NEEDs2, Out).
run_pipeline(Have, NEEDs, Out):- run_pipeline_now(_TID, Have, NEEDs, Out).




run_pipeline_now(TID, Have0, NEEDs0, MOut):- 
   default_pipeline_opts(DefaultOpts),
    merge_defaults(NEEDs0,DefaultOpts,NEEDs),
    strip_module(MOut,_,Out),
   (var(TID)->gensym(iPipeline, TID) ; true),!,
    Have = [tid=TID|Have0],
    setup_call_cleanup(
      once((
         clear_pipeline(TID), init_pipeline(TID), set_pipeline_nvlist(TID, Have),         
         while_tracing_pipeline(dmsg(start(run_pipeline_id(TID, Have, NEEDs, MOut)))),
         flag(TID,_,1),
         while_tracing_pipeline(show_pipeline(TID)))),

      must(run_pipeline_id(TID, NEEDs, ExitWhy, Result)),

      quietly((
         while_tracing_pipeline(dmsg(end(run_pipeline_id(TID, ExitWhy, Result)))),
         must_or_rtrace(get_pipeline_nvlist(1, TID, AllNameValues)),
         %(while_tracing_pipeline(Result==true)->clear_pipeline(TID);true),
         %show_pipeline(TID),
         reverse(AllNameValues, RAllNameValues),
         %show_kvs(RAllNameValues),
         merge_defaults(RAllNameValues,NEEDs,NameValues),
         % print_tree(NameValues),
         mapnvs(NameValues, Out)))), !.

mapnvs(NameValues, O):- var(O),!,NameValues=O,!.
mapnvs(NameValues, O):- canonicalise_defaults(O,CO),
 maplist(ig_value(NameValues),CO).

ig_value(NameValues,N=V):- ignore((member(N=DV,NameValues),V=DV)).
  


factorize_for_print(ListIn,ListO):- \+ is_list(ListIn),!,ListIn=ListO.
factorize_for_print(ListIn,ListO):-
  sort_term_size(ListIn,Set),
  factorize_4_print(Set,Set1),
  reverse(Set1,SetR),
  factorize_4_print(SetR,SetRF),
  reverse(SetRF,ListO),!.

factorize_4_print([N=[V]|SetR],[N=V|List]):- nonvar(N), compound(V),!,
  subst(SetR,V,[$(N)],SetN),
  factorize_4_print(SetN,List).
factorize_4_print([N=V|SetR],[N=V|List]):- nonvar(N), compound(V),  \+ (V = [_]) , !,
  subst(SetR,V,$(N),SetN),
  factorize_4_print(SetN,List).
factorize_4_print(X,X):-!.
  
%must_print_kv(_):- debugging(pipeline),!.
must_print_kv(NonAtom):- compound(NonAtom),compound_name_arity(NonAtom,F,_),!,must_print_kv(F).
must_print_kv(NonAtom):- \+ atom(NonAtom),!,fail.
must_print_kv(e2c_lexical_segs).

%must_print_kv(drs_set).
%must_print_kv(drs1).
%must_print_kv(PNF):- atom_concat('pnf',_,PNF).
%must_print_kv(PNF):- atom_concat('merged_',_,PNF).

show_kvs(Set):- is_list(Set), pretty_numbervars(Set,List), factorize_for_print(List,ListO),sort_term_size(ListO,SetO), !, must_maplist(show_kvs, SetO).
show_kvs(O):- nl,show_kv(O),!.

show_kv(V):- \+ compound(V),!, writeq(V),!.
show_kv(K=V):- !, (must_print_kv(K)-> show_term_tree(K=V) ; show_kv1(K=V)).
show_kv(Term):- show_kv1(Term),!.

show_kv1(Term):- is_webui, !, show_term_tree(Term).
show_kv1(Term):- term_size(Term,Size),
  (Size > 50 -> write_term(Term,[max_depth(5)]) ; show_term_tree(Term)).

% show_term_tree(V):- print_html_term_tree(V,[fullstop(true)]),!.
show_term_tree(V):- print_tree_with_final(V,'.').


:- export(sort_term_size/2).
sort_term_size(List,Sorted):- notrace((predsort(by_word_term_size,List,S),reverse(S,Sorted))).
by_word_term_size(R,A,B):-into_term_size_sort(A,AK),into_term_size_sort(B,BK),compare(RK,AK,BK), (RK == (=) -> compare(R,A,B) ; R = RK).
into_term_size_sort(seg(List),Key):- member(seg(S,E),List),member(lnks(L),List),member(size(W),List),RS is 100-W, Key = seg(S,RS,L,E),!.
into_term_size_sort(I,0):- cyclic_term(I),!.
into_term_size_sort(w(_,AA),Key):- findnsols(2,T, ((sub_term(T,AA),compound(T),arg(1,T,N),number(N));T=AA),Key),!.
into_term_size_sort(Term,Size):- into_term_size(Term,0,Size).

into_term_size(T,M,Size):- var(T), !, Size is M + 1.
into_term_size(T,M,Size):- \+ compound(T), !, Size is M + 1.
into_term_size([H|T],M,Size):-!,into_term_size(H,M,Size1),into_term_size(T,Size1,Size).
into_term_size(T,M,Size):- compound_name_arguments(T,_,Args), into_term_size(Args,M,Size1), Size is Size1 + 1.



%% text_pipeline( +Text:acetext, +NameValues:list )
%
%  Runs Transaction ID with acetext
%
text_pipeline(AceText, AllNameValues):-
  run_pipeline([input=AceText], [untildone=_], AllNameValues).

%% run_pipeline_id( +TID:key, +NEEDs:list )
%
%  Runs Transaction ID until NEEDs is grounded
%
run_pipeline_id(TID, NEEDs, ExitWhy, Result):-
  flag(TID, _, 1),
  while_tracing_pipeline(show_pipeline(TID)),while_tracing_pipeline(sleep(0.3)),
  run_pipeline_id(TID, NEEDs, ExitWhy, 0, Result).

run_pipeline_id(_TID, [] , complete , _N, true):- !.
run_pipeline_id( TID, _NEEDs, error(Name, Err), _N, fail):- local_pipeline_value(1,TID, Name, error(Err)), !.
run_pipeline_id( TID, _NEEDs, no_new_ops, _N, fail):- flag(TID, 0, 0), !.
run_pipeline_id(_TID, _NEEDs, overflow(N), N, fail):- N> 20, !.
run_pipeline_id( TID, _NEEDs, Err, _N, fail):- local_pipeline_value(1,TID, error, Err), !.
run_pipeline_id( TID, NEEDs, ExitWhy, N, Result):-
   partition(is_bound_value(TID), NEEDs, _Bound, Unbound),
   Unbound \== NEEDs, !,
   run_pipeline_id(TID, Unbound, ExitWhy, N, Result).
run_pipeline_id(TID, NEEDsIn, ExitWhy, N, Result):-
    flag(TID, _, 0),
    duplicate_term(NEEDsIn,NEEDs),
    must_or_rtrace((findall(In=Value,local_pipeline_value(1,TID, In, Value),Have),Have\==[])),
    findall(CNV,
      (converter_choice(N,TID,Have, NEEDs, CNV, 7), 
       (while_tracing_pipeline(wdmsg(try_converter(TID, CNV))),
         ignore(try_converter(TID, CNV)))),CNVList),
    ((CNVList \== []) -> nop(flag(TID, _, 1)) ; nop(flag(TID, F, F+1))),
    N2 is N +1,!,
    run_pipeline_id(TID, NEEDs, ExitWhy, N2, Result).

is_bound_value(TID, Name=Value):- var(Value), !, local_pipeline_value(1,TID, Name, Value).
is_bound_value(_TID, _Name=Value):- !, assertion(nonvar(Value)).
is_bound_value(TID, Name):- local_pipeline_value(1,TID, Name, _Value).


currently_supplies(TID,_NEEDs,In):- local_pipeline_value(1,TID, In, _Value),!.
currently_supplies(_TID,NEEDs,In):- member(In=MaybeBound,NEEDs),nonvar(MaybeBound),!.

just_keys(Have,HKeys):- \+ compound(Have),Have=HKeys.
just_keys(Have,HKeys):- is_list(Have), !, maplist(just_keys,Have,HKeys).
just_keys(N=_,N):- nonvar(N),!.
just_keys(N,N).


converter_choice(N,TID,Have,NEEDs, MCNV, MaxDepth):- 
   while_tracing_pipeline((just_keys(Have,HKeys),just_keys(NEEDs,NKeys),show_kvs([have=HKeys,needed=NKeys]))),
  (one_exact_converter_choice(N,TID,Have,NEEDs, MCNV, MaxDepth);%*-> true;
   transitive_converter_choice(N,TID,Have,NEEDs, MCNV, MaxDepth);%*-> true;
   any_converter_choice(N,TID,Have,NEEDs, MCNV, MaxDepth)*-> true;   
   fail),
   while_tracing_pipeline(wdmsg(converter_choice(MCNV))).


one_exact_converter_choice(_, _TID, Have, NEEDs, M:CNV, _MaxDepth):- 
   installed_converter(M, CNV, Ins, Missing),
   \+ \+ member(Missing=_,NEEDs),
   \+ (member(Missing=F,Have),F\==failed),
   forall(member(In,Ins),member(In=_,Have)).

use_exact_converter_choice(_N,_TID,_Have,_NEEDs,_MCNV, MaxDepth):- MaxDepth<0, !, fail.
use_exact_converter_choice(N, TID,Have,NEEDs, MCNV, MaxDepth):- 
   one_exact_converter_choice(N, TID, Have, NEEDs, MCNV, MaxDepth) *-> true
   ; transitive_converter_choice(N, TID, Have, NEEDs, MCNV, MaxDepth).

transitive_converter_choice(N,TID, Have, NEEDs, MCNV, MaxDepth):- 
  findall(In=_,
   (installed_converter(_M, _CNV, Ins, Missing),
    \+ \+ member(Missing=_,NEEDs),
    \+ (member(Missing=F,Have),F\==failed),
    member(In,Ins),\+ member(In=_,Have)), NeededL),
 MaxDepthMinusOne is MaxDepth-1,
 list_to_set(NeededL,Needed), Needed \== [],
 use_exact_converter_choice(N,TID, Have, Needed, MCNV, MaxDepthMinusOne).

any_converter_choice(N,_TID,Have,_NEEDs, M:CNV, _MaxDepth):- 
   (1 is (N mod 2)), N<4,
   installed_converter(M, CNV, Ins, Missing),
   \+ (member(Missing=F,Have),F\==failed),
   forall(member(In,Ins),member(In=_,Have)).


% show stat
show_pipeline(TID):-
  fmt(show_pipeline(TID)),
  forall(local_pipeline_value(1,TID, Name, Value), wdmsg(local_pipeline_value(1,TID, Name, Value))),
  forall(tl:pipeline_pass_fail(TID, Name, Value), wdmsg(pipeline_pass_fail(TID, Name, Value))).

show_pipeline:-
   fmt("List of possible data transformations"),
   forall(installed_converter(M, CNV,_,_), wdmsg(installed_converter(M, CNV))).
%:- dmsg(call(show_pipeline)).


maybe_display(G):- dmsg(call(writeq(G))).

:- fixup_exports.


