/*
% NomicMUD: A MUD server written in Prolog
%
% Some parts used Inform7, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% July 10, 1996 - John Eikenberry
% Copyright (C) 2004 Marty White under the GNU GPL
%
% Dec 13, 2035 - Douglas Miles
%
%
% Logicmoo Project changes:
%
% Main file.
%
*/
:- '$set_source_module'(mu).
%:- rtrace.

% % %:- listing(user:prolog_load_file/2).
% % %:- listing(user:file_search_path/2).
% % %:- listing(user:library_directory/1).
%:- abolish(user:prolog_load_file/2).
%:- retractall(user:prolog_load_file(_, _)).
% :- trace(sub_string/5).
%:- trace(sub_atom/5).
%:- trace('$expanded_term'/10).
%:- trace(user:'prolog_load_file'/2).
%:- trace('$toplevel':'$initialise'/0).
% :- trace(system:'$make_path'/3).
%:- trace('$expand_file_search_path'/4).
%:- trace('functor'/3).

%:- noguitracer.

%:- break.

:- use_module(library('logicmoo_nlu/parser_sharing')).
:- use_module(library('logicmoo_nlu/parser_tokenize')).


adv_load_parsers :-
  parser_e2c:use_module(library(logicmoo_nlu/parser_e2c)),
  parser_pldata:use_module(library(logicmoo_nlu/parser_pldata)),
  parser_chat80:use_module(library(logicmoo_nlu/parser_chat80)),
  !.

adv_load_parsers2 :-
  parser_e2c:ensure_loaded(library(logicmoo_nlu/e2c/e2c_utility)),
  parser_e2c:ensure_loaded(library(logicmoo_nlu/e2c/e2c_commands)),
  parser_e2c:ensure_loaded(library(logicmoo_nlu/e2c/e2c_noun_phrase)),
  !.

:- use_module(library(logicmoo/dcg_must)).
% :- parser_e2c:use_module(library(logicmoo_nlu/parser_e2c)).
%:- adv_load_parsers2.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
% :- ensure_loaded('adv_eng2cmd').
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(300, fx, '~').

cmdalias(d, down).
cmdalias(e, east).
cmdalias(i, inventory).
cmdalias(l, look).
cmdalias(n, north).
cmdalias(s, south).
cmdalias(u, up).
cmdalias(goto, go).
cmdalias(w, west).
cmdalias(x, examine).
cmdalias(z, wait).
cmdalias(a, auto).
cmdalias(get, take).

cmdalias(whom, who).
cmdalias(whois, who).

cmdalias(turn, switch).
cmdalias(flip, switch).

is_prep(P):- munl_call(parser_chat80:prep(prep(PP), _, _, _, _)), PP==P, !.
is_prep(P):- domain_prep(_, P).

is_prep_for_type(P, tObject):- is_prep(P).

domain_prep(_, P) :- xnotrace(member(P, [at, down, in, inside, into, of, off, on, onto, out, over, to, under, up, with])).
domain_prep(_Other, P) :-
 member(P, [of, beside]).

compass_direction(D) :-
 member(D, [north, south, east, west, up, down]).
maybe_compass_direction(D, Actual) :- (cmdalias(D, Actual);D=Actual), compass_direction(Actual).

reflexive_self(W) :- member(W, [self, me, myself , i]). % 'i' inteferes with inventory

strip_noise_words([i, want, to|Tokens], NewTokens) :- strip_noise_words(Tokens, NewTokens).
strip_noise_words(Tokens, NewTokens) :-
 findall(Token,
   ( member(Token, Tokens),
   \+ member(Token, ['please' /* , 'the', 'at', 'a', 'an', 'some', 'thee'*/])),
   NewTokens).

convert_reflexive_self(Agent, Words, NewWords) :-
 % Substitute Agent for 'self'.
 findall(Token,
   ( member(Word, Words),
   ( reflexive_self(Word), Token = Agent;
    Token = Word )),
   NewWords).

:- discontiguous(eng2logic4/4).
:- discontiguous(parse_cmd/4).

% %%%%%%%%%%%%%%
% parser tracing
% %%%%%%%%%%%%%%


:- nb_setval(parsemem, [inst(error)]).

:- meta_predicate(with_parse_mem(*, 0)).
with_parse_mem(Mem, Goal):-
  (nb_current(parsemem, MemWas)->true;parsemem = []),
  setup_call_cleanup(
        b_setval(parsemem, Mem),
        Goal,
        xnotrace(b_setval(parsemem, MemWas))).


is_text_mw(Text):- is_charlist(Text), !.
is_text_mw(Text):- is_list(Text), !, is_codelist(Text).
is_text_mw(Text):- compound(Text), !, fail.
is_text_mw(Text):- string(Text), !.
is_text_mw(Text):- atom_contains(Text, ' '), !.
is_text_mw(Text):- name(Text, Codes), last(Codes, L), code_type(L, punct).

is_logic(Logic):- is_type_functor(E, Logic), E\==eng, !.

is_english(Eng):- is_ftVar(Eng), !, fail.
is_english([Eng|_]):- !, is_english(Eng).
is_english(Eng):- english_directve(Eng), !.
is_english(Eng):- \+ is_logic(Eng), !.

nl_context(Name, Value, Else, Frame ):- declared(Name, Value, Frame)-> true; (Else\=='$fail', Else = Value).
set_nl_context(Name, Value, Frame):- append_term(Name, Value, Prop), redeclare(Prop, Frame, _NewFrame).


parse2state(Text, State):- % reframed_call( Pred, Term, Logic).
       reframed_call(eng2state, istate, Text, State, []).

:- dynamic(parseFrame/2).

parseFrame(e2l, [current_subject(vSpeaker)]).



eng2log(Agent, Words, Logic, Mem):- reframed_call( eng2logic, Agent, Words, Logic, Mem).

eng2cmd(Agent, Prolog, Logic, _Mem):- \+ is_list(Prolog),callable(Prolog),current_predicate(_,Prolog),Logic=call(Prolog),!.
eng2cmd(Agent, Words, Logic, Mem):- reframed_call( eng2action, Agent, Words, Logic, Mem).

eng2log(Term, Logic)  :- eng2logic(Term, Logic).

eng2logic(Term, Logic):- reframed_call(eng2logic, Term, Logic).
eng2action(Term, Logic)  :- reframed_call(eng2action, Term, Logic).
eng2state(Term, Logic):- reframed_call(eng2state, Term, Logic).
eng2query(Term, Logic):- reframed_call(eng2state, Term, Logic).

eng2action(Doer, Cmd, Action, M) :-
  eng2cmd4(Doer, Cmd, Action, M), !.
eng2action(Doer, Cmd, Action, M) :- ( \+ Cmd = [Doer|_] ),
  eng2logic4(Doer, [Doer|Cmd], Action, M).

eng2logic(Self, Words, Cmd, Mem):- [Self|AsCmd] = Words, !,
   eng2cmd4( Self, AsCmd, Cmd, Mem), !.
eng2logic(Self, Words, Cmd, Mem):- eng2cmd4( Self, Words, Cmd, Mem), !.
eng2logic(Self, Words, Logic, Mem):- show_success(eng2state( Self, Words, Logic, Mem)), !.
eng2logic(Self, Words, Logic, Mem):- append([Self, wonders], Words, Decl), show_success(eng2state( Self, Decl, Logic, Mem)), !.




reframed_call(_Pred, Text, _Logic):- var(Text), dumpST, break.
reframed_call(Pred, Text, Logic):-
   nl_context(current_frame, Frame, parseFrame(e2l), istate),
   nl_context(current_subject, Self, vSpeaker, Frame),
   set_nl_context(current_subject, Self, Frame),
   %set_nl_context(current_frame, Mem, Frame),
   into_text80(Text, Term),
   reframed_call(Pred, Self, Term, Logic, Frame), !.

reframed_call( Pred, Self, NonText, Logic, Mem):- 
 reframed_call5( Pred, Self, NonText, PreLogic, Mem), !,
 map_tree_pred(each_logic_var(Mem),PreLogic, Logic).

% expand_logic_vars(PreLogic,Logic, Mem):- ....
each_logic_var(Mem, PreLogic, Here):- 
  (compound(PreLogic), PreLogic= '$'(here);
   PreLogic==here),
  context_agent(Agent, Mem),
  from_loc(Agent, Here, Mem),!.

% -- parse(Doer, WordList, ActionOrQuery, Memory)

reframed_call5(_Pred, _Self, [], [], _Mem) :-!.
reframed_call5(Pred, Self, Logic, NewLogic, Mem) :- compound(Logic), \+ is_list(Logic), is_logic(Logic),
  (Logic = NewLogic -> true;
  (logic2eng_now(Self, Logic, Words), reframed_call_resolve(Pred, Self, Words, NewLogic, Mem))), !.

reframed_call5( Pred, Self, NonText, Logic, Mem) :- \+ is_list(NonText), munl_call(into_text80(NonText, Text)), !, reframed_call5( Pred, Self, Text, Logic, Mem).
reframed_call5( Pred, Self, Words0, Logic, Mem):-
  exclude(=(' '), Words0, Words), Words0\==Words, !,
  reframed_call5( Pred, Self, Words, Logic, Mem).
reframed_call5( Pred, Self, [NonText], Logic, Mem) :- \+ atom(NonText), !, reframed_call5( Pred, Self, NonText, Logic, Mem) .
reframed_call5( Pred, Doer, [rtrace|Args], Logic, M) :- Args\==[], !, rtrace(reframed_call5( Pred, Doer, Args, Logic, M)).
reframed_call5( Pred, Doer, [xnotrace|Args], Logic, M) :- Args\==[], !, xnotrace(reframed_call5( Pred, Doer, Args, Logic, M)).
reframed_call5( Pred, Doer, [notrace|Args], Logic, M) :- Args\==[], !, notrace(reframed_call5( Pred, Doer, Args, Logic, M)).
reframed_call5( Pred, Doer, [cls|Args], Logic, M) :- Args\==[], !, cls, reframed_call5( Pred, Doer, Args, Logic, M).

reframed_call5( Pred, Self, Words, Logic, Mem):- call( Pred, Self, Words, Logic, Mem).


/*
reframed_call( Pred, Doer, [Verb], Action, _M) :- Action=..[Verb, Doer], !.
reframed_call( Pred, Doer, [Verb|TheArgs], Action, M) :-
 args2logical(TheArgs, Args, M), wdmsg( TheArgs->Args), !,
 Action =.. [Verb, Doer|Args].
*/

eng2state(_Doer, Tokens, frame(Logic), Mem) :- fail, current_predicate(declarative/3),
  with_parse_mem(Mem, phrase(declarative(Logic), Tokens, [])).

/*

eng2state(_Doer, Tokens, frame80(Logic), Mem) :- current_predicate(parse_chat80/2),
  with_parse_mem(Mem, parse_chat80(Tokens, Logic)).

eng2state(_Doer, Tokens, frame(Logic), Mem) :- current_predicate(utterance/4),
  with_parse_mem(Mem, phrase(utterance(_, Logic), Tokens, [])).

user:parse_chat80(Text, Q):-
   into_text80(Text, W),
   parser_chat80:((
   try_maybe_p(parser_chat80:sent_to_parsed, W, P),
   try_maybe_p(parser_chat80:i_sentence, P, S),
   try_maybe_p(parser_chat80:clausify80, S, C),
   try_maybe_p(parser_chat80:qplan, C, Q))).
*/
eng2cmd4(_Self, Logic, Logic, _M):- \+ is_list(Logic), !.
eng2cmd4(_Self, [Verb|Args], Logic, _M) :- atom(Verb), verbatum_anon_one_or_zero_arg(Verb), !,
  (Args =[_, _|_] ->
     Logic =.. [Verb, Args];
     Logic =.. [Verb |Args]).

eng2cmd4(_Self, [Verb|Args], Logic, _M) :-  atom(Verb), verbatum_anon_n_args(Verb), !,
  (Args =[_, _|_] ->
      Logic =.. [Verb |Args];
      Logic =.. [Verb |Args]).


eng2cmd4(Self, Words, frame(Cmd), Mem):-
  length(Words, L), L > 3, eng2logic_frame( Self, Words, Cmd, Mem), !.


eng2cmd4(Self, [Alias|Args], Logic, Mem):- cmdalias(Alias, Cmd), !, eng2cmd4(Self, [Cmd|Args], Logic, Mem).
eng2cmd4(Doer, Words, Action, M) :- parse_imperative_movement(Doer, Words, Action, M), !.

% %%%%%%%%%%%%%%
% Take
% %%%%%%%%%%%%%%

eng2cmd4(Doer, [ take| ObjectSpec], (act3('take', Doer, [ Object])), Mem) :- parse2object(ObjectSpec, Object, Mem), !.

eng2cmd4(Doer, Tokens, Logic, Mem) :-
  with_parse_mem(Mem, phrase(parse_cmd(Doer, Logic), Tokens, [])), !.

eng2cmd4(Doer, Words, Action, M) :- fail,
 Words \== [i], % Dont interfere with inventory
 % If not talking to someone else, substitute Agent for 'self'.
 append(Before, [Doer|After], Words),
 reflexive_self(Doer),
 once(thought_check(Agent, propOf(memories, Agent), M);thought_check(Agent, inst(Agent), M)),
 append(Before, [Agent|After], NewWords),
 reframed_call(eng2cmd4, Doer, NewWords, Action, M).



eng2cmd4(Doer, [TheVerb|Args], Action, M) :-
 (F==intransitive;F==transitive),
 (TheVerb = Verb ; Verb = _),
 quietly_talk_db([F, Verb, THE|Forms]),
 member(TheVerb, [Verb, THE|Forms]),

 eng2cmd4(Doer, [Verb|Args], Action, M).

eng2cmd4( Self, Words, Logic, Mem):-  fail,
    \+ member(Self, Words),
   (get_agent_prompt(Self, Prompt)->true;Prompt = [does]),
   append([Self|Prompt], Words, Decl), eng2state( Self, Decl, Logic, Mem), !.

eng2cmd4(Doer, [TheVerb|Args], Action, M) :-
 munl_call(clex_verb(TheVerb, Verb, _, _)),
 Verb\==TheVerb, !,
 eng2cmd4(Doer, [Verb|Args], Action, M).



%parse_cmd(Agent, Logic) --> [X], {parsed_as_simple(X0), same_verb(X0, X), !, Logic=..[X0, Agent]}.


parse_cmd(Agent, Cmd) --> [Alias], {cmdalias(Alias, Cmd), flatten([Cmd], Flat)}, dcg_push(Flat), parse_cmd(Agent, Cmd).

% %%%%%%%%%%%%%%
% Introspection
% %%%%%%%%%%%%%%

self_prop(done_by, mem, memory).
self_prop(object, props, props).

parse_cmd(Doer, inspect(Doer, getprop(Target, PropPred))) --> [PropText],
  {self_prop(Type, PropText, PropPred)}, !, parse_for_optional(Type, Target, Doer).


parse_for_optional(Type, Target, _Else) --> parse_for_kind(Type, Target).
parse_for_optional(_Type, Else, Else) --> [].

%parse_for_kind(_, _) --> [], !, {fail}.
call_dcg_ensure_leftover_parse_for_kind(optional(H,_), TargetH, T, Words, LeftOver):-
  phrase(call_dcg_ensure_leftover(parse_for_kind(H, TargetH), T), Words, LeftOver), !.
call_dcg_ensure_leftover_parse_for_kind(optional(_,Else), Else, _, Words, Words):- !.
call_dcg_ensure_leftover_parse_for_kind(H, TargetH, T, Words, LeftOver):-
  phrase(call_dcg_ensure_leftover(parse_for_kind(H, TargetH), T), Words, LeftOver), !.

parse_for_args_1([], Words, Args):- !, Words =[], Args = [].
parse_for_args_1([optional(H)|T], Words, [TargetH|TargetT]):-
  call_dcg_ensure_leftover_parse_for_kind(H, TargetH, T, Words, LeftOver),
  parse_for_args_1(T, LeftOver, TargetT), !.
parse_for_args_1([optional(_)|T], Words, TargetT):- !, parse_for_args_1(T, Words, TargetT), !.
parse_for_args_1([H|T], Words, [TargetH|TargetT]):-
  call_dcg_ensure_leftover_parse_for_kind(H, TargetH, T, Words, LeftOver),
  parse_for_args_1(T, LeftOver, TargetT), !.
parse_for_args_1([Type], Words, [TargetH]):- parse2object(Type, Words, TargetH, _Ctx).

parse_for_args(ArgTypes, Words, Args):- show_failure(parse_for_args_1(ArgTypes, Words, Args)), !.
parse_for_args(ArgTypes, Words, Args):- fail, (parse_for_args_0(ArgTypes, Words, Args)),
  must((Args\==[Words], length(ArgTypes, L), length(Args, L))), !.


parse_for_args_0(ArgTypes, Words, Args):- phrase(parse_for_kind(ArgTypes, Args), Words, Out), Out==[], !.
parse_for_args_0(_ArgTypes, Text, Args):- coerce_text_to_args(Text, Args), !.

no_more(S, E):- ignore((E=[], S==[])).

parse_for_kind([], []) --> [], !, no_more.
parse_for_kind([H|T], [TargetH|TargetT]) --> {nonvar(T), !},
  call_dcg_ensure_leftover_parse_for_kind(H, TargetH, T),
  parse_for_kind(T, TargetT), !.

parse_for_kind(tfstate, X) --> [X], !.
parse_for_kind(string, X, S, []) :- must_maplist(any_to_string,S,SX),atomics_to_string(SX,' ',X).
parse_for_kind(text, X, S, E) :- parse_for_kind(string, X, S, E).
parse_for_kind(optional(Kind, Value), Target) --> !, (parse_for_kind(Kind, Target); {Value=Target}).
parse_for_kind(or(X, _), Target) --> parse_for_kind(X, Target).
parse_for_kind(or(_, Y), Target) --> !, parse_for_kind(Y, Target).

parse_for_kind(agent, Target) --> !, parse1object(Target).
parse_for_kind(place, Target) --> !, parse1object(Target).
parse_for_kind(inst, Target) --> !, parse1object(Target).
parse_for_kind(agnt2, Target) --> !, parse1object(Target).
parse_for_kind(object, Target) --> !, parse1object(Target).
%parse_for_kind(place, Dest)--> in_agent_model(Dest, h(spatial, _, _, Dest)). %parse_for_kind(place, Dest)--> in_agent_model(Dest, h(Spatially, _, Dest, _)).
parse_for_kind(Type, Target) --> !, parse1object(Target), !, {is_adv_type(Target,Type)}.


parse1object(Target) --> {list_len_between(3, 1, List)}, List, {parse2object(List, Target, _)}.
parse1object(Target) --> [Target], no_more, !.

%parse_for_kind([_], X) --> [X], no_more, !.
% parse_for_kind(_Any, Target) --> !, parse1object(Target).

optional_length(T, Len):- assertion(is_list(T)),length(T,Max),
   include( =(optional(_,_)), T, Opts),length(Opts,Less),
   optional_n_length(Less,Max,Len).
optional_n_length(0,Max,Len):-!,Max=Len.
optional_n_length(Less,Max,Len):-!,Min is Max-Less,between(Min,Max,Len).

call_dcg_ensure_leftover(DCG, [], S, E):- !, call(DCG, S, E).
call_dcg_ensure_leftover(DCG, N, S, E):- number(N),!, length(R, N), append(L, R, S), phrase(DCG, L, Rem), append(Rem, R, E).
call_dcg_ensure_leftover(DCG, T, S, E):- optional_length(T, Len), call_dcg_ensure_leftover(DCG, Len, S, E).

list_len_between(N, M, List):- length(List, N) ;
  (N\=M , (N<M -> N2 is N+1 ; N2 is N-1), list_len_between(N2, M, List)).

word_next_arg_type(who, done_by).
word_next_arg_type(what, object).
word_next_arg_type(where, place).

parse_cmd(Doer, recall(Doer, Who, Target)) --> [Who], {word_next_arg_type(Who, Type)},
  parse_for_optional(Type, Target, Doer).

any_text(Text, Text, []).

eng2assert_text(Text, Action):- eng2logic(Text, Action), !.
eng2assert_text(Text, txt(Text)).
eng2assert_text(Action, S, []):- eng2assert_text(S, Action).

oneOf(List, S, E):-member(I, List), (is_list(I)->phrase(I, S, E);phrase([I], S, E)).
% %%%%%%%%%%%%%%
% Communication
% %%%%%%%%%%%%%%
parse_cmd(Doer, (act3('emote', Doer, [ say, Dest, Emoted]))) --> dcg_from_right(parse_for_kind(agent, Dest), [', ']), eng2assert_text(Emoted).
parse_cmd(Doer, (act3('emote', Doer, [ Say, Dest, Emoted]))) --> [Ask], {ask_to_say(Ask, Say)},
  oneOf([to, from, :, (', '), []]), ignore(parse_for_kind(agent, Dest);parse2object(Dest)), oneOf([to, :, []]), eng2assert_text(Emoted).
%parse_cmd(Doer, (act3('emote', Doer, [ Emoted]))) --> [emote], eng2assert_text(Emoted), !.
%parse_cmd(Doer, say(Doer, Emoted)) --> [say], eng2assert_text(Emoted).

parse_cmd( Self, Logic, [F|Words], []):-
  quietly((type_functor(action, P),
    action_verb_agent_args(P, Fun, Ag, ArgTypes),
    same_verb(F, Fun))),
    % @TODO start using coerce(...).
    show_failure(parse_for_args(ArgTypes, Words, Args)), !,
     (Ag==agent ->
       Logic = act3(Fun, Self,Args);
       Logic =..[Fun|Args]).

% %%%%%%%%%%%%%%
% Simple
% %%%%%%%%%%%%%%

parsed_as_simple(X):- arg(_, v(look, wait, auto, inventory), X).
parse_cmd(Agent, Logic) --> [X|Args], {parsed_as_simple(X0), same_verb(X0, X), !, Logic= act3(X0, Agent,Args)}.

ask_to_say(Ask, say):- arg(_, v(ask, say, tell, talk), Ask).
ask_to_say(Ask, say):- arg(_, v(request, tell), Ask).



% %%%%%%%%%%%%%%
% Give
% %%%%%%%%%%%%%%
/*verb(give,
 [human(Doer), done_by(Doer, Action),
  frame(Action), act_of(give, Action),
  inanimate(Object), objectActedOn(Object, Action),
  human(Recipient), recipient(Recipient, Action) ] ).
*/
%nlac(verbSemTrans, xGiveTheWord, 0, xDitransitiveNPNPFrame, and(objectGiven('ACTION', 'OBJECT'), isa('ACTION', actGivingSomething), giver('ACTION', 'SUBJECT'), givee('ACTION', 'OBLIQUE-OBJECT')), 2046576).
%nlac(infinitive, xGiveTheWord, "give", 638997)
% talkdb:talk_db(transitive, give, gives, gave, giving, given).
acdb(F, A, B):- munl_call(ttholds(F, A, B)).
acdb(F, A, B):- munl_call(acnl(F, A, B, _)).

:- set_prolog_flag(debug_on_error, true).
%munl_call(G):- !, nl_call(G).
munl_call_hide(G):- notrace(fail),
 catch(G, _,
   catch(nl_call(G), _,
      catch(rtrace(nl_call(G)), _, fail))).

munl_call(G):- notrace(\+ current_predicate(_, G)), !, fail.
munl_call(G):- notrace(t_l:rtracing) , notrace, !, (munl_call_1(G)*->trace;(start_rtrace, fail)).
munl_call(G):- tracing , notrace, !, (munl_call_1(G)*->trace;(trace, fail)).
munl_call(G):- munl_call_1(G).
munl_call_1(G):-
 quietly(catch(G, _,
   catch(nl_call_trusted(G), Err,
       (wdmsg(G=Err), !, fail)))).


two_adjs(W1, W2, W3):- var(W1), nonvar(W2), !, two_adjs(W2, W1, W3).
two_adjs(W1, W2, W3):- var(W1), var(W2), !,
      munl_call( wn_s(A, B, W1, _, _, _)), once((munl_call(wn_ant(A, B, C, D)),
      A>C,
      two_adjs_0(A, W2, C, W3, D))).
two_adjs(W1, W2, W3):-
      munl_call(wn_s(A, B, W1, _, _, _)), once((munl_call(wn_ant(A, B, C, D)),
      two_adjs_0(A, W2, C, W3, D))).

two_adjs_0(A, W2, C, W3, D):-
      munl_call(wn_at(A, E)), munl_call(wn_at(C, E)),
      munl_call(wn_s(C, D, W2, _, _, _)), munl_call(wn_s(E, 1, W3, _, _, _)).


%acdb(Past, 'TTWord_Give', "gave")

verb_formtense_str(GiveStr, RootStr, Else):-
    acdb(baseForm, GiveTheWord, GiveStr),
    acdb(posForms, GiveTheWord, xtVerb), !,
    RootStr = GiveStr,
    Else = baseForm.
verb_formtense_str(GaveStr, GiveStr, Past):-
    acdb(Past, GiveTheWord, GaveStr), Past \= inflVerb, Past \= baseForm,
    acdb(posForms, GiveTheWord, xtVerb),
    (acdb(baseForm, GiveTheWord, GiveStr);acdb(inflVerb, GiveTheWord, GiveStr)),
    GaveStr\=GiveStr, !.

verb_formtense_atom(Giving, Give, F-N):-
    (F=transitive;F=intransitive),
    quietly_talk_db([F, Give|Forms]),
    nth0(N, Forms, Giving).
verb_formtense_atom(Giving, Give, Past):-
    munl_call(clex_verb(Giving, Give, _, Past)).


verb_formtense(Var, _, _):- var(Var), !, fail.
verb_formtense(Gave, Give, Past):- atom(Gave), verb_formtense_atom(Gave, Give, Past).
verb_formtense(Gave, Give, Past):- atom(Gave),
    atom_string(Gave, GaveStr),
    verb_formtense_str(GaveStr, GiveStr, Past),
    atom_string(Give, GiveStr).
verb_formtense(GaveStr, GiveStr, Past):- verb_formtense_str(GaveStr, GiveStr, Past).
verb_formtense(GaveStr, GiveStr, Past):-
    atom_string(Gave, GaveStr),
    verb_formtense_atom(Gave, Give, Past),
    atom_string(Give, GiveStr).


verbatum_anon(Verb):- verbatum_anon_n_args(Verb).
verbatum_anon(Verb):- verbatum_anon_one_or_zero_arg(Verb).


verbatum_anon_one_or_zero_arg(Verb):- member(Verb, [
 prolog, make, update_changed_files, cls,
 ls, cd, pwd, debug, mems,
 useragent, echo, halt, english,
 mem, types, props,
 memory, model, properties, state, status, perceptq, help, threads,
 spy, nospy, call,
 rtrace, nortrace,
 trace, xnotrace, notrace %, %whereami, whereis, whoami
 ]).

verbatum_anon_one_or_zero_arg(N):- atom(N), atom_length(N, L), L>1, current_predicate(N/0).

verbatum_anon_n_args(Verb):- member(Verb, [getprops, setprop, path, delprop, rez, derez  %, %whereami, whereis, whoami
 ]).


parse2object(NonList, Target, M):- \+ is_list(NonList), into_text80(NonList,Text80),parse2object(Text80, Target, M).
parse2object(List, Result, M):- append(LList, [R], List), member(R, [(?), (.)]), !, parse2object(LList, Result, M).
parse2object([am, i], Result, M):- once(thought_check(Result, propOf(_, Result), M);thought_check(Result, inst(Result), M)), !.

parse2object([BE| List], Result, M):- fail, quietly_talk_db([_, BE, is|_More]), parse2object(List, Result, M), !.
parse2object([HAS| List], Result, M):- fail, quietly_talk_db([_, have|HASHAVE]), member(HAS, HASHAVE), !, parse2object(List, Result, M).
parse2object([Det| Type], TheThing, M):-
 notrace(nth0(_N, [(unknown), the, thee, old, some, a], Det)), !,
 parse2object(Type, TheThing, M).

parse2object(Requirements0, Thing, Ctx):-
  listify(Requirements0,Requirements),
  guess_req_type(Requirements,Type,NewRequirements),!,
  show_call(parse2object(Type,NewRequirements, Thing, Ctx)).

guess_req_type(Requirements,Type,NewRequirements):-
  append(NewRequirements,[Type],Requirements).

parse2object(Type, Requirements, Thing, Ctx):-
  notrace((as1object_0(Type, Requirements, Thing, Ctx))), !, \+ is_list(Thing).

as1object_0( Type, Requirements, Thing, Ctx):- var(Ctx), !, get_advstate(Ctx), as1object_0(Type, Requirements, Thing, Ctx).
as1object_0(_Type, Requirements, Thing, _Mem):- atom(Requirements), atom_number(Requirements, Thing), !.
as1object_0( Type, Requirements, Thing, M):-  obj_props(M, Thing, Props),
  (same_word(Type, Thing)->true;(match_props(Requirements, Props))),
  is_adv_type(Thing,Type).
as1object_0( Type, [Requirements|More], Thing, M):- !, nonvar(Requirements), as1object_0(Type, Requirements, Thing, M),!,as1object_0(Type, More, Thing, M).
as1object_0(_Type, Requirements, _Thing, _Mem):- \+ atom(Requirements), !, fail. %Requirements=Thing.
as1object_0(_Type, Requirements, Thing, M):- atom_of(inst, Requirements, Thing, M), !.
as1object_0( Type, Requirements, Thing, M):- get_advstate(Mem2), Mem2\=M, as1object_0(Type, Requirements, Thing, Mem2).
% parse2object(Type, Thing, Thing, _Mem).

is_adv_type(I,C):- is_adv_type_0(I,C).
is_adv_type_0(Target,Type):- obj_props(_Both, Target, Props), match_props(Props, Type).
is_adv_type_0(x(Type,_),Type).

match_props(Props, WordSearch):- is_list(WordSearch), !, WordSearch\==[], maplist(match_props(Props), WordSearch).

match_props(_, Word):- is_prep(Word), !, fail.
match_props(Props, Word):- sub_term(Sub, Props), (string(Sub);(atom(Sub))), same_word(Word, Sub), !.

to_string_lc(A, S):- var(A), !, freeze(A, to_string_lc(A, S)).
to_string_lc([], ""):- !.
to_string_lc(S, L):- atomic(S), S\=[], !, string_lower(S, L).
to_string_lc(S, L):- compound_name_arguments(S, 's', SS), !, to_string_lc(SS, L).
to_string_lc(S, L):- catch(text_to_string(L, S), _, fail), !, string_lower(S, L).
to_string_lc(S, L):- is_list(S), !, must_maplist(to_string_lc, S, W), atomics_to_string(W, ' ', L).
to_string_lc(A, S):- format(string(S), '~w', [A]).

adv_to_string_lc(A, LC):- is_x_instance(A), inst_of(A, Type, _), A\=@=Type, !, adv_to_string_lc(Type, LC).
adv_to_string_lc(A, LC):- notrace(to_string_lc(A, LC)), !.


same_word(T1, T2):- notrace((adv_to_string_lc(T1, S1), adv_to_string_lc(T2, S2), !, S1=S2)).
% same_verb(T1, T2):- ground(T1), ground(T2), to_upcase_name(T1, N1), to_upcase_name(T2, N2), !, (atom_concat(N1, _, N2);atom_concat(N2, _, N1)).
same_verb(T1, T2):- notrace((adv_to_string_lc(T1, S1), adv_to_string_lc(T2, S2), !, atom_concat(S2, _, S1))).

same_props(Props1, Props1):- !.
same_props(Props1, Props2):- each_prop(Props1, Prop1), each_prop(Props2, Prop2), same_prop(Prop1, Prop2).
each_prop(Props, Prop):- is_list(Props), !, member(PropsZ, Props), each_prop(PropsZ, Prop).
each_prop(PropC, Prop):- compound(PropC), PropC=Prop.


obj_props(M, Obj, Props):- quietly(obj_props_0(M, Obj, Props)).

get_obj_search(Mem2):- get_advstate(Mem2).
get_obj_search(inst(Agent)):- current_player(Agent).

obj_props_0(M, Obj, Props):- var(M), !, get_obj_search(Mem2), obj_props_0(Mem2, Obj, Props).
obj_props_0(M, Obj, Props):- nonvar(Obj), !, obj_props_0(M, Obj2, Props), Obj=@=Obj2.
obj_props_0(M, Obj, Props):- nonvar(Props), !, obj_props_v(M, Obj, Props2), same_props(Props, Props2).
obj_props_0(M, Obj, Props):- obj_props_v(M, Obj, Props).

obj_props_v(M, _, _):- \+ compound(M), !, fail.
obj_props_v(M, Obj, Props):- is_list(M), !, member(E, M), obj_props_v(E, Obj, Props).
obj_props_v(props(Obj, Props), Obj, Props):- !.
obj_props_v(unused_percept_props(_, _, Obj, _, Props), Obj, Props):- !.
obj_props_v(Term, Obj, Props):- arg(_, Term, M), obj_props_v(M, Obj, Props).

same_prop(X, Y):- X=@=Y, X=Y.

args2logical(TheArgs, [Thing], M):- parse2object(TheArgs, Thing, M), !. % TheArgs\==[Thing], !.
args2logical(TheArgs, TheArgs, _M).

quietly_talk_db(L):- quietly(munl_call(talk_db:talk_db(L))).

mu_is_kind(Thing, inst):- get_advstate(M), member(props(Thing, _), M).
mu_is_kind(Thing, type):- get_advstate(M), member(type_props(Thing, _), M).
%mu_is_kind(Thing, inst):- get_advstate(M), \+ member(type_props(Thing, _), M).

atom_of(Kind, TheThing, Thing, M):- sub_term_atom(Thing, M), mu_is_kind(Thing, Kind), TheThing==Thing, !.
atom_of(Kind, TheThing, Thing, M):- sub_term_atom(Thing, M), mu_is_kind(Thing, Kind), atom_concat(TheThing, _, Thing), !.
atom_of(Kind, TheThing, Thing, M):- sub_term_atom(Thing, M), mu_is_kind(Thing, Kind), atom_concat(_, TheThing, Thing), !.
atom_of(Kind, TheThing, Thing, M):- sub_term_atom(Thing, M), mu_is_kind(Thing, Kind), atom_contains(Thing, TheThing), !.


sub_term_atom(Term, TermO):- \+ compound(Term), !, atom(Term), TermO = Term.
sub_term_atom(Term, [Head|_]) :- nonvar(Head),
 sub_term_atom(Term, Head).
sub_term_atom(Term, [_|Tail]) :- !, nonvar(Tail),
 sub_term_atom(Term, Tail).
sub_term_atom(Term, T) :-
 \+ is_list(T),
 T =.. List,
 sub_term_atom(Term, List).


%to_wordlist_atoms_adv(Sentence, WordsA):- 
%   codelist_to_forms(`( a b "good" #\\. )`,F), to_untyped(F,FF), writeq(FF).

to_wordlist_atoms_adv(Sentence, WordsA):- to_word_list(Sentence, Words), must_maplist(any_to_atom, Words, WordsA), !.
to_wordlist_atoms_adv(Sentence, WordsA):- into_text80(Sentence, WordsA), !.


call_lf(X, LFOut):- freeze(X, ignore(LFOut)).


coerce_text_to_args(Text, Args):- quietly(coerce_text_to_args_0(Text, Args)).

coerce_text_to_args_0(X, []):- []==X, !.
coerce_text_to_args_0(Var, [Arg]):- is_ftVar(Var), Arg=Var, !.
coerce_text_to_args_0(Atom, [Arg]):- is_already_an_arg(Atom), !, Atom=Arg.
coerce_text_to_args_0(Word, [Thing]):- parse2object(Word, Thing, _Mem), !.
coerce_text_to_args_0(Atomic, Arg):- \+ atom(Atomic), !, any_to_atom(Atomic, Atom), !, notrace(coerce_text_to_args_0(Atom, Arg)).
coerce_text_to_args_0(List, [X|Args]):-
   current_predicate(noun_phrase/6),
   is_list(List),
   notrace(to_wordlist_atoms_adv(List, WL)),
   dcg_if_defined(noun_phrase(_SO, X, true, LFOut), WL, Rest), !,
   must((call_lf(X, LFOut),
   from_wordlist_atoms(Rest, More),
   coerce_text_to_args_0(More, Args))).

is_already_an_arg(Var):- is_ftVar(Var), !, fail.
is_already_an_arg(Var):- is_list(Var), !, fail.
is_already_an_arg(Obj):- is_x_instance(Obj), !.
is_already_an_arg(Obj):- number(Obj), !.
is_already_an_arg(NonAtomic):- compound(NonAtomic), !.
is_already_an_arg(Atom):- atom_chars(Atom, [_|Chars]), member(C, Chars),
  (char_type(C, digit); ((char_type(C, to_upper(UC)), C==UC))), !.







% %%%%%%%%%%%%%%
% Movement
% %%%%%%%%%%%%%%

flee_run_escape(flee).
flee_run_escape(run).
flee_run_escape(escape).

% get [out, in, ..] Object
parse_imperative_movement(Doer, [ get, Prep, Object], (act3('go__prep_obj', Doer, [ walk, Prep, Object])), _Mem) :- domain_prep( spatial, Prep).
% n/s/e/w/u/d
parse_imperative_movement(Doer, [Dir], Logic, M):- maybe_compass_direction(Dir, Actual), !, must_mw1(txt2goto(Doer, walk, [Actual], Logic, M)).
% escape/flee/run ..
parse_imperative_movement(Doer, [Escape|Info], Logic, M):- flee_run_escape(Escape), !, must_mw1(txt2goto(Doer, run, Info, Logic, M)).
% out/into
parse_imperative_movement(Doer, [Prep], Logic, M) :- domain_prep(spatial, Prep), !, must_mw1(txt2goto(Doer, walk, [Prep], Logic, M)).
% go ..
parse_imperative_movement(Doer, [go|Info], Logic, M):- !, must_mw1(txt2goto(Doer, walk, Info, Logic, M)).
% outside
parse_imperative_movement(Doer, [ExitName], Logic, M) :-
 in_agent_model(Doer, h(spatial, fn(exit, ExitName), _, _), M), txt2goto(Doer, walk, [ExitName], Logic, M), !.
parse_imperative_movement(Doer, [ ExitName], (act3('go__dir', Doer, [ walk, ExitName])), M) :-
  in_agent_model(Doer, h(spatial, fn(exit, ExitName), _Place, _), M).


parse_imperative_movement(Doer, [get, Prep| More], Logic, M) :- domain_prep(spatial, Prep), !, must_mw1(txt2goto(Doer, walk, [Prep| More], Logic, M)).

% x shelf
% go on shelf

txt2goto(Doer, run, [ ], (act3('go__dir', Doer, [ run, escape])), _Mem) :- !.
txt2goto(Doer, Walk, [to, Prep| More], Logic, M) :- !, txt2goto(Doer, Walk, [Prep| More], Logic, M).
txt2goto(Doer, Walk, [Alias| More], Logic, M) :- cmdalias(Alias, Dir), !, txt2goto(Doer, Walk, [Dir| More], Logic, M).

% go in kitchen
% go in car
txt2goto(Doer, Walk, [ Prep, Dest], (act3('go__prep_obj', Doer, [ Walk, Prep, Where])), M) :-
 domain_prep(spatial, Prep), !,
 must_mw1(txt2place(Dest, Where, M)).

% go north
txt2goto(Doer, Walk, [ ExitName], (act3('go__dir', Doer, [ Walk, ExitName])), M) :-
 in_agent_model(Doer, h(spatial, fn(exit, ExitName), _, _), M).
% go escape
txt2goto(Doer, Walk, [ Dir], (act3('go__dir', Doer, [ Walk, Dir])), _Mem) :- ( compass_direction(Dir);(escape_rel(Escape),Dir==Escape)), !.
txt2goto(Doer, Walk, [ Dir], (act3('go__dir', Doer, [ Walk, Dir])), _Mem) :- (Dir=down;Dir==up), !.
% go [out, in, ..]
txt2goto(Doer, Walk, [ Prep], (act3('go__dir', Doer, [ Walk, Prep])), _Mem) :- domain_prep(spatial, Prep).
% go kitchen
txt2goto(Doer, Walk, Dest, (act3('go__loc', Doer, [ Walk, Where])), M) :-
 txt2place(Dest, Where, M).


txt2place(List, Place, M):- is_list(List), parse2object(List, Object, M), txt2place(Object, Place, M), !.
txt2place(Dest, Place, M):- in_agent_model(advstate_db, h(spatial, _, _, Dest), M), Dest = Place.
txt2place(Dest, Place, M):- in_agent_model(advstate_db, h(spatial, _, Dest, _), M), Dest = Place.
txt2place(Dest, Place, M):- parse2object(Dest, Place, M).



:- discontiguous(verb_frame1/4).
:- discontiguous(eng2flogic_test/1).
:- include(adv_eng2cmd_frame).


:- fixup_exports.


end_of_file.




mu:  ?- xlisting(xGiveTheWord).
nlac(baseForm, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), "giver", 3338280).
nlac(baseForm, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), "giver", 4562556).
nlac(derivedUsingSuffix, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xEr_AgentTheSuffix, 3338267).
nlac(genTemplate, actGivingOfTypeFn, uU(xPhraseFnConstrained, uR(xPhraseFnBar1, xtNoun), uU(xConcatenatePhrasesFn, [uU(xHeadWordOfPhraseFn, uU(xWordFormFnConstrained, xtGerundiveNoun, xGiveTheWord)), uU(xPpPNpFn, xOfTheWord, uU(xParaphraseFnConstrained, nonSingularGeneric, 'ARG1'))])), 2595735).
nlac(genTemplate, agentSupportsAgentGeneric, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG1'), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xPhraseFromStringFn, s("support", "to")), uU(xParaphraseFnNp, 'ARG2')]), 1255496).
nlac(genTemplate, dataTypeSpecifierOfArgumentDeclaration, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG2'), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xNpDetNbarFnDefinite, uU(xPhraseFromStringFn, s("data", "type", "of", "the", "parameter", "declared", "in"))), uU(xParaphraseFnNp, 'ARG1')]), 3927707).
nlac(genTemplate, dataTypeSpecifierOfDeclaration, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG2'), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xNpDetNbarFnDefinite, uU(xPhraseFromStringFn, s("data", "type", "of", "the", "expression", "declared", "by"))), uU(xParaphraseFnNp, 'ARG1')]), 3927705).
nlac(genTemplate, educationInFieldSufficientForActType, uU(xConcatenatePhrasesFn, [uU(xConcatenatePhrasesFn, [uU(xHeadWordOfPhraseFn, uU(xWordFormFnConstrained, gerund, xHaveTheWord)), uU(xNpDetNbarFnIndefinite, uU(xConcatenatePhrasesFn, [uU(xParaphraseFn, 'ARG1'), uU(xHeadWordOfPhraseFn, uU(xWordFormFnConstrained, singularGeneric, xEducateTheWord)), uU(xPpPNpFn, xInTheWord, uU(xParaphraseFnNp, 'ARG2'))]))]), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xPhraseFromStringFn, s("one", "the", "prerequisite", "information", "needed", "to")), uU(xParaphraseFnConstrained, infinitive, 'ARG3')]), 2808553).
nlac(genTemplate, gaveFundsTo, uU(xPhraseFnConstrained, xtNLSentence, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG1'), uU(xHeadVerbForInitialSubjectFn, xHaveTheWord), uU(xWordFormFnConstrained, perfect, xGiveTheWord), uU(xWordFormFnConstrained, plural, xFundTheWord), uU(xPpPNpFn, xToTheWord, uU(xParaphraseFnNp, 'ARG2'))])), 1923227).
nlac(genTemplate, givee, uU(xPhraseFnTensedDefaultPast, 'ARG1', uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG2'), uU(xHeadVerbForInitialSubjectFn, xBeTheWord), uU(xWordFormFnConstrained, perfect, xGiveTheWord), uU(xWordFormFn, xSomethingTheWord), uU(xParaphraseFnPp, xDuringTheWord, 'ARG1')])), 1326005).
nlac(genTemplate, giver, uU(xPhraseFnTensedDefaultPast, 'ARG1', uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG2'), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xPhraseFromStringFn, "something"), uU(xPpPNpFn, xDuringTheWord, uU(xParaphraseFnNp, 'ARG1'))])), 1325837).
nlac(genTemplate, givesPreferentialTreatmentToEntityOfType, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG1'), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xPhraseFromStringFn, s("preferential", "treatment", "to")), uU(xParaphraseFn, 'ARG2'), uU(xPhraseFromStringFn, s("compared", "to", "other", "instances", "of", "the", "class")), uU(xParaphraseFn, 'ARG3')]), 2990141).
nlac(genTemplate, givesPresentationTypeAtMeeting, uU(xPhraseFnConstrained, xtNLSentence, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG1'), uU(xPhraseFnTensed, 'ARG2', uU(xHeadVerbForInitialSubjectFn, xGiveTheWord)), uU(xNpDetNbarFnIndefinite, uU(xParaphraseFn, 'ARG3')), uU(xPpPNpFn, xAtTheWord, uU(xParaphraseFn, 'ARG2'))])), 1428742).
nlac(genTemplate, givesSupportTo, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG1'), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xWordFormFnConstrained, massNumber, xSupportTheWord), uU(xPpPNpFn, xToTheWord, uU(xParaphraseFnNp, 'ARG2'))]), 1648918).
nlac(genTemplate, objectGiven, uU(xPhraseFnTensedDefaultPast, 'ARG1', uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG2'), uU(xHeadVerbForInitialSubjectFn, xBeTheWord), uU(xWordFormFnConstrained, perfect, xGiveTheWord), uU(xPpPNpFn, xToTheWord, uU(xWordFormFn, xSomethingTheWord)), uU(xParaphraseFnPp, xDuringTheWord, 'ARG1')])), 1360265).
nlac(genTemplate, operationalTreeOfOpNumOfInstruction, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG3'), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xNpDetNbarFnDefinite, uU(xPhraseFromStringFn, s("operational", "history", "of", "operand", "number"))), uU(xParaphraseFnNp, 'ARG2'), uU(xWordFormFnConstrained, nonPluralGeneric, xInTheWord), uU(xParaphraseFnNp, 'ARG1')]), 4621348).
nlac(genTemplate, pigments, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG2'), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xParaphraseFnNp, 'ARG1'), uU(xPhraseFromStringFn, "pigment")]), 821335).
nlac(genTemplate, timeStampStringOfProgramActionRecord, uU(xConcatenatePhrasesFn, [uU(xParaphraseFnNp, 'ARG2'), uU(xHeadVerbForInitialSubjectFn, xGiveTheWord), uU(xNpDetNbarFnDefinite, uU(xPhraseFromStringFn, s("timestamp", "of"))), uU(xParaphraseFnNp, 'ARG1')]), 4744535).
nlac(isa, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), tIndividual, 3338270).
nlac(isa, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtDerivedWord, 3338268).
nlac(isa, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtEnglishWord, 3338275).
nlac(isa, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtLexicalWord, 3338269).
nlac(isa, xGiveTheWord, xtEnglishWord, 639002).
nlac(isa, xGiveTheWord, xtLexicalWord, 1133351).
nlac(morphologicallyDerivedFrom, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xGiveTheWord, 3338271).
nlac(posBaseForms, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtCountNoun, 3338272).
nlac(posBaseForms, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtCountNoun, 3338273).
nlac(posBaseForms, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtCountNoun, 4566788).
nlac(posBaseForms, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtNoun, 3338274).
nlac(posBaseForms, xGiveTheWord, xtVerb, 4559023).
nlac(posBaseForms, xGiveTheWord, xtVerb, 4562558).
nlac(posForms, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtCountNoun, 3338282).
nlac(posForms, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtCountNoun, 4570018).
nlac(posForms, xGiveTheWord, xtAdjectiveGradable, 2541643).
nlac(posForms, xGiveTheWord, xtAgentiveNoun, 638996).
nlac(posForms, xGiveTheWord, xtVerb, 4566787).
nlac(posForms, xGiveTheWord, xtVerb, 638994).
nlac(preferredBaseForm, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), "giver", 4559024).
nlac(preferredBaseForm, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), "giver", 4562559).

nlac(agentiveSg, xGiveTheWord, "giver", 638995).

% posForms, derivedUsingSuffix,

nlac(baseForm, xGiveTheWord, "give", 3338279).
nlac(preferredBaseForm, xGiveTheWord, "give", 4561025).
nlac(infinitive, xGiveTheWord, "give", 4562557).
nlac(infinitive, xGiveTheWord, "give", 638997).
nlac(nonGradableAdjectiveForm, xGiveTheWord, "given", 2540248).
nlac(pastTenseUniversal, xGiveTheWord, "gave", 639000).
nlac(perfect, xGiveTheWord, "given", 638993).


nlac(quotedIsa, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtTermSuggestorExpertOnlyTerm, 3338276).
nlac(quotedIsa, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtTermSuggestorExpertOnlyTerm, 3338277).
nlac(quotedIsa, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), xtTermSuggestorExpertOnlyTerm, 3338278).
nlac(singular, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), "giver", 3338281).
nlac(singular, nartR(xWordWithSuffixFn, xGiveTheWord, xEr_AgentTheSuffix), "giver", 4566786).
nlac(compoundSemTransCanonical, xGiveTheWord, uU(vTheListFn, ["an", "alarm", "signal"]), xtMainVerb, xIntransitiveVerbFrame, actGivingAnAlarmSignal, uU(vTheListFn, [performedBy]), 2898338).
nlac(hasVerbAsMember, xGiveVerbClass, xGiveTheWord, 0, 698892).
nlac(lightVerbTransitivesemtrans, xGiveTheWord, actBirthEvent, and(isa('ACTION', actBirthEvent), birthParent('ACTION', 'SUBJECT')), 310818).
nlac(lightVerbTransitivesemtrans, xGiveTheWord, actSpeaking, and(isa('ACTION', actPublicSpeaking), performedBy('ACTION', 'SUBJECT')), 310815).
nlac(participatesInAlternation, xGiveTheWord, 0, xtDoubleObjectAlternation, 639001).
nlac(compoundSemTrans, xGiveTheWord, uU(vTheListFn, ["an", "alarm", "signal"]), xtMainVerb, xIntransitiveVerbFrame, and(isa('ACTION', actGivingAnAlarmSignal), performedBy('ACTION', 'SUBJECT')), 2898393).
nlac(compoundSemTrans, xGiveTheWord, uU(vTheListFn, ["medication"]), xtVerb, xDitransitiveNPNPFrame, and(isa('ACTION', actAdministeringADrug), performedBy('ACTION', 'SUBJECT')), 723758).
nlac(compoundSemTrans, xGiveTheWord, uU(vTheListFn, ["off"]), xtVerb, xTransitiveNPFrame, and(isa('ACTION', eventEmittingAnObject), objectEmitted('ACTION', 'OBJECT'), emitter('ACTION', 'SUBJECT')), 723789).
nlac(headMedialString, uU(vTheListFn, ["Al", "Montaz"]), xGiveTheWord, uU(vTheListFn, ["Ahmed", "Ressam", "money"]), xtVerb, iAlMontazGivesAhmedRessamMoney, 888588).
nlac(headMedialString, uU(vTheListFn, ["the", "organisation", "will"]), xGiveTheWord, uU(vTheListFn, ["you", "advance", "notice", "of", "collection", "dates", "and", "amounts, ", "whether", "you", "set", "up", "a", "Direct", "Debit", "by", "the", "telephone, ", "Internet", "or", "completed", "a", "paper", "Direct", "Debit", "Instruction"]), xtVerb, nartR(stateNthSubSituationTypeOfTypeFn, actSettingUpADirectDebit, actInforming, 2), 1725165).
nlac(verbSemTransCanonical, xGiveTheWord, 4, xTransitiveNPFrame, actYieldingMakingSomethingAvailable, uU(vTheListFn, [performedBy, objectActedOn]), 1896611).
nlac(agentiveNounSemTrans, xGiveTheWord, 0, xGenitiveFrame, and(objectGiven(A, 'POSSESSOR'), isa(A, actGivingSomething), fromPossessor(A, 'NOUN')), 1360226).
nlac(agentiveNounSemTrans, xGiveTheWord, 0, xRegularNounFrame, fromPossessor(_, 'NOUN'), 701451).
nlac(agentiveNounSemTrans, xGiveTheWord, 1, xGenitiveFrame, and(objectGiven(A, 'POSSESSOR'), isa(A, eventTransferringPossession), fromPossessor(A, 'NOUN')), 1360222).
nlac(assertTemplateReln, xtSTemplate, educationInFieldSufficientForActType, uU(vNLPatternListFn, [uU(tSetOfNLPatternExactFn, ["having"]), uU(tSetOfNLPatternExactFn, ["a"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), uU(tSetOfNLPatternExactFn, ["in"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternExactFn, ["one"]), uU(tSetOfNLPatternExactFn, ["the"]), uU(tSetOfNLPatternWordFn, iPrerequisiteTheWord, xtCountNoun), uU(tSetOfNLPatternWordFn, xInformTheWord, xtMassNoun), uU(tSetOfNLPatternExactFn, ["needed"]), uU(tSetOfNLPatternExactFn, ["to"]), uU(tSetOfNLPatternExactFn, ["perform"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), educationInFieldSufficientForActType('ARG1', 'ARG2', 'ARG3'), 2808438).
nlac(assertTemplateReln, xtSTemplate, evaluationsReceived, uU(vNLPatternListFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternExactFn, ["the"]), uU(tSetOfRequireOneFn, ["performer", "performers"]), uU(tSetOfNLPatternExactFn, ["of"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'EVENT'), uU(tSetOfNLPatternExactFn, ["an", "evaluation", "of"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'VAL'), uU(tSetOfNLPatternExactFn, ["for", "how"]), uU(tSetOfRequireOneFn, ["he", "she", "they"]), uU(tSetOfNLPatternExactFn, ["satisfied"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'CRITERIA')]), evaluationsReceived('EVENT', iTheSentenceSubject, 'CRITERIA', 'VAL'), 1388573).
nlac(assertTemplateReln, xtSTemplate, fireSupportToOperation, uU(vNLPatternListFn, [uU(tSetOfNLPatternExactFn, ["during"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), iTemplateCommaMarker, uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["fire", "support"])]), fireSupportToOperation('ARG1', 'ARG2'), 1393531).
nlac(assertTemplateReln, xtSTemplate, fromPossessor, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["In", "During", uU(vWordSequenceFn, ["as", "a", "result", "of"])]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), uU(tSetOfOptionalOneFn, [iTemplateCommaMarker]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xLoseTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xSurrenderTheWord, xtVerb), uU(vWordSequenceFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), "up"])]), uU(tSetOfOptionalOneFn, ["his", "her", "their", "some"]), uU(tSetOfNLPatternExactFn, ["rights", "to"]), uU(tSetOfRequireOneFn, ["something", uU(vWordSequenceFn, ["some", "things"]), uU(vWordSequenceFn, ["some", "stuff"]), uU(vWordSequenceFn, ["some", "objects"])])]), fromPossessor('ARG1', 'ARG2'), 1396360).
nlac(assertTemplateReln, xtSTemplate, giver, uU(vNLPatternListFn, [uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternExactFn, ["something", "during"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1')]), giver('ARG1', 'ARG2'), 1389880).
nlac(assertTemplateReln, xtSTemplate, implementProtectsAgainst, uU(vNLPatternListFn, [uU(tSetOfOptionalSomeFn, ["wearing", "having", "using"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'IMPLEMENT'), uU(tSetOfOptionalSomeFn, ["on"]), uU(tSetOfOptionalSomeFn, ["can", "may", "should", "could"]), uU(tSetOfOptionalSomeFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xLendTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xAffordTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xOfferTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb)]), uU(tSetOfOptionalSomeFn, ["you", "one", "an", "the", "individual"]), uU(tSetOfNLPatternWordFn, xProtectTheWord, xtNLWordForm), uU(tSetOfOptionalSomeFn, ["you", "one", "an", "the", "individual"]), uU(tSetOfRequireSomeFn, ["from", "against"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'AGAINST')]), implementProtectsAgainst('IMPLEMENT', 'AGAINST'), 1394679).
nlac(assertTemplateReln, xtSTemplate, interviewer, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["In", "During"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), uU(tSetOfOptionalOneFn, ["TemplateCommaMarker"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfOptionalOneFn, [uU(vWordSequenceFn, [uU(tSetOfNLPatternWordFn, xBeTheWord, xtVerb), "the", "one", "who"]), uU(vWordSequenceFn, [uU(tSetOfNLPatternWordFn, xBeTheWord, xtVerb), "the", "person", "who"])]), uU(tSetOfOptionalOneFn, [uU(vWordSequenceFn, [uU(tSetOfNLPatternWordFn, xGetTheWord, xtVerb), "to"])]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xConductTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xRunTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xDoTheWord, xtVerb)]), uU(tSetOfRequireOneFn, ["an", "the"]), uU(tSetOfOptionalOneFn, ["actual"]), uU(tSetOfRequireOneFn, ["interview", "interviewing"])]), interviewer('ARG1', 'ARG2'), 1396255).
nlac(assertTemplateReln, xtSTemplate, providesCoverToFrom, uU(vNLPatternListFn, [uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfOptionalOneFn, ["some"]), uU(tSetOfNLPatternExactFn, ["cover", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfNLPatternExactFn, ["from"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), providesCoverToFrom('ARG1', 'ARG2', 'ARG3'), 1388690).
nlac(assertTemplateReln, xtSTemplate, providingCoverToInCOA, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["in", "for"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), uU(tSetOfOptionalOneFn, [iTemplateCommaMarker]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xHelpTheWord, xtVerb)]), uU(tSetOfOptionalSomeFn, ["some", "measure", "any", "amount", "of"]), uU(tSetOfNLPatternExactFn, ["cover", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), providingCoverToInCOA('ARG1', 'ARG2', 'ARG3'), 1395112).
nlac(assertTemplateReln, xtSTemplate, resultsFromTranscriptionOf, uU(vNLPatternListFn, [uU(tSetOfOptionalOneFn, ["the"]), uU(tSetOfOptionalOneFn, ["chemical", uU(vWordSequenceFn, ["process", "of"])]), uU(tSetOfRequireOneFn, ["transcription", "transcribing"]), uU(tSetOfOptionalOneFn, ["of"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xYieldTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xProduceTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xResultTheWord, xtVerb)]), uU(tSetOfOptionalOneFn, ["in"]), uU(tSetOfOptionalOneFn, [uU(vWordSequenceFn, ["the", "generation", "of"]), uU(vWordSequenceFn, ["the", "production", "of"]), uU(vWordSequenceFn, ["the", "expression", "of"])]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1')]), resultsFromTranscriptionOf('ARG1', 'ARG2'), 1396266).
nlac(assertTemplateReln, xtSTemplate, warner, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["in", "during"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'WARN'), uU(tSetOfOptionalOneFn, [iTemplateCommaMarker]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ALARM'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xMakeTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xIssueTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xSoundTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfRequireOneFn, ["an", "a", "the"]), uU(tSetOfRequireOneFn, ["alarm", "sound", "noise", "warned"])]), warner('WARN', 'ALARM'), 1391800).
nlac(assertTemplateReln, xtVPTemplate, agentSupportsAgentGeneric, uU(vNLPatternListFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternExactFn, ["support", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2')]), agentSupportsAgentGeneric(iTheSentenceSubject, 'ARG2'), 1393384).
nlac(assertTemplateReln, xtVPTemplate, fairCoverToFromDirectFireFrom, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xHaveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["fair", "cover", "from", "direct", "fire", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfNLPatternExactFn, ["from"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), fairCoverToFromDirectFireFrom(iTheSentenceSubject, 'ARG2', 'ARG3'), 1390690).
nlac(assertTemplateReln, xtVPTemplate, fairCoverToFromIndirectFireFrom, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xHaveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["fair", "cover", "from", "indirect", "fire", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfNLPatternExactFn, ["from"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), fairCoverToFromIndirectFireFrom(iTheSentenceSubject, 'ARG2', 'ARG3'), 1387129).
nlac(assertTemplateReln, xtVPTemplate, fireSupportToOperation, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["fire", "support"]), uU(tSetOfRequireOneFn, ["for", "during", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1')]), fireSupportToOperation('ARG1', iTheSentenceSubject), 1393031).
nlac(assertTemplateReln, xtVPTemplate, fromPossessor, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xLoseTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xSurrenderTheWord, xtVerb), uU(vWordSequenceFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), "up"])]), uU(tSetOfOptionalOneFn, ["his", "her", "their", "some"]), uU(tSetOfNLPatternExactFn, ["rights", "to"]), uU(tSetOfRequireOneFn, ["something", uU(vWordSequenceFn, ["some", "things"]), uU(vWordSequenceFn, ["some", "stuff"]), uU(vWordSequenceFn, ["some", "objects"])]), uU(tSetOfRequireOneFn, ["in", "during", uU(vWordSequenceFn, ["as", "a", "result", "of"])]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1')]), fromPossessor('ARG1', iTheSentenceSubject), 1389097).
nlac(assertTemplateReln, xtVPTemplate, goodCoverToFromDirectFireFrom, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xHaveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["good", "cover", "from", "direct", "fire", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfNLPatternExactFn, ["from"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), goodCoverToFromDirectFireFrom(iTheSentenceSubject, 'ARG2', 'ARG3'), 1390450).
nlac(assertTemplateReln, xtVPTemplate, goodCoverToFromIndirectFireFrom, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xHaveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["good", "cover", "from", "indirect", "fire", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfNLPatternExactFn, ["from"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), goodCoverToFromIndirectFireFrom(iTheSentenceSubject, 'ARG2', 'ARG3'), 1387366).
nlac(assertTemplateReln, xtVPTemplate, implementProtectsAgainst, uU(vNLPatternListFn, [uU(tSetOfOptionalSomeFn, ["can", "may", "should", "could"]), uU(tSetOfOptionalSomeFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xLendTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xAffordTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xOfferTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb)]), uU(tSetOfOptionalSomeFn, ["you", "one", "an", "the", "individual"]), uU(tSetOfNLPatternWordFn, xProtectTheWord, xtNLWordForm), uU(tSetOfOptionalSomeFn, ["you", "one", "an", "the", "individual"]), uU(tSetOfRequireSomeFn, ["from", "against"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'AGAINST')]), implementProtectsAgainst(iTheSentenceSubject, 'AGAINST'), 1395831).
nlac(assertTemplateReln, xtVPTemplate, mayAuthorize, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["may", "might", "can", "could"]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xAuthorizeTheWord, xtVerb), uU(vWordSequenceFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), "permission", "to"]), uU(vWordSequenceFn, [uU(tSetOfNLPatternWordFn, xGrantTheWord, xtVerb), "permission", "to"]), uU(tSetOfNLPatternWordFn, xPermitTheWord, xtVerb)]), uU(xNLPatternTemplateFn, xtNPTemplate, 'VAR2'), uU(tSetOfNLPatternExactFn, ["to"]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xDoTheWord, xtDoAux), uU(tSetOfNLPatternWordFn, xPerformTheWord, xtVerb)]), uU(xNLPatternTemplateFn, xtNPTemplate, 'VAR3')]), mayAuthorize(iTheSentenceSubject, 'VAR2', 'VAR3'), 1391782).
nlac(assertTemplateReln, xtVPTemplate, partialStructuralExplanation, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xBeTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["a"]), uU(tSetOfOptionalOneFn, ["partial"]), uU(tSetOfNLPatternExactFn, ["structural", "explanation", "for"]), uU(tSetOfRequireOneFn, ["how", "why", "that", uU(vWordSequenceFn, ["the", "fact", "that"]), uU(vWordSequenceFn, ["why", "it", "is", "that"])]), uU(xNLPatternTemplateFn, xtSTemplate, 'S2')]), partialStructuralExplanation(iTheSentenceSubject, 'S2'), 1390295).
nlac(assertTemplateReln, xtVPTemplate, poorCoverToFromDirectFireFrom, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xHaveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["poor", "cover", "from", "direct", "fire", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfRequireOneFn, ["by", "of", "from"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), poorCoverToFromDirectFireFrom(iTheSentenceSubject, 'ARG2', 'ARG3'), 1394511).
nlac(assertTemplateReln, xtVPTemplate, poorCoverToFromIndirectFireFrom, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xHaveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["poor", "cover", "from", "indirect", "fire", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2'), uU(tSetOfNLPatternExactFn, ["from"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), poorCoverToFromIndirectFireFrom(iTheSentenceSubject, 'ARG2', 'ARG3'), 1388532).
nlac(assertTemplateReln, xtVPTemplate, prescribedDosageForPatient, uU(vNLPatternListFn, [iTemplateSingleQuoteMarker, uU(tSetOfNLPatternExactFn, ["s"]), uU(tSetOfOptionalOneFn, ["prescribed"]), uU(tSetOfRequireOneFn, ["dosage", "amount", "quantity"]), uU(tSetOfOptionalSomeFn, ["of", "medicine"]), uU(tSetOfNLPatternExactFn, ["for", "the"]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xDrugTheWord, xtCountNoun), uU(tSetOfNLPatternWordFn, xPharmaceuticalTheWord, xtCountNoun), uU(tSetOfNLPatternWordFn, xMedicineTheWord, xtCountNoun), uU(tSetOfNLPatternWordFn, xSubstanceTheWord, xtCountNoun)]), uU(xNLPatternTemplateFn, xtNPTemplate, 'VAR2'), uU(tSetOfNLPatternWordFn, xBeTheWord, xtVerb), uU(tSetOfOptionalOneFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfOptionalSomeFn, ["at", "the", "rate", "on", "a", "schedule", "of"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'VAR3')]), prescribedDosageForPatient(iTheSentenceSubject, 'VAR2', 'VAR3'), 1391183).
nlac(assertTemplateReln, xtVPTemplate, protectsAgainstType, uU(vNLPatternListFn, [uU(tSetOfOptionalSomeFn, ["may", "can", "could", "might", "should", "often", "will", "does"]), uU(tSetOfOptionalSomeFn, [uU(tSetOfNLPatternWordFn, xBeTheWord, xtBeAux), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xLendTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xOfferTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xAffordTheWord, xtVerb)]), uU(tSetOfOptionalSomeFn, ["used", "to", "for", "in", "a", "an", "one", "way", "means", "method", "you", "the", "individual", uU(tSetOfNLPatternWordFn, nartR(xWordWithSuffixFn, xEffectTheWord, xIveTheSuffix), xtNLWordForm), "provide", "prevent", "solid", "strong", "good"]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProtectTheWord, xtNLWordForm), uU(tSetOfNLPatternWordFn, xDefendTheWord, xtNLWordForm)]), uU(tSetOfOptionalSomeFn, ["you", "one", "an", "the", "individual"]), uU(tSetOfRequireSomeFn, ["from", "against"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ATTACK')]), protectsAgainstType(iTheSentenceSubject, 'ATTACK'), 2941363).
nlac(assertTemplateReln, xtVPTemplate, providingCoverToInCOA, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xHelpTheWord, xtVerb)]), uU(tSetOfOptionalSomeFn, ["some", "measure", "of", "any", "amount"]), uU(tSetOfNLPatternExactFn, ["cover", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3'), uU(tSetOfRequireOneFn, ["in", uU(vWordSequenceFn, ["for", "the"])]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1')]), providingCoverToInCOA('ARG1', iTheSentenceSubject, 'ARG3'), 1394282).
nlac(assertTemplateReln, xtVPTemplate, supportsForceGeneral, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["general", "support", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2')]), supportsForceGeneral(iTheSentenceSubject, 'ARG2'), 1390594).
nlac(assertTemplateReln, xtVPTemplate, supportsUnitDirect, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["direct", "support", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2')]), supportsUnitDirect(iTheSentenceSubject, 'ARG2'), 1392523).
nlac(assertTemplateReln, xtVPTemplate, warner, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xIssueTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xSoundTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xMakeTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfRequireOneFn, ["an", "a", "the"]), uU(tSetOfRequireOneFn, ["sound", "alarm", "noise", "warned"]), uU(tSetOfRequireOneFn, ["in", "during"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'WARN')]), warner('WARN', iTheSentenceSubject), 1395307).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["a", "birthday", "present"]), xtVerb, nartR(tColOfSubcollectionOfWithRelationToTypeFn, actGiftGiving, objectGiven, tBirthdayPresent), 4531425).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["a", "lecture", "on", "a", "scientific", "topic"]), xtVerb, actScientificLecturing, 5078633).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["an", "alarm", "signal"]), xtMainVerb, actGivingAnAlarmSignal, 2898389).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["an", "organism", "water"]), xtVerb, actMakingWaterAvailableToOrganism, 1588709).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["a", "speech", "as", "part", "of", "an", "election", "campaign"]), xtVerb, actDeliveringAStumpSpeech, 2675701).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["a", "stump", "speech"]), xtVerb, actDeliveringAStumpSpeech, 2675702).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["away", "the", "bride"]), xtVerb, actCeremonialGivingAwayOfTheBride, 4545776).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["directions"]), xtVerb, actGivingSomeoneDirections, 4898795).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["economic", "aid"]), xtVerb, actGivingEconomicAid, 5078494).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["forth"]), xtVerb, eventExhalingSomeGasOrOdor, 2784617).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["head"]), xtVerb, actCunnilingus, 473713).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["humanitarian", "aid"]), xtVerb, actGivingHumanitarianAid, 5078441).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["off"]), xtVerb, eventEmission, 1915025).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["out"]), xtVerb, eventEmission, 1915026).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, [s("a", "demonstration")]), xtVerb, actGivingADemonstration, 4755187).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, [s("a", "speech")]), xtVerb, actPoliticalOratory, 5577668).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["something", "water"]), xtVerb, actMakingWaterAvailableToOrganism, 1588706).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, [s("someone", "directions", "to", "a", "restroom")]), xtVerb, actGivingSomeoneDirectionsToARestroom, 5005140).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["the", "bride", "away"]), xtVerb, actCeremonialGivingAwayOfTheBride, 4545777).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["the", "solution", "to", "an", "expression", "comparison", "problem"]), xtGerundiveNoun, nartR(actProvidingTheAnswerToMathProblemOfTypeFn, cwExpressionComparisonProblem), 5653700).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["the", "solution", "to", "an", "expression", "comparison", "problem"]), xtVerb, nartR(actProvidingTheAnswerToMathProblemOfTypeFn, cwExpressionComparisonProblem), 5653701).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["the", "solution", "to", "a", "one-variable", "linear", "equation"]), xtGerundiveNoun, nartR(actProvidingTheAnswerToMathProblemOfTypeFn, cwSolveLinearEquationOfOneVariableProblem), 5616974).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["the", "solution", "to", "a", "one-variable", "linear", "equation"]), xtVerb, nartR(actProvidingTheAnswerToMathProblemOfTypeFn, cwSolveLinearEquationOfOneVariableProblem), 5616975).
nlac(compoundString, xGiveTheWord, uU(vTheListFn, ["up"]), xtVerb, eventSparingTransferringPossession, 2066894).
nlac(denotationRelatedTo, xGiveTheWord, xtAgentiveNoun, 0, fromPossessor, 690404).
nlac(denotationRelatedTo, xGiveTheWord, xtVerb, 2, outputsCreated, 690844).
nlac(denotation, xGiveTheWord, xtAgentiveNoun, 0, giver, 782009).
nlac(denotation, xGiveTheWord, xtVerb, 0, actGivingSomething, 638999).
nlac(denotation, xGiveTheWord, xtVerb, 12, givesSupportToAgentOfCategory, 2803105).
nlac(denotation, xGiveTheWord, xtVerb, 1, eventTransferringPossession, 897295).
nlac(denotation, xGiveTheWord, xtVerb, 3, actGivingOfferingCommunicationAct, 1859844).
nlac(denotation, xGiveTheWord, xtVerb, 4, actYieldingMakingSomethingAvailable, 1896610).
nlac(multiWordString, uU(vTheListFn, ["gift"]), xGiveTheWord, xtGerundiveNoun, actGiftGiving, 756040).
nlac(multiWordString, uU(vTheListFn, ["name"]), xGiveTheWord, xtVerb, actNamingSomething, 5048186).
nlac(nounSemTrans, xGiveTheWord, 78, xGenitiveFrame, and(isa('NOUN', actYieldingMakingSomethingAvailable), objectActedOn('NOUN', 'POSSESSOR')), 1928244).
nlac(queryTemplateReln, xtQuestionTemplate, definitionalDisplaySentence, uU(vNLPatternListFn, [uU(tSetOfOptionalOneFn, [s("can", "you")]), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfRequireOneFn, [uU(vWordSequenceFn, ["give", "me", "information", "about"]), uU(vWordSequenceFn, ["give", "me", "information", "on"])]), uU(xNLPatternTemplateFn, xtNPTemplate, 'THING'), uU(tSetOfOptionalOneFn, [iTemplateQuestionMarkMarker])]), definitionalDisplaySentence('THING', _), 1632333).
nlac(queryTemplateReln, xtQuestionTemplate, fromPossessor, uU(vNLPatternListFn, [uU(tSetOfNLPatternWordFn, xDoTheWord, xtDoAux), uU(xNLPatternTemplateFn, xtNPTemplate, 'SUBJECT'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xLoseTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xSurrenderTheWord, xtVerb), uU(vWordSequenceFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), "up"])]), uU(tSetOfOptionalOneFn, ["his", "her", "their", "some"]), uU(tSetOfNLPatternExactFn, ["rights", "to"]), uU(tSetOfRequireOneFn, ["something", uU(vWordSequenceFn, ["some", "things"]), uU(vWordSequenceFn, ["some", "stuff"]), uU(vWordSequenceFn, ["some", "objects"])]), uU(tSetOfRequireOneFn, ["in", "during", uU(vWordSequenceFn, ["as", "a", "result", "of"])]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), uU(tSetOfOptionalOneFn, [iTemplateQuestionMarkMarker])]), fromPossessor('ARG1', 'SUBJECT'), 1394479).
nlac(queryTemplateReln, xtQuestionTemplate, fromPossessor, uU(vNLPatternListFn, [uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), uU(tSetOfNLPatternWordFn, xDoTheWord, xtDoAux), uU(xNLPatternTemplateFn, xtNPTemplate, 'SUBJECT'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xLoseTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xSurrenderTheWord, xtVerb), uU(vWordSequenceFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), "up"])]), uU(tSetOfOptionalOneFn, ["his", "her", "their", "some"]), uU(tSetOfNLPatternExactFn, ["rights", "to"]), uU(tSetOfRequireOneFn, ["something", uU(vWordSequenceFn, ["some", "things"]), uU(vWordSequenceFn, ["some", "stuff"]), uU(vWordSequenceFn, ["some", "objects"])]), uU(tSetOfRequireOneFn, ["in", "during", uU(vWordSequenceFn, ["as", "a", "result", "of"])]), uU(tSetOfOptionalOneFn, [iTemplateQuestionMarkMarker])]), fromPossessor('ARG1', 'SUBJECT'), 1391817).
nlac(queryTemplateReln, xtQuestionTemplate, givesSupportToAgentOfCategory, uU(vNLPatternListFn, [uU(tSetOfNLPatternWordFn, xBeTheWord, xtBeAux), uU(xNLPatternTemplateFn, xtNPTemplate, 'ORG'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(xNLPatternTemplateFn, xtNPTemplate, 'SUPPORTTYPE'), uU(tSetOfRequireOneFn, ["to", "for"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'AGT')]), givesSupportToAgentOfCategory('ORG', 'AGT', 'SUPPORTTYPE'), 1392982).
nlac(queryTemplateReln, xtQuestionTemplate, implementProtectsAgainst, uU(vNLPatternListFn, [uU(xNLPatternTemplateFn, xtNPTemplate, 'AGAINST'), uU(tSetOfOptionalSomeFn, ["can", "may", "should", "could"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'SUBJECT'), uU(tSetOfOptionalSomeFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xLendTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xAffordTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xOfferTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb)]), uU(tSetOfOptionalSomeFn, ["you", "one", "an", "the", "individual"]), uU(tSetOfNLPatternWordFn, xProtectTheWord, xtNLWordForm), uU(tSetOfOptionalSomeFn, ["you", "one", "an", "the", "individual"]), uU(tSetOfRequireSomeFn, ["from", "against"]), uU(tSetOfOptionalOneFn, [iTemplateQuestionMarkMarker])]), implementProtectsAgainst('SUBJECT', 'AGAINST'), 1392292).
nlac(queryTemplateReln, xtQuestionTemplate, providingCoverToInCOA, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["in", "for"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), iTemplateCommaMarker, uU(tSetOfNLPatternExactFn, ["what"]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xHelpTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["cover", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG3')]), providingCoverToInCOA('ARG1', _, 'ARG3'), 1390089).
nlac(queryTemplateReln, xtQuestionTemplate, supportsForceGeneral, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["who", "what"]), uU(tSetOfNLPatternWordFn, xDoTheWord, xtDoAux), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["general", "support", "to"])]), supportsForceGeneral('ARG1', 'ARG2'), 1392976).
nlac(queryTemplateReln, xtQuestionTemplate, supportsForceGeneral, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["who", "what"]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["general", "support", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2')]), supportsForceGeneral(_, 'ARG2'), 1389351).
nlac(queryTemplateReln, xtQuestionTemplate, supportsUnitDirect, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["who", "what"]), uU(tSetOfNLPatternWordFn, xDoTheWord, xtDoAux), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG1'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["direct", "support", "to"])]), supportsUnitDirect('ARG1', 'ARG2'), 1391860).
nlac(queryTemplateReln, xtQuestionTemplate, supportsUnitDirect, uU(vNLPatternListFn, [uU(tSetOfRequireOneFn, ["who", "what"]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["direct", "support", "to"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'ARG2')]), supportsUnitDirect(_, 'ARG2'), 1393830).
nlac(queryTemplateReln, xtQuestionTemplate, warner, uU(vNLPatternListFn, [uU(tSetOfNLPatternWordFn, xDoTheWord, xtDoAux), uU(xNLPatternTemplateFn, xtNPTemplate, 'SUBJECT'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xIssueTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xSoundTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfNLPatternExactFn, ["the", "warned"]), uU(tSetOfRequireOneFn, ["in", "during"]), uU(xNLPatternTemplateFn, xtNPTemplate, 'WARN'), uU(tSetOfOptionalOneFn, [iTemplateQuestionMarkMarker])]), warner('WARN', 'SUBJECT'), 1389224).
nlac(queryTemplateReln, xtQuestionTemplate, warner, uU(vNLPatternListFn, [uU(xNLPatternTemplateFn, xtNPTemplate, 'WARN'), uU(tSetOfNLPatternWordFn, xDoTheWord, xtDoAux), uU(xNLPatternTemplateFn, xtNPTemplate, 'SUBJECT'), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xIssueTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xSoundTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xMakeTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb)]), uU(tSetOfRequireOneFn, ["an", "a", "the"]), uU(tSetOfRequireOneFn, ["sound", "alarm", "noise", "warned"]), uU(tSetOfRequireOneFn, ["in", "during"]), uU(tSetOfOptionalOneFn, [iTemplateQuestionMarkMarker])]), warner('WARN', 'SUBJECT'), 1387475).
nlac(queryTemplateReln, xtSTemplate, definitionalDisplaySentence, uU(vNLPatternListFn, [uU(tSetOfOptionalOneFn, [uU(tSetOfNLPatternExactFn, ["please"])]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xGiveTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xProvideTheWord, xtVerb), uU(tSetOfNLPatternWordFn, xTellTheWord, xtVerb)]), uU(tSetOfOptionalOneFn, [uU(tSetOfNLPatternExactFn, ["to", "me"]), uU(tSetOfNLPatternExactFn, ["me"]), uU(tSetOfNLPatternExactFn, ["me", "with"])]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternExactFn, ["a"]), uU(tSetOfNLPatternExactFn, ["the"]), uU(tSetOfNLPatternExactFn, ["an"])]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternWordFn, xDefineTheWord, xtCountNoun), uU(tSetOfNLPatternWordFn, xMeanTheWord, xtCountNoun)]), uU(tSetOfRequireOneFn, [uU(tSetOfNLPatternExactFn, ["of"]), uU(tSetOfNLPatternExactFn, ["for"])]), uU(tSetOfOptionalOneFn, [uU(tSetOfNLPatternExactFn, ["a"]), uU(tSetOfNLPatternExactFn, ["an"])]), uU(xNLPatternTemplateFn, xtNBarTemplate, 'THING'), uU(tSetOfOptionalOneFn, [iTemplatePeriodMarker])]), definitionalDisplaySentence('THING', _), 1632324).
nlac(subcatFrame, xGiveTheWord, xtAgentiveNoun, 0, xGenitiveFrame, 1790709).
nlac(subcatFrame, xGiveTheWord, xtAgentiveNoun, 0, xRegularNounFrame, 1790728).
nlac(subcatFrame, xGiveTheWord, xtAgentiveNoun, 1, xGenitiveFrame, 1790733).
nlac(subcatFrame, xGiveTheWord, xtCountNoun, 78, xGenitiveFrame, 1929125).
nlac(subcatFrame, xGiveTheWord, xtVerb, 0, nartR(xPPCompFrameFn, ttDitransitivePPFrameType, xToTheWord), 764623).
nlac(subcatFrame, xGiveTheWord, xtVerb, 0, nartR(xPPCompFrameFn, ttTransitivePPFrameType, xOffTheWord), 765162).
nlac(subcatFrame, xGiveTheWord, xtVerb, 0, xDitransitiveNPNPFrame, 638998).
nlac(subcatFrame, xGiveTheWord, xtVerb, 0, xTransitiveNPFrame, 638992).
nlac(subcatFrame, xGiveTheWord, xtVerb, 1, xDitransitiveNPNPFrame, 638990).
nlac(subcatFrame, xGiveTheWord, xtVerb, 1, xTransitiveNPFrame, 638991).
nlac(subcatFrame, xGiveTheWord, xtVerb, 4, nartR(xPPCompFrameFn, ttDitransitivePPFrameType, xForTheWord), 3309535).
nlac(subcatFrame, xGiveTheWord, xtVerb, 4, nartR(xPPCompFrameFn, ttDitransitivePPFrameType, xToTheWord), 3309257).
nlac(subcatFrame, xGiveTheWord, xtVerb, 4, xTransitiveNPFrame, 1896613).
nlac(verbSemTrans, xGiveTheWord, 0, nartR(xPPCompFrameFn, ttDitransitivePPFrameType, xToTheWord), and(objectGiven('ACTION', 'OBJECT'), isa('ACTION', actGivingSomething), giver('ACTION', 'SUBJECT'), givee('ACTION', 'OBLIQUE-OBJECT')), 1360228).
nlac(verbSemTrans, xGiveTheWord, 0, nartR(xPPCompFrameFn, ttTransitivePPFrameType, xOffTheWord), and(isa('ACTION', eventEmittingAnObject), objectEmitted('ACTION', 'OBLIQUE-OBJECT'), emitter('ACTION', 'SUBJECT')), 764838).
nlac(verbSemTrans, xGiveTheWord, 0, xDitransitiveNPNPFrame, and(objectGiven('ACTION', 'OBJECT'), isa('ACTION', actGivingSomething), giver('ACTION', 'SUBJECT'), givee('ACTION', 'OBLIQUE-OBJECT')), 2046576).
nlac(verbSemTrans, xGiveTheWord, 4, nartR(xPPCompFrameFn, ttDitransitivePPFrameType, xForTheWord), and(isa('ACTION', actYieldingMakingSomethingAvailable), performedBy('ACTION', 'SUBJECT'), transportees('ACTION', 'OBJECT'), target('ACTION', 'OBLIQUE-OBJECT')), 3308275).
nlac(verbSemTrans, xGiveTheWord, 4, nartR(xPPCompFrameFn, ttDitransitivePPFrameType, xToTheWord), and(isa('ACTION', actYieldingMakingSomethingAvailable), performedBy('ACTION', 'SUBJECT'), transferredObject('ACTION', 'OBJECT'), target('ACTION', 'OBLIQUE-OBJECT')), 3307880).
nlac(verbSemTrans, xGiveTheWord, 4, xTransitiveNPFrame, and(isa('ACTION', actYieldingMakingSomethingAvailable), performedBy('ACTION', 'SUBJECT'), objectActedOn('ACTION', 'OBJECT')), 1896612).

