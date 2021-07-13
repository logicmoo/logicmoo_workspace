/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/
:- '$set_source_module'(mu).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
% :- ensure_loaded(adv_log2eng).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flag_level_compare(Flag, Prop):-flag(Flag, Level, Level), Prop=..[F|Args], apply(F, [Level|Args]).

xtreme_english :- flag_level_compare('english', >(2)).
%any_english :- \+ no_english.
%no_english :- flag_level_compare('english', =(0)).

:- ignore(flag('english', 0, 2)).

pretty :- \+ flag_level_compare(pretty, =(0)).

:- ignore(flag(pretty, 0, 1)).


player_pprint(Doer, Logic, always):- xtreme_english, !, must(print_english(Doer, Logic)).
player_pprint(_Doer, D, K):- pprint(D, K).

% print_english(Doer, Logic):- is_list(Logic), !, must_maplist(print_english(Doer), Logic).
print_english(Doer, Logic):- logic2english(Doer, Logic, Text), write(Text). % pprint(Text, always).


same_agent(A, B):- A=@=B.


logic2english(Logic, Text):-
  logic2english(_Doer, Logic, Text).

l2e(Logic):-
 logic2english(Logic, Text), wdmsg(Text).

:- fixup_exports.

% A percept or event:
% - is a logical description of what happened
% - includes English or other translations
% - may be queued for zero, one, many, or all agents.
% - may have a timestamp
% queue_percpt(Agent, [Logical, English|_], S0, S9).
% where Logical is always first, and other versions are optional.
% Logical should be a term, like sees(Thing).
% English should be a list.



list2eng(Obj, Some, English):-
 list2eng([], Obj, Some, English).

punct_or(Punct, Else, Value):- member(Else=Value, Punct)-> true ; Else=Value.

list2eng(Punct, Obj, Some, English):-  is_list(Some), !,
  must_maplist(logic2english(Obj), Some, SomeUnused),
  exclude(unused_text, SomeUnused, SomeGone),
  list2eng_a(Punct, Obj, SomeGone, English).
list2eng(Punct, Obj, Some, English) :-
  punct_or(Punct, logic2eng_now, Log2Eng),
  call(Log2Eng, Obj, Some, English), !.

unused_text([]).
unused_text(unused(_)).
unused_text('').
unused_text([A]):- nonvar(A), !, unused_text(A).

list2eng_a(Punct, _Obj, [], [Nothing]):- punct_or(Punct, '<nothing>', Nothing).
:- nb_setval(list2eng, []).
list2eng_a(_Punct, Obj, Some, [' ['| English]) :- nb_current(list2eng, D), number(D), !,
 list2eng_e(['.'=']', 'and'=', '], Obj, Some, English), !.
list2eng_a(Punct, Obj, Some, English) :- nb_current(list2eng, D), b_setval(list2eng, 1),
 list2eng_e(Punct, Obj, Some, English), !,
 b_setval(list2eng, D).


list2eng_e(Punct, Obj, [Single], English) :- !,
 punct_or(Punct, logic2eng_now, Log2Eng),
 call(Log2Eng, Obj, Single, Named),
 punct_or(Punct, '.', PERIOD),
 flatten([Named, PERIOD], English).

list2eng_e(Punct, Obj, [Last2, Last1], English) :-
 punct_or(Punct, logic2eng_now, Log2Eng),
 call(Log2Eng, Obj, Last2, Named2),
 list2eng(Obj, Last1, Named1),
 punct_or(Punct, 'and', AND),
 punct_or(Punct, '.', PERIOD),
 flatten([Named2, AND, Named1, PERIOD], English).

list2eng_e(Punct, Obj, [Some| More], English) :-
 punct_or(Punct, logic2eng_now, Log2Eng),
 call(Log2Eng, Obj, Some, Named),
 punct_or(Punct, ', ', COMMA),
 list2eng_e(Punct, Obj, More, MoreNamed),
 flatten([Named, COMMA, MoreNamed], English).

list2eng_e(Punct, Obj, Some, English) :-
 punct_or(Punct, logic2eng_now, Log2Eng),
 call(Log2Eng, Obj, Some, English), !.


timestamped_pred(holds_at).


reason_to_english(_Aobj, cant( sense(_Agent, Sense, It, Why)), [ 'can''t sense', It, ' ', ly(Sense), ' here', cuz(Why)]).
reason_to_english(_Aobj, cant( reach(_Agent, It)), [ 'can''t reach ', It]).
reason_to_english(_Aobj, cant( manipulate(self)), [ 'can''t manipulate yourself like that']).
reason_to_english(_Aobj, alreadyhave(It), ['already have', the(It)]).
reason_to_english(_Aobj, mustgetout(It), ['must_mw get out/off ', It, ' first.']).
reason_to_english(_Aobj, self_relation(It), ['can\'t put ', It, ' inside itself!']).
reason_to_english(_Aobj, moibeus_relation( _, _), ['Topological error!']).
reason_to_english(_Aobj, =(Dark, t), ['It''s too ', Dark, ' to ', Sense, in, '!']):- problem_solution(Dark, Sense, _Light).
reason_to_english(_Aobj, must_drop(It), [ 'will have to drop', It, ' first.']).
reason_to_english(_Aobj, cant( act3('move',_Agent,[ It])), [ It, aux( be), 'immobile']).
reason_to_english(_Aobj, cant( act3('take',_Agent,[ It])), [ It, aux( be), 'untakeable']).
reason_to_english(_Aobj, cantdothat(EatCmd), [ 'can\'t do: ', EatCmd]).


logic2eng( Obj, Prop, English):-
 guess_pretty(Prop),
 \+ ground(Prop), copy_term(Prop, Prop2), !,
 mw_numbervars(Prop2, 55, _), logic2eng(Obj, Prop2, English).

logic2eng(_Obj, desc = (Out), [' "', Out, '"']):- !.
logic2eng(_Obj, setprop(Obj, Prop), [Obj, setprop, Prop]):- !.
logic2eng(Obj, Some, English):- \+ attvar(English), \+ pretty, dif(English, []), !, logic2eng(Obj, Some, English).
logic2eng(Context, Inst, TheThing):- atom(Inst), inst_of(Inst, Type, N), !,
 (nth0(N, [(unknown), '', thee, old, some, a], Det) -> true; atom_concat('#', N, Det)),
 compile_eng_txt(Context, [Det, Type], TheThing).

%logic2eng( Obj, Prop, [cap(N), of, O, aux(be), Value]):- Prop =..[N, O, V], list2eng(Obj, V, Value).

% logic2eng(Obj, Prop, English):- Prop =..[N, Obj1, A| VRange], Obj1==Obj, Prop2 =..[N, A| VRange], logic2eng( Obj, Prop2, English).
%logic2eng(_Obj, Prop, [String]):- compound(Prop), !, String=''. % format(atom(String), ' \n {{ ~q. }}\n ', [Prop]), !.

/*logic2eng(_Obj, Prop, [String]):- compound(Prop), \+ xtreme_english, !, format(atom(String), ' {{ ~q }} ', [Prop]), !.
logic2eng( Obj, Prop, [cap(N), Value, aux(be), English]):- Prop =..[N, V| Range],
 logic2eng(Obj, V, Value),
 must_maplist(logic2eng(Obj), Range, English).
%logic2eng(_Obj, Prop, [String]):- format(atom(String), '~w', [Prop]), !.
*/

%logic2eng(Obj, Var, [Text]):- var(Var), !, format(atom(Text), '{{~q}}', [logic2eng(Obj, Var)]).
logic2eng( Obj, Logic, English):- reason_to_english( Obj, Logic, English),!.

logic2eng(_Obj, '$VAR'(Prop), English):- format(atom(English), ' ?VAR-~w', [Prop]), !.
logic2eng(_Obj, English, English):- english_directve(English), !.
logic2eng(_Obj, [English|Rest], [English|Rest]):- english_directve(English), !.
logic2eng(_Obj, [], []).
logic2eng(Context, extra_verbose_eng(Eng), '...extra_verbose_eng...'(Compiled) ):-
 compile_eng_txt(Context, Eng, Compiled).

logic2eng(Context, extra_verbose(Logic), '...extra_verbose...'(Compiled) ):-
 logic2eng(Context, Logic, Compiled).

logic2eng(Context, single_event(Evt), Eng):- !, logic2eng(Context, Evt, Eng).
logic2eng(_Context, '<mystery>'(Why, Prep, About), [a, (mystery), because, quoted([Prep, About]), (', '), aux(be), (Why)]):- !.



logic2eng(Obj, [Prop|Tail], Text) :- !,
 must_mw1((logic2eng(Obj, Tail, UText2) ->
 flatten([UText2], Text2),
 must_mw1(logic2eng(Obj, Prop, UText1)) ->
 flatten([UText1], Text1),
 append_if_new(Text1, Text2, Text))), !.


logic2eng(Obj, HWestFromTo_At, [ Ago | Info]):-
  HWestFromTo_At =.. [H, West, From|To_At],
  timestamped_pred(H),
  append(To, [At], To_At), number(At), !,
  logic2eng(Obj, ago(At), Ago),
  HWestFromTo =.. [H, West, From|To],
  logic2eng(Obj, HWestFromTo, Info).

%logic2eng(_Obj, Prop, [String]):- compound(Prop), no_english, format(atom(String), '~q', [Prop]), !.
logic2eng( Obj, ~(Type), ['(', 'logically', 'not', '(', Out, '))']):- must_mw1(logic2eng(Obj, Type, Out)), !.

logic2eng(_Context, time_passes(Agent), ['Time passes for', Agent, '']).
logic2eng(_Context, attempts(Agent, Doing), [anglify(Agent, Agent), 'attempts to', anglify(Agent, Doing)]).
logic2eng(Context, act3('go__dir',Agent,[ How, Dir]), [ How, Dir]):- Context=Agent.
logic2eng(_Context,act3('go__dir',Agent,[ How, Dir]), [ Agent, How, Dir]).
logic2eng(_Context, Doing, [Agent, does, Did|More]):- is_type_functor(action, Doing), Doing=..[Did, Agent|More].

logic2eng(_Context, percept(_Agent, How, _, _), ''):- How == know, !.

logic2eng(_Context, percept(_Agent, see, Depth, props(Object, [shape=What])), []):-
  (Depth == 1;Depth == 2), atom(Object), atom_contains(Object, What), !.

logic2eng(_Context, percept(Agent, see, Depth, props(Object, [shape=What])), 
   extra_verbose_logic(percept(Agent, see, Depth, props(Object, [has_shape=What])))).

logic2eng(Context, percept(_Agent, _, _Depth, fn_list(Spatially, Exit, Relation, Here, Exits)), ['s'(Exit),'ly'(Spatially), Relation, Here, ' are:', ExitText, '\n']):-   list2eng(Context, Exits, ExitText).

logic2eng(_Context, percept(_Agent, Sense, Depth, h(_Spatial, Prep, '<mystery>'(Closed, _, _), Object)), 
   extra_verbose_eng([Object, aux(be), Closed, from, ing(Sense), cap(Prep)]) ):- Depth \= depth(3).

logic2eng(_Context, percept(_Agent, _Sense, Depth, h(_Spatial, Prep, [], Object)), 
   extra_verbose_eng([nothing, Prep, Object]) ):- Depth \= 1.

logic2eng(Context, percept( Agent, Sense, _Depth, h(_Spatial, Prep, Nearby, Here)),
    [cap(subj(Agent)), es(Sense), Prep, Here, ':'  | SeeText]):-
 select_from(Agent, Nearby, OthersNearby), !, list2eng(Context, OthersNearby, SeeText).

logic2eng(Context, percept( Agent, Sense, _Depth, h(_Spatial, Prep, Nearby, Here)),
 [cap(subj(Agent)), person(Sense, es(Sense)), Prep, Here, ':', SeeText]):-  list2eng(Context, Nearby, SeeText).

logic2eng(_Context, percept(Agent, see, Depth, props(Object, [shape=What])), extra_verbose_logic(percept(Agent, see, Depth, props(Object, [has_shape=What])))).

logic2eng(Context, percept(_Agent, _, _Depth, fn_list(Relation, Here, Exits)), ['Exits', Relation, Here, ' are:', ExitText, '\n']):-  list2eng(Context, Exits, ExitText).

logic2eng(_Context, percept(_Agent, Sense, Depth, child_list(Spatially, Object, Prep, '<mystery>'(Closed, _, _))), extra_verbose_eng([Object, aux(be), ly(Spatially), Closed, from, ing(Sense), cap(Prep)]) ):- Depth \= depth(3).
logic2eng(_Context, percept(_Agent, Sense, Depth, child_list(Spatially, Object, Prep, [])), extra_verbose_eng([nothing, ly(Sense), ly(Spatially), Prep, Object]) ):- Depth \= 1.
logic2eng(Context, percept( Agent, Sense, _Depth, child_list(Spatially, Here, Prep, Nearby)),
    [cap(subj(Agent)), es(Sense), ly(Spatially), Prep, Here, ':'  | SeeText]):-
 select_from(Agent, Nearby, OthersNearby), !, list2eng(Context, OthersNearby, SeeText).
logic2eng(Context, percept( Agent, Sense, _Depth, child_list(Spatially, Here, Prep, Nearby)),
 [cap(subj(Agent)), person(Sense, es(Sense)),  ly(Spatially), Prep, Here, ':', SeeText]):-  list2eng(Context, Nearby, SeeText).


logic2eng(Context, percept(Agent, How, Depth, Info), extra_verbose_logic(notices(Agent, How, Depth, What))):-  Depth=1,
  logic2eng(Context, Info, What).

logic2eng(Context, percept(Agent, How, _, Info), notices(Agent, How, What)):-
 \+ same_agent(Context, Agent), logic2eng(Agent, Info, What).

% (...verbose...: percept(_Player_1, see, 2, props(shovel_X1, [shape=shovel])) )
% {{ percept(_Player_1, see, 2, props(cabinate_X1, [shape=cabinate, opened=f, has rel(in, t), has rel(on, t)])) }}
% {{ percept(_Player_1, see, 3, props(kitchen, [volume capacity=10000, has rel(in, t), has rel(fn(exit, D2), t), desc='cooking happens here'])) }}
% {{ percept(_Player_1, see, 2, props(crate_X1, [shape=crate, opened=f, has rel(in, t)])) }}
% {{ percept(_Player_1, see, 3, props(living room, [volume capacity=10000, has rel(in, t), has rel(fn(exit, D2), t)])) }}
logic2eng(Context, percept(Agent, How, Depth, Info), extra_verbose_eng([Agent, es(How), What])):- (Depth == 2;Depth == 3),
  logic2eng(Context, Info, What).


logic2eng(Context, h(_Spatial, held_by, Agent, Items),
   [cap(subj(Agent)), 'held_by:'|Text]) :-
 list2eng(Context, Items, Text).


logic2eng(_Agent, act3('move', _Doer, [ _Verb, What, From, Prep, To]),
   [cap(subj(What)), 'moves', ' from', From, 'to' , Prep, To]).


logic2eng(_Agent, transformed(Before, After), [Before, 'turns into', After, .]).

logic2eng(_Agent, destroyed(Thing), [Thing, aux(be), 'destroyed.']).

logic2eng(_Context, unused_percept_props(_Agent, _Sense, _Object, _Depth, []), [] ) :- !.
logic2eng(Context, unused_percept_props(Agent, see, Object, _Depth, PropList), [cap(subj(Agent)), sees | English ] ) :-
 logic2eng(Context, do_props(Object, PropList), English).
logic2eng(Context, unused_percept_props(Agent, Sense, Object, _Depth, PropList),
   [cap(subj(Agent)),
    person(Sense, es(Sense))| English] ) :-
 logic2eng(Context, do_props(Object, PropList), English ).

logic2eng(Context, props(Object, PropList), [the(Object),aux( be) |English] ) :-
  logic2eng(Context, do_props(Object, PropList), English ).

logic2eng(_Agent, do_props(_Object, []), '<..>' ) :- !.
logic2eng(_Agent, do_props(Object, PropList), English ) :- list2eng(['.'='.'], Object, PropList, English).


logic2eng(_Agent, memories(Object, PropList), ['\n\n', the(Object), ' remembers:\n'|English] ) :-
 reverse(PropList, PropListR),
 list2eng([', '=', \n', logic2eng=percept2eng], Object, PropListR, English).
logic2eng(_Agent, perceptq(Object, PropList), ['\n\n', the(Object), ' notices:\n'|English] ) :-
 list2eng([', '=', \n'], Object, PropList, English).

logic2eng(_Context, event3('depart', [ In, Actor, Where], [How, Dir]), [ Actor, was, In, Where, but, then, ing(How), Dir] ) :- !.
logic2eng(_Context, event3('arrive', [ In, Actor, Where], [How, Dir]), [ Actor, came, ing(How), Dir, In, Where] ) :- !.

logic2eng(Context, did(Action
 ), ['did happen: '|English] ) :- !, logic2eng(Context, Action, English ).

logic2eng(Context, act3('emote',Speaker,[ EmoteType, Audience, Eng]), [ 'happened: '| Rest]) :- !,
 logic2eng(Context, act3('emote',Speaker,[ EmoteType, Audience, Eng]), Rest).

logic2eng(_, act3('emote',Speaker,[ act, '*'(Place), Eng]), [the(Speaker), at, Place, Text]) :- !,
 eng2txt(Speaker, Speaker, Eng, Text).
logic2eng(_, act3('emote',Speaker,[ act, Audience, Eng]), [Audience, notices, the(Speaker), Text]) :-
 eng2txt(Speaker, Speaker, Eng, Text).
logic2eng(_, act3('emote',Speaker,[ EmoteType, Audience, Eng]), [cap(subj(Speaker)), es(EmoteType), 'to', Audience, ', "', Text, '"']) :-
 eng2txt(Speaker, 'I', Eng, Text).

logic2eng(_Agent, failure(Action), ['Action failed:', Action]).

%logic2eng( Obj, effect(_, _), Out):- logic2eng(Obj, adjs(special), Out), !.
%logic2eng( Obj, effect(_, _), Out):- logic2eng(Obj, sp(adjs, special), Out), !.

logic2eng(Obj, timestamp(Ord, Time), [timestamp, is, Ord, '(', Ago, ')']):- logic2eng(Obj, ago(Time), Ago).

logic2eng(_Obj, ago(Time), [MinutesSecs, ago]):-
 clock_time(Now),
 Dif is round((Now - Time)*10)/10,
 Minutes is round(Dif) // 60,
 USecs is round((Dif - (Minutes*60))*10)/10,
 Secs is round(USecs),
 (Minutes>0 ->
 (Secs<10
  -> format(atom(MinutesSecs), '~w:0~ws', [Minutes, Secs])
  ; format(atom(MinutesSecs), '~w:~ws', [Minutes, Secs]))
  ; format(atom(MinutesSecs), '~ws', [USecs])).


logic2eng(_Obj, h(_Spatial, fn(exit, West), From , To), [To, 'is', West, 'of', From]):- !.
logic2eng(_Obj, h(_Spatial, ExitDown, Object, Speaker), [the(Object), 'has', Exit, Down, 'to', Speaker]):-
 compound(ExitDown),
 ExitDown=..[Exit, Down].
logic2eng(_Obj, h(_Spatial, Held_by , Object, Speaker), [the(Object), aux(be), Held_by, Speaker]).


logic2eng(_Obj, EmittingLight, [aux(be), 'glowing']):- EmittingLight == emitting(see, light), !.
logic2eng(_Obj, emitting(See, X), [aux(be), emitting, X, that, can, aux(be), tense(See, past)]):-!.
logic2eng(_Obj, breaks_into(_), ['looks breakable']).
logic2eng(_Obj, shiny, ['shiny!']).



logic2eng( Obj, initial(Desc), ['initially described as'| Out]):- logic2eng_now( Obj, Desc, Out).
%logic2eng(_Obj, co(_), ['/**/ ']):- pretty, !.
logic2eng( Obj, co(Desc), ['(Created as: ', Out, ')']):- list2eng( Obj, Desc, Out).


%logic2eng(_Obj, adjs(Type), ['adjs:', Type]).
%logic2eng(_Obj, nouns(Type), ['nouns:', Type]).
%logic2eng(_Obj, sp(nouns, Type), ['nouns:', Type]).

%logic2eng(_Obj, oper(OProp, [cap(N), aux(be), V]):- Prop =..[N, V].

logic2eng(_Obj, =(cleanliness, clean), []) :- pretty.
logic2eng(_Obj, =(cleanliness, clean), [clean]).
%logic2eng( Obj, =(shape, Value), extra_verbose_eng([Value, shaped])):- atom_contains(Obj, Value).
logic2eng( Obj, =(shape, Value), []):- atom_contains(Obj, Value).
logic2eng(_Obj, =(shape, Value), [Value, shaped]).
logic2eng(_Obj, =(volume_capacity, 1000), [is, cramped]).
logic2eng(_Obj, =(volume_capacity, 10000), [is, large]).
logic2eng(_Obj, =(volume, 50), [is, normal, size]).

logic2eng( Obj, Prop=F, English):- F==f, !, append_term(Prop, F, ReProp), logic2eng( Obj, ReProp, English).
logic2eng( Obj, Prop=T, English):- T==t, !, logic2eng( Obj, Prop, English).
logic2eng(_Obj, =(Opened, f), [currently, not, Opened]).
logic2eng(_Obj, =(Statused), [aux(be), Statused ]).
logic2eng(_Obj, =(Statused, f), [aux(be), 'not', Statused ]).
logic2eng(_Obj, =(Name, Value), [Name, 'of', Value]).

logic2eng(_Obj, PredF, [currently, not, N]):- PredF=..[N, f].

logic2eng( Obj, Prop, English):- Prop =..[N, V, T| VRange], T==t, Prop2 =..[N, V| VRange], logic2eng( Obj, Prop2, English).
logic2eng(_Obj, has_rel(in), ['thus, has an interior']).
logic2eng(_Obj, has_rel(on), ['has a surface']).
logic2eng(_Obj, has_rel(on), ['has a surface']).
logic2eng(_Obj, has_rel(worn_by), ['can be dressed up']).
logic2eng(_Obj, has_rel(held_by), ['can hold objects']).
logic2eng(_Obj, has_rel(fn(exit, _)), ['can have exits']).
%logic2eng(_Obj, can_be(eat, t), ['looks tasty ', '!']).
%logic2eng(_Obj, can_be(take), ['can be taken!']).
%logic2eng(_Obj, can_be(take, f), ['cannot be taken!']).
logic2eng(_Obj, can_be(Verb), ['Can', be, tense(Verb, past)]).
logic2eng(_Obj, can_be(Verb, t), ['Can', be, tense(Verb, past)]).
logic2eng(_Obj, can_be(Verb, f), ['Can\'t', be, tense(Verb, past)]).
logic2eng(_Obj, knows_verbs(Verb), ['Able to', Verb ]).
logic2eng(_Obj, knows_verbs(Verb, f), ['Unable to', Verb ]).

logic2eng( Obj, inherit(Type), ['is', Out]):- logic2eng(Obj, [Type], Out), !.
logic2eng( Obj, inherit(Type, f), ['wont be', Out]):- logic2eng(Obj, [Type], Out), !.
%logic2eng( Obj, isnt(Type, f), ['isnt '|Out]):- logic2eng(Obj, [Type], Out), !.
logic2eng( Obj, inherited(Type, f), ['isnt '|Out]):- logic2eng(Obj, [Type], Out), !.
logic2eng( Obj, inherited(Type), ['inherits', Out]):- logic2eng(Obj, [Type], Out), !.
logic2eng( Obj, msg(Msg), TxtL):- any2eng(Obj, Obj, Msg, Txt), listify(Txt, TxtL), !.
%logic2eng( Obj, msg(Msg), TxtL):- eng2txt(Obj, Obj, Msg, Txt), listify(Txt, TxtL), !.
logic2eng(_Obj, class_desc(ClassDesc), [hidden(class_desc(ClassDesc))]).
logic2eng(_Obj, $class, ['$class']).

logic2eng( Obj, effect(Action, true) , [the, action, anglify(Obj, Action), has, no, effect]).


logic2eng(_Obj, has_rel(Value, TF) , [TF, 'that it has, ', Value]).

logic2eng( Obj, oper(Act, Precond, PostCond), OUT) :-
 (xtreme_english->OUT = ['{{', if, 'action: ', ActE, ' test:', PrecondE, 'resulting: ', PostCondE, '}}'];
 OUT = []),
 must_maplist(logic2eng_now(Obj), [Act, Precond, PostCond], [ActE, PrecondE, PostCondE]).


% logic2eng( Obj, Prop, English):- Prop =..[N, Obj1, A| VRange], Obj1==Obj, Prop2 =..[N, A| VRange], logic2eng( Obj, Prop2, English).
logic2eng( Obj, Prop, English):- Prop =..[N, V, T| VRange], T==t, Prop2 =..[N, V| VRange], logic2eng( Obj, Prop2, English).
logic2eng(_Obj, act3('examine__D5',Who,[ Sense, Prep, Where, 3]), [cap(subj(Who)), es(Sense), Prep, Where]):-!.

logic2eng(_, V, [V]):- (string(V);(atom(V), atom_needs_quotes(V))), !.
logic2eng(_, V, [String]):- (string(V);(atom(V), atom_needs_quotes(V))), !, format(atom(String), ' "~w" ', [V]), !.



%logic2eng( Obj, Prop, [cap(N), aux(be), Value]):- Prop =..[N, V], list2eng(Obj, V, Value).

% logic2eng( Obj, Prop, [cap(N), of, O, aux(be), Value]):- Prop =..[N, O, V], list2eng(Obj, V, Value).
logic2eng( Obj, Prop, ['(', cap(N), ':', Value, ')']):- Prop =..[N, V], list2eng(Obj, V, Value).
%logic2eng(_Obj, Prop, [String]):- compound(Prop), !, String=''. % format(atom(String), ' \n {{ ~q. }}\n ', [Prop]), !.
%
% sub__examine(_Player_1, see, in, living_room, 3)



logic2eng(_Obj, Prop, [String]):- compound(Prop), \+ xtreme_english, !, format(atom(String), ' {{ ~q }} ', [Prop]), !.
logic2eng(_Obj, Prop, [String]):- format(atom(String), '~p', [Prop]), !.

logic2eng( Obj, Prop, [cap(N), Value, aux(be), English]):- Prop =..[N, V| Range],
   logic2eng(Obj, V, Value),
   must_maplist(logic2eng(Obj), Range, English).


atom_needs_quotes(V):-format(atom(VV), '~q', [V]), V\==VV.

append_if_new1(Text1, Text2, Text):- flatten([Text1], TextF1), flatten([Text2], TextF2), append([_|TextF1], _, TextF2), !, Text=Text2.

append_if_new(Text1, Text2, Text):- append_if_new1(Text1, Text2, Text), !.
append_if_new(Text2, Text1, Text):- append_if_new1(Text1, Text2, Text), !.
append_if_new(Text1, Text2, Text):- append(Text1, Text2, Text), !.

percept2eng(_Agent, [_Logical, English], English) :- !.
percept2eng(_Agent, [_Logical, English|More], [English|More]) :- !.
percept2eng(Context, [Logical|_], Eng) :- logic2eng_now(Context, Logical, Eng), !.
percept2eng(Context, LogicalEnglish, Eng) :- logic2eng_now(Context, LogicalEnglish, Eng).

percept2txt(Agent, LogicalEnglish, Text) :-
 percept2eng(Agent, LogicalEnglish, English),
 eng2txt(Agent, Agent, English, Text), !.


logic2eng_now(Obj, [Prop|Tail], Text) :- nonvar(Prop), !,
 must_mw1((logic2eng_now(Obj, Tail, UText2) ->
 flatten([UText2], Text2),
 must_mw1(logic2eng_now(Obj, Prop, UText1)) ->
 flatten([UText1], Text1),
 append_if_new(Text1, Text2, Text))), !.
logic2eng_now(A,B,C):- logic2eng(A,B,C),!.

:- thread_local(tl_loop:in_logic2english/1).
logic2english(_Doer, Logic, Text):- atomic(Logic), !, Text=Logic, !.
logic2english(_Doer, Logic, Text):- \+ \+ tl_loop:in_logic2english(Logic), !, term_to_atom(Logic, Text).
logic2english( Doer, Logic, Text):- locally(tl_loop:in_logic2english(Logic),
  ((logic2eng_now(Doer, Logic, Eng), must_mw1((eng2txt(Doer, Doer, Eng, Text)))))), !.

:- if( current_prolog_flag(xpce, true) ).
:- noguitracer.
:- endif.

any2eng(_Speaker, _I, Eng, EngLO):- is_english(Eng), !, EngLO=Eng.
any2eng(_Speaker, I, Eng, EngLO):- logic2eng_now(I, Eng, EngLO).

