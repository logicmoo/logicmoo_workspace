

% :- ensure_loaded(adv_lexical).

:- discontiguous(verb_frame1/4).


:- '$set_source_module'(mu).

/*
   Take the sentence:

      London 2pm 200 men mouths loudly protesting law not give police hell


   Add Blanks like:

   " _ London _ 2pm _ 200  _ men _ mouths _ loudly _ protesting _ law _ not _ give  _ police _ hell "


  Fill in blanks:

  " in London at 2pm using mouths by men very loudly orderTo protest about law did not acually give to police some hell"


  Lets make each one a frameroles:

  ?- permutation(["in London", "about 2pm", "acually", "using mouths", "by 200 men", "very loudly", "in_order_to protest", "about a new law",
                                                "did not give", "to police", "some hell"], Res).


  is the permutations output accepbale?


  If this is correct you can immagine a declarion as the "psuedo-preps"

   in _ about _ approx _ using _ by _ very _ orderto _ about  _ did _ acually _ to _ some _  of this GIVE frame



  dataformat would be...


  default_args_prep_order(give, in-place, about-time, approx-number, using-device,
               by-doer, very-adverb, orderto-reason, about-theme, did-truthvalue, acually-verb, to-doee, some-thing).

*/

some_hell_example:- forall(permutation([
           "in London", "about 2pm", "acually", "using mouths", "by 200 men", "very loudly",
           "in_order_to protest", "about a new law",
            "did not give", "some hell", "to police"], Res),

  once((into_text80(Res, WL), any_to_string(WL, S), dmsg(S)))).


/*
   at(Place-London), when(Time-2pm), because(Reason-protesting),
   by(Doer-men), with(MoreDoers, "and women"), own(Instr-"knife"), VrtuhValue, did(
    Action-), toward(Vector-up), to(Doee), of(
*/
% to sally give love
% to sally does _Player_1 give love
% did _Player_1 give love to sally?
% give sally love
% give love to sally
%    From    GIVE    To      That
% _Player_1   give    sally    love
/*

_Player_1   give    [to sally]    [That] love



 "from" SENDS "to" "that"
  joe  sends  sally  love

   A sends  B     C
   from     to  that

   A eats  B     C





O = [[ _Player_1], [did, give], [that, love], [to, sally]] ;
O = [[ _Player_1], [to, sally], [did, give], [that, love]] ;
O = [[_Player_1], [to, sally], [that, love], [did, give]] ;
O = [[_Player_1], [that, love], [did, give], [to, sally]] ;
O = [[_Player_1], [that, love], [ sally], [did, give]] ;
O = [[was, given], [from, _Player_1], [ sally], [ love]] ;
O = [[did, give], [from, _Player_1], [that, love], [to, sally]] ;
O = [[did, give], [to, sally], [from, _Player_1], [ love]] ;
O = [[did, give], [to, sally], [that, love], [from, _Player_1]] ;
O = [[did, give], [that, love], [from, _Player_1], [to, sally]] ;
O = [[did, give], [that, love], [to, sally], [from, _Player_1]] ;
O = [[to, sally], [from, _Player_1], [did, give], [that, love]] ;
O = [[to, sally], [from, _Player_1], [that, love], [did, give]] ;
O = [[to, sally], [did, give], [from, _Player_1], [ love]] ;
O = [[to, sally], [did, give], [that, love], [from, _Player_1]] ;
O = [[to, sally], [that, love], [from, _Player_1], [did, give]] ;
O = [[to, sally], [that, love], [did, give], [from, _Player_1]] ;
O = [[that, love], [from, _Player_1], [did, give], [to, sally]] ;
O = [[that, love], [from, _Player_1], [to, sally], [did, give]] ;
O = [[that, love], [did, give], [from, _Player_1], [to, sally]] ;
O = [[that, love], [did, give], [to, sally], [from, _Player_1]] ;
O = [[that, love], [to, sally], [from, _Player_1], [did, give]] ;
O = [[that, love], [to, sally], [did, give], [from, _Player_1]] ;



|[That love] _Player_1   gave    [to sally]
[That love] [to sally]  _Player_1   gave
[to sally]  [That love]  _Player_1   gave
 _Player_1   gave   [That love]    [to sally]

*/


eng2flogic(Sentence):-
  make,
  call_residue_vars((eng2flogic(Sentence, FrameOut), !,
  print_reply_colored(FrameOut)), Vs),
  remove_term_attr_type(Vs, ['varnames', 'vn']).


eng2flogic(Sentence, FrameOutR):-
  quietly(into_text80(Sentence, WL)), !,
  any_to_string(WL, S),
   wdmsg(' ?- ~p.', [eng2flogic(S)]),
   eng2logic_frame(_, WL, FrameOutR, _Mem), !.

%frame_sort(~(A), ~(B), C):- !, compare(A, B, C).
%frame_sort(~(A), (B), C):- !, compare(A, B, C).
frame_sort(~(A), ~(B), C):- !, frcmp(A, B, C).

:- discontiguous(eng2flogic_test/1).

eng2flogic_test([give, sally, love, joe]).
eng2flogic_test([give, sally, it]).
eng2flogic_test([give, to, sally]).
eng2flogic_test([give | English]):-
   permutation([[by, _Player_1], [to, sally], [love]], P), flatten(P, English).
eng2flogic_test(English):- fail,
  permutation([[by, _Player_1], gave, [to, sally], [love]], P), flatten(P, English).
eng2flogic_test([some, love, we, gave, to, sally]).

eng2flogic_test([to, sally, we, gave, some, love]).


baseKB:feature_test:-test_eng2flogic.

test_eng2flogic:- 
  forall(eng2flogic_test(English), eng2flogic(English)).




parseForMWType(Frame, X, _Type, ParseText, [TextArg], Right):-
   append([a, TextArg], Right, ParseText),
   push_frame(isa(X, TextArg), Frame).

parseForMWType(_Frame, _X, _Type, ParseText, [TextArg], Right):-
   append([TextArg], Right, ParseText), is_prep(TextArg), !, fail.

parseForMWType(Frame, X, Type, ParseText, TextArg, Right):-
   append([L|Eft], [Prep|Rest], ParseText), is_prep(Prep),
   parseForMWType(Frame, X, Type, [L|Eft], TextArg, RightL),
   append(RightL, [Prep|Rest], Right), !.
parseForMWType(Frame, X, _Type, ParseText, TextArg, Right):-
   dcg_if_defined(noun_phrase(_SO, X, true, LFOut), ParseText, Right),
   push_frame(LFOut, Frame),
   append(TextArg, Right, ParseText).


verb_tenses(Verb, VerbTensed, Tense):-
  List = [Verb, _Smooches, _Smoochered, _Smooching, _Smooched],
  %talkdb:talk_db(intransitive, give, gives, gived, giving, gived)
  quietly_talk_db([_|List]),
  nth0(Nth0, List, VerbTensed),
  Key = [now, active, past, nowing, past],
  nth0(Nth0, Key, Tense).
verb_tenses(Verb, Verb, base).


eng2logic_frame(Doer, SomeVerbText, FrameOutR, _Mem):-
    length(SomeVerbText, L), L > 2,
    %talkdb:talk_db(transitive, give, gives, gave, giving, given).
    verb_frame1(Action, Verb, FrameArgSInfo, UNormals),
    verb_tenses(Verb, VerbText, Tense),
    append(PreText, [VerbText|TextRS], SomeVerbText),
    %append([VerbText|TextRS], [by|PreText], Text),
    correct_normals(UNormals, Frame),
    must(member(done_by(Action, Doer), Frame)),
 must_det_l((
    fix_frame_args(FrameArgSInfo, FrameArgS),
    compute_frame_slots(FrameArgS, Slots),
    % all_different_bindings(VarsOf),
    %push_frame(isa(Action, 'tAction'), Frame),
    push_frame(cmd(Verb, Tense, Slots), Frame),
    push_frame(textString(Action, s(VerbText)), Frame),
    push_frame(occurs(Action, Tense), Frame),
    debug_var([Verb, 'Event'], Action),
    debug_var("Actor", Doer),
    shift_text_args_right(PreText, TextRS, TextR),
    % pprint([shift=[TextR], FrameArgS], always),
    once((parse_dataframe_right(FrameArgS, Action, Frame, TextR);pprint(failed([shift=[TextR], FrameArgS], always)))),
    %frame_defaults(FrameArgS, Frame),
    frame_to_asserts(Frame, FrameOut),
    predsort(frcmp, FrameOut, FrameOutS),
    reverse(FrameOutS, FrameOutR))).

shift_text_args_right( [], TextRS, TextR):- !, TextR= TextRS.
shift_text_args_right( [Prep|Text], TextRS, TextR):-
 is_prep_for_type(Prep, Type), parseForMWType(_Frame, _X, Type, Text, Left, Right),
 append(TextRS, [Prep|Left], TextRS), !,
 shift_text_args_right( Right, TextRS, TextR).
shift_text_args_right( Left, TextRS, TextR):-
 append(TextRS, [by|Left], TextR), !.

txt_to_obj(TextArg, _NewArgValue):- var(TextArg), !.
txt_to_obj(List, NewArgValue):-  is_list(List), last(List, TextArg), !, txt_to_obj(TextArg, NewArgValue).
txt_to_obj(TextArg, NewArgValue):- upcase_atom(TextArg, UTextArg), gensym(UTextArg, NewArgValue).

parse_dataframe_right(FrameArgS, Action, Frame, Text):-
   % pprint(parse_dataframe_right(Text , FrameArgS, VarsOf), always),
   nth0(_Nth, FrameArgS, FrameArg, NewFrameArgS),
   %nth0(Nth, VarsOf, NewArg, NewVarsOf),
   member(prep(Prep), FrameArg),
   once((append(Left, [PrepText| TextArgRight], Text),
     same_word(Prep, PrepText))),
   push_frame(prepOf(NewArg, Prep), Frame),
   FrameVars = [$prep=Prep, $action=Action],
   ignore((member(isa(Type), FrameArg), push_frame(isa(NewArg, Type), Frame))),
   show_failure(parseForMWType(Frame, NewArgValue, Type, TextArgRight, TextArg, Right)), !,
   ignore((member(var(NewArg), FrameArg), var(NewArg), ignore(=(NewArgValue, NewArg)))),
   ignore((member(frame(Info), FrameArg), subst_each(Info, FrameVars, FInfo), push_frame(FInfo, Frame))),
   push_frame(textString(NewArg, TextArg), Frame),

   ignore((member(pred(Prop), FrameArg), push_frame(t(Prop, Action, NewArg), Frame))),
   ignore((member(default(Default), FrameArg), debug_var(Default, NewArg))),
   append(Left, Right, NewText),
   parse_dataframe_right(NewFrameArgS, Action, Frame, NewText).

parse_dataframe_right(FrameArgS, Action, Frame, Text):-
  cont_parse_dataframe(FrameArgS , Text, Action, Frame).

cont_parse_dataframe([], [], _Action, _Frame):- !.
cont_parse_dataframe([], Text, _Action, Frame):- !, push_frame(zexistsLeftOverText(Text), Frame).
cont_parse_dataframe(FrameArgS, [], _Action, Frame):- !, push_frame(zexistsLeftOver(FrameArgS), Frame).

cont_parse_dataframe([FrameArg| FrameArgS], Text, Action, Frame):-
   ignore((member(isa(Type), FrameArg), push_frame(isa(NewArg, Type), Frame))),
   member(var(NewArg), FrameArg),

   (((\+ member(optional(true), FrameArg),
    show_failure(parseForMWType(Frame, NewArgValue, Type, Text, TextArg, Right))))
      -> push_frame(textString(NewArg, TextArg), Frame) ; Right = Text),

   ignore((member(pred(Prop), FrameArg), push_frame(t(Prop, Action, NewArg), Frame))),
   ignore((member(default(Default), FrameArg), debug_var(Default, NewArg))),
   ignore((member(var(NewArg), FrameArg), var(NewArg), ignore(=(NewArgValue, NewArg)))),
   ignore((member(prep(Prep), FrameArg), push_frame(prepOf(NewArg, Prep), Frame))),
  cont_parse_dataframe(FrameArgS, Right, Action, Frame).


:- op(700, fx, ('~')).

% _Player_1 give sally love
verb_frame1(Action, Give,
[ (+default(Give))+var(Action)+isa(actGiving)+prep(do),
   default(them)+var(Recipient)+prep(to)+isa(tAnimate)+pred(recipient),
   default(it)+var(Object)+prep(some)+isa(tObject)+pred(objectActedOn),
   default(someone)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
   +default(handOf(Doer))+var(Instr)+prep(using)+isa(tBodyPart)+pred(instrument),
[]],
  [done_by(Action, Doer),

   normally(
            isa(Instr, tBodyPart),
            cntrls(Doer, Instr),
            can_reach(Instr, Recipient)),
   pre(
            cntrls(Doer, Object),
           ~cntrls(Recipient, Object)),
   post(
           ~cntrls(Doer, Object),
            cntrls(Recipient, Object)),
   end_of_list]):-
  arg(_, v(give, kick, throw), Give).



% write name in book with pen
% etch name on tree [with] knife
% _Player_1 etches name onto the tree's bark with a knife
%
% surface
%    tree
%    on tree
%    in tree
%    at tree

%    at bark
%    at tree bark
%    on tree bark
%    in tree bark
%    at bark of tree

%    trunk of tree
% ======================
%    under trunk of tree
%    on trunk of tree
%    in trunk of tree
%    at trunk of tree
%    lower part of trunk of tree
%    in part of trunk of tree

/*

The experts attributed Raphael this picture.

I forwarded Winifred the letter.

Managers presented the_foreman a_gold_watch.

Ted Kicked John the ball

Monica hit Martina the ball.

The critics ascribe Shakespeare this play

She was given the job by the previous manager.

The previous manager gave her the job  from Joe in the office at 9pm for a joke


9pm, The previous manager gave her joe's  office job
 AT      BY                    TO   FROM   IN




       BY            GAVE   TO    THAT

*/


%    on tree = on tree trunk = on tree bark
%    at tree trunk = under tree bark
%    book
%    on book
%    in book
%    at book
%    at book page
%    on book page
%    in book page
%    at page of book
%    at page
%    at book cover
%    at cover of book
%
verb_frame1(Action, Etch,
  [   +default(Etch)+var(Action)+isa(actCarved)+prep(do),

      default(someone)+var(Someone)+prep(to)+isa(tAnimate)+pred(receiver),
      +default(object)+var(Object)+prep(into)+isa(tObject)+pred(objectActedOn)+frame(part_of(Surface, Object)),
      +default(surface)+var(Surface)+prep(on)+prep(in)+prep(under)+isa(tObject)+pred(surface)+
         frame(partOf(Surface, $prep, Object)),
      default(glyphs)+var(Glyphs)+prep(about)+isa(tGlyphic)+pred(deplicts),
      +default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
      +default(instrument(Etch))+var(Instr)+prep(using)+prep(with)+isa(tTool)+pred(instrument)],
  [done_by(Action, Doer),
      pre(isa(Instr, tKnife), cntrls(Doer, Instr), can_reach(Instr, Object)),
      part_of(Surface, Object), later(see(Someone, Object)),
      ~pre(exists(Glyphs)),
       pre(~part_of(Glyphs, Surface)),
       post(part_of(Glyphs, Surface))]):-
  arg(_, v(etch, carve, dig), Etch).


eng2flogic_test("Bertrand wrote Fred a letter").
eng2flogic_test("Bertrand wrote a letter to Fred").
eng2flogic_test(English):-
   permutation(["Bertrand wrote" , "a letter", "about Gottlob", "to Fred"], English).
eng2flogic_test(English):-  fail,
  permutation(["by Bertrand" , "a letter was written", "about Gottlob", "to Fred"], English).

verb_frame1(Action, Write,
  [   +default(Write)+var(Action)+isa(actWriting)+prep(do),
      prep(to)+default(someone)+var(Someone)+prep(for)+isa(tAnimate)+pred(receiver),
      prep(some)+default(book)+var(Object)+isa(tObject)+pred(transcribedTo),
      prep(by)+default(doer)+var(Doer)+isa(tAnimate)+prep(from)+pred(done_by),
      +default(topic)+var(Topic)+prep(about)+isa(tTopic)+pred(topic),
      +default(instrumentOf(Write))+var(Instr)+prep(using)+prep(with)+isa(tTool)+pred(instrument)],
  [
       pre(cntrls(Doer, Instr),
           knows(Doer, Topic),
           can_reach(Instr, Object),
           equals(instrumentOf(Write), Instr),
           ~part_of(Topic, Object)),
       done_by(Action, Doer),
       post(part_of(Topic, Object)),
       eventually(reads(Someone, Object))]):-
  arg(_, v(transcribe, write, pen), Write).


verb_frame1(Action, Put, % to-region, of-container
  [   +default(Put)+var(Action)+isa(actPlacing)+prep(do),
      default(it)+var(Object)+prep(some)+isa(tObject)+pred(objectActedOn),
      default(somewhere)+var(Place)+prep(to)+pred(toLocation),
      default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
      +default(hand)+var(Instr)+prep(using)+isa(tBodyPart)+pred(patient)],
 [done_by(Action, Doer),
  cntrls(Doer, Instr), can_reach(Instr, Place),
  part_of(Place, Container),
  or(h(spatial, How, Place, Container), h(spatial, How, Container, Place)),
  post(h(spatial, How, Container, Object))]):- arg(_, v(put, place), Put).


% %%%%%%%%%%%%%%
% Dig
% %%%%%%%%%%%%%%
/*
reframed_call( Pred, Agent, [dig, ShapeHole],  act3('dig',Agent,[ ShapeHole, Where, Instr]), M) :- fail,
 in_agent_model(Agent, inst(Agent), M),
 in_agent_model(Agent, h(spatial, _, Agent, Where), M),
 Instr=shovel.
*/

verb_frame1(Action, Dig,
 [ +default(dig)+var(Action)+isa(actDiging)+prep(do),
 some-shape_of,
 on-faceOf:surfaceOf(Object),
 in-objectActedOn:tGround,
  default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
 + default(shovel)+var(Instr)+prep(using)+isa(tTool)+pred(instrument),
 []],
 % [Doer, does, $verb, some, ShapeHole, on, Surface, into, Object, using, Instr],
[done_by(Action, Doer),
  normally(
           isa(Instr, tTool),
           cntrls(Doer, Instr),
           can_reach(Instr, Surface)),
 part_of(Surface, Object),
 ~pre(exists(ShapeHole)),
 pre(~part_of(ShapeHole, Object)),
 post(part_of(ShapeHole, Object))]):-
   arg(_, v(dig), Dig),
   debug_var(tool, Instr), debug_var(hole, ShapeHole).

/*
verb(bite,
 [tAnimate(Doer), done_by(Doer, Action),
  frame(Action), act_of(Action, biting),
  tAnimate(Object), object(Object, Action),
  type_of(Instr, teeth), using(Instr, Action),
  part_of(Instr, Doer) ] ).
*/
verb_frame1(Action, bite,
   [
	+default(bite)+var(Action)+isa(actBiting)+prep(do),
	default(it)+var(Object)+prep(some)+isa(tObject)+pred(victem),
        default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
        +default(teeth)+var(Instr)+prep(using)+isa(tTeeth)+pred(instrument),
        []],
  % [Doer, does, $verb, some, Object, using, Instr],
  [done_by(Action, Doer),
   part_of(Instr, Doer),
   can_reach(Instr, Object),
   normally(isa(Instr, tBodyPart))]).

verb_frame1(Action, like,
   [
	default(like)+var(Action)+isa(actLiking)+prep(do),
	default(it)+var(Object)+prep(some)+isa(tObject)+pred(objectActedOn),
	default(much)+var(LotsOrLittle)+prep(so)+pred(amount),
        default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
   []],
  % [Doer, does, $verb, some, Object, a, LotsOrLittle],
  [done_by(Action, Doer),
   feelsAbout(Doer, Object, LotsOrLittle)]).

verb_frame1(Action, want,
  [
   +default(like)+var(Action)+isa(actLiking)+prep(do),
   default(action)+var(AlsoDo)+isa(tAction)+pred(targetAction)+prep(to),
   default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
   []],
  % [Doer, does, $verb, want, to, AlsoDo],
  [done_by(Action, Doer),
   wantsTodo(Doer, Action, AlsoDo)]).


% %%%%%%%%%%%%%%
control_changed(break, broken).
control_changed(repair, unbroken).
control_changed(Smooch, Smooched):-
  munl_call(talkdb:talk_db(_, Smooch, _Smooches, Smooched, _Smooching, Smooched)).
control_changed(light, lit).
control_changed(unlight, unlit).
%control_changed(Open, Opened):- munl_call(clex:tv_pp(Opened, Open)).

% %%%%%%%%%%%%%%
verb_frame1(Action, Light,
  [
   default(like)+var(Action)+isa(actLiking)+prep(do),
   default(it)+var(Object)+prep(some)+isa(tObject)+pred(objectActedOn),
   default(hand)+var(Instr)+prep(using)+isa(tbpart)+pred(instrument),
   default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
   []],
   % [Doer, does, $verb, some, Object, using, Instr],
   [done_by(Action, Doer),
    pre(cntrls(Doer, Instr), can_reach(Instr, Object)),
  symetrically(opposite_values(Lit, Unlit)),
  pre(status(Object, Unlit)),
  pre(~status(Object, Lit)),
  post(~status(Object, Unlit)),
  post(status(Object, Lit)),
  end_of_list]):- control_changed(Light, Lit).




%reframed_call( Pred, Doer, [switch, Thing, OnOff], Result, M) :- preposition(_, OnOff), !, reframed_call( Pred, Doer, [switch, OnOff, Thing], Result, M).

verb_frame1(Action, switch,
 [
 default(switch)+var(Action)+isa(act(switch))+prep(do),
 default(it)+var(Object)+prep(some)+isa(tObject)+pred(objectActedOn),
 default(on)+var(On)+isa(act(state))+pre(tp),
 default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
 to-state:on_off,
 default(hand)+var(Instr)+prep(using)+isa(tbpart)+pred(instrument)],
   % [Doer, does, $verb, some, Object, to, On, using, Instr],
 [done_by(Action, Doer),
  pre(cntrls(Doer, Instr), can_reach(Instr, Object)),
  pre(position(Object, Off)),
  pre(~position(Object, On)),
  symetrically(opposite_values(On, Off)),
  post(~position(Object, Off)),
  post(position(Object, On)),
  end_of_list]):- On = on.


% %%%%%%%%%%%%%%
verb_undos(unlight, lit, bpart).
verb_undos(close, opened, bpart).
verb_undos(unlock, locked, key).
% %%%%%%%%%%%%%%
verb_frame1(Action, Unlock,
 [+default(Unlock)+var(Action)+isa(act(Unlock))+prep(do),
  default(it)+var(Object)+prep(some)+isa(tObject)+pred(objectActedOn),
  default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
  +default(Key)+var(Instr)+prep(using)+pred(instrument),
  []],
 % [Doer, does, $verb, some, Object, using, Instr],
 [done_by(Action, Doer),
  pre(cntrls(Doer, Instr), can_reach(Instr, Object)),
  pre(status(Object, Locked)),
  post(~status(Object, Locked))]):- verb_undos(Unlock, Locked, Key).

% %%%%%%%%%%%%%%
verb_cantbe_causes(open, locked, opened).
% %%%%%%%%%%%%%%
verb_frame1(Action, Open,
 [+default(Open)+var(Action)+isa(act(Open))+prep(do),
  default(it)+var(Object)+prep(some)+isa(tObject)+pred(objectActedOn),
  default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
  +default(hand)+var(Instr)+prep(using)+isa(tbpart)+pred(instrument),
  []],
 % [Doer, does, $verb, some, Object, using, Instr],
  [done_by(Action, Doer),
   pre(cntrls(Doer, Instr), can_reach(Instr, Object)),
   pre(~status(Object, Opened)),
   pre(~status(Object, Locked)),
    post(~status(Object, Locked)),
    post(status(Object, Opened)),
  end_of_list]):- verb_cantbe_causes(Open, Locked, Opened).

% %%%%%%%%%%%%%%
verb_undos_causes1(lock, opened, locked, key).
% %%%%%%%%%%%%%%
verb_frame1(Action, Lock,
 [+default(Lock)+var(Action)+isa(act(Lock))+prep(do),
 default(it)+var(Object)+prep(some)+isa(tObject)+pred(objectActedOn),
 default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
 +default(key)+var(Key)+prep(using)+isa(tKey)+pred(instrument)],
 % [Doer, does, $verb, some, Object, using, Instr],
 [done_by(Action, Doer),
  pre(cntrls(Doer, Instr), can_reach(Instr, Object)),
  pre(~status(Object, Locked)),
  post(~status(Object, StateOpened)),
  post(status(Object, Locked)),
  end_of_list]) :-
  verb_undos_causes1(Lock, StateOpened, Locked, Key).

% %%%%%%%%%%%%%%
verb_tool_ends_ensures(burn, match, unflaming, burned).
verb_tool_ends_ensures(extinguish, extinguiser, flaming, unburned).
% %%%%%%%%%%%%%%
verb_frame1(Action, Burn,
 [default(it)+var(Object)+prep(some)+isa(tObject)+pred(objectActedOn),
  default(doer)+var(Doer)+isa(tAnimate)+prep(from)+prep(by)+pred(done_by),
  +default(match)+var(Match)+prep(using)+isa(tMatch)+pred(instrument)],
 % [Doer, does, $verb, some, Object, with, Instr],
 [done_by(Action, Doer),
  pre(cntrls(Doer, Instr), can_reach(Instr, Object)),
  symetrically(opposite_values(Unflaming, Flaming)),
   pre(status(Object, Unflaming)),
   post(~status(Object, Unflaming)),
   post(status(Object, Flaming)),
  symetrically(opposite_values(Burnt, Unburnt)),
   post(~status(Object, Unburnt)),
   post(status(Object, Burnt)),
  end_of_list]):-
 verb_tool_ends_ensures(Burn, Match, Unflaming, Burnt).


end_of_file.



joe gave sally love
NP armed NP NP








% /mnt/gggg/c/Users/logicmoo/AppData/Local/swi-prolog/pack/logicmoo_nlu/prolog/episodic_memory/adv_eng2cmd compiled into mu 0.26 sec, -4 clauses
% ?-eng2flogic("give sally love joe").
[ cmd( give, now,
    [ do=Give,
      recipient=Sally16,
      objectActedOn=Love7,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Someone, "joe"),
  textString(Give, "give"),
  textString(Love7, "love"),
  textString(Sally16, "sally"),
  recipient(Give, Sally16),
  prepOf(Someone, from),
  prepOf(Give, do),
  prepOf(Love7, some),
  prepOf(Sally16, to),
  occurs(Give, now),
  objectActedOn(Give, Love7),
  iza(Someone, 'Joe'),
  iza(Love7, 'Love'),
  iza(Sally16, 'Sally'),
  isa(Someone, tAnimate),
  isa(Give, actGiving),
  isa(Love7, tObject),
  isa(Sally16, tAnimate),
  done_by(Give, Someone),
  zexistsLeftOver( [[slot(HandOf9), optional(true), default(handOf(Someone)), var(HandOf), prep(using), isa(tBodyPart), pred(instrument)]]),
  pre( cntrls(Someone, Love7)),
  pre( ~( cntrls(Sally16, Love7))),
  post( cntrls(Sally16, Love7)),
  post( ~( cntrls(Someone, Love7))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally16))  ]


% ?-eng2flogic("give sally it").
[ cmd( give, now,
    [ do=Give,
      recipient=Sally17,
      objectActedOn=It13,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(It13, "it"),
  textString(Give, "give"),
  textString(Sally17, "sally"),
  recipient(Give, Sally17),
  prepOf(It13, some),
  prepOf(Give, do),
  prepOf(Sally17, to),
  occurs(Give, now),
  objectActedOn(Give, It13),
  iza(Sally17, 'Sally'),
  isa(It13, tObject),
  isa(Give, actGiving),
  isa(Sally17, tAnimate),
  done_by(Give, Someone),
  &(
       denotableBy(It13, iPronounPersonalFn("it")),
       iza(It13, tInanimateObject),
      denotableBy(It13, agreementFn('3rd')),
     denotableBy(It13, iVarnamedFn("It"))),
  zexistsLeftOver( [ [ slot(Someone7),
      default(someone),
      var(Someone),
      isa(tAnimate),
      prep(from),
      prep(by),
      pred(done_by)  ],
     [ slot(HandOf9),
       optional(true),
       default( handOf(Someone)),
       var(HandOf),
       prep(using),
       isa(tBodyPart),
       pred(instrument)  ]  ]),
  pre( cntrls(Someone, It13)),
  pre( ~( cntrls(Sally17, It13))),
  post( cntrls(Sally17, It13)),
  post( ~( cntrls(Someone, It13))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally17))  ]


% ?-eng2flogic("give to sally").
[ cmd( give, now,
    [ do=Give,
      recipient=Sally18,
      objectActedOn=It,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Sally18, "sally"),
  textString(Give, "give"),
  recipient(Give, Sally18),
  prepOf(Sally18, to),
  occurs(Give, now),
  iza(Sally18, 'Sally'),
  isa(Sally18, tAnimate),
  done_by(Give, Someone),
  zexistsLeftOver( [ [ slot(Give1),
      optional(true),
      default(give),
      var(Give),
      isa(actGiving),
      prep(do)  ],
     [ slot(It5),
       default(it),
       var(It),
       prep(some),
       isa(tObject),
       pred(objectActedOn)  ],
     [ slot(Someone7),
       default(someone),
       var(Someone),
       isa(tAnimate),
       prep(from),
       prep(by),
       pred(done_by)  ],
     [ slot(HandOf9),
       optional(true),
       default( handOf(Someone)),
       var(HandOf),
       prep(using),
       isa(tBodyPart),
       pred(instrument)  ]  ]),
  pre( cntrls(Someone, It)),
  pre( ~( cntrls(Sally18, It))),
  post( cntrls(Sally18, It)),
  post( ~( cntrls(Someone, It))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally18))  ]


% ?-eng2flogic("give by _Player_1 to sally love").
[ cmd( give, now,
    [ do=Give,
      recipient=Sally19,
      objectActedOn=It,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Sally19, "sally"),
  textString(Give, "give"),
  recipient(Give, Sally19),
  prepOf(HandOf, using),
  prepOf(Sally19, to),
  prepOf(Someone, from),
  prepOf(Give, do),
  prepOf(It, some),
  occurs(Give, now),
  objectActedOn(Give, It),
  iza(Sally19, 'Sally'),
  isa(HandOf, tBodyPart),
  isa(Sally19, tAnimate),
  isa(Someone, tAnimate),
  isa(Give, actGiving),
  isa(It, tObject),
  instrument(Give, HandOf),
  done_by(Give, Someone),
  zexistsLeftOverText( [ by,
     _Player_1,
     love  ]),
  pre( cntrls(Someone, It)),
  pre( ~( cntrls(Sally19, It))),
  post( cntrls(Sally19, It)),
  post( ~( cntrls(Someone, It))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally19))  ]


% ?-eng2flogic("give by _Player_1 love to sally").
[ cmd( give, now,
    [ do=Give,
      recipient=Sally20,
      objectActedOn=It,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Sally20, "sally"),
  textString(Give, "give"),
  recipient(Give, Sally20),
  prepOf(HandOf, using),
  prepOf(Sally20, to),
  prepOf(Someone, from),
  prepOf(Give, do),
  prepOf(It, some),
  occurs(Give, now),
  objectActedOn(Give, It),
  iza(Sally20, 'Sally'),
  isa(HandOf, tBodyPart),
  isa(Sally20, tAnimate),
  isa(Someone, tAnimate),
  isa(Give, actGiving),
  isa(It, tObject),
  instrument(Give, HandOf),
  done_by(Give, Someone),
  zexistsLeftOverText( [ by,
     _Player_1,
     love  ]),
  pre( cntrls(Someone, It)),
  pre( ~( cntrls(Sally20, It))),
  post( cntrls(Sally20, It)),
  post( ~( cntrls(Someone, It))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally20))  ]


% ?-eng2flogic("give to sally by _Player_1 love").
[ cmd( give, now,
    [ do=Give,
      recipient=Sally21,
      objectActedOn=It,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Sally21, "sally"),
  textString(Give, "give"),
  recipient(Give, Sally21),
  prepOf(HandOf, using),
  prepOf(Sally21, to),
  prepOf(Someone, from),
  prepOf(Give, do),
  prepOf(It, some),
  occurs(Give, now),
  objectActedOn(Give, It),
  iza(Sally21, 'Sally'),
  isa(HandOf, tBodyPart),
  isa(Sally21, tAnimate),
  isa(Someone, tAnimate),
  isa(Give, actGiving),
  isa(It, tObject),
  instrument(Give, HandOf),
  done_by(Give, Someone),
  zexistsLeftOverText( [ by,
     _Player_1,
     love  ]),
  pre( cntrls(Someone, It)),
  pre( ~( cntrls(Sally21, It))),
  post( cntrls(Sally21, It)),
  post( ~( cntrls(Someone, It))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally21))  ]


% ?-eng2flogic("give to sally love by _Player_1").
[ cmd( give, now,
    [ do=Give,
      recipient=Sally22,
      objectActedOn=Love8,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Sally22, "sally"),
  textString(Give, "give"),
  textString(Love8, "love"),
  recipient(Give, Sally22),
  prepOf(HandOf, using),
  prepOf(Sally22, to),
  prepOf(Someone, from),
  prepOf(Give, do),
  prepOf(Love8, some),
  occurs(Give, now),
  objectActedOn(Give, Love8),
  iza(Sally22, 'Sally'),
  iza(Love8, 'Love'),
  isa(HandOf, tBodyPart),
  isa(Sally22, tAnimate),
  isa(Someone, tAnimate),
  isa(Give, actGiving),
  isa(Love8, tObject),
  instrument(Give, HandOf),
  done_by(Give, Someone),
  zexistsLeftOverText( [ by,
     _Player_1  ]),
  pre( cntrls(Someone, Love8)),
  pre( ~( cntrls(Sally22, Love8))),
  post( cntrls(Sally22, Love8)),
  post( ~( cntrls(Someone, Love8))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally22))  ]


% ?-eng2flogic("give love by _Player_1 to sally").
[ cmd( give, now,
    [ do=Give,
      recipient=Sally23,
      objectActedOn=Love9,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Sally23, "sally"),
  textString(Give, "give"),
  textString(Love9, "love"),
  recipient(Give, Sally23),
  prepOf(HandOf, using),
  prepOf(Sally23, to),
  prepOf(Someone, from),
  prepOf(Give, do),
  prepOf(Love9, some),
  occurs(Give, now),
  objectActedOn(Give, Love9),
  iza(Sally23, 'Sally'),
  iza(Love9, 'Love'),
  isa(HandOf, tBodyPart),
  isa(Sally23, tAnimate),
  isa(Someone, tAnimate),
  isa(Give, actGiving),
  isa(Love9, tObject),
  instrument(Give, HandOf),
  done_by(Give, Someone),
  zexistsLeftOverText( [ by,
     _Player_1  ]),
  pre( cntrls(Someone, Love9)),
  pre( ~( cntrls(Sally23, Love9))),
  post( cntrls(Sally23, Love9)),
  post( ~( cntrls(Someone, Love9))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally23))  ]


% ?-eng2flogic("give love to sally by _Player_1").
[ cmd( give, now,
    [ do=Give,
      recipient=Sally24,
      objectActedOn=Love10,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Sally24, "sally"),
  textString(Give, "give"),
  textString(Love10, "love"),
  recipient(Give, Sally24),
  prepOf(HandOf, using),
  prepOf(Sally24, to),
  prepOf(Someone, from),
  prepOf(Give, do),
  prepOf(Love10, some),
  occurs(Give, now),
  objectActedOn(Give, Love10),
  iza(Sally24, 'Sally'),
  iza(Love10, 'Love'),
  isa(HandOf, tBodyPart),
  isa(Sally24, tAnimate),
  isa(Someone, tAnimate),
  isa(Give, actGiving),
  isa(Love10, tObject),
  instrument(Give, HandOf),
  done_by(Give, Someone),
  zexistsLeftOverText( [ by,
     _Player_1  ]),
  pre( cntrls(Someone, Love10)),
  pre( ~( cntrls(Sally24, Love10))),
  post( cntrls(Sally24, Love10)),
  post( ~( cntrls(Someone, Love10))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally24))  ]


% ?-eng2flogic("some love we gave to sally").
[ cmd( give, past,
    [ do=Give,
      recipient=Sally25,
      objectActedOn=Love11,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Love11, "love"),
  textString(Sally25, "sally"),
  textString(Someone, "we"),
  textString(Give, "gave"),
  recipient(Give, Sally25),
  prepOf(Love11, some),
  prepOf(Sally25, to),
  prepOf(Someone, by),
  occurs(Give, past),
  objectActedOn(Give, Love11),
  iza(Love11, 'Love'),
  iza(Sally25, 'Sally'),
  isa(Love11, tObject),
  isa(Sally25, tAnimate),
  isa(Someone, tAnimate),
  done_by(Give, Someone),
  &(
       denotableBy(Someone, iPronounPersonalFn("we")),
       denotableBy(Someone, agreementFn('1st')),
      denotableBy(Someone, iVarnamedFn("Us")),
     ~( numberOf(Someone, 1))),
  zexistsLeftOver( [ [ slot(Give1),
      optional(true),
      default(give),
      var(Give),
      isa(actGiving),
      prep(do)  ],
     [ slot(HandOf9),
       optional(true),
       default( handOf(Someone)),
       var(HandOf),
       prep(using),
       isa(tBodyPart),
       pred(instrument)  ]  ]),
  pre( cntrls(Someone, Love11)),
  pre( ~( cntrls(Sally25, Love11))),
  post( cntrls(Sally25, Love11)),
  post( ~( cntrls(Someone, Love11))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally25))  ]


% ?-eng2flogic("to sally we gave some love").
[ cmd( give, past,
    [ do=Give,
      recipient=Sally30,
      objectActedOn=Love12,
      done_by=Someone,
      instrument=HandOf  ]),
  textString(Love12, "love"),
  textString(Sally30, "sally"),
  textString(Someone, "we"),
  textString(Give, "gave"),
  recipient(Give, Sally30),
  prepOf(Love12, some),
  prepOf(Sally30, to),
  prepOf(Someone, by),
  occurs(Give, past),
  objectActedOn(Give, Love12),
  iza(Love12, 'Love'),
  iza(Sally30, 'Sally'),
  isa(Love12, tObject),
  isa(Sally30, tAnimate),
  isa(Someone, tAnimate),
  done_by(Give, Someone),
  &(
       denotableBy(Someone, iPronounPersonalFn("we")),
       denotableBy(Someone, agreementFn('1st')),
      denotableBy(Someone, iVarnamedFn("Us")),
     ~( numberOf(Someone, 1))),
  zexistsLeftOver( [ [ slot(Give1),
      optional(true),
      default(give),
      var(Give),
      isa(actGiving),
      prep(do)  ],
     [ slot(HandOf9),
       optional(true),
       default( handOf(Someone)),
       var(HandOf),
       prep(using),
       isa(tBodyPart),
       pred(instrument)  ]  ]),
  pre( cntrls(Someone, Love12)),
  pre( ~( cntrls(Sally30, Love12))),
  post( cntrls(Sally30, Love12)),
  post( ~( cntrls(Someone, Love12))),
  normally( isa(HandOf, tBodyPart)),
  normally( cntrls(Someone, HandOf)),
  normally( can_reach(HandOf, Sally30))  ]


% ?-eng2flogic("Bertrand wrote Fred a letter").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred51,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=Topic,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, Topic),
  textString(iBertrand, "bertrand"),
  textString(Write, "wrote"),
  textString(Book, ""),
  textString(Fred51, "fred"),
  receiver(Write, Fred51),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Write, do),
  prepOf(Topic, about),
  prepOf(Book, some),
  prepOf(Fred51, to),
  occurs(Write, past),
  iza(Fred51, 'Fred'),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Write, actWriting),
  isa(Topic, tTopic),
  isa(Book, tObject),
  isa(Fred51, tAnimate),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, Topic)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(Topic, Book))),
  post( part_of(Topic, Book)),
  eventually( reads(Fred51, Book)),
  true  ]


% ?-eng2flogic("Bertrand wrote a letter to Fred").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred52,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=Topic,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, Topic),
  textString(iBertrand, "bertrand"),
  textString(Fred52, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred52),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred52, to),
  prepOf(Write, do),
  prepOf(Topic, about),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred52, 'Fred'),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred52, tAnimate),
  isa(Write, actWriting),
  isa(Topic, tTopic),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, Topic)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(Topic, Book))),
  post( part_of(Topic, Book)),
  eventually( reads(Fred52, Book)),
  true  ]


% ?-eng2flogic("bertrand wrote a letter about Gottlob to Fred").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred53,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred53, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred53),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred53, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred53, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred53, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred53, Book)),
  true  ]


% ?-eng2flogic("bertrand wrote a letter to Fred about Gottlob").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred54,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred54, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred54),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred54, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred54, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred54, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred54, Book)),
  true  ]


% ?-eng2flogic("bertrand wrote about Gottlob a letter to Fred").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred55,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred55, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred55),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred55, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred55, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred55, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred55, Book)),
  true  ]


% ?-eng2flogic("bertrand wrote about Gottlob to Fred a letter").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred56,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred56, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred56),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred56, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred56, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred56, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred56, Book)),
  true  ]


% ?-eng2flogic("bertrand wrote to Fred a letter about Gottlob").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred57,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred57, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred57),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred57, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred57, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred57, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred57, Book)),
  true  ]


% ?-eng2flogic("bertrand wrote to Fred about Gottlob a letter").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred58,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred58, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred58),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred58, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred58, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred58, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred58, Book)),
  true  ]


% ?-eng2flogic("a letter Bertrand wrote about Gottlob to Fred").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred59,
      transcribedTo=Letter22,
      done_by=Doer,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter22),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(Fred59, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter22, "letter"),
  receiver(Write, Fred59),
  prepOf(iGottlob, about),
  prepOf(InstrumentOf, using),
  prepOf(Fred59, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter22, some),
  occurs(Write, past),
  iza(Fred59, 'Fred'),
  iza(Letter22, 'Letter'),
  isa(iGottlob, tTopic),
  isa(InstrumentOf, tTool),
  isa(Fred59, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter22, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, Doer),
  zexistsLeftOverText( ['Bertrand']),
  pre( knows(Doer, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter22)),
  pre( ~( part_of(iGottlob, Letter22))),
  post( part_of(iGottlob, Letter22)),
  eventually( reads(Fred59, Letter22)),
  true  ]


% ?-eng2flogic("a letter Bertrand wrote to Fred about Gottlob").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred60,
      transcribedTo=Letter26,
      done_by=Doer,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter26),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(Fred60, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter26, "letter"),
  receiver(Write, Fred60),
  prepOf(iGottlob, about),
  prepOf(InstrumentOf, using),
  prepOf(Fred60, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter26, some),
  occurs(Write, past),
  iza(Fred60, 'Fred'),
  iza(Letter26, 'Letter'),
  isa(iGottlob, tTopic),
  isa(InstrumentOf, tTool),
  isa(Fred60, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter26, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, Doer),
  zexistsLeftOverText( ['Bertrand']),
  pre( knows(Doer, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter26)),
  pre( ~( part_of(iGottlob, Letter26))),
  post( part_of(iGottlob, Letter26)),
  eventually( reads(Fred60, Letter26)),
  true  ]


% ?-eng2flogic("a letter about Gottlob Bertrand wrote to Fred").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred61,
      transcribedTo=Letter30,
      done_by=Doer,
      topic=gottlob_bertrand,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter30),
  topic(Write, gottlob_bertrand),
  textString(gottlob_bertrand, "gottlob bertrand"),
  textString(Fred61, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter30, "letter"),
  receiver(Write, Fred61),
  prepOf(gottlob_bertrand, about),
  prepOf(Fred61, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter30, some),
  occurs(Write, past),
  iza(Fred61, 'Fred'),
  iza(Letter30, 'Letter'),
  isa(gottlob_bertrand, tTopic),
  isa(Fred61, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter30, tObject),
  done_by(Write, Doer),
  zexistsLeftOver( [[slot(InstrumentOf11), optional(true), default(instrumentOf(write)), var(InstrumentOf), prep(using), prep(with), isa(tTool), pred(instrument)]]),
  pre( knows(Doer, gottlob_bertrand)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter30)),
  pre( ~( part_of(gottlob_bertrand, Letter30))),
  post( part_of(gottlob_bertrand, Letter30)),
  eventually( reads(Fred61, Letter30)),
  true  ]


% ?-eng2flogic("a letter about Gottlob to Fred Bertrand wrote").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred62,
      transcribedTo=Letter34,
      done_by=Doer,
      topic=gottlob_bertrand,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter34),
  topic(Write, gottlob_bertrand),
  textString(gottlob_bertrand, "gottlob bertrand"),
  textString(Fred62, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter34, "letter"),
  receiver(Write, Fred62),
  prepOf(gottlob_bertrand, about),
  prepOf(Fred62, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter34, some),
  occurs(Write, past),
  iza(Fred62, 'Fred'),
  iza(Letter34, 'Letter'),
  isa(gottlob_bertrand, tTopic),
  isa(Fred62, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter34, tObject),
  done_by(Write, Doer),
  zexistsLeftOver( [[slot(InstrumentOf11), optional(true), default(instrumentOf(write)), var(InstrumentOf), prep(using), prep(with), isa(tTool), pred(instrument)]]),
  pre( knows(Doer, gottlob_bertrand)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter34)),
  pre( ~( part_of(gottlob_bertrand, Letter34))),
  post( part_of(gottlob_bertrand, Letter34)),
  eventually( reads(Fred62, Letter34)),
  true  ]


% ?-eng2flogic("a letter to Fred Bertrand wrote about Gottlob").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred63,
      transcribedTo=Letter38,
      done_by=Doer,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter38),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(Fred63, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter38, "letter"),
  receiver(Write, Fred63),
  prepOf(iGottlob, about),
  prepOf(InstrumentOf, using),
  prepOf(Fred63, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter38, some),
  occurs(Write, past),
  iza(Fred63, 'Fred'),
  iza(Letter38, 'Letter'),
  isa(iGottlob, tTopic),
  isa(InstrumentOf, tTool),
  isa(Fred63, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter38, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, Doer),
  zexistsLeftOverText( ['Bertrand']),
  pre( knows(Doer, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter38)),
  pre( ~( part_of(iGottlob, Letter38))),
  post( part_of(iGottlob, Letter38)),
  eventually( reads(Fred63, Letter38)),
  true  ]


% ?-eng2flogic("a letter to Fred about Gottlob Bertrand wrote").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred64,
      transcribedTo=Letter42,
      done_by=Doer,
      topic=gottlob_bertrand,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter42),
  topic(Write, gottlob_bertrand),
  textString(gottlob_bertrand, "gottlob bertrand"),
  textString(Fred64, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter42, "letter"),
  receiver(Write, Fred64),
  prepOf(gottlob_bertrand, about),
  prepOf(Fred64, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter42, some),
  occurs(Write, past),
  iza(Fred64, 'Fred'),
  iza(Letter42, 'Letter'),
  isa(gottlob_bertrand, tTopic),
  isa(Fred64, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter42, tObject),
  done_by(Write, Doer),
  zexistsLeftOver( [[slot(InstrumentOf11), optional(true), default(instrumentOf(write)), var(InstrumentOf), prep(using), prep(with), isa(tTool), pred(instrument)]]),
  pre( knows(Doer, gottlob_bertrand)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter42)),
  pre( ~( part_of(gottlob_bertrand, Letter42))),
  post( part_of(gottlob_bertrand, Letter42)),
  eventually( reads(Fred64, Letter42)),
  true  ]


% ?-eng2flogic("about gottlob Bertrand wrote a letter to Fred").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred65,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred65, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred65),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred65, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred65, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred65, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred65, Book)),
  true  ]


% ?-eng2flogic("about gottlob Bertrand wrote to Fred a letter").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred66,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred66, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred66),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred66, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred66, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred66, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred66, Book)),
  true  ]


% ?-eng2flogic("about gottlob a letter Bertrand wrote to Fred").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred67,
      transcribedTo=Letter43,
      done_by=Doer,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter43),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(Fred67, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter43, "letter"),
  receiver(Write, Fred67),
  prepOf(iGottlob, about),
  prepOf(InstrumentOf, using),
  prepOf(Fred67, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter43, some),
  occurs(Write, past),
  iza(Fred67, 'Fred'),
  iza(Letter43, 'Letter'),
  isa(iGottlob, tTopic),
  isa(InstrumentOf, tTool),
  isa(Fred67, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter43, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, Doer),
  zexistsLeftOverText( ['Bertrand']),
  pre( knows(Doer, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter43)),
  pre( ~( part_of(iGottlob, Letter43))),
  post( part_of(iGottlob, Letter43)),
  eventually( reads(Fred67, Letter43)),
  true  ]


% ?-eng2flogic("about gottlob a letter to Fred Bertrand wrote").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred68,
      transcribedTo=Letter44,
      done_by=Doer,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter44),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(Fred68, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter44, "letter"),
  receiver(Write, Fred68),
  prepOf(iGottlob, about),
  prepOf(InstrumentOf, using),
  prepOf(Fred68, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter44, some),
  occurs(Write, past),
  iza(Fred68, 'Fred'),
  iza(Letter44, 'Letter'),
  isa(iGottlob, tTopic),
  isa(InstrumentOf, tTool),
  isa(Fred68, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter44, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, Doer),
  zexistsLeftOverText( ['Bertrand']),
  pre( knows(Doer, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter44)),
  pre( ~( part_of(iGottlob, Letter44))),
  post( part_of(iGottlob, Letter44)),
  eventually( reads(Fred68, Letter44)),
  true  ]


% ?-eng2flogic("about gottlob to Fred Bertrand wrote a letter").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred69,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred69, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred69),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred69, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred69, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred69, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred69, Book)),
  true  ]


% ?-eng2flogic("about gottlob to Fred a letter Bertrand wrote").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred70,
      transcribedTo=Letter45,
      done_by=Doer,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter45),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(Fred70, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter45, "letter"),
  receiver(Write, Fred70),
  prepOf(iGottlob, about),
  prepOf(InstrumentOf, using),
  prepOf(Fred70, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter45, some),
  occurs(Write, past),
  iza(Fred70, 'Fred'),
  iza(Letter45, 'Letter'),
  isa(iGottlob, tTopic),
  isa(InstrumentOf, tTool),
  isa(Fred70, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter45, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, Doer),
  zexistsLeftOverText( ['Bertrand']),
  pre( knows(Doer, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter45)),
  pre( ~( part_of(iGottlob, Letter45))),
  post( part_of(iGottlob, Letter45)),
  eventually( reads(Fred70, Letter45)),
  true  ]


% ?-eng2flogic("to fred Bertrand wrote a letter about Gottlob").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred75,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred75, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred75),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred75, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred75, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred75, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred75, Book)),
  true  ]


% ?-eng2flogic("to fred Bertrand wrote about Gottlob a letter").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred80,
      transcribedTo=Book,
      done_by=iBertrand,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(iBertrand, "bertrand"),
  textString(Fred80, "fred"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred80),
  prepOf(iGottlob, about),
  prepOf(iBertrand, by),
  prepOf(InstrumentOf, using),
  prepOf(Fred80, to),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred80, 'Fred'),
  isa(iGottlob, tTopic),
  isa(iBertrand, tAnimate),
  isa(InstrumentOf, tTool),
  isa(Fred80, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, iBertrand),
  zexistsLeftOverText( [letter]),
  pre( knows(iBertrand, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(iBertrand, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(iGottlob, Book))),
  post( part_of(iGottlob, Book)),
  eventually( reads(Fred80, Book)),
  true  ]


% ?-eng2flogic("to fred a letter Bertrand wrote about Gottlob").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred85,
      transcribedTo=Letter46,
      done_by=Doer,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter46),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(Fred85, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter46, "letter"),
  receiver(Write, Fred85),
  prepOf(iGottlob, about),
  prepOf(InstrumentOf, using),
  prepOf(Fred85, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter46, some),
  occurs(Write, past),
  iza(Fred85, 'Fred'),
  iza(Letter46, 'Letter'),
  isa(iGottlob, tTopic),
  isa(InstrumentOf, tTool),
  isa(Fred85, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter46, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, Doer),
  zexistsLeftOverText( ['Bertrand']),
  pre( knows(Doer, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter46)),
  pre( ~( part_of(iGottlob, Letter46))),
  post( part_of(iGottlob, Letter46)),
  eventually( reads(Fred85, Letter46)),
  true  ]


% ?-eng2flogic("to fred a letter about Gottlob Bertrand wrote").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred90,
      transcribedTo=Letter47,
      done_by=Doer,
      topic=gottlob_bertrand,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter47),
  topic(Write, gottlob_bertrand),
  textString(gottlob_bertrand, "gottlob bertrand"),
  textString(Fred90, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter47, "letter"),
  receiver(Write, Fred90),
  prepOf(gottlob_bertrand, about),
  prepOf(Fred90, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter47, some),
  occurs(Write, past),
  iza(Fred90, 'Fred'),
  iza(Letter47, 'Letter'),
  isa(gottlob_bertrand, tTopic),
  isa(Fred90, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter47, tObject),
  done_by(Write, Doer),
  zexistsLeftOver( [[slot(InstrumentOf11), optional(true), default(instrumentOf(write)), var(InstrumentOf), prep(using), prep(with), isa(tTool), pred(instrument)]]),
  pre( knows(Doer, gottlob_bertrand)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter47)),
  pre( ~( part_of(gottlob_bertrand, Letter47))),
  post( part_of(gottlob_bertrand, Letter47)),
  eventually( reads(Fred90, Letter47)),
  true  ]


% ?-eng2flogic("to fred about Gottlob Bertrand wrote a letter").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred95,
      transcribedTo=Book,
      done_by=Doer,
      topic=gottlob_bertrand,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Book),
  topic(Write, gottlob_bertrand),
  textString(gottlob_bertrand, "gottlob bertrand"),
  textString(Fred95, "fred"),
  textString(Doer, "letter"),
  textString(Write, "wrote"),
  textString(Book, ""),
  receiver(Write, Fred95),
  prepOf(gottlob_bertrand, about),
  prepOf(InstrumentOf, using),
  prepOf(Fred95, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Book, some),
  occurs(Write, past),
  iza(Fred95, 'Fred'),
  iza(Doer, 'Letter'),
  isa(gottlob_bertrand, tTopic),
  isa(InstrumentOf, tTool),
  isa(Fred95, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Book, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, Doer),
  zexistsLeftOverText( [by]),
  pre( knows(Doer, gottlob_bertrand)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Book)),
  pre( ~( part_of(gottlob_bertrand, Book))),
  post( part_of(gottlob_bertrand, Book)),
  eventually( reads(Fred95, Book)),
  true  ]


% ?-eng2flogic("to fred about Gottlob a letter Bertrand wrote").
[ cmd( write, past,
    [ do=Write,
      receiver=Fred100,
      transcribedTo=Letter49,
      done_by=Doer,
      topic=iGottlob,
      instrument=InstrumentOf  ]),
  transcribedTo(Write, Letter49),
  topic(Write, iGottlob),
  textString(iGottlob, "gottlob"),
  textString(Fred100, "fred"),
  textString(Doer, ""),
  textString(Write, "wrote"),
  textString(Letter49, "letter"),
  receiver(Write, Fred100),
  prepOf(iGottlob, about),
  prepOf(InstrumentOf, using),
  prepOf(Fred100, to),
  prepOf(Doer, by),
  prepOf(Write, do),
  prepOf(Letter49, some),
  occurs(Write, past),
  iza(Fred100, 'Fred'),
  iza(Letter49, 'Letter'),
  isa(iGottlob, tTopic),
  isa(InstrumentOf, tTool),
  isa(Fred100, tAnimate),
  isa(Doer, tAnimate),
  isa(Write, actWriting),
  isa(Letter49, tObject),
  instrument(Write, InstrumentOf),
  done_by(Write, Doer),
  zexistsLeftOverText( ['Bertrand']),
  pre( knows(Doer, iGottlob)),
  pre( equals(
      instrumentOf(write),
      InstrumentOf)),
  pre( cntrls(Doer, InstrumentOf)),
  pre( can_reach(InstrumentOf, Letter49)),
  pre( ~( part_of(iGottlob, Letter49))),
  post( part_of(iGottlob, Letter49)),
  eventually( reads(Fred100, Letter49)),
  true  ]


true.

mu:  ?-


?- xlisting(xDitransitiveNPNPFrame).
% From database (decompiled)
acnl(cycSubjectClumps, xDitransitiveNPNPFrame, vSubcategorizationFramesLexiconCSC, -335133).
acnl(inTopic, xDitransitiveNPNPFrame, iUI_SubcategorizationFramesLexiconTopic, -1668759).
acnl(isa, xDitransitiveNPNPFrame, iUI_SubcategorizationFramesLexiconTopic, -1660468).
acnl(isa, xDitransitiveNPNPFrame, ttDitransitiveNPGenericframetype, -921694).
acnl(isa, xDitransitiveNPNPFrame, xtPassivizableFrame, -923121).
acnl(oldConstantName, xDitransitiveNPNPFrame, "DitransitiveNPCompFrame", -21328).
acnl(oldConstantName, xDitransitiveNPNPFrame, "DoubleObjectFrame", -20375).
acnl(subcatFramesForAlternation, xtDoubleObjectAlternation, xDitransitiveNPNPFrame, nartR(xPPCompFrameFn, ttDitransitivePPFrameType, xToTheWord), -2227458).
acnl(verbClassSemTrans, xGetVerbClass, xDitransitiveNPNPFrame, and(isa(':ACTION', actGainingUserRights), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -364219).
acnl(verbClassSemTrans, xGiveVerbClass, xDitransitiveNPNPFrame, and(isa(':ACTION', eventTransferringPossession), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -364218).

acnl(verbSemTrans, nartR(xWordFn, "airmail"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Airmailing-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358900).
acnl(verbSemTrans, nartR(xWordFn, "airmail"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Airmailing-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359118).
acnl(verbSemTrans, nartR(xWordFn, "bash"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Bashing-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359165).
acnl(verbSemTrans, nartR(xWordFn, "bash"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Bashing-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358955).
acnl(verbSemTrans, nartR(xWordFn, "conjure"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Conjuring-CreationEvent'), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358831).
acnl(verbSemTrans, nartR(xWordFn, "conjure"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Conjuring-CreationEvent'), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), outputsCreated(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359048).
acnl(verbSemTrans, nartR(xWordFn, "FedEx"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'FedExing-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358902).
acnl(verbSemTrans, nartR(xWordFn, "FedEx"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'FedExing-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359120).
acnl(verbSemTrans, nartR(xWordFn, "fling"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Flinging-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359168).
acnl(verbSemTrans, nartR(xWordFn, "fling"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Flinging-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358958).
acnl(verbSemTrans, nartR(xWordFn, "hurl"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Hurling-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359169).
acnl(verbSemTrans, nartR(xWordFn, "hurl"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Hurling-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358959).
acnl(verbSemTrans, nartR(xWordFn, "lob"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Lobbing-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359171).
acnl(verbSemTrans, nartR(xWordFn, "lob"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Lobbing-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358961).
acnl(verbSemTrans, nartR(xWordFn, "nudge"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Nudging-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359173).
acnl(verbSemTrans, nartR(xWordFn, "nudge"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Nudging-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358963).
acnl(verbSemTrans, nartR(xWordFn, "pawn"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Pawning-TransferringPossession'), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4321412).
acnl(verbSemTrans, nartR(xWordFn, "pawn"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Pawning-TransferringPossession'), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358882).
acnl(verbSemTrans, nartR(xWordFn, "pawn"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Pawning-TransferringPossession'), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359100).
acnl(verbSemTrans, nartR(xWordFn, "UPS"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'UPSing-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358905).
acnl(verbSemTrans, nartR(xWordFn, "UPS"), 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'UPSing-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359123).
acnl(verbSemTrans, nartR(xWordWithPrefixFn, xReThePrefix, xGiftTheWord), 0, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actRegifting), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613425).
acnl(verbSemTrans, xArmTheWord, 0, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actArmingAnAgent), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613432).
acnl(verbSemTrans, xAwardTheWord, 1, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actGivingAnAward), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613428).
acnl(verbSemTrans, xBargeTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectBarge)), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288071).
acnl(verbSemTrans, xBargeTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectBarge)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358997).
acnl(verbSemTrans, xBargeTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectBarge)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359210).
acnl(verbSemTrans, xBatTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballSwing), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359178).
acnl(verbSemTrans, xBatTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballSwing), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289207).
acnl(verbSemTrans, xBatTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballSwing), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358968).
acnl(verbSemTrans, xBestowTheWord, 0, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actGivingSomething), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613435).
acnl(verbSemTrans, xBestowTheWord, 1, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actBestowing), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613431).
acnl(verbSemTrans, xBringTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actCarryingWhileLocomoting), transportees(':ACTION', ':OBJECT'), toLocation(':ACTION', ':INDIRECT-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -446232).
acnl(verbSemTrans, xBuntTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Bunting-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359166).
acnl(verbSemTrans, xBuntTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Bunting-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358956).
acnl(verbSemTrans, xBusTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectBusRoadVehicle)), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288067).
acnl(verbSemTrans, xBusTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectBusRoadVehicle)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358996).
acnl(verbSemTrans, xBusTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectBusRoadVehicle)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359209).
acnl(verbSemTrans, xBuyTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actBuying), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), buyer(':ACTION', ':SUBJECT'), objectPaidFor(':ACTION', ':OBJECT')), -440994).
acnl(verbSemTrans, xCardTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actCreditCardFraud), toPossessor(':ACTION', ':INDIRECT-OBJECT'), buyer(':ACTION', ':SUBJECT'), objectPaidFor(':ACTION', ':OBJECT')), -181066).
acnl(verbSemTrans, xCartTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectWagon)), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288063).
acnl(verbSemTrans, xCartTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectWagon)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358995).
acnl(verbSemTrans, xCartTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectWagon)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359208).
acnl(verbSemTrans, xCastTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actThrowingAnObject), performedBy(':ACTION', ':SUBJECT'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':INDIRECT-OBJECT')), -5614013).
acnl(verbSemTrans, xCastTheWord, 2, xDitransitiveNPNPFrame, and(isa(':ACTION', actCastingAFishingLine), performedBy(':ACTION', ':SUBJECT'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':INDIRECT-OBJECT')), -5614009).
acnl(verbSemTrans, xCauseTheWord, 2, xDitransitiveNPNPFrame, causesThingprop(':SUBJECT', possessiveRelation(':OBJECT', ':OBLIQUE-OBJECT')), -499782).
acnl(verbSemTrans, xChuckTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actChuckingAbandoningSomething), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359181).
acnl(verbSemTrans, xChuckTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actChuckingAbandoningSomething), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289209).
acnl(verbSemTrans, xChuckTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actChuckingAbandoningSomething), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358971).
acnl(verbSemTrans, xChuckTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actThrowingSomethingAway), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359179).
acnl(verbSemTrans, xChuckTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actThrowingSomethingAway), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289208).
acnl(verbSemTrans, xChuckTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actThrowingSomethingAway), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358969).
acnl(verbSemTrans, xConsiderTheWord, 0, xDitransitiveNPNPFrame, opinions(':SUBJECT', isUnderspecified(':OBLIQUE-OBJECT', ':OBJECT')), -558794).
acnl(verbSemTrans, xCookTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actCookingFood), beneficiary(':ACTION', ':INDIRECT-OBJECT'), performedBy(':ACTION', ':SUBJECT'), objectOfStateChange(':ACTION', ':OBJECT')), -431551).
acnl(verbSemTrans, xCostTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actBuying), buyer(':ACTION', ':INDIRECT-OBJECT'), objectOfPossessionTransfer(':ACTION', ':SUBJECT'), expenseFor(':INDIRECT-OBJECT', ':ACTION', ':OBJECT')), -464495).
acnl(verbSemTrans, xDecorateTheWord, 1, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actDecoratingSomeoneGivingAnAward), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613427).
acnl(verbSemTrans, xDelegateTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', eventDelegatingAuthority), performedBy(':ACTION', ':SUBJECT'), delegate(':ACTION', ':INDIRECT-OBJECT'), delegatedAuthority(':ACTION', _, ':OBJECT')), -583177).
acnl(verbSemTrans, xDeliverTheWord, 0, xDitransitiveNPNPFrame, and(objectsDelivered(':ACTION', ':OBJECT'), isa(':ACTION', actDeliveringSomeoneSomething), performedBy(':ACTION', ':SUBJECT'), toLocation(':ACTION', ':INDIRECT-OBJECT')), -3192984).
acnl(verbSemTrans, xDesignTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actDesigning), beneficiary(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288175).
acnl(verbSemTrans, xDesignTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actDesigning), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358836).
acnl(verbSemTrans, xDesignTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actDesigning), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), outputsCreated(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359053).
acnl(verbSemTrans, xDesignTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actMakingAPlan), beneficiary(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288176).
acnl(verbSemTrans, xDesignTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actMakingAPlan), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358837).
acnl(verbSemTrans, xDesignTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actMakingAPlan), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), outputsCreated(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359054).
acnl(verbSemTrans, xDictateTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventCommand), recipientOfInfo(':ACTION', ':OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289165).
acnl(verbSemTrans, xDictateTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventCommand), recipientOfInfo(':ACTION', ':OBLIQUE-OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358922).
acnl(verbSemTrans, xDictateTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventCommand), recipientOfInfo(':ACTION', ':OBLIQUE-OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), infoTransferred(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359221).
acnl(verbSemTrans, xDigTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actDiggingAHole), beneficiary(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288178).
acnl(verbSemTrans, xDigTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actDiggingAHole), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358839).
acnl(verbSemTrans, xDigTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actDiggingAHole), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), outputsCreated(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359056).
acnl(verbSemTrans, xDiscardTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actThrowingSomethingAway), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359180).
acnl(verbSemTrans, xDiscardTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actThrowingSomethingAway), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289210).
acnl(verbSemTrans, xDiscardTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actThrowingSomethingAway), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358970).
acnl(verbSemTrans, xDispatchTheWord, 24, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), primaryObjectMoving(':ACTION', ':OBJECT')), -5613938).
acnl(verbSemTrans, xDispatchTheWord, 24, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), objectMoving(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -5613684).
acnl(verbSemTrans, xDonateTheWord, 0, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actCharitableDonation), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613424).
acnl(verbSemTrans, xExpressTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Expressing-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358901).
acnl(verbSemTrans, xExpressTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Expressing-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359119).
acnl(verbSemTrans, xFerryTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectFerry)), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288055).
acnl(verbSemTrans, xFerryTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectFerry)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358993).
acnl(verbSemTrans, xFerryTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectFerry)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359206).
acnl(verbSemTrans, xFindTheWord, 1, xDitransitiveNPNPFrame, and(isa(':ACTION', eventFindingSomething), objectFound(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT'), affectedAgent(':ACTION', ':INDIRECT-OBJECT')), -2342093).
acnl(verbSemTrans, xFireTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShootingAProjectileWeapon), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359182).
acnl(verbSemTrans, xFireTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShootingAProjectileWeapon), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289211).
acnl(verbSemTrans, xFireTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShootingAProjectileWeapon), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358972).
acnl(verbSemTrans, xFlickTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Flicking-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359167).
acnl(verbSemTrans, xFlickTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Flicking-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358957).
acnl(verbSemTrans, xFlipTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventFlipping), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359184).
acnl(verbSemTrans, xFlipTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventFlipping), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289212).
acnl(verbSemTrans, xFlipTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventFlipping), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358974).
acnl(verbSemTrans, xFlyTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventFlying), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288042).
acnl(verbSemTrans, xFlyTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventFlying), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358990).
acnl(verbSemTrans, xFlyTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventFlying), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359203).
acnl(verbSemTrans, xFlyTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectAirTransportationDevice)), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288047).
acnl(verbSemTrans, xFlyTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectAirTransportationDevice)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358991).
acnl(verbSemTrans, xFlyTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectAirTransportationDevice)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359204).
acnl(verbSemTrans, xForwardTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), primaryObjectMoving(':ACTION', ':OBJECT')), -5613937).
acnl(verbSemTrans, xForwardTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), objectMoving(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -5613683).
acnl(verbSemTrans, xForwardTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288835).
acnl(verbSemTrans, xForwardTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358907).
acnl(verbSemTrans, xForwardTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359125).
acnl(verbSemTrans, xGetTheWord, 3, xDitransitiveNPNPFrame, and(isa(':ACTION', actGainingUserRights), performedBy(':ACTION', ':SUBJECT'), toPossessor(':ACTION', ':OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBLIQUE-OBJECT')), -3055665).
acnl(verbSemTrans, xGiveTheWord, 0, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actGivingSomething), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -2950998).
acnl(verbSemTrans, xGiveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actGivingOfferingCommunicationAct), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288293).
acnl(verbSemTrans, xGiveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actGivingOfferingCommunicationAct), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358877).
acnl(verbSemTrans, xGiveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actGivingOfferingCommunicationAct), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359095).
acnl(verbSemTrans, xGiveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actGivingSomething), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288292).
acnl(verbSemTrans, xGiveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actGivingSomething), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358876).
acnl(verbSemTrans, xGiveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actGivingSomething), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359094).
acnl(verbSemTrans, xGrantTheWord, 24, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actGrantingMoney), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613423).
acnl(verbSemTrans, xHandTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toLocation(':ACTION', ':INDIRECT-OBJECT'), fromLocation(':ACTION', ':SUBJECT'), objectMoving(':ACTION', ':OBJECT')), -477499).
acnl(verbSemTrans, xHandTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288836).
acnl(verbSemTrans, xHandTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358910).
acnl(verbSemTrans, xHandTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359128).
acnl(verbSemTrans, xHeaveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHeavingAnObject), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4287793).
acnl(verbSemTrans, xHeaveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHeavingAnObject), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358846).
acnl(verbSemTrans, xHeaveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHeavingAnObject), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359063).
acnl(verbSemTrans, xHitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHittingAnObject), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359185).
acnl(verbSemTrans, xHitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHittingAnObject), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289213).
acnl(verbSemTrans, xHitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHittingAnObject), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358975).
acnl(verbSemTrans, xHitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHittingCausingAnotherObjectsTranslationalMotion), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359187).
acnl(verbSemTrans, xHitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHittingCausingAnotherObjectsTranslationalMotion), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289215).
acnl(verbSemTrans, xHitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHittingCausingAnotherObjectsTranslationalMotion), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358977).
acnl(verbSemTrans, xHitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShootingAndHittingSomething), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359186).
acnl(verbSemTrans, xHitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShootingAndHittingSomething), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289214).
acnl(verbSemTrans, xHitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShootingAndHittingSomething), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358976).
acnl(verbSemTrans, xHockTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHockingSomethingDisablingSomeone), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288296).
acnl(verbSemTrans, xHockTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHockingSomethingDisablingSomeone), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358880).
acnl(verbSemTrans, xHockTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHockingSomethingDisablingSomeone), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359098).
acnl(verbSemTrans, xIssueTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', eventTransferringPossession), toPossessor(':ACTION', ':INDIRECT-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -439237).
acnl(verbSemTrans, xKickTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actKicking), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4287794).
acnl(verbSemTrans, xKickTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actKicking), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358847).
acnl(verbSemTrans, xKickTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actKicking), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359064).
acnl(verbSemTrans, xKnockTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Knocking-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359170).
acnl(verbSemTrans, xKnockTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Knocking-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358960).
acnl(verbSemTrans, xLeaseTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actRenting), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288295).
acnl(verbSemTrans, xLeaseTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actRenting), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358879).
acnl(verbSemTrans, xLeaseTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actRenting), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359097).
acnl(verbSemTrans, xLeaveTheWord, 1, xDitransitiveNPNPFrame, and(isa(':ACTION', eventTransferringPossession), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), deliberateActors(':ACTION', ':SUBJECT')), -439241).
acnl(verbSemTrans, xLendTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actBorrowingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -459129).
acnl(verbSemTrans, xLendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBorrowingSomething), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288286).
acnl(verbSemTrans, xLendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBorrowingSomething), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358871).
acnl(verbSemTrans, xLendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBorrowingSomething), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359088).
acnl(verbSemTrans, xLendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventLending), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288288).
acnl(verbSemTrans, xLendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventLending), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359090).
acnl(verbSemTrans, xLoanTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actBorrowingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -439232).
acnl(verbSemTrans, xLoanTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBorrowingSomething), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288285).
acnl(verbSemTrans, xLoanTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBorrowingSomething), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358870).
acnl(verbSemTrans, xLoanTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBorrowingSomething), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359087).
acnl(verbSemTrans, xLoanTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventLending), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288287).
acnl(verbSemTrans, xLoanTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventLending), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358872).
acnl(verbSemTrans, xLoanTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventLending), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359089).
acnl(verbSemTrans, xLoftTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Lofting-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359172).
acnl(verbSemTrans, xLoftTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Lofting-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358962).
acnl(verbSemTrans, xMailTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), primaryObjectMoving(':ACTION', ':OBJECT')), -5613934).
acnl(verbSemTrans, xMailTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), objectMoving(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -5613680).
acnl(verbSemTrans, xMailTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288837).
acnl(verbSemTrans, xMailTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358908).
acnl(verbSemTrans, xMailTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359126).
acnl(verbSemTrans, xMakeTheWord, 2, xDitransitiveNPNPFrame, and(isa(':ACTION', actMakingSomething), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), performedBy(':ACTION', ':SUBJECT'), products(':ACTION', ':OBJECT')), -423352).
acnl(verbSemTrans, xMintTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actMakingFn, tObjectCoinCurrency)), beneficiary(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288174).
acnl(verbSemTrans, xMintTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actMakingFn, tObjectCoinCurrency)), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358835).
acnl(verbSemTrans, xMintTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actMakingFn, tObjectCoinCurrency)), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), outputsCreated(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359052).
acnl(verbSemTrans, xPassTheWord, 2, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toLocation(':ACTION', ':INDIRECT-OBJECT'), fromLocation(':ACTION', ':SUBJECT'), objectMoving(':ACTION', ':OBJECT')), -477500).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359189).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288839).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358911).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359129).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288284).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358869).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actHandingSomethingToSomeone), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359086).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actOvertakingACompetitor), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288283).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actOvertakingACompetitor), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358868).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actOvertakingACompetitor), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359085).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPassingAGameBall), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359188).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPassingAGameBall), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288838).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPassingAGameBall), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358912).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPassingAGameBall), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359130).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPassingAGameBall), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288282).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPassingAGameBall), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358867).
acnl(verbSemTrans, xPassTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPassingAGameBall), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359084).
acnl(verbSemTrans, xPayTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actPaying), moneyTransferred(':ACTION', ':OBJECT'), toPossessor(':ACTION', ':INDIRECT-OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -439235).
acnl(verbSemTrans, xPeddleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actOfferingForSale), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288289).
acnl(verbSemTrans, xPeddleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actOfferingForSale), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358873).
acnl(verbSemTrans, xPeddleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actOfferingForSale), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359091).
acnl(verbSemTrans, xPitchTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballDelivery), performedBy(':ACTION', ':SUBJECT'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':INDIRECT-OBJECT')), -5614010).
acnl(verbSemTrans, xPitchTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballDelivery), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359190).
acnl(verbSemTrans, xPitchTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballDelivery), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289216).
acnl(verbSemTrans, xPitchTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballDelivery), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358978).
acnl(verbSemTrans, xPitchTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballPitch), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359191).
acnl(verbSemTrans, xPitchTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballPitch), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289217).
acnl(verbSemTrans, xPitchTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actBaseballPitch), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358979).
acnl(verbSemTrans, xPostTheWord, 2, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), primaryObjectMoving(':ACTION', ':OBJECT')), -5613936).
acnl(verbSemTrans, xPostTheWord, 2, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), objectMoving(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -5613682).
acnl(verbSemTrans, xProvideTheWord, 1, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actGivingSomething), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613434).
acnl(verbSemTrans, xPublishTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPublishingWrittenMaterial), beneficiary(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288177).
acnl(verbSemTrans, xPublishTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPublishingWrittenMaterial), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358838).
acnl(verbSemTrans, xPublishTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPublishingWrittenMaterial), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), outputsCreated(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359055).
acnl(verbSemTrans, xPuntTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Punting-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359174).
acnl(verbSemTrans, xPuntTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Punting-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358964).
acnl(verbSemTrans, xPushTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPushingAnObject), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4287791).
acnl(verbSemTrans, xPushTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPushingAnObject), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358844).
acnl(verbSemTrans, xPushTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPushingAnObject), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359061).
acnl(verbSemTrans, xQuoteTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Quoting-InformationTransferEvent'), recipientOfInfo(':ACTION', ':OBLIQUE-OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358921).
acnl(verbSemTrans, xQuoteTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Quoting-InformationTransferEvent'), recipientOfInfo(':ACTION', ':OBLIQUE-OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), infoTransferred(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359220).
acnl(verbSemTrans, xReadTheWord, 2, xDitransitiveNPNPFrame, and(isa(':ACTION', actReadingAloud), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), performedBy(':ACTION', ':SUBJECT'), informationOrigin(':ACTION', ':OBJECT')), -15240).
acnl(verbSemTrans, xReadTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actReading), recipientOfInfo(':ACTION', ':OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289166).
acnl(verbSemTrans, xReadTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actReading), recipientOfInfo(':ACTION', ':OBLIQUE-OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358923).
acnl(verbSemTrans, xReadTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actReading), recipientOfInfo(':ACTION', ':OBLIQUE-OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), infoTransferred(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359222).
acnl(verbSemTrans, xReadTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'ReadingSomething-Speaking'), recipientOfInfo(':ACTION', ':OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289167).
acnl(verbSemTrans, xReadTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'ReadingSomething-Speaking'), recipientOfInfo(':ACTION', ':OBLIQUE-OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358924).
acnl(verbSemTrans, xReadTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'ReadingSomething-Speaking'), recipientOfInfo(':ACTION', ':OBLIQUE-OBJECT'), informationOrigin(':ACTION', ':SUBJECT'), infoTransferred(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359223).
acnl(verbSemTrans, xRearrangeTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Rearranging-CreationEvent'), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358832).
acnl(verbSemTrans, xRearrangeTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Rearranging-CreationEvent'), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), outputsCreated(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359049).
acnl(verbSemTrans, xReceiveTheWord, 0, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actGivingSomething), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613436).
acnl(verbSemTrans, xRefundTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actPaying), moneyTransferred(':ACTION', ':OBJECT'), toPossessor(':ACTION', ':INDIRECT-OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -457781).
acnl(verbSemTrans, xRefundTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actRefunding), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288281).
acnl(verbSemTrans, xRefundTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actRefunding), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358866).
acnl(verbSemTrans, xRefundTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actRefunding), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359083).
acnl(verbSemTrans, xRenderTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventCausingToBeInACertainCondition), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288291).
acnl(verbSemTrans, xRenderTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventCausingToBeInACertainCondition), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358875).
acnl(verbSemTrans, xRenderTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventCausingToBeInACertainCondition), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359093).
acnl(verbSemTrans, xRentTheWord, 0, xDitransitiveNPNPFrame, rentsFrom(':INDIRECT-OBJECT', ':OBJECT', ':SUBJECT'), -421620).
acnl(verbSemTrans, xRentTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actRenting), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288294).
acnl(verbSemTrans, xRentTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actRenting), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358878).
acnl(verbSemTrans, xRentTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actRenting), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359096).
acnl(verbSemTrans, xReorganizeTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Reorganizing-CreationEvent'), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358833).
acnl(verbSemTrans, xReorganizeTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Reorganizing-CreationEvent'), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), outputsCreated(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359050).
acnl(verbSemTrans, xReserveTheWord, 1, xDitransitiveNPNPFrame, and(isa(':ACTION', actMakingAReservation), doneBy(':ACTION', ':SUBJECT'), exists(A, and(outputs(':ACTION', A), objectReserved(A, ':OBJECT')))), -1287341).
acnl(verbSemTrans, xReturnTheWord, 2, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actReturningSomething), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613430).
acnl(verbSemTrans, xScheduleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Scheduling-CreationEvent'), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358834).
acnl(verbSemTrans, xScheduleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Scheduling-CreationEvent'), beneficiary(':ACTION', ':OBLIQUE-OBJECT'), outputsCreated(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359051).
acnl(verbSemTrans, xSellTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actOfferingForSale), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288290).
acnl(verbSemTrans, xSellTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actOfferingForSale), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358874).
acnl(verbSemTrans, xSellTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actOfferingForSale), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359092).
acnl(verbSemTrans, xSendTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), primaryObjectMoving(':ACTION', ':OBJECT')), -5613935).
acnl(verbSemTrans, xSendTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toPossessor(':ACTION', ':INDIRECT-OBJECT'), objectMoving(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -5613681).
acnl(verbSemTrans, xSendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288840).
acnl(verbSemTrans, xSendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358909).
acnl(verbSemTrans, xSendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSendingSomething), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359127).
acnl(verbSemTrans, xSendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShipping), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288841).
acnl(verbSemTrans, xSendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShipping), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358913).
acnl(verbSemTrans, xSendTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShipping), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359131).
acnl(verbSemTrans, xServeTheWord, 0, xDitransitiveNPNPFrame, and(isa(':ACTION', actServingFoodOrDrink), performedBy(':ACTION', ':SUBJECT'), toLocation(':ACTION', ':INDIRECT-OBJECT'), primaryObjectMoving(':ACTION', ':OBJECT')), -472464).
acnl(verbSemTrans, xShipTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShipping), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288842).
acnl(verbSemTrans, xShipTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShipping), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358914).
acnl(verbSemTrans, xShipTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShipping), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359132).
acnl(verbSemTrans, xShootTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShootingAProjectileWeapon), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359183).
acnl(verbSemTrans, xShootTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShootingAProjectileWeapon), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289218).
acnl(verbSemTrans, xShootTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actShootingAProjectileWeapon), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358973).
acnl(verbSemTrans, xShoveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPushingAnObject), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359192).
acnl(verbSemTrans, xShoveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPushingAnObject), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4287792).
acnl(verbSemTrans, xShoveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPushingAnObject), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358845).
acnl(verbSemTrans, xShoveTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actPushingAnObject), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359062).
acnl(verbSemTrans, xShuttleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventTranslationBackAndForth), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288059).
acnl(verbSemTrans, xShuttleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventTranslationBackAndForth), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358994).
acnl(verbSemTrans, xShuttleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', eventTranslationBackAndForth), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359207).
acnl(verbSemTrans, xSlamTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Colliding'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359193).
acnl(verbSemTrans, xSlamTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Colliding'), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289219).
acnl(verbSemTrans, xSlamTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Colliding'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358980).
acnl(verbSemTrans, xSlapTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Slapping-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359175).
acnl(verbSemTrans, xSlapTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Slapping-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358965).
acnl(verbSemTrans, xSlingTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Slinging-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359176).
acnl(verbSemTrans, xSlingTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Slinging-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358966).
acnl(verbSemTrans, xSlipTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Slipping-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358903).
acnl(verbSemTrans, xSlipTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Slipping-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359121).
acnl(verbSemTrans, xSmashTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Smashing-Propelling'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359177).
acnl(verbSemTrans, xSmashTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Smashing-Propelling'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358967).
acnl(verbSemTrans, xSmuggleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSmuggling), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288843).
acnl(verbSemTrans, xSmuggleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSmuggling), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358915).
acnl(verbSemTrans, xSmuggleTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actSmuggling), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359133).
acnl(verbSemTrans, xSneakTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Sneaking-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358904).
acnl(verbSemTrans, xSneakTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Sneaking-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359122).
acnl(verbSemTrans, xSubmitTheWord, 0, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actSubmittingSomething), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613426).
acnl(verbSemTrans, xSupplyTheWord, 3, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actGivingSomething), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613433).
acnl(verbSemTrans, xTapTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actTappingHitting), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359194).
acnl(verbSemTrans, xTapTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actTappingHitting), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4289220).
acnl(verbSemTrans, xTapTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', actTappingHitting), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358981).
acnl(verbSemTrans, xThrowTheWord, 1, xDitransitiveNPNPFrame, and(isa(':ACTION', actThrowingAnObject), performedBy(':ACTION', ':SUBJECT'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':INDIRECT-OBJECT')), -5614011).
acnl(verbSemTrans, xTipTheWord, 0, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', actGivingAGratuity), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -5613429).
acnl(verbSemTrans, xTossTheWord, 1, xDitransitiveNPNPFrame, and(isa(':ACTION', actThrowingAnObject), performedBy(':ACTION', ':SUBJECT'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':INDIRECT-OBJECT')), -5614012).
acnl(verbSemTrans, xTransmitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'TransmittingSomething'), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288844).
acnl(verbSemTrans, xTransmitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'TransmittingSomething'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358916).
acnl(verbSemTrans, xTransmitTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'TransmittingSomething'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359134).
acnl(verbSemTrans, xTruckTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectTruck)), toLocation(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4288051).
acnl(verbSemTrans, xTruckTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectTruck)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358992).
acnl(verbSemTrans, xTruckTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', nartR(actTransportViaFn, tObjectTruck)), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359205).
acnl(verbSemTrans, xVolunteerTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Volunteering-TransferringPossession'), toPossessor(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4321395).
acnl(verbSemTrans, xVolunteerTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Volunteering-TransferringPossession'), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358881).
acnl(verbSemTrans, xVolunteerTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Volunteering-TransferringPossession'), toPossessor(':ACTION', ':OBLIQUE-OBJECT'), objectOfPossessionTransfer(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359099).
acnl(verbSemTrans, xWireTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Wiring-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4358906).
acnl(verbSemTrans, xWireTheWord, 789, xDitransitiveNPNPFrame, and(isa(':ACTION', 'Wiring-CausingAnotherObjectsTranslationalMotion'), toLocation(':ACTION', ':OBLIQUE-OBJECT'), objectMoving(':ACTION', ':OBJECT'), doneBy(':ACTION', ':SUBJECT')), -4359124).

acnl('verbSemTrans', 'xArmTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actArmingAnAgent'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3308003).
acnl('verbSemTrans', 'xAwardTheWord', 1, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actGivingAnAward'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3307999).
acnl('verbSemTrans', 'xBestowTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actGivingSomething'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3308006).
acnl('verbSemTrans', 'xBestowTheWord', 1, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actBestowing'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3308002).
acnl('verbSemTrans', 'xBringTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actCarryingWhileLocomoting'), 'transportees'('ACTION', 'OBJECT'), 'toLocation'('ACTION', 'INDIRECT-OBJECT'), 'doneBy'('ACTION', 'SUBJECT')), 663266).
acnl('verbSemTrans', 'xBuyTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actBuying'), 'toPossessor'('ACTION', 'OBLIQUE-OBJECT'), 'buyer'('ACTION', 'SUBJECT'), 'objectPaidFor'('ACTION', 'OBJECT')), 658092).
acnl('verbSemTrans', 'xCastTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actThrowingAnObject'), 'performedBy'('ACTION', 'SUBJECT'), 'objectActedOn'('ACTION', 'OBJECT'), 'toLocation'('ACTION', 'INDIRECT-OBJECT')), 3308512).
acnl('verbSemTrans', 'xCastTheWord', 2, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actCastingAFishingLine'), 'performedBy'('ACTION', 'SUBJECT'), 'objectActedOn'('ACTION', 'OBJECT'), 'toLocation'('ACTION', 'INDIRECT-OBJECT')), 3308509).
acnl('verbSemTrans', 'xCauseTheWord', 2, 'xDitransitiveNPNPFrame', 'causesThingprop'('SUBJECT', 'possessiveRelation'('OBJECT', 'OBLIQUE-OBJECT')), 710353).
acnl('verbSemTrans', 'xConsiderTheWord', 0, 'xDitransitiveNPNPFrame', 'opinions'('SUBJECT', 'isUnderspecified'('OBLIQUE-OBJECT', 'OBJECT')), 761838).
acnl('verbSemTrans', 'xCookTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actCookingFood'), 'beneficiary'('ACTION', 'INDIRECT-OBJECT'), 'performedBy'('ACTION', 'SUBJECT'), 'objectOfStateChange'('ACTION', 'OBJECT')), 648799).
acnl('verbSemTrans', 'xCostTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actBuying'), 'buyer'('ACTION', 'INDIRECT-OBJECT'), 'objectOfPossessionTransfer'('ACTION', 'SUBJECT'), 'expenseFor'('INDIRECT-OBJECT', 'ACTION', 'OBJECT')), 681250).
acnl('verbSemTrans', 'xDecorateTheWord', 1, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actDecoratingSomeoneGivingAnAward'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3307998).
acnl('verbSemTrans', 'xDelegateTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'eventDelegatingAuthority'), 'performedBy'('ACTION', 'SUBJECT'), 'delegate'('ACTION', 'INDIRECT-OBJECT'), 'delegatedAuthority'('ACTION', SIT_TYPE, 'OBJECT')), 779757).
acnl('verbSemTrans', 'xDeliverTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('objectsDelivered'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actDeliveringSomeoneSomething'), 'performedBy'('ACTION', 'SUBJECT'), 'toLocation'('ACTION', 'INDIRECT-OBJECT')), 2160880).
acnl('verbSemTrans', 'xDispatchTheWord', 24, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT'), 'primaryObjectMoving'('ACTION', 'OBJECT')), 3308442).
acnl('verbSemTrans', 'xDispatchTheWord', 24, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'objectMoving'('ACTION', 'OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 3308208).
acnl('verbSemTrans', 'xDonateTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actCharitableDonation'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3307995).
acnl('verbSemTrans', 'xFindTheWord', 1, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'eventFindingSomething'), 'objectFound'('ACTION', 'OBJECT'), 'doneBy'('ACTION', 'SUBJECT'), 'affectedAgent'('ACTION', 'INDIRECT-OBJECT')), 1789907).
acnl('verbSemTrans', 'xForwardTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT'), 'primaryObjectMoving'('ACTION', 'OBJECT')), 3308441).
acnl('verbSemTrans', 'xForwardTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'objectMoving'('ACTION', 'OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 3308207).
acnl('verbSemTrans', 'xGetTheWord', 3, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actGainingUserRights'), 'performedBy'('ACTION', 'SUBJECT'), 'toPossessor'('ACTION', 'OBJECT'), 'objectOfPossessionTransfer'('ACTION', 'OBLIQUE-OBJECT')), 2094972).
acnl(verbSemTransTemplate, actGivingSomething, xDitransitiveNPNPFrame, and(objectGiven(':ACTION', ':OBJECT'), isa(':ACTION', ':DENOT'), giver(':ACTION', ':SUBJECT'), givee(':ACTION', ':OBLIQUE-OBJECT')), -2918914).
acnl(verbSemTransTemplate, actSendingSomething, xDitransitiveNPNPFrame, and(isa(':ACTION', ':DENOT'), toPossessor(':ACTION', ':INDIRECT-OBJECT'), fromPossessor(':ACTION', ':SUBJECT'), primaryObjectMoving(':ACTION', ':OBJECT')), -483899).
acnl(verbSemTransTemplate, actSendingSomething, xDitransitiveNPNPFrame, and(isa(':ACTION', ':DENOT'), toPossessor(':ACTION', ':INDIRECT-OBJECT'), objectMoving(':ACTION', ':OBJECT'), fromPossessor(':ACTION', ':SUBJECT')), -483901).
acnl(verbSemTransTemplate, actThrowingAnObject, xDitransitiveNPNPFrame, and(isa(':ACTION', ':DENOT'), performedBy(':ACTION', ':SUBJECT'), objectActedOn(':ACTION', ':OBJECT'), toLocation(':ACTION', ':INDIRECT-OBJECT')), -483900).
acnl('verbSemTrans', 'xGiveTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actGivingSomething'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 2046576).
acnl('verbSemTrans', 'xGrantTheWord', 24, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actGrantingMoney'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3307994).
acnl('verbSemTrans', 'xHandTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actHandingSomethingToSomeone'), 'toLocation'('ACTION', 'INDIRECT-OBJECT'), 'fromLocation'('ACTION', 'SUBJECT'), 'objectMoving'('ACTION', 'OBJECT')), 694161).
acnl('verbSemTrans', 'xIssueTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'eventTransferringPossession'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'objectOfPossessionTransfer'('ACTION', 'OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 656364).
acnl('verbSemTrans', 'xLeaveTheWord', 1, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'eventTransferringPossession'), 'toPossessor'('ACTION', 'OBLIQUE-OBJECT'), 'objectOfPossessionTransfer'('ACTION', 'OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT'), 'deliberateActors'('ACTION', 'SUBJECT')), 656368).
acnl('verbSemTrans', 'xLendTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actBorrowingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'objectOfPossessionTransfer'('ACTION', 'OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 675934).
acnl('verbSemTrans', 'xLoanTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actBorrowingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'objectOfPossessionTransfer'('ACTION', 'OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 656359).
acnl('verbSemTrans', 'xMailTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT'), 'primaryObjectMoving'('ACTION', 'OBJECT')), 3308438).
acnl('verbSemTrans', 'xMailTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'objectMoving'('ACTION', 'OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 3308204).
acnl('verbSemTrans', 'xMakeTheWord', 2, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actMakingSomething'), 'beneficiary'('ACTION', 'OBLIQUE-OBJECT'), 'performedBy'('ACTION', 'SUBJECT'), 'products'('ACTION', 'OBJECT')), 640773).
acnl('verbSemTrans', 'xPassTheWord', 2, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actHandingSomethingToSomeone'), 'toLocation'('ACTION', 'INDIRECT-OBJECT'), 'fromLocation'('ACTION', 'SUBJECT'), 'objectMoving'('ACTION', 'OBJECT')), 694162).
acnl('verbSemTrans', 'xPayTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actPaying'), 'moneyTransferred'('ACTION', 'OBJECT'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 656362).
acnl('verbSemTrans', 'xPostTheWord', 2, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT'), 'primaryObjectMoving'('ACTION', 'OBJECT')), 3308440).
acnl('verbSemTrans', 'xPostTheWord', 2, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'objectMoving'('ACTION', 'OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 3308206).
acnl('verbSemTrans', 'xProvideTheWord', 1, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actGivingSomething'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3308005).
acnl('verbSemTrans', 'xReadTheWord', 2, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actReadingAloud'), 'beneficiary'('ACTION', 'OBLIQUE-OBJECT'), 'performedBy'('ACTION', 'SUBJECT'), 'informationOrigin'('ACTION', 'OBJECT')), 314572).
acnl('verbSemTrans', 'xReceiveTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actGivingSomething'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3308007).
acnl('verbSemTrans', 'xRefundTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actPaying'), 'moneyTransferred'('ACTION', 'OBJECT'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 674612).
acnl('verbSemTrans', 'xRentTheWord', 0, 'xDitransitiveNPNPFrame', 'rentsFrom'('INDIRECT-OBJECT', 'OBJECT', 'SUBJECT'), 639082).
acnl('verbSemTrans', 'xReserveTheWord', 1, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actMakingAReservation'), 'doneBy'('ACTION', 'SUBJECT'), 'exists'(RESERVATION, 'and'('outputs'('ACTION', RESERVATION), 'objectReserved'(RESERVATION, 'OBJECT')))), 1319880).
acnl('verbSemTrans', 'xReturnTheWord', 2, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actReturningSomething'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3308001).
acnl('verbSemTrans', 'xSendTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT'), 'primaryObjectMoving'('ACTION', 'OBJECT')), 3308439).
acnl('verbSemTrans', 'xSendTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actSendingSomething'), 'toPossessor'('ACTION', 'INDIRECT-OBJECT'), 'objectMoving'('ACTION', 'OBJECT'), 'fromPossessor'('ACTION', 'SUBJECT')), 3308205).
acnl('verbSemTrans', 'xServeTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actServingFoodOrDrink'), 'performedBy'('ACTION', 'SUBJECT'), 'toLocation'('ACTION', 'INDIRECT-OBJECT'), 'primaryObjectMoving'('ACTION', 'OBJECT')), 689166).
acnl('verbSemTrans', 'xSubmitTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actSubmittingSomething'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3307997).
acnl('verbSemTrans', 'xSupplyTheWord', 3, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actGivingSomething'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3308004).
acnl('verbSemTrans', 'xThrowTheWord', 1, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actThrowingAnObject'), 'performedBy'('ACTION', 'SUBJECT'), 'objectActedOn'('ACTION', 'OBJECT'), 'toLocation'('ACTION', 'INDIRECT-OBJECT')), 3308510).
acnl('verbSemTrans', 'xTipTheWord', 0, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actGivingAGratuity'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3308000).
acnl('verbSemTrans', 'xTossTheWord', 1, 'xDitransitiveNPNPFrame', 'and'('isa'('ACTION', 'actThrowingAnObject'), 'performedBy'('ACTION', 'SUBJECT'), 'objectActedOn'('ACTION', 'OBJECT'), 'toLocation'('ACTION', 'INDIRECT-OBJECT')), 3308511).
acnl('verbSemTrans', nartR('xWordWithPrefixFn', 'xReThePrefix', 'xGiftTheWord'), 0, 'xDitransitiveNPNPFrame', 'and'('objectGiven'('ACTION', 'OBJECT'), 'isa'('ACTION', 'actRegifting'), 'giver'('ACTION', 'SUBJECT'), 'givee'('ACTION', 'OBLIQUE-OBJECT')), 3307996).


