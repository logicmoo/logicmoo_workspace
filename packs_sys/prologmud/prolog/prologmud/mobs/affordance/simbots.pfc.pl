/*  *  <module> 
% Uses affordances in which agents belief that some outcome will happen
% We also in this file purposelty create disparities
% Example: Buy this new car and you will suddenly become sexy!
%  Result: less money in pocket now but have vehical - but not sexier!
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
 */

:- include(prologmud(mud_header)).
:- include('improve.pfc').

==>tCol(mobSimian).

:- dynamic(defined_affordance/1).
:- discontiguous(defined_affordance/1).

% See the the seemingly white (not dirrectly usable) in some tUsefull way
defined_affordance([subjType= "Passable",actionVerb= "TravelThru"]).

==>prologHybrid(mudDescription(ftTerm,ftString)).
prologHybrid(nameString(ftTerm,ftString)).

rtArgsVerbatum(defined_affordance).

defined_affordance([subjType= "Television",
stringMatch= "TV",
actionVerb= "Observe",
mudActionMaxDistance= 4,
'NonLoneliness_Social'= 3 * -2, % this form means the AI player thinks observing a TV will satisfy their NonLoneliness_Social needs by 3% .. yet instead, it reduces by 2%
'NonHunger'= 1 * -1, 
'BladderEmpty'= 0 * 0,
'Hygiene'= 0 * 0,
'Secure_Room'= 1 * 0,
'Fun'= 2 * 1,
'Sad_To_Happy'= 2 * 1,
'Energy'= 1 * -1]).

defined_affordance([subjType= "Door",
stringMatch= " * doorway",
stringMatch= "gate",
stringMatch= "Curtain",
superType= "Passable"]).

defined_affordance([subjType= "Floor",
stringMatch= "floor",
superType= "Passable"]).

defined_affordance([subjType= "Ladder",
stringMatch= "ladder",
superType= "Passable"]).

% looks like a very bad idea but not (this is mainly for testing the system)
defined_affordance([subjType= tFurniture,actionVerb= "BumpIntoBarrier",
'NonLoneliness_Social'= -300 * 0,
'Hygiene'= -300 * 0,
'Comfort'= -300 * 0,
'Energy'= -300 * 0,
'Fun'= -300 * 0]).

% yet every minute you are alive, God wishes to punish you
defined_affordance([ subjType=tAgent, actionVerb= "LiveAtLeastAMinute",
   'Energy'= 0 * -2,
   'NonHunger'= 0 * -2,
   'BladderEmpty'= 0 * -2,
   'Hygiene'= 0 * -2,
   'Secure_Room'= 0 * -2,
   'NonLoneliness_Social'= 0 * -2,
   'Fun'= 0 * -2,
   'Sad_To_Happy'= 0 * -2,
   'Comfort'= 0 * -2 ]).

% TODO hook_one_minute_timer_tick:- \+ suspend_timers, forall(no_repeats(tAgent(X)),agent_command_now(X,actLiveAtLeastAMinute(X))).

defined_affordance([subjType= "Shower",
actionVerb= "Operate",
slAnim= anim_AFRAID,
textName= "wash self with X",
textName= "take a shower",
'Comfort'= 10 * 10,
'Hygiene'= 30 * 30,
actionVerb= "Clean"]).

defined_affordance([subjType= "BathTub",
stringMatch= "bath",
stringMatch= "bathtub",
slExceptFor= "PlasticTub",
actionVerb= "Operate",
textName= "wash self with X",
textName= "Take a Bath",
slSit= true,
'Comfort'= 20 * 20,
'Energy'= -20 * -20,
'Hygiene'= 100 * 100,
actionVerb= "Clean"]).

defined_affordance([subjType= "Sink",
actionVerb= "Operate",
textName= "Wash Hands",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

% defined_affordance([subjType= "BigThing",require(mudSize > 8)]).

% defined_affordance([actionVerb= "Attach_To_Self",textName= "Attach it",slAnim= anim_RPS_PAPER,'Comfort'= 20 * 20,'LispScript'= "(progn (TheBot.Attach_To_Self Target))"]).

defined_affordance([subjType= "DanceBall",
actionVerb= "Dance",
textName= "Dance! Dance!",
slTouch= true,
slAnim= anim_DANCE,
'NonLoneliness_Social'= 10 * 10,
'Energy'= -10 * -20,
'Fun'= 10 * 10,
'Hygiene'= -10 * -10]).

defined_affordance([subjType= "PoseBall",
stringMatch= " * pose",
stringMatch= " * Pose",
slTouch= true]).

defined_affordance([subjType= tWashingMachine,
stringMatch= "Washing Machine",
actionVerb= "actOperate",
textName= "Wash The Clothes",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

defined_affordance([subjType= tClothesDryer,
stringMatch= "Dryer",
actionVerb= "actOperate",
textName= "Dry The Clothes",
'Comfort'= 0 * 0,
'Hygiene'= 10 * 10,
actionVerb= "Clean"]).

defined_affordance([subjType= "Bed",
actionVerb= "Sleep",
textSitName= "Sleep a few",
slSit= true,
slAnim= anim_SLEEP,
'Comfort'= 10 * 30,
'Energy'= 100 * 80]).

defined_affordance([subjType= "Mattress",
actionVerb= "Sleep",
textSitName= "Sleep a few",
slSit= true,
slAnim= anim_SLEEP,
'Comfort'= 10 * 30,
'Energy'= 100 * 80]).

defined_affordance([subjType= "Chair",
stringMatch= " * chair",
stringMatch= " * stool",
stringMatch= " * recliner",
actionVerb= "Sit",
textSitName= "Sit down",
slSit= true,
slAnim= anim_SMOKE_IDLE,
'Comfort'= 15 * 10,
'Energy'= 10 * 20]).

defined_affordance([subjType= "Couch",
stringMatch= "Sofa",
stringMatch= " * luvseat * ",
stringMatch= " * loveseat * ",
actionVerb= "Sit",
textSitName= "Sit down",
slSit= true,
slAnim= anim_SMOKE_IDLE,
'Comfort'= 20 * 20,
'Energy'= 10 * 20]).

defined_affordance([subjType= "Radio",
actionVerb= "Observe",
textName= "Listen to Radio",
mudActionMaxDistance= 4,
'Secure_Room'= 1 * 0,
'Fun'= 10 * 10,
'Sad_To_Happy'= 10 * 10,
'Energy'= 1 * -1]).

defined_affordance([subjType= "Mirror",
actionVerb= "Observe",
textName= "Pop your zits",
mudActionMaxDistance= 2,
'Secure_Room'= 1 * 0,
'Fun'= 10 * 10,
'Sad_To_Happy'= 10 * -1,
'Energy'= 1 * -1]).

defined_affordance([subjType= "Toilet",
actionVerb= "Potty",
textSitName= "Go potty",
slSit= true,
'BladderEmpty'= 100 * 100,
'Hygiene'= 0 * -10,
actionVerb= "Clean",
textName= "Flush it",
slAnim= anim_POINT_YOU,
'Hygiene'= 1 * 4,
'Fun'= 5 * 4]).

defined_affordance([subjType= "Fridge",
stringMatch= " * Fridge * ",
stringMatch= " * Frige * ",
stringMatch= " * icebox",
actionVerb= "Search",
textName= "Raid the fridge",
slAnim= anim_DRINK,
slGrab= true]).

defined_affordance([subjType= "Stove",
stringMatch= "Oven",
stringMatch= " * kitchen range",
actionVerb= "actOperate",
slAnim= anim_DRINK,
slGrab= true]).

defined_affordance([subjType= "Microwave",
actionVerb= "actOperate",
textName= "see what was forgotten in the microwave",
slAnim= anim_DRINK,
slGrab= true]).

defined_affordance([subjType= "Treadmill",
   actionVerb= "Operate",
   textName= "Excersize with X",
   textName= "Tread the mill",
   slSit= true]).

defined_affordance([subjType= "FixedLamp",
   stringMatch= " * floorlamp",
   stringMatch= "lamp",
   stringMatch= "lantern",
   stringMatch= "lightbulb",
   stringMatch= "lighting",
   actionVerb= "Operate",
   textName= "flip the switch",
   slAnim= anim_AIM_BAZOOKA_R]).

defined_affordance([subjType= "Pooltable",
stringMatch= " * pool table * ",
actionVerb= "Operate",
textName= "Play pool",
slAnim= anim_AIM_BAZOOKA_R]).

defined_affordance([subjType= "Barrier",
stringMatch= " * Wall * ",
stringMatch= " * Fence * ",
stringMatch= " * Pillar * ",
stringMatch= " * Roof * ",
stringMatch= " * Beam * "]).

defined_affordance([subjType= "Shelf",
stringMatch= " * cupboard",
stringMatch= " * Cabinet",
stringMatch= " * cabinate",
stringMatch= " * FoodStore",
actionVerb(2)=actPutXOn]).

defined_affordance([subjType= "Desk",
stringMatch= " * Lab Bench",
stringMatch= " * workbench",
stringMatch= " * officedesk",
actionVerb(2)=actPutXOn]).

defined_affordance([subjType= "Counter",stringMatch= "Bar",actionVerb(2)=actPutXOn]).

defined_affordance([subjType= "Container",stringMatch= "Plastic",actionVerb(2)="Put_X_In"]).

defined_affordance([subjType= "Table",
stringMatch= " * Coffee Table",
acceptsChild= tReadAble,
acceptsChild= tEatAble,
actionVerb(2)=actPutXOn]).

defined_affordance([subjType= tTrashContainer,
   stringMatch= "garbage * c",
   stringMatch= "trash * c",
   stringMatch= "trash * bin",
   stringMatch= "waste",
   stringMatch= "recycle * bin",
   acceptsChild= "Take",
   actionVerb(2)="Put_X_In",acceptsChild= takeAble]).

defined_affordance([subjType= "Bookcase",
stringMatch= " * Bookcase",
stringMatch= " * Bookshelf",
stringMatch= " * Bookshelve",
acceptsChild= tReadAble,
actionVerb(2)=actPutXOn,
textName= "Organize books",
slAnim= anim_YES,
'Fun'= 10 * 10,
'Secure_Room'= 20 * 20]).

defined_affordance([subjType= tReadAble,
stringMatch= "Book",
stringMatch= "Magazine",
actionVerb= "Observe",
textName= "Read book",
slGrab= true,
slAnim= anim_LAUGH_SHORT,
'Fun'= 10 * 10,
'Secure_Room'= 20 * 20,
actionVerb= "Take",
textName= "Take the materials"]).

defined_affordance([subjType= tEatAble,
'slAcceptsParent'= "Avatar",
actionVerb= "Eat",
textName= "Eat the food",
slAnim= anim_DRINK,
actionVerb= "Take",
textName= "Take the food"]).

defined_affordance([subjType= "Art",
stringMatch= "Art  * ",
actionVerb= "Observe",
textName= "Apreciate the Art",
slAnim= anim_YES_HAPPY,
'Fun'= 10 * 10,
'Secure_Room'= 20 * 20]).

defined_affordance([subjType= tDanceFloor,
actionVerb= "Operate",
textName= "Dance! Dance!",
slAnim= anim_DANCE2]).

defined_affordance([subjType= "Computer",
stringMatch= "keyboard",
stringMatch= "keypad",
stringMatch= "workstation",
stringMatch= "Monitor",
actionVerb= "Operate",
textName= "Look busy doing something!",
slAnim= anim_TYPE]).

defined_affordance([subjType= tAgent,
actionVerb= "Talk",
'NonLoneliness_Social'= 10 * 15,
'Fun'= 1 * 1,
actionVerb= "Argue",
'NonLoneliness_Social'= 10 * 15,
'Energy'= 0 * -10,
'Sad_To_Happy'= -10 * -10,
'Fun'= 20 * 10,
actionVerb= "Attack",
'NonLoneliness_Social'= 10 * 15,
'Energy'= 0 * -10,
'Sad_To_Happy'= 0 * -10,
'Fun'= 20 * 10,
actionVerb= "Kiss",
'NonLoneliness_Social'= 10 * 15,
'Sad_To_Happy'= 10 * 10,
'Fun'= 10 * 10]).

defined_affordance([subjType= touchAble,actionVerb= "Touch",
textName= "Touch",
slGrab= true,
'Fun'= 1 * 1,
'Secure_Room'= 1 * 1]).

defined_affordance([subjType= tSitAble,actionVerb= "Sit",
textName= "Sit on",
slSit= true,
slAnim= anim_SIT,
'Comfort'= 1 * 0,
'Fun'= 1 * 1,
'Secure_Room'= 1 * 1]).

defined_affordance([subjType= tHasSurface,actionVerb(2)=actPutXOn,
textName= "This is a Put_X_On placeholder",
slAnim= anim_FINGER_WAG,
'Fun'= -2 * 2,
'Energy'= 0 * -1]).


defined_affordance([subjType= tEatAble,actionVerb= "Eat",
textName= "Eat it",
slDestroyedOnUse= true,
'NonHunger'= 100 * 100,
'Hygiene'= 0 * -10]).

defined_affordance([subjType= tCarryAble,actionVerb= "Take", 
textName= "Take it",
'slAcceptsParent'= "Avatar"]).

defined_affordance([subjType= tLayAble,actionVerb= "Sleep",
   textName= "Lay on",
   slSit= true,
   slAnim= anim_SLEEP,
   'Comfort'= 5 * 5,
   'Energy'= 20 * 20]).

defined_affordance([alsoType= tLookAble,actionVerb= "Clean",
   textName= "Clean",
   slAnim= anim_FINGER_WAG,
   'Fun'= -2 * 2,
   'Energy'= 0 * -1]).

defined_affordance([alsoType= tLookAble,actionVerb= "Observe",
   textName= "Observe",
   mudActionMaxDistance= 5,
   slAnim= anim_CLAP,
   'Fun'= 2 * 1,
   'Energy'= 0 * -1]).

defined_affordance([subjType= tSitAble,actionVerb= "Excersize",
textName= "Excersize",
slAnim= animETWO_PUNCH,
'Fun'= 10 * 10,
'Hygiene'= -10 * -10]).

defined_affordance([subjType= tAgent,actionVerb= "Tickle",
textName= "Play with",
slAnim= anim_SHOOT_BOW_L,
alsoType= tLookAble,
'Energy'= -10 * -10,
'Fun'= 20 * 10]).

defined_affordance([subjType= tContainer,actionVerb= "Search",
textName= "Eat from",
slAnim= anim_DRINK,
'Hygiene'= 0 * -5,
'NonHunger'= 40 * 20]).

defined_affordance([subjType= tAgent,actionVerb= "Argue",
textName= "Argue",
alsoType= tLookAble,
slAnim= anim_ONETWO_PUNCH,
'Energy'= -11 * -20]).

defined_affordance([subjType= tAgent,actionVerb= "Talk",
textName= "Talk to",
mudActionMaxDistance= 3,
alsoType= tLookAble,
slAnim= anim_TALK,
'NonLoneliness_Social'= 11 * 20]).

defined_affordance([subjType= tAgent,actionVerb= "Attack",
textName= "Beat up",
slAnim= anim_SWORD_STRIKE,
'Energy'= -11 * -20]).

defined_affordance([subjType= tAgent,actionVerb= "Kiss",
textName= "Kiss",
slAnim= anim_BLOW_KISS,
'NonLoneliness_Social'= 11 * 20,
'Fun'= 21 * 20]).

defined_affordance([subjType= tLookAble,actionVerb= "Think_About",
textName= "Think about",
slAnim= anim_SHRUG,
'Fun'= 1 * 2]).

recreate(F/A):- abolish(F,A),dynamic(F/A),functor(P,F,A),export(F/A),nop(retractall(P)),!.
:-recreate(verb_desc/3).
:-recreate(verb_for_type/2).
:-recreate(verb_affordance_2/2).
:-recreate(can_hold_type/2).
:-recreate(verb_affordance/5).

:- check_clause_counts.
:- kb_shared(argIsa/3).
:- check_clause_counts.
:- kb_shared(genls/2).
:- kb_shared(mudActionMaxDistance(vtActionTemplate,  ttObjectType,ftInt)).

to_personal(mudEnergy,mudEnergy).
to_personal(Pred,APred):-atom_concat('',Pred,APred).

do_define_affordance(LIST):-
  (member(subjType= SType,LIST);member(alsoType= SType,LIST)),
  ti_name('t',SType,Type),!,
  ain(tCol(Type)),
  do_define_type_affordance(Type,LIST).

do_define_type_affordance1(Type,_= Type):-!.
do_define_type_affordance1(Type,subjType= String):-
 no_repeats(call_u(coerce_hook(String,ftString,StringM))),
 ain_expanded(nameString(Type,StringM)).

%coerce(A,B,C):-no_repeats(call_u(coerce_hook(A,B,C))),nop((sanity(show_failure(call_u(isa(C,B))))->!;true)).

do_define_type_affordance1(Type,alsoType= TWhat):-ti_name(t,TWhat,ParentType),ain(genls(Type,ParentType)).
do_define_type_affordance1(Type,superType= TWhat):-ti_name(t,TWhat,ParentType),ain(genls(Type,ParentType)).
do_define_type_affordance1(Type,actionVerb= SVerb):-ti_name(act,SVerb,Verb),nb_setval(actionVerb,Verb),!,assert_if_new(verb_for_type(Verb,Type)).
do_define_type_affordance1(Type,actionVerb(2)= SVerb):-ti_name(act,SVerb,Verb),nb_setval(actionVerb,Verb),
  (nb_current(acceptsChild,ChildType)->true;ChildType=tCarryAble),
  assert_if_new(verb_affordance_2(Verb,Type,ChildType)).
do_define_type_affordance1(Type,acceptsChild= TWhat):-ti_name(t,TWhat,ChildType),
  nb_setval(acceptsChild,ChildType),!,assert_if_new(can_hold_type(Type,ChildType)),
 (nb_current(actionVerb,Verb)->assert_if_new(verb_affordance_2(Verb,Type,ChildType));dmsg(warn(verb_affordance_3_no_verb(error(vVerb),Type,ChildType)))),!.
do_define_type_affordance1(Type,SPred= Wants * Gets):-ti_name(mud,SPred,Pred),nb_getval(actionVerb,Verb),to_personal(Pred,APred),
  to_rel_value(Wants,WantsR),
  to_rel_value(Gets,GetsR),
  assert_if_new(verb_affordance(Verb,Type,APred,WantsR,GetsR)).
do_define_type_affordance1(Type,mudActionMaxDistance= Distance):-nb_getval(actionVerb,Verb),ain(mudActionMaxDistance(Verb,Type,Distance)).
do_define_type_affordance1(Type,textSitName= String):-do_define_type_affordance1(Type,textName= String).
do_define_type_affordance1(Type,textName= String):-nb_getval(actionVerb,Verb),assert_if_new(verb_desc(Verb,Type,String)).
do_define_type_affordance1(Type,stringMatch= String):-assert_if_new(type_desc(Type,String)).
do_define_type_affordance1(_,Skipped=_):-atom_concat('sl',_,Skipped).
do_define_type_affordance1(Type,Skipped):-dmsg(error(skipped(do_define_type_affordance1(Type,Skipped)))).

do_define_type_affordance(_,[]).
do_define_type_affordance(Type,[H|LIST]):-do_define_type_affordance1(Type,H),!,do_define_type_affordance(Type,LIST),!.


to_rel_value(Val,- NVal):-number(Val), Val<0, NVal is Val * -1.
to_rel_value(Val,+ NVal):-number(Val), Val>0, NVal is Val .
to_rel_value( - Val,- Val):-!.
to_rel_value( + Val,+ Val):-!.
to_rel_value(Val,+ Val).

world_agent_plan(_World,Agent,Act):-
   (isa(Agent,mobSimian);isa(Agent,tAgent)),
   simian_idea(Agent,Act).

:-export(simian_ideas_possible/2).
% simian_ideas_possible(Agent,actTextcmd(Think_about,Visible)) :- verb_for_type(Think_about, Type),available_instances_of_type(Agent,Visible,Type).
simian_ideas_possible(Agent,actDo(Think_about,Visible)) :- verb_for_type(Think_about, Type),available_instances_of_type(Agent,Visible,Type).

simian_idea(Agent,Act):-
   findall(Act,simian_ideas_possible(Agent,Act),CMDS),choose_best(Agent,CMDS,Act).

% verb_affordance(Verb,Type,APred,Wants,Gets)
choose_best(_Agent,CMDS,Act):-random_permutation(CMDS,[Act|_]).

show_call_fmt(Call):-show_failure(Call),fmt(Call).

% args_match_types(ARGS,Types).
args_match_types(In,Out):-In==[],!,Out=[].
args_match_types(TemplIn,Templ):-is_list(TemplIn),is_list(Templ),!,maplist(args_match_types,TemplIn,Templ).
args_match_types([X],X):- nonvar(X),!.
args_match_types(TemplIn,Templ):-compound(TemplIn),!,TemplIn=..TemplInL, Templ=..TemplL, args_match_types(TemplInL,TemplL).
args_match_types(Templ,Templ):-!.
args_match_types(Obj,Type):-!,isa(Obj,Type).

% hook for toplevel pass 1
baseKB:agent_command(Agent,Templ):- on_x_debug(agent_command_affordance(Agent,Templ)).

% hook for toplevel pass last
agent_command_fallback(Agent,TemplIn):-agent_command_simbots_real(Agent,TemplIn).

agent_command_simbots_real(Agent,actImprove(Trait)):- nonvar(Trait),doActImprove(Agent,Trait).

actImprove(Trait):- current_agent(Agent),doActImprove(Agent,Trait).

agent_command_simbots_real(Agent,TemplIn):- nonvar(TemplIn), 
   simbots_templates(Templ),
   args_match_types(TemplIn,Templ),
    must_det_l((
    affordance_side_effects(Agent,Templ,Template),
    fmt(agent_command_simbots_real(Agent,Templ,Template)),
    ignore(affordance_message(Agent,Templ,Template)))),!.
  


affordance_side_effects(Agent,Templ,Template):-
  must_det_l((
      Templ=..[ActVerb|ARGS],
      verb_affordance(ActVerb,Types,_,_,_),args_match_types(ARGS,Types),!,must(Template=..[ActVerb,Types]),
      findall(t(Trait,Agent,Real), verb_affordance(ActVerb,Types,Trait,_Think,Real),NewAdds),
      show_call(forall(member(Add,NewAdds),mpred_ain(Add))))). % db_assert_sv

affordance_message(Agent,Templ,Template):- Templ=..[ActVerb|ARGS],
      verb_desc_or_else(ActVerb,Types,Mesg),args_match_types(ARGS,Types),!,must(Template=..[ActVerb,Types]),
      fmt(affordance_message(Agent,Templ,verb_affordance(ActVerb,Types,Mesg))),!.
      
verb_desc_or_else(ActVerb,Types,Mesg):-verb_desc(ActVerb,Types,Mesg).
verb_desc_or_else(ActVerb,Types,verb_desc(ActVerb,Types)):-nonvar(ActVerb),nonvar(Types),not(verb_desc(ActVerb,Types,_)).

agent_command_affordance(Agent,Templ):- simbots_templates(Templ), (fmt(agent_command_simbots_real_3(Agent,Templ)),fail).

==> baseKB:action_info(actDo(vtVerb,ftListFn(ftTerm)),"reinterps a action").
agent_command_affordance(Agent,actDo(A)):-CMD=..[A],!,agent_command_affordance(Agent,CMD).
agent_command_affordance(Agent,actDo(A,B)):-CMD=..[A,B],!,agent_command_affordance(Agent,CMD).
agent_command_affordance(Agent,actDo(A,B,C)):- CMD=..[A,B,C],!,agent_command_affordance(Agent,CMD).
agent_command_affordance(Agent,actDo(A,B,C,D)):- CMD=..[A,B,C,D],!,agent_command_affordance(Agent,CMD).
agent_command_affordance(Agent,actDo(A,B,C,D,E)):- CMD=..[A,B,C,D,E],!,agent_command_affordance(Agent,CMD).

==> baseKB:action_info(actTextcmd(ftString),"reinterps a term as text").
agent_command_affordance(Agent,actTextcmd(A)):-sformat(CMD,'~w',[A]),!,do_agent_action(Agent,CMD).
agent_command_affordance(Agent,actTextcmd(A,B)):-sformat(CMD,'~w ~w',[A,B]),!,do_agent_action(Agent,CMD).
agent_command_affordance(Agent,actTextcmd(A,B,C)):-sformat(CMD,'~w ~w ~w',[A,B,C]),!,do_agent_action(Agent,CMD).

doActImprove(Agent,Trait):-
      findall(agentTODO(Agent,actDo(ActVerb,Types)),
        (verb_affordance(ActVerb,Types,Trait,+ Think,_Real),ThinkN is Think,ThinkN>0), NewAdds),
      show_call(forall(member(Add,NewAdds),ain(Add))).



genls(tShelf,tHasSurface).
genls(tCounter,tHasSurface).
genls(tFood,tEatAble).
genls(tBar,tHasSurface).
genls(tSitAble,tHasSurface).
genls(tSofa,tCouch).
genls(tCouch,tSitAble).
genls(tChair,tSitAble).
genls(tMattress,tLayAble).
genls(tLayAble,tSitAble).
genls(tBed,tMattress).
genls(tCrib,tLayAble).
genls(tHasSurface, tContainer).
genls(tHasSurface, tPutTargetAble).
genls(tContainer, tPutTargetAble).



genls(tClothesDryer,tFurniture).
genls(tWashingMachine,tFurniture).
genls(tShower,tFurniture).
genls(tSitAble,tFurniture).
genls(tChair,tFurniture).
genls(tBed,tFurniture).
genls(tSink,tFurniture).
genls(tToilet,tFurniture).
genls(tBathTub,tFurniture).
genls(tFurniture,tUseAble).
genls(tFurniture,tObj).

baseKB:text_actverb("observe",actUse).
baseKB:text_actverb("operate",actUse).

simbots_t_v_o(Templ,V,O):- any_to_atom(V,A),Templ=..[A,O].
:- export(simbots_t_v_o/3).


==> (baseKB:action_info(Templ,DESC):-verb_desc(V,O,DESC),simbots_t_v_o(Templ,V,O)).
==> (baseKB:action_info(Templ,text([verb_for_type,V,O,DOC])):- no_repeats([V,O],verb_affordance(V,O,_,_,_)),simbots_t_v_o(Templ,V,O), 
                  findall(pir(P,I,R),((verb_affordance(V, O,P,I,R))),DOC)).

simbots_templates(Templ):-no_repeats(simbots_templates0(Templ)).
simbots_templates0(Templ):-verb_for_type(V, O),simbots_t_v_o(Templ,V,O).
simbots_templates0(Templ):-verb_desc(V,O,_),simbots_t_v_o(Templ,V,O).
simbots_templates0(Templ):-verb_affordance(V,O,_,_,_),simbots_t_v_o(Templ,V,O).


:-forall(defined_affordance(Attrs),
    must(do_define_affordance(Attrs))).

:-ain((verb_affordance(Verb, Obj, _,_,_)==>verb_for_type(Verb, Obj))).


:- dmsg(call((listing(verb_desc/3),
      listing(verb_for_type/2),
      listing(verb_affordance_2/2),
      listing(can_hold_type/2),
      listing(verb_affordance/5)))).


/*


Yields

:- dynamic verb_desc/3.

verb_desc(actOperate, tShower, "wash self with X").
verb_desc(actOperate, tShower, "take a shower").
verb_desc(actOperate, tBathTub, "wash self with X").
verb_desc(actOperate, tBathTub, "Take a Bath").
verb_desc(actOperate, tSink, "Wash Hands").
verb_desc(actDance, tDanceBall, "Dance! Dance!").
verb_desc(actOperate, tWashingMachine, "Wash The Clothes").
verb_desc(actOperate, tClothesDryer, "Dry The Clothes").
verb_desc(actSleep, tBed, "Sleep a few").
verb_desc(actSleep, tMattress, "Sleep a few").
verb_desc(actSit, tChair, "Sit down").
verb_desc(actSit, tCouch, "Sit down").
verb_desc(actObserve, tRadio, "Listen to Radio").
verb_desc(actObserve, tMirror, "Pop your zits").
verb_desc(actPotty, tToilet, "Go potty").
verb_desc(actClean, tToilet, "Flush it").
verb_desc(actSearch, tFridge, "Raid the fridge").
verb_desc(actOperate, tMicrowave, "see what was forgotten in the microwave").
verb_desc(actOperate, tTreadmill, "Excersize with X").
verb_desc(actOperate, tTreadmill, "Tread the mill").
verb_desc(actOperate, tFixedLamp, "flip the switch").
verb_desc(actOperate, tPoolTable, "Play pool").
verb_desc(actPutXOn, tBookcase, "Browse books").
verb_desc(actObserve, tReadAble, "Read book").
verb_desc(actTake, tReadAble, "Take the materials").
verb_desc(actEat, tEatAble, "Eat the food").
verb_desc(actTake, tEatAble, "Take the food").
verb_desc(actObserve, tArt, "Apreciate the Art").
verb_desc(actOperate, tDanceFloor, "Dance! Dance!").
verb_desc(actOperate, tComputer, "Look busy doing something!").
verb_desc(actTouch, tTouchAble, "Touch").
verb_desc(actSit, tSitAble, "Sit on").
verb_desc(actPutXOn, tHasSurface, "This is a Put_X_On placeholder").
verb_desc(actEat, tEatAble, "Eat it").
verb_desc(actTake, tCarryAble, "Take it").
verb_desc(actSleep, tLayAble, "Lay on").
verb_desc(actClean, tLookAble, "Clean").
verb_desc(actObserve, tLookAble, "Observe").
verb_desc(actExcersize, tSitAble, "Excersize").
verb_desc(actTickle, tAgent, "Play with").
verb_desc(actSearch, tContainer, "Eat_from").
verb_desc(actArgue, tAgent, "Argue").
verb_desc(actTalk, tAgent, "Talk to").
verb_desc(actAttack, tAgent, "Beat up").
verb_desc(actKiss, tAgent, "Kiss").
verb_desc(actThinkAbout, tLookAble, "Think about").

:- dynamic verb_affordance/5.

verb_affordance(actObserve, tTelevision, mudNonLonelinessSocial, + 3, + -2).
verb_affordance(actObserve, tTelevision, mudNonHunger, 1, + -1).
verb_affordance(actObserve, tTelevision, mudBladderEmpty, 0, 0).
verb_affordance(actObserve, tTelevision, mudHygiene, 0, 0).
verb_affordance(actObserve, tTelevision, mudSecureRoom, 1, 0).
verb_affordance(actObserve, tTelevision, mudFun, 2, 1).
verb_affordance(actObserve, tTelevision, mudSadToHappy, 2, 1).
verb_affordance(actObserve, tTelevision, mudEnergy, + 1, + -1).
verb_affordance(actBumpIntoBarrier, tFurniture, mudNonLonelinessSocial, + -300, 0).
verb_affordance(actBumpIntoBarrier, tFurniture, mudHygiene, + -300, 0).
verb_affordance(actBumpIntoBarrier, tFurniture, mudComfort, + -300, 0).
verb_affordance(actBumpIntoBarrier, tFurniture, mudEnergy, + -300, 0).
verb_affordance(actBumpIntoBarrier, tFurniture, mudFun, + -300, 0).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudEnergy, 0, + -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudNonHunger, 0, + -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudBladderEmpty, 0, + -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudHygiene, 0, + -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudSecureRoom, 0, + -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudNonLonelinessSocial, 0, + -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudFun, 0, + -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudSadToHappy, 0, + -1).
verb_affordance(actLiveAtLeastAMinute, tAgentSelf, mudComfort, 0, + -1).
verb_affordance(actOperate, tShower, mudComfort, 10, 10).
verb_affordance(actOperate, tShower, mudHygiene, 30, 30).
verb_affordance(actOperate, tBathTub, mudComfort, 20, 20).
verb_affordance(actOperate, tBathTub, mudHygiene, 100, 100).
verb_affordance(actOperate, tSink, mudComfort, 0, 0).
verb_affordance(actOperate, tSink, mudHygiene, 10, 10).
verb_affordance(actDance, tDanceBall, mudNonLonelinessSocial, 10, 10).
verb_affordance(actDance, tDanceBall, mudFun, 10, 10).
verb_affordance(actDance, tDanceBall, mudHygiene, + -10, + -10).
verb_affordance(actOperate, tWashingMachine, mudComfort, 0, 0).
verb_affordance(actOperate, tWashingMachine, mudHygiene, 10, 10).
verb_affordance(actOperate, tClothesDryer, mudComfort, 0, 0).
verb_affordance(actOperate, tClothesDryer, mudHygiene, 10, 10).
verb_affordance(actSleep, tBed, mudComfort, 10, 30).
verb_affordance(actSleep, tBed, mudEnergy, 100, 80).
verb_affordance(actSleep, tMattress, mudComfort, 10, 30).
verb_affordance(actSleep, tMattress, mudEnergy, 100, 80).
verb_affordance(actSit, tChair, mudComfort, 15, 10).
verb_affordance(actSit, tChair, mudEnergy, 10, 20).
verb_affordance(actSit, tCouch, mudComfort, 20, 20).
verb_affordance(actSit, tCouch, mudEnergy, 10, 20).
verb_affordance(actObserve, tRadio, mudSecureRoom, 1, 0).
verb_affordance(actObserve, tRadio, mudFun, 10, 10).
verb_affordance(actObserve, tRadio, mudSadToHappy, 10, 10).
verb_affordance(actObserve, tRadio, mudEnergy, 1, + -1).
verb_affordance(actObserve, tMirror, mudSecureRoom, 1, 0).
verb_affordance(actObserve, tMirror, mudFun, 10, 10).
verb_affordance(actObserve, tMirror, mudSadToHappy, 10, + -1).
verb_affordance(actObserve, tMirror, mudEnergy, 1, + -1).
verb_affordance(actPotty, tToilet, mudBladderEmpty, 100, 100).
verb_affordance(actPotty, tToilet, mudHygiene, 0, + -10).
verb_affordance(actClean, tToilet, mudHygiene, 1, 4).
verb_affordance(actClean, tToilet, mudFun, 5, 4).
verb_affordance(actPutXOn, tBookcase, mudFun, 10, 10).
verb_affordance(actPutXOn, tBookcase, mudSecureRoom, 20, 20).
verb_affordance(actObserve, tReadAble, mudFun, 10, 10).
verb_affordance(actObserve, tReadAble, mudSecureRoom, 20, 20).
verb_affordance(actObserve, tArt, mudFun, 10, 10).
verb_affordance(actObserve, tArt, mudSecureRoom, 20, 20).
verb_affordance(actTalk, tAgent, mudNonLonelinessSocial, 10, 15).
verb_affordance(actTalk, tAgent, mudFun, 1, 1).
verb_affordance(actArgue, tAgent, mudNonLonelinessSocial, 10, 15).
verb_affordance(actArgue, tAgent, mudEnergy, 0, + -10).
verb_affordance(actArgue, tAgent, mudSadToHappy, + -10, + -10).
verb_affordance(actArgue, tAgent, mudFun, 20, 10).
verb_affordance(actAttack, tAgent, mudNonLonelinessSocial, 10, 15).
verb_affordance(actAttack, tAgent, mudSadToHappy, 0, + -10).
verb_affordance(actAttack, tAgent, mudEnergy, 0, + -10).
verb_affordance(actAttack, tAgent, mudFun, 20, 10).
verb_affordance(actKiss, tAgent, mudNonLonelinessSocial, 10, 15).
verb_affordance(actKiss, tAgent, mudSadToHappy, 10, 10).
verb_affordance(actKiss, tAgent, mudFun, 10, 10).
verb_affordance(actTouch, tTouchAble, mudFun, 1, 1).
verb_affordance(actTouch, tTouchAble, mudSecureRoom, 1, 1).
verb_affordance(actSit, tSitAble, mudComfort, 1, 0).
verb_affordance(actSit, tSitAble, mudFun, 1, 1).
verb_affordance(actSit, tSitAble, mudSecureRoom, 1, 1).
verb_affordance(actPutXOn, tHasSurface, mudFun, + -2, 2).
verb_affordance(actPutXOn, tHasSurface, mudEnergy, 0, + -1).
verb_affordance(actEat, tEatAble, mudNonHunger, 100, 100).
verb_affordance(actEat, tEatAble, mudHygiene, 0, + -10).
verb_affordance(actSleep, tLayAble, mudComfort, 5, 5).
verb_affordance(actSleep, tLayAble, mudEnergy, 20, 20).
verb_affordance(actClean, tLookAble, mudFun, + -2, 2).
verb_affordance(actClean, tLookAble, mudEnergy, 0, + -1).
verb_affordance(actObserve, tLookAble, mudFun, 2, 1).
verb_affordance(actObserve, tLookAble, mudEnergy, 0, + -1).
verb_affordance(actExcersize, tSitAble, mudFun, 10, 10).
verb_affordance(actExcersize, tSitAble, mudHygiene, + -10, + -10).
verb_affordance(actTickle, tAgent, mudEnergy, + -10, + -10).
verb_affordance(actTickle, tAgent, mudFun, 20, 10).
verb_affordance(actSearch, tContainer, mudHygiene, 0, + -5).
verb_affordance(actSearch, tContainer, mudNonHunger, 40, 20).
verb_affordance(actArgue, tAgent, mudEnergy, + -11, + -20).
verb_affordance(actTalk, tAgent, mudNonLonelinessSocial, 11, 20).
verb_affordance(actAttack, tAgent, mudEnergy, + -11, + -20).
verb_affordance(actKiss, tAgent, mudNonLonelinessSocial, 11, 20).
verb_affordance(actKiss, tAgent, mudFun, 21, 20).
verb_affordance(actThinkAbout, tLookAble, mudFun, 1, 2).

:- dynamic verb_for_type/2.

verb_for_type(actTravelThru, tPassAble).
verb_for_type(actObserve, tTelevision).
verb_for_type(actBumpIntoBarrier, tFurniture).
verb_for_type(actLiveAtLeastAMinute, tAgentSelf).
verb_for_type(actOperate, tShower).
verb_for_type(actClean, tShower).
verb_for_type(actOperate, tBathTub).
verb_for_type(actClean, tBathTub).
verb_for_type(actOperate, tSink).
verb_for_type(actClean, tSink).
verb_for_type(actDance, tDanceBall).
verb_for_type(actOperate, tWashingMachine).
verb_for_type(actClean, tWashingMachine).
verb_for_type(actOperate, tClothesDryer).
verb_for_type(actClean, tClothesDryer).
verb_for_type(actSleep, tBed).
verb_for_type(actSleep, tMattress).
verb_for_type(actSit, tChair).
verb_for_type(actSit, tCouch).
verb_for_type(actObserve, tRadio).
verb_for_type(actObserve, tMirror).
verb_for_type(actPotty, tToilet).
verb_for_type(actClean, tToilet).
verb_for_type(actSearch, tFridge).
verb_for_type(actOperate, tStove).
verb_for_type(actOperate, tMicrowave).
verb_for_type(actOperate, tTreadmill).
verb_for_type(actOperate, tFixedLamp).
verb_for_type(actOperate, tPoolTable).
verb_for_type(actPutXOn, tShelf).
verb_for_type(actPutXOn, tDesk).
verb_for_type(actPutXOn, tCounter).
verb_for_type(actPutXIn, tContainer).
verb_for_type(actPutXOn, tTable).
verb_for_type(actPutXIn, tTrashContainer).
verb_for_type(actPutXOn, tBookcase).
verb_for_type(actObserve, tReadAble).
verb_for_type(actTake, tReadAble).
verb_for_type(actEat, tEatAble).
verb_for_type(actTake, tEatAble).
verb_for_type(actObserve, tArt).
verb_for_type(actOperate, tDanceFloor).
verb_for_type(actOperate, tComputer).
verb_for_type(actTalk, tAgent).
verb_for_type(actArgue, tAgent).
verb_for_type(actAttack, tAgent).
verb_for_type(actKiss, tAgent).
verb_for_type(actTouch, tTouchAble).
verb_for_type(actSit, tSitAble).
verb_for_type(actPutXOn, tHasSurface).
verb_for_type(actEat, tEatAble).
verb_for_type(actTake, tCarryAble).
verb_for_type(actSleep, tLayAble).
verb_for_type(actClean, tLookAble).
verb_for_type(actObserve, tLookAble).
verb_for_type(actExcersize, tSitAble).
verb_for_type(actTickle, tAgent).
verb_for_type(actSearch, tContainer).
verb_for_type(actThinkAbout, tLookAble).


*/


:-ain({simbots_templates(Templ)} ==> vtActionTemplate(Templ)).

:- dmsg(call(listing(vtActionTemplate))).