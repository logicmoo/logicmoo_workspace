
:-module(agent,[]).

:- use_module(library(pfc_lib)).
:- style_check(- discontiguous).  

same_meanings(UserSaid,UserSaid).

initial_goal(introductions(Agent2,Agent1)):- 
   prop(robot,self,Agent2),
   prop(user,self,Agent1).

builtin(Mode,(A,B)):- !,call(Mode,A),call(Mode,B).
builtin(Mode, [A|B]):- !,call(Mode,A),call(Mode,B).
builtin(Mode,(A;B)):- !, (call(Mode,A);call(Mode,B)).
builtin(_,play_mode(Mode)):- retractall(play_mode(_)),ain(play_mode(Mode)).
builtin(_,noplay_mode):- retractall(play_mode(_)),!.
%builtin(Mode,ensure(Game)):- !, ensure(Mode,Game).
builtin(_Mode,debug(English)):- nop(ain(pipeline(English))),wdmsg(debug(English)).
builtin(_Mode,expect(Agent2,English)):- said(Agent2,UserSaid),!,same_meanings(UserSaid,English).

ensure(Sit):- var(Sit),!,throw(var_ensure(Sit)).
ensure([]).
ensure(ensure(Sit)):- !, ensure(Sit).
ensure(Sit):- clause(builtin(ensure,Sit),Body),!,call(Body).
ensure(Sit):- already_true(Sit),!, wdmsg(already_true(Sit)).
ensure(Sit):- \+ \+ is_assumable_happens(Sit),!, wdmsg(happen(Sit)),ain(Sit).
ensure(Sit):- \+ \+ is_assumable(Sit),!, wdmsg(happen(Sit)),ain(Sit).
ensure(Sit):- clause(make_true(Sit),Body),wdmsg(trying(make_true(Sit))),call(Body),!,wdmsg(success(make_true(Sit))),ain(Sit).
ensure(Sit):- wdmsg(failed(ensure(Sit))),!,fail,ain(Sit).

is_assumable_happens(X):- \+ \+ X = prop(_,_,_),!, fail.
is_assumable_happens(wants(_,_)).
is_assumable_happens(X):- is_assumable(X).

is_assumable(prop(_,_,_)).
is_assumable(heard(Robot,_)):- Robot\==user.
is_assumable(Prop):-compound(Prop),arg(1,Prop,V),nonvar(V),functor(Prop,F,A),functor(Prop2,F,A),!,is_assumable(Prop2).
is_assumable(avoids(agent,prop)).
is_assumable(wants(agent,prop)).
is_assumable(said(agent,prop)).
%is_assumable(heard(agent,prop)).


:- dynamic(prop/3).

prop(user,name,grace).
%prop(robot,name,doug).
prop(user,self,user).
prop(robot,self,robot).
prop(S,P,O):- atom(P), O=..[P,S].



state_pred(unknown(agent,prop)).
state_pred(know(agent,prop)).
state_pred(suspects(agent,prop)).


define_state_pred(Decl):- functor(Decl,F,A),dynamic(F/A).
:- forall(state_pred(Pred),define_state_pred(Pred)).
% agents already know their props
already_true(know(Agent2,Sit)):- compound(Sit), arg(1,Sit,Agent2).
already_true(Sit):- compound(Sit), clause(Sit,true).

satisfy(Agent1,wants(Agent1,Sit)) :- already_true(know(Agent1,Sit)),!.
satisfy(Agent1,wants(Agent1, know(Agent2,Sit))) :-
  %Sit = prop(Agent1,name,_), 
  already_true(know(Agent1,Sit)),!,
  make_true(from_to_said(Agent1,Agent2,Sit)).

%make_true(Sit) :- already_true(Sit).

make_true(from_to_said(Agent1,Agent2,Sit)):- 
   nonvar(Sit),
     wdmsg(from_to_said(Agent1,Agent2,Sit)),
     ensure((
       said(Agent1,Sit),
       heard(Agent2,Sit))).

 
make_true(heard(Agent1,ask(Agent2,Say))):- 
  make_true(say_from_to(Agent1,Agent2,ask(Say))).

% dont ask what they already know
make_true(know(Agent2,Sit)) :- already_true(know(Agent2,Sit)).
% may ask questions when they dont know
make_true(know(Agent2,Sit)) :-
    compound(Sit),
    arg(1,Sit,Agent1),
    dif(Agent1,Agent2),
    ensure([
      % heard(Agent2,ask(Agent1,Sit)),
       heard(Agent1,tell(Agent2,Sit))]).


make_true( heard(Agent1,tell(Agent2,Sit))):-
  ensure(heard(Agent2,ask(Agent1,Sit))).

make_true(introductions(Agent1,Agent2)):-
 ensure((
  know(Agent2,prop(Agent1,name,_)),
  know(Agent1,prop(Agent2,name,_)))).

:- dynamic(play_mode/1).
  
satisfies(wants(Agent1,know(Agent2,prop(Agent1,name,_)))):-  make_true(from_to_said(Agent1,Agent2,"My name is Grace.")).

make_true(achieves(Agent1,Goal)):-
 Goal = know(Agent1,prop(Agent2,name,_)),
 ensure((
  %play_mode(know(Agent1,$term)),
  prop(Agent1,pronoun,"I"), 
  prop(Agent2,pronoun,"you"),
  debug("i want you to know my name"), 
  %play_mode(ensure(Agent1,$term)),
  play_mode(ensure),
  wants(Agent1,know(Agent2,prop(Agent1,name,_))), % in case the system is in daydream mode
  said(Agent1,"My name is Professor_Einstein."),
  debug("i want to know your name so I am going to ask you"),
  ensure(wants(Agent1,Goal)), % in case the system is in daydream mode
  Reject = not(wants(Agent2,Goal)),
  Accepts = wants(Agent2,Goal),
  %Fullfills = make_true(Agent2,Goal),
  said(Agent1,"What is your first name so that I can know it?"),
 ((expect(Reject)->make_true(find_out_why(not(wants(Agent2,Goal)))) ;
  (expect(Accepts)-> ensure(wants(Agent2,know(Agent1,prop(Agent2,name,_))))))))).


make_true(knows_each_others_name):- 
 Agent1=robot,
 Agent2=user,
 ensure((
  play_mode(ensure),
  prop(Agent1,pronoun,"I"),
  prop(Agent2,pronoun,"you"),
  debug("i want you to know my name"),
  wants(Agent1,know(Agent2,prop(Agent1,name,_))),
  said(Agent1,"My name is Professor Einstein."),
  debug("i want me to know your name so I am going to ask you to spell it"),
  ensure(wants(Agent1,know(Agent1,list(Agent2,letters_of_first_name,_)))),
  said(Agent1,"Would you spell your first name so that I can know it?"),
 ((expect(Agent2,"no")->make_true(find_out_why(not(wants(Agent2,know(Agent1,list(Agent2,letters_of_first_name,_)))))) ;
  (expect(Agent2,"yes")-> ensure(wants(Agent2,know(Agent1,list(Agent2,letters_of_first_name,_))))))))).



/*
wants(Agent1,know(Agent1,prop(Agent2,name,?))),
said(Agent1,"Hi, my name is Professor Einstein."),
debug("i want me to know your name's length"),
wants(Agent1,know(Agent1,prop(Agent2,name,N),prop(N,length,?))),
debug("i want me to know your name's length"),
(prop(Agent2,name,N),prop(N,length,L))=>
prop(Agent2,name,N),prop(N,length,_))
prop(N,length,L) ? 
wants(me,know(Agent2,prop(me,name))))
"Hi, my name is Professor Einstein."
"Would you spell your name for me?"
"D. O. U. G."
"How many letters is your name?"  
"7"
"I've only heard 4 so far.. I debug you have 3 to go?"
*/

 /*
BINA48: Initializing... [about 5 sec] ...What's next?

human:  [moves an object]

BINA48: [does not know the object] How is it called?

human:  coffee-cup

BINA48: Did you say coffee-cup?

human:  yes

BINA48: Ok. Now I know. What kind of object is coffee-cup?

human:  a cup

BINA48: Did you say cup?

human:  yes

BINA48: So coffee cup is a cup. What's next?

BINA48: Servus! The Bavarian Agent1 is up and fit!

BINA48: What is the name of the WHITE CLUSTER on the TABLE?
human:  "the mug" 

[BINA48 addshOBJ001 rdfs:label "the mug"i stmt]
BINA48: What kind of object is the mug?

human:  Mug

BINA48: What is a Mug? [BINA48: adds hOBJ001 rdf:type Mugi but it does not know the Mug concept]

human:  Tableware [BINA48: knows about Tableware - belongs to the common-sense ontology. It adds
hMug rdfs:subClassOf Tablewarei]
(...)

BINA48: What is the name of the BLACK CLUSTER on the TABLE?
human:  my laptop

BINA48: What kind of object is my laptop?
human:  Computer

BINA48: What is a Computer?
human:  Artifact

BINA48: Do you want me to learn more objects?
human:  no

BINA48: These objects are odd: my laptop
human:  It is a tableware.

BINA48: [retrieves possible objects: blue-bottle, yellow-bottle, orange-bottle, cup-with-handle]
[keeps visible objects: blue-bottle, yellow-bottle, cup-with-handle]
[obtains discriminants: type, color.]
Which type of object is: bottle or cup?

human:  Bottle.

BINA48: [obtains possible objects: blue-bottle, yellow-bottle.]
[obtains discriminants: color.]
What color the object is: blue or yellow?

human:  Blue.

BINA48: [obtains possible objects: blue-bottle.]
The object is the blue-bottle!

*/
:- fixup_exports.
end_of_file.

toploop(UserOrSelf):- 
  re_enumerate_agent_goals(UserOrSelf,Goals),
  expand_to_other_implicit_goals(UserOrSelf,Goals,GoalsAll),
  get_previous__goals(UserOrSelf,PrevGoals),
  divide(GoalsAll,PrevGoals,NoLonger,Continued,New),
  why_goal_satisfied(UserOrSelf,NoLonger,WhyNoLonger),
  said(Agent1,WhyNoLonger),
  preconds_of_goals(UserOrSelf,Continued,ContinuedPreconds),
  preconds_of_goals(UserOrSelf,New,NewPreconds),
  divide(ContinuedPreconds,NewPreconds,NoLongerPrecond,PrecondsStillNeeded,ActualNewPreconds),
  note(predconds_still_needed(UserOrSelf,PrecondsStillNeeded)),
  note(released_preconds(UserOrSelf,NoLongerPrecond)),
  note(add_req_preconds(UserOrSelf,ActualNewPreconds)),
  !.

