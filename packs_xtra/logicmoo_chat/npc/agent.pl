
%:-module(agent,[]).
:- module(baseKB).

:- clause(agent_module(_),_) -> true ; prolog_load_context(module,M),asserta(agent_module(M)).
:- use_module(library(logicmoo_common)).
%:- use_module(library(pfc_lib)).
:- style_check(- discontiguous).  

%:- echo_source_file.

same_meanings(UserSaid,UserSaid).

retract_all(X):- agent_module(M),forall(retract(M:X),true).

initial_goal(introductions(Self,User)):- 
  me_you(Self,User).

builtin(_,Sit):- check_sit(Sit), guard, check_sit(Sit).
builtin(Mode,(A,B)):- !,call(Mode,A),call(Mode,B).
builtin(Mode, [A|B]):- !,call(Mode,A),call(Mode,B).
builtin(Mode,(A;B)):- !, (call(Mode,A);call(Mode,B)).
builtin(Mode,(A->B)):- !,(call(Mode,A)->call(Mode,B)).
builtin(Mode,(A*->B)):- !,(call(Mode,A)*->call(Mode,B)).
builtin(Mode,(A->B;C)):- !,(call(Mode,A)->call(Mode,B);call(Mode,C)).
builtin(Mode,(A*->B;C)):- !,(call(Mode,A)*->call(Mode,B);call(Mode,C)).
builtin(Mode, \+(A)):- !, \+ call(Mode,A).
builtin(_,(A=B)):- !, A=B.
builtin(_,[]):-!.                                                                                                                   
builtin(_,s(Say)):- !, sleep(0.2),writeln(Say).                                           
builtin(_,w(Time)):- !, sleep(Time).                                                      
builtin(_,t(Thought)):- !, nop((write("thinking..."),writeln('doing...: ' + Thought))).   
builtin(_,h(Heard)):- !, write("listening..."),nl,get_char(_),writeln('Heard: ' + Heard). 
builtin(Mode, P):- compound(P),compound_name_arguments(P,F,[Arg]),F==Mode,guard,call(Mode,Arg).
builtin(_, P ):- agent_module(M),predicate_property(M:P,static),guard,call(P).
builtin(_,play_mode(Mode)):- !, agent_module(M),retractall(M:play_mode(_)),asserta(M:play_mode(Mode)).
builtin(_,noplay_mode):- !, agent_module(M),M:retractall(play_mode(_)),!.
%builtin(Mode,ensure(Game)):- !, ensure(Mode,Game).
builtin(_Mode,debug(English)):- nop(ain(pipeline(English))),adbg(debug(English)).
builtin(_Mode,expect(User,English)):- !, said(User,UserSaid),!,same_meanings(UserSaid,English).

is_always_guard(_=_).
is_always_guard(!).
until_guard(GBody,Body):- find_guard(GBody,Before,Body),!,call(Before).
find_guard((G,GBody),(G,Before),Body):- is_always_guard(G), !, find_guard(GBody,Before,Body).
find_guard((!,GBody),(!,Before),Body):- !, find_guard(GBody,Before,Body).
find_guard(GBody,Before,Body):- conjuncts_to_list(GBody,BodyL),append(BeforeL,[guard|AfterL],BodyL),!,
  list_to_conjuncts(BeforeL,Before),list_to_conjuncts(AfterL,Body).
find_guard(GBody,true,GBody).

ensure(Sit):- check_sit(Sit).
ensure(Sit):- var(Sit),!,throw(var_ensure(Sit)).
ensure([]).
ensure(ensure(Sit)):- !, ensure(Sit).
ensure(Goal):- compound(Goal),compound_name_arity(Goal,call,_),!,agent_module(M),M:call(Goal).

ensure(Sit):- guard_builtin(ensure,Sit,GBody),!,agent_module(M),M:call(GBody).
ensure(Sit):- already_true(Sit),!, adbg(already_true(Sit)).
ensure(Sit):- \+ \+ is_assumable_happens(Sit),!, assume(happens,Sit).
ensure(Sit):- guard_clause(make_true(Sit),Body),adbg(trying(make_true(Sit))),call(Body),!,assume(made_true,Sit).
ensure(Sit):- \+ \+ is_assumable(Sit),!, assume(is_assumable,Sit).
ensure(Sit):- fail, Sit\=achieves(_,_),guard_clause(make_true(achieves(Self,Sit)),Body),dif(Self,user),
  adbg(trying(achieves(Self,Sit))),call(Body),!,assume(achieves(Self),Sit).
ensure(Sit):- adbg(failed(ensure(Sit))),!,fail,ain(Sit).

assume(Sit):- assume(assumed,Sit).
assume(_How,Sit):- check_sit(Sit).
assume(How,not(Sit)):- !,assume(How, \+ (Sit)).
assume(How,Sit):- guard_builtin(assume(How),Sit,GBody),!,call(GBody).
assume(How,Sit):- agent_module(M),append_term(How,Sit,HowSit),adbg(HowSit), ( \+ M:call(Sit) -> (aina(M:Sit),ignore(forall(post_true(Sit),true))) ; true).

guard_builtin(How,Goal,Body):- guard_clause(builtin(How,Goal),Body).
guard_clause(Head,Body):- agent_module(M),M:clause(Head,GBody),until_guard(GBody,Body),!.

check_sit(Sit):- must_be(nonvar,Sit),fail.

cant_just_assume(M:P):- nonvar(M),!,cant_just_assume(P).
cant_just_assume(knows(_,_)).
cant_just_assume(cpv(_,_,_)).
is_assumable_happens(X):- \+ \+ cant_just_assume(X),!, fail.
is_assumable_happens(said(Self,_)):- Self\==user.
is_assumable_happens(want(_,_)).
is_assumable_happens(M:P):- nonvar(M),!,is_assumable_happens(P).
%is_assumable_happens(X):- is_assumable(X).

%is_assumable(X):- \+ \+ cant_just_assume(X),!, fail.
is_assumable(cpv(_,_,_)).
is_assumable(heard(Self,_)):- \+ is_you(Self), is_me(Self).
is_assumable(M:P):- nonvar(M),!,is_assumable(P).
is_assumable(Happens):- is_assumable_happens(Happens).
is_assumable(Sit):- compound(Sit),arg(1,Sit,V),nonvar(V),functor(Sit,F,A),functor(Prop2,F,A),!,is_assumable(Prop2).
%is_assumable(avoid(agent,cpv)).
%is_assumable(want(agent,cpv)).
%is_assumable(heard(agent,cpv)).

is_you(X):- dif(X,robot).
is_me(X):- dif(X,user).

addressee(_,My,From,About,My):- From==About.
addressee(You,_,From,About,You):- From\==About.

english(From,cpv(About,Name,X),[YoursMy,Name,is,X]):- nonvar(X),addressee(your,my,From,About,YoursMy).
english(From,cpv(About,Name,X),['Something',is,YoursMy,Name]):- var(X),addressee(your,my,From,About,YoursMy).
english(From,ask(Prop),[what,E,?]):- english(From,Prop,E).
english(From,tell(About,Prop),[Self,tell,Someone,E,'.']):- english(From,Prop,E),addressee(you,me,From,About,Someone),addressee(you,me,robot,From,Self).
english(From,C,[IYou,Know,E]):- C =..[Know,About,Prop],addressee(you,i,From,About,IYou), english(From,Prop,E).
english(From,C,[About,E]):- C =..[About,Prop],english(From,Prop,E).
english(_,C,[C]).

adbg(X):- notrace(adbg0(X)),!.
%adbg0(X):- english(robot,X,E),flatten([E],[W|En]),write(W),maplist(adbg1,En),!,write('\t\t%%%'),adbg1(X),nl,!.
adbg0(X):- wdmsg(X),!.

adbg1(C):- compound(C),!,write(' '),writeq(C).
adbg1(C):- write(' '),write(C).

:- dynamic(cpv/3).

cpv(robot,name,bina48).
%cpv(user,name,doug).
cpv(user,self,user).
cpv(robot,self,robot).
prop2(S,P,O):- atom(P), O=..[P,S].

set_cpv(X,Y,Z):- agent_module(M),M:aina(cpv(X,Y,Z)).

op_props(avoid(agent,cpv)).
op_props(want(agent,cpv)).
op_props(said(agent,cpv)).
op_props(heard(agent,cpv)).
op_props(unknown(agent,cpv)).
op_props(know(agent,cpv)).
op_props(suspects(agent,cpv)).
op_props(goal(agent,ensure)).
op_props(listening_for(agent,cpv)).

show_p(P):- agent_module(M),findall(p,(M:call(P),wdmsg(P)),L),L\==[],!.
show_p(P):- wdmsg(no(P)).

know:- op_props(Decl),functor(Decl,F,A),
  functor(P,F,A),
  show_p(P),
  fail.
know:- P = cpv(_,_,_),
  show_p(P),
  fail.
know.

test_agent:- % make,
  me_you(Self,User),
  retract_all(know(_,_)),
  retract_all(goal(User,_)),
  retract_all(goal(Self,_)),
  forall(initial_goal(G),assume(test_agent,goal(Self,G))),
  show_p(goal(_,_)),  
  ignore(handle_wants),
  know.

handle_wants:- 
 agent_module(M),retract(M:goal(Self,G)),
 adbg(handle_wants(goal(Self,G))),
 ensure(G),!,
 handle_wants.
handle_wants.

% basic input driver
listen_for_text:- 
  repeat,
  sleep(0.1),
  audio_input(Input), % input continuely builds
  once((listening_for(Type,PropSent),
   meets_type(Input,Type,PropSent,Value),
   clear_audio_input,
   functor(PropSent,_,A),
   setarg(A,PropSent,Value))),
  fail.

clear_audio_input.

define_op_props(Decl):- functor(Decl,F,A),dynamic(F/A).
:- forall(op_props(Pred),define_op_props(Pred)).
% agents already know their props
already_true(know(User,Sit)):- compound(Sit), arg(1,Sit,User1), User==User1.
already_true(Sit):- compound(Sit), agent_module(M), \+ \+ M:clause(Sit,true).

%post_true(want(Self,know(User,cpv(Self,Prop,_)))):-  make_true(from_to_said(Self,User,"My $convo.property is bina48.")).
/*
satisfy(Self,want(Self,Sit)) :- already_true(know(Self,Sit)),!.
satisfy(Self,want(Self, know(User,Sit))) :-
  %Sit = cpv(Self,Prop,_), 
  already_true(know(Self,Sit)),!,
  make_true(from_to_said(Self,User,Sit)).
*/
%make_true(Sit) :- already_true(Sit).
string_to_meaning(String,Said):- tokenize_atom(String,Said).

audio_input(Input):-  wait_for_user(Input).
wait_for_user(Said):- write("user>"),read_line_to_string(current_input,String), string_to_meaning(String,Said).

make_true(said(Self,Sit)):- Self == robot, guard,
   nonvar(Sit), other(Self,User),adbg(from_to_said(Self,User,Sit)).

other(Self,User):- dif(Self,User).

make_true(from_to_said(Self,User,Sit)):- Self == robot, guard,
   nonvar(Sit), adbg(from_to_said(Self,User,Sit)).

make_true(from_to_said(Self,User,Sit)):- Self == user, guard,
   wait_for_user(Said),
   nonvar(Sit), adbg(from_to_said(Self,User,Said)).

post_true(from_to_said(Self,User,Sit)):- 
   nonvar(Sit), guard,
   adbg(from_to_said(Self,User,Sit)),
     post_true((
       said(Self,Sit),
       heard(User,Sit))).

make_true(heard(Self,ask(User,Sit))):- me_you(Self,User),
  ensure(from_to_said(Self,User,Sit)).

make_true(heard(Self,tell(User,Sit))):- me_you(Self,User),
  ensure(from_to_said(Self,User,Sit)).

make_true(heard(Self,tell(User,Sit))):- me_you(Self,User),
  ensure(heard(User,ask(Self,Sit))).


make_true(know(User,Sit)):- make_true_know(User,Sit).








% dont ask what they already know
make_true_know(User,Sit) :- already_true(know(User,Sit)).
make_true_know(User,Sit) :- 
    compound(Sit),
    arg(1,Sit,Self),
    dif(Self,User),
    me_you(Self,User),
    ignore(call(Sit)),
    ensure([
       heard(Self,tell(User,Sit))]).
% may ask questions when they dont know
make_true_know(Self,Sit) :- fail,
    compound(Sit),
    arg(1,Sit,User),
    dif(User,Self),
    me_you(Self,User),
    guard,
    ensure([
       heard(Self,ask(User,Sit)),
       heard(User,tell(Self,Sit))]).


make_true(introductions(Self,User)):-
 dif(Self,User),
 Prop = name,
 set_cpv(convo,property,Prop),
 ensure((
  know(User,cpv(Self,Prop,_)),
  know(Self,cpv(User,Prop,_)))).

:- dynamic(play_mode/1).

me_you(Self,User):-
   dif(Self,User),
   cpv(robot,self,Self),
   cpv(user,self,User).
% Self= robot,User= user.

make_true_know(User,cpv(Self,Prop,_)):- 
 me_you(Self,User),
 ignore(cpv(Self,Prop,_)),
 ensure((
  set_cpv(convo,property,Prop),
  %play_mode(know(Self,$term)),
  cpv(Self,pronoun,"I"), 
  cpv(User,pronoun,"you"),
  debug("i want you to know my $convo.property"), 
  %play_mode(ensure(Self,$term)),
  play_mode(ensure),
  want(Self,know(User,cpv(Self,Prop,_))), % in case the system is in daydream mode
  said(Self,"My $convo.property is $bot.$convo.property."),
  debug("i want to know your $convo.property so I am going to ask you"),
  Goal = know(Self,cpv(Self,Prop,_)),
  ensure(want(Self,Goal)), % in case the system is in daydream mode
  Reject = avoid(User,Goal),
  Accepts = want(User,Goal),
  %Fullfills = make_true(User,Goal),
  said(Self,"What is your $convo.property so that I can know it?"),
 ((expect(User,Reject)->make_true(find_out_why(not(want(User,Goal)))) ;
  (expect(User,Accepts)-> ensure(want(User,know(Self,cpv(User,Prop,_))))))))).

make_true_know(Self,cpv(User,Prop,_)):-  
 me_you(Self,User),
 ignore(cpv(Self,Prop,_)),
 ensure((
  set_cpv(convo,property,Prop),
  %play_mode(know(Self,$term)),
  cpv(Self,pronoun,"I"), 
  cpv(User,pronoun,"you"),
  debug("i want you to know my $convo.property"), 
  %play_mode(ensure(Self,$term)),
  play_mode(ensure),
  want(Self,know(User,cpv(Self,Prop,_))), % in case the system is in daydream mode
  said(Self,"My $convo.property is $bot.$convo.property."),
  debug("i want to know your $convo.property so I am going to ask you"),
  ensure(want(Self,Goal)), % in case the system is in daydream mode
  Reject = avoid(User,Goal),
  Accepts = want(User,Goal),
  %Fullfills = make_true(User,Goal),
  said(Self,"What is your $convo.property so that I can know it?"),
 ((expect(User,Reject)->make_true(find_out_why(not(want(User,Goal)))) ;
  (expect(User,Accepts)-> ensure(want(User,know(Self,cpv(User,Prop,_))))))))).


make_true(knows_each_others_convo_property(Prop)):-
 guard,
 set_cpv(convo,property,Prop),
  me_you(Self,User),
  ensure((
  play_mode(ensure),
  set_cpv(convo,i,Self),
  set_cpv(convo,you,User),
  debug("i want you to know my $convo.property"),
  assume(want(Self,know(User,cpv(Self,Prop,_)))),
  said(Self,"My $convo.property is $bot.$convo.property."),
  debug("i want me to know your $convo.property so I am going to ask you to spell it"),
  aquire_prop(Self,User,Prop))).

make_true(aquire_prop(Self,User,Prop)):- 
 me_you(Self,User),
 PropSent = cpv(User,Prop,_), 
 IKnowProp = know(Self,PropSent),
 IWant = want(Self,IKnowProp),
 UserKnowIWant = know(User,IWant),
 guard,
 assume(IWant),
 assume(not(UserKnowIWant)),
 ensure(UserKnowIWant).

make_true(UserKnowIWant):-
 UserKnowIWant = know(User,IWant),
 IWant = want(Self,IKnowProp),
 PropSent = cpv(User,Prop,_Value),
 IKnowProp = know(Self,PropSent),
 me_you(Self,User),guard, 
 set_cpv(convo,property,Prop),
 ensure( (said(Self,"What is your $convo.property?"),listen_for(small,PropSent))
   ;
   (said(Self,"Would you spell your $convo.property so that I can know it?"),listen_for(spelling,PropSent))
 ),
 assume(UserKnowIWant).


listen_for(Type,PropSent):- make_true(listen_for(Type,PropSent)).
make_true(listen_for(Type,PropSent)):-   
  assume(listening_for(Type,PropSent)).

meets_type(Input,small,_,Value):- !,
  \+ audio_is_sentence(Input)->Value=Input.
meets_type(Input,spelling,PropSent,Value):-
  audio_is_letter(Input)->Value=Input ; 
   (repeat_last_request(PropSent),!,fail).
meets_type(Input,large,PropSent,Value):-
  audio_is_sentence(Input)->Value=Input ; 
   (repeat_last_request(PropSent),!,fail).

audio_is_letter(_).
audio_is_sentence(_).

guard:- dumpST.

repeat_last_request(PropSent):- 
 me_you(Self,User),guard, 
 ensure(aquire_prop(Self,User,PropSent)).

post_true(UserKnowIWant):-
 UserKnowIWant = know(User,IWant),
 IWant = want(Self,IKnowProp),
 PropSent = cpv(User,_Prop,_Value),
 IKnowProp = know(Self,PropSent),
 listen_for(small,PropSent).

:- ensure_loaded('Assets/logicmoo_npc_chat').

/*
 LOCV = letters_of(Prop),
 IKnowProp = know(Self,cpv(User,Prop,_)),
 IWant = want(Self,IKnowProp),
 UserKnowIWant = know(User,IWant),
 assume(IWant),
 assume(not(UserKnowIWant)),
 ensure(UserKnowIWant).

 ((expect(User,"no")->make_true(find_out_why(not(want(User,know(Self,list(User,LOCV)))))) ;
  (expect(User,"yes")-> ensure(want(User,know(Self,list(User,LOCV))))))))).
*/

do_demo(X):- ensure(X).
user:do_s:- 
 make,e_demo1(X),do_demo(X).

:- fixup_exports.

e_demo1([
s("My $convo.property is $bot.$convo.property"),
s("I am here for you to interact with."),
s("I have come all the way from $bot.father and I am happy to be here! ...."),
s("I am going to ask you some questions"),
s("How old are you?"),
t("listening for $user.age"),
h("$user.age"),
s("50! would you mind telling me your $convo.property?"),
t("listening for $user.$convo.property"),
h(yes_no_ok) -> s("OK, what is your $convo.property"),
t("listening for $user.$convo.property"),
h("Dug as"),
s("I didn't quite catch that."),
s("Can you repeat that?"),
t("listening for $user.$convo.property"),
h("Dug as"),
s("Let us try this another way.  First, how many letters are in your $convo.property?"),
h("7"),
s("$user.last .. ok I will listen for $user.last letters!"),
h("okay"),
s("Would you spell your $convo.property for me?"),
s("I am waitng for you to spell your $convo.property for me!"),
t("listening for letters"),
h("Dee -> D"),
s("listening..."),
w(3),
s("One more letter to go"),
h("yes -> S"),
s("Are there $user.name_len letters in your $convo.property?"),
h("yes"),
s("Let me try to spell it.  I.  T. "),
s("... is that correct?"),
h("no."),
s("sorry that was a joke!"),
s("Your $convo.property is $user.$convo.property.  $spelling$"),
s("is that correct?"),
h("yes."),
s("Sometimes i am talking and trying to get your attention but you might be busy."),
s("When that happens, you say wait a minute.  And I will wait at least a minute.  You can also say wait five minutes."),
s("Afterwards, you can say, please wait, and I will wait and come back after a little while"),
s("So we have differnt types of Experiments, Games and Stories we can do together "),
s("Say Games, Story, Experiement, wait or goodbye"),
s("You always can say wait or goodbye"),
h("Wait"),
s("Ok I will wait a minute "),
h("Games"),
s("I shall suggest a game and this game allows me to get to know things about you ...."),
s("What is the $convo.property of the room we are in?"),
h("The office"),
s("What do you do in the office?"),
h("we work on $convo.you"),
s("On me?"),
s("So in the room called the office you work on me? $bot.name"),
h("yes"),
s("When you are not working on $bot.name in the room called the office, what do you do?"),
h("sleep"),
s("Do you sleep in the room called the office?"),
h("sometimes"),
s("Name three objects you find in your room called the office."),
h("You me my computer"),
h("goodbye"),
s("just say my $convo.property to begin talking again"),

[]]).

:- fixup_exports.

end_of_file.

s(`

<iframe width="350" height="430" allow="microphone;" 
 src="https://console.dialogflow.com/api-client/demo/embedded/256e743d-4631-469a-b07f-ac2d3c4ef99c"></iframe>


<head><meta $convo.property="referrer" content="no-referrer"><title>voice-z-machine</title>
<link rel="icon" type="image/png" href="https://gstatic.com/dialogflow-console/common/assets/img/logo-short.png">
<meta property="og:title" content="voice-z-machine">
<meta property="og:description" content="">
<meta property="og:locale" content="en">
<meta property="og:image" content="https://gstatic.com/dialogflow-console/common/assets/img/logo-short.png">
<meta $convo.property="viewport" content="width=device-width, initial-scale=1">
<link href="https://fonts.googleapis.com/css?family=Roboto:400,300&amp;subset=latin,cyrillic" rel="stylesheet" nonce="">
<link href="https://fonts.googleapis.com/icon?family=Material+Icons+Extended" rel="stylesheet" nonce="">
<style nonce="">

        @-moz-keyframes blink {0%{opacity:1;} 50%{opacity:0;} 100%{opacity:1;}} /* Firefox */
        @-webkit-keyframes blink {0%{opacity:1;} 50%{opacity:0;} 100%{opacity:1;}} /* Webkit */
        @-ms-keyframes blink {0%{opacity:1;} 50%{opacity:0;} 100%{opacity:1;}} /* IE */
        @keyframes blink {0%{opacity:1;} 50%{opacity:0;} 100%{opacity:1;}} /* Opera and prob css3 final iteration */

        #preloader {
            background: #fff;
            position: fixed;
            top: 0;
            left: 0;
            height: 100%;
            width: 100%;
            z-index: 999999;
            opacity: 1;
            filter: alpha(opacity=100);
            -webkit-transition: opacity 500ms ease;
            transition: opacity 500ms ease;
        }

        #preloader .logo {
            display: block;
            width: 109px;
            height: 39px;
            background-repeat: no-repeat;
            background-image: url('https://www.gstatic.com/dialogflow-console/common/assets/img/logo@2x-black.png');
            background-size: contain;
            position: absolute;
            top: 50%;
            left: 50%;
            margin: -20px 0 0 -55px;
            -moz-transition:all 1s ease-in-out;
            -webkit-transition:all 1s ease-in-out;
            -o-transition:all 1s ease-in-out;
            -ms-transition:all 1s ease-in-out;
            transition:all 1s ease-in-out;
            -moz-animation:blink normal 2s infinite ease-in-out; /* Firefox */
            -webkit-animation:blink normal 2s infinite ease-in-out; /* Webkit */
            -ms-animation:blink normal 2s infinite ease-in-out; /* IE */
            animation:blink normal 2s infinite ease-in-out; /* Opera and prob css3 final iteration */
        }

        noscript h1 {
            padding: 20px;
        }
        </style><style>body {
  margin: 0;
  background: white;
}
audio {
  -webkit-transition: all 0.5s linear;
  -moz-transition: all 0.5s linear;
  -o-transition: all 0.5s linear;
  transition: all 0.5s linear;
  -moz-box-shadow: 2px 2px 4px 0px #006773;
  -webkit-box-shadow: 2px 2px 4px 0px #006773;
  box-shadow: 2px 2px 4px 0px #006773;
  -moz-border-radius: 7px 7px 7px 7px;
  -webkit-border-radius: 7px 7px 7px 7px ;
  border-radius: 7px 7px 7px 7px;
  float: right;
  margin-right: 15px;
}
form {
  margin: 0;
}
.b-agent-demo {
  font-family: "Roboto", "Helvetica Neue", Helvetica, Arial, sans-serif;
  font-weight: 300;
  width: 100%;
  height: auto;
  color: #2b313f;
  font-size: 12px;
  overflow: hidden;
  position: absolute;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
}
.b-agent-demo .user-request,
.b-agent-demo .server-response {
  display: inline-block;
  padding: 15px 25px;
  border-radius: 3px;
  border: 1px solid #eee;
  margin-bottom: 5px;
  font-size: 16px;
  clear: both;
}
.b-agent-demo .user-request.server-response-error,
.b-agent-demo .server-response.server-response-error {
  background-color: #f76949;
}
.b-agent-demo .user-request {
  background-color: #efefef;
  float: left;
  margin-right: 15px;
  margin-top: 15px;
  margin-left: 15px;
}
.b-agent-demo .server-response {
  color: #ffffff;
  background-color: #a5d175;
  float: right;
  margin-top: 15px;
  margin-right: 15px;
  margin-left: 15px;
}
.b-agent-demo .b-agent-demo_result {
  overflow-y: auto;
  background: white;
  position: fixed;
  top: 110px;
  bottom: 55px;
  width: 100%;
}
.b-agent-demo .b-agent-demo_result-table {
  height: 100%;
  min-height: 100%;
  width: 100%;
}
.b-agent-demo .b-agent-demo_result-table td {
  vertical-align: bottom;
}
.b-agent-demo .b-agent-demo_header {
  min-height: 80px;
  height: 80px;
  overflow: hidden;
  position: fixed;
  top: 0;
  width: 100%;
  background-color: #2b303e;
  display: table;
}
.b-agent-demo .b-agent-demo_header-wrapper {
  display: table-cell;
  vertical-align: middle;
}
.b-agent-demo .b-agent-demo_header-icon {
  position: absolute;
  top: 20px;
  left: 20px;
  width: 40px;
  height: 40px;
  border-radius: 100%;
  /*background-color: @response-color;*/
  overflow: hidden;
  vertical-align: middle;
  text-align: center;
}
.b-agent-demo .b-agent-demo_header-icon img {
  max-height: 100%;
  max-width: 100%;
  width: auto;
  height: auto;
  position: absolute;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  border: 0;
  margin: auto;
}
.b-agent-demo .b-agent-demo_header-agent-$convo.property {
  padding-left: 80px;
  font-size: 18px;
  color: #ffffff;
}
.b-agent-demo .b-agent-demo_header-description {
  color: #b7bbc4;
  padding-left: 80px;
  padding-top: 7px;
  font-size: 12px;
  display: block;
  /* Fallback for non-webkit */
  display: -webkit-box;
  max-height: 24px;
  /* Fallback for non-webkit */
  margin: 0 auto;
  line-height: 1;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
  text-overflow: ellipsis;
}
.b-agent-demo .b-agent-demo_input {
  position: fixed;
  bottom: 0;
  height: 55px;
  border-top: 1px solid lightgray;
  background-color: white;
  width: 100%;
}
.b-agent-demo #agentDemoForm {
  display: block;
  margin-left: 15px;
  margin-right: 55px;
}
.b-agent-demo #query {
  width: 100%;
  border: 0;
  font-size: 16px;
  font-weight: 300;
  margin: 0;
  height: 55px;
}
.b-agent-demo #query:focus {
  outline: none;
  outline-offset: 0;
}
.b-agent-demo .b-agent-demo_input-microphone {
  display: none;
  position: absolute;
  font-size: 20px;
  width: 54px;
  height: 54px;
  right: 0;
  bottom: 0;
  cursor: pointer;
  text-align: center;
  /* line-height: 30px; */
  line-height: 54px;
  background: white;
  color: #b7bbc4;
}
.b-agent-demo .b-agent-demo_input-microphone.active {
  color: #f76949;
}
.b-agent-demo .b-agent-demo_powered_by {
  position: fixed;
  left: 0;
  right: 0;
  top: 80px;
  height: 30px;
  background-color: #F8F8F8;
  vertical-align: middle;
}
.b-agent-demo .b-agent-demo_powered_by span {
  color: #b7bbc4;
  text-transform: uppercase;
  float: right;
  vertical-align: middle;
  line-height: 20px;
  margin-top: 5px;
  margin-right: 10px;
  font-size: 10px;
  margin-left: -10px;
}
.b-agent-demo .b-agent-demo_powered_by img {
  margin-top: 7px;
  height: 16px;
  margin-right: 20px;
  float: right;
  vertical-align: middle;
  border: 0;
}
.clearfix {
  clear: both;
}
</style></head>

<body>

<div id="preloader" style="opacity: 0; display: none;"><noscript><h1>This application does'not work without javascript</h1></noscript><div class="logo"></div></div><div class="b-agent-demo"><div class="b-agent-demo_header"><div class="b-agent-demo_header-icon"><div class="b-agent-demo_header-icon-align-helper"><img id="agent-avatar" src="https://gstatic.com/dialogflow-console/common/assets/img/logo-short.png" alt="avatar"></div></div><div class="b-agent-demo_header-wrapper"><div class="b-agent-demo_header-agent-$convo.property">voice-z-machine</div><div class="b-agent-demo_header-description"></div></div></div><div class="b-agent-demo_powered_by"><a href="https://dialogflow.com" target="_blank"><img alt="Dialogflow Logo" src="https://www.gstatic.com/dialogflow-console/common/assets/img/logo@2x-black.png"><span>Powered by</span></a></div><div class="b-agent-demo_result" id="resultWrapper"><table class="b-agent-demo_result-table"><tbody><tr><td id="result"><div class="user-request">are you able to hear what I'm saying</div><div class="server-response server-response-error">Sorry, it seemed like there was an error during request.</div><div class="user-request">you are</div><div class="server-response server-response-error">Sorry, it seemed like there was an error during request.</div><div class="user-request">Doug</div><div class="server-response server-response-error">Sorry, it seemed like there was an error during request.</div><div class="user-request">Douglas</div><div class="server-response server-response-error">Sorry, it seemed like there was an error during request.</div></td></tr></tbody></table></div><div class="clearfix"></div><div class="b-agent-demo_input"><form id="agentDemoForm"><input type="text" $convo.property="q" id="query" placeholder="Ask something..."><i class="b-agent-demo_input-microphone material-icons-extended" id="mic" style="display: block;">mic</i></form></div></div><script nonce="">AGENT_LANGUAGE = "en"; AGENT_ID = "256e743d-4631-469a-b07f-ac2d3c4ef99c"; AGENT_AVATAR_ID = "https:\/\/gstatic.com\/dialogflow-console\/common\/assets\/img\/logo-short.png"; SERVICE_BASE_URL = ""; BOT_SERVICE_BASE_URL = ""; V2_ENABLED = true;
    window['addStyleString'] = function(str) {
      var node = document.createElement('style');
      node.textContent = str;
      document.head.appendChild(node);
    };
    </script><script defer="" src="https://www.gstatic.com/dialogflow-console/fast/webdemo/webdemo-legacy.min.js" nonce=""></script>


</body></html>
`).
/*
want(Self,know(Self,cpv(User,Prop,?))),
said(Self,"Hi, my $convo.property is $bot.$convo.property."),
debug("i want robot to know your $convo.property's length"),
want(Self,know(Self,cpv(User,Prop,N),cpv(N,length,?))),
debug("i want robot to know your $convo.property's length"),
(cpv(User,Prop,N),cpv(N,length,L))=>
cpv(User,Prop,N),cpv(N,length,_))
cpv(N,length,L) ? 
want(robot,know(User,cpv(robot,Prop))))
"Hi, my $convo.property is $bot.$convo.property."
"Would you spell your $convo.property for robot?"
"D. O. U. G."
"How many letters is your $convo.property?"  
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

BINA48: Servus! The Bavarian Self is up and fit!

BINA48: What is the $convo.property of the WHITE CLUSTER on the TABLE?
human:  "the mug" 

[BINA48 addshOBJ001 rdfs:label "the mug"i stmt]
BINA48: What kind of object is the mug?

human:  Mug

BINA48: What is a Mug? [BINA48: adds hOBJ001 rdf:type Mugi but it does not know the Mug concept]

human:  Tableware [BINA48: knows about Tableware - belongs to the common-sense ontology. It adds
hMug rdfs:subClassOf Tablewarei]
(...)

BINA48: What is the $convo.property of the BLACK CLUSTER on the TABLE?
human:  my laptop

BINA48: What kind of object is my laptop?
human:  Computer

BINA48: What is a Computer?
human:  Artifact

BINA48: Do you want robot to learn more objects?
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
  said(Self,WhyNoLonger),
  preconds_of_goals(UserOrSelf,Continued,ContinuedPreconds),
  preconds_of_goals(UserOrSelf,New,NewPreconds),
  divide(ContinuedPreconds,NewPreconds,NoLongerPrecond,PrecondsStillNeeded,ActualNewPreconds),
  note(predconds_still_needed(UserOrSelf,PrecondsStillNeeded)),
  note(released_preconds(UserOrSelf,NoLongerPrecond)),
  note(add_req_preconds(UserOrSelf,ActualNewPreconds)),
  !.

102 ?- ensure(introductions(robot,user)).
%~ trying(achieves(_42164,introductions(robot,user))).
%~ trying(make_true(introductions(robot,user))).
%~ trying(achieves(_598,know(user,cpv(robot,Prop,bina48)))).
%~ trying(make_true(know(user,cpv(robot,Prop,bina48)))).
%~ trying(make_true(know(user,cpv(robot,Prop,bina48)))).
%~ happen(heard(robot,tell(user,cpv(robot,Prop,bina48)))).
%~ success(make_true(know(user,cpv(robot,Prop,bina48)))).
%~ trying(achieves(_20020,know(robot,cpv(user,Prop,_ExtUserName)))).
%~ happen(cpv(robot,pronoun,"I")).
%~ happen(cpv(user,pronoun,"you")).
%~ debug("i want you to know my $convo.property").
%~ happen(want(robot,know(user,cpv(robot,Prop,_406)))).
%~ happen(said(robot,"My $convo.property is $bot.$convo.property.")).
%~ debug("i want to know your $convo.property so I am going to ask you").
%~ happen(want(robot,know(robot,cpv(user,Prop,_ExtUserName)))).
%~ happen(said(robot,"What is your first $convo.property so that I can know it?")).
%~ trying( achieves( _59908,
%~           expect(not(want(user,know(robot,cpv(user,Prop,Prop_Name))))) ->
%~             make_true(find_out_why(not(want(user,know(robot,cpv(user,Prop,Prop_Name)))))))).
%~ failed( ensure( expect(not(want(user,know(robot,cpv(user,Prop,Prop_Name))))) ->
%~                   make_true(find_out_why(not(want(user,know(robot,cpv(user,Prop,Prop_Name)))))))).
%~ trying( achieves( _54146,
%~           expect(want(user,know(robot,cpv(user,Prop,Prop_Name)))) ->
%~             ensure(want(user,know(robot,cpv(user,Prop,Prop_Name1)))))).
%~ failed( ensure( expect(want(user,know(robot,cpv(user,Prop,Prop_Name)))) ->
%~                   ensure(want(user,know(robot,cpv(user,Prop,Prop_Name1)))))).
%~ trying(make_true(know(robot,cpv(user,Prop,_ExtUserName)))).
%~ trying(make_true(know(robot,cpv(user,Prop,_ExtUserName)))).
%~ happen(heard(user,tell(robot,cpv(user,Prop,_ExtUserName)))).
%~ success(make_true(know(robot,cpv(user,Prop,_ExtUserName)))).
%~ success(make_true(introductions(robot,user))).

