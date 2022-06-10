
%:-module(agent,[]).

:- use_module(library(pfc_lib)).
:- style_check(- discontiguous).  

same_meanings(UserSaid,UserSaid).

initial_goal(introductions(Agent2,Agent1)):- 
   prop(robot,self,Agent2),
   prop(user,self,Agent1).

builtin(Mode,(A,B)):- !,call(Mode,A),call(Mode,B).
builtin(Mode,[A|B]):- !,call(Mode,A),call(Mode,B).
builtin(Mode,(A;B)):- !,(call(Mode,A);call(Mode,B)).
builtin(Mode,(A->B)):- !,(call(Mode,A)->call(Mode,B)).
builtin(Mode,(A*->B)):- !,(call(Mode,A)*->call(Mode,B)).
builtin(Mode,(A->B;C)):- !,(call(Mode,A)->call(Mode,B);call(Mode,C)).
builtin(Mode,(A*->B;C)):- !,(call(Mode,A)*->call(Mode,B);call(Mode,C)).
builtin(Mode, \+(A)):- !, \+ call(Mode,A).
builtin(_,(A=B)):- !, A=B.
% builtin(_, P ):- predicate_property(P,static),call(P).
builtin(_,play_mode(Mode)):- !, retractall(play_mode(_)),asserta(play_mode(Mode)).
builtin(_,noplay_mode):- !, retractall(play_mode(_)),!.
%builtin(Mode,ensure(Game)):- !, ensure(Mode,Game).
builtin(_Mode,debug(English)):- nop(ain(pipeline(English))),adbg(debug(English)).
builtin(_Mode,expect(Agent2,English)):- !, said(Agent2,UserSaid),!,same_meanings(UserSaid,English).

ensure(Sit):- var(Sit),!,throw(var_ensure(Sit)).
ensure([]).
ensure(ensure(Sit)):- !, ensure(Sit).
ensure(Sit):- clause(builtin(ensure,Sit),(A,Body)),call(A),!,call(Body).
ensure(Sit):- already_true(Sit),!, adbg(already_true(Sit)).
ensure(Sit):- \+ \+ is_assumable_happens(Sit),!, update_true(happens,Sit).
ensure(Sit):- \+ \+ is_assumable(Sit),!, update_true(is_assumable,Sit).
ensure(Sit):- fail, Sit\=achieves(_,_),clause(make_true(achieves(Agent1,Sit)),Body),dif(Agent1,user),
  adbg(trying(achieves(Agent1,Sit))),call(Body),!,update_true(achieves(Agent1),Sit).
ensure(Sit):- clause(make_true(Sit),Body),adbg(trying(make_true(Sit))),call(Body),!,update_true(made_true,Sit).
ensure(Sit):- adbg(failed(ensure(Sit))),!,fail,ain(Sit).


update_true(How,Sit):- clause(builtin(update_true(How),Sit),(A,Body)),call(A),!,ignore(call(Body)).
update_true(How,Sit):- append_term(How,Sit,HowSit),adbg(HowSit), ( \+ call_u(Sit) -> (ain(Sit),ignore(forall(post_true(Sit),true))) ; true).

cant_assume(knows(_,_)).
cant_assume(prop(_,_,_)).
is_assumable_happens(X):- \+ \+ cant_assume(X),!, fail.

is_assumable_happens(said(Robot,_)):- Robot\==user.
%is_assumable_happens(X):- is_assumable(X).

%is_assumable(X):- \+ \+ cant_assume(X),!, fail.
is_assumable(prop(_,_,_)).
is_assumable(want(_,_)).
%is_assumable(heard(User,_)):- robot\==User.
is_assumable(Sit):-compound(Sit),arg(1,Sit,V),nonvar(V),functor(Sit,F,A),functor(Prop2,F,A),!,is_assumable(Prop2).
is_assumable(avoid(agent,prop)).
is_assumable(want(agent,prop)).
%is_assumable(heard(agent,prop)).

addressee(_,My,From,About,My):- From==About.
addressee(You,_,From,About,You):- From\==About.

english(From,prop(About,Name,X),[YoursMy,Name,is,X]):- nonvar(X),addressee(your,my,From,About,YoursMy).
english(From,prop(About,Name,X),['Something',is,YoursMy,Name]):- var(X),addressee(your,my,From,About,YoursMy).
english(From,ask(Prop),[what,E,?]):- english(From,Prop,E).
english(From,tell(About,Prop),[Robot,tell,Someone,E,'.']):- english(From,Prop,E),addressee(you,me,From,About,Someone),addressee(you,me,robot,From,Robot).
english(From,C,[IYou,Know,E]):- C =..[Know,About,Prop],addressee(you,i,From,About,IYou), english(From,Prop,E).
english(From,C,[About,E]):- C =..[About,Prop],english(From,Prop,E).
english(_,C,[C]).

adbg(X):- notrace(adbg0(X)).
%adbg0(X):- english(robot,X,E),flatten([E],[W|En]),write(W),maplist(adbg1,En),!,write('\t\t%%%'),adbg1(X),nl,!.
adbg0(X):- wdmsg(X),!.

adbg1(C):- compound(C),!,write(' '),writeq(C).
adbg1(C):- write(' '),write(C).

:- dynamic(prop/3).

prop(user,name,bina48).
%prop(robot,name,doug).
prop(user,self,user).
prop(robot,self,robot).
prop(S,P,O):- atom(P), O=..[P,S].


op_props(avoid(agent,prop)).
op_props(want(agent,prop)).
op_props(said(agent,prop)).
op_props(unknown(agent,prop)).
op_props(know(agent,prop)).
op_props(suspects(agent,prop)).


define_op_props(Decl):- functor(Decl,F,A),dynamic(F/A).
:- forall(op_props(Pred),define_op_props(Pred)).
% agents already know their props
already_true(know(Agent2,Sit)):- compound(Sit), arg(1,Sit,Agent2).
already_true(Sit):- compound(Sit), clause(Sit,true).

%post_true(want(Agent1,know(Agent2,prop(Agent1,name,_)))):-  make_true(from_to_said(Agent1,Agent2,"My name is bina48.")).
/*
satisfy(Agent1,want(Agent1,Sit)) :- already_true(know(Agent1,Sit)),!.
satisfy(Agent1,want(Agent1, know(Agent2,Sit))) :-
  %Sit = prop(Agent1,name,_),
  already_true(know(Agent1,Sit)),!,
  make_true(from_to_said(Agent1,Agent2,Sit)).
*/
%make_true(Sit) :- already_true(Sit).
string_to_meaning(String,Said):- tokenize_atom(String,Said).

wait_for_user(Said):- write("user>"),read_line_to_string(current_input,String), string_to_meaning(String,Said).

make_true(from_to_said(Agent1,Agent2,Sit)):- Agent1 == robot,
   nonvar(Sit), adbg(from_to_said(Agent1,Agent2,Sit)).

make_true(from_to_said(Agent1,Agent2,Sit)):- Agent1 == user,
   wait_for_user(Said),
   nonvar(Sit), adbg(from_to_said(Agent1,Agent2,Said)).

post_true(from_to_said(Agent1,Agent2,Sit)):- 
   nonvar(Sit),
   adbg(from_to_said(Agent1,Agent2,Sit)),
     post_true((
       said(Agent1,Sit),
       heard(Agent2,Sit))).
 
make_true(heard(Agent1,ask(Agent2,Sit))):- 
  ensure(say_from_to(Agent1,Agent2,Sit)).

make_true(heard(Agent1,tell(Agent2,Sit))):- 
  ensure(say_from_to(Agent1,Agent2,Sit)).

make_true(heard(Agent1,tell(Agent2,Sit))):-
  ensure(heard(Agent2,ask(Agent1,Sit))).


make_true(know(Agent2,Sit)):- make_true_know(Agent2,Sit).


% dont ask what they already know
make_true_know(Agent2,Sit) :- already_true(know(Agent2,Sit)).
% may ask questions when they dont know
make_true_know(Agent2,Sit) :-
    compound(Sit),
    arg(1,Sit,Agent1),
    dif(Agent1,Agent2),
    ensure([
       heard(Agent2,ask(Agent1,Sit)),
       heard(Agent1,tell(Agent2,Sit))]).


make_true(introductions(Agent1,Agent2)):-
 dif(Agent1,Agent2),
 ensure((
  know(Agent2,prop(Agent1,name,_)),
  know(Agent1,prop(Agent2,name,_)))).

:- dynamic(play_mode/1).

make_true_know(Agent1,prop(Agent2,name,_)):-  
 ensure((
  %play_mode(know(Agent1,$term)),
  prop(Agent1,pronoun,"I"), 
  prop(Agent2,pronoun,"you"),
  debug("i want you to know my name"), 
  %play_mode(ensure(Agent1,$term)),
  play_mode(ensure),
  want(Agent1,know(Agent2,prop(Agent1,name,_))), % in case the system is in daydream mode
  said(Agent1,"My name is Bina48."),
  debug("i want to know your name so I am going to ask you"),
  ensure(want(Agent1,Goal)), % in case the system is in daydream mode
  Reject = avoid(Agent2,Goal),
  Accepts = want(Agent2,Goal),
  %Fullfills = make_true(Agent2,Goal),
  said(Agent1,"What is your name so that I can know it?"),
 ((expect(Reject)->make_true(find_out_why(not(want(Agent2,Goal)))) ;
  (expect(Accepts)-> ensure(want(Agent2,know(Agent1,prop(Agent2,name,_))))))))).


make_true(knows_each_others_name):- 
 Agent1=robot,
 Agent2=user,
 ensure((
  play_mode(ensure),
  prop(Agent1,pronoun,"I"),
  prop(Agent2,pronoun,"you"),
  debug("i want you to know my name"),
  want(Agent1,know(Agent2,prop(Agent1,name,_))),
  said(Agent1,"My name is Bina48."),
  debug("i want me to know your name so I am going to ask you to spell it"),
  ensure(want(Agent1,know(Agent1,list(Agent2,letters_of_first_name,_)))),
  said(Agent1,"Would you spell your first name so that I can know it?"),
 ((expect(Agent2,"no")->make_true(find_out_why(not(want(Agent2,know(Agent1,list(Agent2,letters_of_first_name,_)))))) ;
  (expect(Agent2,"yes")-> ensure(want(Agent2,know(Agent1,list(Agent2,letters_of_first_name,_))))))))).



s("my name is professor einstein").
s("i am here for you to interact with.  I have come all the way from Hanson Robotics and I am happy to be here").
s("i am going to ask you some questions").
s("How old are you?").
s("think: listening for small phrase").
s("50").
s("50! would you mind telling me your name?").
s("think: listening for small phrase").
s("heard: Dug as").
s("I didn't quite catch that.<break time='2s'/>").
s("Can you repeat that? <break time='2s'/>").
s("Let us try this another way.  First, how many letters are in your name?<break time='2s'/>").
s("Seven .. ok I will listen for seven letters.").
s("I didn quite catch that.. would you spell your name for me?").
s("think: listening for letters").
s("heard: Dee -> D").
s("heard: Oh -> O").
s("heard: you -> U").
s("heard: Gee -> G").
s("heard: L").
s("heard: ay -> A").
s("heard: yes -> S").
s("Are there seven letters in your name?").
s("heard: yes").
s("Let me try to spell it.  I.  T. ").
s("is that correct?").
s("heard: no.").
s("sorry that was a joke!").
s("You name is Douglas.  D. O. U. G. L. A. S.").
s("is that correct?").
s("heard: yes.").
s("So we have three differnt types of activities: Experiments, Games and Stories").



end_of_file.

s(`

<iframe width="350" height="430" allow="microphone;" 
 src="https://console.dialogflow.com/api-client/demo/embedded/256e743d-4631-469a-b07f-ac2d3c4ef99c"></iframe>


<head><meta name="referrer" content="no-referrer"><title>voice-z-machine</title>
<link rel="icon" type="image/png" href="https://gstatic.com/dialogflow-console/common/assets/img/logo-short.png">
<meta property="og:title" content="voice-z-machine">
<meta property="og:description" content="">
<meta property="og:locale" content="en">
<meta property="og:image" content="https://gstatic.com/dialogflow-console/common/assets/img/logo-short.png">
<meta name="viewport" content="width=device-width, initial-scale=1">
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
.b-agent-demo .b-agent-demo_header-agent-name {
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

<div id="preloader" style="opacity: 0; display: none;"><noscript><h1>This application does'not work without javascript</h1></noscript><div class="logo"></div></div><div class="b-agent-demo"><div class="b-agent-demo_header"><div class="b-agent-demo_header-icon"><div class="b-agent-demo_header-icon-align-helper"><img id="agent-avatar" src="https://gstatic.com/dialogflow-console/common/assets/img/logo-short.png" alt="avatar"></div></div><div class="b-agent-demo_header-wrapper"><div class="b-agent-demo_header-agent-name">voice-z-machine</div><div class="b-agent-demo_header-description"></div></div></div><div class="b-agent-demo_powered_by"><a href="https://dialogflow.com" target="_blank"><img alt="Dialogflow Logo" src="https://www.gstatic.com/dialogflow-console/common/assets/img/logo@2x-black.png"><span>Powered by</span></a></div><div class="b-agent-demo_result" id="resultWrapper"><table class="b-agent-demo_result-table"><tbody><tr><td id="result"><div class="user-request">are you able to hear what I'm saying</div><div class="server-response server-response-error">Sorry, it seemed like there was an error during request.</div><div class="user-request">you are</div><div class="server-response server-response-error">Sorry, it seemed like there was an error during request.</div><div class="user-request">Doug</div><div class="server-response server-response-error">Sorry, it seemed like there was an error during request.</div><div class="user-request">Douglas</div><div class="server-response server-response-error">Sorry, it seemed like there was an error during request.</div></td></tr></tbody></table></div><div class="clearfix"></div><div class="b-agent-demo_input"><form id="agentDemoForm"><input type="text" name="q" id="query" placeholder="Ask something..."><i class="b-agent-demo_input-microphone material-icons-extended" id="mic" style="display: block;">mic</i></form></div></div><script nonce="">AGENT_LANGUAGE = "en"; AGENT_ID = "256e743d-4631-469a-b07f-ac2d3c4ef99c"; AGENT_AVATAR_ID = "https:\/\/gstatic.com\/dialogflow-console\/common\/assets\/img\/logo-short.png"; SERVICE_BASE_URL = ""; BOT_SERVICE_BASE_URL = ""; V2_ENABLED = true;
    window['addStyleString'] = function(str) {
      var node = document.createElement('style');
      node.textContent = str;
      document.head.appendChild(node);
    };
    </script><script defer="" src="https://www.gstatic.com/dialogflow-console/fast/webdemo/webdemo-legacy.min.js" nonce=""></script>


</body></html>
`).
/*
want(Agent1,know(Agent1,prop(Agent2,name,?))),
said(Agent1,"Hi, my name is Bina48."),
debug("i want robot to know your name's length"),
want(Agent1,know(Agent1,prop(Agent2,name,N),prop(N,length,?))),
debug("i want robot to know your name's length"),
(prop(Agent2,name,N),prop(N,length,L))=>
prop(Agent2,name,N),prop(N,length,_))
prop(N,length,L) ? 
want(robot,know(Agent2,prop(robot,name))))
"Hi, my name is Bina48."
"Would you spell your name for robot?"
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
  said(Agent1,WhyNoLonger),
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
%~ trying(achieves(_598,know(user,prop(robot,name,bina48)))).
%~ trying(make_true(know(user,prop(robot,name,bina48)))).
%~ trying(make_true(know(user,prop(robot,name,bina48)))).
%~ happen(heard(robot,tell(user,prop(robot,name,bina48)))).
%~ success(make_true(know(user,prop(robot,name,bina48)))).
%~ trying(achieves(_20020,know(robot,prop(user,name,_ExtUserName)))).
%~ happen(prop(robot,pronoun,"I")).
%~ happen(prop(user,pronoun,"you")).
%~ debug("i want you to know my name").
%~ happen(want(robot,know(user,prop(robot,name,_406)))).
%~ happen(said(robot,"My name is Bina48.")).
%~ debug("i want to know your name so I am going to ask you").
%~ happen(want(robot,know(robot,prop(user,name,_ExtUserName)))).
%~ happen(said(robot,"What is your first name so that I can know it?")).
%~ trying( achieves( _59908,
%~           expect(not(want(user,know(robot,prop(user,name,Prop_Name))))) ->
%~             make_true(find_out_why(not(want(user,know(robot,prop(user,name,Prop_Name)))))))).
%~ failed( ensure( expect(not(want(user,know(robot,prop(user,name,Prop_Name))))) ->
%~                   make_true(find_out_why(not(want(user,know(robot,prop(user,name,Prop_Name)))))))).
%~ trying( achieves( _54146,
%~           expect(want(user,know(robot,prop(user,name,Prop_Name)))) ->
%~             ensure(want(user,know(robot,prop(user,name,Prop_Name1)))))).
%~ failed( ensure( expect(want(user,know(robot,prop(user,name,Prop_Name)))) ->
%~                   ensure(want(user,know(robot,prop(user,name,Prop_Name1)))))).
%~ trying(make_true(know(robot,prop(user,name,_ExtUserName)))).
%~ trying(make_true(know(robot,prop(user,name,_ExtUserName)))).
%~ happen(heard(user,tell(robot,prop(user,name,_ExtUserName)))).
%~ success(make_true(know(robot,prop(user,name,_ExtUserName)))).
%~ success(make_true(introductions(robot,user))).

