% ===================================================================
% File 'logicmoo_module_aiml.pl'
% Purpose: An Implementation in CS-prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%:- style_check(-discontiguous).
%:- style_check(-atom).
%:- style_check(-string).



:-discontiguous(what/3).
:-discontiguous(response/2).
:-discontiguous(load_aiml_structure/2).
:-discontiguous(load_aiml_structure/3).

:-dynamic(flagsaved/2).

swi_flag(X,OldVal,NewVal):-var(OldVal),retract(flagsaved(X,OldVal)),
swi_flag(X,OldVal,NewVal).
swi_flag(_,4,_).

atom_codes(A,C):-name(A,C).

to_upper(Any,Any):-fail.
to_upper(Any,Any):-!.


ignore(C) :- C ; true.

format(A,B,C):-writeq(format(A,B,C)).
flush_output(A):-nl.

call_with_depth_limit(C,D,M):-C.

%reverse([], []).
%reverse([Head|Tail], Result):- reverse(Tail, Reduced),append(Reduced, [Head], Result).



   flatten([],[]).
   flatten([[]|L],L).
   flatten([X|L1],[X|L2]) :- atomic(X), flatten(L1,L2).
   flatten([X|L1],L4) :- flatten(X,L2),
                         flatten(L1,L3),
                         append(L2,L3,L4).

 nth([F|R],1,F).
 nth([F|R],N,M) :- N > 1, N1 is N-1, nth(R,N1,M).

 is_string(S):-string(S).

%%:-['aimlbotFromBoth.pl'].

ta:-alicebot2(['HI'],X),!,writeln(X).



run_chat_tests:-
   test_call(alicebot('Hi')),
   test_call(alicebot('What is your name')),
   test_call(alicebot('My name is Fred.')),
   test_call(alicebot('what is my name?')).

test_call(G):-writeln(G),ignore(once(catch(G,E,writeln(E)))).

main_loop1(Atom):- current_input(In),!,
            read_line_with_nl(In,Codes,[]),!,
            atom_codes(Atom,Codes),!,
            alicebot(Atom),!.

main_loop:-repeat,main_loop1(Atom),catch(atom_to_term(Atom,Term,Vars),_,fail),
      once(callInteractive0(Term,Vars)),fail.

% callInteractive(Term,V):-
callInteractive(Term,Var):-callInteractive0(Term,Var).

callInteractive0(Term,Var):-atom(Term),!,Term,writeln(called(Term)),!.
callInteractive0(Term,_):-catch((call(Term),writeq(Term),nl,fail),_,fail).
callInteractive0(Term,_):-!.


% ===============================================================================================
% Save Possible Responses (Degrade them as well)
% ===============================================================================================

dyn_retractall(X):- numbervars(X,0,E),call(X),retract(X),fail.
dyn_retractall(_).

possibleResponse( 0.1, [very,bad]).

:-dynamic(possibleResponse/2).

savePosibleResponse(N,O):- call( possibleResponse(_,O) ),!.
savePosibleResponse(N,O):-
    findall(1, degraded(O), L),!,
   length(L,K),
   SN is N - (K * 0.6)  , !,
   asserta(possibleResponse(SN,O)).


   
% ===============================================================================================
% ALICE IN PROLOG
% ===============================================================================================


convert_to_string(I,ISO):-
                term_to_string(I,IS),!,
		string_to_list(IS,LIST),!,
		list_replace(LIST,92,[92,92],LISTM),
		list_replace(LISTM,34,[92,34],LISTO),
		string_to_atom(ISO,LISTO),!.

list_replace(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace(List,_Char,_Replace,List):-!.


term_to_string(I,IS):- catch(string_to_atom(IS,I),_,(term_to_atom(I,A),string_to_atom(IS,A))),!.
%well i played with a couple few differnt environment impls.. they have their pros cons.. one impl.. that was unique is that an array of "binding pairs" live in an arraylist.. to be "in" an environment it meant that you held an "index" into the arry list that as you went backwards you'd find your bindings.. each symbol had a java int field "lastBindingIndex" .. that was a "hint" to where you could fastforward the backwards search .. end named binding context also had a "index" to when you leave a named block.. you could quickly reset the top of an index.

say(X):-format(user_output,'~q~n',X),flush_output(user_output).


alicebot(Input):-
   alicebot(Input,Resp),
   say(Resp).
alicebot(Input):-say('-no response-').
   

removePMark(UCase,Atoms):-member(Last,[?,('.'),(',')]),append(AtomsPre,[Last],UCase),!,removePMark(AtomsPre,Atoms).
removePMark(Atoms,Atoms).



%clean_out_atom(X,X).

randomPick(List,Ele):-length(List,Len),Pick is random(Len),nth0(Pick,List,Ele),!.

all_upper_atom(X):-toUppercase(X,N),N=X.

:-dynamic(default_channel/1).
:-dynamic(default_user/1).

default_channel( "#logicmoo").
default_user(    "unknown_partner").         

% say(Say):-writeq(Say),nl.
toCodes(B,A):-cyc:stringToCodelist(B,AO),(is_list(A) -> A=AO ; string_to_list(AO,A)),!.


dumpList([]):-!.
dumpList([A|B]):-say(A),dumpList(B),!.

dumpList(B):-say(  dumpList(B)).

alicebot(Input,Resp):-
   getWordTokens(Input,Tokens),
   toUppercase(Tokens,UCase),
   removePMark(UCase,Atoms),
   alicebot2(Atoms,Resp).

alicebot2(Atoms,[O-N]):-	
   computeAnswer(1,srai(Atoms),O,N),!.

alicebot2(Atoms,Resp):-	
   dyn_retractall(possibleResponse(_,_)),
   swi_flag(a_answers,_,0),!,
   ignore((
   call_with_depth_limit(computeAnswer(1,srai(Atoms),O,N),8000,DL),
	 ignore((nonvar(N),nonvar(O),savePosibleResponse(N,O))),swi_flag(a_answers,X,X+1),X>3)),!,
   findall(NR-OR, call(possibleResponse(NR,OR)) ,L),!,
   %format('~n-> ~w~n',[L]),
   keysort(L,S),!,
   dumpList(S),!,
   reverse(S,[Resp|RR]),!,
   degrade(Resp),!,
   rememberSaidIt(Resp),!.

% ===============================================================================================
% Degrade Response
% ===============================================================================================

:-dynamic(degraded/1).
degraded([]).

degrade(_-OR):- degraded(OR),!.
degrade(OR):- asserta(degraded(OR)).


% ===============================================================================================
% Compute Answer Probilities
% ===============================================================================================
from_atom_codes(Atom,Atom):-atom(Atom),!.
from_atom_codes(Atom,Codes):-convert_to_string(Codes,Atom),!.
from_atom_codes(Atom,Codes):-atom_codes(Atom,Codes).

computeAnswer(Votes,[],[],Votes):-!.


computeAnswer(Votes,['.'],[],Votes):-!.
computeAnswer(Votes,I,_,_):-(Votes>20;Votes<0.3),!,fail.
computeAnswer(Votes,srai(Input),O,VotesO):- !,flatten([Input],Flat),computeSRAI(Votes,Flat,Mid,VotesM),computeAnswer(VotesM,Mid,O,VotesO).
computeAnswer(Votes,nick,A,Votes):-!,default_user(B),!,from_atom_codes(A,B),!.
computeAnswer(Votes,person,A,Votes):-!,default_user(B),!,from_atom_codes(A,B),!.
computeAnswer(Votes,botnick,'jllykifsh',Votes):-!.
computeAnswer(Votes,mynick,'jllykifsh',Votes):-!.
computeAnswer(Votes,name=name,'jllykifsh',Votes):-!.
computeAnswer(Votes,mychan,A,Votes):-!,default_channel(B),!,from_atom_codes(A,B),!.
computeAnswer(Votes,randomsentence,O,VotesO):-!, choose_randomsentence(X),!,computeAnswer(Votes,X,O,VotesO).
computeAnswer(Votes,Resp,Resp,Votes):-atomic(Resp),!.
computeAnswer(Votes,li(List),AA,VotesO):-!,computeAnswer(Votes,List,AA,VotesO).
computeAnswer(Votes,random(List),AA,VotesO):-!,randomPick(List,Pick),computeAnswer(Votes,Pick,AA,VotesO).
computeAnswer(Votes,condition(List),AA,VotesO):-!,member(Pick,List),computeAnswer(Votes,Pick,AA,VotesO).
computeAnswer(Votes,String,Atom,Votes):-string(String),!,string_to_atom(String,Atom).
computeAnswer(Votes,String,Atom,Votes):-is_string(String),toCodes(String,Codes),!,from_atom_codes(Atom,Codes).
computeAnswer(Votes,'$stringCodes'(List),AA,Votes):-!,from_atom_codes(AA,List).
computeAnswer(Votes,gossip(Thought),O,VotesO):-!,computeAnswer(Votes,Thought,O,VotesO).
computeAnswer(Votes,think(Thought),[],VotesO):-!,computeAnswer(Votes,Thought,O,VotesO).
computeAnswer(Votes,get([X]),Resp,VotesO):-getAliceMem(X,E),!,computeAnswer(Votes,E,Resp,VotesM),VotesO is VotesM * 1.1.
computeAnswer(Votes,get(X),Resp,VotesO):-getAliceMem(X,E),!,computeAnswer(Votes,E,Resp,VotesM),VotesO is VotesM * 1.1.
computeAnswer(Votes,get(_),_,_):-!,fail.
computeAnswer(Votes,_-A,A,Votes):-!.
computeAnswer(Votes,set(X,E),Resp,VotesO):-!,computeAnswer(Votes,E,Resp,VotesM),setAliceMem(X,Resp),!,VotesO is VotesM * 1.1.
%computeAnswer(Votes,B,Out,VotesO):-append(BB,['.','.'|BBB],B),append(BB,['.'|BBB],RB),!,computeAnswer(Votes,RB,Out,VotesO).
computeAnswer(Votes,B,Out,VotesO):-append(BB,['.'|BBB],B),append(BB,BBB,RB),!,computeAnswer(Votes,RB,Out,VotesO).
computeAnswer(Votes,[A|L],OO,VotesO):-!,
	 computeAnswer(Votes,A,AA,VotesM),
	 computeAnswer(VotesM,L,LL,VotesO),
	 once(flatten([AA,LL],OO)).
computeAnswer(Votes,Resp,Resp,Votes).





% ===============================================================================================
% Apply Input Match
% ===============================================================================================

computeSRAI(Votes,[],_,_):-!,fail.
computeSRAI(Votes,Input,Output,VotesO):-append(Left,[Last],Input),
         member(Last,[('.'),('!'),('?')]),!,
         computeSRAI(Votes,Left,Output,VotesO).

computeSRAI(Votes,Input,TopicStarO,VotesO):-
	 getLastSaid(WhatSaid),
	 set_matchit(Input,MatchIt),get_aiml_what(What,MatchIt,Out),
	 rateMatch(What,WhatSaid,What,NewTopic,TopicVote,TopicStar), 
	 rateMatch(MatchIt,Input,Out,Next,Voted,_), 
	 flatten([Next],NextO),
	 subst(NextO,topicstar,TopicStar,TopicStarO),
	 VotesO is Votes * (Voted + TopicVote).





% this next line is what it does on fallback
%computeSRAI(Votes,[B|Flat],[B|TopicStarO],VotesO):-computeSRAI(Votes,Flat,TopicStarO,VotesO).

set_matchit([Input|_],[Input|_]).
set_matchit([Input|_],[_,Input|_]).
set_matchit([_,Input|_],[_,Input|_]).

%get_aiml_what(What,Match,OOut):-get_aiml_cyc(What,Match,Out),(([srai(Out)] = OOut);OOut=Out).
get_aiml_what(What,Match,Out):-what(What, Match,Out).
get_aiml_what([*],Match,Out):-response(Match,Out).

%get_aiml_cyc([*],[String|ListO],[Obj,*]):-poStr(Obj,[String|List]),append(List,[*],ListO).
%get_aiml_cyc([*],[String,*],[Obj,*]):-poStr(Obj,String).


% ===============================================================================================
% Rate Match
% ===============================================================================================
rateMatch([],[],Out,Out,1,[]):-!.

rateMatch(Match,Match,Out,Out,1.3,[]):-!.

rateMatch([This|More],[This|More2],Out,OOut,Vote2,Grabbed):-!,
      rateMatch(More,More2,Out,OOut,Vote,Grabbed),!,
      Vote2 is Vote * (1.11).

rateMatch([*],More,Out,OOut,0.77,More):-!,subst(Out,*,More,OOut),!.
rateMatch(['_'],More,Out,OOut,0.87,More):-!,subst(Out,*,More,OOut),!.

rateMatch([*|Rest],More,Out,OOut,VoteO,Grabbed):-!,
      append(Grabbed,Rest,More),
      rateMatch(More,More2,Out,OOut,Vote,_),
      subst(Out,*,Grabbed,OOut),
      VoteO is Vote * (0.72),!.

rateMatch(['_'|Rest],More,Out,OOut,VoteO,Grabbed):-!,
      append(Grabbed,Rest,More),
      rateMatch(More,More2,Out,OOut,Vote,_),
      subst(Out,*,Grabbed,OOut),
      VoteO is Vote * (0.82),!.
            


% ===============================================================================================
% Run answer procs
% ===============================================================================================

choose_randomsentence(X):-
	repeat,
		retract(random_sent(Y)),
		assertz(random_sent(Y)),
		4 is random(10),!,
		Y=X.

getAliceMem(X,E):-isAliceMem(X,E),!.
 
setAliceMem(X,[E]):-!,setAliceMem(X,E),!.
setAliceMem(X,'nick').
setAliceMem(X,E):-dyn_retractall(isAliceMem(X,_)),asserta(isAliceMem(X,E)),writeln(debug(isAliceMem(X,E))),!.
:-dynamic(isAliceMem/2).

% ===============================================================================================
% Get and rember Last Said
% ===============================================================================================

:-dynamic(getLastSaid/1).
getLastSaid([*]).

rememberSaidIt([]):-!.
rememberSaidIt(_-R1):-!,rememberSaidIt(R1).
rememberSaidIt(R1):-append(New,['.'],R1),!,rememberSaidIt(New).
rememberSaidIt(R1):-ignore(retract(getLastSaid(_))),toUppercase(R1,SR1),!,asserta(getLastSaid(SR1)).


% =================================================================================
% AIML Loading
% =================================================================================
load_aiml_files:-load_aiml_files('*.aiml'),!.

load_aiml_files([]).
load_aiml_files([H|T]):-
   load_aiml_files(H),!,
   load_aiml_files(T).
load_aiml_files(F):-atom(F),
   expand_file_name(F,[F]),!,
   load_aiml_file(F).
load_aiml_files(F):-atom(F),expand_file_name(F,FILES),!,load_aiml_files(FILES).

aiml_files(F,Files):-atom_concat(F,'/',WithSlashes),expand_file_name(WithSlashes,WithOneSlash,[relative_to('./')]),
                    atom_concat(WithOneSlash,'/*.aiml',Mask),expand_file_name(Mask,Files).

load_aiml_file(F):- exists_directory(F), aiml_files(F,Files),!,load_aiml_files(Files).
load_aiml_file(F):- file_name_extension(F,'aiml',Aiml).


create_aiml_file(F):-
   atom_concat(F,'.pl',PLNAME),
   time_file(PLNAME,PLTime), % fails on non-existent
   time_file(F,FTime),
   PLTime > FTime,!,
   [PLNAME],!.

create_aiml_file(F):-
   atom_concat(F,'.pl',PLNAME),
   tell(PLNAME),
   (format(user_error,'%~w~n',[F])),
   load_structure(F,X,[space(remove)]),!,
   load_aiml_structure(notopic,X),!,
   told,[PLNAME],!.


load_aiml_structure(_,O):-ignore_aiml(O),!.
load_aiml_structure(N,[X|L]):-load_aiml_structure(N,X),!,load_aiml_structure(N,L),!.
   
load_aiml_structure(N,element(aiml,L,T)):-!,load_aiml_structure(N,T).
load_aiml_structure(N,element(a,L,T)):-!,load_aiml_structure(N,T).
load_aiml_structure(N,element(substitute,[name=Catalog, find=Find, replace=Replace],[])):-!,
	 load_aiml_structure(N,substitute(Catalog,Find,Replace)).
load_aiml_structure(N,element(substitute,[name=Catalog, find=Find, replace=Replace],Stuff)):-!,
	 load_aiml_structure(N,substitute(Catalog,Find,Replace,Stuff)).
load_aiml_structure(N,element(topic,[name=Name],T)):-!,load_topic(Name,T),!.

load_topic(_,L):-ignore_aiml(L).
load_topic(Name,[H|T]):-
      load_aiml_structure(Name,H),!,
      load_topic(Name,T).

load_aiml_structure(N,element(category,[],T)):-!,load_category(N,T),!.
load_aiml_structure(N,element(startup,_,T)):-!.

load_aiml_structure(N,substitute(Dict,Find,Replace)):-
      atomSplit(Find,F),atomSplit(Replace,Resp),!,
      load_aiml_structure(N,dict(Dict,F,Resp)).

load_aiml_structure(N,X):-format('~q.~n',[X]).
%      saveFAttribute(F,A),
     % 
      %(catch(X,_,fail);asserta(X)),!.

:-dynamic(saveDFAttribute/2).
:-dynamic(replace_t/5).
:-dynamic(response_t/5).
                
%saveFAttribute(F,A):-saveDFAttribute(F,A),!.
%saveFAttribute(F,A):-asserta(saveDFAttribute(F,A)),dynamic(F/A).

% ===============================================================================================
% ===============================================================================================

ignore_aiml([]).
ignore_aiml(A):-atom(A),atom_codes(A,C),clean_codes(C,[]).

%clean_codes(X,X).


aiml_classify([],[]).
aiml_classify(Find,[atom]):-atomic(Find).
aiml_classify([H|T],Out):-
      classifySingle(H,Class),
      aiml_classify(T,More),
      sort([Class|More],OutM),!,
      classify2(OutM,Out).
aiml_classify(T,[unk]).

classify2([in,out|Resp],[out|Resp]).
classify2(Out,Out).

classifySingle('_',var('_')).
classifySingle(*,var('*')).
classifySingle(Atom,in):-atom(Atom),all_upper_atom(Atom).
classifySingle(Atom,out):-atom(Atom).
classifySingle(Atom,spec(F)):-compound(Atom),functor(Atom,F,_).
classifySingle(Atom,unknown).

                                        
      
varize(Find,Replace,FindO,ReplaceO):-
      subst((Find,Replace),'_','$VAR'(0),(FindM,ReplaceM)),
      subst((FindM,ReplaceM),'*','$VAR'(0),(FindO,ReplaceO)),!.
      

load_category(_,L):-ignore_aiml(L).
load_category(N,L):-
      ignore(member(element(that,IA,IB),L)),
      ignore((member(element(justbeforethat,[],[]),L),(IA= [index='1,2'],IB=[]))),
      ignore(member(element(pattern,PA,PB),L)),
      ignore(member(element(template,TA,TB),L)),!,
      load_category(N,IA,IB,PA,PB,TA,TB),!.

load_category(Topic,IA,IB,PA,PB,TA,TB):-
      ignore(ignore_aiml(IA)),
      convert_template(IB,I),
      convert_template(PB,P),
      convert_template(TB,T),
      parse_category(Topic,IA,I,PA,P,TA,T).
load_category(Topic,IA,IB,PA,PB,TA,TB):-trace.

parse_category(notopic,[],[],[],PB,[],TB):-!,
      load_aiml_structure(N,response(PB,TB)).

parse_category(notopic, [],What, [], A, [], B):-!,
      load_aiml_structure(N,what(What,A,B)).

parse_category(Topic,[],[],[],PB,[],TB):-!,
   load_aiml_structure(N,topic_resp(Topic,PB,TB)).

parse_category(Topic,IA,IB,PA,PB,TA,TB):-!,
   load_aiml_structure(N,category(Topic,IA,IB,PA,PB,TA,TB)).
      


convert_template(Input,Out):-
   convert_template1(Input,M),
   flatten(M,MM),
   convert_template1(MM,O),
   flatten(O,Out).


convert_template1([],[]).
convert_template1([I|P],L):-
      ignore_aiml(I),!,convert_template1(P,L).
convert_template1([I|P],[O|L]):-
      convert_element(I,O),!,
      convert_template1(P,L).
convert_template1(P,P).

      
convert_element(Input,Out):-convert_ele(Input,M),!,convert_ele(M,Out),!.


convert_ele(li(A),li(AA)):-convert_template(A,AA).
convert_ele(get(A,[]),get(AA)):-convert_template(A,AA).
convert_ele(element(set, [name=N], V),set(N,VV)):-convert_template(V,VV).
convert_ele(element(get, [name=N], V),get(N,VV)):-convert_template(V,VV).
convert_ele(element(SET_ATOM, [], V),set(N,VV)):-atom_concat('set_',N,SET_ATOM),convert_template(V,VV).
convert_ele(element(GET_ATOM, [], V),get(N,VV)):-atom_concat('get_',N,GET_ATOM),convert_template(V,VV).
convert_ele(element(BOT_ATOM, [], V),match(N,VV)):-atom_concat('bot_',N,BOT_ATOM),convert_template(V,VV).
convert_ele(element(bot, [N=V], []),N=V).
convert_ele(element(srai, [], B),srai(BB)):-convert_template(B,BB).
convert_ele(element(random, [], B),random(BB)):-convert_template(B,BB).
convert_ele(element(li, [], B),li(BB)):-convert_template(B,BB).
convert_ele(element(star, [], []),(*)).
convert_ele(element(a, [Target, Link], Name),A):-sformat(S,'<a ~q ~q>~w</a>',[Target, Link, Name]),string_to_atom(S,A).
convert_ele(element(a, [Link], Name),A):-sformat(S,'<a ~q>~w</a>',[Link, Name]),string_to_atom(S,A).

convert_ele(element(A, [], []),A):-!.
convert_ele(element(A, [], B),T):-!,convert_template(B,BB), T=..[A,BB].
convert_ele(element(A, B, []),T):-!,convert_template(B,BB), T=..[attrib,A,BB].
convert_ele(element(A, B, C),T):-!,
      convert_template(B,BB),
      convert_template(C,CC),!, 
      T=..[A,BB,CC].
convert_ele(attrib(get, name=Var),get(Var)):-!.
convert_ele(attrib(learn, filename=F),load_any_file(F)):-!.
convert_ele(sr,[srai([*])]).
convert_ele(star,*).

convert_ele(L,LO):-is_list(L),flatten(L,M),!,
	    (L==M -> LO=M ; convert_template(M,LO)).

convert_ele(A,W):-atom(A),clean_out_atom(A,B),
	    atomSplit(B,W).
convert_ele(O,O).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%PROBLY SHOULD E IN CYC
toUppercase(VAR,VAR):-var(VAR),!.
toUppercase([],[]):-!.
toUppercase(V,V2):-string(V),!,atom_codes(V,VC),toUppercase(VC,CVC),string_to_atom(V2,CVC),!.
toUppercase(95,45):-!.
toUppercase(I,O):-integer(I),!,to_upper(I,O).
toUppercase([A|L],[AO|LO]):-
   toUppercase(A,AO),!,
   toUppercase(L,LO),!.
toUppercase(MiXed,UPPER):-atom(MiXed),!,
   atom_codes(MiXed,Codes),
   toUppercase(Codes,UCodes),
   atom_codes(UPPER,UCodes),!.
toUppercase(MiXed,UPPER):-compound(MiXed),MiXed=..MList,toUppercase(MList,UList),!,UPPER=..UList.
toUppercase(A,A).

toLowercase(VAR,VAR):-var(VAR),!.
toLowercase(V,V2):-string(V),!,atom_codes(V,VC),toLowercase(VC,CVC),string_to_atom(V2,CVC),!.
toLowercase([],[]):-!.
toLowercase(95,45):-!.
toLowercase(I,O):-integer(I),!,to_lower(I,O).
toLowercase([A|L],[AO|LO]):-
   toLowercase(A,AO),!,
   toLowercase(L,LO),!.
toLowercase(MiXed,UPPER):-atom(MiXed),!,
   atom_codes(MiXed,Codes),
   toLowercase(Codes,UCodes),
   atom_codes(UPPER,UCodes),!.
toLowercase(MiXed,UPPER):-compound(MiXed),MiXed=..MList,toLowercase(MList,UList),!,UPPER=..UList.
toLowercase(A,A).


subst(A,B,C,D):- 
   nd_subst(A,B,C,D),!.
subst(A,B,C,A).

nd_subst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_subst(  P, X,Sk, P1 ) :- functor(P,_,N),nd_subst1( X, Sk, P, N, P1 ).

nd_subst1( _,  _, P, 0, P  ).
nd_subst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], 
            nd_subst2( X, Sk, Args, ArgS ),
            nd_subst2( X, Sk, [F], [FS] ),  
            P1 =.. [FS|ArgS].

nd_subst2( _,  _, [], [] ).
nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- nd_subst( A,X,Sk,Ap ),nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, L, L ).


atomSplit(Atom,Words):-var(Atom),!,
   concat_atom(Words,' ',Atom).
atomSplit(Atom,Words):-
   concat_atom(Words1,' ',Atom),
   atomSplit2(Words1,Words),!.

atomSplit2([],[]).
atomSplit2([W|S],[A,Mark|Words]):- member(Mark,['.',',','?']),atom_concat(A,Mark,W),not(A=''),!,atomSplit2(S,Words).
atomSplit2([W|S],[Mark,A|Words]):- member(Mark,['.',',','?']),atom_concat(Mark,A,W),not(A=''),!,atomSplit2(S,Words).
atomSplit2([W|S],[W|Words]):-atomSplit2(S,Words).



:-['cynd/bootstrap.aiml.pl'].


%:-load_aiml_files.


%:-debug,run_chat_tests.

%:-main_loop.


