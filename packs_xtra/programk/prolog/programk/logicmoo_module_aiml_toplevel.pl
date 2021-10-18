% ===================================================================
% File 'logicmoo_module_aiml_toplevel.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
/*
:- if( (current_prolog_flag(version,MMmmPP), MMmmPP<70000) ).
%:- style_check(-atom).
:- style_check(-string).
:- endif.
*/

% :-catch(noguitracer,E,writeq(E)),nl.

:-multifile(what/3).
:-multifile(response/2).
:-dynamic(dict/3).
:-multifile(dict/3).

:-dynamic(lineInfoElement/4).

%% ======================================================
%% Add a library directory 2 levels above this file
%% ======================================================
asserta_if_new_hlper1(C):-catch(C,_,fail),!.
asserta_if_new_hlper1(C):-asserta(C),!.

:-dynamic(local_directory_search/1).
:-multifile(local_directory_search/1).
:-module_transparent(local_directory_search/1).

addPaths:- source_location(File,_Line),file_directory_name(File, Directory),
   %context_module(M),
   file_directory_name(Directory,ParentDir),% cd(ParentDir),   
   asserta_if_new_hlper1(user:library_directory(ParentDir)),
   !,% writeq(M:asserta(user:library_directory(ParentDir))),nl,
   file_directory_name(ParentDir,ProgramK),
   asserta_if_new_hlper1(user:file_search_path(programk,ProgramK)),
   asserta_if_new_hlper1(local_directory_search(ProgramK)),
   absolute_file_name('aiml/',[relative_to(ProgramK),file_type(directory)],AIMLDir),
   asserta_if_new_hlper1(user:file_search_path(aiml,AIMLDir)),!.


:- addPaths.

:-ensure_loaded(library('programk/logicmoo_module_aiml_shared.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_graphmaster.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_memory.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_natlang.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_cxt_path.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_loader.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_convertor.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_eval.pl')).
%%:-ensure_loaded(library('notaiml/tokenize.pl')).

local_directory_search('cynd').
local_directory_search('cynd/programk').
local_directory_search('programk').
local_directory_search('../aiml').
local_directory_search('aiml').
local_directory_search('aiml/test_suite').
local_directory_search('special').
local_directory_search('..').
local_directory_search('../..').

local_directory_search_combined2(PL):-local_directory_search(A),local_directory_search(B),join_path(A,B,PL),exists_directory_safe(PL).

local_directory_search_combined(X):-local_directory_search(X).
local_directory_search_combined(X):-local_directory_search_combined2(X).
%% for now dont do the concat 3 version
local_directory_search_combined(PL):-local_directory_search_combined2(A),local_directory_search(B),join_path(A,B,PL),exists_directory_safe(PL).

run_chat_tests:-
   test_call(alicebot('Hi')),
   test_call(alicebot('What is your name')),
   test_call(alicebot('My name is Fred.')),
   test_call(alicebot('what is my name?')).

test_call(G):-writeln(G),ignore(once(error_catch(G,E,writeln(E)))).


main_loop1(Atom):- current_input(In),!,
            read_line_to_codes(In,Codes),!,            
            atom_codes_or_eof(Atom,Codes),!,
            ignore(once(alicebot(Atom))),!.

atom_codes_or_eof(end_of_file,end_of_file):-!.
atom_codes_or_eof(Atom,Codes):- atom_codes(Atom,Codes).


ping_default_files:-exists_source(aiml('../temp/aimlCore3.pl')),trace,ensure_loaded(aiml('../temp/aimlCore3.pl')),!.
ping_default_files:-exists_source(aiml('../temp/aimlCore.pl')),trace,ensure_loaded(aiml('../temp/aimlCore.pl')),!.
ping_default_files.

main_loop:- ping_default_files,repeat,main_loop1(EOF),EOF==end_of_file.

:-dynamic(default_channel/1).
:-dynamic(default_user/1).

%default_channel( "#logicmoo").
%default_user(    "default_user").

% say(Say):-writeq(Say),nl.

% ===============================================================================================
% ALICE IN PROLOG
% ===============================================================================================

say(X):-currentContext(say(X),Ctx),say(Ctx,X),!.
say(Ctx,X):- aiml_eval(Ctx,X,Y),!,answerOutput(Y,O),showTransition(O,Y,Trans),debugFmt(say(Trans)),!.

showTransition(O,Y,O):-O==Y,!.
showTransition(O,Y,O=Y).

alicebot:-
        currentContext(alicebot,Ctx),
        alicebotCTX(Ctx).

alicebotCTX(Ctx):-
        repeat,
        current_input(In),
	read_line_to_codes(In,Codes),
        ( Codes==end_of_file 
         -> ! ;
        (tokenizeInput(Atom,Codes),
        %%atom_codes(Atom,Codes),
         ignore(once(alicebotCTX(Ctx,Atom))),
         Atom==end_of_file)).

% ===============================================================================================
% Main Alice Input
% ===============================================================================================

alicebot(Input):-
  currentContext(alicebot(Input),Ctx),
  alicebotCTX(Ctx,Input),!.

alicebotCTX(Ctx,Input):- prolog_must(nonvar(Input)), alicebotCTX(Ctx,Input,Resp),!,say(Ctx,Resp),!.
%%alicebotCTX(Ctx,_):- atrace, say(Ctx,'-no response-'),!.

% ===============================================================================================
% Main Alice Input-Output
% ===============================================================================================
alicebot([],_):-debugFmt('no input'),!,fail.
alicebot(Input,Resp):- currentContext(alicebot(Input),Ctx),alicebotCTX(Ctx,Input,Resp),!.
alicebot(In,Res):- !,ignore(Res='-no response-'(In)).

alicebotCTX(_Ctx,[],[]):-!.
alicebotCTX(Ctx,Input,Resp):-
      context_module(M),
      debugOnError(tokenizeInput(Input,Tokens)),
      prolog_statistics:time(M:debugOnError(alicebotCTX4(Ctx,Input,Tokens,Resp))),!.


% ===============================================================================================
% Main Alice Source-Input-Output
% ===============================================================================================
alicebotCTX4(Ctx,String,Input,Res):- callableInput(Ctx,String,Input,Res),!.
alicebotCTX4(_Ctx,String,Input,_Resp):- isNoInput(String,Input), debugFmt('no input'),!,fail.
alicebotCTX4(Ctx,String,Input,Resp):-
  prolog_mustEach((
   ground(String),ground(Input),
   Atoms = Input,
   retractall(posibleResponse(_,_)),
   flag(a_answers,_,0),!,
   prolog_mustEach((
   getAliceMem(Ctx,'bot','you',User),
   getAliceMem(Ctx,'bot','me',_Robot),
   getAliceMem(Ctx,'bot',default('minanswers',1),MinAns0),unlistify(MinAns0,MinAns1),MinAnsMinusOne is MinAns1 -1,
   getAliceMem(Ctx,'bot',default('maxanswers',1),_MaxAns),
   %%setAliceMem(Ctx,User,'input',Atoms),
   pushInto1DAnd2DArray(Ctx,'request','input',5,Atoms,User),
   setAliceMem(Ctx,User,'rawinput',String))),
   thread_local_flag(sraiDepth,_,0),
   prolog_must((call_with_depth_limit_traceable(computeInputOutput(Ctx,1,Atoms,Output,N),8000,_DL),
	 ignore((nonvar(N),nonvar(Output),savePosibleResponse(N,Output))),flag(a_answers,X,X+1),
                X>=MinAnsMinusOne)),!,
   findall(NR-OR,posibleResponse(NR,OR),L),!,
   (format('~n-> ~w~n',[L])),
   sort(L,S),
   dumpList(S),
   reverse(S,[Resp|_RR]),
   degrade(Resp),
   rememberSaidIt(Ctx,Resp))),!.

isNoInput(String,Input):-trimWhitepaceOffEnds(Input,InputTrimed),Input\==InputTrimed,!,isNoInput(String,InputTrimed).
isNoInput(_,[]):-!.
isNoInput(_,['']):-!.
isNoInput(_,_):-fail.
% ===============================================================================================
% Call like a SRAI tag
% ===============================================================================================
computeInput(Ctx, VoteIn,NotList,InputM):-not(is_list(NotList)),!,listify(NotList,Input),maplist_safe(computeInnerEach(Ctx, VoteIn),Input,InputM).
computeInput(Ctx, VoteIn,Input,InputM):-maplist_safe(computeInnerEach(Ctx, VoteIn),Input,InputM).

computeInputOutput(Ctx,VoteIn,Input,Output,VotesOut):-
    prolog_mustEach((computeInput(Ctx, VoteIn,Input,InputM),!,
                     computeElement(Ctx,VoteIn,srai,[],InputM,OutputM,VotesOM),
                     computeTemplateOutput(Ctx,VotesOM,OutputM,Output,VotesOut))).

computeInputOutput(Ctx,VoteIn,Input,Output,VotesOut):-
   ((prolog_mustEach((computeAnswer(Ctx,VoteIn,element(srai,[],Input),OutputM,VotesOM),
                          computeTemplateOutput(Ctx,VotesOM,OutputM,Output,VotesOut))))),!.



% ===============================================================================================
% Save Possible Responses (Degrade them as well)
% ===============================================================================================
:-dynamic(posibleResponse/2).

savePosibleResponse(_N,Output):-posibleResponse(_,Output),!.
savePosibleResponse(N,Output):-
   findall(1,degraded(Output),L),!,
   length(L,K),
   SN is N - (K * 0.6)  , !,
   asserta(posibleResponse(SN,Output)).

% ===============================================================================================
% Degrade Response
% ===============================================================================================

:-dynamic(degraded/1).

degrade(_-OR):-!,degrade(OR).
degrade(OR):-asserta(degraded(OR)).


% ===============================================================================================
% Expand Answers
% ===============================================================================================
flatten_if_list(List,Flat):-is_list(List),!,flatten(List,Flat),!.

computeTemplate(Ctx,Votes,Input,Output,VotesO):-flatten_if_list(Input,Flat),!,computeTemplate0(Ctx,Votes,Flat,Output,VotesO).
computeTemplate(Ctx,Votes,Input,Output,VotesO):-computeTemplate0(Ctx,Votes,Input,Output,VotesO).

computeTemplate0(Ctx,Votes,Input,Output,VotesO):-prolog_must(computeTemplate1(Ctx,Votes,Input,Output,VotesO)).

computeTemplate1(_Ctx,Votes,In,Out,VotesO):-In==[],!,prolog_must((In=Out,Votes=VotesO)).
computeTemplate1(Ctx,Votes,IN,Out,VotesO):-IN=[I|N],!,computeTemplate11(Ctx,Votes,[I|N],Out,VotesO).
computeTemplate1(Ctx,VotesM,In,Out,VotesM):-traceAIML,expandVar(Ctx,In,Out).
computeTemplate1(_Ctx,Votes,In,Out,VotesO):-prolog_must((In=Out,Votes=VotesO)).

computeTemplate11(_Ctx,Votes,In,Out,VotesO):-In==[],!,prolog_must((In=Out,Votes=VotesO)).
computeTemplate11(Ctx,Votes,[<,BR,/,>|B],OO,VotesO):-atom(BR),!,
   computeTemplate11(Ctx,Votes,[element(BR,[],[])|B],OO,VotesO).
computeTemplate11(Ctx,Votes,[A|B],[A|BB],VotesO):-atomic(A),!,computeTemplate11(Ctx,Votes,B,BB,VotesO).
computeTemplate11(Ctx,Votes,[A|B],OO,VotesO):-expandVar(Ctx,A,AA),computeTemplate11(Ctx,Votes,B,BB,VotesO),once(flatten([AA,BB],OO)).


traceAIML:-!.

expandVar(_Ctx,Var,Var):-var(Var),!,traceAIML.
expandVar(_Ctx,[Var|_],Var):- !,atrace,traceAIML.
expandVar(Ctx,In,Out):-atom(In),atom_concat('$',NameVar,In),!,expandVariable(Ctx,NameVar,Out).
expandVar(_Ctx,In,Out):-atomic(In),Out=In,!.
expandVar(Ctx,star(A,B,C),Out):-!,starName(A,AStar),!,expandVar(Ctx,element(AStar,B,C),Out).
expandVar(Ctx,element(A,B,C),Out):-!,computeElementMust(Ctx,1,A,B,C,Out,_VotesO).
expandVar(Ctx,In,Out):-computeAnswerMaybe(Ctx,1,In,Out,_VotesO),!.
expandVar(_Ctx,In,Out):-atrace,Out=In,!.

expandVariable(Ctx,In,Out):-atom(In),atom_concat('$',NameVar,In),!,expandVariable(Ctx,NameVar,Out),!.
expandVariable(Ctx,In,Out):-atom(In),atom_concat(NameVar,'$',In),!,expandVariable(Ctx,NameVar,Out),!.
expandVariable(Ctx,name=Name,Result):-!,expandVariable(Ctx,Name,Result),!.

expandVariable(_Ctx,nick,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVariable(_Ctx,person,A):-!,default_user(B),!,from_atom_codes(A,B),!.
expandVariable(_Ctx,botnick,'jllykifsh'):-!.
expandVariable(_Ctx,mynick,'jllykifsh'):-!.
expandVariable(_Ctx,version,[push_nobrkspace,'1','.','0','.','1',pop_nobrkspace]):-!.
expandVariable(_Ctx,id,'$botnick$'):-!.
expandVariable(_Ctx,size,Size):-aimlCateSig(X),predicate_property(X,number_of_clauses(Size)),!.
%TODO extract the machine TimeZone
expandVariable(_Ctx,date,[Y,'-',M,'-',D]):-get_time(TimeStamp),stamp_date_time(TimeStamp,DateTime,'UTC'),date_time_value(date,DateTime,date(Y,M,D)),!.
expandVariable(_Ctx,mychan,A):-!,default_channel(B),!,from_atom_codes(A,B),!.
expandVariable(Ctx,NameVar,Result):-getAliceMem(Ctx,'bot',NameVar,Result),!.
expandVariable(Ctx,NameVar,Result):-getAliceMem(Ctx,'global',NameVar,Result),!.


globalAliceTagVar(BOT_ATOM):-not(atom(BOT_ATOM)),!,fail.
globalAliceTagVar(BOT_ATOM):-member(BOT_ATOM,[version,id,favfood,date,size,news,emotion,time,favoritebook,birthdate]).
globalAliceTagVar(BOT_ATOM):-atom_concat('fav',_,BOT_ATOM).
globalAliceTagVar(BOT_ATOM):-atom_concat('bot_',_,BOT_ATOM).
globalAliceTagVar(BOT_ATOM):-atom_concat('get_',_,BOT_ATOM).
globalAliceTagVar(BOT_ATOM):-atom_concat('get',_,BOT_ATOM).


from_atom_codes(Atom,Atom):-atom(Atom),!.
from_atom_codes(Atom,Codes):-convert_to_string(Codes,Atom),!.
from_atom_codes(Atom,Codes):-atom_codes(Atom,Codes).


:-dynamic(recursiveTag/1).


notRecursiveTag(system).
%%notRecursiveTag(template).
notRecursiveTag(condition).
notRecursiveTag(Loader):-loaderTag(Loader).
notRecursiveTag(li).

loaderTag(Loader):-member(Loader,[aiml,topic,category,learn,load]).

recursiveTag(random).
recursiveTag(srai).
recursiveTag(NoRec):-notRecursiveTag(NoRec),!,fail.
recursiveTag(_).

isAimlTag(result):-!,fail.
isAimlTag(proof):-!,fail.
isAimlTag(get).
isAimlTag(_).

prolog_mostly_ground(Out):-ground(Out),!.
prolog_mostly_ground(Out):-var(Out),!,atrace.
prolog_mostly_ground([H|_Out]):-!,prolog_must(prolog_mostly_ground1(H)),!.
prolog_mostly_ground(Out):- ((arg(_N,Out,Arg),prolog_must(prolog_mostly_ground1(Arg)),fail));true.
prolog_mostly_ground1(Out):-prolog_must(nonvar(Out)).

computeInner(_Ctx, _Votes, In, Out) :- atom(In),!,Out=In.
computeInner(Ctx,Votes, In, Out) :- not(Out=(_-_)),!,computeAnswer(Ctx,Votes, In, Out, _VoteMid),!.
computeInner(Ctx,Votes, In, VoteMid-Out) :-  computeAnswer(Ctx,Votes, In, Out, VoteMid),!,prolog_must(nonvar(Out)),prolog_must(nonvar(VoteMid)).

computeInnerEach(_Ctx, _Votes, In, Out) :- atom(In), !, Out=In , prolog_mostly_ground(Out).

computeInnerEach(Ctx, _Votes, element(eval,ATTRIBS,INNER_XML),Rendered):-!,
   withAttributes(Ctx,ATTRIBS,aiml_eval_each(Ctx,INNER_XML,Rendered)),!.

computeInnerEach(  Ctx, Votes, In, Out) :- prolog_must(computeAnswer(Ctx,Votes, In, Out, _VoteMid)),!, prolog_mostly_ground(Out).
computeInnerEach(_Ctx, _Votes, In, Out) :- !, Out=In,  prolog_mostly_ground((Out)).

isNonCallable(A):-atomic(A),!.
isNonCallable([_|_]):-!.
isNonCallable(Pred):-predicate_property(Pred,number_of_clauses(_)),!,fail.
isNonCallable(_).

isCallable(Pred):-not(isNonCallable(Pred)).

% ===============================================================================================
% Compute Answer Element Probilities
% ===============================================================================================
computeElementMust(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO):-prolog_must(catch(computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO),E,throw(E))).

computeAnswerMaybe(Ctx,Votes,element(Tag,Attribs,InnerXml),Output,VotesO):-!,computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Output,VotesO).
computeAnswerMaybe(Ctx,Votes,InnerXml,Resp,VotesO):-isCallable(InnerXml),!,prolog_ecall(computeAnswerMaybeInnerLast(Ctx,Votes,Resp,VotesO),InnerXml).
computeAnswerMaybe(Ctx,Votes,InnerXml,Resp,VotesO):-computeAnswer(Ctx,Votes,InnerXml,Resp,VotesO).
computeAnswerMaybe(Ctx,Votes,InnerXml,Resp,VotesO):-debugFmt(computeAnswerMaybe(Ctx,Votes,InnerXml,Resp,VotesO)),fail.

computeAnswerMaybeInnerLast(Ctx,Votes,Resp,VotesO,InnerXml):-computeAnswerMaybe(Ctx,Votes,InnerXml,Resp,VotesO).

unused_computeElement(Ctx,Votes, Tag, ATTRIBS, [DO|IT], OUT, VotesO) :- recursiveTag(Tag),!,
      withAttributes(_Ctx,ATTRIBS,
        ((findall(Out,((member(In,[DO|IT]),computeTemplate(Ctx,Votes, In, Out, _VoteMid))),INNERDONE),
         NOUT=..[Tag,ATTRIBS,INNERDONE],!,
         computeAnswer(Ctx,Votes,NOUT,OUT,VotesO)))).

% element inner reductions
still_computeElement(Ctx,Votes, Tag, ATTRIBS, [DO|IT], OUT, VotesO) :- recursiveTag(Tag),not(DO=(_-_)),!,
     appendAttributes(Ctx,ATTRIBS, [computeAnswer=[side_effects_allow=[transform],intag=Tag]], ATTRIBS_NEW),
     withAttributes(_Ctx,ATTRIBS_NEW,
       ((findall(OutVoteMid,((member(In,[DO|IT]),computeInner(Ctx,Votes, In, OutVoteMid))),INNERDONE),
        NOUT=..[Tag,ATTRIBS,INNERDONE]))),!,
         withAttributes(Ctx,ATTRIBS,computeInnerTemplate(Ctx,Votes,NOUT,OUT,VotesO)).

:-discontiguous(computeElement/7).%%ah iot would be fine to move the OXY

computeElement(_Ctx,Votes,Tag,ATTRIBS,InnerXml,Output,VotesO):- G=a(Votes,Tag,ATTRIBS,InnerXml),
   (prolog_must(ground(G)),not(var(Output);var(VotesO))),!,atrace,throw(G).

% <justthat ...>
computeElement(Ctx,Votes,justthat,ATTRIBS,InnerXml,Output,VotesO):-!, computeElement(Ctx,Votes,input,[index=[2]|ATTRIBS],InnerXml,Output,VotesO).
computeElement(Ctx,Votes,justhat,ATTRIBS,InnerXml,Output,VotesO):-!, computeElement(Ctx,Votes,input,[index=[2]|ATTRIBS],InnerXml,Output,VotesO).

% <beforethat ...>
computeElement(Ctx,Votes,beforethat,ATTRIBS,InnerXml,Output,VotesO):-!, computeElement(Ctx,Votes,input,[index=[3]|ATTRIBS],InnerXml,Output,VotesO).

% <justbeforethat ...>
computeElement(Ctx,Votes,justbeforethat,ATTRIBS,InnerXml,Output,VotesO):-!, computeElement(Ctx,Votes,that,[index=[2,1]|ATTRIBS],InnerXml,Output,VotesO).

% <html:br/>
computeElement(Ctx,Votes,Htmlbr,ATTRIBS,Input,Output,VotesO):- atom(Htmlbr),atom_concat_safe('html:',Br,Htmlbr),!,
   computeElementMust(Ctx,Votes,html:Br,ATTRIBS,Input,Output,VotesO).
computeElement(Ctx,Votes,html:Br,ATTRIBS,Input,Output,VotesO):- atom(Br),
   computeElementMust(Ctx,Votes,Br,ATTRIBS,Input,Output,VotesO).

% <br/>
computeElement(_Ctx,Votes,br,[],[],'\n',Votes):-!.
% <p/>
computeElement(_Ctx,Votes,p,[],[],'\r\n',Votes):-!.

% <sr/>
computeElement(Ctx,Votes,sr,ATTRIBS,Input,Output,VotesO):- !,
   computeElementMust(Ctx,Votes,srai,ATTRIBS,[element(star,ATTRIBS,Input)],Output,VotesO).

computeElement(Ctx,Votes,Tag,ATTRIBS,Input,element(Tag,ATTRIBS,Output),VotesO):- isGenTemplate(Ctx,ATTRIBS),member(Tag,[srai,personf]),!,computeInnerTemplate(Ctx,Votes,Input,Output,VotesO).

% <srai/>s
computeElement(_Ctx,Votes,srai,ATTRIBS,[],result([],srai=ATTRIBS),VotesO):-atrace,!,VotesO is Votes * 0.6.

% <srai>s
computeElement(Ctx,Votes,srai,ATTRIBS,Input,Output,VotesO):- !, % for evalSRAI
  withAttributes(Ctx,ATTRIBS,((
    computeInnerTemplate(Ctx,Votes,Input,Middle,VotesM),!,
     prolog_must(computeSRAIElement(Ctx,VotesM,ATTRIBS,Middle,Output,VotesO))))),!.

% genTemplate strippers
computeElement(Ctx,Votes,Tag,ATTRIBS,Input,Output,VotesO):-member(Tag,[li,template,pre]),isGenTemplate(Ctx,ATTRIBS),!,computeInnerTemplate(Ctx,Votes,Input,Output,VotesO).

% <li...>
computeElement(Ctx,Votes,li,Preconds,InnerXml,OutProof,VotesO):- !, computeElement_li(Ctx,Votes,Preconds,InnerXml,OutProof,VotesO).

% <li> PASSED
computeElement_li(Ctx,Votes,Preconds,InnerXml,OutProof,VotesO):-
     precondsTrue(Ctx,Preconds),!,computeInnerTemplate(Ctx,Votes,InnerXml,Output,VotesM),VotesO is VotesM * 1.1,!,
     prolog_must(OutProof = proof(Output,li(Preconds))).

% <li> FAILED ==> []
computeElement_li(Ctx,Votes,Preconds,_InnerXml,OutProof,VotesO):-makeBlank(Ctx,Votes,failed(li(Preconds)),OutProof,VotesO),!.

  precondsTrue(Ctx,PC):-lastMember(name=Name,PC,WO),lastMember(value=Value,WO,Rest),!,precondsTrue0(Ctx,[Name=Value|Rest]).
  precondsTrue(_Ctx,PC):-PC==[];var(PC),!.
  precondsTrue(Ctx,PC):-precondsTrue0(Ctx,PC).

  precondsTrue0(_Ctx,PC):-PC==[];var(PC),!.
  precondsTrue0(Ctx,[NV|MORE]):-!,precondsTrue0(Ctx,MORE),!,precondsTrue0(Ctx,NV).
  precondsTrue0(Ctx,N=V):- peekNameValue(Ctx,user,N,Value,'$value'([])),!,(valuesMatch(Ctx,Value,V)->debugFmt(valuesMatch(Value,V));debugFmt(valuesMatch(not,Value,V))),valuesMatch(Ctx,Value,V).
  precondsTrue0(_Ctx,_NV):-atrace.

%%%%%%%%%%%%%%%%%%%%%
% <random...>
%%%%%%%%%%%%%%%%%%%%%
computeElement(Ctx,Votes,random,Attribs,List,AA,VotesO):-isGenTemplate(Ctx,Attribs),!,member(Pick,List),computeAnswer(Ctx,Votes,Pick,AA,VotesO).
computeElement(Ctx,Votes,random,_Attribs,List,AA,VotesO):-!,randomPick(List,Pick),computeAnswer(Ctx,Votes,Pick,AA,VotesO).

%%%%%%%%%%%%%%%%%%%%%
% <condition...>
%%%%%%%%%%%%%%%%%%%%%
computeElement(Ctx,Votes,condition,Attribs,List,AA,VotesO):-isGenTemplate(Ctx,Attribs),!,member(Pick,List),computeAnswer(Ctx,Votes,Pick,AA,VotesO).
computeElement(Ctx,Votes,condition,CondAttribs,InnerXml,Result,VotesO):-
     prolog_must(computeElement_condition(Ctx,Votes,CondAttribs,InnerXml,Result,VotesO)),!.

% <condition name="foo" value="bar"> (acts like a <li name="foo" value="bar">)
computeElement_condition(Ctx,Votes,CondAttribs,InnerXml,Result,VotesO):-
   copy_term(CondAttribs,CondAttribsCopy),
     attributesContainOneOf0(CondAttribsCopy,[value=_]),attributesContainOneOf0(CondAttribsCopy,[var=_,name=_]),!,
      prolog_must(prolog_may(computeAnswerMaybe(Ctx,Votes,element(li,CondAttribs,InnerXml),Result,VotesO));makeBlank(Ctx,Votes,failed(CondAttribs),Result,VotesO)),!.

% <condition name="foo"> <li value="bar"...>
computeElement_condition(Ctx,Votes,CondAttribs,InnerXml,Result,VotesO):-
   last(InnerXml,Last),
   copy_term(CondAttribs,CondAttribsCopy),
     not(attributesContainOneOf0(CondAttribsCopy,[value=_])),attributesContainOneOf0(CondAttribsCopy,[var=VarName,name=VarName]),!,
   lastKVMember(Ctx,[var,name],VarName,CondAttribs,_NEWCondAttribs),
   isValid(VarName),
   debugFmt(condition(varname=VarName,InnerXml)),
   withAttributes(Ctx, [name=VarName|CondAttribs],
      once((member(Pick,InnerXml),once((computeAnswerMaybe(Ctx,Votes,withAttributes(CondAttribs,Pick),Result,VotesO),isNonBlank(Result)))))
         ; (Result = Last,VotesO is Votes * 0.9)),!.

% <condition><li..>
computeElement_condition(Ctx,Votes,CondAttribs,InnerXml,Result,VotesO):-
  last(InnerXml,Last),
   withAttributes(Ctx, CondAttribs,
     once(
      once((member(Pick,InnerXml),once((computeAnswerMaybe(Ctx,Votes,withAttributes(CondAttribs,Pick),Result,VotesO),isNonBlank(Result)))))
         ; (Result = Last, VotesO is Votes * 0.9))),!.

makeBlank(_Ctx,Votes,Message,result([],Message),VotesO):- VotesO is Votes * 0.3 . %%%failed(CondAttribs)

attributesContainOneOf(CondAttribs,AllOf):-copy_term(CondAttribs,CondAttribsCopy),attributesContainOneOf0(CondAttribsCopy,AllOf),!.
attributesContainOneOf0(CondAttribsCopy,AllOf):-member(Find,AllOf),member(Find,CondAttribsCopy),!.
isNonBlank(Result):-answerOutput(Result,Stuff),!,nonvar(Stuff),Stuff\==[].

% <input/response/that index="1"...>
computeElement(Ctx,Votes,InputResponse,Attribs,InnerXml,Resp,VotesO):-member(InputResponse,[input,response,that]),!,
  lastMemberOrDefault(index=Index,Attribs,AttribsNew,['1']),
  prolog_must(computeMetaStar(Ctx,Votes,InputResponse,Index,AttribsNew,InnerXml,Resp,VotesO)).

% <gossip...>
computeElement(Ctx,Votes,gossip,_Attribs,Input,Output,VotesO):-!,computeAnswer(Ctx,Votes,Input,Output,VotesO).

% <think...>
computeElement(Ctx,Votes,think,Attribs,_Input,[],Votes):-isGenTemplate(Ctx,Attribs),!.
computeElement(Ctx,Votes,formatter,[type=[think]],_Input,[],Votes):-isGenTemplate(Ctx,[]),!.
computeElement(Ctx,Votes,think,_Attribs,Input,proof([],think=Hidden),VotesO):-!,computeInnerTemplate(Ctx,Votes,Input,Hidden,VotesO).

% <formatter type="prologcall">
computeElement(Ctx,Votes,formatter,Attribs,Input,Result,VotesO):-
      computeInnerTemplate(Ctx,Votes,Input,Mid,VotesO),
      lastMember(type=ProcI,Attribs,_NEW),listify(ProcI,[Proc|More]),atom(Proc),atomic_list_concat_aiml(['format_',Proc|More],Pred),
      functor(Callable,Pred,3),predicate_property(Callable,_),!,
      computeCall(Ctx,Pred,Mid,Result,'$error'),Result\=='$error'.

% <formatter type="sometag">
computeElement(Ctx,Votes,formatter,Attribs,Input,Result,VotesO):- !,
      computeInnerTemplate(Ctx,Votes,Input,Hidden,VotesO),
      lastMember(type=ProcI,Attribs,NEW),unlistify(ProcI,Proc),!,
      withAttributes(Ctx,NEW,computeElement(Ctx,Votes,Proc,NEW,Hidden,Result,VotesO)),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% <get,set,bot...>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

computeElement(Ctx,Votes,GetSetBot,Attribs,InnerXml,Resp,VotesO):-member(GetSetBot,[get,set,bot]),!,computeGetSet(Ctx,Votes,GetSetBot,Attribs,InnerXml,Resp,VotesM),VotesO is VotesM * 1.1,!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% for sure botvar-ish
% <version/id/date/size>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeElement(Ctx,Votes,BOT_ATOM,Attribs,InnerXml,element(BOT_ATOM,Attribs,InnerXml),Votes):- isGenTemplate(Ctx,Attribs), globalAliceTagVar(BOT_ATOM),!.
% HANDLE this in computeAnswer now convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=BOT_ATOM|ALIST],VV)):- globalAliceTagVar(BOT_ATOM),convert_template(Ctx,V,VV),!.
computeElement(Ctx,Votes,BOT_ATOM,[],[],proof(Resp,globalAliceTagVar(BOT_ATOM)),VotesO):- globalAliceTagVar(BOT_ATOM),!,expandVariable(Ctx,BOT_ATOM,Resp),VotesO is Votes  * 1.1.

% <topicstar,star,thatstar...>
computeElement(Ctx,Votes,StarTag,Attribs,InnerXml,Resp,VotesO):- hotrace(starType(StarTag,StarName)),!, %%atrace,
      computeStar(Ctx,Votes,StarName,Attribs,InnerXml,Resp,VotesM),VotesO is VotesM * 1.1,!.


computeElement(Ctx,Votes,Tag,Attribs,Input,element(Tag,Attribs,Input),Votes):-  isGenTemplate(Ctx,Attribs),verbatumGenTemplate(Tag),!.

verbatumGenTemplate(Tag):-member(Tag,[cycsystem,cyceval,cycquery,cycrandom,embed,img,script]).
verbatumGenTemplate(Tag):-member(Tag,[system,eval,load,learn]).


% <cycrandom...>
computeElement(Ctx,Votes,cycrandom,_Attribs,RAND,Output,VotesO):-!, computeAnswer(Ctx,Votes,cyceval(RAND),RO,VotesO),randomPick(RO,Output).

% <system...>
computeElement(Ctx,Votes,Tag,Attribs,Input,result(RESULT,Tag=EVAL),VotesO):-
   member(Tag,[system]),
   checkNameValue(peekNameValue,Ctx,Attribs,[lang],Lang, '$value'('bot')),
   computeInnerTemplate(Ctx,Votes,Input,EVAL,VotesO),
   systemCall(Ctx,Lang,EVAL,RESULT).

% <cyc..>
computeElement(Ctx,Votes,Tag,Attribs,Input,result(RESULT,Tag=EVAL),VotesO):-
   member(Tag,[cycsystem,cyceval,cycquery]),
   checkNameValue(peekNameValue,Ctx,Attribs,[lang],Lang,'$value'(Tag)),
   computeInnerTemplate(Ctx,Votes,Input,EVAL,VotesO),
   systemCall(Ctx,Lang,EVAL,RESULT).

% <template, pre ...>
computeElement(Ctx,Votes,Tag,Attribs, DOIT, result(OUT,Tag=Attribs), VotesO) :- member(Tag,[template,pre]), isGenTemplate(Ctx,Attribs), !,
  computeTemplate(Ctx,Votes,DOIT,OUT,VotesO).
computeElement(Ctx,Votes,Tag,Attribs, DOIT, result(OUT,Tag=Attribs), VotesO) :- member(Tag,[template,pre]), !,
  computeTemplate(Ctx,Votes,DOIT,OUT,VotesO).

% <uppercase, lowercase ...>
computeElement(Ctx,Votes,Tag,Attribs, Input, Output, VotesO) :- formatterProc(Tag),
   formatterTypeMethod(Tag,Type,Method),!,
   computeInnerTemplate(Ctx,Votes,Input,Mid,VotesO),
   computeCall(Ctx,Method,Mid,Output,prologCall(Method, result(Mid,element(Tag,[type=Type|Attribs])))),!.

% .... computeCall to formatter ....
computeCall(Ctx,Method,Mid,Output,ElseResult):-
   error_catch(prolog_must(call(Method,Ctx,Mid,Output)),
     E, (debugFmt(error(E,call(Method,Mid,Output))), atrace, Output = ElseResult)).
computeCall(_Ctx,_Pred,_Mid,Result,ElseResult):-prolog_must(Result=ElseResult).

% .... formatters ....
format_formal(_Ctx,In,Out):-toPropercase(In,Out),!.
format_sentence(_Ctx,[In1|In],[Out1|Out]):-In=Out,toPropercase(In1,Out1),!.
format_sentence(_Ctx,In,Out):-In=Out.
format_think(_Ctx,_In,Out):-[]=Out.
format_gossip(_Ctx,In,Out):-In=Out.
format_uppercase(_Ctx,In,Out):-toUppercase(In,Out),!.
format_lowercase(_Ctx,In,Out):-toLowercase(In,Out),!.


% <gender..>
computeElement(Ctx,Votes,Gender,Attribs,Input,Result,VotesO):- substitutionDictsName(Gender,DictName),!,
   computeElement_subst(Ctx,Votes,Gender,DictName,Attribs,Input,Result,VotesO).

% <gender|person2/>
computeElement_subst(Ctx,Votes,_Gender,DictName,Attribs,[],Result,VotesO):-
    computeElementMust(Ctx,Votes,star,Attribs,[],Hidden,VotesO),
    substituteFromDict(Ctx,DictName,Hidden,Result),!.

computeElement_subst(Ctx,Votes,_Gender,DictName,Attribs,Input,Result,VotesO):-
      withAttributes(Ctx,Attribs,((computeInnerTemplate(Ctx,Votes,Input,Hidden,VotesO),
      substituteFromDict(Ctx,DictName,Hidden,Result)))),!.

% <load...>
computeElement(Ctx,Votes,Tag,ATTRIBS,Input,result(RESULT,Tag=EVAL),VotesO):-
   member(Tag,[load]),!,
   computeInnerTemplate(Ctx,Votes,Input,EVAL,VotesO),
   tag_eval(Ctx,element(Tag,ATTRIBS,EVAL),RESULT).

% <learn...>
computeElement(Ctx,Votes,Tag,ATTRIBS, NEWXML, result([learned,Diff,new,patterns]),Votes) :-
  member(Tag,[learn]),!,
       NEW = element(aiml,ATTRIBS,NEWXML),
       aimlCateSig(CateSig),
       predicate_property(CateSig,number_of_clauses(Before)),
        withAttributes(Ctx,ATTRIBS, load_aiml_structure(Ctx,NEW)),!,
           predicate_property(CateSig,number_of_clauses(After)),
           Diff is After - Before.

% <aiml/topic/category...>
computeElement(Ctx,Votes,Tag,ATTRIBS, NEWXML, result([learned,Tag,Diff,new,patterns]),Votes) :- member(Tag,[aiml,topic,category]),!,
       NEW = element(Tag,ATTRIBS,NEWXML),
       aimlCateSig(CateSig),
       predicate_property(CateSig,number_of_clauses(Before)),
        withAttributes(Ctx,ATTRIBS, load_aiml_structure(Ctx,NEW)),!,
           predicate_property(CateSig,number_of_clauses(After)),
           Diff is After - Before.

% <eval...>
computeElement(Ctx,Votes,Tag,ATTRIBS, DOIT, RESULT, Votes) :- member(Tag,[eval]),!,
      withAttributes(Ctx,ATTRIBS,aiml_eval(Ctx,DOIT,RESULT)),!.

% other evals
computeElement(Ctx,Votes,Tag,ATTRIBS,Input,RESULT,VotesO):- evaluatorTag(Tag),
   computeInnerTemplate(Ctx,Votes,Input,EVAL,VotesO),!,
   tag_eval(Ctx,element(Tag,ATTRIBS,EVAL),RESULT),!.

% maybe not for sure botvar-ish
% <favfood/master>
% HANDLE this in computeAnswer now convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=BOT_ATOM|ALIST],VV)):- globalAliceTagVar(BOT_ATOM),convert_template(Ctx,V,VV),!.
computeElement(Ctx,Votes,BOT_ATOM,[],[],proof(Resp,globalAliceTagVar(BOT_ATOM)),VotesO):- expandVariable(Ctx,BOT_ATOM,Resp),!, VotesO is Votes  * 1.1.

% rewrites
computeElement(_Ctx,Votes,Tag,[],[],result([reply,from,tag,Tag],element(Tag,[],[])),Votes):-!.
computeElement(_Ctx,Votes,Tag,Attribs,[],result([reply,from,Tag|Attribs],Tag,Attribs),Votes):-!,atrace.
computeElement(Ctx,Votes,Tag,Attribs,InnerXml,Resp,VotesO):-
  GETATTRIBS = element(Tag,Attribs,InnerXml),
  convert_element(Ctx,GETATTRIBS,GETATTRIBS0),
  GETATTRIBS \== GETATTRIBS0,!,
  %%atrace,
  convert_element(Ctx,GETATTRIBS,_GETATTRIBS1),
  computeAnswer(Ctx,Votes,GETATTRIBS0, Resp,VotesO).


% ===============================================================================================
% Compute Star
% ===============================================================================================

starName(get_star,star):-!.
starName(StarStar,StarStar):- atom_concat(_,'star',StarStar),!.
starName(Star,StarStar):- atom_concat(Star,'star',StarStar),!.

starElementName(starstar,star).
starElementName(pattern,star).
starElementName(get_star,star).
starElementName(star,star).
starElementName(patternstar,star).
starElementName(StarStar,StarStar):- atom_concat(_Star,'star',StarStar),!.
starElementName(Star,StarStar):- atom_concat(Star,'star',StarStar),!.

:-dynamic(inGenOutput/0).
isGenerateUnknown(_Ctx,_ATTRIBS):-!,inGenOutput,!.
isGenerateUnknown(Ctx,ATTRIBS):- peekNameValue(Ctx,ATTRIBS,generateUnknownVars,GenerateUnknown,'false'),!,GenerateUnknown=true.
isGenTemplate(_Ctx,_ATTRIBS):-!,inGenOutput,!.
isGenTemplate(Ctx,Attribs):-peekNameValue(Ctx,Attribs,generateTemplate,GenTempl,'$failure'),GenTempl=true.

computeStar(Ctx,Votes,Star,Attribs,InnerXml,Resp,VotesO):-
   isGenTemplate(Ctx,Attribs),!,VotesO=Votes,
   starElementName(Star,EName),Resp=element(EName,Attribs,InnerXml),!.

computeStar(Ctx,Votes,Star,Attribs,InnerXml,Resp,VotesO):-
   starName(Star,StarStar),Star \== StarStar,!,
   computeStar(Ctx,Votes,StarStar,Attribs,InnerXml,Resp,VotesO),!.

computeStar(Ctx,Votes,Star,Attribs,InnerXml,Resp,VotesO):-
    lastMember(index=Index,Attribs,AttribsNew),!,
    computeStar1(Ctx,Votes,Star,Index,AttribsNew,InnerXml,Resp,VotesO),!.

computeStar(Ctx,Votes,Star,Attribs,InnerXml,Resp,VotesO):-
    computeStar1(Ctx,Votes,Star,[1],Attribs,InnerXml,Resp,VotesO),!.

computeStar1(Ctx,Votes,Star,Major,ATTRIBS,InnerXml,Proof,VotesO):-atomic(Major),!,
    computeStar1(Ctx,Votes,Star,[Major],ATTRIBS,InnerXml,Proof,VotesO),!.

computeStar1(Ctx,Votes,Star,Index,ATTRIBS,_InnerXml,proof(ValueO,StarVar=ValueI),VotesO):- is_list(Index),
      CALL=atomic_list_concat_aiml([Star|Index],StarVar),
      prolog_must(error_catch(CALL,E,(debugFmt(CALL->E),fail))),
   getDictFromAttributes(Ctx,'evalsrai',ATTRIBS,Dict),
   computeStar2(Ctx,Votes,Dict,ATTRIBS,StarVar,ValueI,ValueO,VotesM),!,
   VotesO is VotesM * 1.1.

computeStar1(_Ctx,Votes,Star,Index,ATTRIBS,InnerXml,Resp,VotesO):-
      traceIf(Resp = result(InnerXml,Star,Index,ATTRIBS)),!,VotesO is Votes * 1.1.

computeStar2(Ctx,Votes,Dict,ATTRIBS,StarVar,ValueI,ValueO,VotesM):-
   getStoredStarValue(Ctx,Dict,StarVar,ValueI),
   %%ValueO=ValueI,VotesM=Votes,
   computeTemplate(Ctx,Votes,element(template,ATTRIBS,ValueI),ValueO,VotesM),
   !.

getStoredStarValue(Ctx,_Dict,StarVar,ValueI):-current_value(Ctx,StarVar,ValueI),!.
getStoredStarValue(Ctx,Dict,StarVar,ValueI):-getStoredValue(Ctx,Dict,StarVar,ValueI),!.
getStoredStarValue(_Ctx,Dict,StarVar,[starvar,StarVar,Dict]):-!,unify_listing(dict(Dict,_,_)),atrace.


computeMetaStar(Ctx,Votes,Star,Index,ATTRIBS,InnerXml,Resp,VotesO):-computeMetaStar0(Ctx,Votes,Star,Index,ATTRIBS,InnerXml,Resp,VotesO),!.

computeMetaStar0(Ctx,Votes,Star,MajorMinor,ATTRIBS,InnerXml,Resp,VotesO):-isGenTemplate(Ctx,ATTRIBS),!,VotesO=Votes,Resp=element(Star,[index=MajorMinor|ATTRIBS],InnerXml).

computeMetaStar0(Ctx,Votes,Star,MajorMinor,ATTRIBS,_InnerXml,proof(ValueO,Star=ValueI),VotesO):-
      getDictFromAttributes(Ctx,'evalsrai',ATTRIBS,Dict),
      getIndexedValue(Ctx,Dict,Star,MajorMinor,ValueI),!,
      computeInnerTemplate(Ctx,Votes,element(template,ATTRIBS,[ValueI]),ValueO,VotesM),VotesO is VotesM * 1.1.

computeMetaStar0(_Ctx,Votes,Star,Index,ATTRIBS,InnerXml,Resp,VotesO):- atrace,
      traceIf(Resp = result(InnerXml,Star,Index,ATTRIBS)),!,VotesO is Votes * 0.9.

%%%%%%  peekNameValue(Ctx,Scope,Name,Value,else). %%
getDictFromAttributes(Ctx,VarHolder,_ATTRIBS,SYM):-current_value(Ctx,VarHolder,SYM).
getDictFromAttributes(_Ctx,_VarHolder,_ATTRIBS,'user'):-!. %%atrace.

% ===============================================================================================
% Compute Get/Set Probilities
% ===============================================================================================
/*

computeGetSet(Ctx,Votes,GetSetBot,ATTRIBS,[],Resp,VotesO):- !,computeGetSet(Ctx,Votes,GetSetBot,user,ATTRIBS,Resp,VotesO).
computeGetSet(Ctx,Votes,GetSetBot,name=NAME,MORE,Resp,VotesO):- !, computeGetSet(Ctx,Votes,GetSetBot,user,[name=NAME|MORE],Resp,VotesO).
computeGetSet(Ctx,Votes,GetSetBot,WHO,[X],Resp,VotesO):- !,computeGetSet(Ctx,Votes,GetSetBot,WHO,X,Resp,VotesO).
computeGetSet(Ctx,Votes,GetSetBot,ATTRIBS,Value,Resp,VotesO):- delete(ATTRIBS,type=bot,NEW),!,computeGetSet(Ctx,Votes,bot=GetSetBot,NEW,Value,Resp,VotesO).
computeGetSet(Ctx,Votes,GetSetBot,TYPE,[],Resp,VotesO):- !,computeGetSet(Ctx,Votes,GetSetBot,user,TYPE,Resp,VotesO).

*/

computeGetSet(Ctx,Votes,bot,ATTRIBS,InnerXml,Resp,VotesO):- !, computeGetSetVar(Ctx,Votes,bot,get,_VarName,ATTRIBS,InnerXml,Resp,VotesO),!.

computeGetSet(Ctx,Votes,GetSet,ATTRIBS,InnerXml,Resp,VotesO):- computeGetSetVar(Ctx,Votes,user,GetSet,_VarName,ATTRIBS,InnerXml,Resp,VotesO),!.

dictVarName(N):-member(N,[dictionary,dict,userdict,type,user,botname,username,you,me]).
% tests the dictionary contains at least one value
dictFromAttribs(Ctx,ATTRIBS,Dict,NEW):-dictVarName(N),lastMember(N=DictV,ATTRIBS,NEW),convert_dictname(Ctx,DictV,Dict),getContextStoredValue(Ctx,Dict,_Name,Value),valuePresent(Value),!.
dictFromAttribs(Ctx,ATTRIBS,Dict,NEW):-dictVarName(N),lastMember(N=DictV,ATTRIBS,NEW),atrace,convert_dictname(Ctx,DictV,Dict),!,atrace.

lastKVMember(_Ctx,Keys,Value,ATTRIBS,NEW):-member(N,Keys),lastMember(N=Value,ATTRIBS,NEW),prolog_must(isValid(Value)),!.
lastKVMember(Ctx,Keys,Value,ATTRIBS,ATTRIBS):-member(N,Keys),current_value(Ctx,N,Value),prolog_must(isValid(Value)),!.
lastKVMember(Ctx,Keys,Value,ATTRIBS,ATTRIBS):-member(N,Keys),peekNameValue(Ctx,ATTRIBS,N,Value,'$failure'),prolog_must(isValid(Value)),!.

%%computeGetSetVar(Ctx,Votes,_Dict,bot,VarName,ATTRIBS,InnerXml,Resp,VotesO):- !,computeGetSetVar(Ctx,Votes,user,get,VarName,ATTRIBS,InnerXml,Resp,VotesO).
%% computeGetSetVar(Ctx,Votes,Dict,GetSetBot,VarName,ATTRIBS,InnerXml,Resp,VotesO).


computeGetSetVar(Ctx,Votes,Dict,GetSet,OVarName,ATTRIBS,InnerXml,Resp,VotesO):- atom(ATTRIBS),ATTRIBS \= [],!, VarName = ATTRIBS,
   nop(debugFmt(computeGetSetVarName(GetSet,Dict:OVarName->VarName))),
     computeGetSetVar(Ctx,Votes,Dict,GetSet,VarName,[],InnerXml,Resp,VotesO),!.

computeGetSetVar(Ctx,Votes,Dict,GetSet,OVarName,ATTRIBS,InnerXml,Resp,VotesO):-
      member(N,[var,name]),lastMember(N=VarName,ATTRIBS,NEW),!,
   nop(debugFmt(computeGetSetVarDict(GetSet,Dict:OVarName->VarName))),
      computeGetSetVar(Ctx,Votes,Dict,GetSet,VarName,NEW,InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,OldDict,GetSet,VarName,ATTRIBS,InnerXml,Resp,VotesO):-
     dictFromAttribs(Ctx,ATTRIBS,Dict,NEW),
   nop(debugFmt(computeGetSetVarDict2(GetSet,OldDict->Dict,VarName))),
     %% MAYBE NEED THIS LATER ((member(EVarName,VarName),delete(ATTRIBS,EVarName,ATTRIBSOUT));ATTRIBSOUT=ATTRIBS),
      computeGetSetVar(Ctx,Votes,Dict,GetSet,VarName,NEW,InnerXml,Resp,VotesO).

computeGetSetVar(Ctx,Votes,_Dict,'set',OVarName,Attribs,_InnerXml,OVarName,Votes):-isGenTemplate(Ctx,Attribs),getStoredValue(Ctx,setReturn(_Default),OVarName,NameOrValue),NameOrValue=name,!.
computeGetSetVar(Ctx,Votes,_Dict,'set',_OVarName,Attribs,InnerXml,Resp,VotesO):-isGenTemplate(Ctx,Attribs),!,computeTemplate(Ctx,Votes,InnerXml,Resp,VotesO).
computeGetSetVar(Ctx,Votes,Dict,GetSetBot,OVarName,Attribs,InnerXml,Resp,VotesO):-isGenTemplate(Ctx,Attribs),!,VotesO=Votes,Resp=element(GetSetBot,[dict=Dict,ovar=OVarName|Attribs],InnerXml).

computeGetSetVar(Ctx,Votes,Dict,get,VarName,ATTRIBS,_InnerXml,proof(ValueO,VarName=ValueI),VotesO):-
      getAliceMemComplete(Ctx,Dict,VarName,ValueI),!,
      computeAnswer(Ctx,Votes,element(template,ATTRIBS,ValueI),ValueO,VotesM),VotesO is VotesM * 1.1.

% GET no value found
computeGetSetVar(Ctx,Votes,Dict,get,VarName,ATTRIBS,_InnerXml,proof(ReturnValueO,Dict:VarName='OM',ATTRIBS),VotesO):-!,VotesO is Votes * 0.7,
     once(isGenerateUnknown(Ctx,ATTRIBS) -> DefaultEmpty=[unKnowN,VarName,of,Dict];DefaultEmpty=[]),
     lastMemberOrDefault('default'=DefaultValue,ATTRIBS,_AttribsNew,DefaultEmpty),
     returnNameOrValue(Ctx,Dict,VarName,DefaultValue,ReturnValueO),!.

computeGetSetVar(Ctx,Votes,Dict,set,VarName,ATTRIBS,InnerXml,proof(ReturnValue,VarName=InnerXml),VotesO):-!,
      computeAnswer(Ctx,Votes,element(template,ATTRIBS,InnerXml),ValueM,VotesM),
      computeInnerTemplate(Ctx,VotesM,ValueM,ValueO,VotesMO),
      ensureValue(ValueO,ValueOO),
      setAliceMem(Ctx,Dict,VarName,ValueOO),!,
      returnNameOrValue(Ctx,Dict,VarName,ValueO,ReturnValue),
      VotesO is VotesMO * 1.1.

%%computeGetSetVar(_Ctx,_Votes,_Get,_,_,_,_):-!,fail.

returnNameOrValue(Ctx,IDict,VarNameI,Value,ReturnValue):-dictNameDictNameC(Ctx,IDict,VarNameI,Scope,Name),!,returnNameOrValue(Ctx,Scope,Name,Value,ReturnValue).
returnNameOrValue(Ctx,_Dict,VarName,ValueO,ReturnValueO):-
      once(getStoredValue(Ctx,setReturn(_Default),VarName,NameOrValue);NameOrValue=value),
      returnNameOrValue0(NameOrValue,VarName,ValueO,ReturnValue),!,listify(ReturnValue,ReturnValueO).

returnNameOrValue0([NameOrValue],VarName,ValueO,ReturnValue):-!,returnNameOrValue0(NameOrValue,VarName,ValueO,ReturnValue),!.
returnNameOrValue0(name,VarName,_ValueO,VarName).
returnNameOrValue0(_Value,_VarName,ValueO,ValueO).


% ===============================================================================================
% Compute Answer Probilities
% ===============================================================================================
computeAnswer(Ctx,Votes,IN,Result,VotesOut):-computeAnswerND(Ctx,Votes,IN,Result,VotesOut),!.

:-discontiguous(computeAnswerND/5).

computeAnswerND(Ctx,Votes,IN,Result,VotesOut):- not(tracing),computeAnswer0(Ctx,Votes,IN,Result,VotesOut),fail.

computeAnswer0(Ctx,Votes,IN,Result,VotesOut):- prolog_must((number(Votes),nonvar(IN),var(Result),var(VotesOut))),
      nop(debugFmt(computeAnswer(Ctx,Votes,IN,Result,VotesOut))),fail.

computeAnswer0(Ctx,Votes,MidVote - In,Out,VotesO):- prolog_must(nonvar(MidVote)),
                           atrace, !, computeAnswer(Ctx,Votes,In,Out,VotesA), VotesO is VotesA * MidVote.

computeAnswer0(_Ctx,Votes,_I,_,_):-(Votes>20;Votes<0.3),!,fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% elements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

% element inner reductions
computeAnswer_1_disabled(Ctx,Votes, element(Tag, ATTRIBS, [DO|IT]), OUT, VotesO) :- recursiveTag(Tag),not(DO=(_-_)),!,
     appendAttributes(Ctx,ATTRIBS, [computeAnswer=[side_effects_allow=[transform],intag=Tag]], ATTRIBS_NEW),
       withAttributes(_Ctx,ATTRIBS_NEW, findall(Each,((member(In,[DO|IT]),computeInner(Ctx,Votes, In, Each))),INNERDONE)),
       computeElementMust(Ctx,Votes,Tag, ATTRIBS, INNERDONE, OUT, VotesO).

computeAnswerND(Ctx,Votes, element(Tag, ATTRIBS, [DO|IT]), OUT, VotesO) :- recursiveTag(Tag),not(DO=(_-_)),!,
     appendAttributes(Ctx,ATTRIBS, [computeAnswer=[side_effects_allow=[transform],intag=Tag]], ATTRIBS_NEW),
         withAttributes(Ctx,ATTRIBS_NEW, computeInput(Ctx, Votes,[DO|IT],INNERDONE)),
       prolog_mostly_ground((INNERDONE)),
       computeElementMust(Ctx,Votes,Tag, ATTRIBS, INNERDONE, OUT, VotesO).

computeAnswerND(Ctx,Votes,element(Tag,Attribs,List),Out,VotesO):-!,computeElement(Ctx,Votes,Tag,Attribs,List,Out,VotesO).

% never gets here due to element/3 cutted above
computeAnswerND(Ctx,Votes,element(Tag,Attribs,List),Output,VotesO):- computeElement(Ctx,Votes,Tag,Attribs,List,Output,VotesO),!.
computeAnswerND(Ctx,Votes,element(Tag,Attribs,List),Output,VotesO):-atrace,computeElement(Ctx,Votes,Tag,Attribs,List,Output,VotesO),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% strings (must happen before list-check)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswerND(Ctx,Votes,String,Out,VotesO):-string(String),string_to_atom(String,Atom),!,computeAnswer(Ctx,Votes,Atom,Out,VotesO).
computeAnswerND(_Ctx,Votes,String,Atom,Votes):-is_string(String),toCodes(String,Codes),!,from_atom_codes(Atom,Codes),!.
computeAnswerND(_Ctx,Votes,'$stringCodes'(List),AA,Votes):-!,from_atom_codes(AA,List),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% list-check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% (should be no stars)
computeAnswerND(_Ctx,_Votes,Pattern,_,_):- traceIf(isStarValue(Pattern)),fail.

computeAnswerND(_Ctx,Votes,[],[],Votes):-!.
computeAnswerND(Ctx,Votes,[A|B],OO,VotesO):-
    atomic(A) ->
      (!,computeTemplate(Ctx,Votes,B,BB,VotesO),OO=[A|BB]) ;
      computeTemplate(Ctx,Votes,[A|B],OO,VotesO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% atomic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswerND(Ctx,Votes,randomsentence,Output,VotesO):-!, choose_randomsentence(X),!,computeAnswer(Ctx,Votes,X,Output,VotesO).

computeAnswerND(Ctx,Votes,In,Out,Votes):-atomic(In),expandVar(Ctx,In,Out).
computeAnswerND(_Ctx,Votes,Resp,Resp,Votes):-atomic(Resp),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% prologCall
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswerND(Ctx,Votes,prologCall(Method,Stuff),Resp,VotesO):-
  computeAnswer(Ctx,Votes,Stuff,Mid,VotesO),
  call(Method,Ctx,Mid,Resp),!.

computeAnswerND(_Ctx,Votes,prologCall(Method),Resp,VotesO):- atrace,
   once(call(Method)->(Resp=pass(Method),VotesO=Votes);(Resp=failed(Method),VotesO is Votes*0.5)),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% Star Compounds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
computeAnswerND(Ctx,Votes,star(Star,Attribs,InnerXml),Output,VotesO):- computeStar(Ctx,Votes,Star,Attribs,InnerXml,Output,VotesO),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% withAttributes Compounds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

computeAnswerND(Ctx,Votes,Input,Output,VotesO):- functor(Input,withAttributes,_),
  Input = withAttributes(OuterAttribs,element(Tag,Attribs,InnerXml)),
  append(Attribs,OuterAttribs,AllAttribs),
  withAttributes(Ctx,OuterAttribs,once(computeAnswer(Ctx,Votes,element(Tag,AllAttribs,InnerXml),Output,VotesO);Failed=failed)),!,Failed \== failed.

computeAnswerND(Ctx,Votes,withAttributes(OuterAttribs,InnerXml),Output,VotesO):-
  withAttributes(Ctx,OuterAttribs,once(computeAnswer(Ctx,Votes,InnerXml,Output,VotesO);Failed=failed)),!,Failed \== failed.

computeAnswerND(Ctx,Votes,compute(InnerXml),Output,VotesO):-!, computeAnswer(Ctx,Votes,InnerXml,Output,VotesO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% Result or Proof already
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

computeAnswerND(_Ctx,Votes,Res, Res,Votes):-resultOrProof(Res,_Mid),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% Other Compounds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

computeAnswerND(Ctx,Votes,GETATTRIBS, Resp,VotesO):-
  convert_element(Ctx,GETATTRIBS,GETATTRIBS0),
  GETATTRIBS \== GETATTRIBS0,!,
  atrace,
  convert_element(Ctx,GETATTRIBS,_GETATTRIBS1),
  computeAnswer(Ctx,Votes,GETATTRIBS0, Resp,VotesO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
% errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

computeAnswerND(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET], isAimlTag(GET), !, computeElementMust(Ctx,Votes,GET,[],[],Resp,VotesO).
computeAnswerND(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET,ATTRIBS], isAimlTag(GET), !, computeElementMust(Ctx,Votes,GET,ATTRIBS,[],Resp,VotesO).
computeAnswerND(Ctx,Votes,GETATTRIBS, Resp,VotesO):- GETATTRIBS=..[GET,ATTRIBS,INNER], isAimlTag(GET), !, computeElementMust(Ctx,Votes,GET,ATTRIBS,INNER,Resp,VotesO).

computeAnswerND(_Ctx,Votes,Resp,Resp,Votes):-atrace,aiml_error(computeAnswer(Resp)).

% ===============================================================================================
% Run answer procs
% ===============================================================================================

choose_randomsentence(X):-
	repeat,
		retract(random_sent(Y)),
		assertz(random_sent(Y)),
		4 is random(10),!,
		Y=X.

% ===============================================================================================
% Get and rember Last Said
% ===============================================================================================
%% using dict now :-dynamic(getLastSaid/1).
%%:-setAliceMem(_,_,that,(['where',am,'I'])).

rememberSaidIt(_Ctx,[]):-!.
rememberSaidIt(Ctx,_-R1):-!,rememberSaidIt(Ctx,R1).
rememberSaidIt(Ctx,R1):-append(New,'.',R1),!,rememberSaidIt(Ctx,New).
rememberSaidIt(Ctx,R1):-answerOutput(R1,SR1),R1\==SR1,!,rememberSaidIt(Ctx,SR1).
rememberSaidIt(Ctx,R1):- !,
   getAliceMem(Ctx,'bot','you',User),
   getAliceMem(Ctx,'bot','me',Robot),
   rememberSaidIt_SH(Ctx,R1,Robot,User).

rememberSaidIt_SH(_Ctx,[],_Robot,_User):-!.
rememberSaidIt_SH(Ctx,_-R1,Robot,User):-!,rememberSaidIt_SH(Ctx,R1,Robot,User).
rememberSaidIt_SH(Ctx,R1,Robot,User):-append(New,[Punct],R1),sentenceEnderOrPunct_NoQuestion(Punct),!,rememberSaidIt_SH(Ctx,New,Robot,User).
rememberSaidIt_SH(Ctx,R1,Robot,User):-answerOutput(R1,SR1),!,
   setAliceMem(Ctx,Robot,'lastSaid',SR1),
   pushInto1DAnd2DArray(Ctx,'response','that',5,SR1,User).

getRobot(Robot):-atrace,getAliceMem(_Ctx,'bot','me',Robot),!.

getLastSaid(LastSaid):-getRobot(Robot),getAliceMem(_Ctx,Robot,'lastSaid',LastSaid).

getLastSaidAsInput(LastSaidMatchable):-getLastSaid(That),convertToMatchable(That,LastSaidMatchable),!.

% set some sane defaults to be overiden in config.xmls
:-prolog_must(setAliceMem('bot','me','bot')).
:-setAliceMem('bot','you','user').
:-setAliceMem('bot','name',['The','Robot']).
:-setAliceMem('bot',version,'1.0.1').
:-setAliceMem('user','name',['Unknown','Partner']).
:-setAliceMem('user','me','user').
:-setAliceMem('user','is_type','agent').
:-setAliceMem('bot','is_type','agent').
:-setAliceMem('default','is_type','role').
:-setAliceMem('bot','infinite-loop-input',['INFINITE','LOOP']).
%%:-setAliceMem(substitutions(_DictName),'is_type','substitutions').


% ===============================================================================================
% Template Output to text
% ===============================================================================================

%% <template> Used as a return result (non text maybe)(
computeTemplateOutput(Ctx,Votes,Input,Output,VotesO):-prolog_must(computeTemplate(Ctx,Votes,Input,Output,VotesO)).

%% like the above howeverr since its job is to be submitted to anohjter evaluator.. it needs to make "text" ussually
computeInnerTemplate(Ctx,Votes,Input,Output,VotesO):-
    prolog_mustEach((computeTemplateOutput(Ctx,Votes,Input,Mid,VotesO),answerOutput(Mid,Output))).

answerOutput(Output,NonVar):-nonvar(NonVar),answerOutput(Output,Var),!,valuesMatch(_Ctx,Var,NonVar).
answerOutput(Output,[Output]):-var(Output),!.
answerOutput([],Output):- !, Output=[].
%answerOutput(Output,Split):-atom(Output),atomWSplit(Output,Split),Split==[Output],!.
%answerOutput(Output,Split):-atom(Output),atrace,atomWSplit(Output,Split),!.
answerOutput('<br/>',['\n']):-!.
answerOutput('<p/>',['\r\n']):-!.
answerOutput(element('br',[],[]),['\n']):-!.
answerOutput(Output,[Output]):-atomic(Output),!.
answerOutput([<,BR,/,>|B],OO):-atom(BR),!,
  answerOutput([element(BR,[],[])|B],OO),!.
answerOutput([A|AA],Output):-!,
   answerOutput(A,B),
   answerOutput(AA,BB),
   flatten([B,BB],Output).

answerOutput(element(template,[],InnerXML),Output):- answerOutput(InnerXML,Output),!.

answerOutput(element(Tag,Attribs,InnerXML),[element(Tag,Attribs,Output)]):- answerOutput(InnerXML,Output),!.
answerOutput(star(Tag,Attribs,InnerXML),[star(Tag,Attribs,Output)]):- answerOutput(InnerXML,Output),!.
answerOutput(Term,Output):-resultOrProof(Term,Mid),!,answerOutput(Mid,Output).
answerOutput(Term,Output):-compound(Term),Term=..[_,Mid|_],debugFmt(answerOutput(Term->Mid)),!,answerOutput(Mid,Output).
answerOutput(Output,[Output]):-!.

lastMemberOrDefault(E,L,N,_D):-lastMember(E,L,N),!.
lastMemberOrDefault(_Named=E,L,N,D):-L=N,E=D,!.
lastMemberOrDefault(E,L,N,D):-L=N,E=D.


% ===================================================================
% Substitution based on Pred like sameWordsDict(String,Pattern).
% ===================================================================

convert_substs(A,D):-simplify_atom0(A,M),A\==M,!,convert_substs(M,D).
convert_substs(A,D):-A=D.

simplify_atom0(A,A):-A==[],!.
simplify_atom0(A0,DD):- is_list(A0),joinAtoms(A0,' ',A),!,simplify_atom0(A,D),!,atomWSplit(D,DD),!.
simplify_atom0(A,D):- atom(A),!,literal_atom(A,B),atomic_list_concat_aiml(L0,'\\b',B),delete(L0,'',L),joinAtoms(L,' ',C),!,atomWSplit(C,D),!.


sameWordsDict([String|A],[Pattern|B]):-!,sameWordsDict0(String,Pattern),!,sameWordsDict_l(A,B),!.
sameWordsDict(String,Pattern):-sameWordsDict0(String,Pattern),!.

sameWordsDict_l([String|A],[Pattern|B]):-sameWordsDict0(String,Pattern),sameWordsDict_l(A,B),!.
sameWordsDict_l([],[]):-!.

sameWordsDict0(verbatum(_String),_):-!,fail.
sameWordsDict0(_,verbatum(_Pattern)):-!,fail.
sameWordsDict0(String,Pattern):-compound(String),
   prolog_must((answerOutput_atom(String,String1),debugFmt(interactStep(String,String1,Pattern)),String \== String1)),!,
   sameWordsDict0(String1,Pattern).

sameWordsDict0(String,Pattern):-compound(Pattern),
   prolog_must((answerOutput_atom(Pattern,Pattern1),debugFmt(interactStep(Pattern,Pattern1,String)), Pattern \== Pattern1)),!,
   sameWordsDict0(String,Pattern1).

sameWordsDict0(String,Pattern):-String==Pattern,!,debugFmt(sameWordsDict(String,Pattern)).
%sameWordsDict0(String,Pattern):-debugFmt(sameWordsDict0(String,Pattern)),wildcard_match(Pattern,String),!.
%sameWordsDict0(String,Pattern):-dwim_match(Pattern,String),!.

answerOutput_atom(Pattern,Pattern1):-unresultify(Pattern,Pattern0),unlistify(Pattern0,Pattern1),!,prolog_must(atomic(Pattern1)).

dictReplace(DictName,Before,After):-dict(substitutions(DictName),Before,After).%%convert_substs(Before,B),convert_template(Ctx,After,A).

substituteFromDict(Ctx,DictName,Hidden,Output):-answerOutput(Hidden,Mid),Hidden\==Mid,!,substituteFromDict(Ctx,DictName,Mid,Output),!.

substituteFromDict(Ctx,DictName,Hidden,Output):- dictReplace(DictName,_,_),prolog_must(substituteFromDict_l(Ctx,DictName,Hidden,Output)),!.

substituteFromDict(_Ctx,DictName,Hidden,Output):- dictReplace(DictName,_,_),!,
      recorda(DictName,Hidden),
      forall(dictReplace(DictName,Before,After),
          (recorded(DictName,Start,Ref),
          erase(Ref),
          pred_subst(sameWordsDict,Start,Before,verbatum(After),End),
          recorda(DictName,End))),
      recorded(DictName,Output,Ref),
      debugFmt(substituteFromDict(Hidden,Output)),
      erase(Ref),!.

substituteFromDict(Ctx,DictName,Hidden,Result):- isGenTemplate(Ctx,[]),!,Result=[substs,DictName,on,Hidden].
substituteFromDict(_Ctx,DictName,Hidden,result([substs,DictName,on,Hidden],Result)):-Result=..[DictName,Hidden].

substituteFromDict_l(_Ctx,_DictName,Hidden,Output):-atomic(Hidden),!,Hidden=Output.
substituteFromDict_l(Ctx,DictName,[V|Hidden],[V|Output]):-verbatum(_)==V,!,substituteFromDict_l(Ctx,DictName,Hidden,Output).
substituteFromDict_l(Ctx,DictName,Hidden,[verbatum(After)|Output]):-dictReplace(DictName,Before,After),
   debugOnError((length(Before,Left),length(NewBefore,Left))),
   append(NewBefore,Rest,Hidden),sameBinding(NewBefore,Before),!,substituteFromDict_l(Ctx,DictName,Rest,Output).
substituteFromDict_l(Ctx,DictName,[V|Hidden],[V|Output]):-substituteFromDict_l(Ctx,DictName,Hidden,Output).

:- addScopeParent(toplevel,cateFallback).
:- addScopeParent(filelevel,toplevel).
:- addScopeParent(category,filelevel).
:- prolog_mustEach((cateFallback(ATTRIBS), pushAttributes(_Ctx,cateFallback,ATTRIBS))).
%%:- prolog_mustEach((cateFallback(ATTRIBS), popAttributes(_Ctx,cateFallback,ATTRIBS), !)).
%%:- cateFallback(ATTRIBS), pushAttributes(_Ctx,cateFallback,ATTRIBS).

% run main loop if this was the toplevel file
do_main_if_load:- current_prolog_flag(associated_file,File),file_base_name(File, 'logicmoo_module_aiml.pl')->main_loop;true.

