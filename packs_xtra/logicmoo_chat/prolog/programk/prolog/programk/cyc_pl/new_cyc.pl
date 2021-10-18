% ===================================================================
% File 'cyc.pl'
% Purpose: Lower-level connection based utilities for interfacing to CYC from SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'cyc.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

:-module(cyc,[
	 cycInit/0,
	 getCycConnection/4,
	 finishCycConnection/3,
	 converse/1,
	 converse/2,
	 %evalSubL/2,
	 evalSubL/3,
	 converseRaw/2,
	 cycInfo/0,
         toCycVar/3,
         toCycVar/2,
	 formatCyc/3,
	 toCycApiExpression/2,
	 toCycApiExpression/3,
         %transparent isCycConstantGuess/1,
	 cycQuery/1,
	 cycQuery/2,
	 cycQuery/6,
	 cycQuery/8,
	 toMarkUp/4,
	 cycAssert/1,
	 cycAssert/2,
	 cycRetract/1,
	 is_string/1,
	 balanceBinding/2,
	 cycRetract/2,
	 cycRetractAll/1,
	 cycRetractAll/2,
	 %transparent isDebug/0,
	 makeConstant/1,
	 ensureMt/1,
	 isCycConstant/1,
	 readCycL/2,
	 %%user: termCyclify/2,
         %constant/4,
	 idGen/1,
	 subst/4,
	 isCycOption/1,
	 isCycOption/2,
	 unnumbervars/2,
	 %% transparent defaultAssertMt/1,
         mtForCycL/2,
         assertIfNew/1,
	 getMtForPred/2,
	 isRegisterCycPred/3,
	 registerCycPred/1,
	 setCycOption/2,
	 setCycOption/1,
	 unsetCycOption/1,
	 getWordTokens/2,
	 registerCycPred/2,
	 registerCycPred/3,
         getSurfaceFromChars/3,
         getSurface/2,
         getSurface/3,

         lisp_read/1,
         lisp_read/3,
         lisp_read_codes/2,
         lisp_read_fully/2,

	 assertThrough/1,
         cycSync/0,
         cycSyncThread/0,
         cycAssertBuffer/1,
         cycAssertBuffer/2,
	 assertThrough/2,
	 assertThrough/2,
	 assertThrough/2,
	 writel/1,
	 writel/2,
	 % atomSplit/2,
	 list_to_conj/2,
	 testCYC/0,

         flush_output_safe/1,

         %processRequestHook/1,

         loadLispFile/1,
           	 
         badConstant/1,
         quoteAtomString/2,
         unquoteAtom/2,
         termCyclifyAtom/2,

	 createCycServer/1,
	 xmlPrologServer/1,
	 read_line_with_nl/3,
	 decodeRequest/2,
	 invokePrologCommandRDF/6,
	 serviceAcceptedClientSocketAtThread/2,

	 servantProcessCreate/1,
	 servantProcessCreate/3,
	 servantProcessCreate/4,
	 servantProcessCreate/5,
	 isCycProcess/2,
	 isCycProcess/5,
	 createProcessedGoal/1,
	 servantProcessSelfClean/0,
	 showCycStatisticsHTML/0,
	 cleanOldProcesses/0,
	 showCycProcessHTML/0,

         dynamic_transparent/1,

         guessConstant/2,

         readUntil/3,
         readCycLTermChars/3,
         readCycLTermChars/2,
         atom_to_number/2,

         toUppercase/2,
         toLowercase/2,
         toPropercase/2,
         toCamelcase/2,

         logOnFailure/1,

         writeHTMLStdHeader/1,
         writeHTMLStdFooter/0,
	 
         debugFmt/1,
	 debugFmt/2,
	 debugFmtFast/1,
	 logOnFailureIgnore/1,
	 sendNote/1,
	 sendNote/4,
	 writeFailureLog/2,
	 debugOnFailure/2,
	 debugOnFailure/1,
         debugOnError/1,
	 writeObject/2,
	 writeObject/3,
	 writeObject_conj/2,
         loadCycL/1,
         cycCacheToDo/1,
         cycCache/1,
         cycQueryV/2,

      writeFmtFlushed/1,
      writeFmtFlushed/2,
      writeFmtFlushed/3,
      trim/2,
      pterm_to_sterm/2,
      canTrace/0, 
      ctrace/0,
      isConsole/0,
      multi_transparent/1

	 ]).
:-module_transparent(user:nart/3).


cyc:cyc_magic.

current_file(FileBase,Dir):-current_stream(File,read,_Stream),atom(File),is_absolute_file_name(File),
   file_directory_name(File,Dir),file_base_name(File,FilePart),once(FileBase=FilePart;file_name_extension(FileBase,_Ext,FilePart)).

asserta_if_new(A):-retract(A),fail.
asserta_if_new(A):-asserta(A),!.

:-current_file(F,Dir),writeq(current_file(F,Dir)),nl,!,
   asserta_if_new(user:library_directory(Dir)),
   asserta_if_new(user:file_search_path(cyc_api, Dir)),
   asserta_if_new(user:file_search_path(('.'), Dir)),!,
   asserta_if_new(cyc_magic_dir(Dir)),!.

addStagedDirs:-
  cyc_magic_dir(Dir),expand_file_name('./stage*',X),member(A,X),absolute_file_name(A,[relative_to(Dir)],O),asserta_if_new(user:library_directory(O)),!.
addStagedDirs.


:-dynamic(withoutCyc).
withoutCyc:-fail.

:-dynamic(canTrace/0).
canTrace.

%isConsole :- telling(user).
isConsole :- current_output(X),!,stream_property(X,alias(user_output)).

willTrace:-not(isConsole),!,fail.
willTrace:-canTrace.
hideTrace:-
   trace(cyc:hideTrace/0, -all),
   trace(willTrace/0, -all),
   trace(canTrace/0, -all),
   trace(ctrace/0, -all),
   trace(system:throw/1, +all),
   trace(system:print_message/2, +all),
   trace(user:message_hook/3 , +all),
   trace(system:message_to_string/2, +all).

ctrace:-willTrace->trace;notrace.
:-hideTrace.

withoutCyc(_,[]):-fail.

:-use_module(library(system)).
:-use_module(library(shlib)).
:-use_module(library(listing)).
:-use_module(library(sgml)).
%:-use_module(library(rdf)).
:- use_module(library(socket)).
:- use_module(library(readutil)).

%Load the TCP Library
%:-use_module(library('http/http_open')).
:-use_module(library('http/http_client')).
%:-use_module(library('http/http_header')).
%:-use_module(library('http/thread_httpd')).

%:- use_module(library(unix)).
:- use_module(library(shell)).
:- use_module(library(shlib)).
:- use_module(library(url)).
:- use_module(library(quintus)).
:- use_module(library(qsave)).

%:- use_module((javart)).

:- style_check(-singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
:- style_check(-string).

dynamic_transparent([]):-!.
dynamic_transparent([X]):-dynamic_transparent(X),!.
dynamic_transparent([X|Xs]):-!,dynamic_transparent(X),dynamic_transparent(Xs),!.
dynamic_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A).
dynamic_transparent(F/A):-!,multi_transparent(user:F/A).
dynamic_transparent(X):-functor(X,F,A),dynamic_transparent(F/A),!.

multi_transparent([]):-!.
multi_transparent([X]):-multi_transparent(X),!.
multi_transparent([X|Xs]):-!,multi_transparent(X),multi_transparent(Xs),!.
multi_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A),multifile(M:F/A).
multi_transparent(F/A):-!,multi_transparent(user:F/A).
multi_transparent(X):-functor(X,F,A),multi_transparent(F/A),!.
   
:-multi_transparent(holds/1).
:-multi_transparent(holds/2).
:-multi_transparent(holds/3).
:-multi_transparent(holds/4).
:-multi_transparent(holds/5).
:-multi_transparent(holds/6).
:-multi_transparent(holds/7).
:-multi_transparent(holds/8).



:-dynamic(double_quotes_was/1).
:-current_prolog_flag(double_quotes,X),asserta(double_quotes_was(X)).

%:- set_prolog_flag(optimise,true).
%:- set_prolog_flag(file_name_variables,false).
%:- set_prolog_flag(agc_margin,0).
%:- set_prolog_flag(trace_gc,false).
%:-set_prolog_flag(character_escapes,true).
%:-set_prolog_flag(double_quotes,string).
%:-set_prolog_flag(report_error,true).
%:-set_prolog_flag(verbose,normal).
:-set_prolog_flag(double_quotes,codes).
:-set_prolog_flag(float_format,'%.12g').
:-set_prolog_flag(gc,false).
:-dynamic_transparent(cycConnectionAvalable/5).
:-dynamic_transparent(cycConnectionUsed/5).
:-dynamic_transparent(cycMutex/2).
:-dynamic_transparent(cycChatMode/1).
:-dynamic_transparent(termCyclify/2).



loadLispFile(Filename):-
   open(Filename,read,Stream),
   loadLispStream(Stream,doLispLine).

loadLispStream(Stream,Callback):-
   repeat,
   %%%readUntil(10,Stream,Get),
   lisp_read(Stream,_String,Get),
   doSexpLine(Callback,Get),
   at_end_of_stream(Stream).
   
doSexpLine(Callback,Get):-
  once(getSurfaceFromTokens(Get,Surf,Vars)),
   call(Callback,Surf,Vars). 


s2p(X,Y,Feat):-s2p(nothingExtra,X,Y,Feat).
nothingExtra(X,Y,Z):-fail.

s2p(Extra,X,Y,Feat):-call(Extra,X,Y,Feat),!.
s2p(Extra,X,X,[]):- (var(X);number(X);is_string(X)),!.
s2p(Extra,svar(X,Name),X,[Name=X]):-atom_concat(':',_,Name),!.
s2p(Extra,svar(X,Name),Name,[Name=X])-!.
s2p(Extra,var(X,Name),X,[Name=X]):-!.
s2p(Extra,[H|T],R,Features):-!,s2Pred(Extra,H,T,R,Features),!.
s2p(Extra,C,R,Features):-compound(C),C=..[H|T],s2Pred(Extra,H,T,R,Features),!.
s2p(Extra,Atom,X,[Atom=X]):-atom(Atom),atom_concat(':',_,Atom),hash_term(Atom,Hash),X='$VAR'(Hash),!.
s2p(Extra,Atom,[Atom],[]):-atom(Atom),atom_concat('SKF',_,Atom),!.
%s2p(Extra,Atom,[X],[Atom=X]):-atom(Atom),atom_concat('SKF',_,Atom),hash_term(Atom,Hash),X='$VAR'(Hash),!.
s2p(Extra,X,X,[]):-!.


s2Pred(Extra,H,T,R,Features):-atom(H),isFn(H),!,s2List(Extra,T,TT,Features),R=[H|TT],!.
s2Pred(Extra,H,T,R,Features):-atom(H),isKeyword(H),!,s2List(Extra,T,TT,Features),R=[H|TT],!.
%s2Pred(Extra,H,T,R,Features):-is_list(H),s2List(Extra,H,HH,F1),!,s2List(Extra,T,TT,F2),R=[HH|TT],!,append(F1,F2,Features).
%s2Pred(Extra,H,T,R,Features):-isFn(H),!,s2List(Extra,T,TT,Features),R=[H|TT],!.
%s2Pred(Extra,H,T,R,Features):-isKeyword(H),!,s2List(Extra,T,TT,Features),R=[H|TT],!.
%s2Pred(Extra,H,T,R,Features):-s2List(Extra,T,TT,Features),R=..[H|TT],!.
s2Pred(Extra,H,T,R,Features):-s2p(Extra,H,HH,F1),s2List(Extra,T,TT,F2),append(F1,F2,Features),!, 
                                             s2Pred(Extra,H,T,HH,TT,R,Features).

s2Pred(Extra,H,T,HH,TT,R,Features):- atom(HH),proper_list(TT),not(isFn(HH)),!,R=..[H|TT],!.
s2Pred(Extra,H,T,HH,TT,R,Features):- R=[HH|TT].

s2List(Extra,X,X,[]):- (var(X);number(X);string(X);X==[]),!.
%s2List(Extra,X,X,[]):- atom(X),!.
s2List(Extra,[H|T],[HH|TT],Features):-s2p(Extra,H,HH,F1),s2List(Extra,T,TT,F2),append(F1,F2,Features),!.
s2List(Extra,X,Y,Features):-s2p(Extra,X,Y,Features),!.


isKeyword(X):-atom_prefix(X,':').

isFn(X):-isKeyword(X),!.
isFn(X):-name(X,[Cap|_]),char_type(Cap,upper),!.
isFn(X):-atom_concat(_,'Fn',X),!.
/*
implied with uppercheck
isFn(X):-atom_concat('The',_,X).  
isFn(X):-atom_concat('SKF',_,X).
isFn(X):-atom_concat('MT',_,X).
*/


balanceBindingS2P(X,Z):-balanceBindingS2P(X,Z,_Feats).

balanceBindingS2P(X,Z,Feats):-
      balanceBinding(X,Y),
      unnumbervars(Y,UN),
      s2p(UN,Z,Feats),!.

:-dynamic(cyc:dbCache/2).
doLispLine([P|Surf],Vars):-toUppercase(P,UP),not(UP==P),!,doLispLine([UP|Surf],Vars).
doLispLine(['CYC-ASSERT',quote(STUFF),quote(WHERE)|_],Vars):-!,doLispLine(['CYC-ASSERT',quote(STUFF),(WHERE)|_],Vars).
doLispLine(['CYC-ASSERT',quote(STUFF),(WHERE)|_],Vars):-
         balanceBindingS2P(STUFF,Prolog),
         balanceBindingS2P(WHERE,Mt),
         writeq(Mt:Prolog),nl,
         cycAssertBuffer(Prolog,Mt),!.
         %assertIfNew(cyc:dbCache(Prolog,Mt)),!.
         %writel(ist(WHERE,Prolog):Vars),nl,!.
doLispLine(Surf,Vars):-
      writeq(user_error,Surf:Vars),
      nl(user_error).


cycBaseJavaClass('org.cyc.prolog.JavaRt').


% =====================================
% Utitity
% =====================================


%assertIfNew(CX):-not(ground(CX)),!,throw(assertIfNew(CX)).
assertIfNew(CX):-catch(CX,_,fail),!.
assertIfNew(CX):-asserta(CX),!.

catchIgnore(CX):-ignore(catch(CX,_,true)).

% =====================================
% Database
% =====================================
:-dynamic(cycCacheToDo/1).
:-dynamic(cycCache/1).

                    

cycAssertBuffer(Out,Mt):-cycAssertBuffer(Mt:Out).
cycAssertBuffer(_:end_fo_file):-!.
cycAssertBuffer(forward(Out)):-!,cycAssertBuffer((Out)).
cycAssertBuffer(Out):-not(Out=_:_),!,mtForCycL(Out,MT),!,cycAssertBuffer(MT:Out).
cycAssertBuffer(Out):-cycCacheToDo(Out),!.
cycAssertBuffer(Out):-cycCache(Out),!.
cycAssertBuffer(U):-unusedCycL(U),!.
%cycAssertBuffer(Mt:Out):-coerceCyc(Out,CycL)
cycAssertBuffer(Out):-debugFmt('% cycAssertion ~w ~n',[Out]),assertIfNew(cycCacheToDo(Out)),!.
unusedCycL(_:end_fo_file).
%unusedCycL(_:comment(_,_)).
%unusedCycL(_:isa(A,'Property')):-nonvar(A),!.

cycSync:-user:cycCacheToDo(Out),cycSync(Out),fail.
cycSync:-!.%%d3Info.

cycSync(_:end_fo_file):-!.
cycSync(U):-unusedCycL(U),!.
cycSync(Out):-cycCache(Out),!,ignore(retract(cycCacheToDo(Out))).
cycSync(Out):-Out= MT : Assert,!,catch((myCycAssert(MT : Assert),ignore(assertIfNew(cycCache(Out))),ignore(retract(cycCacheToDo(Out)))),E,debugFmt('%%%%%%%%%%%%% ~q',[E])),!.
cycSync(Out):-mtForCycL(Out,Mt),!,cycSync(Mt:Out).

% ===================================================================
%  Predicates need and Assertion Mt
% ===================================================================


mtForCycL(isa(_,'Collection'),'UniversalVocabularyMt').
mtForCycL(isa(_,'Microtheory'),'UniversalVocabularyMt').
mtForCycL(isa(_,'Predicate'),'UniversalVocabularyMt').
mtForCycL(arity(_,_),'UniversalVocabularyMt').
mtForCycL(Out,Mt):-predOfCycL(Out,Pred),!,getMtForPred(Pred,Mt).

predOfCycL(SENT,Y):-member(OP,[forward,and,':',or,implies,not]),SENT=..[OP|LIST],!,member(A,LIST),predOfCycL(A,Y),!.
predOfCycL(CX,Y):-functor(CX,Y,_).


:-dynamic_transparent(mtForPred/2).

getMtForPred(X,Y):-mtForPred(X,Y),!.
getMtForPred(genlMt,'BaseKB').
getMtForPred(CycL,Mt):-nonvar(CycL),functor(CycL,Pred,_),isRegisterCycPred(Mt,Pred,_),!.
getMtForPred(CycL,Mt):-defaultAssertMt(Mt).


% ===================================================================
% Cyc Option Switches
%
%  setCycOption(Var,Value) - sets an option first removing the prevoius value
%
%  isCycOption(Var,Value). - tests for option
%
% ===================================================================

setCycOption([]):-!.
setCycOption([H|T]):-!,
      setCycOption(H),!,
      setCycOption(T),!.
setCycOption(Var=_):-var(Var),!.
setCycOption(_=Var):-var(Var),!.
setCycOption((N=V)):-nonvar(N),!,setCycOption_thread(N,V),!.
setCycOption(N):-atomic(N),!,setCycOption_thread(N,true).
	
setCycOption(Name,Value):-setCycOption_thread(Name,Value).
setCycOption_thread(Name,Value):-
	((thread_self(Process),
	retractall('$CycOption'(Process,Name,_)),
	asserta('$CycOption'(Process,Name,Value)),!)).


unsetCycOption(Name=Value):-nonvar(Name),
	unsetCycOption_thread(Name,Value).
unsetCycOption(Name):-nonvar(Name),
	unsetCycOption_thread(Name,_).
unsetCycOption(Name):-(retractall('$CycOption'(_Process,Name,_Value))).


unsetCycOption_thread(Name):-
	unsetCycOption_thread(Name,_Value).

unsetCycOption_thread(Name,Value):-
	thread_self(Process),
	retractall('$CycOption'(Process,Name,Value)).
	
getCycOption_nearest_thread(Name,Value):-
	getCycOption_thread(Name,Value),!.
getCycOption_nearest_thread(Name,Value):-
      '$CycOption'(_,Name,Value),!.
getCycOption_nearest_thread(_Name,_Value):-!.


'WRITEL'(F):-writel(F).

isCycOption(Name):-isCycOption(Name,true).
isCycOption(Name):-isCycOption(Name,on).
isCycOption(Name):-isCycOption(Name,yes).
isCycOption(Name=Value):-isCycOption(Name,Value).

isCycOption(Name,Value):-getCycOption_thread(Name,Value).


getCycOption_thread(Name,Value):-
	((thread_self(Process),
	('$CycOption'(Process,Name,Value);'$CycOption'(_,Name,Value)))),!.

getCycOption(Name=Value):-nonvar(Name),!,ensureCycOption(Name,_,Value).
getCycOption(Name=Default,Value):-nonvar(Name),!,ensureCycOption(Name,Default,Value).
getCycOption(Name,Value):-nonvar(Name),!,ensureCycOption(Name,_,Value).


ensureCycOption(Name=Default,Value):-
	ensureCycOption(Name,Default,Value),!.
	
ensureCycOption(Name,_Default,Value):-
	getCycOption_thread(Name,Value),!.

ensureCycOption(Name,Default,Default):-
	setCycOption_thread(Name,Default),!.

ensureCycOption(Name,_Default,Value):-nonvar(Name),!,   
	setCycOption_thread(Name,Value),!.

ensureCycOption(_Name,Default,Default).

setCycOption(Name,Value):-setCycOption_thread(Name,Value).


setCycOptionDefaults:-
             (unsetCycOption(_)),
             setCycOption(makeConstant='on'),
             setCycOption(opt_callback='sendNote'),
             setCycOption(cb_consultation='off'),
             setCycOption(opt_debug='on'),
             setCycOption(cb_error='off'),
             setCycOption(cb_result_each='off'),

% User Agent Defaults for 
             setCycOption(opt_cxt_request='BaseKB'),
             setCycOption(opt_ctx_assert='BaseKB'),
             setCycOption(opt_tracking_number='generate'),
             setCycOption(opt_agent='ua_parse'),
             setCycOption(opt_precompiled='off'),
             getCycOption(opt_theory,Context),setCycOption(opt_theory=Context),
             setCycOption(opt_notation='cycl'),
             setCycOption(opt_timeout=2),
             setCycOption(opt_readonly='off'),
             setCycOption(opt_debug='off'),
             setCycOption(opt_compiler='Byrd'),
             setCycOption(opt_language = 'pnx_nf'),

%Request Limits
             setCycOption(opt_answers_min=1),
             setCycOption(opt_answers_max=999), %TODO Default
             setCycOption(opt_backchains_max=5),
             setCycOption(opt_deductions_max=100),
             setCycOption(opt_backchains_max_neg=5),
             setCycOption(opt_deductions_max_neg=20),
             setCycOption(opt_forwardchains_max=1000),
             setCycOption(opt_max_breath=1000), %TODO Default

%Request Contexts
             setCycOption(opt_explore_related_contexts='off'),
             setCycOption(opt_save_justifications='off'),
             setCycOption(opt_deductions_assert='on'),
             setCycOption(opt_truth_maintence='on'),
             setCycOption(opt_forward_assertions='on'),
             setCycOption(opt_deduce_domains='on'),
             setCycOption(opt_notice_not_say=off),


%Request Pobibility
             setCycOption(opt_certainty_max=1),
             setCycOption(opt_certainty_min=1),
             setCycOption(opt_certainty=1),
             setCycOption(opt_resource_commit='on').

% ===================================================================
% Cyc initialization - call cycInit. once and this fiule will be loaded if not already
% ===================================================================
cycInit.

:-dynamic_transparent('$CycOption'/3).

:-((setCycOptionDefaults)).

:-(( %at_initialization
    setCycOption(cycServer,'10.1.1.104':36001),
      setCycOption(cycCoServer,'10.10.10.198':3679),
      setCycOption(cycServer,'10.10.10.198':13701),
      %setCycOption(cycCFasl,'10.10.10.198':3615),
     setCycOption(cycServer,'10.10.10.193':3601),
     setCycOption(cycServer,'10.10.10.198':3601),
     setCycOption(cycServer,'logicmoo.ath.cx':3601),
      setCycOption(query(backchains),3),
      setCycOption(query(number),nil),
      setCycOption(query(time),20), %max ten seconds maybe?
      setCycOption(query(depth),nil),
   !)).
      
:-((
      setCycOption(defaultAssertOptions,[':DIRECTION', ':FORWARD', ':STRENGTH', ':MONOTONIC']),
      setCycOption(':DIRECTION', ':FORWARD'),
      setCycOption(':STRENGTH', ':MONOTONIC'),
      setCycOption(hookCycPredicates,true),
      setCycOption(makeConstants,true),
   !)).


% ===================================================================
% Connecter to Cyc TCP Server
% ===================================================================

getCycConnection3(SocketId,OutStream,InStream):- 
      ignore(once(isCycOption(cycServer,Server))),
      getCycConnection(Server,SocketId,OutStream,InStream).

%% Reuse an Available connection
getCycConnection(Server,SocketId,OutStream,InStream):- fail,
      thread_self(Thread),
      ignore((var(Server),throw(no_server(getCycConnection(Server,SocketId,OutStream,InStream))))),
      once(nonvar(SocketId);nonvar(OutStream);nonvar(InStream)),
      once(cyc:cycConnectionAvalable(Thread,Server,SocketId,OutStream,InStream);cyc:cycConnectionUsed(Thread,Server,SocketId,OutStream,InStream)),!.

%% Reuse an Available connection
getCycConnection(Server,SocketId,OutStream,InStream):-
      thread_self(Thread),
      retract(cyc:cycConnectionAvalable(Thread,Server,SocketId,OutStream,InStream)),
      ignore(system:retractall(cyc:cycConnectionUsed(Thread,Server,SocketId,OutStream,InStream))),
      assertz(cyc:cycConnectionUsed(Thread,Server,SocketId,OutStream,InStream)),!.

%% Or Create a new Available connection
getCycConnection(Server,SocketId,OutStream,InStream):-
      tcp_socket(SocketId),
      tcp_connect(SocketId,Server),
      tcp_open_socket(SocketId, InStream, OutStream),!,
      thread_self(Thread),
      debugFmt('Thread ~w Connected to Cyc TCP Server {~w,~w}\n',[Thread,InStream,OutStream]),
      thread_at_exit(finishCycConnectionThread(Thread)),
      assertz(cyc:cycConnectionUsed(Thread,Server,SocketId,OutStream,InStream)),!.

finishCycConnectionThread(Thread):-
      ignore((
       system:retract(cyc:cycConnectionAvalable(Thread,Server,SocketId,OutStream,InStream)),ignore(catch(tcp_close_socket(SocketId),_,true)),fail)),
      ignore((
       system:retract(cyc:cycConnectionUsed(Thread,Server,SocketId,OutStream,InStream)),ignore(catch(tcp_close_socket(SocketId),_,true)),fail)),!.
      

finishCycConnection(SocketId,OutStream,InStream):-
      thread_self(Thread),
      ignore(system:retractall(cyc:cycConnectionUsed(Thread,Server,SocketId,OutStream,InStream))),
      ignore(system:retractall(cyc:cycConnectionAvalable(Thread,Server,SocketId,OutStream,InStream))),
      asserta(cyc:cycConnectionAvalable(Thread,Server,SocketId,OutStream,InStream)),!.
      
% ===================================================================
% cycInfo. - Prints Cyc Usage info to current output 
% ===================================================================
cycInfo:- % will add more 
   listing(cycConnectionAvalable),
   listing(cycConnectionUsed),
%   listing(user:isCycConstantMade),
  % listing('$CycOption'),
   number_of_clauses(cycCache(_)),
   number_of_clauses(cycCacheToDo(_)).

loadCycL(File):-
      see(File),
      repeat,
      read(Term),
      cycAssertBuffer(Term),
      Term=end_of_file,
      seen,cycSync.


cycSyncThread:-
         createProcessedGoal(cycSyncThreadCode),
         debugFmt(createProcessedGoal(cycSyncThreadCode)).

cycSyncThreadCode:-repeat,once((cycSync,sleep(5))),fail.

cycDatabase(CX):-nonvar(CX),cycCacheToDo(CX).
cycDatabase(CX):-nonvar(CX),cycCacheToDo(_:CX).
cycDatabase(CX):-nonvar(CX),cycCache(CX).
cycDatabase(CX):-nonvar(CX),cycCache(_:CX).

cycCache(_:end_of_file).


number_of_clauses(P):-number_of_clauses(P,Y),functor(P,F,A),debugFmt('% number_of_clauses(~q/~q)->~q~n',[F,A,Y]).
number_of_clauses(P,Y):-predicate_property(P, number_of_clauses(Y)),!.
number_of_clauses(P,0).

% ===================================================================
% Invoke SubL
% converseRaw(+Send[,-Receive]).
% 
% ?- converseRaw('(find-constant "Dog")').
% Dog
%
% ===================================================================

converse(Send):-
      converseRaw(Send,Receive),
      debugFmt('% recv> ~s~n',[Receive]).

converse(Send,Receive):-
      converseRaw(Send,ReceiveCodes),
      atom_codes(Receive,ReceiveCodes).

converseRaw(Send,Receive):-withoutCyc,!,ctrace,withoutCyc(Send,Receive).

converseRaw(Send,Receive):-
      getCycConnection3(SocketId,OutStream,InStream),
      streamClear(InStream),
      writel(OutStream,Send),
      readSubL(InStream,Get),!,
      finishCycConnection(SocketId,OutStream,InStream),!,
      checkSubLError(InStream,Send,Get,Receive),!.

checkSubLError(InStream,Send,[53,48,48,_|Info],Info):-!, %Error "500 "
      atom_codes(ErrorMsg,Info),%true,
%      streamClear(InStream),
      throw(cyc_error(ErrorMsg,Send)).
checkSubLError(InStream,_,[50,48,48,_|Info],Trim):-!, % "200 "
      %true,
      trim(Info,Trim).
checkSubLError(InStream,Send,Info,Info).

evalSubL(Send,Surface:Vars):-evalSubL(Send,Surface,Vars).

evalSubL(Send,Surface,Vars):-
     converseRaw(Send,Receive),!,
     getSurfaceFromChars(Receive,Surface,Vars).

% ===================================================================
% Lowlevel printng
% ===================================================================
writeFmtFlushed(X,Y,Z):-catch((format(X,Y,Z),flush_output_safe(X)),_,true).
writeFmtFlushed(X,Y):-catch((format(X,Y),flush_output),_,true).
writeFmtFlushed(X):- once((atom(X) -> catch((format(X,[]),flush_output),_,true) ; writeFmtFlushed('~q~n',[X]))).

writel(Lisp):-writel(user_output,Lisp).

writel(OutStream,Send):-     
      writel(OutStream,Send,_Vars).

writel(OutStream,N,Vars):-N==[],!,writeFmtFlushed(OutStream,'NIL~n',[]).
writel(OutStream,Send,Vars):-     
%      (var(Send) ->
 %        throw(cyc_error('Unbound SubL message',Send));
         is_string(Send) ->
	    formatCyc(OutStream,'~s~n',[Send]);
	      % atom(Send) -> formatCyc(OutStream,'~w~n',[Send]);
      	       toCycApiExpression(Send,Vars,STerm),formatCyc(OutStream,'~w~n',[STerm]).
%	       throw(cyc_error('SubL message type not supported',Send)),


formatCyc(OutStream,Format,Args):-
      writeFmtFlushed(OutStream,Format,Args),
      debugFmt(Format,Args),
      flush_output_safe(OutStream),!.

readSubL(InStream,[G,E,T,Space|Response]):-
      get_code(InStream,G),
      get_code(InStream,E),
      get_code(InStream,T),
      get_code(InStream,Space),
      readCycLTermChars(InStream,Response),!.



% ===================================================================
% Lowlevel getSurface/lisp_read
% ===================================================================

getSurface(Surf,Vars):-getSurface(user_input,Surf,Vars).
getSurface(InStream,Surf,Vars):-
    readCycLTermChars(InStream,Response),
    getSurfaceFromChars(Response,Surf,Vars).

getSurface(Response):- readCycLTermChars(user_input,Response).


:-dynamic(saved_stream_buffer/2).
find_stream_buffer(I,B):-saved_stream_buffer(I,B),!.
find_stream_buffer(I,[]):-!. %%throw(nobuffer(I)).
set_stream_buffer(I,B):-retractall(saved_stream_buffer(I,_)),asserta(saved_stream_buffer(I,B)).

read_codes_one_at_a_time(Stream,NewBuffer):-
      set_stream_buffer(Stream,[]),
      repeat,
      read_line_to_codes_one_at_a_time_util(Stream,NewBuffer).

read_line_to_codes_one_at_a_time_util(Stream,_):-
      find_stream_buffer(Stream,NewBuffer),
      NewBuffer = [Code|Buffer],char_type(Code,space),
      set_stream_buffer(Stream,Buffer),fail.

%%debugFmt('~n NewBuffer codes: ~s~n',[NewBuffer]),
read_line_to_codes_one_at_a_time_util(Stream,NewBuffer):-
      find_stream_buffer(Stream,Buffer),
      get_code_no_eof(Stream,Code),
      append(Buffer, [Code], NewBuffer),
      set_stream_buffer(Stream,NewBuffer),
      charGoodForInput(Stream,Code),!.

:-flag(inbracket,_,0).

charGoodForInput(_Stream,''):-!,fail.
charGoodForInput(_Stream,C):-code_type(C,end_of_line),!.
charGoodForInput(_Stream,C):-flag(inbracket,N,N),charGoodForInput1(_Stream, N,C).
charGoodForInput1(_Stream,N,40):- flag(inbracket,N,N+1),!, fail.
charGoodForInput1(_Stream,N,41):- flag(inbracket,N,N-1), !, N < 2.
charGoodForInput1(_Stream,N,C):- N > 0,!,fail.
charGoodForInput1(_Stream,N,C):-code_type(C,space),!.


/*
 lisp_read_codes("() ",Sexp)
 lisp_read_codes("1 ",Sexp)
 lisp_read_codes("a1 ",Sexp)
 lisp_read_codes("( a )",S)
 lisp_read_codes("( a ) ",S)
 lisp_read_codes("(a )",S)
 
 */

% "fully" means at least one whitespace character was there
lisp_read_fully(String,SexpO):-lisp_read_fully(user_input,String,SexpO).
lisp_read_fully(InStream,String,SexpO):- phrase_codes(read_sexp_fully0(Sexp), String, []),!,
   set_stream_buffer(InStream,[]),!,processEachReadResult(InStream,Sexp,SexpO).
lisp_read_fully(InStream,String,SexpO):- %trace,
    phrase_codes(read_sexp_fully0(Sexp), String, More),set_stream_buffer(InStream,More),
    reportLeftInBuffer(InStream),!,
    processEachReadResult(InStream,Sexp,SexpO). 

%processEachReadResult(InStream,whitespace(String),SexpO):-!,whitespace(String)=SexpO.
processEachReadResult(InStream,iterateEach(List),SexpO):-!,member(E,List),processEachReadResult(InStream,E,SexpO).
processEachReadResult(InStream,read_from_string(String),SexpO):-!,lisp_read_fully(InStream,String,SexpO).
%processEachReadResult(InStream,reader_error(Error,String),_SexpO):-!,throw(reader_error(Error,String)).
processEachReadResult(InStream,SexpO,SexpO).

lisp_read_codes(String,Sexp):-phrase_codes(sexp0(Sexp), String),!.

lisp_read(Sexp) :- current_input(I),lisp_read(I,_,Sexp),!.
lisp_read(InStream,String,Sexp) :-         
        repeat,
        read_line_to_codes_one_at_a_time_util(InStream,String),        
        lisp_read_fully(InStream,String,Sexp).

reportLeftInBuffer(I):- saved_stream_buffer(I,F),F \== [], debugFmt('~n reportLeftInBuffer: "~s" ~n',[F]),!.
reportLeftInBuffer(_):- !.

phrase_codes(DCG,LEFT):-phrase_codes(DCG,LEFT,[]).
phrase_codes(A,[C|ODES],REST):-number(C),!,phrase(A,[C|ODES],REST).
phrase_codes(A,[C|ODES],REST):-atom(C),atom_length(C,1),char_code(C,NumberCode),number(NumberCode),!,atom_to_chars(Atom,[C|ODES]),!,phrase_codes(A,Atom,REST).
phrase_codes(A,String,REST):-string(String),string_to_list(String,Codes),!,phrase(A,Codes,REST).
phrase_codes(A,String,REST):-atom(String),atom_codes(String,Codes),!,phrase(A,Codes,REST).


spanStarted0(string,"\"") --> "\"".
spanStarted0(pipequoted,"|#") --> "#|".
spanStarted0(pipequoted,endOfLine) --> ";".

endOfLine --> [13,10].
endOfLine --> [10].
endOfLine --> [13].
endOfLine --> trimOffOneAndAllWhiteSpaces,!,endOfLine.



allCharsAsString(Chars,Codes,[]):-string_to_list(Chars,Codes).

% "fully" means at least one whitespace character was there
read_sexp_fully0(whitepace(Chars)) --> dcgBothC(endOfLine,allCharsAsString(Chars)),!.
read_sexp_fully0(iterateEach([whitepace(Chars),S])) --> dcgBothC(trimOffOneAndAllWhiteSpaces,allCharsAsString(Chars)),!,read_sexp_fully0(S).
read_sexp_fully0(S) --> sexp0(S), !.
read_sexp_fully0(iterateEach([Error,read_from_string(Next)])) 
  --> {member(Chars,[re(",",'unmatched close parenthesis'),re(")",'Comma not inside a backquote.')])},
      syntaxError(Chars,Error),!,
      allCharsAsString(Next).
read_sexp_fully0(iterateEach(Ns)) --> /*{!,trace},*/ listReader0(iterateEach,noMoreChars,alwaysFail,spaces0,Ns).

syntaxError(re(Chars,Error),reader_error(Error,String)) --> Chars,{string_to_list(String,Chars)}.

alwaysFail(_,_):-fail.
noMoreChars(What,[]):-What==[].

peekChar(Peek,SMORE,SMORE) :- phrase(Peek,SMORE,_).
peekContainsChar(Peek,SMORE,SMORE) :- append(Left,Right,SMORE), phrase(Peek,Right,_).


sexp0(S) --> trimOffOneAndAllWhiteSpaces,!,sexp0(S).
sexp0(S) --> sexp1(S), allowWhiteSpaces.

sexp1(quoted(S)) --> "'",!,sexp0(S).
sexp1(back_quoted(S)) --> "`",!,sexp0(S).
sexp1(comma_at_quoted(S)) --> ",@",!,sexp0(S).
sexp1(comma_quoted(S)) --> ",",!,sexp0(S).
sexp1(S) --> peekChar("("),!,list0(S).
sexp1(S) --> atom0(S).


list0(NsTyped) --> listSpan(Type,Ending,Dotted,LegalSeps),!,spaces0, listReader0(Type,Ending,Dotted,LegalSeps,Ns),{NsTyped= Ns /*..[Type,Ns]*/ ,!}.


trimOffOneAndAllWhiteSpaces --> "(",{!,fail}.
trimOffOneAndAllWhiteSpaces --> ")",{!,fail}.
trimOffOneAndAllWhiteSpaces --> oneSpace,!, spaces0.

listReader0(Type,Ending,Dotted,LegalSeps,Ns) --> trimOffOneAndAllWhiteSpaces,!,listReader0(Type,Ending,Dotted,LegalSeps,Ns).
listReader0(Type,Ending,Dotted,LegalSeps,[]) --> Ending, !.
listReader0(Type,Ending,Dotted,LegalSeps, N) --> Dotted,!, sexp0(N),!, Ending.
listReader0(Type,Ending,Dotted,LegalSeps,[N|Ns]) --> sexp0(N),!,listReader0(Type,Ending,Dotted,LegalSeps,Ns).


consElementSep-->spaces0.
listSpan(cons,")",".",consElementSep) --> "(".

allowWhiteSpaces --> endOfLine,!.
allowWhiteSpaces --> oneSpace,!,allowWhiteSpaces.
allowWhiteSpaces --> [].

oneSpace --> [X],{ X<33 }.%%%, !, code_type(X,space),! }.

spaces0 --> oneSpace,!,spaces0.
spaces0 --> [].

atom0(S) --> "(",{!,fail}.
atom0(A) --> atom1(A).

atom1(A) --> spanStarted0(Type,EndChars),{!}, charsUpTo(S,EndChars), {A=..[Type,S]}.
atom1(A) --> char0(A).
atom1(A) --> number0(A).
%%atom1(A) --> {getTypeHintFromChar_hash(InStream,Chars,HashType)},Chars,symbolChars(Named),{A=..[HashType,Named]}.
atom1(A) --> symbol0(A).


number0(N) --> float0(N),!.
number0('/'(N,D)) --> integer0(N),"/",number0(D).
number0(N) --> integer0(N).

symbol0('#$'(S)) --> "#$",{!},symbolChars(S).

symbol0(nullPk(S)) --> "#:",{!},symbol1(S).
symbol0(S) --> ":",{!},symbol1(SC),{atom_concat(':',SC,S)}.
symbol0(S) --> symbol1(SC),{atom_concat('',SC,S)}.

% no upcase
symbol1(S) --> "|",{!}, charsUpTo(S,"|").
symbol1(S) --> symbolChars(S), { upcase_atom(S,S)}.
%% symbol1('#$'(S)) --> symbolChars(S).
symbol1(S) --> symbolChars(S).
%% symbol1(U) --> symbolChars(S), { upcase_atom(S,U)}.

char0(char(S))-->"#\\",char1(S).
char0(reader_error(char,C))-->"#\\",[C].
char1(S)--> ")",{!,string_to_list(S,")")}.
char1(S)--> "\\",{!,string_to_list(S,"\\")}.
char1(S)--> symbolChars(S),{!}.
char1(S)--> [C],{!,string_to_list(S,[C])}.


symbolChars(S) --> validSymbolChars([C|Cs]), {  string_to_list(S,[C|Cs]) }.



validSymbolChars([])--> dcgNoConsumeStartsC(invalidSymbolChar),!.
validSymbolChars([C|Cs])--> validSymbolChar(C),!,validSymbolChars(Cs).
validSymbolChars([])-->[].

validSymbolChar(C) --> dcgBothC(char0ExceptFor(C,[graph],[white]),dcgNotC(invalidSymbolChar)).
invalidSymbolChar --> meetsCharConstraitDCG([white,paren(_),quote|":,)"]).
meetsCharConstraitDCG(Include) -->  [C], {(( meetsCharConstrait(C,Include)))}.


dcgNotC(DCG2,S,E) :- not(phrase(DCG2,S,E)).
dcgBothC(DCG1,DCG2,S,R) :- append(L,R,S),phrase(DCG1,L,[]),once(phrase(DCG2,L,[])).
dcgNoConsumeStartsC(DCG,SE,SE):-phrase(DCG,SE,_).


escapedChar(C)-->[92,C]. %%,{trace}.

meetsCharConstrait(C,[]):-! /*,trace*/,  fail.
meetsCharConstrait(C,List):- member(LType,List),(LType==C ;( code_type(C,Type), LType=Type)),!.

char0ExceptFor(C,Include,Exclude) --> escapedChar(C),{!,not(member(escape, Exclude))}.
char0ExceptFor(C,Include,Exclude) --> [C], {notrace(( meetsCharConstrait(C,Include) , not(meetsCharConstrait(C,Exclude)) ))}.

chars0ExceptFor([C|Cs],Include,Exclude) --> escapedChar(C),{!,not(member(escape, Exclude))},chars0ExceptFor(Cs,Include,Exclude).
chars0ExceptFor([C|Cs],Include,Exclude) --> char0ExceptFor(C,Include,Exclude), chars0ExceptFor(Cs,Include,Exclude),!.
chars0ExceptFor([],_Include,_ExceptFor) --> [].

charsUpTo(Cs0,Except) --> charsUpTo0(Cs,Except),{string_to_list(Cs0,Cs)}.
charsUpTo0([C|Cs],Except) --> escapedChar(C), !, charsUpTo0(Cs,Except).
charsUpTo0([],Except) --> Except,{!}.
charsUpTo0([C|Cs],Except) --> [C], {!}, charsUpTo0(Cs,Except).


integer0(I) -->
        digit0(D0),
        digits0(D),!,
        noMoreSymbol,
        { number_chars(I, [D0|D]) }.



noMoreSymbol --> noMoreChars.
noMoreSymbol --> peekChar(noMoreSymbol1).
noMoreSymbol1 --> oneSpace.
noMoreSymbol1 --> invalidSymbolChar.

float0(I) -->
        digits0([D0|D0s]),
        ".",
        digits0([D1|D1s]),
        { append([D0|D0s],[46,D1|D1s],CODES),  number_codes(I, CODES) }.

digits0([D|T]) --> digit0(D), !, digits0(T).
digits0([]) --> [].

digit0(D) --> [D], { code_type(D, digit) }.


% ===================================================================
% Lowlevel readCycLTermChars
% ===================================================================
readCycLTermChars(InStream,Response):-
  readCycLTermChars(InStream,Response,_ResponseType).

readCycLTermChars(InStream,Response,ResponseType):-
   debugOnFailure(readCycLTermChars(InStream,[],[sexp],Response,ResponseType)),!.
   %%(validLisp(Response)-> (!) ;(readMoreChars(Chars))).

subType(_Type,_ExpectedType).

:-set_prolog_flag(double_quotes,codes).

getTypeHintFromChar(InStream,"\"",double_quotes).
getTypeHintFromChar(InStream,"\\",any_char).
getTypeHintFromChar(InStream,";",line_comment).
getTypeHintFromChar(InStream,[10],newline).
getTypeHintFromChar(InStream,[X],whitespace):- X < 33.
getTypeHintFromChar(InStream,"#",hash_reader).
getTypeHintFromChar(InStream,":",symbol(kw)).
getTypeHintFromChar(InStream,"|",symbol(quoted)).
getTypeHintFromChar(InStream,"#|",lisp_multiline_start).
getTypeHintFromChar(InStream,"|#",lisp_multiline_end).

getTypeHintFromChar(InStream,"#<",lisp_uglyobject).

getTypeHintFromChar(InStream,Chars,HashType):-getTypeHintFromChar_hash(InStream,Chars,HashType).

getTypeHintFromChar_hash(InStream,"#$",hash_dollar).
getTypeHintFromChar_hash(InStream,"#\\x",lisp_char_hex).
getTypeHintFromChar_hash(InStream,"#\\o",lisp_char_oct).
getTypeHintFromChar_hash(InStream,"#\\d",lisp_char_dec).
getTypeHintFromChar_hash(InStream,"#\\",lisp_char).


% "whitepace reader is first"
readCycLTermChars(InStream,Sofar,[whitespace|OuterType],Response,ResponseType):-
   peek_code(InStream,White),White<33,get_code(InStream,White),
   append(Sofar,[White],Sofar2), readCycLTermChars(InStream,Sofar2,[whitespace|OuterType],Response,ResponseType).

% "whitepace reader terminator"
readCycLTermChars(InStream,Sofar,[whitespace|OuterType],ResponseOut,ResponseType):-
   peek_code(InStream,White),White>32,ResponseType=whitespace,
   typedResponse(Sofar,ResponseType,ResponseOut).

% "keyword symbol"
readCycLTermChars(InStream,":",[sexp|OuterType],ResponseOut,SubSubResponseType):-
   skip(InStream,58),
   readCycLTermChars(InStream,[],[symbol(_)|OuterType],Response,SubSubResponseType),
   atom_concat(':',Response,ResponseOut),!.

% "| quoted symbol"
readCycLTermChars(InStream,"|",[sexp|OuterType],ResponseOut,ResponseType):-
   skip(InStream,192),
   ResponseType = symbol(quoted),
   readUntilUnless("|",[92,_],InStream,Codes),
   skip(InStream,192),
   typedResponse(Codes,ResponseType,ResponseOut).

% "#\\ character start"
readCycLTermChars(InStream,"\\#",[lisp_char|OuterType],Response,char(LispCharSubType)):-
   skip(InStream,92),peek_code(InStream,Start),
   getTypeHintFromChar(InStream,[35,92,Start],LispCharType),subType(LispCharType,OuterType),
   skip(InStream,Start),
   readCycLTermChars(InStream,[],[LispCharSubType|OuterType],Response,_Done).

% "lisp char above found no subtype"
readCycLTermChars(InStream,"\\#",[lisp_char|OuterType],char(Response),char(LispCharSubType)):-
   skip(InStream,92),peek_code(InStream,Start),
   readCycLTermChars(InStream,[Start],[symbol(_)|OuterType],Response,LispCharSubType).

% "# hashreader start"
readCycLTermChars(InStream,[35],[hash_reader|OuterType],Response,HashSubTypeSubType):-
   skip(InStream,35),peek_code(InStream,Start),
   getTypeHintFromChar(InStream,[35,Start],HashSubType),subType(HashSubType,OuterType),
   skip(InStream,Start),
   readCycLTermChars(InStream,[],[HashSubType|OuterType],Response,HashSubTypeSubType).

% "\" double quotes" 
readCycLTermChars(InStream,"\"",[sexp|OuterType],ResponseOut,ResponseType):-
   skip(InStream,34),
   ResponseType = lisp_string,
   readUntilUnless("\"",[92,_],InStream,Codes),
   skip(InStream,34),
   typedResponse(Codes,ResponseType,ResponseOut).

% "( cons reader"
readCycLTermChars(InStream,"(",[sexp|OuterType],cons(ResponseHead,ResponseTail),cons(ResponseTypeHead,ResponseTypeTail)):-
   skip(InStream,40),
   readCycLTermChars(InStream,"",[sexp|OuterType],ResponseHead,ResponseTypeHead),
   readCycLTermChars(InStream,"",[constail|OuterType],ResponseTail,ResponseTypeTail),!.

% ") cons tail"
readCycLTermChars(InStream,")",[constail|OuterType],[],constail):-
   skip(InStream,41),!.

% ". cons dot"
readCycLTermChars(InStream,".",[constail|OuterType],ResponseTail,ResponseTypeTail):-
   skip(InStream,46),
   readCycLTermChars(InStream,"",[constail|OuterType],ResponseTail,ResponseTypeTail),!.

% " cons next "
readCycLTermChars(InStream,[],[constail|OuterType],ResponseTail,ResponseTypeTail):-
   readCycLTermChars(InStream,[],[sexp,constail|OuterType],ResponseTail,ResponseTypeTail),!.

% "any sexpr"
readCycLTermChars(InStream,[],[sexp|OuterType],ResponseOut,ResponseType):-
   peek_code(InStream,Start),
   readCycLTermChars(InStream,[Start],[sexp|OuterType],ResponseOut,ResponseType),!,
   debugFmt('cyc>~s (~w)~n',[RepsonseOut,Type]).

% "number sexpr"
readCycLTermChars(InStream,[Start],[sexp|OuterType],ResponseOut,ResponseType):-code_type(Start,digit),
   readCycLTermChars(InStream,[Start],[number|OuterType],ResponseOut,ResponseType),!.

% "nonwhite sexpr"
readCycLTermChars(InStream,[Start],[sexp|OuterType],ResponseOut,ResponseType):-not(code_type(Start,white)),
   readCycLTermChars(InStream,[Start],[symbol(_)|OuterType],ResponseOut,ResponseType),!.


/*
% "nonwhite sexpr"
readCycLTermChars(InStream,[Start],[symbol(_)|OuterType],ResponseOut,ResponseType):-not(code_type(Start,white)),
   readCycLTermChars(InStream,[],[symbol(_)|OuterType],ResponseOut,ResponseType),!.

% "white sexpr"
readCycLTermChars(InStream,[Start],[NotWhite|OuterType],ResponseOut,ResponseType):-code_type(Start,white),NotWhite \= whitespace,
   readCycLTermChars(InStream,[Start],[NotWhite|OuterType],ResponseOut,ResponseType),!,
*/


typedResponse(Sofar,whitespace,ResponseOut):-string_to_list(ResponseOut,Sofar).
typedResponse(Sofar,lisp_string,ResponseOut):-string_to_list(ResponseOut,Sofar).
typedResponse(Sofar,number,ResponseOut):-number_codes(ResponseOut,Sofar).
typedResponse(Sofar,symbol,ResponseOut):-atom_codes(ResponseOut,Sofar).

/*
readCycLTermCharsUntil(34,InStream,Response,string):-!, % double quoted
   get_code(InStream,_),
   readUntil(34,InStream,Response),  
   streamClear(InStream).
*/

readCycLTermCharsUntil(34,InStream,Response,string):-!, % double quoted
   readCycL(InStream,[_|Response]),
   streamClear(InStream).


readCycLTermCharsUntil(35,InStream,Response,term):-!, % removed the 35 at head
  % get_code(InStream,_),
    readCycL(InStream,[_|Response]),
   %%readUntil(10,InStream,Response),
   streamClear(InStream).

readCycLTermCharsUntil(84,InStream,[],true):-!, % TRUE
   get_code(InStream,_), % T
   streamClear(InStream).

readCycLTermCharsUntil(78,InStream,[],nil):-!, % FALSE
   get_code(InStream,_), % N
   get_code(InStream,_), % I
   get_code(InStream,_), % L
   streamClear(InStream).

readCycLTermCharsUntil(40,InStream,Trim,cons):-!,
  % get_code(InStream,_),
   readCycL(InStream,[_|Trim]),
   streamClear(InStream).

readCycLTermCharsUntil(Char,InStream,Trim,atom):-!,
  % get_code(InStream,_),
   readCycL(InStream,[_|Trim]),
   streamClear(InStream).


streamClear(InStream) :- !. %% trace,!.
% needs better solution!  .01 seconds works but .001 seconds don't :(  meaning even .01 might in some circumstances be unreliable
streamClear(InStream) :- once(catch(wait_for_input([InStream], Inputs, 0.01),E,(trace,debugFmt(E)))),Inputs=[],!.
%streamClear(InStream) :-get_code(InStream, Was),((Was == -1) -> (true);(debugFmt('FoundMore ~c ~q ~n',[Was,Was]),streamClear(InStream))),!.
streamClear(InStream) :- get_code(InStream, _Was),( (_Was == -1) -> true ; streamClear(InStream)),!.

% ===================================================================
%  Read until
% ===================================================================
get_code_no_eof(InStream,NoEOFCode):- get_code(InStream,NoEOFCode), (NoEOFCode == -1 -> throw(cyc_error('eof on',InStream)) ; true).

readUntil(Char,InStream,Response):-readUntilUnless([Char],[92,_],InStream,Response).

readUntilUnless(Chars,Unless,InStream,Response):-
      peek_code(InStream,C),
      readUntilUnless([C],Chars,Unless,InStream,Response).

% must skip over unless
readUntilUnless([Prev|Prevs],Chars,Unless,InStream,Response):-append(_,Unless,[Prev|Prevs]),!,
   get_code_no_eof(InStream,_),
   peek_code(InStream,Next),
   append([Prev|Prevs],[Next],AllPrev),
   readUntilUnless(AllPrev,Chars,Unless,InStream,ResponseSub).

% Hit termination
readUntilUnless(Prevs,Chars,Unless,InStream,Response):-append(Response,Chars,Prevs),!.

% must consume
readUntilUnless([Prev|Prevs],Chars,Unless,InStream,Response):-
   get_code_no_eof(InStream,_),
   peek_code(InStream,Next),
   append([Prev|Prevs],[Next],AllPrev),
   readUntilUnless(AllPrev,Chars,Unless,InStream,ResponseSub).
   

      
% ===================================================================
%  conversion toCycApiExpression
% ===================================================================
toMarkUp(_,Term,Vars,Out):-
   toCycApiExpression(Term,Vars,Out),!.


toCycAtom(B,'#$different'):-member(B,[neq,dif,diff,(\=)]).
%toCycAtom(B,'QUOTE'):-member(B,[('\''),(quote),'QUOTE']).
toCycAtom(B,'()'):-member(B,[(nil),([]),'NIL']).
toCycAtom(B,' '):-member(B,[(holds),('#$holds')]).
toCycAtom(B,'#$or'):-member(B,[(;)]).
%toCycAtom(B,'#$and'):-member(B,[(,)]).
toCycAtom('=>','#$implies').
toCycAtom('<=>','#$equiv').
toCycAtom('=','#$equals').
toCycAtom('==','#$same').




escapeString(R,RS):- (string(R);is_list(R)) ,string_to_atom(R,A),atom_codes(A,Codes),escapeCodes([34,92],92,Codes,RS),!.

escapeCodes(Escaped,EscapeChar,[],[]):-!.
escapeCodes(Escaped,EscapeChar,[EscapeChar,Skip|Source],[EscapeChar,Skip|New]):-!,
   escapeCodes(Escaped,EscapeChar,Source,New),!.
escapeCodes(Escaped,EscapeChar,[Char|Source],[EscapeChar,Char|New]):-member(Char,Escaped),!,
   escapeCodes(Escaped,EscapeChar,Source,New),!.
escapeCodes(Escaped,EscapeChar,[Skipped|Source],[Skipped|New]):-
   escapeCodes(Escaped,EscapeChar,Source,New),!.

is_charlist([X]):-atom(X),not(number(X)),atom_length(X,1).
is_charlist([X|T]):-atom(X),not(number(X)),atom_length(X,1),is_charlist(T),!.

is_codelist([A]):-integer(A),!,A>8,A<129,!.
is_codelist([A|L]):-integer(A),!,A>8,A<129,is_codelist(L).

is_string(X):-atom(X),!,atom_length(X,L),L>1,atom_concat('"',_,X),atom_concat(_,'"',X),!.
is_string(X):-var(X),!,fail.
is_string(string(_)):-!.
is_string("").
is_string(X):-string(X),!.
is_string(L):-is_charlist(L),!.
is_string(L):-is_codelist(L),!.

:-dynamic(asserted/4).
:-dynamic(assertion/13).
:-dynamic(('/')/2).


decyclify(X,X):-var(X);number(X),!.
decyclify([],[]):-!.
decyclify([H|T],[HH|TT]):-!,decyclify(H,HH),decyclify(T,TT),!.
decyclify(X,P):-compound(X),not(isNonCompound(X)),X=..LIST,decyclify(LIST,DL),P=DL,!.
decyclify(X,X):-not(atom(X)),!.
decyclify(B,A):-atom_concat('#$',A,B),!.
decyclify(B,B):-!.

destringify(X,X):-var(X);number(X),!.
destringify(X,S):-is_string(X),stringToCodelist(X,CL),name(S,CL),!.
destringify([],[]):-!.
destringify([H|T],[HH|TT]):-!,destringify(H,HH),destringify(T,TT),!.
destringify(X,P):-compound(X),X=..LIST,destringify(LIST,DL),P=..DL,!.
destringify(X,X):-not(atom(X)),!.
destringify(B,A):-atom_concat('#$',A,B),!.
destringify(B,B):-!.

%stringToList(X,Y):-writeq(string_to_list(X,Y)),nl,fail.
stringToList(X,Y):-var(X),!,string_to_list(X,Y).
stringToList([],[]).
stringToList("",[]).
stringToList(X,Y):-atom(X),atom_codes(X,Codes),!,stringToList(Codes,Y),!.
stringToList(X,Y):-string(X),string_to_atom(X,M),!,stringToList(M,Y).
stringToList(X,Y):-string(X),!,string_to_list(X,Y).
stringToList(X,Y):-is_string(X),!,string_to_list(X,Y).
stringToList([X|XX],Y):-concat_atom([X|XX],' ',XXX),!,string_to_list(XXX,Y).
%prologPredToCyc(Predicate):-arity(PredicateHead)

stringToCodelist(S,CL):-stringToCodelist2(S,SL),!,escapeString(SL,CS),!,stringToList(CL,CS),!.

stringToCodelist2(string(S),Codes):-!,stringToCodelist2(S,Codes).
stringToCodelist2([],[]):-!.
stringToCodelist2([[]],[]):-!.
stringToCodelist2([''],[]):-!.
stringToCodelist2([X|T],[X|T]):-is_codelist([X|T]),!.
stringToCodelist2([X|T],Codes):-atom(X),is_charlist([X|T]),!,stringToList([X|T],Codes),!.
stringToCodelist2(String,Codes):-string(String),!,string_to_atom(String,Atom),atom_codes(Atom,Codes),!.
stringToCodelist2(Atom,Codes):-atom(Atom),atom_codes(Atom,Codes),!.
stringToCodelist2(A,Codes):-toCycApiExpression_l(A,_,L),atom_codes(L,Codes),!.
stringToCodelist2(Term,Codes):-sformat(Codes,'~q',[Term]),true.

toCycApiExpressionEach([],[]).
toCycApiExpressionEach([H|T],[HH|TT]):-toCycApiExpression(H,HH),toCycApiExpressionEach(T,TT),!.

toCycApiExpression(Prolog,CycLStr):-toCycApiExpression(Prolog,[],CycLStr),!.
toCycApiExpression(Prolog,Vars,Chars):-var(Prolog),!,toCycVar(Prolog,Vars,Chars),!.
toCycApiExpression('$VAR'(0),Vars,Chars):-!,sformat(Chars,'?A',[]).
toCycApiExpression('$VAR'(VAR),Vars,Chars):-!,sformat(Chars,'?~p',['$VAR'(VAR)]).
toCycApiExpression([],_,S):-sformat(S,'()',[]),!.
toCycApiExpression(B,Vars,A):-atom(B),toCycAtom(B,A),!.
toCycApiExpression(format(S,List),Vars,Out):-!,toCycApiExpressionEach(List,OList),sformat(Out,S,OList),!.
toCycApiExpression(Prolog,Vars,Prolog):- (atom(Prolog);number(Prolog)),!.
toCycApiExpression(string(Rep),Vars,SVar):-free_variables(Rep,[Var]),!,toCycApiExpression(Var,Vars,SVar),!.
toCycApiExpression(string(Rep),Vars,'""'):-Rep==[],!.
toCycApiExpression(string(Rep),Vars,'""'):-Rep==[''],!.
toCycApiExpression(string(Rep),Vars,Chars):-nonvar(Rep),stringToCodelist(Rep,Prolog),!,sformat(Chars,'"~s"',[Prolog]),!.
toCycApiExpression(Rep,Vars,Chars):-is_string(Rep),!,stringToCodelist(Rep,Prolog),sformat(Chars,'"~s"',[Prolog]),!.
toCycApiExpression(listofvars([]),Vars,'NIL'):-!. %listofvars
toCycApiExpression(listofvars([P|List]),Vars,Chars):-!,toCycApiExpression_l([P|List],Vars,Term),sformat(Chars,'\'(~w)',[Term]),!.
toCycApiExpression(nv(List),Vars,Chars):-!,toCycApiExpression_l(List,Vars,Chars),!.
toCycApiExpression(nart(List),Vars,Chars):-!,toCycApiExpression(List,Vars,Chars),!.
toCycApiExpression(svar(_,List),Vars,Chars):-!,toCycApiExpression(List,Vars,Chars),!.
toCycApiExpression(varslist(List),Vars,Chars):-!,toCycApiExpression_vars(List,Vars,Chars),!.
toCycApiExpression(varslist(List,Vars),_,Chars):-!,toCycApiExpression_vars(List,Vars,Chars),!.
toCycApiExpression(quote(List),Vars,Chars):-toCycApiExpression(List,Vars,Term),sformat(Chars,'\'~w',[Term]),!.
toCycApiExpression((PROLOG:VARS),Vars,Chars):-append(Vars,VARS,NewVars),toCycApiExpression(PROLOG,NewVars,Chars),!.
toCycApiExpression(Prolog,Vars,Chars):-compound(Prolog),Prolog=..[P|List],not(P='.'),!,toCycApiExpression([P|List],Vars,Chars),!.
toCycApiExpression([P,A,B],Vars,Chars):-P==(':-'),B==true,toCycApiExpression(A,Vars,Chars),!.
toCycApiExpression([P,A,B],Vars,Chars):-P==(':-'),toCycApiExpression(A,Vars,TA),toCycApiExpression(B,Vars,TB),
                  sformat(Chars,'(#$sentenceImplies ~w ~w)',[TB,TA]),!. % ? enables-Generic ?
toCycApiExpression([P,A,B],Vars,Chars):-P==(':-'),toCycApiExpression(A,Vars,TA),toCycApiExpression(B,Vars,TB),
                  sformat(Chars,'(#$enables-ThingProp ~w ~w)',[TB,TA]),!. % ? enables-Generic ?
toCycApiExpression([P|List],Vars,Chars):-
	       toCycApiExpression_l([P|List],Vars,Term),
	       sformat(Chars,'(~w)',[Term]),!.

toCycApiExpression_l(NIL,Vars,''):-NIL==[].
toCycApiExpression_l([A|Rest],Vars,Chars):- Rest==[],
      toCycApiExpression(A,Vars,Chars),!.
toCycApiExpression_l([A|Rest],Vars,Chars):- is_list(Rest),
      toCycApiExpression(A,Vars,Chars1),
      toCycApiExpression_l(Rest,Vars,Chars2),
      sformat(Chars,'~w ~w',[Chars1,Chars2]),!.
toCycApiExpression_l([A|Rest],Vars,Chars):-
      toCycApiExpression(A,Vars,Chars1),
      toCycApiExpression(Rest,Vars,Chars2),
      sformat(Chars,'~w . ~w',[Chars1,Chars2]),!.

toCycApiExpression_vars(List,Vars,''):-var(List),!.
toCycApiExpression_vars([Var],Vars,Chars):-!,
		  toCycApiExpression_var(Var,Vars,Chars).
toCycApiExpression_vars([Var|Rest],Vars,Chars):-
		  toCycApiExpression_var(Var,Vars,C1),
	       toCycApiExpression_vars(Rest,Vars,C2),
	       sformat(Chars,'~w , ~w',[C1,C2]).

toCycApiExpression_var(Var,Vars,Chars):-
	    Var=..[_,Name,Value],
            %toCycVar(Name,Vars,C1),	 
	    toCycApiExpression(Value,Vars,C2),!,
	    sformat(Chars,'?~w = ~w',[Name,C2]).
toCycApiExpression_var(Value,Vars,Chars):-
	       toCycApiExpression(Value,Vars,Chars).

	       



toCycVar(Var,Val):-toCycVar(Var,_,Val).

toCycVar(Var,[VV|_],NameQ):-nonvar(VV),VV=..[_,Name,VarRef],Var==VarRef,!,sformat(NameQ,'?~w',[Name]).
toCycVar(Var,[_|Rest],Name):-nonvar(Rest),toCycVar(Var,Rest,Name).
toCycVar(VAR,_,VarName):-
      term_to_atom(VAR,AVAR),
      atom_codes(AVAR,[95|CODES]),!,
      catch(sformat(VarName,'?~s',[CODES]),_,VarName='?HYP-VAR').


% ===================================================================
%  Debugging Cyc 
% ===================================================================
     
:-dynamic_transparent(isDebug).

% Uncomment this next line to see Cyc debug messages

isDebug.

isDebug(Call):- isDebug -> Call ; true.


% ===================================================================
%  Cyc Query Cache Control
% ===================================================================
:-dynamic_transparent(cyc:cachable_query/1).
:-dynamic_transparent(cyc:cached_query/2).

user:save_cached_query:-
   tell(saved_cached_queries),
   listing_cq,
   told.

cachable_query(isa(_,_)).
cachable_query(arity(_,_)).
cachable_query(assertTemplate(_X,_Y,_Z)).
% everything?
cachable_query(_).

listing_cq:-listing(cached_query),!.
listing_cq:-
   current_prolog_flag(toplevel_print_options,TLPO),
   (format(':-dynamic(saved_cached_queries/2).~n~n')),
   set_prolog_flag(toplevel_print_options,[quoted(true), portray(true), max_depth(0), attributes(portray),numbervars(false)]),
   listingWithnumberVars(cached_query/2),
   set_prolog_flag(toplevel_print_options,TLPO).

listingWithnumberVars(F/A):-
      functor(P,F,A),
      clause(P,Ant),
      (Ant==true -> (write_term(P,[quoted(true), portray(true), max_depth(0), attributes(portray),numbervars(false)]))
         ; (write_term(P,[quoted(true), portray(true), max_depth(0), attributes(portray),numbervars(false)]),
            write(':-'),
            (write_term(Ant,[quoted(true), portray(true), max_depth(0), attributes(portray),numbervars(false)])))),
      write('.'),nl,
      fail.
listingWithnumberVars(F/A).


:-exists_file(saved_cached_queries)->true;user:save_cached_query.

:-catch([saved_cached_queries],_,true).

% ===================================================================
%  Cyc Assert
% ===================================================================

myCycAssert(Mt:CycL):-cycAssert(Mt:CycL).

cycAssert(Mt:CycL):-!,cycAssert(CycL,Mt).
cycAssert(CycL):-
   getMtForPred(CycL,Mt),
   cycAssert(CycL,Mt).

cycAssert(arity(P,A),Mt):-cycAssertNow(isa(P,'FixedArityRelation'),Mt),cycAssertNow(arity(P,A),Mt).
cycAssert(objectFoundInLocation(P,A),Mt):-cycSpatial(P),cycSpatial(A),cycAssertNow(objectFoundInLocation(P,A),Mt).
cycAssert(adjacentTo(P,A),Mt):-cycSpatial(P),cycSpatial(A),cycAssertNow(adjacentTo(P,A),Mt).
cycAssert(pathControl(A,P),Mt):-cycIsa(P,'Path-Simple'),cycIsa(A,'PhysicalDevice'),cycAssertNow(pathControl(A,P),Mt).
cycAssert(pathBetween(P,A,B),Mt):-cycIsa(P,'Path-Simple'),cycSpatial(A),cycSpatial(B),cycAssertNow(pathBetween(P,A,B),Mt).
cycAssert('locatedAtPoint-Spatial'(A,B),Mt):-cycSpatial(A),cycAssertNow('locatedAtPoint-Spatial'(A,B),Mt).
%cycAssert(genlPreds(A,P),Mt):-cycIsa(P,'Relation'),cycIsa(A,'PhysicalDevice'),cycAssertNow(pathControl(A,P),Mt).

cycAssert(genls(P,A),Mt):-cycCollection(P),cycCollection(A),cycAssertNow(genls(P,A),Mt).
cycAssert(CycL,Mt):-cycAssertNow(CycL,Mt).

cycSpatial(A):-cycIsa(A,'SpatialThing-Localized').
cycCollection(A):-cycIsa(A,'Collection').
cycIsa(A,B):-cycAssertNow(isa(A,B),'doom:VocabularyMt').

cycAssertNow(CycL,Mt):-
      system:retractall(cyc:cached_query(_,_)),
      termCyclify(CycL,CycLified),
      termCyclify(Mt,Mtified),
      defaultAssertOptions(DefaultOptions), 
      toCycApiExpression('CYC-ASSERT'(quote(CycLified),quote(Mtified),quote(DefaultOptions)),API),
      converse(API),!.

defaultAssertOptions(Opts):-isCycOption(defaultAssertOptions,Opts).

:-dynamic_transparent(defaultAssertMt/1).
:-dynamic_transparent(everythingMt/1).

defaultAssertMt('doom:VocabularyMt').
everythingMt('EverythingPSC').

% ===================================================================
%  Cyc Unassert/Retract
% ===================================================================
cycRetract(CycL):-getMtForPred(CycL,Mt),cycRetract(CycL,Mt).
cycRetractAll(CycL):-getMtForPred(CycL,Mt),cycRetractAll(CycL,Mt).

cycRetract(CycL,Mt):-cycQuery(CycL,Mt),!,cycUnassert(CycL,Mt),!.

cycRetractAll(CycL,Mt):-cycQuery(CycL,Mt),cycUnassert(CycL,Mt),fail.
cycRetractAll(CycL,Mt):-!.

cycUnassert(CycL,Mt):-
      system:retractall(cyc:cached_query(_,_)),
      termCyclify(CycL,CycLified),
      termCyclify(Mt,Mtified),
      converse('CYC-UNASSERT'(quote(CycLified),Mtified)).


% ===================================================================
%  Cyc Query
% ===================================================================




cycQuery(CycL):-cycQuery(CycL,'InferencePSC').
cycQuery(CycL,Mt):-
	 queryParams(Backchain,Number,Time,Depth),
	 cycQuery(CycL,Mt,Backchain,Number,Time,Depth).

cycQuery(CycL,Mt,Backchain,Number,Time,Depth):-
      copy_term(CycL,Copy),
      safe_numbervars(Copy,'$VAR',0,_),!,
      cycQuery(Copy,CycL,Mt,Vars,Backchain,Number,Time,Depth).

cycQuery(Copy,CycL,Mt,Vars,Backchain,Number,Time,Depth):-
      cached_query(Copy,Results),!,
      member(CycL,Results).

%cachable?
cycQuery(Copy,CycL,Mt,Result,Backchain,Number,Time,Depth):-cachable_query(Copy),!,
      findall(CycL,cycQueryReal(CycL,Mt,Result,Backchain,Number,Time,Depth),Save),
      (Save=[] -> (true,asserta(cached_query(Copy,[]))); (ground(Save)->asserta(cached_query(Copy,Save));true)),!,
      member(CycL,Save).

% non cachable realtime
cycQuery(Copy,CycL,Mt,Vars,Backchain,Number,Time,Depth):-
      cycQueryReal(CycL,Mt,Vars,Backchain,Number,Time,Depth).

/*
	  (clet ((*cache-inference-results* t)
	    (*allow-forward-skolemization*t)  
	    (*compute-inference-results* nil)  
	    (*unique-inference-result-bindings* t) 
	    (*generate-readable-fi-results* t))
	    (without-wff-semantics
	       (ask-template '(?SEL1 ?SEL2)  '?Formula BaseKB 0 nil nil nll )) )
	       
*/
%queryParams(Backchain,Number,Time,Depth).
%queryParams(0,	nil,	nil,	nil). % default
%queryParams(1,	nil,	nil,	nil). % used here

:-set_prolog_flag(double_quotes,codes).

queryParams(Backchain,Number,Time,Depth):-
   ignore(isCycOption(query(backchains),Backchain)),
   ignore(isCycOption(query(number),Number)),
   ignore(isCycOption(query(time),Time)),
   ignore(isCycOption(query(depth),Depth)),!.

cycQueryV(Vars,CycL):-free_variables(Vars,Free),cyc:cycQueryReal(CycL,'EverythingPSC',Free,Backchains,Number,Time,Depth).

cycQueryReal(CycL,Mt,Vars,Backchain,Number,Time,Depth):-
         once((queryParams(Backchain,Number,Time,Depth),
         termCyclify(CycL,CycLified),
         termCyclify(Mt,Mtified),
         ignore(free_variables(CycLified,Vars)))),
      %  backchain number time depth
      sublTransaction(clet('((*cache-inference-results* t)(*compute-inference-results* t)(*unique-inference-result-bindings* t)(*generate-readable-fi-results* t))',
      'without-wff-semantics'('ask-template'(listofvars(Vars),quote(CycLified),quote(Mtified),Backchain,Number,Time,Depth))),Vars).


      %(progn (csetq *pq1* (Cyc-query '(#$doom:frameRelationAllExists #$genls ?X ?Y) #$doom:FrameRelatingMt)) T)
% (clet ((res (car *pq1* )))(csetq *pq1* (cdr *pq1*))res)
      
% ===================================================================
%  Generic Cyc Transactions
% ===================================================================
sublTransaction(SubL,Result):-withoutCyc,!,withoutCyc(SubL,Result).
sublTransaction(SubL,Result):-
      ignore(once(isCycOption(cycServer,Server))),
   sublTransaction(Server,SubL,Result).

sublTransaction(Server,SubL,Result):-
   once(getCycConnection(Server,SocketId,OutStream,InStream)),
   streamClear(InStream),
   once((gensym('pqsym',PQSYM1),ignore(concat_atom(['*',PQSYM1,'*'],PQSYM)))),
   writel(OutStream,progn(defvar(PQSYM,'NIL'),csetq(PQSYM,'REMOVE-DUPLICATES'(SubL,'#\'TREE-EQUAL')),length(PQSYM))),
   get_code(InStream,G),
   get_code(InStream,E),
   get_code(InStream,T),
   get_code(InStream,Space),
   call_cleanup(
      (getTransactionSize([G,E,T],OutStream,InStream,PQSYM,SubL,Size),transGetResults(Size,OutStream,InStream,PQSYM,Result)),_,
                  (releaseTransaction(PQSYM),finishCycConnection(SocketId,OutStream,InStream))).

getTransactionSize([53,48,48],OutStream,InStream,PQSYM,SubL,Size):-
      get_code(InStream,Quote),
      readUntil(34,InStream,Errors),
      debugFmt('~n% ~w> ~s~n',[PQSYM,Errors]),!,
      streamClear(InStream),
         string_to_atom(Errors,Error),   
         sformat(SCycL,'~q',[SubL]),
      releaseTransaction(PQSYM),
      finishCycConnection(SocketId,OutStream,InStream),
      throw(cyc_error(Error,SCycL)).

getTransactionSize([50,48,48],OutStream,InStream,PQSYM,SubL,Size):-
      readUntil(10,InStream,Sizess),
      debugFmt('~n% ~w> ~s~n',[PQSYM,Sizess]),
      once((trim(Sizess,Sizes),number_codes(Size,Sizes))).

releaseTransaction(PQSYM1):-converse(csetq(PQSYM1,nil)),!.

transGetResults(0,OutStream,InStream,PQSYM,Vars):-!,fail.  % no results
transGetResults(1,OutStream,InStream,PQSYM,Vars):-Vars==[],!. %TRUE with no variables
transGetResults(Size,OutStream,InStream,PQSYM,Vars):- fail,
         Size<15,% BUG: in the reader os temporalily always ussing this normally <100 is better
         sformat(Send,'(cons (car ~w)(cdr ~w))',[PQSYM,PQSYM]),
         once((writel(OutStream,Send), 
         get_code(InStream,G),
         get_code(InStream,E),
         get_code(InStream,T),
         get_code(InStream,Space),
         readUntil(10,InStream,Result),
         %releaseTransaction(Exit,PQSYM,SocketId,OutStream,InStream),
         debugFmt('~n~s~n',[Result]),
         getSurfaceFromChars(Result,Bindings,_))),!,
         %debugFmt('~q.~n',[Result]),true,
         member(VarSet,Bindings),syncCycLVars(VarSet,Vars).

transGetResults(Size,OutStream,InStream,PQSYM,Result):-
      sformat(Send,'(clet ((res (car ~w )))(csetq ~w (cdr ~w))res)',[PQSYM,PQSYM,PQSYM]),      
      repeat,
      once((writel(OutStream,Send), 
      get_code(InStream,G),
      get_code(InStream,E),
      get_code(InStream,T),
      get_code(InStream,Space),
      peek_code(InStream,PCode),

      %readUntil(10,InStream,ResultTrim),
      readCycLTermCharsUntil(PCode,InStream,ResultTrim,Type),
      debugFmt('~n~s~n',[[PCode|ResultTrim]]))),
      eachResult(PCode,InStream,Result,[PCode|ResultTrim],Cut),
              ((Cut==cut,!);(Cut==fail,!,fail);true).

eachResult(40,InStream,Vars,ResultTrim,more):-
      getSurfaceFromChars(ResultTrim,Results,_),%debugFmt(('~q.~n',[Result])),
      syncCycLVars(Results,Vars),!.
eachResult(78,InStream,Vars,Result,fail). % NIL
%eachResult(78,InStream,Vars,Result,cut). % True/NIL
eachResult(N,InStream,Vars,Result,more):-N=N.
eachResult(35,InStream,Vars,Result,fail). % No solutions 'locatedAt-Spatial' all
eachResult(73,InStream,Vars,Result,fail). % Depth limit exceeded
eachResult(41,InStream,Vars,Result,fail).  % The previous solution was the last

syncCycLVars(Binding,PBinding):- (var(Binding);var(PBinding)),!,once(balanceBinding(Binding,PBinding)).
syncCycLVars(_,[]).
syncCycLVars([Binding|T],[PBinding|VV]):-
      once(balanceBinding(Binding,PBinding)),
      syncCycLVars(T,VV),!.

% ===================================================================
%  SubL Transactions
% ===================================================================


getAllTermAssertions(Term,Result):-termCyclify(Term,CTerm), sublTransaction(mapcar('#\'assertion-el-formula','all-term-assertions'(CTerm)),Result).


%list_to_conj(X,Y):- balanceBinding(X,Y).
list_to_conj(X,Y):-nonvar(X),var(Y),!,list_to_conjs_lr(X,Y).
list_to_conj(X,Y):-list_to_conjs_rl(X,Y).
list_to_conjs_rl(List,(A,B)):-list_to_conjs_rl(A,AL),list_to_conjs_rl(B,BL),append(AL,BL,List).
list_to_conjs_rl(List,(A;B)):-list_to_conjs_rl(A,AL),list_to_conjs_rl(B,BL),append(AL,[or|BL],List).
list_to_conjs_lr([],true):-!.
list_to_conjs_lr([T],T):-!.
list_to_conjs_lr([H|T],(H,TT)):-!,list_to_conjs_lr(T,TT).
   


balanceBinding(Binding,Binding):- (var(Binding);number(Binding)),!.
balanceBinding(string(B),string(B)):-!.
balanceBinding(Binding,BindingP):-atom(Binding),atom_concat('#$',BindingP,Binding),!.
balanceBinding(nart(B),nart(BA)):-balanceBinding(B,BA),!.
balanceBinding(nart(B),(BA)):-!,balanceBinding(B,BA),!.
balanceBinding(string(B),List):-atomSplit(List,B),!.
balanceBinding(string(B),B):-!.
balanceBinding(string([]),""):-!.
balanceBinding(quote(B),BO):-!,balanceBinding(B,BO).
balanceBinding(['noparens','#','G',[GU|ID]],guid([GU|ID])):-!.
balanceBinding([A|L],Binding):-balanceBindingCons(A,L,Binding).
%balanceBinding(Binding,BindingO):-not(Binding=[_|_]),compound(Binding),Binding=..[A|L],balanceBindingCons(A,L,BindingO),!.
balanceBinding(Binding,Binding):-!.
 
balanceBindingCons(A,L,[A|L]):- (var(A);var(L);A=string(_);number(A)),!.
balanceBindingCons('and-also',L,Binding):-balanceBindingS(L,LO), list_to_conj(LO,Binding),!.
balanceBindingCons('eval',L,Binding):-balanceBindingS(L,LO), list_to_conj(LO,Binding),!.
balanceBindingCons('#$and-also',L,Binding):-balanceBindingS(L,LO), list_to_conj(LO,Binding),!.

balanceBindingCons(A,L,Binding):-
	 balanceBinding(A,AO),
         balanceBindingCons(A,AO,L,Binding).
balanceBindingCons(A,AO,L,Binding):-
         atom(AO),!,
	 balanceBindingS(L,LO),
	 Binding=..[AO|LO],!.
balanceBindingCons(A,AO,L,Binding):-
	 balanceBindingS(L,LO),
	 Binding=[AO|LO],!.

balanceBindingS(Binding,Binding):- (var(Binding);atom(Binding);number(Binding)),!.
balanceBindingS([],[]).
balanceBindingS([V,[L]|M],[LL|ML]):-V=='\'',balanceBindingS(L,LL),balanceBindingS(M,ML).
balanceBindingS([A|L],[AA|LL]):-balanceBinding(A,AA),balanceBindingS(L,LL).
   

% ===================================================================
%  Cyclification
%
%    termCyclify(Statement,Cyclified)
%     Makes sure that atoms in Statement are prefixed witbh '#$' when comunicationg with Cyc
%
%    termCyclify(Statement,Cyclified)
%     same as termCyclify/2 but adds the constant names with (CREATE-CONSTANT "~w")
%
% ===================================================================

atom_to_number(Value,Value):-number(Value),!.
atom_to_number(Atom,Value):-catch(atom_number(Atom,Value),_Error,fail),!.
atom_to_number(Atom,Value):-catch((atom_concat('.',_,Atom),atom_concat('0',Atom,Dec),atom_number(Dec,Value)),_Error,fail),!.

termCyclify(Same,Same):- (var(Same);number(Same);string(Same);Same='$VAR'(_)),!.
termCyclify(Var,Value):-var(Var),!,toCycVar(Var,Value).
termCyclify('','""'):-!.
termCyclify([],[]):-!.
termCyclify(string(Before),Before):-var(Before),!.
termCyclify(string(Before),string(Before)):-!.
termCyclify(quote(Before),quote(After)):-!,termCyclify(Before,After).
termCyclify(nart(LIST),(O)):-termCyclify(LIST,O),!.
termCyclify(c(Before),'find-or-create-constant'(string(Before))):-atom(Before),!.
termCyclify(nart(LIST),quote(O)):-termCyclify(LIST,O),!.
termCyclify(c(Before),Before).
termCyclify(':-'(A,B),CA):-B==true,termCyclify(A,CA).
termCyclify(':-'(A,B),['#$sentenceImplies',CB,CA]):-termCyclify(A,CA),termCyclify(B,CB),!.
%termCyclify([C],Term):-compound(C),!,termCyclify(C,Term).
termCyclify(P:C,Term):-ground(P:C),concat_atom([P,':',C],A),!,termCyclify(A,Term),!.
termCyclify(Before,After):-atom(Before),!,termCyclifyAtom(Before,After),!.
termCyclify([B|BL],[A|AL]):-!,termCyclify(B,A),termCyclify(BL,AL),!.
termCyclify(Before,After):- compound(Before),!, Before=..[B|BL],termCyclify(B,CB),termCyclify(BL,CBL),!,After=[CB|CBL].


termCyclifyAtom('','""').
termCyclifyAtom('?','"?"').
termCyclifyAtom(Const,CycL):-constant(Const,_,_,_),!,atom_concat('#$',Const,CycL).
termCyclifyAtom(Before,Before):-atom_length(Before,L),L<3,!.
termCyclifyAtom(Before,After):-
      atom(Before),
      sub_atom(Before,0,1,_,F),!,
      termCyclifyAtom3(F,Before,After),!.

termCyclifyAtom3('#',Before,Before).
termCyclifyAtom3('?',Before,Before).
termCyclifyAtom3(':',Before,Before).
termCyclifyAtom3('(',Before,Before).
termCyclifyAtom3('!',Before,After):-atom_concat('!',After,Before).
termCyclifyAtom3('"',Before,Before).
termCyclifyAtom3(_,Before,After):-atom_to_number(Before,After),!.
termCyclifyAtom3(_,Before,After):-badConstant(Before),quoteAtomString(Before,After).
termCyclifyAtom3(_,Before,After):-atom_concat('#$',Before,After),makeConstant(Before).      

badConstant(Atom):-member(Char,['/','*','"','.',',',' ','!','?','#','%']),concat_atom([S,T|UFF],Char,Atom),!.
quoteAtomString([34|T],Out):-name(Out,[34|T]),!.
quoteAtomString([H|T],Out):-!,append([34,H|T],[34],Quote),name(Out,Quote).
quoteAtomString(QU,QU):-concat_atom(['"'|_],QU),!.
quoteAtomString(UQ,QU):-concat_atom(['"',UQ,'"'],QU),!.

unquoteAtom(Atom,New):-concat_atom(LIST,'"',Atom),concat_atom(LIST,'',New),!.

% ============================================
% Make new CycConstant
% ============================================

:-dynamic_transparent(makeConstant/0).
:-dynamic_transparent(user:isCycConstantMade/1).
:-dynamic_transparent(isCycConstantNever/1).
:-dynamic_transparent(isCycConstantNever/2).
:-dynamic_transparent(isCycConstantGuess/1).
:-dynamic_transparent(isCycConstantGuess/2).


user:isCycConstantMade(isa).
user:isCycConstantMade(or).
user:isCycConstantMade(genls).
user:isCycConstantMade('UniversalVocabularyMt').
user:isCycConstantMade('BaseKB').
user:isCycConstantMade('Collection').
user:isCycConstantMade('Predicate').
user:isCycConstantMade('Microtheory').
user:isCycConstantMade(X):-constant(X,_,_,_).

%user:isCycConstantMade(X):-nonvar(X),isCycConstantGuess(X).


isCycConstantGuess(X):-nonvar(X),isCycConstant(X).
isCycConstantGuess(X):-nonvar(X),isCycConstantNever(X),!,fail.
isCycConstantGuess(X):-nonvar(X),isCycConstantGuess(H,T),atom_concat(H,T,X).
isCycConstantGuess(X):-member(X,[and,or,isa,not]).
isCycConstantGuess(ist,_).
isCycConstantGuess(implies,_).
isCycConstantGuess(adjacentTo,_).
isCycConstantGuess(objectF,_).

isCycConstantGuess(_,'Genl').
isCycConstantGuess(_,'Genls').
isCycConstantGuess(_,'isa').
isCycConstantGuess(_,'Isa').
isCycConstantGuess(_,'Type').
isCycConstantGuess(_,'Predicate').
isCycConstantGuess(_,'Relation').
isCycConstantGuess(_,'Function').
isCycConstantGuess(_,'Collection').
isCycConstantGuess(_,'Microtheory').
%isCycConstantGuess(_,'Mt').
isCycConstantGuess(object,_).
isCycConstantGuess(inverse,_).
isCycConstantGuess(located,_).


isCycConstantGuess('arg',_).
isCycConstantGuess('genl',_).

isCycConstantNever(X):-nonvar(X),isCycConstantNever(H,T),atom_concat(H,T,X).
isCycConstantNever('doom:',_).
isCycConstantNever(_,'1').
isCycConstantNever(_,'2').
isCycConstantNever(_,'3').
isCycConstantNever(_,'4').

:-dynamic_transparent(termCyclify/2).

isCycConstant(Const):-(var(Const);is_string(Const);number(Const)),!,fail.
isCycConstant(Const):-user:isCycConstantMade(Const),!.
isCycConstant(Const):-cyc:constant(Const,_,_,_),!.
isCycConstant(Const):-holds(isa,Const,_),!.
%isCycConstant(Const):-termCyclify(Const,_),!,fail.
isCycConstant(Const):-atom(Const),atom_concat('#$',X,Const),!,isCycConstant(X).
isCycConstant(Const):-sformat(S,'(find-constant "~w")',[Const]),converseRaw(S,R),!,R=[35|_],asserta(user:isCycConstantMade(Const)).


:-dynamic(aliasConstant/2).
aliasConstant(type_of,isa).
aliasConstant(friendly,friends).
guessConstant(Xs,Y):-string_to_atom(Xs,X),aliasConstant(X,Y),!.
guessConstant(X,Y):-guessConstantFind(X,Y).
%findall(Y,guessConstantFind(X,Y),YY),member(Y,YY).
guessConstantFind(Const,Const):-isCycConstantMade(Const),!.
%guessConstantFind(Const,Const):-atom(Const),atom_concat('#$',X,Const),!.
%guessConstantFind(X,Const):-isCycConstantMade(X),atom_concat('#$',X,Const),!.
guessConstantFind(Name,R):-evalSubL('find-constant'(string(Name)),RE,_),isTrue(RE),!,balanceBinding(RE,R).
%guessConstantFind(Name,R):-evalSubL('ps-harvest-nps'(string(Name)),RS,_),isTrue(RS),!,member([_|RE],RS),balanceBinding(RE,R).
guessConstantFind(Name,R):-cycQuery('termStrings'(R,string(Name))),isTrue(R),atom(R).
guessConstantFind(Name,R):-evalSubL('denotation-mapper'(string(Name)),RSR,_),isTrue(RSR),reverse(RSR,RS),!,member([_|RE],RSR),balanceBinding(RE,R).
guessConstantFind(Name,R):-evalSubL('ps-get-cycls-for-np'(string(Name)),RS,_),isTrue(RS),!,member(RE,RS),balanceBinding(RE,R).
%guessConstantFind(Name,R):-cycQuery('wordStrings'(W,string(Name))),isTrue(W),atom(W),cycQuery('denotationRelatedTo'(W,_,_,R)).
guessConstantFind(Name,R):-evalSubL('constant-apropos'(string(Name)),RSR,_),isTrue(RSR),reverse(RSR,RS),!,member(RE,RS),balanceBinding(RE,R).
%guessConstantFind(Name,R):-evalSubL('cyclify'(string([a,Name])),RS,_),isTrue(RS),!,member([_|RE],RS),balanceBinding(RE,R).

:-dynamic_transparent(makeConstant/0).
%makeConstant.

makeConstant(_Const):-not(isCycOption(makeConstant)),!.
%makeConstant(_Const):-!.
makeConstant(Const):-atom_concat('#$',New,Const),!,makeConstant(New).
makeConstant(Const):-
   (isCycConstant(Const)->true;
   (sformat(String,'(CREATE-CONSTANT "~w")',[Const]),
   debugOnFailure(converse(String)),
   asserta(user:isCycConstantMade(Const)))),!.

%makeConstant(_Const):-!.
killConstant(Const):-atom_concat('#$',New,Const),!,killConstant(New).
killConstant(Const):-
   sformat(String,'(FI-KILL (find-or-create-constant "~w"))',[Const]),
   debugOnFailure(converse(String)),
   retractall(user:isCycConstantMade(Const)),!.

% ============================================
% Make new Microtheory
% ============================================

ensureMt(Mt):-
   makeConstant(Mt),
   cycAssert('BaseKB':'isa'(Mt,'Microtheory')).

ensureGenlMt(Sub,Super):-ensureMt(Sub),ensureMt(Super),
   cycAssert('BaseKB':'genlMt'(Sub,Super)).


% ============================================
% Get An English Paraphrase
% ============================================
genParaphrase(CycL,English):-
      termCyclify(CycL,CycLfied),
      catch(evalSubL('GENERATE-PHRASE'('QUOTE'(CycLfied)),REnglish,_), cyc_error(_,REnglish),true),
      (REnglish=string(English);English=REnglish),!.

% ============================================
% dynamic Default Microtheory
% ============================================

   %everythingMt(EverythingPSC),
 %  :-cycAssert('BaseKB':'#$genlMt'(Mt,'CourseOfAction-AnalysisMt')). % Puts the defaultAssertMt/1 into Cyc 


% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% ?- registerCycPred('BaseKB':isa/2). 
% ?- registerCycPred('BaseKB':isa(_,_)). 
% ?- registerCycPred(isa(_,_),'BaseKB'). 
% ?- registerCycPred('BaseKB',isa,2). 
%
%  Will make calls 
% ?- isa(X,Y)
%  Query into BaseKB for (isa ?X ?Y) 
%
% ============================================
:-dynamic_transparent(isRegisterCycPred/3).

:-module_transparent(isRegisterCycPred/3).

% ?- registerCycPred('BaseKB':isa/2). 
registerCycPred(Mt:Pred/Arity):-!,
   registerCycPred(Mt,Pred,Arity).
% ?- registerCycPred('BaseKB':isa(_,_)). 
registerCycPred(Mt:Term):-
   functor(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).
registerCycPred(Term):-
   functor(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).
   


% ?- registerCycPred(isa(_,_),'BaseKB'). 
registerCycPred(Term,Mt):-
   functor(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).
   
% ?- registerCycPred('BaseKB',isa,2). 
registerCycPred(Mt,Pred,0):-!,registerCycPred(Mt,Pred,2).
registerCycPred(Mt,Pred,Arity):-isRegisterCycPred(Mt,Pred,Arity),!.
registerCycPred(Mt,Pred,Arity):-
      functor(Term,Pred,Arity),
      ignore(defaultAssertMt(Mt)),
      asserta(( user:Term :- cycQuery(Term,Mt))),
      %asserta(( Mt:Term :- cycQuery(Term,Mt))),
      assertz(isRegisterCycPred(Mt,Pred,Arity)),!.


% ============================================
% Assert Side Effect Prolog to Cyc Predicate Mapping
%
% ============================================

user:exception(undefined_predicate, Pred ,retry):- isCycOption(hookCycPredicates,true),cycDefineOrFail(Pred).

cycDefineOrFail(Mod:Pred/Arity):-atom_concat('#$',_,Pred),
      cycDefineOrFail(Mod,Pred,Arity).
cycDefineOrFail(Pred/Arity):-atom_concat('#$',_,Pred),registerCycPred(Mod,Pred,Arity).

cycDefineOrFail(Mod,Pred,Arity):-
      atom_concat('#$',_,Mod),
      registerCycPred(Mod,Pred,Arity).
cycDefineOrFail(_,Pred,Arity):-
      registerCycPred(_,Pred,Arity).

% ============================================
% Assert Side Effect Prolog to Cyc Predicate Mapping
%
% ?- assert(isa('Fido','Dog')).
% Will assert (isa Fido Dog) into BaseKB
%
% ?- assert('DogsMt':isa('Fido','Dog')).
% Will assert (isa Fido Dog) into DogsMt
% ============================================
%'$toplevel':assert(X):-assertz(Term).

ifHookRedef(_):-!.
%ifHookRedef(C):-C,!.

:-ifHookRedef((redefine_system_predicate(system:assert(_)),assert((system:assert(Term):-nonvar(Term),assertThrough(Term))))).

assertThrough(Mt:CycL):-assertThrough(Mt,CycL).
assertThrough(CycL):-mtForCycL(CycL,Mt),assertThrough(Mt,CycL).

assertThrough(ToMt,CycL):-
      functor(CycL,Pred,Arity),
      (isRegisterCycPred(Mt,Pred,Arity);atom_concat('#$',_,Pred)),!,
      ignore(ToMt=Mt),cycAssert(CycL,ToMt),!.

assertThrough(ToMt,CycL):-
      (predicate_property(Mod:CycL,_);context_module(Mod);Mod=ToMt),!,
      ignore(Mod=ToMt),
      assertz(Mod:CycL),!.

% ============================================
% Retract (All) Side Effect Prolog to Cyc Predicate Mapping
%
% ?- retractall(isa('Fido','Dog')).
% Will retract (isa Fido Dog) from BaseKB
%
% ?- retractall('DogsMt':isa('Fido','Dog')).
% Will retract (isa Fido Dog) from DogsMt
% ============================================
:-ifHookRedef((redefine_system_predicate(retractall(_)),asserta((retractall(Term):-nonvar(Term),retractAllThrough(Term))))).

retractAllThrough(Mt:CycL):-
      retractAllThrough(Mt,CycL).

retractAllThrough(CycL):-
      retractAllThrough(Mt,CycL).

retractAllThrough(ToMt,CycL):-
      functor(CycL,Pred,Arity),
      isRegisterCycPred(Mt,Pred,Arity),!,
      ignore(ToMt=Mt),
      cycRetract(CycL,ToMt),!.

retractAllThrough(ToMt,CycL):-
      (predicate_property(Mod:CycL,_);context_module(Mod);Mod=ToMt),!,
      ignore(Mod=ToMt),
      system:retractall(Mod:CycL),!.
            
% ============================================
% Retract (First) Side Effect Prolog to Cyc Predicate Mapping
%
% ?- retractall(isa('Fido','Dog')).
% Will retract (isa Fido Dog) from BaseKB
%
% ?- retractall('DogsMt':isa('Fido','Dog')).
% Will retract (isa Fido Dog) from DogsMt
% ============================================
:-ifHookRedef((redefine_system_predicate(retract(_)),asserta((retract(Term):-nonvar(Term),retractOnceThrough(Term))))).

retractOnceThrough(Mt:CycL):-
      retractOnceThrough(Mt,CycL).

retractOnceThrough(CycL):-
      retractOnceThrough(Mt,CycL).

retractOnceThrough(ToMt,CycL):-
      functor(CycL,Pred,Arity),
      isRegisterCycPred(Mt,Pred,Arity),!,
      ignore(ToMt=Mt),
      cycRetract(CycL,ToMt),!.

retractOnceThrough(ToMt,CycL):-
      (predicate_property(Mod:CycL,_);context_module(Mod);Mod=ToMt),!,
      ignore(Mod=ToMt),
      system:retract(Mod:CycL),!.

% ============================================
% Register isa/genls (more for testing :)
% ============================================

% examples
%:-registerCycPred('BaseKB',isa,2).
%:-registerCycPred('BaseKB',genls,2).
:-registerCycPred('BaseKB',genlMt,2).


% ============================================
% Testing 
% ============================================
      
testCYC:-!.

% ===================================================================

isSlot(Var):-var(Var),!.
isSlot('$VAR'(Var)):-number(Var).

isNonCompound(Var):-isSlot(Var),!.
isNonCompound(Var):-not(compound(Var)),!.
isNonCompound(svar(_,_)):-!.
isNonCompound(Var):-is_string(Var),!.
isNonCompound(string(Var)):-!.

% ===================================================================
% CycL Term Reader
% ===================================================================
:-dynamic reading_in_comment/0.
:-dynamic reading_in_string/0.
:-dynamic read_in_atom/0.

readCycL(CHARS):-readCycL(user_input,CHARS).

really_at_end_of_stream(Stream):-not(at_end_of_stream(Stream)),not_in_escape,!,fail.
really_at_end_of_stream(Stream):-catch(stream_property(file_name,_Atom),E,(trace,debugFmt(E))).
really_at_end_of_stream(Stream):- once(wait_for_input([Stream], Inputs,0.3)),Inputs=[].

%readCycL(Stream,[])  :-at_end_of_stream(Stream).     
readCycL(Stream,CHARS)  :-
		flag('bracket_depth',_,0),
		retractall(reading_in_comment),
		retractall(reading_in_string),!,
		readCycLChars_p0(Stream,CHARS),!. %%,trim(CHARS,Trim).

readCycLChars_p0(Stream,[]):- really_at_end_of_stream(Stream),!.
readCycLChars_p0(Stream,[Char|Chars]):-
        get_code(Stream,C),
	cyclReadStateChange(C),
	readCycLChars_p1(C,Char,Stream,Chars),!.
	
readCycLChars_p1(C,Char,Stream,[]):- isCycLTerminationStateChar(C,Char),!.
readCycLChars_p1(C,Char,Stream,[]):- really_at_end_of_stream(Stream),!.
readCycLChars_p1(C,Char,Stream,Chars):-cyclAsciiRemap(C,Char),
      flag('$prev_char',_,Char),
      readCycLChars_p0(Stream,Chars),!.

not_in_escape :- not(reading_in_comment),not(reading_in_string),not(read_in_atom),flag('bracket_depth',X,X),(X<1).

isCycLTerminationStateChar(10,32):-not_in_escape,!.
isCycLTerminationStateChar(13,32):-not_in_escape,!.
isCycLTerminationStateChar(41,41):-not_in_escape,!.
%isCycLTerminationStateChar(C,C):- debugFmt('Not terminal ~c ~q ~n',[C,C]),!,fail.

cyclReadStateChange(_):- reading_in_comment,!.
cyclReadStateChange(34):-flag('$prev_char',Char,Char),   % char 92 is "\"" and will escape a quote mark
      (Char=92 -> true;(retract(reading_in_string) ; assert(reading_in_string))),!.
cyclReadStateChange(_):- reading_in_string,!.
cyclReadStateChange(59):- assert(reading_in_comment),!.
cyclReadStateChange(40):-!,flag('bracket_depth',N,N + 1).
cyclReadStateChange(41):-!,flag('bracket_depth',N,N - 1).
cyclReadStateChange(_).

cyclAsciiRemap(X,32):- (not(number(X));X>128;X<32),!.
cyclAsciiRemap(X,X):-!.


% ===================================================================
% CycL Term Parser
% ===================================================================
/*===================================================================
% getSurfaceFromChars/3 does less consistancy checking then conv_to_sterm

Always a S-Expression: 'WFFOut' placing variables in 'VARSOut'

|?-getSurfaceFromChars("(isa a b)",Clause,Vars).
Clause = [isa,a,b]
Vars = _h70

| ?- getSurfaceFromChars("(isa a (b))",Clause,Vars).
Clause = [isa,a,[b]]
Vars = _h70

|?-getSurfaceFromChars("(list a b )",Clause,Vars)
Clause = [list,a,b]
Vars = _h70

| ?- getSurfaceFromChars("(genlMt A ?B)",Clause,Vars).
Clause = [genlMt,'A',_h998]
Vars = [=('B',_h998)|_h1101]

| ?- getSurfaceFromChars("(goals Iran  (not   (exists   (?CITIZEN)   (and    (citizens Iran ?CITIZEN)    (relationExistsInstance maleficiary ViolentAction ?CITIZEN)))))",Clause,Vars).

Clause = [goals,Iran,[not,[exists,[_h2866],[and,[citizens,Iran,_h2866],[relationExistsInstance,maleficiary,ViolentAction,_h2866]]]]]
Vars = [=(CITIZEN,_h2866)|_h3347]

| ?- getSurfaceFromChars("
(queryTemplate-Reln QuestionTemplate definitionalDisplaySentence 
       (NLPatternList 
           (NLPattern-Exact \"can you\") 
           (RequireOne 
               (NLPattern-Word Acquaint-TheWord Verb) 
               (NLPattern-Word Tell-TheWord Verb)) 
           (RequireOne 
               (NLPattern-Exact \"me with\") 
               (NLPattern-Exact \"me what\")) 
           (OptionalOne 
               (WordSequence \"the term\") \"a\" \"an\") 
           (NLPattern-Template NPTemplate :THING) 
           (OptionalOne \"is\") 
           (OptionalOne TemplateQuestionMarkMarker)) 
       (definitionalDisplaySentence :THING ?SENTENCE))",Clause,Vars).

| ?- getSurfaceFromChars("(#$STemplate #$bioForProposal-short (#$NLPatternList (#$NLPattern-Template #$NPTemplate :ARG1) (#$NLPattern-Exact \"short bio for use in proposals\") (#$NLPattern-Word #$Be-TheWord #$Verb) (#$NLPattern-Exact \"\\\"\") (#$NLPattern-Template #$NPTemplate :ARG2)) (#$bioForProposal-short :ARG1 :ARG2))",Clause,Vars).

// ==================================================================== */
getSurfaceFromChars(V,Term,Vars):-var(V),!,throw(error(getSurfaceFromChars/3,'Arguments are not sufficiently instantiated')).
getSurfaceFromChars([],[],VARS).
getSurfaceFromChars(C,TERM,VARS):-atom(C),atom_codes(C,Chars),!,getSurfaceFromChars(Chars,TERM,VARS).
getSurfaceFromChars(C,TERM,VARS):-string(C),stringToList(C,List),not(C=List),!,getSurfaceFromChars(List,TERM,VARS),!.
getSurfaceFromChars(Chars,TERM,VARS):-trim(Chars,CharsClean),catch(
         (once(getSurfaceFromCleanChars(CharsClean,TERMO,VARS)),TERM=TERMO),E,(TERM=[error,E],!,fail)).

getSurfaceFromCleanChars([],[end_of_file],_):-!.
getSurfaceFromCleanChars([41|_],[end_of_file],_):-!. %% ")"
getSurfaceFromCleanChars([59|Comment],[file_comment,Atom],VARS):-atom_codes(Atom,Comment),!. %% ";"
getSurfaceFromCleanChars(Chars,WFFOut,VARSOut):- 
               once(getWordTokens(Chars,WFFClean)),
               once(getSurfaceFromToks(WFFClean,WFFOut,VARSOut)),
	       retractall(numbered_var(_,_)).
getSurfaceFromCleanChars(Comment,[unk_comment,Atom],VARS):-atom_codes(Atom,Comment),!,
      writeq(getSurfaceFromCleanChars(Comment,[unk_comment,Atom],VARS)),nl,
      trace.
               
getSurfaceFromToks(WFFClean,WFFOut,VARSOut):-
               catch((
               (once(phrase(cycl(WFF),WFFClean))),
               collect_temp_vars(VARS),!,
               ((VARS=[],VARSOut=_,WFFOut=WFF);
                    (unnumbervars(VARS,LIST),
                     cyclVarNums(LIST,WFF,WFFOut,VARSOut2) ,
                     list_to_set(VARSOut2,VARSOut1),
                     open_list(VARSOut1,VARSOut)))),ERROR,(trace,WFFOut=[error_tok,ERROR,WFFClean])),!.

%getSurfaceFromToks(['('|WFFClean],WFFOut,VARSOut):-getSurfaceFromToks(WFFClean,WFFOut,VARSOut),!.
getSurfaceFromToks(WFFClean,OUT,VARSOut):- OUT=[unk_comment,WFFClean], debugFmt('getSurfaceFromToks: ~q ~n',[OUT]),sleep(2),!.


%===================================================================
% Removes Leading and Trailing whitespaces and non ANSI charsets.
%====================================================================
:-assert(show_this_hide(trim,2)).
:-set_prolog_flag(double_quotes,codes).

trim(S,Y):-flatten(S,S2),trim2(S2,Y).

trim2(S,Y):-
      ground(S),%true,
      stringToList(S,X),
      ltrim(X,R),lists:reverse(R,Rvs), 
      addSpaceBeforeSym(Rvs,Rv),      
      ltrim(Rv,RY),lists:reverse(RY,Y),!.
     
addSpaceBeforeSym([H|T],[H,32|T]):-member(H,"?.!"),!.
addSpaceBeforeSym(H,H).

:-set_prolog_flag(double_quotes,string).

ltrim([],[]):-!.
ltrim([32,32,32,32,32,32,32|String],Out) :-trim(String,Out),!.
ltrim([32,32,32,32,32|String],Out) :-trim(String,Out),!.
ltrim([32,32,32|String],Out) :- trim(String,Out),!.
ltrim([32,32|String],Out) :- trim(String,Out),!.
ltrim([P|X],Y):- (isWhitespace(P);not(number(P));P<33;P>128),trim(X,Y),!.
ltrim(X,X).


% ===================================================================
%  CycL String to DCG Converter
% Converts up to 13 forms
%     13 Terms long
%  
% =169 Parens Pairs 'locatedAt-Spatial' the First 2 levels  
% 
% ===================================================================


%?- getSurfaceFromChars("'(ls dfg)",S,V).
%S = quote([ls, dfg])
%V = _G465 

cycl(quote(WFF)) --> ['\''],{!},cycl(WFF).
%cycl(vector(LIST)) -->  ['#','('],{!},cycl_s(LIST),[')'].
cycl(LIST) -->  ['#','('],{!},cycl_s(LIST),[')'].
cycl(nart(LIST)) -->  ['#','<','('],{!},cycl_s(LIST),[')','>'].
cycl(LIST) -->  ['('],{!},cycl_s(LIST),[')'].
cycl(WFF) -->  atomic(WFF), { ! }.
cycl('?') --> ['?'].

atomic([]) --> ['NIL'].
atomic([]) --> ['nil'].
atomic(WFF) -->  variable(WFF).
atomic(_) --> [UQ|_], {member(UQ,['(',')','<','>','?','.','#']),!,fail}.
atomic(string(WFF)) -->  string(WFF).
atomic(WFF) -->  quantity(WFF).
atomic(WFF) -->  constant(WFF).
atomic(WFF) -->  symname(WFF).

cycl_s(T) -->  ['.'],{!},cycl(T).
cycl_s([A|L]) --> cycl(A),{!} , cycl_s(L).
cycl_s([]) --> [].

quantity(Number) -->  [Number] , {  number(Number),! } .

variable(VN)-->  ['??',A], { var_number(A,VN)   } . 
variable(VN)-->  ['??'], { var_gen(A),var_number(A,VN)   } .     %Anonymous
variable(VN)-->  ['?',A], { var_number(A,VN)   } . 

checkValidConstAtoms(UQ,R):-not(member(UQ,['(',')','<','>','?','.','#'])),
      once(is_list(UQ) -> (stringToList(RR,UQ),R=string(RR)) ; R=UQ),!.
      %once(user:isCycConstantMade(UQ) -> true; assert(user:isCycConstantMade(UQ))),

                                                %preconditionFor-Props
constant(Constant) -->  ['#$'],{!},symname(C1) , { atom_concat('#$',C1,Constant)}.
constant(Constant) -->  [':'],{!},symname(C1) , { atom_concat(':',C1,Constant)}.
constant(Constant) -->  [':|'],{!},symname(C1),['|'] , { concat_atom([':|','|'],C1,Constant)}.
constant(Constant) -->  [':|','|'],{!},symname(C1),['|','|'] , { concat_atom([':||','||'],C1,Constant)}.
constant(Constant) -->  ['*'],symname(C1),['*'] , { concat_atom(['*',C1,'*'],Constant) } .

string(AA) -->  [[U|Q]] , { stringToList(AA,[U|Q]),! } .
string(UQ) -->  [UQ] , { (string(UQ);is_string(UQ)),! } .

symname(Sym) -->  [Head,':',UQ] , { checkValidConstAtoms(Head,C1),checkValidConstAtoms(UQ,C2),!,concat_atom([C1,':',C2],Sym) } .
symname(Sym) -->  [UQ] , { checkValidConstAtoms(UQ,Sym),! } .

% Makes up sequencial Variable names for anonymous cycl getPrologVars
var_gen(Atom):-idGen(Number),number_codes(Number,Codes),atom_codes(Atom,[86,65,82|Codes]). % "VAR"

variables_list([list,A]) --> qual_var(A).
variables_list([list,A]) -->  ['('],qual_var(A),[')'],!.
variables_list([list,A,B]) -->  ['('],qual_var(A),qual_var(B),[')'],! .
variables_list([list,A|QV]) -->  ['('],qual_var(A),many_qual_var(QV),[')'],!.
many_qual_var([A]) -->  qual_var(A).
many_qual_var([A|T]) -->  qual_var(A),many_qual_var(T).

% Var/Quality pairs that Sowa''s ACE examples use

qual_var(VN) --> ['('],variable(VN),[')'].
qual_var(VN) --> variable(VN).
qual_var(VN) --> ['('],variable(VN),qual(_Quality),[')'].

qual(Q) --> constant(Q), { nonvar(Q) }.

% Construct arbitrary list of args
arbitrary([]) -->  [].
arbitrary(VN)-->  ['?',A], { var_number(A,VN)   } . 
arbitrary([Head]) -->  cycl(Head).
arbitrary([A|L]) --> cycl(A) , cycl_s(L).

%======================================================================
% CLSID Generation
% idGen(+Var)
% Creates a new unique number   TODO
%
% Example:
% | ?- idGen(X).
% X = 2234
%======================================================================
idGen(X):-flag(idGen,X,X+1).
     
var_number(A,'$VAR'(VN)):-numbered_var(A,'$VAR'(VN)),!.
var_number(A,'$VAR'(VN)):-flag(get_next_num,VN,VN+1),asserta(numbered_var(A,'$VAR'(VN))),!.

:-dynamic_transparent(numbered_var/2).

% This creates ISO Prolog getPrologVars w/in a CycL/STANDARD expression to be reconstrated as after parsing is complete 

cyclVarNums([],WFF,WFF,_):-!.

cyclVarNums(LIST,'$VAR'(NUM),VAR,[=(SYM,VAR)]):-numbered_var(SYM,'$VAR'(NUM)),
               member(=(SYM,VAR),LIST).

cyclVarNums(_,Atom,Atom,[]):-atomic(Atom).
cyclVarNums(LIST,Term,NewTerm,VARLIST):-Term=..[F|ARGS],cyclVarNums_list(LIST,ARGS,VARARGS,VARLIST),NewTerm=..[F|VARARGS].

cyclVarNums_list(_LIST,[],[],[]).
cyclVarNums_list(LIST,[A|RGS],[V|ARARGS],VARLIST):-
            cyclVarNums(LIST,A,V,VARS1),
            cyclVarNums_list(LIST,RGS,ARARGS,VARS2),
            append(VARS1,VARS2,VARLIST).


unnumbervars(STUFF,UN):-sformat(S,'~W',[STUFF,[quoted(true),character_escapes(true),module(user),numbervars(true),portray(false),double_quotes(true)]]),string_to_atom(S,Atom),atom_to_term(Atom,UN,_).

open_list(V,V):-var(V).
open_list(A,B):-append(A,_,B).

unnumbervars_nil(X,Y):-!,unnumbervars(X,Y).

collect_temp_vars(VARS):-!,(setof(=(Name,Number),numbered_var(Name,Number),VARS);VARS=[]).

%================================================================
% STRING TOKENIZATION                            
%================================================================
:-assert(show_this_hide(tokenize,2)).

:-set_prolog_flag(double_quotes,codes).

toCodeList(string(S),XS):-ground(S),toCodeList(S,XS),!.
toCodeList(S,XS):-string(S),!,string_to_atom(S,A),toCodeList(A,XS),!.
toCodeList(S,XS):-atom(S),atom_codes(S,C),trim(C,XS),!.
toCodeList(S,XS):-ground(S),stringToList(S,X),trim(X,XS),!.

%getWordTokens(M,['(',surf,')']):-nonvar(M),member(34,M),!.
getWordTokens(S,Y):-toCodeList(S,XS),once( tokenize3(XS,Y) ). %,debugFmt('~q.~n',[Y]).

isWhitespace(32).
isWhitespace(N):-N<33;N>128.

tokenize3([],[]).
tokenize3([White|T],O):-isWhitespace(White),!, tokenize3(T,O).
tokenize3([C|List],[Token|TList])  :- 
%  append(_,[C|List],CharList), not(isWhitespace(C)),
  get_token(C,List,Token,Rest),
  tokenize3(Rest,TList),!.


%  cyc-> "\""
get_token(34,List,Token,Rest):-
  get_chars_until(List,Lchars,Rest,34),!,
  atom_codes(Token,[34|Lchars]).
%  atom_codes(Token,Lchars).

%  cyc-> "?"
get_token(35,[36|List],Token,Rest):-not(List=[32|_]),
  get_chars_type(List,Lchars,Rest,Type),!,
  atom_codes(Token,[35, 36|Lchars]).
%  atom_codes(Token,Lchars).

%  cyc-> "#$"
get_token(35,[36|List],Token,Rest):-not(List=[32|_]),
  get_chars_type(List,Lchars,Rest,Type),!,
  atom_codes(Token,[35, 36|Lchars]).
%  atom_codes(Token,Lchars).

%  cyc-> ":"
get_token(58,List,Token,Rest)  :-not(List=[32|_]),
  get_chars_type(List,Lchars,Rest,Type),!,
  atom_codes(Token,[58|Lchars]).

%  cyc-> "~"
get_token(126,List,Token,Rest)  :-not(List=[32|_]),
  get_chars_type(List,Lchars,Rest,Type),!,
  atom_codes(Token,[126|Lchars]).

get_token(A,List,Token,Rest)  :- 
  get_chars_type([A|List],Lchars,Rest,Type),!,
  type_codes(Type,Lchars,Token),!.


get_token(A,List,Token,Rest)  :- 
  get_chars_type([A|List],Lchars,Rest,Type),!,
  type_codes(Type,Lchars,Token),!.


%% get_chars_until(In,Out,Rest,UntilChar).
get_chars_until([UNTIL|Rest],[UNTIL],Rest,UNTIL):-!.
get_chars_until([92,UNTIL|List],Lchars,Rest,UNTIL):-
   get_chars_until(List,[UNTIL|Lchars],Rest,UNTIL).
get_chars_until([Safe|List],[Safe|Lchars],Rest,UNTIL):-
   get_chars_until(List,Lchars,Rest,UNTIL).


type_codes(num,CODES,Num):-catch(number_codes(Num,CODES),_,fail),!.
type_codes(_,[34|Lchars],String):-!,
      reverse(Lchars,[_|Rchars]),
      reverse(Rchars,LcharsNoQuotes),ground(LcharsNoQuotes),stringToList(String,LcharsNoQuotes).
      %getWordTokens(LcharsNoQuotes,S),
      %stringToList(O,LcharsNoQuotes).
type_codes(_,Lchars,Token):-!,atom_codes(Token,Lchars).

get_chars_type(L,S,L1,sep)  :-  separator(L,S,L1),!.
get_chars_type([C|L],[C|Lc],L1,S)  :- 
  check_start(S,C),
  get_word_chars(S,L,Lc,L1).

get_word_chars(S,L,Lc,L1)  :- 
  check_end(S,L,Lc,L1).
get_word_chars(S,[C|L],[C|Lc],L1)  :- 
  legal_char(S,C),
  get_word_chars(S,L,Lc,L1).

legal_char(num,C)    :-  digit(C).
legal_char(quote,C)  :-  not(bracket(_,C,_)).
legal_char(symb,C)   :-  valid_char(C).

check_start(Name,S):-bracket(Name,S,_E).
check_start(num, C)   :- start_digit(C).
check_start(symb,C)   :- valid_char(C). %, 'not'(digit(C)).
check_start(string,34) :- !.
check_start(other,C) :- true.

check_end(_,[],[],[])  :-  !.
check_end(num, [C|L],[],[C|L])  :-  'not'(digit(C)),!.
check_end(Name,[E|L],[E],L)  :-  bracket(Name,S,E),!.
%check_end(symb,[C1,C2|L],[],[C1,C2|L])  :-  member([C1,C2],["Fn"]),!.
check_end(symb,[C|L],[],[C|L])  :-  'not'(valid_char(C)).

separator([C,D,E|L],[C,D,E],L)  :-member([C,D,E],["<=>","=:=","=\\=","\\==","@=<","@>=","=..","-->"]),!.
separator([C,D|L],[C,D],L)  :-member([C,D],["~a","=>",":-","\\+","->","\\=","==","@<","@>","=<",">=","#$","//","??"]),!. %,"Fn"
separator([C|L],[C],L)  :- member(C,"*,.()'[\\]`!';= < >^{}|?%$#/"),!.

valid_char(46):-!,fail.
valid_char(39):-!,fail.
valid_char(C)  :-  letter(C); digit(C); C = 95 ; C=45 ; C=58; C=39 .
letter(C)  :-   C=45 ; C=58 ; (97 =< C, C =< 122) ; (65 =< C, C =< 90) ; C = 95 ; C=63 ; C = 64.
start_digit(C)   :- member(C,"-01234567890").
digit(C)   :- member(C,"-_.01234567890+eE").

%get_word([C|T],C,T)  :-  member(C,":,.?&amp;%"),!. % ( : , . ?)
get_word([C|T],[C],T)  :- member(C,"=&amp;()`"),!. % (=)
get_word([C,C1|T],[C,C1],T)  :- member([C,C1],["??"]),!. %"Fn",
get_word([C|T],[C|W],T2)  :-  bracket(_,C,C1),!,get_chars(0,C1,T,W,T2).
get_word([C|T],[C|W],T2)  :-  valid_start(C),!, get_chars(1,32,T,W,T2).

get_chars(K,C1,[C|T],[C|W],T2)  :-  valid_char(K,C,C1),!,get_chars(K,C1,T,W,T2).
get_chars(0,C,[C|T],[],T)  :- bracket(_,C,_), !.
get_chars(0,C,[C|T],[C],T)  :-  (C = 41; C = 93),!. % ) or ]
get_chars(1,_C1,[C|T],[],[C|T])  :-  member(C, [10,13|"=,?"]).
%get_chars(2,_C1,[C,C2|T],[],[C,C2|T])  :-  member([C,C2], ["Fn"]).

valid_start(C)  :-  valid(C). %; C = 37.  % (%)
valid_start(35).
valid_char(K,C,C1)  :-  K = 0,!, C \= C1; K = 1, valid(C).

%bracket(quote,39,39).  % single quotes
bracket(quote,34,34).  % double quotes
%bracket(list,91,93).  % square brackets []
%bracket(quote,37,37).  % Literal Percent %%
%bracket(quote,35,35).  % Literal Percent ##

quote_found(0,B,B)  :-  member(B,[34]),!.
quote_found(Q,Q,0).

var_found(0,B,C)  :-  'not'(valid(B)),var_start(C).

var_start(C)  :-  (65 =< C,C =< 90);C = 95;C = 39.

valid(C)  :-   (65 =< C, C =< 90);    % A - Z
             (97 =< C, C =< 122);   % a - z
             (48 =< C, C =< 57);    % 0 - 9
             C = 95; C = 96; C = 39;C = 45;C = 58;C = 63.  % underscore; hyphen; colon; questionmark


/*===================================================================
Convert S-Expression originating from user to a Prolog Clause representing the surface level

Recursively creates a Prolog term based on the S-Expression to be done after compiler
                                                 
Examples:

| ?- sterm_to_pterm([a,b],Pterm).
Pterm = a(b)

| ?- sterm_to_pterm([a,[b]],Pterm).    %Note:  This is a special Case
Pterm = a(b)

| ?- sterm_to_pterm([holds,X,Y,Z],Pterm).    %This allows Hilog terms to be Converted
Pterm = _h76(_h90,_h104)                    

| ?- sterm_to_pterm([X,Y,Z],Pterm).   %But still works in normal places
Pterm = _h76(_h90,_h104)                    

| ?- sterm_to_pterm(['AssignmentFn',X,[Y,Z]],Pterm).                                
Pterm = 'AssignmentFn'(_h84,[_h102,_h116])
// ====================================================================*/

sterm_to_pterm(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm([VAR],VAR):-isSlot(VAR),!.
sterm_to_pterm([X],Y):-!,nonvar(X),sterm_to_pterm(X,Y).

sterm_to_pterm([S|TERM],PTERM):-isSlot(S),
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds,S|PLIST].

sterm_to_pterm([S|TERM],PTERM):-number(S),!,
            sterm_to_pterm_list([S|TERM],PTERM).            
	    
sterm_to_pterm([S|TERM],PTERM):-nonvar(S),atomic(S),!,
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[S|PLIST].

sterm_to_pterm([S|TERM],PTERM):-!,  atomic(S),
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds,S|PLIST].

sterm_to_pterm(VAR,VAR):-!.

sterm_to_pterm_list(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm_list([],[]):-!.
sterm_to_pterm_list([S|STERM],[P|PTERM]):-!,
              sterm_to_pterm(S,P),
              sterm_to_pterm_list(STERM,PTERM).
sterm_to_pterm_list(VAR,[VAR]).


atomSplit(Atom,WordsO):- atomSplit(Atom,WordsO,[' ','\'',';',',','"','`',':','?','!','.','\n','\t','\r','\\','*','%','(',')']),!.

atomSplit(Atom,WordsO,List):- atom(Atom), concat_atom(Words1,' ',Atom),!, atomSplit2(Words1,Words,List),!,Words=WordsO.
atomSplit(Atom,Words,[Space|List]):-var(Atom),ground(Words),!,concat_atom(Words,Space,AtomO),!,Atom=AtomO.


atomSplit2([],[],_List):-!.
atomSplit2([Mark|S],[Mark|Words],List):- member(Mark,List),!,atomSplit2(S,Words,List),!.
atomSplit2([W|S],[A,Mark|Words],List):- member(Mark,List),atom_concat(A,Mark,W),!,atomSplit2(S,Words,List).
atomSplit2([W|S],[Mark,A|Words],List):- member(Mark,List),atom_concat(Mark,A,W),!,atomSplit2(S,Words,List).
atomSplit2([Word|S],Words,List):- member(Space,List),concat_atom(Atoms,Space,Word),Atoms=[_,_|_],interleave(Atoms,Space,Left),
                  atomSplit2(S,Right,List),append(Left,Right,WordsM),!,atomSplit2(WordsM,Words,List),!.
atomSplit2([W|S],[W|Words],List):-atomSplit2(S,Words,List).

interleave([''],Space,[Space]):-!.
interleave([Atom],_Space,[Atom]):-!.
interleave([''|More],Space,[Space|Result]):-interleave(More,Space,Result),!.
interleave([Atom|More],Space,[Atom,Space|Result]):-interleave(More,Space,Result),!.

pterm_to_sterm(VAR,VAR):-isNonCompound(VAR),!.
pterm_to_sterm([X|L],[Y|Ls]):-!,pterm_to_sterm(X,Y),pterm_to_sterm(L,Ls),!.
pterm_to_sterm(X,Y):-compound(X),X=..L,pterm_to_sterm(L,Y),!.
pterm_to_sterm(X,X).

% ===================================================================
% Substitution based on ==
% ===================================================================

% Usage: subst(+Fml,+X,+Sk,?FmlSk)

subst(A,B,C,D):- 
      catch(notrace(nd_subst(A,B,C,D)),_,fail),!.
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




weak_nd_subst(  Var, VarS,SUB,SUB ) :- Var=VarS,!.
weak_nd_subst(        P, X,Sk,        P1 ) :- functor(P,_,N),weak_nd_subst1( X, Sk, P, N, P1 ).

weak_nd_subst1( _,  _, P, 0, P  ).

weak_nd_subst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], weak_nd_subst2( X, Sk, Args, ArgS ),
            weak_nd_subst2( X, Sk, [F], [FS] ),
            P1 =.. [FS|ArgS].

weak_nd_subst2( _,  _, [], [] ).
weak_nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X = A, !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- weak_nd_subst( A,X,Sk,Ap ),weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, L, L ).

% ===================================================================
% PURPOSE
% This File is the bootstrap SWI-Prolog listener to hanndle CYC API requests
% So first is loads the proper files and then starts up the system
% ===================================================================


% ===================================================================
% Prolog Dependant Code
% ===================================================================

    
/*
:-module(system_dependant,
      [getCputime/1,
      safe_numbervars/1,
      unnumbervars/2,
      debugFmt/1,
      debugFmt/2,
      writeFmt/1,
      writeFmt/2,
      writeFmt/3,
      fmtString/2,
      fmtString/3,
      writeFmtFlushed/1,
      writeFmtFlushed/2,
      writeFmtFlushed/3,
      saveUserInput/0,
      writeSavedPrompt/0,
      if_prolog/2,
      callIfPlatformWin32/1,
      callIfPlatformUnix/1,
      at_initialization/1,
      thread_create/3,
      current_thread/2,
      thread_exit/1,
      thread_self/1,
      thread_at_exit/1,
      thread_signal/2,
      thread_join/2,
      prolog_notrace/1,
      prolog_statistics/0,
      main/1]).
      
*/      



% ========================================================================================
% Using getCputime/1 (in Cyc code) since Eclipse prolog (another port for Cyc)  chokes on getCputime/1
% ========================================================================================
getCputime(Start):-statistics(cputime,Start).
prolog_statistics:-statistics.
prolog_notrace(G):-notrace(G).

% ========================================================================================
% Threads 
% ========================================================================================
/*thread_create(Goal,Id,Options):-thread_create((Goal),Id,[Options]).
current_thread(Id,Status):-current_thread(Id,Status).
thread_exit(Goal):-thread_exit(Goal).
thread_self(Id):-thread_self(Id).
thread_at_exit(Goal):-thread_at_exit(Goal).
thread_signal(ID,Goal):-thread_signal(ID,Goal).
thread_join(Id,X):-thread_join(Id,X).
  */
% ========================================================================================
% Some prologs have a printf() type predicate.. so I made up fmtString/writeFmt in the Cyc code that calls the per-prolog mechaism
% in SWI it''s formzat/N and sformat/N
% ========================================================================================
:-dynamic_transparent(isConsoleOverwritten/0).

/*
defined above
writeFmtFlushed(X,Y,Z):-catch((format(X,Y,Z),flush_output_safe(X)),_,true).
writeFmtFlushed(X,Y):-catch((format(X,Y),flush_output),_,true).
writeFmtFlushed(X):- once((atom(X) -> catch((format(X,[]),flush_output),_,true) ; writeFmtFlushed('~q~n',[X]))).
*/

writeFmt(X,Y,Z):-catch(format(X,Y,Z),_,true).
writeFmt(X,Y):-format(X,Y).
writeFmt(X):-format(X,[]).

fmtString(X,Y,Z):-sformat(X,Y,Z).
fmtString(Y,Z):-sformat(Y,Z).

saveUserInput:-retractall(isConsoleOverwritten),flush_output.
writeSavedPrompt:-not(isConsoleOverwritten),!.
writeSavedPrompt:-flush_output.
writeOverwritten:-isConsoleOverwritten,!.
writeOverwritten:-assert(isConsoleOverwritten).

writeErrMsg(Out,E):- message_to_string(E,S),writeFmtFlushed(Out,'<cycml:error>~s</cycml:error>\n',[S]),!.
writeErrMsg(Out,E,Goal):- message_to_string(E,S),writeFmtFlushed(Out,'<cycml:error>goal "~q" ~s</cycml:error>\n',[Goal,S]),!.
writeFileToStream(Dest,Filename):-
        catch((
        open(Filename,'r',Input),
        repeat,
                get_code(Input,Char),
                put(Dest,Char),
        at_end_of_stream(Input),
        close(Input)),E,
        writeFmtFlushed('<cycml:error goal="~q">~w</cycml:error>\n',[writeFileToStream(Dest,Filename),E])).


% ========================================================================================
% safe_numbervars/1 (just simpler safe_numbervars.. will use a rand9ome start point so if a partially numbered getPrologVars wont get dup getPrologVars
% Each prolog has a specific way it could unnumber the result of a safe_numbervars
% ========================================================================================

safe_numbervars(X):-get_time(T),convert_time(T,A,B,C,D,E,F,G),!,safe_numbervars(X,'$VAR',G,_).
safe_numbervars(Copy,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).
safe_numbervars(Copy,_,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).
%unnumbervars(X,Y):-term_to_atom(X,A),atom_to_term(A,Y,_).

% ========================================================================================
% Ensure a Module is loaded
% ========================================================================================
moduleEnsureLoaded(X):-
        catch(ensure_loaded(X),_,(catch((atom_concat('mod/',X,Y),
        ensure_loaded(Y)),_,debugFmt(';; file find error ~q ~q',[X,E])))).

% ========================================================================================
% Platform specifics
% ========================================================================================
callIfPlatformWin32(G):-prolog_flag(windows,true),!,ignore(G).
callIfPlatformWin32(G):-!.

callIfPlatformUnix(G):-not(prolog_flag(windows,true)),!,ignore(G).
callIfPlatformUnix(G):-!.

/*
:- callIfPlatformWin32(set_prolog_flag(debug_on_error,true)).
:- callIfPlatformUnix(set_prolog_flag(debug_on_error,false)).
:- callIfPlatformUnix(set_prolog_flag(debug_on_error,true)).
*/

% ========================================================================================
% Prolog specific code choices
% ========================================================================================
if_prolog(swi,G):-call(G).  % Run B-Prolog Specifics
if_prolog(_,_):-!.  % Dont run SWI Specificd or others

% used like if_prolog(bp,do_bp_stuff),if_prolog(swi,do_swi_stuff) inline in Cyc code


%at_initialization(V):-at_initialization(V),!,logOnFailureIgnore(V).


%englishAsk(String):-!.

% ===========================================================
% SOCKET SERVER - Looks 'locatedAt-Spatial' first charicater of request and decides between:
%  Http, Native or Soap and replies accordingly
% ===========================================================
/*
:-module(cyc_httpd,[
   createCycServer/1,
   xmlPrologServer/1,
   read_line_with_nl/3,
   decodeRequest/2,
   invokePrologCommandRDF/6,
   serviceAcceptedClientSocketAtThread/1]).
*/

% :-include(cyc_header).



% :-use_module(cyc_threads).
%% :-ensure_loaded(system_dependant).

:-dynamic_transparent(isKeepAlive/1).

:-dynamic_transparent(isServerCreated/1).
%startCycAPIServer:-isServerCreated,!.
startCycAPIServer:- createCycServer(4600),!.
createCycServer(BasePort) :-isServerCreated(BasePort),!.
createCycServer(BasePort) :-
         asserta(isServerCreated(BasePort)),
         AsciiPort1 is BasePort+1,
         AsciiPort2 is BasePort+2,
         CFASLPORT is BasePort+14,
         COSRVER is BasePort+79,
        servantProcessCreate(nokill,'Logicmoo/CYC HTTPD/CycL/XML/SOAP Server Socket',xmlPrologServer(AsciiPort1),_,[global(4000),local(4000),trail(4000),detatched(true)]),
        servantProcessCreate(nokill,'Prolog HTTPD Server Socket',xmlPrologServer(AsciiPort2),_,[global(4000),local(4000),trail(4000),detatched(true)]), %%global(4000),local(4000),trail(4000)
        %servantProcessCreate(nokill,'CFASL Server Socket',cfaslServer(CFASLPORT),_,[]),
        %servantProcessCreate(nokill,'COPROCESSOR Server Socket',coServer(COSRVER),_,[]),
        gethostname(Hostname),catch(tcp_host_to_address(Hostname,ip(A,B,C,D)),_,true),
        sformat(IP,'~a.~a.~a.~a',[A,B,C,D]),
        ignore(IP=Hostname),
        ensureCycCallsProlog(IP,AsciiPort1),!.

xmlPrologServer(Port):-
        tcp_socket(ServerSocket),
        catch(ignore(tcp_setopt(ServerSocket, reuseaddr)),_,true),
        at_halt(tcp_close_socket(ServerSocket)),
        attemptServerBind(ServerSocket, Port),
        tcp_listen(ServerSocket, 655),
        repeat,
          acceptClientsAtServerSocket(ServerSocket),
        fail.



attemptServerBind(ServerSocket, Port):-
        catch((tcp_bind(ServerSocket, Port),
        debugFmt('% CYC Prolog API server started on port ~w. \n',[Port])),
        error(E,_),
        debugFmt('% CYC Prolog API server not started on port ~w becasue: "~w"\n',[Port,E])).

acceptClientsAtServerSocket(ServerSocket):-
		tcp_open_socket(ServerSocket, AcceptFd, _),
                 tcp_accept(AcceptFd, ClientSocket, ip(A4,A3,A2,A1)),!,
                cleanOldProcesses,!,
                %setCycOption('$source_ip',ip(A4,A3,A2,A1)),
                getPrettyDateTime(DateTime),
                %setCycOption('$datetime',DateTime),
                sformat(Name,'Dispatcher for ~w.~w.~w.~w  started ~w ',[A4,A3,A2,A1,DateTime]),
         	debugFmt('~s',[Name]),!,
        servantProcessCreate(killable,Name,serviceAcceptedClientSocketAtThread([ip(A4,A3,A2,A1)],ClientSocket),_,[]),!. %global(12800),local(12800),trail(12800)

serviceAcceptedClientSocketAtThread(OClientInfo,ClientSocket):-
       tcp_open_socket(ClientSocket,In,Out),!,
       ClientInfo = ['io'(ClientSocket,In,Out)|OClientInfo],
        setCycOption('socket',ClientSocket),
%        setCycOption('$socket_in',In),
%        setCycOption('$socket_out',Out),!,
        %set_prolog_IO(In,Out,user_error),
        ignore(catch(serviceIO(ClientInfo,In,Out),E,debugFmt(E:serviceIO(ClientInfo,In,Out)))),
        flush_output,seen,told,
        ignore(catch(close(In,[force(true)]),_,true)),
	ignore(catch(close(Out,[force(true)]),_,true)),
	ignore(catch(tcp_close_socket(ClientSocket),_,true)),
	thread_exit(ClientInfo).      

getPrettyDateTime(String):-get_time(Time),convert_time(Time, String).


%my_peek_char(In,Char):-debugFmt(my_peek_char(In,Char)),peek_char(In,Char).

serviceIO(ClientInfo,In,Out):-
        peek_char(In,Char),!,
	debugFmt('~q',serviceIOBasedOnChar([firstChar(Char)|ClientInfo],Char,In,Out)),
        serviceIOBasedOnChar([firstChar(Char)|ClientInfo],Char,In,Out),!.

serviceIOBasedOnChar(ClientInfo,'G',In,Out):-!,serviceHttpRequest(ClientInfo,In,Out).
serviceIOBasedOnChar(ClientInfo,'P',In,Out):-!,serviceHttpRequest(ClientInfo,In,Out).

serviceIOBasedOnChar(ClientInfo,'<',In,Out):-!,
         serviceSoapRequest(In,Out).  % see cyc_soap.pl

serviceIOBasedOnChar(ClientInfo,'+',In,Out):-!,
            get0(In,Plus),serviceJavaApiRequest(In,Out).

serviceIOBasedOnChar(ClientInfo,end_of_file,In,Out):-!,throw(end_of_file(In,Out)).

serviceIOBasedOnChar(ClientInfo,'(',In,Out):-!,  
         serviceCycApiRequest(ClientInfo,In,Out).

serviceIOBasedOnChar(ClientInfo,SkipChar,In,Out):- char_type(SkipChar,space),!,
	debugFmt(serviceIOBasedOnChar(ClientInfo,SkipChar)),
        get_char(In,_),
        peek_char(In,Char),!,
	%debugFmt(serviceIOBasedOnChar(ClientInfo,Char,In,Out)),
        serviceIOBasedOnChar(ClientInfo,Char,In,Out),!.

serviceIOBasedOnChar(ClientInfo,ANY,In,Out):-!,serviceJavaApiRequest(In,Out).

% ===========================================================
% PROLOGD for CYC SERVICE
% ===========================================================

readLispStream(In,PrologGoal,ToplevelVars):-readCycL(In,Trim),getSurfaceFromChars(Trim,PrologGoal,ToplevelVars),!.


serviceCycApiRequest(ClientInfo,In,Out):-
   thread_self(Session),
   retractall(isKeepAlive(Session)),
   %asserta(isKeepAlive(Session)),
        tell(Out),
 %  repeat,
	 once((readLispStream(In,PrologGoal,ToplevelVars),debugFmt('remote API Call "~q" ~n',[PrologGoal]))),
	 ignore(once(catch(once(callCycApi(Out,PrologGoal,ToplevelVars)),E,(writeFmtFlushed(Out,'500 "~q"\n',[E]),writeFmtFlushed(user_error,'% sent cyc: 500 "~q"\n',[E]))))),
         !.
  %       not(isKeepAlive(Session)).



isCycAPIQuit([A]):-nonvar(A),!,isCycAPIQuit(A).
isCycAPIQuit('API-QUIT').
isCycAPIQuit('api-quit').
isCycAPIQuit('CLOSE-JAVA-API-SOCKET').
isCycAPIQuit('QUIT').
isCycTrue(APIQUIT):-isCycAPIQuit(APIQUIT).
isCycTrue('INITIALIZE-JAVA-API-PASSIVE-SOCKET').

      
'PRINT'(X):-writel(X).

'TEST':-format('"hi"').

cfaslRead(In,Object):-get_code(In,Type),cfaslRead(Type,In,Object).
% CFASL_P_8BIT_INT
cfaslCode('CFASL_P_8BIT_INT',0).
cfaslRead(0,In,Object):-get_code(I,Object).
% CFASL_N_8BIT_INT
cfaslCode('CFASL_N_8BIT_INT',1).
cfaslRead(1,In,Object):-get_code(I,O), Object is -1*O.
% CFASL_P_16BIT_INT
cfaslCode('CFASL_P_16BIT_INT',2).
cfaslRead(2,In,Object):-get_code(I,V1),get_code(I,V2),Object is (V1+V2*256).
% CFASL_N_16BIT_INT
cfaslCode('CFASL_N_16BIT_INT',3).
cfaslRead(3,In,Object):-get_code(I,V1),get_code(I,V2),Object is -1*(V1+V2*256).
% CFASL_P_24BIT_INT
cfaslCode('CFASL_P_24BIT_INT',4).
cfaslRead(4,In,Object):-get_code(I,V1),get_code(I,V2),get_code(I,V3),Object is (V1+V2*256+V3*256*256).
% CFASL_N_24BIT_INT
cfaslCode('CFASL_N_24BIT_INT',5).
cfaslRead(5,In,Object):-get_code(I,V1),get_code(I,V2),get_code(I,V3),Object is -1*(V1+V2*256+V3*256*256).
% CFASL_P_32BIT_INT
cfaslCode('CFASL_P_32BIT_INT',6).
cfaslRead(6,In,Object):-get_code(I,V1),get_code(I,V2),get_code(I,V3),get_code(I,V4),Object is (V1+V2*256+V3*256*256+V4*256*256*256).
% CFASL_N_32BIT_INT
cfaslCode('CFASL_N_32BIT_INT',7).
cfaslRead(7,In,Object):-get_code(I,V1),get_code(I,V2),get_code(I,V3),get_code(I,V4),Object is -1* (V1+V2*256+V3*256*256+V4*256*256*256).
%CFASL_P_FLOAT
cfaslCode('CFASL_P_FLOAT',8).
cfaslRead(8,In,Object):-cfaslRead(In,V1),cfaslRead(In,V2),Object is V1 * 2^V2.
%CFASL_N_FLOAT
cfaslCode('CFASL_N_FLOAT',9).
cfaslRead(9,In,Object):-cfaslRead(In,V1),cfaslRead(In,V2),Object is -1 * V1 * 2^V2.
%CFASL_P_BIGNUM
cfaslCode('CFASL_P_BIGNUM',23).
cfaslRead(23,In,Object):-cfaslRead(In,Len),readBignum(In,Len,0,Object).
readBignum(In,0,N,N):-!.
readBignum(In,Len,SoFar,All):-cfaslRead(In,Num),R is SoFar +Num * (256^Len),Len2 is Len-1,readBignum(In,Len2,R,All).
%CFASL_N_BIGNUM
cfaslCode('CFASL_N_BIGNUM',24).
cfaslRead(24,In,Object):-cfaslRead(In,Len),readBignum(In,Len,0,O),Object is -1 * O.
%CFASL_KEYWORD
cfaslCode('CFASL_KEYWORD',10).
cfaslRead(10,In,Object):-cfaslRead(In,String),string_to_atom(String,O),prependAtom(':',O,Output).
prependAtom(S,Output,Output):-atom_concat(S,_,Output),!.
prependAtom(S,O,Output):-atom_concat(S,O,Output),!.
%CFASL_SYMBOL
cfaslCode('CFASL_SYMBOL',11).
cfaslRead(11,In,Object):-cfaslRead(In,String),string_to_atom(String,Object).
%CFASL_NIL
cfaslCode('CFASL_NIL',12).
cfaslRead(12,In,[]).
%CFASL_LIST
cfaslCode('CFASL_LIST',13).
cfaslRead(13,In,List):-cfaslRead(In,Len),cfaslReadList(In,Len,[],List).
cfaslReadList(In,0,N,N):-!.
cfaslReadList(In,Len,N,List):-cfaslRead(In,O),append(N,[O],Mid),Len2 is Len-1,cfaslReadList(In,Len2,Mid,List).
%CFASL_CONS
cfaslCode('CFASL_DOTTED',17).
cfaslRead(30,In,Out):-cfaslRead(In,Len),cfaslReadList(In,Len,[],List),cfaslRead(In,Dot),append(List,Dot,Out).
%CFASL_VECTOR
cfaslCode('CFASL_VECTOR',14).
cfaslRead(14,In,v(List)):-cfaslRead(In,Len),cfaslReadList(In,Len,[],List).
%CFASL_STRING
cfaslCode('CFASL_STRING',15).
cfaslRead(15,In,List):-cfaslRead(In,Len),cfaslReadString(In,Len,[],List).
cfaslReadString(In,0,N,N):-!.
cfaslReadString(In,Len,N,List):-get_code(In,O),append(N,[O],Mid),Len2 is Len-1,cfaslReadString(In,Len2,Mid,List).
%CFASL_CHARACTER
cfaslCode('CFASL_CHARACTER',16).
cfaslRead(16,In,Char):-get_char(In,Char).
%CFASL_GUID
cfaslCode('CFASL_GUID',25).
cfaslRead(25,In,guid(Data)):-cfaslRead(In,Data).
%CFASL_UNICODE_STRING
cfaslCode('CFASL_UNICODE_STRING',53).
cfaslRead(53,In,List):-cfaslRead(In,Len),cfaslReadString(In,Len,[],List).
%CFASL_UNICODE_CHAR
cfaslCode('CFASL_UNICODE_CHAR',52).
cfaslRead(52,In,Char):-get_char(In,Char).
%CFASL_BYTE_VECTOR
cfaslCode('CFASL_BYTE_VECTOR',26).
cfaslRead(26,In,v(List)):-cfaslRead(In,Len),cfaslReadList(In,Len,[],List).
%CFASL_CONSTANT
cfaslCode('CFASL_CONSTANT',30).
cfaslRead(30,In,Const):-cfaslRead(In,Len),toConstant(Len,Const).
%CFASL_NART
cfaslCode('CFASL_NART',31).
cfaslRead(31,In,Const):-cfaslRead(In,Len),toNart(Len,Const).
%CFASL_ASSERTION
cfaslCode('CFASL_ASSERTION',33).
cfaslRead(33,In,ist(Mt,Form)):-cfaslRead(In,Form),cfaslRead(In,Mt).
%CFASL_VARIABLE
cfaslCode('CFASL_VARIABLE',40).
cfaslRead(40,In,'$VAR'(Form)):-cfaslRead(In,Form).
%CFASL_EXTERNALIZATION
cfaslCode('CFASL_EXTERNALIZATION',51).
cfaslRead(51,In,'QUOTE'(Form)):-cfaslRead(In,Form).
%CFASL_UNKNOWN
cfaslCode('CFASL_HASHTABLE',18).
cfaslCode('CFASL_BTREE_LOW_HIGH',19).
cfaslCode('CFASL_BTREE_LOW',10).
cfaslCode('CFASL_BTREE_HIGH',21).
cfaslCode('CFASL_BTREE_LEAF',22).
cfaslCode('CFASL_RESULT_SET_SLICE',27).
cfaslCode('CFASL_ASSERTION_SHELL',34).
cfaslCode('CFASL_ASSERTION_DEF',35).
cfaslCode('CFASL_SOURCE',36).
cfaslCode('CFASL_SOURCE_DEF',37).
cfaslCode('CFASL_AXIOM',38).
cfaslCode('CFASL_AXIOM_DEF',39).
cfaslCode('CFASL_INDEX',41).
cfaslCode('CFASL_SPECIAL_OBJECT',50).
cfaslCode('CFASL_DICTIONARY',64).
cfaslCode('CFASL_SERVER_DEATH',-1).
cfaslRead(Type,In,E):-cfaslCode(Name,Type),throw(Name).

cfaslServer(Port):-
        tcp_socket(ServerSocket),
        catch(ignore(tcp_setopt(ServerSocket, reuseaddr)),_,true),
        at_halt(tcp_close_socket(ServerSocket)),
        attemptServerBind(ServerSocket, Port),
        tcp_listen(ServerSocket, 655),
        repeat,
	       acceptCFaslClient(ServerSocket),
        fail.
acceptCFaslClient(ServerSocket):-
		tcp_open_socket(ServerSocket, AcceptFd, _),
                cleanOldProcesses,!,
		tcp_accept(AcceptFd, ClientSocket, ip(A4,A3,A2,A1)),!,
                getPrettyDateTime(DateTime),
                sformat(Name,'Dispatcher for CFASL ~w.~w.~w.~w  started ~w ',[A4,A3,A2,A1,DateTime]),
                servantProcessCreate(killable,Name,serviceCfaslClient(ClientSocket),_,[global(128000),local(12800),trail(12800),detatched(true)]),!. %%
serviceCfaslClient(ClientSocket):-
	tcp_open_socket(ClientSocket, In, Out),!,
        setCycOption('$socket_in',In),
        setCycOption('$socket_out',Out),!,
        servCFasl(In,Out),
        flush_output,
	catch(tcp_close_socket(ClientSocket),_,true),
	thread_exit(complete).

servCFasl(In,Out):-
   thread_self(Session),
   retractall(isKeepAlive(Session)),
   asserta(isKeepAlive(Session)),
   repeat,
      once((
	 once((cfaslRead(In,PrologGoal), 
	    %set_output(Out),set_input(In),
	 debugFmt('%CFASL API Call ~q~n',[PrologGoal]), !,
      callCycApi(PrologGoal,ToplevelVars,Result))),
         cfaslWrite(Out,Result),flush_output_safe(Out))),
       isCycAPIQuit(PrologGoal),!.
%catch(callCycApi(Out,PrologGoal,ToplevelVars),E,writeFmtFlushed(Out,'500 "~q"\n',[E])),
%flush_output_safe(Out))),
      	
% NUMBER
cfaslWrite(Out,O):-number(O),encodeNumber(O,E),cfaslWriteSeq(Out,E).
cfaslWriteSeq(Out,[]):-!.
cfaslWriteSeq(Out,[V|O]):-put(Out,V),cfaslWriteSeq(Out,O).
% STRING                                                                                                          //53
cfaslWrite(Out,S):-is_string(S),string_to_atom(S,A),atom_codes(A,C),length(C,L),put(Out,53),cfaslWrite(Out,L),cfaslWriteSeq(Out,C).
cfaslWrite(Out,S):-atom(S),atom_concat('"',_,S),unquoteAtom(S,New),atom_codes(New,C),cfaslWrite(Out,C).
% NIL
cfaslWrite(Out,[]):-put(Out,12).
cfaslWrite(Out,'NIL'):-put(Out,12).
% LIST
cfaslWrite(Out,[H|T]):-proper_list([H|T]),length([H|T],N),put(Out,13),cfaslWrite(Out,N),cfaslWriteList(Out,[H|T]).
cfaslWriteList(Out,[]):-!.
cfaslWriteList(Out,[H|T]):-cfaslWrite(Out,H),cfaslWriteList(Out,T).
% CONS
cfaslWrite(Out,[H|T]):-not(proper_list([H|T])),properPart([H|T],PP),length(PP,N),put(Out,17),cfaslWrite(Out,N),cfaslWriteList(Out,PP),improperPart([H|T],IP),cfaslWrite(Out,IP).
% HLVAR
cfaslWrite(Out,'$VAR'(N)):-put(Out,40),cfaslWrite(Out,N).
% ELVAR
cfaslWrite(Out,V):-var(V),term_to_atom(V,A),cfaslWrite(Out,A).
% CFASL_KEYWORD 
cfaslWrite(Out,V):-atom(V),atom_concat(':',N,V),put(Out,10),string_to_atom(S,N),cfaslWrite(Out,S).
%CFASL_SYMBOL
cfaslWrite(Out,V):-atom(V),atom_concat('_',N,V),atom_concat('?',N,VV),cfaslWrite(Out,VV).
cfaslWrite(Out,V):-atom(V),once(toUppercase(V,VU)),VU==V,put(Out,11),string_to_atom(S,N),cfaslWrite(Out,S).
%CONSTANT
cfaslWrite(Out,V):-atom(V),fromConstant(V,Guid),put(Out,30),cfaslWrite(Out,Guid),!.
%GUID
cfaslWrite(Out,guid(Data)):-put(Out,25),cfaslWrite(Out,Data).
%NART
cfaslWrite(Out,nart([H|T])):-put(Out,51),put(Out,31),cfaslWrite(Out,[H|T]).
cfaslWrite(Out,nart(Int)):-put(Out,31),cfaslWrite(Out,Int).
%ASSERTION
cfaslWrite(Out,ist(Mt,Assert)):-put(Out,51),put(Out,33),cfaslWrite(Out,Assert),cfaslWrite(Out,Mt).

%:-module_transparent(assertion/13).
%:-dynamic(assertion/13).
%:-multifile(assertion/13).

:-module_transparent(constant/4).
:-dynamic(constant/4).
%user:constant(A,B,C,D):-cyc:constant(A,B,C,D).

:-dynamic(constantGuid/2).
constantGuid(Const,ID):-cyc:constant(Const,_,ID,_).
fromConstant(Const,guid(BB)):-constantGuid(Const,Guid),!,atom_codes(Guid,BB).
fromConstant(Const,Id):-constantId(Const,Id),!.
fromConstant(Const,BB):-sformat(S,'(constant-external-id (find-constant "~w"))',[Const]),evalSubL(S,X,_),balanceBinding(X,BB),
         BB=guid(String),string_to_atom(String,Atom),
         asserta(constantGuid(Const,Atom)).

:-dynamic(constantId/2).
constantId(Const,ID):-cyc:constant(Const,ID,_,_).
toConstant(Len,Const):-constantId(Const,Len),!.
toConstant(Len,Const):-integer(Len),sformat(S,'(find-constant-by-internal-id ~w)',[Len]),evalSubL(S,X,_),balanceBinding(X,BB),
         unhashConstant(BB,Const),
         asserta(constantId(Const,Len)),!.
toConstant(guid(Guid),Const):-!,toConstant(Guid,Const).
toConstant(Guid,Const):-is_string(Guid),!,string_to_atom(Guid,Atom),toConstant(Atom,Const).
toConstant(Guid,Const):-constantGuid(Const,Guid),!.
toConstant(Len,Const):-concat_atom([A,B|C],'-',Len),sformat(S,'(find-constant-by-external-id (string-to-guid "~w"))',[Len]),evalSubL(S,X,_),balanceBinding(X,BB),
         unhashConstant(BB,Const),
         asserta(constantGuid(Const,Len)),!.
toConstant(C,CU):-unhashConstant(C,CU).

isTrue(V):-var(V),!.
isTrue([]):-!,fail.
isTrue('NIL'):-!,fail.
isTrue('nil'):-!,fail.
isTrue('false'):-!,fail.
isTrue('fail'):-!,fail.
isTrue('no'):-!,fail.
isTrue(X:_):-!,isTrue(X).
isTrue(_).



:-module_transparent(user:nart/3).

nart(not,not,not).
toNart(Id,Nart):-nart(Id,_,Nart),!.
toNart(Id,Nart):-nart(_,Id,Nart),!.
toNart(nart(Nart),nart(Nart)):-!.
toNart(Nart,nart(Nart)).
 

unhashConstant(HConst,Const):-atom_concat('#$',Const,HConst),!.
unhashConstant(Const,Const).

properPart(T,[]):-not(T=[_|_]).
properPart([H|T],[H|CDR]):-properPart(T,CDR).
improperPart(T,T):-not(T=[_|_]).
improperPart([H|T],CDR):-improperPart(T,CDR).






isWhole(O,W):-number(O),W is round(O),atom_number(A2,W),atom_number(A1,O),(atom_concat(A2,'.0',A1);atom_concat(A2,'',A1)).

encodeIntNumber(Int,[NCode|Rest]):-Int<0, NInt is -1 * Int,encodeIntNumber(NInt,[Code|Rest]),NCode is Code+1,!.
encodeIntNumber(Int,[0,Int]):-Int<256.
encodeIntNumber(Int,[2,V1,V2]):-Int<65536,V1 is 255 /\ Int,V2 is ((255*256) /\ Int)>>8.
encodeIntNumber(Int,[4,V1,V2,V3]):-Int<16777216,V1 is 255 /\ Int,V2 is ((255*256) /\ Int)>>8,V3 is ((255*256) /\ Int)>>16.
encodeIntNumber(Int,[6,V1,V2,V3,V4]):-Int<4294967296, V1 is 255 /\ Int,V2 is ((255*256) /\ Int)>>8 ,V3 is ((255*256*256) /\ Int)>>16,V4 is ((255*256*256*256) /\ Int)>>24.
encodeIntNumber(Int,[23|VAL]):-V1 is 255 /\ Int,Int2 is (Int - V1)>>8, encodeIntNumber(Int2,1,REST,Len),encodeIntNumber(Len,LE),append(LE,[V1|REST],VAL).
encodeIntNumber(Int,N,[],N):-Int<1,!.
encodeIntNumber(Int,N,[V1|All],O):-V1 is 255 /\ Int, NN is N+1, IntN is (Int-V1)>>8,encodeIntNumber(IntN,NN,All,O).


%CFASL_P_BIGNUM
%cfaslRead(23
encodeNumber(N,E):-integer(N),!,encodeIntNumber(N,E).
encodeNumber(N,E):-encodeRNumber(N,E).
encodeRNumber(Int,[NCode|Rest]):-Int<0, NInt is -1.0 * Int,encodeRNumber(NInt,[Code|Rest]),NCode is Code+1,!.
%encodeRNumber(N,[8|RESAT]):-isWhole(N,W),!,encodeIntNumber(W,IE),append(IE,[0,0],RESAT).
encodeRNumber(N,[8|STUFF]):-encodeRNumber(N,0,W,RR),encodeIntNumber(W,IE),encodeIntNumber(RR,RE),append(IE,RE,STUFF).

encodeRNumber(N,R,W,R):-isWhole(N,W),!.
encodeRNumber(N,R,NNN,RRR):-NN is N*2,RR is R-1,encodeRNumber(NN,RR,NNN,RRR).


   
%callCycApi(Out,[string("prologProcForCycPred-pos-proc"), ['#$prologCycPred2', 1, _G5354]], [var0=_G5354|_G5514], _G5523).
callCycApi(Out,[string(Predstring), Call],ToplevelVars):- string_to_atom(Predstring,Atom),concat_atom([H|T],'-',Atom),!,
      cycPredCall([H|T],Call,Result),!,
      writel(Out,Result,ToplevelVars),!.
         
callCycApi(Out,PrologGoal,ToplevelVars):-cycGoal(PrologGoal,ToplevelVars,Result),writel(Out,Result,ToplevelVars),!.





:-dynamic(evalSubLCache/2).


%passAlong([H|T],Y):-X=..[H|T],!,passAlong(X,Y).
%passAlong(X,Y):-evalSubLCache(X,Y),!.
passAlong(X,Y):-toCycApiExpression(X,CycLX),evalSubL(CycLX,Y,Vars),asserta(evalSubLCache(X,Y)).


notraceTry(Goal):-notrace(ignore(catch(Goal,E,debugFmt(Goal-E)))).


cycGoal(X,Y,Z):-debugFmt('?- ~q.~n',[cycGoal(X,Y,Z)]),fail.
cycGoal([],Vars,[]):-!.
%cycGoal(['END-OF-FILE'|_],_,'NIL'):-!.
cycGoal(['END-OF-FILE'|_],_,'NIL'):-!.
%cycGoal([APIQUIT|_],_,'T'):-isCycTrue(APIQUIT),!.
%writecycGoal(['CONSTANT-INFO-FROM-GUID-STRINGS'|
%cycGoal(['CYC-QUERY',PrologGoal,MT|OPTS],ToplevelVars,Result):-sterm_to_pterm(PrologGoal,PTERM), findall(ToplevelVars,PTERM,Result),!.
%cycGoal(X,Vars,Lisp):- debugFmt(passIn(X)),passAlong(X,Y),toCycApiExpression(Y,Vars,Lisp),debugFmt('\n% Pass back->'),debugFmt(Lisp),!.

cycGoal(['PEVAL', string(EVAL)], _G94, Result):-
      catch((atom_to_term(EVAL,PTERM,ToplevelVars),catch(predicate_property(PTERM,_),_,fail),!,
          debugFmt('?- ~q.~n',[PTERM]),
         catch(findall(ToplevelVars,PTERM,Result),E,Result=[E]),
         debugFmt('-> ~q.~n',[ToplevelVars:Result])),E,Result=[E]),!.


cycGoal(PrologGoal,ToplevelVars,Result):-once(sterm_to_pterm(PrologGoal,PTERM)),catch(predicate_property(PTERM,_),_,fail),!,
          debugFmt('?- ~q.~n',[PTERM]),
         findall(ToplevelVars,PTERM,Result),!,
          debugFmt('-> ~q.~n',[ToplevelVars:Result]),!.
          
%cycGoal([F|A],ToplevelVars,Result):-toUppercase([F|A],FU),not([F|A]==FU),!,cycUCaseGoal(FU,ToplevelVars,Result),!.
cycGoal(X,Y,Z):-functor(X,F,_),sformat(S,'"~q is not defined in the API"',[F]),throw(unknown(S,X)),!.

%cycGoal(['FIND-CONSTANT',STRING],ToplevelVars,Result):-string_to_atom(String,Atom),atom_concat('#$',Atom,Result).
%cycGoal(['DEFVAR',NAME,VALUE|_],ToplevelVars,NAME).


user:keepUpWithAssertions:-
      evalSubL('(assertion-count)',X:Var),
       flag('$cyc_assertion_pointer',_,X-100),
       crawlAssertionsBG.





crawlAssertions:- 
     flag('$cyc_assertion_pointer',_,0),crawlAssertionsBG,crawlAssertionsBG.
   
crawlAssertionsBG:-
createProcessedGoal((
      repeat,
      flag('$cyc_assertion_pointer',Current,Current+1),
      ignore(once(once(catch(cacheAssertionById(Current),_,(flag('$cyc_assertion_pointer',Down,Down-100),sleep(5),fail));flag('$cyc_assertion_pointer',Down,Down-100)))),
      fail)).


:- dynamic cycAssertionCache/6.
:- index(cycAssertionCache(0,1,1,1,1,0)).
cacheAssertionById(Id):-cycAssertionCache(Id,Assertion,Mt,Strength,Direction,Vars),!.
cacheAssertionById(Id):-getAssertionById(Id,Assertion,Mt,Strength,Direction,Vars),
      asserta(cycAssertionCache(Id,Assertion,Mt,Strength,Direction,Vars)).


getAssertionById(Id,Assertion,Mt,Strength,Direction,Vars):-
      sformat(S,'(clet ((assrt (find-assertion-by-id ~w)))(list (assertion-id assrt) (assertion-formula assrt) (assertion-mt assrt) (assertion-el-formula assrt) (asserted-by assrt)(assertion-truth assrt) (asserted-when assrt)  (GET-ASSERTED-ARGUMENT  asrt) (assertion-direction assrt)  (assertion-strength assrt)))',[Id]),
      evalSubL(S,Surf),
      once(getAssertionById2(Surf,Assertion,Mt,Strength,Direction,Vars)).

getAssertionById2([[[[ist,Mt,[Assertion]]],Strength,Direction]]:Vars,NewAssertion,NewMt,Strength,Direction,GVars):-
         s2p(Assertion,NewAssertion,MoreVars),s2p(Mt,NewMt,MtVars),append(MoreVars,Vars,AllVars1),append(AllVars1,MtVars,AllVars),ssort(AllVars,GVars).
getAssertionById2(Surf:Vars,Surf,'NIL',Strength,Direction,Vars).

ssort(X,[]):-var(X),!.
ssort(X,Y):-sort(X,Y).


'API-QUIT':-'api-quit'.
'api-quit':-thread_self(Session),retractall(isKeepAlive(Session)),told.


listingToString(Pred,RS):-
      findall(':-'(Pred,X),clause(Pred,X),R1),
      termCyclify(R1,R2),
      toCycApiExpression(R2,_,R),escapeString(R,RS),!.


noCaseChange([],[]):-!.
noCaseChange(VAR,VAR):-var(VAR),!.
noCaseChange(MiXed):-atom(MiXed),atom_concat('#$',_,MiXed),!.
noCaseChange(c(VAR),c(VAR)):-!.

toUppercase(MiXed,MiXed):-noCaseChange(MiXed),!.
toUppercase(V,V2):-string(V),!,atom_codes(V,VC),toUppercase(VC,CVC),string_to_atom(V2,CVC),!.
toUppercase(95,45):-!.
toUppercase(I,O):-integer(I),!,to_upper(I,O).
toUppercase([A|L],[AO|LO]):-
   toUppercase(A,AO),!,
   toUppercase(L,LO),!.
toUppercase(MiXed,CASED):-atom(MiXed),upcase_atom(MiXed,CASED),!.
toUppercase(MiXed,CASED):-atom(MiXed),!,
   atom_codes(MiXed,Codes),
   toUppercase(Codes,UCodes),
   atom_codes(CASED,UCodes),!.
toUppercase(MiXed,CASED):-compound(MiXed),MiXed=..MList,toUppercase(MList,UList),!,CASED=..UList.
toUppercase(A,A).

toLowercase(MiXed,MiXed):-noCaseChange(MiXed),!.
toLowercase(V,V2):-string(V),!,atom_codes(V,VC),toLowercase(VC,CVC),string_to_atom(V2,CVC),!.
toLowercase(95,45):-!.
toLowercase(I,O):-integer(I),!,to_lower(I,O).
toLowercase([A|L],[AO|LO]):-
   toLowercase(A,AO),!,
   toLowercase(L,LO),!.
toLowercase(MiXed,CASED):-atom(MiXed),downcase_atom(MiXed,CASED),!.
toLowercase(MiXed,CASED):-atom(MiXed),!,
   atom_codes(MiXed,Codes),
   toLowercase(Codes,UCodes),
   atom_codes(CASED,UCodes),!.
toLowercase(MiXed,CASED):-compound(MiXed),MiXed=..MList,toLowercase(MList,UList),!,CASED=..UList.
toLowercase(A,A).


toPropercase(VAR,VAR):-var(VAR),!.
toPropercase([],[]):-!.
toPropercase([CX|Y],[D3|YY]):-!,toPropercase(CX,D3),toPropercase(Y,YY).
toPropercase(D3,DD3):-atom(D3),member(V,[' ','-','_',':','mt','doom','Mt','Doom']),concat_atom([L,I|ST],V,D3),toPropercase([L,I|ST],LIST2),toPropercase(V,VV),concat_atom(LIST2,VV,DD3).
toPropercase(CX,Y):-atom(CX),name(CX,[S|SS]),char_type(S,to_lower(NA)),name(NA,[N]),name(Y,[N|SS]),!.
toPropercase(MiXed,UPPER):-compound(MiXed),MiXed=..MList,toPropercase(MList,UList),!,UPPER=..UList.
toPropercase(A,A).


toCamelcase(VAR,VAR):-var(VAR),!.
toCamelcase([],[]):-!.
toCamelcase([CX|Y],[D3|YY]):-!,toCamelcase(CX,D3),toCamelcase(Y,YY).
toCamelcase(D3,DD3):-atom(D3),member(V,[' ','-','_',':','mt','doom','Mt','Doom']),concat_atom([L,I|ST],V,D3),toCamelcase([L,I|ST],LIST2),toCamelcase(V,VV),concat_atom(LIST2,VV,DD3).
toCamelcase(CX,Y):-atom(CX),name(CX,[S|SS]),char_type(S,to_upper(NA)),name(NA,[N]),name(Y,[N|SS]),!.
toCamelcase(MiXed,UPPER):-compound(MiXed),MiXed=..MList,toCamelcase(MList,UList),!,UPPER=..UList.
toCamelcase(A,A).

      
      

% ===========================================================
% PROLOGD for OpenCyc SERVICE
% ===========================================================

serviceCycApiRequest5(In,Out):-
       readCycL(In,Trim), 
       isDebug(format('"~s"~n',[Trim])),
       serviceCycApiRequestSubP(In,Trim,Out).
   
serviceCycApiRequestSubP(In,Trim,Out):-
       getSurfaceFromChars(Trim,[Result],ToplevelVars),!,
       balanceBinding(Result,PrologGoal),
        thread_self(Session),
        retractall(isKeepAlive(Session)),
        xmlClearTags,
       invokePrologCommand(Session,In,Out,PrologGoal,ToplevelVars,Returns).

serviceCycApiRequestSubP(Trim):-
       getSurfaceFromChars(Trim,[Result],ToplevelVars),!,
       balanceBinding(Result,PrologGoal),
	 ignore(catch(PrologGoal,_,true)).

% ===========================================================
% PROLOGD for Java SERVICE
% ===========================================================
serviceJavaApiRequest(In,Out):-
        thread_self(Session),
        retractall(isKeepAlive(Session)),
        xmlClearTags,
   %     writeFmt(Out,'<session:id goal="~q">\n',[Session]),
        flush_output,
        repeat,
               ignore((once(( catch(
                        read_term(In,PrologGoal,[variable_names(ToplevelVars),character_escapes(true),syntax_errors(error)]),
                        E,
                        writeErrMsg(Out,E)),
                invokePrologCommand(Session,In,Out,PrologGoal,ToplevelVars,Returns))))),
                notKeepAlive(Out,Session),!.

invokePrologCommand(Session,In,Out,PrologGoal,ToplevelVars,Returns):-
         writeFmt(Out,'<cycml:solutions goal="~q">\n',[PrologGoal:ToplevelVars]),var(PrologGoal),!.

invokePrologCommand(Session,In,Out,PrologGoal,ToplevelVars,Returns):-flush_output,
      set_output(Out),set_input(In),!,
      %   tell(Out),
	PrologGoal,
        inform_xml_vars(PrologGoal,ToplevelVars),
        flush_output,
        xmlExitTags,!.


% ===========================================================
% HTTPD SERVICE
% ===========================================================
%prologHTTPD(Port):-http_server(reply, [port(Port)]).

serviceHttpRequest(ClientInfo,In,Out):-!,
        setCycOption(client,html),
        readHTTP(In,Request),!,
        tell(Out),
        ignore(reply([clientinfo=ClientInfo|Request])),
        told.


reply(Request) :-
        debugFmt('REQUEST DATA: ~q',[Request]),
        setCycOption(client,html),!,
        %Set-Cookie: nameTest=valueTest
        writeFmtFlushed('HTTP/1.1 200 OK\nServer: LOGICMOO HTTPD\nContent-Type: text/html\n\n',[]),
        once(processRequest(Request)),!,
        flush_output,!.

% ===================================================================
% Semi-Prolog Dependant Code
% ===================================================================
sigma_ua(X):-processRequest(X).


:-module_transparent(user:processRequestHook/1).
:-dynamic(user:processRequestHook/1).
:-multifile(user:processRequestHook/1).

processRequest(X):-catch(user:processRequestHook(X),E,( debugFmt('processRequestHook: "~q" \n',[X:E]), fail)),!.

% =================================================
% SubL
% =================================================
:-dynamic(processRequestHook/1).
:-multifile(processRequestHook/1).
:-module_transparent(processRequestHook/1).

user:processRequestHook(ARGS):-member(file='subl.moo',ARGS),!,
      ignore(member(formula=W,ARGS)),
      ignore(W=''),
      writeHTMLStdHeader('SubL Interactor'),
      writeFmtFlushed('
      <form method="GET">
	<p><textarea rows="9" name="formula" cols="40">~w</textarea><br>
	<input type="submit" value="Call" name="submit">&nbsp;<input type="reset" value="Reset" name="resetButton"></p>
      </form>',[W]),
      writeHTMLStdFooter,!.
	
        
processRequest(X):-
        once((writeHTMLStdHeader(X),  
        once(showCycProcessHTML),
        % serviceSoapRequest(In,Out),
	writeHTMLStdFooter)),!.


readHTTP(In,DOPTS):-!,
      http_header:http_read_request(In,Request),!,
      getData(Request,Data),!,decodeRequestArguments(Data,DData),!,
     request_to_options(Request,Options),!,
      append(Options,DData,DOPTS),!.

%request_to_options(Request,Options),
request_to_options([],[]):-!.
request_to_options([R|Request],[O|Options]):-r2o(R,O),!,request_to_options(Request,Options),!.

r2o(path(R),file=V):-atom_concat('/',V,R),!.
r2o(R,N=V):-functor(R,N,1),R=..[_,V],!.
r2o(RO,RO).

getData(Request,Data):-member(method(post), Request),http_read_data(Request, Data, []),!.
getData(Request,Data):-member(search(Data), Request),!.
getData(Request,[]).


readHTTP(In,Request):-
        read_line_with_nl(In, Codes, []),
        append([71, 69, 84, _, _],Stuff,Codes), % "GET /"
        append(RequestCodes,[72,84,84,80|_],Stuff),
        atom_codes(RequestEncoded,RequestCodes),
        decodeRequest(RequestEncoded,Request).

readHTTP(In,Request):-
        read_line_with_nl(In, Codes, []),
        append([80, 79, 83, 84,_, _],Stuff,Codes), % "POST /"
        append(RequestCodes,[72,84,84,80|_],Stuff),
        atom_codes(RequestEncoded,RequestCodes),
        decodeRequest(RequestEncoded,Request).


read_line_with_nl(Fd, Codes, Tail) :-
        get_code(Fd, C0),
        read_line_with_nl(C0, Fd, Codes, Tail).
read_line_with_nl(end_of_file, _, Tail, Tail) :- !.
read_line_with_nl(-1, _, Tail, Tail) :- !.
read_line_with_nl(10, _, [10|Tail], Tail) :- !.
read_line_with_nl(C, Fd, [C|T], Tail) :-
        get_code(Fd, C2),
        read_line_with_nl(C2, Fd, T, Tail).

decodeRequest(RequestEncoded,[file=FileT]):-
      www_form_encode(RequestDecoded,RequestEncoded),
      concat_atom([File],'?',RequestDecoded),!,
      decodeRequestAtom(File,FileT).
decodeRequest(RequestEncoded,[file=FileT|ENCARGS]):-
      concat_atom([File|_],'?',RequestEncoded),
      atom_concat(File,'?',FilePart),
      atom_concat(FilePart,ARGS,RequestEncoded),
      concat_atom(ArgList,'&',ARGS),
      decodeRequestAtom(File,FileT),!,
      decodeRequestArguments(ArgList,ENCARGS),!.

decodeRequestArguments([],[]):-!.
decodeRequestArguments([ctx=Value|List],[ctx=CValue,theory=KValue|ARGS]):- concat_atom([KValue,CValue],':',Value),!,
          decodeRequestArguments(List,ARGS).
decodeRequestArguments([Arg|List],[DDName=DDValue|ARGS]):-
          split_nv(Arg,Name,Value),
          decodeRequestAtom(Name,DName),
          decodeRequestAtom(Value,DValue),!,
          decodeRequestArguments(List,ARGS),!,
          unatom(DName,DDName),
          unatom(DValue,DDValue),!.


refixArgs([],[]):-!.
refixArgs([A|RGS],[R|ARGS]):-
      refixArg(A,R),!,
      refixArgs(RGS,ARGS).
refixArg(A=B,AA=BB):-unatom(A,AA),unatom(B,BB).
refixArg(A,AA):-unatom(A,AA).

unatom(B,BB):-atom(B),catch(atom_to_term(B,BB,_),_,fail),ground(BB),!.
unatom(B,B).



%ctx=PrologMOO%3ASTRUCTURAL-ONTOLOGY&amp;

split_nv(Name=Value,Name,Value):-!.
split_nv(Arg,Name,Value):-concat_atom([Name,Value],'=',Arg),!.
split_nv(Arg,Arg,Arg).
                        

decodeRequestAtom(RequestEncoded,X):-www_form_encode(RequestDecoded,RequestEncoded),!,decodeRequestAtom2(RequestDecoded,X),!.
decodeRequestAtom2(A,A):-var(A),!.
decodeRequestAtom2(tn,tn):-!.
decodeRequestAtom2(N,N):-number(N),!.
decodeRequestAtom2(A=B,AA=BB):-decodeRequestAtom2(A,AA),decodeRequestAtom2(B,BB),!.
decodeRequestAtom2(A,T):-catch(atom_to_term(A,T,_),_,fail),number(T),!.
decodeRequestAtom2(A,T):-catch(atom_to_term(A,T,_),_,fail),not(var(T)),not(compound(T)),!.
decodeRequestAtom2(A,T):-atom(A),catch(atom_codes(A,[95|_]),_,fail),catch(atom_to_term(A,T,_),_,fail),!.
decodeRequestAtom2(Request,RequestT):-atom_concat(RequestT,' ',Request),!.
decodeRequestAtom2(Request,Request).


% ===========================================================
% NATIVE SERVICE
% ===========================================================

serviceNativeRequestAsRDF(_,In,Out):-
        writeFmt(Out,'<?xml version="1.0" encoding="ISO-8859-1"?>\n',[]),
        thread_self(Session),
        retractall(isKeepAlive(Session)),
        xmlClearTags,
        repeat,
                catch(
                        read_term(In,PrologGoal,[variable_names(ToplevelVars),character_escapes(true),syntax_errors(error)]),
                        E,
                        writeErrMsg(Out,E)),
                %debugFmt(PrologGoal:ToplevelVars),
                invokePrologCommandRDF(Session,In,Out,PrologGoal,ToplevelVars,Returns),
                notKeepAlive(Out,Session),!.

notKeepAlive(Out,Session):-isKeepAlive(Session),
        write(Out,
                'complete.\n'
                %'<cycml:keepalive/>\n'
                                ),flush_output_safe(Out),!,fail.
notKeepAlive(Out,Session):-flush_output_safe(Out).


keep_alive:-thread_self(Me),retractall(isKeepAlive(Me)),assert(isKeepAlive(Me)),writeFmtFlushed('<keepalive/>\n',[]).
goodbye:-thread_self(Me),retractall(isKeepAlive(Me)),writeFmt('<bye/>/n',[]).


invokePrologCommandRDF(Session,In,Out,PrologGoal,ToplevelVars,Returns):-var(PrologGoal),!.

invokePrologCommandRDF(Session,In,Out,PrologGoal,ToplevelVars,Returns):-
        term_to_atom(Session,Atom),concat_atom(['$answers_for_session',Atom],AnswersFlag),
        writeFmt(Out,'<cycml:solutions goal="~q">\n',[PrologGoal]),
        flag(AnswersFlag,_,0),
        set_output(Out),set_input(In),!,
        getCputime(Start),
        callNondeterministicPrologCommandRDF(Session,AnswersFlag,In,Out,PrologGoal,ToplevelVars),
        xmlExitTags,
        getCputime(End),
        flag(AnswersFlag,Returns,Returns),
%       (Returns > 0 ->
%               writeFmt(Out,'<cycml:yes/>\n',[]) ;
%               writeFmt(Out,'<cycml:no/>\n',[])),!,
        Elapsed is End -Start,
        writeFmt(Out,'</cycml:solutions answers="~w" cputime="~g">\n',[Returns,Elapsed]),!.

callNondeterministicPrologCommandRDF(Session,AnswersFlag,In,Out,PrologGoal,ToplevelVars):-
        ground(PrologGoal),!,
        catch(
                (PrologGoal,
                 flag(AnswersFlag,Answers,Answers+1),
                 writePrologToplevelVarsXML(Out,PrologGoal,AnswersFlag,ToplevelVars)
                 ),
           Err,writeErrMsg(Out,Err,PrologGoal)),!.

callNondeterministicPrologCommandRDF(Session,AnswersFlag,In,Out,PrologGoal,ToplevelVars):-
        catch(
                (PrologGoal,
                 flag(AnswersFlag,Answers,Answers+1),
                 writePrologToplevelVarsXML(Out,PrologGoal,AnswersFlag,ToplevelVars),
                 fail),
           Err,writeErrMsg(Out,Err,PrologGoal)),!.
callNondeterministicPrologCommandRDF(Session,AnswersFlag,In,Out,PrologGoal,ToplevelVars):-!.


writePrologToplevelVarsXML(Out,PrologGoal,AnswersFlag,ToplevelVars):-
         flag(AnswersFlag,Answers,Answers),
        writeFmt(Out,'<cycml:result solution="~w">\n',[Answers]),
        writePrologToplevelVarsXML2(Out,ToplevelVars),
        writeFmt(Out,'</cycml:result>\n',[]),!.

writePrologToplevelVarsXML2(Out,[]):-!.
writePrologToplevelVarsXML2(Out,[Term|REST]):-!,Term=..[_,N,V],
         writeFmtFlushed(Out,'       <cycml:p>~w = ~q</cycml:p>\n',[N,V]),
         writePrologToplevelVarsXML2(Out,REST),!.


writeFmt(A,B,C):-!.
writeFmt(A,B):-!.

writeFmt(A,B,C):-
        writeFmtFlushed(A,B,C).
writeFmt(A,B):-
        writeFmtFlushed(A,B).


throwCyc(Module,Type,Details):-
        current_prolog_flag(debug_on_error, DebugOnError),
        set_prolog_flag(debug_on_error, false),!,
        throw(cycException(Module,Type,Details,DebugOnError)),
        ifInteractive(debugFmt('Post throwCyc')),!.

ifInteractive(X):-X.


% ===========================================================
% NATIVE SOAPD SERVER FOR SWI-PROLOG
% ===========================================================

			    
%:-module(cyc_soap,[]).

% :-include('cyc_header.pl').

:-dynamic_transparent(xmlCurrentOpenTags/2).

serviceSoapRequest(In,Out):-
      debugFmt('SOAP Request'),
        catch(read_do_soap(stream(In),Out),E,
        writeFmt(Out,'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<error>~w</error>\n',[E])),
        flush_output_safe(Out).


read_do_soap(Source):-
        open(Source,read,Stream),
        read_do_soap(Stream,user_output).

read_do_soap(Source,Out):-
       thread_self(Self),
        write(Out,'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'),
       % writeFmt(Out,'<?xml version="1.0" encoding="ISO-8859-1"?>\n<answer thread="~w">\n',[Self]),
        flush_output_safe(Out),
        load_structure(Source,RDF,[]),
        structure_to_options(RDF,Options),
%       writeFmt(user_error,'structure="~q"\noptions="~q"\n',[RDF,Options]),
        flush_output_safe(user_error),
        processRequest([client=soap|Options]).
        %writeFmt(Out,'</answer>\n',[]).


% request
structure_to_options([element(request, Options, [Atom])],[submit=ask,sf=Atom|Options]):-!.

% assert
structure_to_options([element(assert, Options, [Atom])],[submit=assert,sf=Atom|Options]):-!.
structure_to_options([element(asssertion, Options, [Atom])],[submit=assert,sf=Atom|Options]):-!.
structure_to_options([element(assertion, Options, [Atom])],[submit=assert,sf=Atom|Options]):-!.

% get inner
structure_to_options([element(Ptag, ['xmlns:cyc'=Server], Inner)],[opt_server=Server,opt_outter=Ptag|Out]):-!,
        structure_to_options(Inner,Out).


writeFmtServer(A,B):-format(A,B),flush.
writeFmtServer(O,A,B):-format(A,B),flush_output_safe(O).

xmlOpenTag(Name):-thread_self(Self),asserta(xmlCurrentOpenTags(Self,A)),writeFmtServer('<~w>',[Name]),!.
xmlOpenTagW(Out,Name,Text):-thread_self(Self),asserta(xmlCurrentOpenTags(Self,A)),writeFmtServer(Out,'~w',[Text]),!.

xmlCloseTag(Name):-thread_self(Self),ignore(retract(xmlCurrentOpenTags(Self,A))),writeFmtServer('</~w>',[Name]),!.
xmlCloseTagW(Name,Text):-thread_self(Self),ignore(retract(xmlCurrentOpenTags(Self,A))),writeFmtServer('~w',[Text]),!.
xmlCloseTagW(Out,Name,Text):-thread_self(Self),ignore(retract(xmlCurrentOpenTags(Self,A))),writeFmtServer(Out,'~w',[Text]),!.

xmlClearTags:-thread_self(Self),retractall(xmlCurrentOpenTags(Self,A)).

xmlExitTags:-thread_self(Self),retract(xmlCurrentOpenTags(Self,A)),writeFmtServer('</~w>',[Name]),fail.
xmlExitTags.


% ===========================================================
% Insert
% ===========================================================
parse_cyc_soap(Options):-memberchk(submit=assert,Options),!,
        getCycOption(opt_ctx_assert='BaseKB',Ctx),
        getCycOption(opt_theory='doom:DataMt',Context),
        getCycOption(sf=surf,Assertion),
        atom_codes(Assertion,Assertion_Chars),
        getCycOption(user='Web',User),
        getCycOption(interp='cycl',Interp),
        logOnFailure(getCycOption(tn=_,EXTID)),
        %sendNote(user,'Assert',formula(NEWFORM),'Ok.'). %,logOnFailure(saveCycCache)
        logOnFailure(getCleanCharsWhitespaceProper(Assertion_Chars,Show)),!,
        xml_assert(Show,Ctx,Context,User).

xml_assert(Show,Ctx,Context,User):-
        getSurfaceFromChars(Show,STERM,Vars),
        toCycApiExpression(STERM,NEWFORM),
        xml_assert(Show,NEWFORM,Vars,Ctx,Context,User).

xml_assert(Show,Ctx,Context,User):-!,
        writeFmt('<assertionResponse accepted="false">\nUnable to parse: "~s"\n</assertionResponse>\n',[Show]).

xml_assert(Show,NEWFORM,Vars,Ctx,Context,User):-
        logOnFailure(getTruthCheckResults(tell,[untrusted],surface,NEWFORM,Ctx,STN,Context,Vars,Maintainer,Result)),
        (Result=accept(_) ->
                        (
                        once(invokeInsert([trusted,canonicalize,to_mem],surface,NEWFORM,Ctx,EXTID,Context,Vars,User)),
                        write('<assertionResponse accepted="true">\nOk.\n</assertionResponse>\n')
                        )
                        ;
                        (
                        Result=notice(FormatStr,Args),
                        write('<assertionResponse accepted="false">\n'),
                        writeFmt(FormatStr,Args),
                        write('\n</assertionResponse>\n')
                        )
        ),!.

xml_assert(Show,NEWFORM,Vars,Ctx,Context,User):-!.


% ===========================================================
% Ask a Request
% ===========================================================
parse_cyc_soap(Options):-memberchk(submit=ask,Options),!,make,
        %write('<!DOCTYPE cyc:ask SYSTEM "/opt/tomcat-4.0/webapps/cyc-1.4b1/dtd/java_prolog.dtd">\n'),
        write('<cycml:ask xmlns:cycml="http://localhost">\n'),
        getCycOption(opt_ctx_request='BaseKB',Ctx),
        getCycOption(opt_theory='doom:DataMt',Context),
        getCycOption(sf=surf,Askion),
        atom_codes(Askion,Askion_Chars),
        getCycOption(user='Web',User),
        getCycOption(interp='cycl',Interp),
         logOnFailure(getCleanCharsWhitespaceProper(Askion_Chars,Show)),!,
         logOnFailure(getSurfaceFromChars(Show,STERM,Vars)),!,
         logOnFailure(toCycApiExpression(STERM,NEWFORM)),!,
              logOnFailure(once(( NEWFORM=comment(_) ->
                     (writeFmt('<cycml:error>Syntax Error: Unmatched parentheses in "~s"</cycml:error>\n',[Show]),!,FORM=_) ;(!,
                     logOnFailure(invokeRequest_xml(NEWFORM,ChaseVars,Ctx,TrackingAtom,Context,User,Vars,CPU))
                     )))),
        write('</cycml:ask>\n').

:-dynamic_transparent(invokeRequestToBuffer(NEWFORM,ChaseVars,Ctx,TrackingAtom,Context,User,Vars,CPU)).

invokeRequest_xml(NEWFORM,ChaseVars,Ctx,TrackingAtom,Context,User,Vars,CPU):-
        invokeRequestToBuffer(NEWFORM,ChaseVars,Ctx,TrackingAtom,Context,User,Vars,CPU),
        final_answer(Logic:How),
        invoke_final_answer(Logic,How,CPU).

invoke_final_answer(possible,How,CPU):-!,
        writeFmt('<requestResponse yesno="~w" numBindings="0" seconds="~w"/>\n',[How,CPU]).

invoke_final_answer(Logic,How,CPU):-
        writeFmt('<requestResponse yesno="~w" numBindings="~w" seconds="~w">\n<bindings>\n',[Logic,How,CPU]),
        cite_xml_buffered_answers,
        write('</bindings>\n</requestResponse>\n').


cite_xml_buffered_answers:-
        retract(requestBuffer_db(UResultsSoFar,Result,Explaination,Status)),
        once(inform_xml_agent(UResultsSoFar,Result,Explaination,Status)),fail.

% Call to write Summary
/*
cite_xml_buffered_answers:-
        final_answer(Logic:How),
        debugFmt(final_answer(Logic:How)),
        inform_xml_agent(How, ['Summary'=Logic|_G14093],final_answer(Logic:How),final_answer(Logic:How) ).
*/
cite_xml_buffered_answers:-!.

:-dynamic_transparent(cycassertgaffast/4).

cycassertgaffast(P,X,Y,Mt):-
   termCyclify(P,CP),
   termCyclify(X,CX),
   termCyclify(Y,CY),
   termCyclify(Mt,CMT),
   evalSubL('cyc-assert'(quote([CP,CX,CY]),quote(CMT)),_),!.
      

removalPredicateCodedInProlog(Pred,Prolog).

prologEval(X,Y):-Y is X + 111.
prologEval(X,Y):-Y is X + 211.

:-use_module(library(odbc)).  
:-odbc_debug(5).


:-dynamic(ensureCycCallsProlog/2).

ensureCycCallsProlog(Host,Port):-withoutCyc,!.
ensureCycCallsProlog(Host,Port):-
   sformat(SFormat,'(progn (define callprologpred (outval) (clet ((*retval* nil)(*stream* (OPEN-TCP-STREAM *prolog-host* *prolog-port*)))
   (prin1 outval *stream*)(force-output *stream*)(terpri *stream*)(force-output *stream*)(csetq *retval* (read *stream*))
   (close *stream*)(ret *retval*))) (defvar *prolog-host* "~w")(csetq *prolog-host* "~w")(defvar *prolog-port* ~w)(csetq *prolog-port* ~w))',[Host,Host,Port,Port]),
   evalSubL(SFormat,_,_),

   asserta((ensureCycCallsProlog(_,Port):-!)),!.
      
                                    
%odbc_current_table(CON, Table, Facet).


numberArgs(_,[]):-!.
numberArgs(N,[A|Args]):-ignore(N=A),NN is N+1,numberArgs(NN,Args),!.


cycPredCall([Pred, neg, Proc], Stuff, Stuff):-ground(Stuff),!,cycPredCall([Pred, pos, Proc], Stuff,R),!,R=[].
cycPredCall([Pred, pos, proc], [P|Args],[[P|Args]]):-!,numberArgs(0,Args),!.
 
cycPredCall(X,Y,[]):-attach_console,true,debugFmt(cycPredCall(X,Y)),!.
cycPredCall([Prolog,pos|_],[CycPred|CallArgs],Result):-is_list(CallArgs),Call=..[Prolog|CallArgs],!,findall([CycPred|CallArgs],catch(Call,_,fail),Result).
cycPredCall([Prolog,neg|_],[CycPred|CallArgs],[]):-!.
cycPredCall(Predstring,Call,[Call]):-!.

prologProcForCycPred(X,X).
  

dsnForDB('AdventureWorks','AdventureWorks').
connectionForDB(DB,CON):-odbc_current_connection(CON,database_name(DB)),!.
connectionForDB(DB,CON):-dsnForDB(DB,DSN),odbc_connect(DSN, CON,[ /*user('Administrator'),%password(Password),%alias(wordnet),*/open(once)]),!.

databaseForSource('AdventureWorksSource','AdventureWorks').
tableForSource('AdventureWorksSource',addressInfo,'Person.Address').
predicatesOfSource('AdventureWorksSource',addressInfo).

selectAll(Predicate,ROWS):-
   predicatesOfSource(Source,Predicate),
   tableForSource(Source,Predicate,Table),
   databaseForSource(Source,DB),
   connectionForDB(DB,CON),
   queryOfPredicate(Predicate,Query),
   odbc_query(CON,Query,ROWS).

queryOfPredicate(addressInfo,'SELECT * FROM Person.Address').

%rowInfo


insert_child(Child, Mother, Father, Affected) :- 
        odbc_query(parents,'INSERT INTO parents (name,mother,father) VALUES ("mary", "christine", "bob")',affected(Affected)).


% defineRemovalPred(prologEval,prologEval). 
defineRemovalPred(Predname,Prologname):-
   evalSubL('find-or-create-constant'(string(Predname)),_),
   termCyclify(Predname,CPredname),
   cycassertgaffast(isa,CPredname,'VariableArityRelation','UniversalVocabularyMt'),!,
   cycassertgaffast(comment,CPredname,string(['Defined via the defineRemovalPred',CPredname,'->',Prologname]),'UniversalVocabularyMt'),
   cycassertgaffast(isa,CPredname,'RemovalModuleSupportedPredicate-Specific','CycAPIMt'),
  % cycassertgaffast(arity,CPredname,2,'UniversalVocabularyMt'),!,
   atom_concat(':removal-',Predname,RemovalPrefix),
   atom_concat(RemovalPrefix,'-pos',RemovalPos),
   atom_concat(RemovalPrefix,'-neg',RemovalNeg),
   atom_concat(Prologname,'-pos-proc',RemovalPosProc),
   atom_concat(Prologname,'-neg-proc',RemovalNegProc),
   evalSubL('inference-removal-module'(RemovalPos,quote(
   [':sense',':pos',':predicate',% ':module-subtype' , ':kb',          
   CPredname,':cost-expression',0,':completeness',':complete',':input-verify-pattern',':anything',
   ':output-generate-pattern',[':call',RemovalPosProc,':input']])),_),
   evalSubL('inference-removal-module'(RemovalNeg,quote(
   [':sense',':neg',':predicate',     %  ':module-subtype' , ':kb',
   CPredname,':cost-expression',0,':completeness',':complete',':input-verify-pattern',':anything',
   ':output-generate-pattern',[':call',RemovalNegProc,':input']])),_),
   evalSubL('register-solely-specific-removal-module-predicate'(CPredname),_),!,
   evalSubL('define'(RemovalPosProc,['value'],ret(callprologpred(list(string(RemovalPosProc),'value')))),_),!,
   evalSubL('define'(RemovalNegProc,['value'],ret(callprologpred(list(string(RemovalNegProc),'value')))),_),!.


  /*
  
(define callprologpred (values) (print values) (ret NIL))
(define callprologpred (values) (print values) (ret (list '(1 2))))



(inference-removal-module :removal-gameNear-unbound-unbound
 '(:sense :pos 
	:predicate #$doom:gameNear 
   	:cost-expression 0 :completeness :complete 
	:input-extract-pattern (:template (#$doom:gameNear (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
	:input-verify-pattern :anything
	:output-generate-pattern (:call removal-gameNear-pos-uu :input)
	:output-construct-pattern  (#$doom:gameNear (:call first :input) (:call second :input))))
(inference-removal-module :removal-gameNear-bound-bound 
'( :sense :pos 
	:predicate #$doom:gameNear 
	:check t 
	:required-pattern (#$doom:gameNear :fully-bound :fully-bound)
	:cost-expression 0 :completeness :complete
	:input-extract-pattern (:template (#$doom:gameNear (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
	:input-verify-pattern :anything
	:output-check-pattern (:call removal-gameNear-pos-bb (:tuple (value-1 value-2) ((:value value-1) (:value value-2))))))


(define removal-gameNear-pos-bu (value) (clet ((*newvalue* value)) (csetq *newvalue* (GAME-EVAL (list "gameNear-pbu" value))) (ret  *newvalue* )))
(define removal-gameNear-pos-ub (value) (clet ((*newvalue* value)) (csetq *newvalue* (GAME-EVAL (list "gameNear-pub" value))) (ret  *newvalue* )))
(define removal-gameNear-pos-bb (vvs) (clet ((*newvalue* vvs)) (csetq *newvalue* (GAME-EVAL (list "gameNear-pbb" vvs))) (ret  *newvalue* )))
(define removal-gameNear-pos-uu (vvs) (clet ((*newvalue* vvs)) (csetq *newvalue* (GAME-EVAL (list "gameNear-puu" vvs))) (ret  *newvalue* )))
(register-solely-specific-removal-module-predicate #$doom:gameNear)
                                     */
    /*
(tableName TABLE STRING) 
isSKSI(prologSKSIConnect,'TheList'(
prologSKSIConnect(DataSrc,Result):-
       cycQueryOnce(tableName(DataSrc,TablName),
     cycQueryOnce(usernameForAccount(DataSrc,Username)),
     cycQueryOnce(passwordForAccount(DataSrc,Password)),
                */


% ===========================================================
% Send to debugger
% ===========================================================
inform_xml_agent(UResultsSoFar,Result,InExplaination,Status):-
        debugFmt(inform_xml_agent(UResultsSoFar,Result,InExplaination,Status)),fail.

% ===========================================================
% Hide certain returns
% ===========================================================
inform_xml_agent(-1,Result,Explaination,Status).

inform_xml_agent(0, ['Result'=none|A], 'Unproven', done(possible:searchfailed)).
inform_xml_agent(_, ['Result'=true|A], found(_), done(true:_)).
inform_xml_agent(_, ['Summary'=_|_G5892], _, _).

% ===========================================================
% Write Answers
% ===========================================================
:-dynamic_transparent(length_explaination/2).

inform_xml_agent(UResultsSoFar,Result,InExplaination,Status):-
        writeFmt('<binding>\n',[]),
        inform_xml_vars(Result,Vars),
        length_explaination(InExplaination,InLength),
        findall(Length-Explaination,
                (retract(inform_xml_agent_buffer_db(_,Result,Explaination,_)),
                 length_explaination(Explaination,Length)
                 ),KeyList),
        keysort([(InLength-InExplaination)|KeyList],[(_-ChoiceExplaination)|_]),
        inform_xml_explaination(InLength,ChoiceExplaination,Result),
        writeFmt('</binding>\n',[]).

inform_xml_vars(Result,Vars):-
        length_var(Result,NumVar),
        writeFmt('<variables numVars="~w">\n',[NumVar]),
        inform_each_variable(Result,Vars),
        writeFmt('</variables>\n',[]).

length_var([],0).
length_var([A|'$VAR'(_)],1).
length_var([A|L],N):-
          length_var(L,NN),
          N is NN +1.

inform_each_variable([],Vars).
inform_each_variable('$VAR'(_),Vars).
inform_each_variable([NV|Rest],Vars):-
        inform_nv(NV,Vars),
        inform_each_variable(Rest,Vars).


inform_nv('$VAR'(_),Vars).
inform_nv(Name=Value,Vars):-
        toMarkUp(cycl,Name,Vars,OName),
        toMarkUp(cycl,Value,Vars,OValue),
        writeFmt('<var varName="~w" value="~w"/>\n',[OName,OValue]).


inform_xml_explaination(InLength,ChoiceExplaination,Result):-
        writeFmt('<explaination numSteps="~w">',[InLength]),
        flag(explaination_linenumber,_,0),
        writeObject_explaination(ChoiceExplaination,Result),
        writeFmt('</explaination>\n').

writeObject_explaination(deduced,_).
writeObject_explaination('$VAR'(_),_).
writeObject_explaination(explaination(Choice1) ,Result):-!,
        writeObject_explaination(Choice1,Result),!.
writeObject_explaination(Choice1 * Choice2 ,Result):-!,
        writeObject_explaination(Choice1,Result), !,
        writeObject_explaination(Choice2,Result),!.
writeObject_explaination(Choice1,Result):-!,
             write('<explainationStep isRule="true">\n<originalRule>\n'),
             toMarkUp(html,Choice1,Result,Out),!,
             ignore(write_escaped(Out)),
             write('\n</originalRule>\n</explainationStep>\n').

write_escaped([O|T]):-!,
        write_e_codes([O|T]),!.
write_escaped(Out):-atom(Out),!,
        atom_codes(Out,Codes),!,write_escaped(Codes),!.
write_escaped(String):- !,
        string_to_atom(String,Atom),
         atom_codes(Atom,Codes),!,
        write_e_codes(Codes),!.

write_e_codes([]):-!.
write_e_codes([E|Cs]):-!,
        write_e(E),!,
        write_e_codes(Cs),!.
write_e(34):-write('&amp;qt;'),!.
write_e(60):-write('&amp;lt;'),!.
write_e(62):-write('&amp;gt;'),!.
write_e(C):-put_code(C),!.


% ===================================================================
% writeIfOption(class(input),message(input),respoinse(output))
% generic call interface that was hooked into the belief engine with "ua_set_agent_callback(console_post)"
%This is not a predicate the useragent calls, but one that is called by the belief module to communicate  a question to the useragent or inform it of something.  
% The useragent decides if it can answer the a question and if not itself may ask a human user that is using it.
% There is three arguments to the my_callback predicate: Class, Message and Response
%
% Whenever the belief engine calls 'my_callback' only the first two arguments (Class,Message) are bound to supply information relevant to a Server invoked request.
%
% Class is a programmer defined message catagory  
% The Class is inteded to contain user defined message names that are sent as a callback function that is sent to the user's module consultation 
% Is is the type of Message catagory for the user agent.. A list of these are in TABLE 1.1 in <http://10.10.10.198/cyc_interface_advanced.html>
% (Class is always a ground Term)
%
% Message is a prolog term in the writeFmt defined by it's Class
% Each Class has a one known Message writeFmt shown in the table.   
% Message sometimes is ground term. 
%
%
% Response has normally has 2 response single_bindings: continue or abort
% This response is sent back to the belief_engine.
% If the belief_engine didn''t receive 'abort', then it moves to the next stage in the command.
% 
% ===================================================================

			  /*      				   
:-module(cyc_generation,
	 [ 
	 debugFmt/1,
	 debugFmt/2,
	 debugFmtFast/1,
	 logOnFailureIgnore/1,
	 setCycOptionExplicitWriteSettings/0,
	 setCycOptionImplicitWriteSettings/0,
	 sendNote/1,
	 sendNote/4,
	 writeFailureLog/2,
	 debugOnFailure/2,
	 writeObject/2,
	 writeObject/3,
	 writeObject_conj/2]).
					 */

% :-include('cyc_header.pl').

% :-use_module(cyc_globalisms).

% ==========================================================
%  Sending Notes
% ==========================================================
 

logOnFailureIgnore(X):-ignore(logOnFailure(X)),!.

writeModePush(_Push):-!.
writeModePop(_Pop):-!.

%debugFmt([-1]).
%debugFmt([[-1]]).
%debugFmt(T):- isCycOption(opt_debug=off),!.
debugFmt(Stuff):-!,debugFmt('% ~q~n',[Stuff]).
debugFmt(T):-!,
	((
	if_prolog(swi,
		(prolog_current_frame(Frame),
		prolog_frame_attribute(Frame,level,Depth),!,
		Depth2 = (Depth-25))),
	writeFmt(';;',[T]),!,
	indent_e(Depth2),!,
	writeFmt('~q\n',[T]))),!.

indent_e(X):- catch((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).

%debugFmt(C,T):- isCycOption(opt_debug=off),!.
debugFmt(_,F):-F==[-1];F==[[-1]].
debugFmt(F,A):-
        nl(user_error),
        writeFmtFlushed(user_error,F,A),
        nl(user_error),
        flush_output_safe(user_error),!.

debugFmt(C,T):-!,
	((
	writeFmt('<font size=+1 color=~w>',[C]),
	debugFmt(T),
        writeFmt('</font>',[]))),!.

dumpstack_argument(T):-isCycOption(opt_debug=off),!.  
	
dumpstack_argument(Frame):-
	write(frame=Frame),write(' '),
	dumpstack_argument(1,Frame).

dumpstack_argument(1,Frame):-!,
	prolog_frame_attribute(Frame,goal,Goal),!,
	write(goal=Goal),write('\n').
	
dumpstack_argument(N,Frame):-
	prolog_frame_attribute(Frame,argument(N),O),!,
	write(N=O),write(' '),
	NN is N +1,
	dumpstack_argument(NN,Frame).
	
dumpstack_argument(N,Frame):-!,write('\n').
	
:-dynamic_transparent(mods/1).

write_response_begin:-!.
write_response_end:-!.

sendNote(X):-var(X),!.
sendNote(X):-mods(X),!.
sendNote(X):-!,assert(mods(X)).
sendNote(X).			 

sendNote(To,From,Subj,Message):-sendNote(To,From,Subj,Message,_).

sendNote(To,From,Subj,Message,Vars):-
	not(not((safe_numbervars((To,From,Subj,Message,Vars)),
	%debugFmt(sendNote(To,From,Subj,Message,Vars)),
	catch(sendNote_1(To,From,Subj,Message,Vars),E,
	writeFmt('send note ~w ~w \n <HR>',[E,sendNote(To,From,Subj,Message,Vars)]))))).


sendNote_1(To,From,Subj,surf,Vars):-!.
sendNote_1(To,From,[],surf,Vars):-!.
sendNote_1(To,From,[],end_of_file,Vars):-!.
sendNote_1(doug,From,_,_,Vars):-!.
sendNote_1(extreme_debug,From,_,_,Vars):-!.
sendNote_1(debug,'Belief',_,_,Vars):-!.

%sendNote_1(canonicalizer,From,Subj,Message,Vars):-!.


sendNote_1(canonicalizer,From,Subj,Message,Vars):-
            toMarkUp(cycl,From,Vars,SFrom),
            toMarkUp(cycl,nv(Subj),Vars,SS),
            toMarkUp(cycl,nv(Message),Vars,SA),
            writeFmt('<font color=red>canonicalizer</font>: ~w "~w" (from ~w). \n',[SA,SS,SFrom]),!.

/*

sendNote_1(debug,From,Subj,Message,Vars):- %isCycOption(disp_notes_nonuser=on),!,
            toMarkUp(cycl,From,Vars,SFrom),
            toMarkUp(cycl,Subj,Vars,SS),
            toMarkUp(cycl,Message,Vars,SA),
            writeFmt('% debug: ~w "~w" (from ~w). \n',[SA,SS,SFrom]).
sendNote_1(debug,From,Subj,Message,Vars):-!.
*/

            /*


sendNote_1(To,From,Subj,Message,Vars):- isCycOption(client=consultation),  !, 
            toMarkUp(cycl,To,Vars,STo),
            toMarkUp(cycl,From,Vars,SFrom),
            toMarkUp(cycl,nv(Subj),Vars,S),
            toMarkUp(cycl,nv(Message),Vars,A),
            fmtString(Output,'~w (~w from ~w) ',[A,S,SFrom]),
	    sayn(Output),!.

sendNote_1(To,From,'Rejected',Message,Vars):- isCycOption(client=automata),  !.

sendNote_1(To,From,Subj,Message,Vars):- isCycOption(client=automata),  !, 
            toMarkUp(cycl,To,Vars,STo),
            toMarkUp(cycl,From,Vars,SFrom),
            toMarkUp(cycl,nv(Subj),Vars,S),
            toMarkUp(cycl,nv(Message),Vars,A),
            writeFmt(user_error,'% ~w (~w from ~w) ',[A,S,SFrom]).

sendNote_1(To,From,Subj,Message,Vars):- isCycOption(client=html),  !, %  In Html
            toMarkUp(cycl,To,Vars,STo),
            toMarkUp(cycl,From,Vars,SFrom),
            toMarkUp(cycl,nv(Subj),Vars,S),
            toMarkUp(html,nv(Message),Vars,A),
            writeFmt('<hr><B>To=<font color=green>~w</font> From=<font color=green>~w</font> Subj=<font color=green>~w</font></B><BR>~w\n',[To,From,S,A]),!.

sendNote_1(To,From,Subj,Message,Vars):- isCycOption(client=console),!, % In CYC
            toMarkUp(cycl,To,Vars,STo),
            toMarkUp(cycl,From,Vars,SFrom),
            toMarkUp(cycl,nv(Subj),Vars,SS),
            toMarkUp(cycl,nv(Message),Vars,SA),
            writeFmt(user_error,'; ~w: ~w "~w" (from ~w). \n',[STo,SA,SS,SFrom]),!.
  
sendNote_1(To,From,Subj,Message,Vars):-  % In CYC
            toMarkUp(cycl,To,Vars,STo),
            toMarkUp(cycl,From,Vars,SFrom),
            toMarkUp(cycl,nv(Subj),Vars,SS),
            toMarkUp(cycl,nv(Message),Vars,SA),
            writeFmt(user_error,'; ~w: ~w "~w" (from ~w). \n',[STo,SA,SS,SFrom]),!.

sendNote(To,From,Subj,Message,Vars):-!.
                                                                       */
debugFmtFast(X):-writeq(X),nl.

logOnFailure(assert(X,Y)):- catch(assert(X,Y),_,Y=0),!.
logOnFailure(assert(X)):- catch(assert(X),_,true),!.
logOnFailure(assert(X)):- catch(assert(X),_,true),!.
%logOnFailure(X):-catch(X,E,true),!.
logOnFailure(X):-catch(X,E,(writeFailureLog(E,X),!,catch((true,X),_,fail))),!.
logOnFailure(X):- writeFailureLog('Predicate Failed',X),!.


flush_output_safe(X):-catch(flush_output(X),_,true),!.
flush_output_safe(X).

writeFailureLog(E,X):-
		writeFmt(user_error,'\n% error:  ~q ~q\n',[E,X]),flush_output_safe(user_error),!,
		%,true.
		writeFmt('\n;; error:  ~q ~q\n',[E,X]),!,flush_output. %,writeFmtFlushed([E,X]).
		
debugOnFailure(assert(X,Y)):- catch(assert(X,Y),_,Y=0),!.
debugOnFailure(assert(X)):- catch(assert(X),_,true),!.
debugOnFailure(assert(X)):- catch(assert(X),_,true),!.
debugOnFailure(X):-catch(X,E,(writeFailureLog(E,X),fail)).
debugOnFailure(X):-ctrace,call(X).

debugOnFailure(arg_domains,CALL):-!,logOnFailure(CALL),!.
debugOnFailure(Module,CALL):-debugOnFailure(Module:CALL),!.


debugOnError((X,Y)):-!,debugOnError(X),debugOnError(Y).
debugOnError((X;Y)):-!,(debugOnError(X);debugOnError(Y)).
debugOnError(call(X)):-!,debugOnError(X).
debugOnError(X):-catch(X,E,(writeFailureLog(E,X),ctrace,call(X))).


noDebug(CALL):-CALL.
	


%unknown(Old, autoload).

% ================================================================
%   Serialize Objects to XML
% ================================================================


%%writeObject(OBJ,Vars):-!. %,writeq(OBJ),!.
%writeObject(OBJ,Vars):-!,catch(writeq(OBJ),_,true),nl,!.

writeObject(quiet,Term,Vars):-!.

writeObject(Verbose,Term,Vars):-writeObject(Term,Vars).

		
writeObject(OBJ,Vars):- isCycOption(client=html),!,
		((toMarkUp(html,OBJ,Vars,Chars),write(Chars))),!.
		
writeObject(OBJ,Vars):- isCycOption(client=atomata),!,
		((toMarkUp(cycl,OBJ,Vars,Chars),write(Chars))),!.

writeObject(OBJ,Vars):- isCycOption(client=console),!,
		((toMarkUp(cycl,OBJ,Vars,Chars),write(Chars))),!.

writeObject(OBJ,Vars):- !,
		((toMarkUp(cycl,OBJ,Vars,Chars),write(Chars))),!.


writeObject_conj(A,Vars):-isSlot(A),!,
	writeObject(A,Vars).

writeObject_conj(and(A,true),Vars):-!,
	writeObject_conj(A,Vars).

writeObject_conj(and(true,A),Vars):-!,
	writeObject_conj(A,Vars).

writeObject_conj(and(A,B),Vars):-!,
	writeObject_conj(A,Vars),
	writeObject_conj('\n\n Also \n\n ',Vars),
	writeObject_conj(B,Vars).

writeObject_conj(Output,Vars):-
	%write(Output),nl.
	writeObject(Output,Vars).


:-dynamic_transparent(resolve_skolem/2).
:-dynamic_transparent(final_answer/1).

ignoreOnce(X):-ignore(once(X)).

writeIfOption(C,P):-ignoreOnce(writeCycEvent(C,P,_)).
writeIfOption(C,M,Vars):-ignoreOnce(writeCycEvent(C,M,Vars)).


write_val(Any,Vars):- isCycOption(client=html)
      -> write_val_xml(Any,Vars) ;
      (toMarkUp(cycl,Any,Vars,Chars),write(Chars),nl).
%      write_sterm(Any,Vars).
      
write_val_xml(Any,Vars):-
      toMarkUp(leml,Any,Vars,Chars),write(Chars),nl.
                                        /*

:-dynamic_transparent(telling_file).               

writeCycEvent(_,_,_):-isCycOption(disp_hide_all=true),!.
writeCycEvent(_,_,_):-telling_file,!.
writeCycEvent(Class,_,_):-isCycOption(Class=false),!.
writeCycEvent(Class,_,_):-isCycOption(disp_explicit=true),not(isCycOption(_Class=true)),!.

writeCycEvent(request_start,Note,Vars):-!,
         (isCycOption(client=html) -> 
          (writeFmt('<Answer>\n'),le_push('Answer'));
          true).

writeCycEvent(request_end,(Result,Normal,Elapsed,Num,Bindings),Vars):-!, 
                  (isCycOption(client=html) -> 
                     ((    
                       (toMarkUp(leml,note('user',logicEngine,Result,(Result,Normal,Elapsed,Num,Bindings)),Vars,Chars),once((var(Chars);write(Chars)))),
                       writeFmt('<Summary result="~w" solutions="~d" bindings="~d" cpu="~f"/>\n</Answer>\n',[Result,Num,Bindings,Elapsed]),
                       le_pull('Answer')
                     ));
                       writeFmt('\n%%  ~w solutions="~d" bindings="~d" cpu="~f"\n',[Result,Num,Bindings,Elapsed])).

writeCycEvent(Class,Message,Vars):-not(isCycOption(client=html)),!, toMarkUp(cycl,[Class,Message],Vars,Chars),write(Chars),nl.
writeCycEvent(Class,Message,Vars):-isCycOption(client=html),!, event_to_chars(leml,Class,_Message,Vars,Chars),write(Chars),nl.
writeCycEvent(cb_consultation, assertion([PredicateI|ConsultTemplate],_Context_atom,_SN), continue):- 
               agentConsultation(_Context_atom,[PredicateI|ConsultTemplate], _ListOfGafsAsserted).
writeCycEvent(_,_,_):-!.

                                                         */
/*
Where the parameters are some string syntax or other straightforward data
structure and we've used I to signify a parameter that is used by the
function and O to signify a parameter that is returned by the
function.  If that were forall it had, we think that is sufficient for
the bulk of interactions.  Everything else is helpful but not strictly
essential.  Because of that, we believe that it is possible to run
our system with just the above commands after startup.

   We have shown a number of features implemented such as

  - explaination trees
  - belief execution time and search controls
  - compilation
  - consultation mode

The expanded API is
*/          
%=================================================================
%  CONSULTATION MANAGEMENT DIRECTIVES
%=================================================================

/*
where the xxxNative versions take the disp_modification WFSform and the other
versions take STANDARD.  Consultation mode has a cycl default interface too:
*/



/* ; where the list is of arguments
missing that is requested from the user.  The default is to ask for
any and forall arguments that are missing

%TODO

ua_consultationModeEvery() ; ask the user for as many inputs as he's willing

to give
etc. ; other modes...

A further expansion to handle communication with a user agent external to
Prolog would add a message sent to a socket that process is listening to.
and a message string sent from Prolog to the user agent to request user input

"userInputRequest predicateName<cr>"

Where <cr> indicates a carriage return or some other suitable delimiter.

*/


% User Agent
:-dynamic_transparent('$CycOption'/3).
:-dynamic_transparent(saved_note/4).
:-dynamic_transparent(act_mem/3).


% ===========================================================
% THREAD SERVICE
% ===========================================================

% imports these models from SWI-Prolog
% thread_create(Goal,Id,Options)
% current_thread(Id,Status)
% thread_at_exit(Goal)
% thread_self(Id)
% thread_at_exit(Id,Goal)
% thread_join(Id,_)

/*
:-module(cyc_threads,
      [ 
      	 servantProcessCreate/1,
	 servantProcessCreate/3,
	 servantProcessCreate/4,
	 servantProcessCreate/5,
	 isCycProcess/2,
	 isCycProcess/5,
	 createProcessedGoal/1,
	 servantProcessSelfClean/0,
	 showCycStatisticsHTML/0,
	 cleanOldProcesses/0,
	 showCycProcessHTML/0]).
  */
% :-include('cyc_header.pl').

:-dynamic_transparent(isCycProcess/5).


createProcessedGoal(Goal):-
      servantProcessCreate((thread_at_exit((
	 (thread_self(Id),thread_exit(i_am_done(Id))))),Goal),Id,[]).


servantProcessCreate(Perms,Name,Goal,Id,Options):-
        thread_create((thread_at_exit(servantProcessSelfClean),Goal),Id,Options),
        asserta(isCycProcess(Perms,Name,Goal,Id,Options)).

servantProcessCreate(Name,Goal,Id,Options):-
        thread_create((thread_at_exit(servantProcessSelfClean),Goal),Id,Options),
        asserta(isCycProcess(killable,Name,Goal,Id,Options)).

servantProcessCreate(Goal,Id,Options):-
        thread_create((thread_at_exit(servantProcessSelfClean),Goal),Id,Options),
        asserta(isCycProcess(killable,thread(Id),Goal,Id,Options)).

servantProcessCreate(Goal):-servantProcessCreate(Goal,_Id,[]),!. %%global(2800),local(2800),trail(2800)

isCycProcess(ID,Goal):-
        isCycProcess(_,_,Goal,ID,_).

debugProcess(T):-
	thread_signal(T, (attach_console, true)).


servantProcessSelfClean:-
      thread_self(Id),
      retractall(isCycProcess(_Perms,_Name,_Goal,Id,_Options)).




showCycStatisticsHTML:-
   writeFmt('<pre>'),prolog_statistics,
   threads,
   writeFmt('<br>'),
   writeFmt('</pre>').

showCycProcessHTML:-
        once(showCycStatisticsHTML),
        writeFmt('<hr><table border=1 width=80%><th>Id</th><th>Name</th><th>Status</th><th>Actions</th><th>Options</th><th>Goals</th>',[]),
        ignore((current_thread(Id,Status),
        isCycProcess(Perms,Name,Goal,Id,Options),
        once(writeCycProcessesHTML(Perms,Name,Goal,Id,Options,Status)),
        fail)),writeFmt('</table>',[]),!.


writeCycProcessesHTML(nokill,Name,Goal,Id,Options,Status):-
        writeFmt('<tr><td>~w</td><td><nobr>~w</td><td>~w</td><td>nokill</a></td><td>~w</td><td>~w</td><tr>\n ',[Id,Name,Status,Options,Goal]),!.

writeCycProcessesHTML(Perms,Name,Goal,Id,Options,Status):-
        writeFmt('<tr><td>~w</td><td><nobr>~w</td><td>~w</td><td><A href="controlpanel.jsp?killable=~w">Kill</a></td><td>~w</td><td>~w</td><tr>\n ',[Id,Name,Status,Id,Options,Goal]),!.

cleanOldProcesses:-
        saveUserInput,
        current_thread(Id,Status),
        handleProcessStatus(Id,Status),fail.
cleanOldProcesses:-writeSavedPrompt,!.
cleanOldProcesses:-!.

handleProcessStatus(Id,running):-!. %Normal
handleProcessStatus(Id,exited(complete)):-!,thread_join(Id,_),!.
handleProcessStatus(Id,true):-!, debugFmt('% Process ~w complete.\n',[Id]),!,thread_join(Id,_),!.
handleProcessStatus(Id,exception(Error)):-!, debugFmt('% Process ~w exited with exceptions: ~q \n',[Id,Error]),!. %,thread_join(Id,_),!.
handleProcessStatus(Id,O):-!, debugFmt('% Process ~w exited "~q". \n',[Id,O]),!,thread_join(Id,_),!.

mutex_call(Goal,Id):-
                        mutex_create(Id),
                        mutex_lock(Id),!,
                        with_mutex(Id,Goal),!,
                        mutex_unlock_all.



%:-defaultAssertMt(Mt),!,ensureMt(Mt),cycAssert('BaseKB':'#$genlMt'(Mt,'InferencePSC')). % Puts the defaultAssertMt/1 into Cyc 
%:-defaultAssertMt(Mt),ensureMt(Mt).




% ===========================================================
% HTML
% ===========================================================

writeHTMLStdHeader(Title):-
   writeFmtFlushed('
   <html>
   <head>
   <meta http-equiv="Content-Language" content="en-us">
   <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
   <meta name="Keywords" content="PROLOG Artificial Intelligence Ontology AI MOO DARPA Douglas Miles">
   <meta name="Description" content="PROLOG Artificial Intelligence Ontology AI DARPA MOO">
   <title>MOO Engine - ~w</title>
   </head>
   <body>
          <a href="browse.moo">Browse</a> 
	    <a href="english.moo">English</a>
	    <a href="cycl.moo">CycL</a>
	    <a href="subl.moo">SubLisp</a>
	    <a href="daml.moo">Daml</a>
	    <a href="wn.moo">WordNet</a>
	    <a href="cycml.moo">CycML</a>
	    <a href="prolog.moo">Prolog</a>
	    <a href="settings.moo">Settings</a>
	    <a href="system.moo">System</a>
	    <a href="help.moo">Help</a>
	  <br><font size=+1>Create/Edit</font>
	   <a href="predicate.moo">Predicate</a>
	   <a href="function.moo">Function</a>
	   <a href="collection.moo">Collection</a>
	   <a href="microtheory.moo">Microtheory</a>
	  <font size=+1>Tests</font>
	   <a href="inference_tests.moo">Inference</a><br>
       <br><font size=+1 color=green><b>~w</b></font><br>
   ',[Title,Title]).

writeHTMLStdFooter:-
   writeFmtFlushed('
   </body>
   </html>',[X]).



 
clauseForVirtual(Predicate/Arity,Head):-functor(Head,Predicate,Arity),!.
clauseForVirtual(Predicate,Predicate).
clauseForVirtual(Predicate,Call):-not(atom(Predicate)),!.
clauseForVirtual(Term,asserted(Data,Mt,Vars,List)):-asserted(Data,Mt,Vars,List),once(memberchk(Term,List)).
clauseForVirtual(Predicate,Call):-Call=..[Predicate,_].
clauseForVirtual(Predicate,Call):-Call=..[Predicate,_,_].
clauseForVirtual(Predicate,Call):-Call=..[Predicate,_,_,_].
clauseForVirtual(Predicate,Call):-Call=..[Predicate,_,_,_,_].

getClauseDB(saved(Data,Mt,Vars,List),saved(Data,Mt,Vars,List)):-!.
getClauseDB(Head,imported_from(X,Head)):-predicate_property(Head,imported_from(X)).
getClauseDB(Head,implimentedInCode(Head)):-predicate_property(Head,built_in),!.
getClauseDB(Head,implimentedInCode(Head)):-predicate_property(Head,foreign),!.
getClauseDB(Head,entails(VirtualClauses,Head)):-clause(Head,VirtualClauses).


htmlListing(Predicate):-
      writeFmtFlushed('<pre>'),
      clauseForVirtual(Predicate,Head),
      getClauseDB(Head,VirtualClauses),
      writeHtml(VirtualClauses),fail.

htmlListing(Predicate):-
      make,
      writeFmtFlushed('~nEnd of Clauses with ~w</pre>',[Predicate]),!.

writeHtml(linkEach([])):-!.
writeHtml(linkEach([H|T])):-writeHtml(linkFor(H)),
      writeHtml(nl),writeHtml(linkEach(T)).
writeHtml(nl):-format('<br>').

writeHtml(linkFor(H)):-
	 my_www_form_encode(H,E),
	 writeFmtFlushed('<A href="browse.moo?find=~w">~w</A>',[E,H]).
%writeHtml((H:-T)):-!,writeHtml(prologEntails(T,H)).      
writeHtml(Clauses):-
        flag(indent,_,0),
      numbervars(Clauses,0,_),%true,
      toCycApiExpression(html(Clauses),[],O),!,
      writeFmtFlushed('~w~n',[O]).

my_www_form_encode(X,Y):-www_form_encode(X,Y).


writeHyperLink(NameFmt,NameArgs,UrlFmt,UlrArgs):-
      writeFmtFlushed('<a href="'),writeFmtFlushed(UrlFmt,UlrArgs),
      writeFmtFlushed('">'),writeFmtFlushed(NameFmt,NameArgs),writeFmtFlushed('</a>'),!.

writeCheckbox(Name,Text,Default):-
      getCycOption(Name=Default,Value),
      valueToCheckMark(Value,OnOff,More),
      writeFmtFlushed('<label for="~w"><input id="~w" type="checkbox" name="~w" value="~w" ~w>~w</label>',[Name,Name,Name,OnOff,More,Text]).

writeSpaces(N):-not(number(N)),!.
writeSpaces(N):-N<1,!.
writeSpaces(N):-format('&nbsp;'),NN is N-1,writeSpaces(NN),!.


valueToCheckMark(Value,'ON','CHECKED'):-memberchk(Value,['ON',yes,true,'TT','T','True','Yes']).
valueToCheckMark(Value,'OFF',' ').

:-set_prolog_flag(double_quotes,string).


%% ======================================
%% lisp_reade_term(Lisp)
%% ======================================
% :-set_prolog_flag(double_quotes,codes).


:-retract(double_quotes_was(X)),set_prolog_flag(double_quotes,X).


% ===================================================================
% Lowlevel readCycLTermChars
% ===================================================================
%%getSurfaceFromTokens(GET,TERM,VARS):-!.

%getSurfaceFromTokens(GET,SURF,VARS):-debugOnFailure(s2p(surfFromTokens,GET,SURF,VARS)),!.
getSurfaceFromTokens(GET,token(GET),_VARS):-!.
