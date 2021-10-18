% ===================================================================
% File 'logicmoo_module_aiml_shared.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_shared.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

% :- use_module(library(logicmoo/util_strings)).
% is_string

%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
%:- if((current_prolog_flag(version,MMmmPP),MMmmPP<70000)).
%:- style_check(-atom).
%:- style_check(-string).
%:- endif.

:- current_prolog_flag(version,MMmmPP),
   (MMmmPP<70000 -> 
      consult(logicmoo_module_aiml_include_547); 
      consult(logicmoo_module_aiml_include_700)).

debugFmt(Stuff):- debugFmt('~N~n% ~q',[Stuff]),!.
debugFmt(F,A):- hide_complex_ctx(A,AA),!, once(lmdebugFmt(F,AA)).
debugFmt(F,A):- once(lmdebugFmt(F,A)).


hide_complex_ctx(I,O):- \+ compound(I),!,O=I.
hide_complex_ctx(I,O):- is_list(I),!,maplist(hide_complex_ctx,I,O).
hide_complex_ctx(I,'$..$'(A1)):- functor(I,F,_),F==frame,!,arg(1,I,A0),functor(A0,A1,_).
hide_complex_ctx(I,O):- I=..M,hide_complex_ctx(M,N),!,O=..N.

:- dynamic(noConsoleDebug/0).
noConsoleDebug.

lmdebugFmt(Stuff):- noConsoleDebug,Stuff \= say(_),!.
lmdebugFmt(Stuff):- notrace((fresh_line,debugFmtS(Stuff),fresh_line)),!.

lmdebugFmt(_,_):- noConsoleDebug,!.
lmdebugFmt(F,A):- 
        fresh_line(user_error),
        writeFmtFlushed(user_error,F,A),
        fresh_line(user_error),
        flush_output_safe(user_error),!.

debugFmtS(XXX):- XXX == [], !.
debugFmtS([A|L]):-!,debugFmt('% ~q',[[A|L]]).
debugFmtS(Comp):-toReadableObject(Comp,Comp2),!,debugFmt('% ~q',[Comp2]).
debugFmtS(Stuff):-!,debugFmt('% ~q',[Stuff]).


%================================================================
:-multifile(expire1Cache/0).
%================================================================

:- op(1100,xfy,(=>)).

% ( Antecedent => Consequent ) :-
%     \+ ( Antecedent,
%          \+ Consequent
%        ).
( Antecedent => Consequent ) :- forall(( Antecedent ),( Consequent )).


devmode:-fail.

atrace:- not(devmode),!.
atrace:- cyc:ctrace.
unused:atrace:-prolog_is_vetted_safe->notrace;(willTrace->trace;notrace).


f0rmt(A,B,C):-'format'(A,B,C).

%%% Modified version of <http://pastebin.com/GvmVQ1f1>
/*
%================================================================
prolog_trace_interception_pce(A, B, C, E) :- true,
%================================================================
    pce_prolog_tracer:
    (   current_prolog_flag(gui_tracer, true),
        (   notrace(intercept(A, B, C, D)),
            map_action(D, B, E)
        ->  true
        ;   print_message(warning, noguitracer(intercept_failed(A, B, C, E))),
            E = continue
        )
    ).


define_self_trace :-
    (   \+ clause(prolog_trace_interception(_, _, _, _),pce_prolog_tracer:_)
    ->  f0rmt(user_error, '~N % already defineSelfTrace~n',[]) % done already?
    ;   abolish(prolog_trace_interception,4),
        asserta(( prolog_trace_interception(A, B, C, E) :-
                      prolog_trace_interception_pce(A, B, C, E) ))
    ).

:- initialization define_self_trace.
*/  
%================================================================
%% print_stack_trace(+Stream,[+Option,...],+Depth)
% `Option' being one of
% `goal',`level',`context_module',`has_alternatives',`show_hidden'
%================================================================

print_stack_trace :-
    print_stack_trace(user_error
                     ,[goal,show_hidden,level
                      ,has_alternatives,alternative
                      ,hide(hmod:_),hide(_:hpred)]
                     ,10).

print_stack_trace(Stream,Options,Depth):-
    prolog_current_frame(Frame),
    print_stack_trace(Stream,Options,Depth,frame(Frame),1).

%%% Modified from older pastes
%% <http://pastebin.com/Tq7eQqDT>,<http://pastebin.com/fk5agLgE>
% print_stack_trace(Stream,Options,Depth,frame(Frame) :-
%     (   Depth = 0
%     ->  f0rmt(Stream,'~N<toodeep/>~n',[])
%     ;   print_stack_trace1(Stream,Options,Depth,frame(Frame)),
%         parent_frame_of(Frame,Parent),Depth2 is Depth -1,
%         print_stack_trace(Stream,Options,Depth2,Parent)
%     ).
%
% :- index(print_stack_trace1(0,0,0,1)).
% print_stack_trace1(Stream,Options,Depth,top         ) :-
%     f0rmt(Stream,'~N<top/>~n',[]).
% print_stack_trace1(Stream,Options,Depth,frame(Frame)) :-
%     f0rmt(Stream,'~N<frame id="~w">~n',[Frame]),
%     (   prolog_frame_attribute(Frame,hidden,true),
%         \+ memberchk(show_hidden,Options)
%     ->  parent_frame_of(Frame,Parent),
%         print_stack_trace(Stream,Options,Depth,Parent)
%     ;   (   member(Opt,Options),
%             \+ memberchk(Opt,[show_hidden,other_fake_properties]),
%         =>  prolog_frame_attribute(Frame,Opt,Value),
%             f0rmt(Stream,' ~w = ~q',[Opt,Value])
%         )
%     ),
%     f0rmt(Stream,'~N</frame>~n',[]).

print_stack_trace(Stream,Options,Depth,PFrame,PD) :-
    (   Depth = 0
    ->  pd(Stream,PD),f0rmt(Stream,'<toodeep/>~n',[])
    ;   print_stack_trace_aux(Stream,Options,Depth,PFrame,PD)
    ).

%:- if((current_prolog_flag(version,MMmmPP),MMmmPP<70000)).
%:- index(print_stack_trace_aux(0,0,0,1,0)).
%:- endif.
print_stack_trace_aux(Stream,_Options, _Depth,top         ,PD) :-
    pd(Stream,PD),f0rmt(Stream,'<top/>~n',[]).
print_stack_trace_aux(Stream,Options,Depth,frame(Frame),PD) :-
    (   (   prolog_frame_attribute(Frame,predicate_indicator,MFA),
            pred_mf(MFA,MF),
            memberchk(hide(MF),Options)
        ;   prolog_frame_attribute(Frame,hidden,true),
            \+ memberchk(show_hidden,Options)
        )
    ->  Depth2  = Depth
    ;   print_stack_frame(Stream,Options,Frame,PD),
        Depth2 is Depth - 1
    ),
    parent_frame_of(Frame,Parent),
    PD2 is PD + 2,
    print_stack_trace(Stream,Options,Depth2,Parent,PD2).

parent_frame_of(Frame,Parent0) :-
    (  prolog_frame_attribute(Frame,parent,Parent)
    -> Parent0 = frame(Parent)
    ;  Parent0 = top
    ).

pred_mf((M: F)/_A ,M   :F) :- !.
pred_mf( M:(F /_A),M   :F) :- !.
pred_mf(    F/ _A ,user:F).

print_stack_frame(Stream,Options,Frame,PD) :-
    pd(Stream,PD),f0rmt(Stream,'<frame id="~w">~n',[Frame]),
    (   member(Opt,Options), 
        \+ memberchk(Opt
                    ,[show_hidden,alternative,other_fake_properties,hide(_)])
    =>  prolog_frame_attribute(Frame,Opt,Value),
        pd(Stream,PD),f0rmt(Stream,' ~w = ~q',[Opt,Value])
    ),
    (   memberchk(alternative,Options)
    =>  delete(Options,alternative,Options2),
        prolog_frame_attribute(Frame,alternative,Alt),
        PD1 is PD + 1,PD3 is PD + 3,
        pd(Stream,PD1),f0rmt(Stream,'<alt>~n',[]),
            print_stack_frame(Stream,Options2,Alt,PD3),
        pd(Stream,PD1),f0rmt(Stream,'</alt>~n',[])
    ),
    pd(Stream,PD),f0rmt(Stream,'</frame>~n',[]).

pd(Stream,PD) :- f0rmt(Stream,'~N',[]),tab(Stream,PD + 1).

%================================================================
%% printStackTrace(+Stream,[+Options..,goal,level,context_module,has_alternatives,show_hidden],+Depth). 
%================================================================

printStackTrace:-printStackTrace(user_error).

printStackTrace(Stream):-printStackTrace(Stream,[goal,show_hidden,level,has_alternatives,alternative,hide(hmod:_),hide(_:printStackTrace)],10).

printStackTrace(Stream,Options,Depth):-prolog_current_frame(Frame),printStackTrace(Stream,Options,Depth,Frame,1).

printStackTrace(Stream,_Options,_Depth,top,PD):-!,sindent(Stream,PD),f0rmt(Stream,'<top/>~n',[]).
printStackTrace(Stream,_Options,Depth,_Frame,PD):- 0 is Depth,!,sindent(Stream,PD),f0rmt(Stream,'<toodeep/>~n',[]).
printStackTrace(Stream,Options,Depth,Frame,PD):- 
    ( (prolog_frame_attribute(Frame,predicate_indicator,MFA),pred_mf(MFA,MF),memberchk(hide(MF),Options)) ;   %% hidden module:pred
      (prolog_frame_attribute(Frame,hidden,true), \+ memberchk(show_hidden,Options))),!, %% hidden frame
   parentFrameOf(Frame,Parent),printStackTrace(Stream,Options,Depth,Parent,PD+2).
printStackTrace(Stream,Options,Depth,Frame,PD):-
         printStackFrame(Stream,Options,Frame,PD),
         parentFrameOf(Frame,Parent),
         printStackTrace(Stream,Options,Depth-1,Parent,PD+2).

parentFrameOf(Frame,Parent):-prolog_frame_attribute(Frame,parent,Parent),!.
parentFrameOf(_,top).

printStackFrame(Stream,_Options,Frame,PD):-sindent(Stream,PD),f0rmt(Stream,'<frame id="~w">~n',[Frame]),fail.
printStackFrame(Stream,Options,Frame,PD):-member(Opt,Options), 
     \+ memberchk(Opt,[show_hidden,alternative,other_fake_properties,hide(_)]), 
     prolog_frame_attribute(Frame,Opt,Value),sindent(Stream,PD),f0rmt(Stream,' ~w = ~q',[Opt,Value]),fail.
printStackFrame(Stream,Options,Frame,PD):-memberchk(alternative,Options),delete(Options,alternative,Options2),
     prolog_frame_attribute(Frame,alternative,Alt),
     sindent(Stream,PD+1),
     f0rmt(Stream,'<alt>~n',[]),
     printStackFrame(Stream,Options2,Alt,PD+3),
     sindent(Stream,PD+1),

     f0rmt(Stream,'</alt>~n',[]),
     fail.
printStackFrame(Stream,_Options,_Frame,PD):-sindent(Stream,PD),f0rmt(Stream,'</frame>~n',[]).

sindent(Stream,PD):-f0rmt(Stream,'~N',[]),sindent(Stream,PD,' ').
sindent(Stream,PD,Txt):-PD2 is PD,forall(between(0,PD2,_),f0rmt(Stream,Txt,[])).



%================================================================
:-dynamic(prolog_is_vetted_safe/0).
%================================================================
%% True means the program skips many many runtime safety checks (runs faster)
prolog_is_vetted_safe:-fail.

% tryHide(_MFA):-!.
tryHide(MFA):- ignore(catch(module_transparent(MFA),_,fail)),asserta(remember_tryHide(MFA)).

:-tryHide(tryCatchIgnore/1).
tryCatchIgnore(MFA):- error_catch(MFA,_E,true). %%debugFmt(tryCatchIgnoreError(MFA:E))),!.
tryCatchIgnore(MFA):- !,debugFmt(tryCatchIgnoreFailed(MFA)).

:-dynamic(remember_tryHide/1).

inThreadJoin(Goal):-thread_create(Goal,Id,[]),thread_join(Id,_).

:-tryHide(prolog_may/1).
prolog_may(Call):-notrace((prolog_is_vetted_safe)),!,Call.
prolog_may(Call):-debugOnError(Call).

:-tryHide(prolog_mustEach/1).
:- meta_predicate prolog_mustEach(0).
prolog_mustEach(Call):-notrace((prolog_is_vetted_safe)),!,Call.
prolog_mustEach(Call):-prolog_Each(prolog_must,Call).


prolog_Each(Pred,(A,B)):- !,prolog_Each(Pred,A),prolog_Each(Pred,B).
prolog_Each(Pred,notrace(A)):-!, hotrace(prolog_Each(Pred,A)).
prolog_Each(Pred,hotrace(A)):-!, hotrace(prolog_Each(Pred,A)).
prolog_Each(Pred,Call):- prolog_call(Pred,Call).

:-tryHide(prolog_must/1).
:- meta_predicate prolog_must(0).
prolog_must(Call):-tracing,!,debugOnError(Call).
%%prolog_must(Call):- prolog_is_vetted_safe,!,debugOnError(Call).
prolog_must(OneA):- !, (OneA *-> true ; (atrace,OneA)).
prolog_must(Call):-prolog_must_call(Call).


%%%term_expansion_call(Call, Call0):- term_expansion_safe(Call, Call0),!.
%%term_expansion_call(Call, Call0):- term_expansion(Call, Call0),!.
term_expansion_call(Call, Call):- (var(Call);atomic(Call)),!.
term_expansion_call([A|B],[AA|BB]):-!,term_expansion_call(A,AA),term_expansion_call(B,BB).
term_expansion_call(Call0, Call):-compound(Call0),Call0=..[A|RGS],term_expansion_call(RGS,RGS1),RGS\==RGS1,Call=..[A|RGS1].
term_expansion_call(Call, Call).

%%term_expansion_call(Call, Call0):-atomic(Call),!,Call=Call0.

term_expansion_safe(Call, _):-atomic(Call),!,fail.
term_expansion_safe(prolog_must(Call), Call0):-!,term_expansion_call(Call, Call0).
term_expansion_safe(once(Call), (Call0,!)):-!,term_expansion_call(Call, Call0).
term_expansion_safe(prolog_may(Call), Call0):-!,term_expansion_call(Call, Call0).
term_expansion_safe(prolog_mustEach(Call), Call0):-!,term_expansion_call(Call, Call0).
term_expansion_safe((A,B),(AA,BB)):-!,term_expansion_call(A,AA),term_expansion_call(B,BB).
term_expansion_safe((:- G),:- G0):- !,term_expansion_call(G,G0).
term_expansion_safe((?- G),:- G0):- !,term_expansion_call(G,G0).
term_expansion_safe((_:-true),_):- !,fail.
term_expansion_safe((A:-B),(A:-BB)):- term_expansion_call(B,BB),!,B\=BB.

term_expansion_safe(Call, Call0):-term_expansion_call(Call, Call0).

%%user:term_expansion(Call, Call0):-trace,prolog_is_vetted_safe,!,term_expansion_safe(Call, Call0),!.
%%user:expand_goal(Call, Call0):-prolog_is_vetted_safe,!,term_expansion_safe(Call, Call0),!.


error_catch(C,E,F):-E=error(E1,E2),!,catch(C,error(E1,E2),F).
error_catch(C,E,F):-nonvar(E),!,catch(C,E,F).
error_catch(C,E,F):-catch(C,E,(needs_rethrown(E),F)).
needs_rethrown(E):- functor(aiml_goto,E,_),!,throw(E).
needs_rethrown(E):- functor(aiml_novalue,E,_),!,throw(E).
needs_rethrown(_).

:-tryHide(tryCatchIgnore/1).
:-tryHide(error_catch/3).
:-tryHide(prolog_is_vetted_safe/0).

prolog_extra_checks:-true.

:-tryHide(findall/3).
:-tryHide(catch/3).
:-tryHide(not/1).
:-tryHide(call/1).
:-'tryHide'('$bags':findall/3).


:-set_prolog_flag(debug,true).
:-set_prolog_flag(debug_on_error,true).
:-set_prolog_flag(write_attributes,write).

user:prolog_exception_hook(A, B, C, D) :-A=error(evaluation_error(ErrorType), _), writeq(prolog_exception_hook(A, B, C, D)),interactStep(ErrorType,atrace,fail).

aiml_error(EE):-copy_term(EE,E),randomVars(E),debugFmt('~q~n',[error(E)]),!,interactStep(E,throw_safe(error(evaluation_error(E),E)),debugFmt(aiml_error(E))).

frame_depth(Depth):-prolog_current_frame(Frame),prolog_frame_attribute(Frame,level,Depth).

throw_aiml_goto(Output,VotesO):- throw(aiml_goto(Output,VotesO)).
throw_aiml_novalue(Output,VotesO):- throw(aiml_novalue(Output,VotesO)).


thread_local_flag(F,B,A):-flag(F,B,A).

debugFmtList(ListI):-notrace((copy_term(ListI,List),debugFmtList0(List,List0),randomVars(List0),debugFmt(List0))),!.
debugFmtList0([],[]):-!.
debugFmtList0([A|ListA],[B|ListB]):-debugFmtList1(A,B),!,debugFmtList0(ListA,ListB),!.

debugFmtList1(Value,Value):-var(Value),!.
debugFmtList1(Name=Number,Name=Number):-atomic(Number).
debugFmtList1(Name=Value,Name=Value):-var(Value),!.
debugFmtList1(Name=Value,Name=(len:Len)):-copy_term(Value,ValueO),append(ValueO,[],ValueO),is_list(ValueO),length(ValueO,Len),Len>9,!.
debugFmtList1(Name=Value,Name=(F:A)):-not(is_list(Value)),functor(Value,F,A).
debugFmtList1(Value,shown(Value)).



mapsome_openlist(_Pred,EndOfList):-endOfList(EndOfList),!.
mapsome_openlist(Pred,[Item|List]):-call(Pred,Item),!,mapsome_openlist(Pred,List).
mapsome_openlist(Pred,[_|List]):- mapsome_openlist(Pred,List).
mapsome_openlist(_Pred,_):-!.


/*
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
*/

:- module_transparent(user:library_directory/1).

/*
throw_safe(Exc):-atrace,throw(Exc).
string_to_atom_safe(ISO,LISTO):-LISTO==[],!,string_to_atom(ISO,'').
string_to_atom_safe(ISO,LISTO):-string_to_atom(ISO,LISTO).
atom_concat_safe(L,R,A):- ((atom(A),(atom(L);atom(R))) ; ((atom(L),atom(R)))), !, atom_concat(L,R,A),!.
exists_file_safe(File):-prolog_must(atomic(File)),exists_file(File).
exists_directory_safe(File):-prolog_must(atomic(File)),exists_directory(File).
concat_atom_safe(List,Sep,[Atom]):-atom(Atom),!,atomic_list_concat_aiml(List,Sep,Atom),!.
concat_atom_safe(List,Sep,Atom):-atom(Atom),!,atomic_list_concat_aiml(ListM,Sep,Atom),!,List = ListM.
concat_atom_safe(List,Sep,Atom):- atomic_list_concat_aiml(List,Sep,Atom),!.
upcase_atom_safe(A,B):-atom(A),upcase_atom(A,B),!.
time_file_safe(F,INNER_XML):-exists_file_safe(F),time_file(F,INNER_XML).
list_to_set_safe(A,A):-(var(A);atomic(A)),!.
list_to_set_safe([A|AA],BB):- (not(not(lastMember(A,AA))) -> list_to_set_safe(AA,BB) ; (list_to_set_safe(AA,NB),BB=[A|NB])),!.
*/

%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[X,X,X],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?
/*
maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST),prolog_must(apply(Pred,[E]))),LISTO),!, ignore(LIST=LISTO),!.
%% though this should been fine %%  maplist_safe(Pred,[A|B]):- copy_term(Pred+A, Pred0+A0), prolog_must(once(call(Pred0,A0))),     maplist_safe(Pred,B),!.

maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,LISTIN, LIST):-!, findall(EE, ((member(E,LISTIN),prolog_must(apply(Pred,[E,EE])))), LISTO),  ignore(LIST=LISTO),!.
%% though this should been fine %% maplist_safe(Pred,[A|B],OUT):- copy_term(Pred+A, Pred0+A0), prolog_mustEach(once(call(Pred0,A0,AA))),  maplist_safe(Pred,B,BB), !, ignore(OUT=[AA|BB]).
*/

:- dynamic(buggerDir/1).
:- abolish(buggerDir/1),prolog_load_context(directory,D),asserta(buggerDir(D)).
:- dynamic(buggerFile/1).
:- abolish(buggerFile/1),prolog_load_context(file,D),asserta(buggerFile(D)).


hasLibraryBuggerySupport :- absolute_file_name(library('logicmoo/logicmoo_util_library.pl'),File),exists_file(File).

throwNoLibBugger:- atrace,absolute_file_name('.',Here), buggerFile(BuggerFile), listing(user:library_directory/1), throw(error(existence_error(url, BuggerFile), context(_, status(404, [BuggerFile, from( Here) ])))).

addLibraryDir :- buggerDir(Here),atom_concat(Here,'/..',UpOne), absolute_file_name(UpOne,AUpOne),asserta(user:library_directory(AUpOne)).

% if not has library suport, add this direcotry as a library directory
% :-not(hasLibraryBuggerySupport) -> addLibraryDir ; true .

%:-hasLibraryBuggerySupport->true;throwNoLibBugger.

% TODO remove this next line
% :-ensure_loaded(library('logicmoo/logicmoo_util_bugger.pl')).
% and replace with...


% ==========================================================
%  can/will Tracer.
% ==========================================================

%:- if( \+ predicate_property(canTrace,defined)).
%:- dynamic(canTrace/0).
%canTrace.
%:- endif.

%isConsole :- telling(user).
unused:isConsole :- current_output(X),!,stream_property(X,alias(user_output)).

willTrace:-not(isConsole),!,fail.
willTrace:-canTrace.

hideTrace:-
   hideTrace([hotrace/1], -all),
   %%hideTrace(computeInnerEach/4, -all),

   hideTrace(
     [maplist_safe/2, 
              maplist_safe/3], -all),


   hideTrace([hideTrace/0,
         canTrace/0,
         atrace/0,         
         willTrace/0], -all),

   hideTrace([
         traceafter_call/1,

         notrace_call/1], -all),

   hideTrace(user:[
      call/1,
      call/2,
      apply/2,
      '$bags':findall/3,
      '$bags':findall/4,
      once/1,
      ','/2,
      catch/3,
      member/2], -all),

   hideTrace(user:setup_call_catcher_cleanup/4,-all),

   hideTrace(system:throw/1, +all),
   %%hideTrace(system:print_message/2, +all),
   hideTrace(user:message_hook/3 , +all),
   hideTrace(system:message_to_string/2, +all),
   !,hideRest,!.
   %%findall(File-F/A,(functor_source_file(M,P,F,A,File),M==user),List),sort(List,Sort),debugFmt(Sort),!.

hideRest:- fail, logicmoo_util_library:buggerDir(BuggerDir),
      functor_source_file(M,_P,F,A,File),atom_concat(BuggerDir,_,File),hideTraceMFA(M,F,A,-all),
      fail.
hideRest:- functor_source_file(system,_P,F,A,_File),hideTraceMFA(system,F,A,-all), fail.
hideRest:-doTryHides.

:- meta_predicate(hideTrace(:,+)).

functor_source_file(M,P,F,A,File):-functor_source_file0(M,P,F,A,File). %% prolog_must(ground((M,F,A,File))),prolog_must(user:nonvar(P)).
functor_source_file0(M,P,F,A,File):-current_predicate(F/A),functor(P,F,A),source_file(P,File),predicate_module(P,M).

predicate_module(P,M):- predicate_property(P,imported_from(M)),!.
predicate_module(M:_,M):-!. %strip_module(P,M,_F),!.
predicate_module(_P,user):-!. %strip_module(P,M,_F),!.
%%predicate_module(P,M):- strip_module(P,M,_F),!.

hideTrace(_:A, _) :-
        var(A), !, atrace, fail,
        throw(error(instantiation_error, _)).
hideTrace(_:[], _) :- !.
hideTrace(A:[B|D], C) :- !,
        hideTrace(A:B, C),
        hideTrace(A:D, C),!.

hideTrace(M:A,T):-!,hideTraceMP(M,A,T),!.
hideTrace(MA,T):-hideTraceMP(_,MA,T),!.

hideTraceMP(M,F/A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMP(M,P,T):-functor(P,F,0),atrace,hideTraceMFA(M,F,_A,T),!.
hideTraceMP(M,P,T):-functor(P,F,A),hideTraceMFA(M,F,A,T),!.

hideTraceMFA(_,M:F,A,T):-!,hideTraceMFA(M,F,A,T),!. 
hideTraceMFA(M,F,A,T):-user:nonvar(A),functor(P,F,A),predicate_property(P,imported_from(IM)),IM \== M,!,nop(debugFmt(doHideTrace(IM,F,A,T))),hideTraceMFA(IM,F,A,T),!.
hideTraceMFA(M,F,A,T):-hideTraceMFAT(M,F,A,T),!.

hideTraceMFAT(M,F,A,T):-doHideTrace(M,F,A,T),!.

doHideTrace(_M,_F,_A,[]):-!.
doHideTrace(M,F,A,[hide|T]):- tryHide(M:F/A),!,doHideTrace(M,F,A,T),!.
doHideTrace(M,F,A,ATTRIB):- tryHide(M:F/A),!, tryCatchIgnore(trace(M:F/A,ATTRIB)),!.



bugger:-hideTrace,traceAll,error_catch(noguitracer,_,true),debug,list_undefined.

singletons(_).

% ===================================================================
:-thread_local(lineInfoElement/4).

nthAnswerOf(Call,Nth):-flag(nthAnswerOf,_,Nth), Call,flag(nthAnswerOf,R,R-1),R=1,!.

toReadableObject(I,I):- (var(I);atomic(I)),!.
toReadableObject([I|_],[[fctx]]):-nonvar(I),I=frame(_,_,_),!.
toReadableObject(I,fctxa(Name)):-nonvar(I),I=frame(Name,_,_),!.
toReadableObject([I|N],[I0|N0]):-!,toReadableObject(I,I0),toReadableObject(N,N0),!.
toReadableObject(Comp,Comp2):-compound(Comp),Comp=..[L,I|ST],toReadableObject([I|ST],[OI|OIST]),debugOnError(Comp2=..[L,OI|OIST]),!.
toReadableObject(I,I):-!.

exists_file_safe(File):-prolog_must(atomic(File)),exists_file(File).
exists_directory_safe(File):-prolog_must(atomic(File)),exists_directory(File).

% ===============================================================================================
% listify/ unlistify / unresultify
% ===============================================================================================

%listify(OUT,OUT):-not(not(is_list(OUT))),!.
%listify(OUT,[OUT]).

unlistify([L],O):-nonvar(L),unlistify(L,O),!.
unlistify(L,L).

%%:-traceLevel(unresultify/2,+enter).
unresultify(Var,Var):-(var(Var);atomic(Var)),!.
unresultify([DictI|NV],Dict):-nonvar(DictI),NV==[],!,unresultify(DictI,Dict),!.
unresultify(Fun,Dict):-resultOrProof(Fun,DictI),!,unresultify(DictI,Dict),!.
unresultify(Var,Var).

resultOrProof(element(_,_,_),_):-!,fail.
resultOrProof(Term,Mid):-compound(Term),resultOrProof0(Term,Mid).
resultOrProof0(_=Mid,Mid):-!.
resultOrProof0(Term,Mid):-Term=..[RP,Mid|_],member(RP,[result,proof,fromTo,verbatum]),!.

%%unresultifyC(DictI,[Dict]):-atom(DictI),member(Chop,['0','1']),atom_concat(Dict,Chop,DictI),!.
unresultifyC(DictI,Dict):-unresultify(DictI,Dict),DictI\==Dict,!.

deleteAll(A,[],A):-!.
deleteAll(A,[L|List],AA):-delete(A,L,AAA),deleteAll(AAA,List,AA),!.


% ===============================================================================================
%% join_path(CurrentDir,Filename,Name)
% ===============================================================================================

join_path(CurrentDir,Filename,Name):-
         atom_ensure_endswith(CurrentDir,'/',Out),atom_ensure_endswith('./',Right,Filename),
         atom_concat(Out,Right,Name),!.

atom_ensure_endswith(A,E,A):-atom(E),atom_concat(_Left,E,A),!.
atom_ensure_endswith(A,E,O):-atom(A),atom(E),atom_concat(A,E,O),!.
atom_ensure_endswith(A,E,O):-atom(A),atom(O),atom_concat(A,E,O),!.
atom_ensure_endswith(A,O,O):-atom(A),atom(O),!.


atomic_list_concat_aiml(A,C):-  
 catch(concat_atom(A,C),_,
  catch(atomic_list_concat(A,C),_,
    (atomic_list_concat_safe(A,C)))),!.

atomic_list_concat_aiml(A,B,C):- B=='', !,atomic_list_concat_aiml(A,C).
atomic_list_concat_aiml(A,B,C):-  
 catch(atomic_list_concat(A,B,C),_,
  catch(concat_atom(A,B,C),_,
    atomic_list_concat_safe(A,B,C))),!.


os_to_prolog_filename(OS,_PL):-prolog_must(atom(OS)),fail.
os_to_prolog_filename(_OS,PL):-prolog_must(var(PL)),fail.
os_to_prolog_filename(OS,PL):-exists_file_safe(OS),!,PL=OS.
os_to_prolog_filename(OS,PL):-exists_directory_safe(OS),!,PL=OS.
os_to_prolog_filename(OS,PL):-local_directory_search_combined(CurrentDir),join_path(CurrentDir,OS,PL),exists_file_safe(PL),!.
os_to_prolog_filename(OS,PL):-local_directory_search_combined(CurrentDir),join_path(CurrentDir,OS,PL),exists_directory_safe(PL),!.
os_to_prolog_filename(OS,PL):- debugOnError(os_to_prolog_filename0(OS,PL)).
os_to_prolog_filename0(OS,PL):-atom(OS),atomic_list_concat_aiml([X,Y|Z],'\\',OS),
  atomic_list_concat_aiml([X,Y|Z],'/',OPS),!,
  os_to_prolog_filename(OPS,PL).
os_to_prolog_filename0(OS,PL):-atom_concat_safe(BeforeSlash,'/',OS),os_to_prolog_filename(BeforeSlash,PL).
os_to_prolog_filename0(OS,PL):-absolute_file_name(OS,OSP),OS \= OSP,!,os_to_prolog_filename(OSP,PL).


% ===============================================================================================
%% dont really call_with_depth_limit/3 as it spoils the debugger
% ===============================================================================================
call_with_depth_limit_traceable(G,Depth,Used):-tracing,!,G,ignore(Depth=1),ignore(Used=1).
%%call_with_depth_limit_traceable(G,Depth,Used):-call_with_depth_limit(G,Depth,Used),debugFmt(depth=Used),Used\==depth_limit_exceeded,!.
call_with_depth_limit_traceable(G,_Depth,_Used):-G.

throw_safe(Error):-var(Error),atrace,debugFmt('~N throwing VAR?! ~N'),throw(Error). %% this throws still
throw_safe(aiml_goto(A,B)):- throw(aiml_goto(A,B)).
throw_safe(aiml_novalue(A,B)):- throw(aiml_novalue(A,B)).
throw_safe(error(A,B)):- atrace, throw(error(A,B)).
throw_safe(Exc):-throw(error(Exc,Exc)).

:-op(1150,fx,meta_predicate_transparent).

must_assign(From,To):-To=From,!.
must_assign(From,To):-atrace,To=From.

:-tryHide(system:catch/3).
:-tryHide(system:not/1).

:-tryHide(cyc:ctrace/0).

:-tryHide(debugOnError/1).
:-tryHide(debugOnError0/1).
debugOnError(Call):-notrace((prolog_is_vetted_safe)),!,Call.
debugOnError(Call):-prolog_ecall(debugOnError0,Call).
debugOnError0(Call):- E = error(_,_),error_catch(Call,E,(notrace(debugFmt(caugth1st(Call,E))),debugOnError1((atrace,Call)))).
debugOnError1(Call):- E = error(_,_),error_catch(Call,E,(notrace(debugFmt(caugth2nd(Call,E))),throw(E))).

:-tryHide(prolog_must_call/1).
:-tryHide(prolog_must_call0/1).
prolog_must_call(Call):-notrace((prolog_is_vetted_safe)),!,Call.
prolog_must_call(Call):- prolog_ecall(prolog_must_call0,Call).   
prolog_must_call0(Call):- atLeastOne(Call,interactStep(prolog_must_call0,Call,true)).


:-tryHide(prolog_must_tracing/1).
:-tryHide(prolog_must_tracing0/1).
prolog_must_tracing(Call):- prolog_ecall(prolog_must_tracing0,Call).   
%%%prolog_must_tracing0(Call):-trace(Call,[-all,+fail]), atLeastOne(Call,atrace,interactStep(prolog_must_tracing0,aiml_error(Call),aiml_error(Call))).
prolog_must_tracing0(Call):-Call.


datatypeMustPred(prolog_must_call0,Call):-functor(Call,F,A),((A=1,test_pred(F));member(F,[=,==,\==])).

test_pred(T):-member(T,[var,nonvar,atom,atomic,number,is_list,compound,ground,not]),!.

:-tryHide(prolog_ecall/2).
prolog_ecall(Pred,Call):-notrace((prolog_is_vetted_safe)),!,call(Pred,Call).
prolog_ecall(Pred,Call):-prolog_ecall(call,Pred,Call).

prolog_ecall(_Conj,_Pred,Call):-var(Call),!,atrace,randomVars(Call).
prolog_ecall(Conj,Pred,Call):-tracing,!,prolog_ecall_conj(Conj,Pred,Call).
prolog_ecall(call,Pred,notrace(Call)):-prolog_ecall(notrace,Pred,Call).
prolog_ecall(Conj,Pred,(X->Y;Z)):-!,(prolog_call(Conj,X) -> prolog_ecall(Conj,Pred,Y) ; prolog_ecall(Conj,Pred,Z)).
prolog_ecall(Conj,Pred,(X->Y)):-!,(prolog_call(Conj,X)->prolog_ecall(Conj,Pred,Y)).
prolog_ecall(Conj,Pred,catch(C,E,H)):-!,catch(prolog_ecall(Conj,Pred,C),E,prolog_ecall(Conj,Pred,H)).
prolog_ecall(Conj,Pred,error_catch(C,E,H)):-!,error_catch(prolog_ecall(Conj,Pred,C),E,prolog_ecall(Conj,Pred,H)).
prolog_ecall(Conj,Pred,(X;Y)):-!,prolog_ecall(Conj,Pred,X);prolog_ecall(Conj,Pred,Y).
prolog_ecall(Conj,Pred,(X,Y)):- !,
                             debugOnError(prolog_call(Conj,X)), %%prolog_ecall(Conj,Pred,X),
                             prolog_ecall(Conj,Pred,Y).
prolog_ecall(Conj,Pred,prolog_must(Call)):-!,prolog_must(prolog_ecall(Conj,Pred,Call)).
prolog_ecall(Conj,_Pred,prolog_may(Call)):-!,prolog_may(prolog_call(Conj,Call)).
%%prolog_ecall(Conj,Pred,Call):- datatypeMustPred(Pred,Call),!,((prolog_call(Pred,Call),!);atrace).
prolog_ecall(_Conj,_Pred,Call):- fail, ignore((Call=atom(_),atrace)), 
    predicate_property(Call,number_of_clauses(_Count)),
    error_catch((clause(Call,AB),AB\==true),_,((atrace,predicate_property(Call,number_of_clauses(_Count2)),fail))),!,
    clause(Call,Body),debugOnError(Body).
prolog_ecall(Conj,Pred,Call):-prolog_ecall_conj(Conj,Pred,Call).

:-tryHide(prolog_ecall_conj/3).
prolog_ecall_conj(call,call,Call):- !, Call.
prolog_ecall_conj(call,Pred,Call):- !, prolog_call(Pred,Call).
prolog_ecall_conj(Pred,call,Call):- !, prolog_ecall(call,Pred,Call).
prolog_ecall_conj(Conj,Pred,Call):- !, prolog_call(Pred,prolog_call(Conj,Call)).
prolog_ecall_conj(Conj,Pred,Call):- debugOnError0(prolog_call(Pred,prolog_call(Conj,Call))).

prolog_call(call,Call):-!,Call.
prolog_call(Pred,Call):-call(Pred,Call).

:-tryHide(atLeastOne/1).
:-tryHide(atLeastOne/2).
:-tryHide(atLeastOne0/3).
atLeastOne(Call):-notrace((prolog_is_vetted_safe)),!,call(Call).
atLeastOne(OneA):- atLeastOne(OneA,(atrace,OneA)).
atLeastOne(OneA,Else):- !, (OneA *-> true ; Else).
atLeastOne(OneA,Else):- gensym(atLeastOne,AtLeast),flag(AtLeast,_,0),atLeastOne0(AtLeast,OneA,Else).

atLeastOne0(AtLeast,OneA,_Else):- OneA, flag(AtLeast,X,X+1).
atLeastOne0(AtLeast,OneA,Else):- flag(AtLeast,X,X),!,X=0,debugFmt(notAtLeastOnce(OneA)),tryCatchIgnore(Else).

%%atLeastOne0(OneA,_Else):-copy_term(OneA,One),findall(One,call(One),OneL),[_|_]=OneL,!,member(OneA,OneL).
%%atLeastOne0(OneA,Else):-debugFmt(failed(OneA)),!,Else,!,fail.


atLeastN(OneA,N):- atLeastN(OneA,N,(atrace,OneA)).
atLeastN(OneA,1,Else):-!,atLeastOne(OneA,Else).
atLeastN(OneA,N,Else):- gensym(atLeastN,AtLeast),flag(AtLeast,_,0),atLeastN0(AtLeast,N,OneA,Else).
atLeastN0(AtLeast,_N,OneA,_Else):- OneA, flag(AtLeast,X,X+1).
atLeastN0(AtLeast,N,OneA,Else):- flag(AtLeast,X,X),!,X<N,debugFmt(atLeastN(OneA,X>=N)),tryCatchIgnore(Else).



randomVars(Term):- R is random(10000)/10000, Start is round('*'(R,1000000)), !,
  numbervars(Term, Start, _End, [attvar(skip),functor_name('$VAR')]).

prolog_must_not(Call):-Call,!,atrace,!,aiml_error(prolog_must_not(Call)).
prolog_must_not(_Call):-!.

%:- meta_predicate dynamic_if_missing(:).
%:- meta_predicate meta_predicate_transparent(:).


dynamic_if_missing(F/A):-functor(X,F,A),predicate_property(X,_),!.
dynamic_if_missing(F/A):- 
  dynamic([F/A]).

meta_predicate_transparent(X):-strip_module(X,M,F),!, meta_predicate_transparent(M,F).
meta_predicate_transparent(M,(X,Y)):-!,meta_predicate_transparent(M,X),meta_predicate_transparent(M,Y),!.
meta_predicate_transparent(_M,X):-atom(X),!.
meta_predicate_transparent(_M,X):- 
   prolog_mustEach((   
   arg(1,X,A),functor(X,F,_),
   FA=F/A,
   dynamic_if_missing(FA),
   %module_transparent(FA),
   %%meta_predicate(X),
   %trace(FA, -all),
   %%tryHide(FA),
   !)).


asserta_new(_Ctx,NEW):-ignore(retract(NEW)),asserta(NEW).
writeqnl(_Ctx,NEW):- debugFmt('~N%%LOADING ~q.~N',[NEW]),!.


revappend([], Ys, Ys).
revappend([X|Xs], Ys, Zs) :- revappend(Xs, [X|Ys], Zs).

reverseA(Xs,Ys) :- revappend(Xs,[],Ys).

appendAttributes(_Ctx,L,R,AA):-hotrace((mergeAppend0(L,R,A),list_to_set_safe(A,AA))),!.
mergeAppend0(L,R,R):-var(L),!,var(R),!.
mergeAppend0(L,R,A):-var(R),append(L,R,A),!.
mergeAppend0(L,R,A):-var(L),append(L,R,A),!.
mergeAppend0(L,[R|RR],A):-eqmember(R,L),mergeAppend0(L,RR,A).
mergeAppend0([L|LL],R,A):-eqmember(L,R),mergeAppend0(LL,R,A).
mergeAppend0(L,R,A):-append(L,R,A).

eqmember(E,List):-copy_term_numvars(E:List,E0:List0),member(E0,List0).

list_to_set_safe(A,A):-(var(A);atomic(A)),!.
list_to_set_safe([A|AA],BB):- (not(not(lastMember(A,AA))) -> list_to_set_safe(AA,BB) ; (list_to_set_safe(AA,NB),BB=[A|NB])),!.


lastMember(E,List):-hotrace(lastMember0(E,List)).

lastMember0(_E,List):-var(List),!,fail.
lastMember0(E,[H|List]):-lastMember0(E,List);E=H.

lastMember(E,List,Rest):-hotrace(lastMember0(E,List,Rest)).

lastMember0(E,List,Rest):-lastMember0(E,List),!,delete_safe(List,E,Rest),!.
lastMember0(E,List,Rest):-lastMember0(EE,List),!,lastMember0(E,EE,Rest),!,atrace. %%delete_safe(List,EE,Rest),!.

delete_safe(List,_E,Rest):-var(List),!,Rest=List.
delete_safe(List,E,Rest):-is_list(List),!,delete(List,E,Rest).
delete_safe([H|List],E,Rest):- H==E,!,delete_safe(List,E,Rest).
delete_safe([H|List],E,[H|Rest]):-delete_safe(List,E,Rest).


getKeyValue(FullList,N=V):-lastMember(N=V,FullList),!.
%%addKeyValue(FullList,N=V):-nonvar(N),!,append(_Closed,[N=V|_],FullList),!.
addKeyValue(FullList,NV):- prolog_must((not(ground(FullList)),nonvar(NV))),append(_Closed,[NV|_],FullList),!.


lastMember2(E,List):-to_open_list(_,Closed,_Open,List),reverse(Closed,Rev),member(E,Rev).

%lastMember(End,List) :- append(_,[End|_],List).


to_open_list(FullList,Closed,Open,FullList) :- append(Closed,Open,FullList),var(Open),!.
to_open_list(Closed,Closed,Open,FullList) :- append(Closed,Open,FullList),!.


copy_term_numvars(OLD,NEW):-copy_term(OLD,NEW),numbervars(NEW,0,_).

%%%retractall(E):- retractall(E),functor(E,File,A),dynamic(File/A),!.

pp_listing(_Pred):-!. %%functor(Pred,File,A),functor(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.

% =================================================================================
% Utils
% =================================================================================

printPredCount(Msg,Pred,N1):- compound(Pred), prolog_mustEach((arg(_,Pred,NG))),nonvar(NG),!,
   findall(Pred,Pred,LEFTOVERS),length(LEFTOVERS,N1),debugFmt(num_clauses(Msg,Pred,N1)),!.

printPredCount(Msg,Pred,N1):-!,functor(Pred,File,A),functor(FA,File,A), predicate_property(FA,number_of_clauses(N1)),debugFmt(num_clauses(Msg,File/A,N1)),!.

fresh_line:-current_output(Strm),fresh_line(Strm),!.
fresh_line(Strm):-stream_property(Strm,position('$stream_position'(_,_,POS,_))),ifThen(POS>0,nl(Strm)),!.
fresh_line(Strm):-atrace,nl(Strm),!.

% =================================================================================
% Loader Utils
% =================================================================================

dynamic_load(Key,PLNAME):- creating_aiml_file(Key,PLNAME),throw_safe(creating_aiml_file(Key,PLNAME)),assert(pending_aiml_file(Key,PLNAME)).
dynamic_load(Key,PLNAME):- not(ground(dynamic_load(Key,PLNAME))),throw_safe(not(ground(dynamic_load(Key,PLNAME)))).
dynamic_load(Key,PLNAME):- loaded_aiml_file(Key,PLNAME,Time),!,debugFmt(loaded_aiml_file(Key,PLNAME,Time)).
dynamic_load(Key,_PLNAME):- nonvar(Key), once(cateForFile(_,Key,Cate)),Cate,!.

dynamic_load(Key,PLNAME):- 
   global_pathname(PLNAME,PN),
   exists_file_safe(PLNAME),
   time_file_safe(PN,Time),
   assert(loaded_aiml_file(Key,PLNAME,Time)),
   dynamic_load2(Key,PLNAME).

dynamic_load2(_Key,PLNAME):-consult(PLNAME),!.

dynamic_load2(_Key,PLNAME):- %% unload_file(PLNAME),
     open(PLNAME, read, In, []),
     repeat,
      line_count(In,Lineno),
      %% double_quotes(_DQBool)
      Options = [variables(_Vars),variable_names(_VarNames),singletons(_Singletons),comment(_Comment)],
      error_catch((read_term(In,Term,[syntax_errors(error)|Options])),E,(debugFmt(E),fail)),      
      load_term(Term,[line_count(Lineno),file(PLNAME),stream(In)|Options]),
     Term==end_of_file,
     close(In).

load_term(end_of_file,_Options):-!.
load_term(Term,Options):-error_catch(load_term2(Term,Options),E,(debugFmt(error(load_term(Term,Options,E))),throw_safe(E))).

load_term2(':-'(Term),Options):-!,load_dirrective(Term,Options),!.
load_term2(:-(H,B),Options):-!,load_assert(H,B,Options).
load_term2(Fact,Options):-!,load_assert(Fact,true,Options).

load_assert(H,B,_Options):-assert((H:-B)),!.

load_dirrective(include(PLNAME),_Options):-  (atom_concat_safe(Key,'.pl',PLNAME);Key=PLNAME),!, dynamic_load(Key,PLNAME).
load_dirrective(module(M,Preds),_Options):-!,module(M),maplist(M:export,Preds).
load_dirrective(Term,_Options):-!,Term.

showProfilerStatistics(FileMatch):-
   statistics(global,Mem), MU is (Mem / 1024 / 1024),
   printPredCount('showProfilerStatistics: '(MU),FileMatch,_N1).

aimlPredCount:-printAll(aimlPredCount(_,_,0)),printAll((aimlPredCount(Pred,File,Count),Count>0),aimlPredCount(Pred,File,Count)).
aimlPredCount(Pred,File,Count):-source_file(File),atom_contains(File,'aiml'),source_file(Pred,File),functor(Pred,F,A),current_predicate(F/A),
    predicate_property(Pred,dynamic),predicate_property(Pred,number_of_clauses(Count)).

% =================================================================================
% list_to_set_preserve_order/2
% =================================================================================
list_to_set_preserve_order([],[]):-!.
list_to_set_preserve_order([H|T],[H|TT]):-delete(T,H,M),list_to_set_preserve_order(M,TT).

% =================================================================================
% Utils
% =================================================================================

unify_listing(F/A):-!,functor(P,F,A),unify_listing(P).
unify_listing(FileMatch):-unify_listing(FileMatch,_NumberFound).
unify_listing(FileMatch,NumberFound):-unify_listing0(FileMatch),flag(printAll,NumberFound,0).

unify_listing0(FileMatch):-functor(FileMatch,F,A),unify_listing(FileMatch,F,A),!.


unify_listing_header(FileMatch):-functor(FileMatch,F,A),unify_listing_header(FileMatch,F,A),!.

unify_listing_header(_FileMatch,F,A):- fresh_line,(format('~n/* Prediate:  ~q/~q ',[F,A])),fresh_line,fail.
unify_listing_header(FileMatch,_F,_A):- forall(predicate_property(FileMatch,Print),(format('~q.~n',[Print]))),fail.
unify_listing_header(FileMatch,_F,_A):- (format('Pattern: ~q. ~n */~n',[FileMatch])),fresh_line,fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,dynamic),(format(':-dynamic(~q).~n',[F/A])),fresh_line,fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,multifile),(format(':-multifile(~q).~n',[F/A])),fresh_line,fail.
unify_listing_header(_FileMatch,_F,_A):-fresh_line.

unify_listing(FileMatch,F,A):- unify_listing_header(FileMatch,F,A), printAll(FileMatch).

printAll(FileMatch):-printAll(FileMatch,FileMatch).
printAll(Call,Print):- flag(printAll,_,0), forall((Call,flag(printAll,N,N+1)),(toReadableObject(Print,Print2),(format('~q.~n',[Print2])))),fail.
printAll(_Call,Print):- flag(printAll,PA,PA),(format('~n /* found ~q for ~q. */ ~n',[PA,Print])),!.



contains_term(SearchThis,Find):-Find==SearchThis,!.
contains_term(SearchThis,Find):-compound(SearchThis),functor(SearchThis,Func,_),(Func==Find;arg(_,SearchThis,Arg),contains_term(Arg,Find)).

% =================================================================================
% Utils
% =================================================================================

global_pathname(B,C):-absolute_file_name(B,A),!,canonical_pathname(A,C),!.
global_pathname(B,A):-relative_pathname(B,A).

relative_pathname(Path,Relative):-
   absolute_file_name(Path,[relative_to('./')],Absolute),
   member(Rel,['./','../','../../']),
   absolute_file_name(Rel,Clip),
   canonical_pathname(Absolute,AbsoluteA),
   canonical_pathname(Clip,ClipA),
   atom_concat_safe(ClipA,RelativeA,AbsoluteA),!,atom_concat_safe(Rel,RelativeA,Relative),!.
relative_pathname(Path,Relative):- canonical_pathname(Path,Relative),!.

canonical_pathname(Absolute,AbsoluteB):-prolog_to_os_filename(AbsoluteA,Absolute),canonical_pathname0(AbsoluteA,AbsoluteB),!.

canonical_pathname0(AbsoluteA,AbsoluteB):-error_catch(expand_file_name(AbsoluteA,[AbsoluteB]),E,(debugFmt(E:AbsoluteA),fail)),!.
canonical_pathname0(AbsoluteA,AbsoluteA).



% =================================================================================
% Utils
% =================================================================================

% ===============================================================================================
% UTILS
% ===============================================================================================

callInteractive(Term,Var):-time(error_catch(callInteractive0(Term,Var),E,aiml_error(E))),!.

%callInteractive0(Term,_):-atom(Term),!,Term,!,writeln(called(Term)),!.
callInteractive0(Term,Var):- fresh_line,call(Term),writeq(Term:Var),fresh_line,fail.
callInteractive0(_,_):-fresh_line,!.

%getWordTokens(WORDS,TOKENS):-atomic_list_concat_aiml(TOKENS,' ',WORDS).
%is_string(S):-string(S).


sentenceBreakChar(Last):-member(Last,[?]);sentenceEnder_NoQuestion(Last).
sentenceEnderOrPunct(Last):-member(Last,[?]);sentenceEnderOrPunct_NoQuestion(Last).
sentenceEnderOrPunct_NoQuestion(Last):-member(Last,[(',')]);sentenceEnder_NoQuestion(Last).
sentenceEnder_NoQuestion(Last):-member(Last,[('.'),('!'),('\n'),('\n\n'),('\r\n')]).

removePMark(UCase,Atoms):-append(AtomsPre,[Last],UCase),sentenceEnderOrPunct(Last),!,removePMark(AtomsPre,Atoms).
removePMark(Atoms,Atoms).

leftTrim([B|Before],ToRemove,After):-call(ToRemove,B),!,leftTrim(Before,ToRemove,After).
leftTrim(After,_ToRemove,After):-!.
leftTrim([B|Before],ToRemove,[B|After]):-leftTrim(Before,ToRemove,After).
leftTrim([],_ToRemove,[]):-!.

rightTrim(Before,ToRemove,After):-append(LeftOf,[B],Before),call(ToRemove,B),!,rightTrim(LeftOf,ToRemove,After).
rightTrim(After,_ToRemove,After):-!.

randomPick(List,Ele):-length(List,Len),Pick is random(Len),nth0(Pick,List,Ele),!.

%================================================================
% Atom / String functions
%================================================================
atomsSameCI(Name1,Name1):-!.
atomsSameCI(Name1,Name2):-atom(Name1),atom(Name2),literal_atom(Name1,D1),literal_atom(Name2,D2),!,D1=D2.

clean_codes(X,Y):-trim(X,Y),!.  % actually cyc:trim/2
clean_codes(X,X).

%clean_out_atom(X,Y):-atomWSplit(X,C),delete(C,'',O),concat_atom_safe(C,' ',Y).
clean_out_atom(X,Y):-atom_codes(X,C),clean_codes(C,D),!,atom_codes(X,D),!,Y=X.

%%%%%% everything tries to lowercase (fails case-sensitive tests)
%%atomWSplit(A,B):-atom(A),literal_atom(A,L),hotrace((cyc:atomSplit(L,BB),!,BB=B)).
%%%%%% puts backspaces in places of no spaces
:-dynamic(atomWSplit_cached/2).
:-volatile(atomWSplit_cached/2).
expire1Cache:-retractall(atomWSplit_cached(_,_)).
%%atomWSplit(A,B):- hotrace((cyc:atomWSplit(A,BB),!,BB=B)).
atomWSplit(A,B):-prolog_must(ground(A)),atomWSplit_cached(A,B),!.
atomWSplit(A,B):- hotrace((cyc:atomSplit(A,BB),!,BB=B,asserta(atomWSplit_cached(A,B)))).



expireCaches:-expire1Cache,fail.
expireCaches:-garbage_collect_atoms,garbage_collect.



%%atomWSplit(A,B):-token_stream_of(A,AA),findall(B0,arg(1,AA,B),B).

atom_concat_safe(L,R,A):- ((atom(A),(atom(L);atom(R))) ; ((atom(L),atom(R)))), !, atom_concat(L,R,A),!.

concat_atom_safe(List,Sep,[Atom]):-atom(Atom),!,atomic_list_concat_aiml(List,Sep,Atom),!.
concat_atom_safe(List,Sep,Atom):-atom(Atom),!,atomic_list_concat_aiml(ListM,Sep,Atom),!,List = ListM.
concat_atom_safe(List,Sep,Atom):- atomic_list_concat_aiml(List,Sep,Atom),!.

atom_contains(F,C):- hotrace((atom(F),atom(C),sub_atom(F,_,_,_,C))).

toCodes(B,A):-cyc:stringToCodelist(B,AO),(is_list(A) -> A=AO ; string_to_list(AO,A)),!.

% convert any term to 'atom' string
convert_to_string(I,ISO):- I \= [],
                term_to_string(I,IS),!,
		string_to_list(IS,LIST),!,
		list_replace(LIST,92,[92,92],LISTM),
		list_replace(LISTM,34,[92,34],LISTO),!,
		string_to_atom_safe(ISO,LISTO),!.
convert_to_string([],[]).

string_to_atom_safe(ISO,LISTO):-LISTO==[],!,string_to_atom(ISO,'').
string_to_atom_safe(ISO,LISTO):-string_to_atom(ISO,LISTO).

list_replace(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace(List,_Char,_Replace,List):-!.


%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[X,X,X],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?

:-tryHide(maplist_safe/2).
maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST),prolog_must(apply(Pred,[E]))),LISTO), prolog_must(LIST=LISTO),!.
%% though this should been fine %%  maplist_safe(Pred,[A|B]):- copy_term(Pred+A, Pred0+A0), prolog_must(once(call(Pred0,A0))),     maplist_safe(Pred,B),!.

:-tryHide(maplist_safe/3).
maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,LISTIN, LIST):-!, findall(EE, ((member(E,LISTIN),prolog_must(apply(Pred,[E,EE])))), LISTO),  prolog_must(LIST=LISTO),!.
%% though this should been fine %% maplist_safe(Pred,[A|B],OUT):- copy_term(Pred+A, Pred0+A0), prolog_must(once(call(Pred0,A0,AA))),  maplist_safe(Pred,B,BB), !, OUT=[AA|BB].

%================================================================
% decends tree
%================================================================

map_tree_to_list(_,PATTERN,Output):- (var(PATTERN)),!,must_assign([PATTERN],Output).
map_tree_to_list(_,NPATTERN,Output):- (number(NPATTERN),atom_to_number(PATTERN,NPATTERN)),!,must_assign([PATTERN],Output).
map_tree_to_list(_,[],OUT):-!,must_assign([],OUT).
map_tree_to_list(Pred,IN,Output):- once(call(Pred,IN,MID)),prolog_must((MID=IN -> flatten([MID],OUT) ; map_tree_to_list(Pred,MID,OUT))),!,must_assign(OUT,Output).
map_tree_to_list(Pred,[I|IN],Output):-!,prolog_must((map_tree_to_list(Pred,I,O1),map_tree_to_list(Pred,IN,O2),!,append(O1,O2,OUT))),!,must_assign(OUT,Output).
map_tree_to_list(Pred,IN,Output):-atom(IN),prolog_must((atomWSplit(IN,MID),!,map_tree_to_list(Pred,MID,OUT))),!,must_assign(OUT,Output).
map_tree_to_list(Pred,IN,Output):-
  prolog_must((compound(IN), IN=..INP, append(Left,[Last],INP), map_tree_to_list(Pred,Last,UT),!, 
   append(Left,[UT],OUTP),!, OUT =.. OUTP)),must_assign([OUT],Output).
map_tree_to_list(_,IN,IN):-atrace,must_assign([IN],IN).


dcg_maplist(_DCGPred,[    ],[    ]) --> [].
dcg_maplist( DCGPred,[A|As],[B|Bs]) --> call(DCGPred,A,B),dcg_maplist(DCGPred,As,Bs).


dumpList(B):-currentContext(dumpList,Ctx),dumpList(Ctx,B).
dumpList(_,AB):-debugFmt(dumpList(AB)),!.

dumpList(_,[]):-!.
%dumpList(Ctx,[A|B]):-!,say(Ctx,A),dumpList(Ctx,B),!.
%dumpList(Ctx,B):-say(Ctx,dumpList(B)).


ifThen(When,Do):-When->Do;true.

traceCall(A):-!,A.
traceCall(A):-trace(A,[-all,+fail]),A,!.

/*

This stuff was not really good as it surrounded everying with a once/1 secretly

:-tryHide(prolog_must/1).
:-tryHide(prolog_must0/1).
prolog_must(Call):-prolog_ecall(prolog_must0,Call).

%%%%%%%%%%%%%5%%prolog_must(Call):- clause(Call,(_A,_B)),!,clause(Call,Body),atrace,prolog_must(Body),!.
prolog_must0(Call):-  Call,!.
%%%%%%%%%%%%%%prolog_must(Call):- prolog_mustTrace(Call),!.
prolog_must0(Call):- beenCaught(Call),!.

prolog_mustTrace(prolog_must(Call)):-!,prolog_mustTrace(Call),!.
prolog_mustTrace((A,B)):- !,prolog_mustTrace(A),!,prolog_mustTrace(B),!.
prolog_mustTrace(Call):- prolog_mustTrace1(Call),debugFmt(success(Call)),!.
prolog_mustTrace(Call):- debugFmt(faild(Call)),!,atrace,Call.


prolog_mustTrace1((A,B)):- !,prolog_mustTrace1(A),!,prolog_mustTrace1(B),!.
prolog_mustTrace1(Call):- functor(Call,F,A),member(F/A,[retract/_,retractall/_]),!,debugFmt(fakingCall(Call)),numbervars(Call,0,_),!.
prolog_mustTrace1(Call):- Call,!.

beenCaught(prolog_must(Call)):- !, beenCaught(Call).
beenCaught((A,B)):- !,beenCaught(A),beenCaught(B).
beenCaught(Call):- fail, predicate_property(Call,number_of_clauses(_Count)), clause(Call,(_A,_B)),!,clause(Call,Body),beenCaught(Body).
beenCaught(Call):- E=error(_,_), catch(once(Call),E,(debugFmt(caugth(Call,E)),beenCaught(Call))),!.
beenCaught(Call):- traceAll,debugFmt(tracing(Call)),debug,atrace,Call.
*/

takeout(_,[],[]):-!.
takeout(X,[Y|R],RR):-not(not(X=Y)),!,takeout(X,R,RR),!.
takeout(X,[F|R],[F|S]) :- takeout(X,R,S),!.
takeout(_,X,X).

local_predicate(_,_/0):-!,fail.
local_predicate(_,_/N):-N>7,!,fail.
local_predicate(P,_):-predicate_property(P,built_in),!,fail.
local_predicate(P,_):-predicate_property(P,imported_from(_)),!,fail.
local_predicate(P,_):-predicate_property(P,file(F)),!,atom_contains(F,'aiml_'),!.
local_predicate(P,F/N):-functor(P,F,N),!,fail.


time_file_safe(F,INNER_XML):-exists_file_safe(F),time_file(F,INNER_XML).


%%:- current_predicate(F/N),trace(F/N, -all),fail.
/*
traceAll:- current_predicate(user:F/N),
   functor(P,F,N),
   local_predicate(P,F/N),
   trace(F/N, +fail),fail.
traceAll:- not((predicate_property(clearCateStack/1,_))),!.
traceAll:-findall(_,(member(F,[member/2,debugFmt/1,takeout/3,findall/3,clearCateStack/1]),trace(F, -all)),_).
*/
traceAll:-doTryHides.



% ===================================================================
% When you trust the code enough you dont to debug it
%  but if that code does something wrong while your not debugging, you want to see the error
% ===================================================================

:-tryHide(hotrace/1).
hotrace(X):-tracing,!, notrace(X).
hotrace(X):- call(X).

:-tryHide(lotrace/1).
lotrace(X):-tracing,!,catchAnRethrow(notrace(X)).
lotrace(X):-catchAnRethrow(X).

catchAnRethrow(X):-catch(X,E,(debugFmt(X->E),throw(E))).

% ===================================================================
% tracing durring notrace
% ===================================================================
%% "trace,tracing" .. detects if we are in a notrace/1
%% prolog_exception_hook
interactStep(String):-interactStep(String,true,true).
interactStep(String,CallYes,CallNo):-not(devmode),debugFmt(promptUser(String,[call,-,CallYes,-,or,-,CallNo])),!,CallYes.
interactStep(String,CallYes,CallNo):-debugFmt(promptUser(String,[call,-,CallYes,-,or,-,CallNo])),trace,tracing,CallYes.
interactStep(_String,CallYes,CallNo):-printStackTrace,prompt1('>>>>>>>>>>>>>>'),read(YN),debugFmt(red(YN)),YN=yes->CallYes;CallNo.

% ===================================================================
% traceIf/warnIf(_Call):-!.
% ===================================================================
:-tryHide(traceIf/1).

traceIf(Call):-copy_term(Call,Call0),Call0->debugFmt(traceIf0(Call0));true.

:-tryHide(warnIf/1).
warnIf(Call):-hotrace(ignore((Call,debugFmt(warnIf(Call))))).

% ===================================================================
% Usage: pred_subst(+Pred, +Fml,+X,+Sk,?FmlSk)
% ===================================================================

pred_subst(Pred,A,[B],C,D):- nonvar(B),!,pred_subst(Pred,A,B,C,D).
pred_subst(Pred,A,B,C,D):- error_catch(lotrace(nd_subst(Pred,A,B,C,D)),E,(debugFmt(E),fail)),!.
pred_subst(_Pred,A,_B,_C,A).

nd_subst(Pred,  Var, VarS,SUB,SUB ) :- call(Pred,Var,VarS),!.
nd_subst(Pred,  P, X, Sk, P1 ) :- functor(P,_,N),nd_subst1(Pred, X, Sk, P, N, P1 ).

nd_subst1(_Pred, _,  _, P, 0, P  ).
nd_subst1(Pred, X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], 
            nd_subst2(Pred, X, Sk, Args, ArgS ),
            nd_subst2(Pred, X, Sk, [F], [FS] ),  
            P1 =.. [FS|ArgS].

nd_subst2(_Pred, _,  _, [], [] ).
nd_subst2(Pred, X, Sk, [A|As], [A|AS]  ) :- nonvar(A), A=verbatum(_), !, nd_subst2(Pred, X, Sk, As, AS).
nd_subst2(Pred, X, Sk, [A|As], [Sk|AS] ) :- call(Pred, X, A), !, nd_subst2(Pred, X, Sk, As, AS).
nd_subst2(Pred, X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_subst2(Pred, X, Sk, As, AS).
nd_subst2(Pred, X, Sk, [A|As], [Ap|AS] ) :- nd_subst(Pred, A, X, Sk, Ap ),nd_subst2(Pred, X, Sk, As, AS).
nd_subst2(_Pred, _X, _Sk, L, L ).


%%% peekAttributes/2,pushAttributes/2,pushCateElement/2.


lengthAtLeast(N,GE):-atom(N),atom_length(N,L),L>=GE.
/*
neverUse:- meta_predicate_transparent
	maplist_safe(2,:),
	maplist_safe(3,:,:),
        asserta_new(2,:),
        writeqnl(2,:),
        prolog_mustTrace(1),
        prolog_must(1),
        beenCaught(1),
        prolog_mustEach(1),
        prolog_must(1),ignore(1), %%withAttributes(3,:,:),call_cleanup(0,0),call_cleanup(0,?,0),
        !.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5


:- use_module(library(memfile)).

%% Use a memory-file. The resulting handling is closed using close/1. 
string_to_stream(String,InStream):- string(String),string_to_atom(String,Atom),!,string_to_stream(Atom,InStream).
string_to_stream(Atom,InStream):- atom_to_memory_file(Atom, Handle),open_memory_file(Handle,read,InStream).

            
stt:- string_to_stream('hi.\nthere.\n',X),read(X,Y),read(X,Z),close(X),writeln([hi=Y,there=Z]).

tryHideProc(_MFA):-!.
tryHideProc(MFA):-tryCatchIgnore('$hide'(MFA)),tryCatchIgnore('trace'(MFA,[-all])),tryCatchIgnore(noprofile(MFA)).

doTryHides:-retract(remember_tryHide(MFA)),once(tryHideProc(MFA)),fail.
doTryHides.

