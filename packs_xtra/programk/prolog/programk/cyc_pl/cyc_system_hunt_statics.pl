
% prolog_must/1 is like Java/C's assert/1
prolog_must(X):-X,!.
prolog_must(X):-trace,not(X).

:-dynamic(asserted/4).
:-dynamic(assertion/13).

%atom_concat_safe(L,R,A):- ((atom(A),(atom(L);atom(R))) ; ((atom(L),atom(R)))), !, atom_concat(L,R,A),!.

:-use_module('cyc.pl').

current_file(FileBase,Dir):-current_stream(File,read,_Stream),atom(File),is_absolute_file_name(File),
   file_directory_name(File,Dir),file_base_name(File,FilePart),once(FileBase=FilePart;file_name_extension(FileBase,_Ext,FilePart)).

asserta_if_new(A):-retract(A),fail.
asserta_if_new(A):-asserta(A),!.

:- current_file(F,Dir),writeq(current_file(F,Dir)),nl,!,
   asserta_if_new(user:file_search_path(cyc_api, Dir)),asserta_if_new(library_directory(Dir)).

some_data_file(File,Filename):-
   absolute_file_name(library(File),[extensions(['.pl','.qlf',''])],Filename),exists_file(Filename),!.
some_data_file(File,Filename):-
   absolute_file_name(cyc_api(File),[extensions(['.pl','.qlf',''])],Filename),exists_file(Filename),!.
some_data_file(Filename,Filename).

cycdataFile(File):-some_data_file(File,Filename),exists_file(Filename),debugFmt(ensure_qlf_loaded(Filename)),ensure_qlf_loaded(Filename).

cycdataFile(File,File):-cycdataFile(File).

init_cycdataFile(F):-initialization(cycdataFile(F)).

:-ensure_loaded('../logicmoo_module_aiml_shared').

ensure_qlf_loaded(Name):-!,translate_cyc_datafile(Name).
ensure_qlf_loaded(Name):-!,ensure_loaded(Name).
ensure_qlf_loaded(Name):- file_name_extension(FileStem,_,Name),file_name_extension(FileStem,'qlf',QLF),  
   catch((load_files(FileStem,[qcompile(auto)]),(ensure_loaded(Name))),_,(qcompile(FileStem),ensure_loaded(QLF))),!.


load_lljs([]):-!.
load_lljs([X|Xs]):-load_llj(X),!,load_lljs(Xs).
load_llj(X):-[X].

:-dynamic(cyc_meta_predicate/1).
:-multifile(cyc_meta_predicate/1).
:-dynamic(nd_cyc_meta_predicate/3).
:-multifile(nd_cyc_meta_predicate/3).
cyc_meta_predicate([]):-!.
cyc_meta_predicate([X|L]):-cyc_meta_predicate(X),cyc_meta_predicate(L),!.
%%cyc_meta_predicate(X):-var(X),X = F/A, !,nd_cyc_meta_predicate(F,A,_LA).
cyc_meta_predicate(F/A):- nd_cyc_meta_predicate(F,A,A)->true;
 (X=F/A, asserta(nd_cyc_meta_predicate(F,A,A)),!,dynamic(X),multifile(X),style_check(-singleton)),!.
%cyc_meta_predicate(X):-functor(X,F,A),F \= '/', !, cyc_meta_predicate(F/A).
cyc_meta_predicate(X):-throw(X).

:-dynamic(cyc_predicate/1).
:-multifile(cyc_predicate/1).
:-dynamic(nd_cyc_predicate/3).
:-multifile(nd_cyc_predicate/3).
%%cyc_predicate(X):-var(X),X = F/A,!,nd_cyc_predicate(F,A,_LA).
cyc_predicate([]):-!.
cyc_predicate([X]):-!,cyc_predicate(X),!.
cyc_predicate([X|L]):-cyc_predicate(X),cyc_predicate(L),!.
cyc_predicate(F/A):- nd_cyc_predicate(F,A,A)->true;
 (X=F/A, asserta(nd_cyc_predicate(F,A,A)),!,dynamic(X),multifile(X),style_check(-singleton)),!.
%cyc_predicate(X):-functor(X,F,A),F \= '/', !, cyc_predicate(F/A).
cyc_predicate(X):-throw(X).


:-dynamic(savedStubs/3).
:-multifile(savedStubs/3).

%:- ensure_loaded('splits/allSplits.pl').

:- ignore((savedStubs(_, F, A), dynamic(F/A) , multifile(F/A),fail)).
:- ignore((nd_cyc_meta_predicate(F, A,_), dynamic(F/A) , multifile(F/A),fail)).
:- ignore((nd_cyc_predicate(F, A,_), dynamic(F/A) , multifile(F/A),fail)).

checkStatics(P):-not(predicate_property(P,dynamic)),trace,throw(P).
checkStatics(P):-not(predicate_property(P,multifile)),trace,throw(P).


orig_source_file(File,P):-absolute_file_name(File,F),var(P),source_file(_,F),!,source_file(P,F).

checkStatics:- orig_source_file('splits/allSplits.pl',P),once(checkStatics(P)),fail.
checkStatics:-!.


:-checkStatics.


saveOfLoaded:-!.

file_name_no_extension(File,Base):-file_name_extension(Base,_,File).
load_cyc_data_file(ZZZ):-debugFmt(ZZZ),maplist(cycdataFile,ZZZ,_).

loadRestOfStages:-expand_file_name('./stage*/*.pl',X),expand_file_name('./stage*/*.qlf',Y),append(X,Y,Z),maplist(file_name_no_extension,Z,ZZ),sort(ZZ,ZZZ),load_cyc_data_file(ZZZ).

loadRestOfKif:-expand_file_name('./stage*/*.kif',X),sort(X,ZZZ),maplist(load_cyc_kif_file,ZZZ).




%extract date
outputDateDir(FORWARD,NIL1,T1,ASSERTEDTRUEMON,DATE,NIL2,[date(DATE)|MORE]):-integer(DATE),DATE>0,!,
    outputDateDir(FORWARD,NIL1,T1,ASSERTEDTRUEMON,0,NIL2,MORE).

% backwards
outputDateDir('BACKWARD',NIL1,T1,ASSERTEDTRUEMON,DATE,NIL2,[dir('BACKWARD')|MORE]):-!,
    outputDateDir('FORWARD',NIL1,T1,ASSERTEDTRUEMON,DATE,NIL2,MORE).

outputDateDir('CODE',NIL1,T1,ASSERTEDTRUEMON,DATE,NIL2,[dir('CODE')|MORE]):-!,
    outputDateDir('FORWARD',NIL1,T1,ASSERTEDTRUEMON,DATE,NIL2,MORE).

outputDateDir('FORWARD',NIL1,T1,ASSERTEDTRUEMON,_DATE,NIL2,MORE):-
   outputDateDir2(NIL1,T1,ASSERTEDTRUEMON,NIL2,MORE).

outputDateDir2(NIL1,T1,ASSERTEDTRUEMON,NIL2,[ntan(NIL1,T1,ASSERTEDTRUEMON,NIL2)]).


makeLogicTV('TRUE', 'MONOTONIC',_ASSERTEDTRUEMON,':TRUE-MON').
makeLogicTV('TRUE', 'DEFAULT',_ASSERTEDTRUEMON,':TRUE-DEF').
makeLogicTV(_TRUE, _MONOTONIC,'ASSERTED-TRUE-MON',':TRUE-MON').
makeLogicTV(_TRUE, _DEFAULT,'ASSERTED-TRUE-DEF',':TRUE-DEF').
makeLogicTV('FALSE', 'MONOTONIC',_ASSERTEDTRUEMON,':FALSE-MON').
makeLogicTV('FALSE', 'DEFAULT',_ASSERTEDTRUEMON,':FALSE-DEF').
makeLogicTV(_TRUE, _MONOTONIC,'ASSERTED-FALSE-MON',':FALSE-MON').
makeLogicTV(_TRUE, _DEFAULT,'ASSERTED-FALSE-DEF',':FALSE-DEF').
makeLogicTV(A,B,C,D):-concat_atom([':',A,'-',B,'-',C],'',D),!.

tdval(TD):-atom(TD),!,member(TD,[':UNKNOWN',':TRUE-DEF',':TRUE-MON',':FALSE-DEF',':FALSE-MON']),!.



betterSentence(EL,HL,SENT):-atom(HL),SENT=EL,!.
betterSentence(EL,HL,SENT):-atom(EL),SENT=HL,!.
betterSentence(EL,_HL,SENT):-SENT=EL.

% ===================================================================
%% immediate
% ===================================================================
outputTrans(IN,OUT):-functor(IN,F,A),outputTrans(F,A,IN,MID),!,outputTrans2(MID,OUT).

outputTrans2(IN,OUT):-IN=..[output,TermOfUnit|_],functor(TermOfUnit,termOfUnit,_),!,OUT=skip(IN).
outputTrans2(OUT,OUT).

outputTrans(savedStubs,3,A,prologDb(A)).
outputTrans(':-',1,':-'(A),call(':-'(A))):-debugOnFailure(A),!.
% ===================================================================
%% allp.pl translator
% ===================================================================
outputTrans(assertion,_FA,assertion(NUM,'termOfUnit',
          [nart(NART), NART],
      'BaseKB',
       _,'TRUE','FORWARD','MONOTONIC','NIL','T','ASSERTED-TRUE-MON',0,'NIL'),skip(nart(NUM,NART))):-!.

outputTrans(assertion,_FA,assertion(NUM,'termOfUnit',
          [nart(NART), NART],
      'BaseKB',
       _,'TRUE','FORWARD','MONOTONIC','NIL','T','ASSERTED-TRUE-MON',0,'NIL'),skip(nart(NUM,NART))):-!.


outputTrans(assertion,_FA,assertion(NUM,ELPRED,ELARGS,MT, HL, TRUE, FORWARD, MONOTONIC,NIL1,T1,ASSERTEDTRUEMON,ZERO,NIL2),
      output(NEWSENT, NEWMT, PROPS, TRUEMON)):-
      EL=[ELPRED|ELARGS],
      betterSentence(EL,HL,SENT),
      fixupEL(MT,SENT,NEWMT,NEWSENT,[],VARS),!,
      makeLogicTV(TRUE,MONOTONIC,ASSERTEDTRUEMON,TRUEMON),!,
      %debug option
      (not(tdval(TRUEMON))->debugFmt(nonTD(TRUEMON,assertion(NUM,ELPRED,ELARGS,MT, HL, TRUE, FORWARD, MONOTONIC,NIL1,T1,ASSERTEDTRUEMON,ZERO,NIL2)));true),
      outputDateDir(FORWARD,NIL1,T1,ASSERTEDTRUEMON,ZERO,NIL2,MORE),!,append([num(NUM)|VARS],MORE,PROPS),!.

% ===================================================================
%% asserted.pl
% ===================================================================
outputTrans(asserted,4,asserted(('.'),NART,REST,[]), skip(nart([NART|REST]))):-!.

% ===================================================================
%% el_holds.pl
% ===================================================================
outputTrans(asserted,_FA,asserted(PRED,ARG1,ARG2,REST), assertedWithoutMt(NEWSENT,MtInfo,VARS)):-
  fixupEL('UnknownMt',[PRED,ARG1,ARG2|REST],MtInfo,NEWSENT,[],VARS),!.

outputTrans(holds,_FA,HOLDS, assertedWithoutMt(NEWSENT,MtInfo,VARS)):- HOLDS=..[holds|SENT],
  fixupEL('UnknownMt',SENT,MtInfo,NEWSENT,[],VARS),!.
 

% ===================================================================
%% littlebetter.pl/nocomment.pl/nonewlines.pl translator
% ===================================================================

outputTrans(TRUEMON,_FA,TDSENT, output(NEWSENT, NEWMT, PROPS, TRUEMON)):-
    TDSENT=..[TRUEMON,TD1,TD2,TD3|TDN],
    tdval(TRUEMON),
    append(PREFIX,[MT,NUM],[TD2,TD3|TDN]),
    SENT=[TD1|PREFIX],
    number(NUM),!,
    fixupEL(MT,SENT,NEWMT,NEWSENT,[num(NUM)],PROPS).

outputTrans('ASSERTION',_FA,'ASSERTION'(TRUEMON,_HL,MT,VARLIST,NUM,BACKWARDFORWARD,/*EL*/SENT),
    output(NEWSENT, NEWMT, PROPS, TRUEMON)):- compound(SENT),
    tdval(TRUEMON),
    number(NUM),!,
    fixupEL(MT,SENT,NEWMT,NEWSENT,VARLIST,VARS),!,
   append([num(NUM),dir(BACKWARDFORWARD)],VARS,PROPS).

% ===================================================================
%% read_big_kb.pl translator
% ===================================================================
outputTrans('ASSERTION',_FA,'ASSERTION'(TRUEMON,[[],SENT],MT,VARLIST,NUM,BACKWARDFORWARD), 
    output(NEWSENT, NEWMT, PROPS, TRUEMON)):- compound(SENT),
    tdval(TRUEMON),
    number(NUM),!,
    fixupEL(MT,SENT,NEWMT,NEWSENT,VARLIST,VARS),!,
       append([num(NUM),dir(BACKWARDFORWARD)],VARS,PROPS).

outputTrans('ASSERTION',_FA,'ASSERTION'(TRUEMON,_,MT,VARLIST,NUM,BACKWARDFORWARD,_HL,SENT),
    output(NEWSENT, NEWMT, PROPS, TRUEMON)):- compound(SENT),
    tdval(TRUEMON),
    number(NUM),!,
    fixupEL(MT,SENT,NEWMT,NEWSENT,VARLIST,VARS),!,
       append([num(NUM),dir(BACKWARDFORWARD)],VARS,PROPS).

outputTrans(Fun,_FA,TDSENT,error(TDSENT)):-tdval(Fun),!,fail.
outputTrans(Fun,_FA,TDSENT,error(TDSENT)):-member(Fun,['asserted','assertion','ASSERTION','holds']),!,fail.

% ===================================================================
%% newver.pl
% ===================================================================

outputTrans(PRED,_FA,TDSENT, output(NEWSENT, NEWMT, PROPS, TRUEMON)):-
    TDSENT=..[PRED,TD1,TD2,TD3|TDN],
    append(PREFIX,[MT,NUM,TRUEMON],[TD2,TD3|TDN]),
    tdval(TRUEMON),
    SENT=[PRED,TD1|PREFIX],
    number(NUM),!,
    fixupEL(MT,SENT,NEWMT,NEWSENT,[num(NUM)],PROPS).



fixupEL(MT,SENT,NEWMT,NEWSENT,VARLIST,VARS):-
   normalizeDataTerms('istAsserted'(MT,SENT),'istAsserted'(NEWMT,NEWSENT),Feats),append_nonvars(VARLIST,Feats,VARS),!.
fixupEL(_MT,_SENT,mt,[a,b,c,d,e],VARS,[v='?x'|VARS]):-!.

append_nonvars(VARLIST,Feats,VARS):-var(VARLIST),!,append_nonvars([],Feats,VARS).
append_nonvars(VARLIST,Feats,VARS):-var(Feats),!,append_nonvars(VARLIST,[],VARS).
append_nonvars(VARLIST,Feats,VARS):-append(VARLIST,Feats,VARS).

normalizeDataTerms(X,Y,Feats):- debugOnFailure((normalizeDataTerms1(X,XO,Feats),balanceBinding(XO,Y))).
 
normalizeDataTerms1(X,Z,Feats):-
      balanceBinding(X,Y),
      unnumbervars(Y,UN),
      cyc:s2p(cleanUpData, UN,Z,Feats),!.

cleanUpDataRecurse(I,O,Feats):- cyc:s2p(cleanUpData,I,O,Feats),!.

cleanUpData(S,S,[]):-var(S),!.
cleanUpData(S,S,[]):-string(S),!.
cleanUpData(M,M,[]):- atom(M),atom_concat(':',_I,M),!.
cleanUpData(nart(I),O,Feats):-!,cleanUpDataRecurse(I,O,Feats).

cleanUpData(kw(I),O,Feats):- atom_concat(':',I,M),!,cleanUpDataRecurse(M,O,Feats).
cleanUpData('#$'(I),O,Feats):- atom_concat('#$',I,M),!,cleanUpDataRecurse(M,O,Feats).

cleanUpData(string([I]),Out,[]):-localDestringify(I,Out).
cleanUpData(string(I),Out,[]):-localDestringify(I,Out).
cleanUpData(I,O,[]):-atom(I),atom_concat('#$',O,I),!,trace.
cleanUpData(I,O,[]):-atom(I),atom_concat('"',_,I),!,localDestringify(I,O).
cleanUpData(I,Out,[]):-is_string(I),localDestringify(I,Out).


cleanUpData(List,Out,[]):-is_list(List),ground(List),member('~a',List),trace,debugOnFailure(localDestringify(List,Out)).
cleanUpData([H|T],R,Features):-atom(H),cyc:isKeyword(H),!,cyc:s2List(cleanUpData,T,TT,Features),R=[H|TT],!.


localDestringify(string([I]),Out):-!,localDestringify(I,Out).
localDestringify(string(I),Out):-!,localDestringify(I,Out).
localDestringify([],""):-!,trace.
localDestringify([I],Out):-!,localDestringify(I,Out).
localDestringify([N,S],Out):-number(N),atom_number(AN,N),debugOnFailure((localDestringify(S,SS),string_concat(AN,SS,Out))).
localDestringify([AN,S],Out):-atom(AN),catch(atom_number(AN,_N),_,fail),trace,debugOnFailure((localDestringify(S,SS),string_concat(AN,SS,Out))).
localDestringify(S,Out):-not(atom(S)),!,cyc:toCycApiExpression(string(S),_,M),!,string_to_atom(M,A),debugOnFailure(localDestringify(A,Out)).
localDestringify(S,Out):-atom(S),pUnquoteAtom(S,OutA),!,string_to_atom(Out,OutA).
localDestringify(S,Out):-atom(S),!,string_to_atom(Out,S).

localDestringify(S,Out):-trace,cyc:destringify(S,DS),cyc:unquoteAtom(DS,UQ),string_to_atom(Out,UQ),!.
localDestringify(Out,error(Out)):-!,trace.

pUnquoteAtom(Atom,New):-concat_atom(['',New,''],'"',Atom).

translate_cyc_datafile(InputFileHint):-load_single_cyc_data_file(InputFileHint),!.

load_single_cyc_data_file(InputFileHint):-
  debugOnFailure((global_pathname(InputFileHint,InputFile))),
  debugOnFailure((
   guess_cyc_predname(InputFile,PredName),
   atom_concat_safe(InputFile,'.llj',OutputName),
   load_single_cyc_data_file(InputFile,OutputName,PredName,[load]))).

load_single_cyc_data_file(InputFile,OutputName,_PredMatch,_Load):- loaded_cyc_data_file(InputFile,OutputName),!.
load_single_cyc_data_file(InputFile,OutputName,PredName,Load):-
   create_cyc_data_file2(InputFile,OutputName,PredName,Load),!,
   asserta(pending_cyc_data_file(InputFile,OutputName)).

guess_cyc_predname(File,PredName):-ignore(File=PredName).

translate_single_cyc_data_file(InputFileHint):-
  debugOnFailure((
   global_pathname(InputFileHint,InputFile))),
   debugOnFailure((
   guess_cyc_predname(InputFile,PredName),
   atom_concat_safe(PredName,'.llj',OutputName),
   create_cyc_data_file2(InputFile,OutputName,PredName,[noload]))),!.


% =================================================================================
%  -> Prolog pretransating
% =================================================================================

:-dynamic(creating_cyc_data_file/2).
:-dynamic(loaded_cyc_data_file/2).
:-dynamic(pending_cyc_data_file/2).

do_pending_data_loads:-forall(retract(pending_cyc_data_file(FileName,OutputName)),load_pending_cyc_data_file(FileName,OutputName)).

load_pending_cyc_data_file(InputFile,OutputName):- debugFmt(load_pending_cyc_data_file(InputFile,OutputName)),
  catch(debugOnFailure(create_cyc_data_file2(InputFile,OutputName)),E,(debugFmt(E),asserta(pending_cyc_data_file(InputFile,OutputName)))),!.

%temp comment create_cyc_data_file2(File,OutputName,PredName,_Load):- creating_cyc_data_file(File,OutputName),!, throw_safe(already(creating_cyc_data_file(File,OutputName),PredName)).
create_cyc_data_file2(File,OutputName,PredName,_Load):- loaded_cyc_data_file(File,OutputName),!, throw_safe(already(loaded_cyc_data_file(File,OutputName),PredName)).

create_cyc_data_file2(File,OutputName,_PredMatch,Load):-
   exists_file(OutputName),
   time_file_safe(OutputName,PLTime), % fails on non-existent
   time_file_safe(File,FTime),
   %not(cyc_dataOption(rebuild__Files,true)),
   PLTime > FTime,!,
   debugFmt(up_to_date(create_cyc_data_file(File,OutputName))),!,   
   ignore((member(load,Load),asserta(pending_cyc_data_file(File,OutputName)))),!.

/* create_cyc_data_file2('c:/development/opensim4opencog/bin/cynd/cyc_api/stage1/allp.pl',
                         'c:/development/opensim4opencog/bin/cynd/cyc_api/stage1/allp.pl.llj',
                         'c:/development/opensim4opencog/bin/cynd/cyc_api/stage1/allp.pl',[load]) */
create_cyc_data_file2(InputFile,OutputName,_PredMatch,_Load):- 
 debugOnFailure((
       retractall(lineInfoElement(InputFile,_,_,_)),
        open(InputFile, read, In, [type(binary)]),
     repeat,
      line_count(In,Lineno),
      %% double_quotes(_DQBool)
      Options = [variables(_Vars),variable_names(_VarNames),singletons(_Singletons),comment(_Comment)],
      catch((read_term(In,Term,[syntax_errors(error)|Options])),E,(debugFmt(E),fail)),      
      prolog_must((load_cyc_data_term(Term,[line_count(Lineno),file(OutputName),stream(In)|Options],OutputName))),
     Term==end_of_file,
     close(In))).


commitOutput(skip(_),_Options,_OutputName):-!.
commitOutput(Output,_Options,_OutputName):-debugFmt(Output),!.

load_cyc_data_term(end_of_file,_Options,_OutputName):-!.
load_cyc_data_term(Term,Options,OutputName):-
      catch(
          debugOnFailure((outputTrans(Term,Output),commitOutput(Output,Options,OutputName))),
            E,
            (debugFmt(error(load_term(Term,Output,E))),throw_safe(E))).


create_cyc_data_file2___XXXXXX(InputFile,OutputName,PredName,Load):-        
        asserta(creating_cyc_data_file(InputFile,OutputName)),
        debugFmt(doing(create_cyc_data_file(InputFile,OutputName))),
        %%cyc_dataCateSig(CateSig),!,
   (format('%-----------------------------------------~n')),
        printPredCount('Cleaning',PredName,_CP),
   (format('%-----------------------------------------~n')),
        unify_listing(PredName),
        retractall(PredName),
   (format('%-----------------------------------------~n')),

 tell(OutputName),
        flag(cateSigCount,PREV_cateSigCount,0),
  /*
   (format('%-----------------------------------------~n')),
      withAttributes([withCategory=[asserta_cate]],
               ( fileToLineInfoElements(InputFile,AILSTRUCTURES), 
                  load_cyc_data_structure(AILSTRUCTURES))),
  */
   (format('%-----------------------------------------~n')),
        unify_listing(PredName),
   (format('%-----------------------------------------~n')),
        listing(xmlns),
   (format('%-----------------------------------------~n')),
        told,
        flag(cateSigCount,NEW_cateSigCount,PREV_cateSigCount),
        printPredCount('Errant Lines',lineInfoElement(InputFile,_,_,_),_EL),
        printPredCount('Total Categories',toDoCateSig,_TC),!,
        debugFmt('NEW_cateSigCount=~q~n',[NEW_cateSigCount]),!,
        statistics(global,Mem),MU is (Mem / 1024 / 1024),
        debugFmt(statistics(global,MU)),!,
        printPredCount('Loaded',PredName, FM),
       %% retractall(PredName),
        retractall(lineInfoElement(InputFile,_,_,_)),
        retractall(xmlns(_,_,_)),        
        retractall(creating_cyc_data_file(InputFile,OutputName)),
        ignore((FM == 0, PREV_cateSigCount>0, retractall(loaded_cyc_data_file(InputFile,OutputName)),  member(load,Load), asserta(pending_cyc_data_file(InputFile,OutputName)))),!.


        /*

        copy_term(NEW,OLD),
      cyc_dataCateOrder(Order)
         ignore(retract(OLD)),

        */

load_cyc_kif_file(File):-atom_concat(File,'.pl',OutputName),
   load_cyc_kif_file(File,OutputName,_PredName).


load_cyc_kif_file(File,OutputName,PredName):- loaded_cyc_data_file(File,OutputName),!, throw_safe(already(load_cyc_kif_file(File,OutputName),PredName)).

load_cyc_kif_file(File,OutputName,_PredMatch):-fail,
   exists_file(OutputName),
   time_file_safe(OutputName,PLTime), % fails on non-existent
   time_file_safe(File,FTime),
   %not(cyc_dataOption(rebuild__Files,true)),
   PLTime > FTime,!,
   debugFmt(up_to_date(create_cyc_data_file(File,OutputName))),!,
   Load = [load],
   ignore((member(load,Load),asserta(pending_cyc_data_file(File,OutputName)))),!.

/* create_cyc_data_file2('c:/development/opensim4opencog/bin/cynd/cyc_api/stage1/allp.pl',
                         'c:/development/opensim4opencog/bin/cynd/cyc_api/stage1/allp.pl.llj',
                         'c:/development/opensim4opencog/bin/cynd/cyc_api/stage1/allp.pl',[load]) */
load_cyc_kif_file(InputFile,_OutputName,_PredMatch):-                   
             setup_call_cleanup(open(InputFile, read, In),                   
                                load_cyc_kif_file0(InputFile, In),               
                                close(In) ).    

load_cyc_kif_file0(_InputFile, In):- 
  %%retractall(lineInfoElement(InputFile,_,_,_)),
   debugOnFailure((
    %% repeat,
      %%line_count(In,Lineno),
      %% double_quotes(_DQBool)
      %%Options = [variables(_Vars),variable_names(_VarNames),singletons(_Singletons),comment(_Comment)],
         loadLispStream(In,loadKifTerm))).

loadKifTerm(token(Surf),Vars):-!,loadKifTerm(Surf,Vars).
loadKifTerm((whitepace(_)),_Vars):- !.
loadKifTerm([H|T],Feats):- cleanUpDataRecurse([H|T],Z,Feats),!, writeFmtFlushed(surface(Z:Feats)),!.
loadKifTerm(Surf,Vars):- writeFmtFlushed(surface((Surf:Vars))),!.

%:-loadRestOfStages.
%:-loadRestOfKif.

%% getSurfaceFromChars("(:DIRECTION :FORWARD :MONOTONICITY :DEFAULT :MICROTHEORY TestVocabularyMt :CREATOR NIL :CREATION-DATE 20050707 :KIF (softwareParameterValueInSpecification InferenceSentenceParameter (Quote (isa oldConstantName (UnsuccessfulAttemptToFn ?ACT-TYPE))) LilliputQuery-OpenNoGood-165)) ",Surf,Chars).

rpl:- repeat,(cyc:lisp_read(user_input,S,Sexp)),debugFmt((string(S)->Sexp)),S="EOF\n".
%:-rpl. 

:- tell('translate.pl'),loadRestOfKif,told.
