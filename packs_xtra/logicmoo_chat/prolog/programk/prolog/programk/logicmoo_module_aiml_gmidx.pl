% ===================================================================
% File 'logicmoo_module_aiml_gmidx.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_gmidx.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================


%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
/*
:- if((current_prolog_flag(version,MMmmPP),MMmmPP<70000)).
:- style_check(-atom).
:- style_check(-string).
:- endif.
*/

:-ensure_loaded(library('programk/logicmoo_module_aiml_graphmaster.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_convertor.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_cxt_path.pl')).

:-discontiguous(load_dict_structure/2).

% ===============================================================================================
% ===============================================================================================
%%:-abolish(dict/3).

:-retractall(dict(_,_,_)).

:-pp_listing(dict(_,_,_)).


useNewCateSigSearch_broken_now:-fail.
useIndexPatternsForCateSearch:-true.
useCateID:-true.
dontAssertIndex:-fail.  % true adds 5 test failures! saves 7mb


innerTagPriority(cateid,[template,postpattern]):-useCateID.
innerTagPriority(graph,[topic,prepattern]).
innerTagPriority(precall,[that,prepattern]).
innerTagPriority(topic,[topic,prepattern]).
innerTagPriority(that,[that,prepattern]).
innerTagPriority(request,[that,prepattern]).
innerTagPriority(response,[that,prepattern]).
innerTagPriority(pattern,[pattern]).
innerTagPriority(flags,[pattern,prepattern]).
innerTagPriority(call,[that,postpattern]).
innerTagPriority(guard,[that,postpattern]).
innerTagPriority(userdict,[template,postpattern]).
innerTagPriority(template,[template,postpattern]).




% ===================================================================
%  aimlCate database decl
% ===================================================================

:-dynamic(aimlCateSigCached/1).
aimlCateSig(X):-aimlCateSigCached(X),!.
aimlCateSig(X):-aimlCateOrder(List),length(List,L),functor(Pred,aimlCate,L),asserta(aimlCateSigCached(Pred)),!,copy_term(Pred,X).

aimlCateOrder([graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile,cateid]):-useCateID,!.
aimlCateOrder([graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile]):-not(useCateID),!.


oneOrList([ID],ID):-!.
oneOrList([],_ID):-!,fail.
oneOrList(IDs,IDs).

oneOrListEach([],_IDs):-!,fail.
oneOrListEach(ID,ID):-atomic(ID),!.
oneOrListEach(IDs,ID):-member(ID,IDs).

%%textToMatchPattern([Text],MatchPattern):-textPred(Text,Pred),!,member(MatchPattern,[Pred,*,'_']).
textToMatchPattern([Text,_P|_Attern],MatchPattern):-textPred(Text,Pred),functor(MatchPattern,Pred,1).
textToMatchPattern([_Text|Pattern],MatchPattern):-member(T,Pattern),textPred(T,Pred),functor(MatchPattern,Pred,2).
textToMatchPattern(_TextPattern,'*').
textToMatchPattern(_TextPattern,'_').
%%textToMatchPattern(_TextPattern,_).

aimlPattern2CateID(Name,Text,IDO):-argNumsIndexedRepr(aimlCate,Name,N,textInput),aimlCateSig(CateSig),arg(N,CateSig,Pattern),arg(14,CateSig,ID),!,   
   findall(ID,(textToMatchPattern(Text,Pattern),CateSig),IDs),oneOrList(IDs,IDO).

aimlCate2ID(Name,Pattern,IDO):-argNumsIndexedRepr(aimlCate,Name,N,textInput),!,argNFound(aimlCate,Name,_S,Pattern),aimlCateSig(CateSig),arg(N,CateSig,Pattern),arg(14,CateSig,ID),findall(ID,CateSig,IDs),oneOrList(IDs,IDO).
aimlCate4ID(Name,IDs,Result):-aimlCateSig(CateSig),argNumsIndexedRepr(aimlCate,Name,N,_IndexType),arg(N,CateSig,Result),oneOrListEach(IDs,ID),arg(14,CateSig,ID),CateSig.

% textPatternToMatchPattern([suggest,a,topic],Pattern),aimlCate2ID(pattern,Pattern,IDO),aimlCate4ID(template,IDO,Result).

% [graph,precall,topic,that,pattern,flags,call,guard,template,userdict]
cateMemberTags(Result):- aimlCateOrder(List), findall(E,(member(E0,List),once((E0=[E|_];E0=E))), Result).

makeAimlCateSig(Ctx,ListOfValues,Pred):-aimlCateSig(Pred),!,makeAimlCate(Ctx,ListOfValues,Pred),!.

:- aimlCateOrder(List),length(List,L),dynamic(aimlCate/L),multifile(aimlCate/L).

replaceArgsVar(_Ctx,[],_CateSig):-!.
replaceArgsVar(Ctx,[E=Replacement|L],CateSig):-
    getCategoryArg1(Ctx,E,_Old,StarNumber,CateSig),
    nb_setarg(StarNumber,CateSig,Replacement),
    replaceArgsVar(Ctx,L,CateSig),!.

% ===============================================================================================
%  Indexing of Categories
% ===============================================================================================

:-dynamic(argNumsTracked/3).
:-dynamic(argNFound/4).
:-multifile(argNFound/4).


argTypeIndexable(textInput).
%%argTypeIndexable(name).

argNumsIndexedRepr(aimlCate,topic,3,textInput).
argNumsIndexedRepr(aimlCate,that,4,textInput).
argNumsIndexedRepr(aimlCate,pattern,6,textInput).

argNumsIndexedRepr(aimlCate,graph,1,name).
argNumsIndexedRepr(aimlCate,precall,2,callable).
argNumsIndexedRepr(aimlCate,request,5,flags).
argNumsIndexedRepr(aimlCate,flags,7,flags).
argNumsIndexedRepr(aimlCate,call,8,callable).
argNumsIndexedRepr(aimlCate,guard,9,callable).
argNumsIndexedRepr(aimlCate,userdict,10,name).
argNumsIndexedRepr(aimlCate,template,11,textOutput).
argNumsIndexedRepr(aimlCate,srcinfo,12,any).
argNumsIndexedRepr(aimlCate,srcfile,13,any).
argNumsIndexedRepr(aimlCate,cateid,14,name):-useCateID.

aimlCateSigArg(That,Aiml,Arg):-aimlCateSig(Aiml),argNumsIndexedRepr(aimlCate,That,N,_),arg(N,Aiml,Arg).
aimlCateArg(That,Aiml,Arg):-aimlCateSigArg(That,Aiml,Arg),call(Aiml).

%%graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile

argNumsTracked(Pred,ArgName,Position):-argNumsIndexedRepr(Pred,ArgName,Position,ArgType),argTypeIndexable(ArgType).

argNFound(F,A,'_','_'):-argNumsIndexedRepr(F,A,_,textInput).
argNFound(F,A,*,*):-argNumsIndexedRepr(F,A,_,textInput).
argNFound(F,A,List,Index):- dontAssertIndex, argNumsIndexedRepr(F,A,Num,textInput),aimlCateSig(Call),arg(Num,Call,Index),!,call(Call),
    not(member(Index,[*,'_'])),fromIndexableSArg(Index,List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5555
%% aimlCateOrder([graph,precall,topic,that,request,pattern,flags,call,guard,userdict,template,srcinfo,srcfile]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5555

makeBanner(Name):-
   'format'(':-dynamic(cid_~w/2).~n',[Name]),
   'format'(':-discontiguous(cid_~w/2).~n',[Name]),
   'format'(':-multifile(cid_~w/2).~n',[Name]),
   'format'(':-indexed(cid_~w(1,1)).~n',[Name]).

makeAccesors:-aimlCateOrder(List),member(Name,List),aimlCateSig(CateSig),
   once((argNumsIndexedRepr(aimlCate,Name,Num,_ADef),arg(Num,CateSig,V),arg(14,CateSig,ID),cateFallback(Name,Def),ID='ID',V='V')),
   makeBanner(Name),
   'format'('cateid_~w(ID,V):- cid_~w(ID,V),!.~n',[Name,Name]),
   'format'('cateid_~w(ID,V):- cateid_~w0(ID,V),!.~n',[Name,Name]),
   ignore( (arg(N,CateSig,Old),var(Old),nb_setarg(N,CateSig,'_'),fail ) ),
   'format'('cateid_~w(_,~q).~n',[Name,Def]),
   'format'('cateid_~w0(~w,~w):- ~w.~n',[Name,ID,V,CateSig]),
   'format'('~n~n',[]).

makePreloaders:-aimlCateSig(CateSig),aimlCateOrder(List),member(Name,List),
   once((argNumsIndexedRepr(aimlCate,Name,Num,_ADef),arg(Num,CateSig,V),arg(14,CateSig,ID),cateFallback(Name,Def),ID='ID',V='V')),   
   'format'('makeBanner(~w),cateid_~w0(ID,V),V \\== (~w),\'format\'(\'~~q.~~n\',[cid_~w(ID,V)]),fail.',[Name,Name,Def,Name]).

dumpCates:-aimlCateSig(CateSig),aimlCateOrder(Order),CateSig,
    %%aimlCate(Graph,Precall,Topic,That,Request,Pattern,Flags,Call,Guard,Userdict,Template,Srcinfo,Srcfile,ID),
    arg(14,CateSig,ID),
    CateSig=..[_|List],
    dumpSigPairs(ID,Order,List),
    'format'('~n',[]).

aimlCateFormed(Graph,Precall,Topic,That,Request,Pattern,Flags,Call,Guard,Userdict,Template,Srcinfo,Srcfile,ID):- cid_template(ID,Template),
  cateid_graph(ID,Graph),cateid_precall(ID,Precall),cateid_topic(ID,Topic),cateid_that(ID,That),cateid_request(ID,Request),cateid_pattern(ID,Pattern),cateid_flags(ID,Flags),
  cateid_call(ID,Call),cateid_guard(ID,Guard),cateid_userdict(ID,Userdict),cateid_srcinfo(ID,Srcinfo),cateid_srcfile(ID,Srcfile).

aimlCate(Graph,Precall,Topic,That,Request,Pattern,Flags,Call,Guard,Userdict,Template,Srcinfo,Srcfile,ID):-
   aimlCateFormed(Graph,Precall,Topic,That,Request,Pattern,Flags,Call,Guard,Userdict,Template,Srcinfo,Srcfile,ID).

dumpSigPairs(_ID,[],_List):-!.
dumpSigPairs(ID,[O|Order],[L|List]):-dumpSPairs(ID,O,L),dumpSigPairs(ID,Order,List).

dumpSPairs(_ID,Name,Def):-cateFallback(Name,Def),!.
dumpSPairs(_ID,cateid,_):-!.
dumpSPairs(ID,srcfile,File:Line-POS):- Term=..[File,Line,POS],!,dumpSPairs(ID,srcfile,Term).
dumpSPairs(ID,Name,[Value]):-!,dumpSPairs(ID,Name,Value).
dumpSPairs(ID,Name,Value):-'format'('cid_~w(~q,~q).~n',[Name,ID,Value]).

redumpFormed:-
   aimlCateFormed(Graph,Precall,Topic,That,Request,Pattern,Flags,Call,Guard,Userdict,Template,Srcinfo,Srcfile,ID),
   'format'('~q.~n',[aimlCate(Graph,Precall,Topic,That,Request,Pattern,Flags,Call,Guard,Userdict,Template,Srcinfo,Srcfile,ID)]),fail.
redumpFormed.

:-dynamic(cid_graph/2).
:-multifile(cid_graph/2).
cateid_graph(ID,V):- cid_graph(ID,V),!.
%cateid_graph(ID,V):- cateid_graph0(ID,V),!.
cateid_graph(_,default).
cateid_graph0(ID,V):- aimlCate(V,_G951,_G952,_G953,_G954,_G955,_G956,_G957,_G958,_G959,_G960,_G961,_G962,ID).



:-dynamic(cid_precall/2).
:-multifile(cid_precall/2).
cateid_precall(ID,V):- cid_precall(ID,V),!.
%cateid_precall(ID,V):- cateid_precall0(ID,V),!.
cateid_precall(_,true).
cateid_precall0(ID,V):- aimlCate(_G950,V,_G952,_G953,_G954,_G955,_G956,_G957,_G958,_G959,_G960,_G961,_G962,ID).



:-dynamic(cid_topic/2).
:-multifile(cid_topic/2).
cateid_topic(ID,V):- cid_topic(ID,V),!.
%cateid_topic(ID,V):- cateid_topic0(ID,V),!.
cateid_topic(_,*).
cateid_topic0(ID,V):- aimlCate(_G950,_G951,V,_G953,_G954,_G955,_G956,_G957,_G958,_G959,_G960,_G961,_G962,ID).



:-dynamic(cid_that/2).
:-multifile(cid_that/2).
cateid_that(ID,V):- cid_that(ID,V),!.
%cateid_that(ID,V):- cateid_that0(ID,V),!.
cateid_that(_,*).
cateid_that0(ID,V):- aimlCate(_G950,_G951,_G952,V,_G954,_G955,_G956,_G957,_G958,_G959,_G960,_G961,_G962,ID).



:-dynamic(cid_request/2).
:-multifile(cid_request/2).
cateid_request(ID,V):- cid_request(ID,V),!.
%cateid_request(ID,V):- cateid_request0(ID,V),!.
cateid_request(_,*).
cateid_request0(ID,V):- aimlCate(_G950,_G951,_G952,_G953,V,_G955,_G956,_G957,_G958,_G959,_G960,_G961,_G962,ID).



:-dynamic(cid_pattern/2).
:-multifile(cid_pattern/2).
cateid_pattern(ID,V):- cid_pattern(ID,V),!.
%cateid_pattern(ID,V):- cateid_pattern0(ID,V),!.
cateid_pattern(_,*).
cateid_pattern0(ID,V):- aimlCate(_G950,_G951,_G952,_G953,_G954,V,_G956,_G957,_G958,_G959,_G960,_G961,_G962,ID).



:-dynamic(cid_flags/2).
:-multifile(cid_flags/2).
cateid_flags(ID,V):- cid_flags(ID,V),!.
%cateid_flags(ID,V):- cateid_flags0(ID,V),!.
cateid_flags(_,*).
cateid_flags0(ID,V):- aimlCate(_G950,_G951,_G952,_G953,_G954,_G955,V,_G957,_G958,_G959,_G960,_G961,_G962,ID).



:-dynamic(cid_call/2).
:-multifile(cid_call/2).
cateid_call(ID,V):- cid_call(ID,V),!.
%cateid_call(ID,V):- cateid_call0(ID,V),!.
cateid_call(_,true).
cateid_call0(ID,V):- aimlCate(_G950,_G951,_G952,_G953,_G954,_G955,_G956,V,_G958,_G959,_G960,_G961,_G962,ID).



:-dynamic(cid_guard/2).
:-multifile(cid_guard/2).
cateid_guard(ID,V):- cid_guard(ID,V),!.
%cateid_guard(ID,V):- cateid_guard0(ID,V),!.
cateid_guard(_,true).
cateid_guard0(ID,V):- aimlCate(_G950,_G951,_G952,_G953,_G954,_G955,_G956,_G957,V,_G959,_G960,_G961,_G962,ID).



:-dynamic(cid_userdict/2).
:-multifile(cid_userdict/2).
cateid_userdict(ID,V):- cid_userdict(ID,V),!.
%cateid_userdict(ID,V):- cateid_userdict0(ID,V),!.
cateid_userdict(_,user).
cateid_userdict0(ID,V):- aimlCate(_G950,_G951,_G952,_G953,_G954,_G955,_G956,_G957,_G958,V,_G960,_G961,_G962,ID).



:-dynamic(cid_template/2).
:-multifile(cid_template/2).
cateid_template(ID,V):- cid_template(ID,V),!.
%cateid_template(ID,V):- cateid_template0(ID,V),!.
cateid_template(_,[]).
cateid_template0(ID,V):- aimlCate(_G950,_G951,_G952,_G953,_G954,_G955,_G956,_G957,_G958,_G959,V,_G961,_G962,ID).



:-dynamic(cid_srcinfo/2).
:-multifile(cid_srcinfo/2).
cateid_srcinfo(ID,V):- cid_srcinfo(ID,V),!.
%cateid_srcinfo(ID,V):- cateid_srcinfo0(ID,V),!.
cateid_srcinfo(_,missinginfo).
cateid_srcinfo0(ID,V):- aimlCate(_G950,_G951,_G952,_G953,_G954,_G955,_G956,_G957,_G958,_G959,_G960,V,_G962,ID).



:-dynamic(cid_srcfile/2).
:-multifile(cid_srcfile/2).
cateid_srcfile(ID,V):- cid_srcfile(ID,V),!.
%cateid_srcfile(ID,V):- cateid_srcfile0(ID,V),!.
cateid_srcfile(_,missingfile).
cateid_srcfile0(ID,V):- aimlCate(_G950,_G951,_G952,_G953,_G954,_G955,_G956,_G957,_G958,_G959,_G960,_G961,V,ID).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5555
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5555


assert_cate_in_load(NEW) :- currentContext(assert_cate_in_load,Ctx),prolog_must(assert_cate_in_load(Ctx,NEW)),!.

assert_cate_in_load(Ctx,CateSig):-
    duplicate_term(CateSig,CateSigTest),
    load_category(Ctx,CateSigTest),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% load_category(Ctx,CateSig)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assertaFront(argNFound(_,_,_,_)):-dontAssertIndex,!.
assertaFront(Indexable):-tryCatchIgnore(retractall(Indexable)),asserta(Indexable),!.

load_category(Ctx,CateSig):-
      isRetraction(Ctx,CateSig,RemovemeMask),!,
      withArgIndexing(RemovemeMask,dirtyArgIndex,Removeme),
      immediateCall(Ctx,findall(Removeme,retract_cate_post_index(Removeme),Retracted)),!,
      findall(Removeme,retract_cate_post_index(Removeme),Retracted),!.

load_category(Ctx,CateSig):-
      withArgIndexing(CateSig,addArgIndex,Indexable),
      assertaFront(Indexable),!,
      traceIf(((not(not(Indexable==CateSig))),not(arg(6,CateSig,*)))),!,
      immediateCall(Ctx,assert_cate_post_index(Indexable)),!,
      confirm_args_indexed(Indexable).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% assert_cate_post_index(Indexable)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_cate_post_index(Indexable):-assertaFront(Indexable),confirm_args_indexed(Indexable),!,immediateCall(_Ctx,assert_cate_post_index(Indexable)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% retract_cate_post_index(Indexable)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
retract_cate_post_index(Removeme):-
   immediateCall(_Ctx,retract_cate_post_index(Removeme)),!,
   %%withArgIndexing(Retract,dirtyArgIndex,Removeme),
   %%debugFmt(retract_cate_post_index(Removeme)),!,
   prolog_must(ignore(retract(Removeme))),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% confirm_args_indexed(Indexable)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

confirm_args_indexed(_Indexable):-dontAssertIndex,!.
confirm_args_indexed(Indexable):-functor(Indexable,F,_),
      argNumsIndexedRepr(F,ArgName,N,ArgType),argTypeIndexable(ArgType),
      arg(N,Indexable,Value),
      confirm_1arg_indexed(F,ArgName,Value),
      fail.
confirm_args_indexed(_Indexable). %%confirmed

confirm_1arg_indexed(F,ArgName,Value):-argNFound(F,ArgName,_,Value),!. %% all good
confirm_1arg_indexed(F,ArgName,Value):-fromIndexableSArg(Value,SList),debugFmt(not(argNFound(F,ArgName,SList,Value))),!. %% made now!

%%%
ffffffff.
noTrickyIndexing:-fail.

toNonIndexable(FAKE,FAKE):-noTrickyIndexing,!.
toNonIndexable(OF,INDEXABLE):-OF=..[F|ARGS],functor(OF,F,A),toNonIndexable0(A,F,ARGS,NEWARGS),!,INDEXABLE=..[F|NEWARGS].
toNonIndexable0(0,_F,_,[]):-!.
toNonIndexable0(3,aimlCate,List,List):-!.
toNonIndexable0(N,F,[A|ARGS],[NEW|NEWARGS]):-N2 is N-1, toNonIndexableArg(A,NEW),toNonIndexable0(N2,F,ARGS,NEWARGS).

toNonIndexableArg(A,A):-var(A),!.

toNonIndexableArg(A,A):-(member(A,['*','[]','_',true]);atomic(A)),!.
toNonIndexableArg([A|H],[A|H]):-!.
toNonIndexableArg(A,A):-not(compound(A)),!.
toNonIndexableArg(A,[A]):-not(compound(A)),!.
toNonIndexableArg(A,B):-fromIndexableSArg(B,A),!.
toNonIndexableArg(A,[A0]):- trace,A=..[A0/*,idx0*/].
toNonIndexableArg(A,[A0|AN]):-A=..[A0,idx|AN].
toNonIndexableArg(A,[AA|AL]):-A=..[A0,idxl,AN|AL],AA=..[A0|AN].
toNonIndexableArg(A,[A]).

/*

 [*] ==> *
 [he] ==> he

 [*,likes,*] ==>  likes(idxm,*)
 [*,likes,it] ==>  likes(idxm,it)
 [he,likes,it] ==>  he(likes,it)
 ['DO', 'THE', 'GENDER', 'TEST'] => do(idx, the, gender, test)).


 [*,Word|More]  => Word(idxm,REST)

 [i,like,birds] ===> i(_), like(_,_), birds(_,_,_), star_star(_,_), '*'

 [i,like,many,birds] ===> i(_), like(_,_), many(_,_), birds(_,_,_), star_star(_,_), '*'

*/

notStarCard(X):- \+ isStarCard(X), prolog_must(atom(X)).

is1Star(X):-isStar0(X).
is1Star(element(_,_,_)).
is1Star(star(_,_,_)).

mustCardSpec(X):-prolog_must(is1Star(X)),!.

isStarCard(X):-var(X),!,aiml_error(isStarCard(X)).
isStarCard(X):-is1Star(X).
isStarCard([_|_]):-!,fail.
isStarCard(X):- functor(X,F,_A),member(F,[star1,star_star]).


fromIndexableSArg0(I,[var(I)]):-var(I),!.
%%fromIndexableSArg0(I,S):-argNFound(_,_,S,I),!.
fromIndexableSArg0([A|B],ABS):-!,fromIndexableSArg0(A,AS),fromIndexableSArg0(B,BS),append(AS,BS,ABS),!.
fromIndexableSArg0([],[]):-!.
fromIndexableSArg0(I,[I]):-atomic(I),!.
fromIndexableSArg0(element(E,A,B),[element(E,A,B)]):-!.
fromIndexableSArg0(idx_startswith(E),[E]):-!.
fromIndexableSArg0(star_star(_Len,List),ABS):-fromIndexableSArg0(List,ABS).
fromIndexableSArg0(I,ABS):-I=..[A,B],!,fromIndexableSArg0(A,AS),fromIndexableSArg0(B,BS),append(AS,BS,ABS).
fromIndexableSArg0(I,ABS):-I=..[B,idx_endswith,A,_],!,fromIndexableSArg0(A,AS),fromIndexableSArg0(B,BS),append(AS,BS,ABS).
fromIndexableSArg0(I,ABCS):-I=..[B,A,C],!,fromIndexableSArg0(A,AS),fromIndexableSArg0(B,BS),fromIndexableSArg0(C,CS),append(AS,BS,ABS),append(ABS,CS,ABCS),!.
fromIndexableSArg0(OTHER,[OTHER]):-debugFmt(fromIndexableSArg0(OTHER)).

fromIndexableSArg(B,A):-dontAssertIndex,!,prolog_must(fromIndexableSArg0(B,A)),!.
fromIndexableSArg(B,A):-prolog_must(fromIndexableSArg0(B,A)),!.
fromIndexableSArg(B,A):-nonvar(B),isStarCard(B),!,prolog_must(desegmentStars(B,A)).
fromIndexableSArg(B,A):-prolog_must(toIndexableSArg(A,B)),!.


reSegmentStars(Star,StarStarO):-desegmentStars(Star,Seg),segmentStar(Seg,StarStar),
    (StarStar=Star->StarStarO=Star;StarStarO=StarStar),!.

segmentStar([Star],Star):-is1Star(Star),!.
segmentStar([Star],star1(Star)):-mustCardSpec(Star),!.
segmentStar([Star|SegS],star_star(Len,[Star|SegS])):-length([Star|SegS],Len),mustCardSpec(Star).

desegmentStars([Star|SegS],[Star|SegS]):-mustCardSpec(Star).
desegmentStars(star_star(_,B),A):-!,desegmentStars(B,A).
desegmentStars(star1(B),A):-!,desegmentStars(B,A).
desegmentStars(Star,[Star]):-mustCardSpec(Star).

mergeStars(Star1,Star2,star_star(Len,StarStar)):-desegmentStars(Star1,Seg1),desegmentStars(Star2,Seg2),append(Seg1,Seg2,StarStar),length(StarStar,Len).

% star1/1
toIndexableSArg([Star],StarStar):-isStarCard(Star),!,toIndexableSArg(Star,StarStar).
% star1/1 and(/)  star_star/2
toIndexableSArg(Star,StarStar):-isStarCard(Star),!,prolog_must(reSegmentStars(Star,StarStar)).

toIndexableSArg(Star,Star):-atomic(Star),!.

% word/0
toIndexableSArg([Word],Word):-!.
toIndexableSArg([Star],StarStar):-toIndexableSArg(Star,StarStar).

% word/3
toIndexableSArg([Star,Word|[]],INDEXABLE):-isStarCard(Star),notStarCard(Word),INDEXABLE=..[Word,idx_endswith,(Star),Word].
% star_star/2 implicit
toIndexableSArg([Star1,Star2|[]],StarStar):-isStarCard(Star1),isStarCard(Star2),mergeStars(Star1,Star2,StarStar).
% word/1
toIndexableSArg([Word,Star|[]],INDEXABLE):-isStarCard(Star),notStarCard(Word),INDEXABLE=..[Word,StarStar],reSegmentStars(Star,StarStar).
% word/1 implicit
%toIndexableSArg([Word1,Word2|[]],INDEXABLE):-notStarCard(Word1),notStarCard(Word2),INDEXABLE=..[Word1,Word2].

% word/2
toIndexableSArg([Star,Word|More],INDEXABLE):-isStarCard(Star),notStarCard(Word),INDEXABLE=..[Word,idx_startswith(Star),REST],toIndexableArg(More,REST).
% Unk/N
toIndexableSArg([W,Star1,Star2|More],INDEXABLE):-isStarCard(Star1),isStarCard(Star2),mergeStars(Star1,Star2,StarStar),toIndexableArg([W,StarStar|More],INDEXABLE).
% Unk/N
toIndexableSArg([Star1,Star2|More],INDEXABLE):-isStarCard(Star1),isStarCard(Star2),mergeStars(Star1,Star2,StarStar),toIndexableArg([StarStar|More],INDEXABLE).
% word/1
toIndexableSArg([Word,Star|More],INDEXABLE):-isStarCard(Star),notStarCard(Word),INDEXABLE=..[Word,REST],toIndexableArg([Star|More],REST).
% word/1
toIndexableSArg([Word1,Word2|More],INDEXABLE):-notStarCard(Word1),notStarCard(Word2),INDEXABLE=..[Word1,REST],!,toIndexableArg([Word2|More],REST).



%%toIndexable(FAKE,FAKE):-!.

toIndexable(OF,INDEXABLE):-OF=..[F|ARGS],functor(OF,F,A),toIndexable0(A,F,ARGS,NEWARGS),!,INDEXABLE=..[F|NEWARGS].
toIndexable0(0,_F,_,[]):-!.
toIndexable0(3,aimlCate,List,List):-!.
toIndexable0(N,F,[A|ARGS],[NEW|NEWARGS]):-N2 is N-1, makeIndexableArg(F,N,A,NEW),!,toIndexable0(N2,F,ARGS,NEWARGS).


makeIndexableArg(_,_,A,A):-noTrickyIndexing,!.  %%TODO: REMOVE THIS DISABLER
makeIndexableArg(F,ArgNumber,A,AHL):-argNumsIndexedRepr(F,_Pattern,ArgNumber,ArgType),makeIndexableArg(F,ArgType,A,AHL).
makeIndexableArg(F,ArgType,A,AHL):-argNumsIndexedRepr(F,Pattern,_,ArgType),makeIndexableArg(Pattern,ArgType,A,AHL).
makeIndexableArg(Pattern,ArgType,A,AH):-argNumsIndexedRepr(_F,Pattern,_ArgNumber,ArgType),argTypeIndexable(ArgType),toIndexableArg(Pattern,ArgType,A,AH).
makeIndexableArg(_,_,A,A).

toIndexableArg(_,_,B,AHL):-toIndexableArg(B,A),!,!,prolog_must(toLowercase(A,AHL)).

toIndexableArg(A,A):- noTrickyIndexing,!.  %%TODO: REMOVE THIS DISABLER
toIndexableArg(A,A):-var(A),!.
toIndexableArg(A,AH):-is_list(A),removeSkippables(A,AL),A\==AL,!,toIndexableArg(AL,AH).
toIndexableArg(A,A):-member(A,['*','[]','_']),!.
toIndexableArg([A],AA):-not(compound(A)),!,toIndexableArg(A,AA).
toIndexableArg(A,A):-not(compound(A)),!.
toIndexableArg([A],AH):- atom(A),!,AH=..[A/*,idx0*/],!.
toIndexableArg([A],N):-toIndexableArg(A,N).
toIndexableArg(B,A):-prolog_must(toIndexableSArg(B,A)),!.
toIndexableArg([A|H],AH):- trace,atom(A),AH=..[A,idx|H],!.
toIndexableArg([A|H],AH):- A=..[A0|AN],predify(A,AH,A0,AN,H),!.
toIndexableArg(A,A).

predify(_A,AH,A0,AN,H):-predify(AH,A0,AN,H).
predify(A,[A|H],_A0,_AN,H).

predify(AH,A0,[],H):-AH=..[A0,idx|H].
predify(AH,A0,H,[]):-AH=..[A0,idx|H].
predify(AH,A0,AN,H):-AH=..[A0,idxl,AN|H].

%%%%%%%%%%%%%%%%%%%
%%withArgIndexing(+CateSig,+DoWhat,-Indexable):-!.
%%%%%%%%%%%%%%%%%%5
%%withArgIndexing(CateSig,DoWhat):-prolog_must(withArgIndexing(CateSig,DoWhat,_Indexable)).

withArgIndexing(CateSig,_DoWhat,Indexable):- \+ useIndexPatternsForCateSearch,!,duplicate_term(CateSig,Indexable).
withArgIndexing(CateSig,DoWhat,Indexable):-
  functor(CateSig,F,A),
  prolog_must(var(Indexable)),
  functor(Indexable,F,A),
  duplicate_term(CateSig,Indexable),
  prolog_must(withArgIndexing4(CateSig,F,DoWhat,Indexable)),!.

withArgIndexing4(CateSig,Functor,DoWhat,Indexable):- argNumsTracked(Functor,ArgName,ArgNumber),
  argNumsIndexedRepr(Functor,ArgName,ArgNumber,ArgType),
  once((arg(ArgNumber,CateSig,Arg),
         once((
          % call/9 is missing?  call(DoWhat,CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType),
          apply(DoWhat,[CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType]),
              nb_setarg(ArgNumber,Indexable,IndexableArg))))),fail.

withArgIndexing4(_CateSig,_F,_DoWhat,_Indexable).

staredArgIndex(_CateSig,_Indexable,_Functor,_ArgName,_ArgNumber,[IndexableArg],IndexableArg,ArgType):-argTypeIndexable(ArgType),is1Star(IndexableArg),!.
staredArgIndex(_CateSig,_Indexable,_Functor,_ArgName,_ArgNumber,IndexableArg,IndexableArg,ArgType):-argTypeIndexable(ArgType),is1Star(IndexableArg),!.

addArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):-staredArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType),!.

addArgIndex(_CateSig,_Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):-argTypeIndexable(ArgType),
  makeIndexableArg(Functor,ArgNumber,Arg,IndexableArg),
  assertaFront(argNFound(Functor,ArgName,Arg,IndexableArg)),!,
  immediateCall(_Ctx,assert_argNFound(Functor,ArgName,Arg,IndexableArg)).

addArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):- atrace,
  debugFmt(addArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType)),
  prolog_must(Arg=IndexableArg),!.


assert_argNFound(Functor,ArgName,Arg,IndexableArg):-assertaFront(argNFound(Functor,ArgName,Arg,IndexableArg)).

dirtyArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):-staredArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType),!.
dirtyArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType):-
  debugFmt(dirtyArgIndex(CateSig,Indexable,Functor,ArgName,ArgNumber,Arg,IndexableArg,ArgType)),!.

%%%% todo maybe this.. once((retract(NEW),asserta(NEW)) ; (asserta(NEW),(debugFmt('~q.~n',[asserta(N)])))),!.
/*
asserta_if_new(NEW):-!,
  once(
   (retract(NEW),asserta(NEW)) ;
   (asserta(NEW),debugFmt('~q.~n',[asserta(NEW)])) ),!.
*/
asserta_if_new(N):-catch(N,E,debugFmt(error_in(E,N))),!.
asserta_if_new(N):-asserta(N),debugFmt(asserta_if_new(N)),!.

% ===============================================================================================
%  Save Categories
% ===============================================================================================
assertCate(Ctx,Cate,DoWhat):-
      prolog_must(makeAimlCate(Ctx,Cate,Value)),!,
      prolog_must(ground(Value)),
      prolog_must(assertCate3(Ctx,Value,DoWhat)),!.

%% todo maybe this.. once((retract(NEW),asserta(NEW)) ; (asserta(NEW),(debugFmt('~q.~n',[NEW])))),!.
% assertCate3(Ctx,NEW,DoWhat):-NEW,!.
assertCate3(Ctx,NEW,DoWhat):-
  flag(cateSigCount,X,X+1),
  forall(member(Pred,DoWhat),prolog_must(call(Pred,Ctx,NEW))).
% ===============================================================================================
%  Make AIML Categories
% ===============================================================================================
makeAimlCate(Ctx,Cate,Value):-
 prolog_mustEach((
   convert_template(Ctx,Cate,Assert),
   aimlCateOrder(Order),
   makeAllParams(Ctx,Order,Assert,Result),
   arg2OfList(Result,LISTO), Value =.. [aimlCate|LISTO])).

arg2OfList(LIST,LISTO):-maplist_safe(arg2,LIST,LISTO),!.
arg2(_=Value,Value):-!.
arg2(Value,Value):-!,atrace.


translate_cate(Ctx,CateSig):-replaceArgsVar(Ctx,[srcinfo=_],CateSig),assert_cate_in_load(Ctx,CateSig).

is_xml_missing(Var):-prolog_must(nonvar(Var)),!,member(Var,['[]','*','_']),!.

isRetraction(Ctx,CateSig,OF):-getCategoryArg1(Ctx,'template',NULL,_StarNumber,CateSig),!,is_xml_missing(NULL),
   duplicate_term(CateSig,OF),replaceArgsVar(Ctx,['template'=_,srcinfo=_,srcfile=_,cateid=_],OF),!.

% ===============================================================================================
%  Popping when Building categories
% ===============================================================================================

clearCateStack(_Ctx):- retractall(dict(category,_,_)).

peekCateElements(Ctx,Cate):- cateMemberTags(CATETAGS), peekAttributes(Ctx,CATETAGS,category,Cate),!.

popCateElements(Ctx,Cate):- cateMemberTags(CATETAGS), peekAttributes(Ctx,CATETAGS,category,Cate),!.
popCateElements(Ctx,CateO):- popCateElements1(Ctx,Cate1),popCateElements2(Ctx,Cate2),append(Cate1,Cate2,Cate),!,CateO=Cate.
popCateElements1(Ctx,CateO):- findall(Tag=DCG,cateNodes1(Ctx,category,Tag,DCG),Cate),!,CateO=Cate.
popCateElements2(Ctx,CateO):- findall(Tag=DCG,cateNodes2(Ctx,Tag,DCG),Cate),!,CateO=Cate.


cateNodes1(Ctx,Scope,Tag,DCGO):-member(Tag,[pattern,template]),once(cateNodes1a(Ctx,Scope,Tag,TEMPLATE)),once(convert_template(Ctx,TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes1a(Ctx,Scope,Tag,DCGO):-peekNameValue(Ctx,Scope,Tag,DCG,'$failure'),popNameValue(Ctx,Scope,Tag,DCG),!,DCG=DCGO.
cateNodes1a(Ctx,Scope,Tag,DCGO):-listing(dict),aiml_error(peekNameValue(Ctx,Scope,Tag,DCG)),!,DCG=DCGO.
cateNodes1a(Ctx,Scope,Tag,DCGO):-peekNameValue(Ctx,Other,Tag,DCG,'$error'),Other\==Scope,!,DCG=DCGO.


cateNodes2(Scope,Tag,DCGO):-
 member(Tag,
    [that,guard,topic]),once(cateNodes2a(Scope,Tag,TEMPLATE)),once(convert_template(_Ctx,TEMPLATE,DCG)),!,DCG=DCGO.

cateNodes2a(Scope,Tag,DCGO):-peekNameValue(_Ctx,Other,Tag,DCG,'$failure'),Other\==Scope,!,DCG=DCGO.
cateNodes2a(Scope,Tag,DCGO):-aiml_error(peekNameValue(_Ctx,Scope,Tag,DCG)),!,DCG=DCGO.

defaultPredicates(N,V):-member(N,[username,botname]),V='*'.

%defaultPredicates(N,V):-member(N,[input,pattern]),V='*'.
defaultPredicates(N,V):-defaultPredicatesS(S),member(N=V,S).
defaultPredicatesS([
             cateid=gensym(cateid),
             graph='default',
             precall='true',
             topic='*',
             that='*',
             request='*',
             flags='*',
             pattern='*',
             call='true',
             % hide for testing
             dictionary='default',
             userdict='user',
             substitutions='input',
             guard='*',
             template=['is ERROR IN CATE'],
             lang='bot',
             srcinfo=missinginfo,
             srcfile=missingfile,
             withCategory=[writeqnl,assert_cate_in_load]]).

cateMember(Tag):-cateMemberTags(List),member(Tag,List).

defaultCatePredicatesS(Defaults):-cateFallback(Defaults).

/*
And your chair is kept this time For some confidence you can ask them when the next trip to Value Villiage is..
You should be permited to keep your chair since you are willing to not leave the grounds except on nursing facility sanctioned trips.
I Think they have trips to the Dollar Tree and other places
They also can give us special permission on Mondays and Thursdays

The main thing is that you are willing to give them the peace of mind that they dont need to "watch you".

*/
cateFallback(N,V):-cateFallback(List),!,member(N=V,List).
cateFallback([
       cateid=gensym(arule),
       graph = 'default',
       precall = 'true',
       topic = '*',
       that = '*',
       request = '*',
       pattern='*',
       flags= '*',
       call = 'true',
       guard = '*',
       userdict = 'user',
       % template = [],  ['Nothing'] ?
       % template= [gensym(template)],
       srcinfo=missinginfo,
       srcfile=missingfile,
       withCategory=[writeqnl,assert_cate_in_load]]).
       %%|MORE]):-findall(N=V,defaultPredicates(N,V),MORE).

pathAttrib(S):-pathAttribS(SS),member(S,SS).
pathAttribS([filename,uri,loc,url,path,dir,file,pathname,src,srcfile,location]).


