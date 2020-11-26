:- if((prolog_load_context(file,File),
   must((unload_file(File),
   prolog_load_context(module,Module),
   module_property(Module,file(SourceFile)),
   dmsg(pfc_file_api(SourceFile)),
   asserta(pfcxform:pfc_file_api(SourceFile))))
   )).
:- endif.

%:- throw(module(pfc_umt,[umt/1])).

:- module(pfcxform,[
    op(500,fx,'~'),
    op(1050,xfx,('=>')),
    op(1050,xfx,'<=>'),
    op(1050,xfx,('==>')),
    op(1050,xfx,'<==>'),
    op(1050,xfx,('<-')),
    op(1050,xfx,('<=')),
    op(1100,fx,('==>')),
    op(1150,xfx,('::::')),
    reading_pfc_api_file/3,
    pfcapi_to_mpred/2,
    mp2pfc/2]).


%:- throw(module(pfcxform,[pfcapi_to_mpred/2])).


:- use_module(pfc_pack_umt).

show_if_changed(Type,MPRED,PFC):- ignore((MPRED\==PFC, dmsg(changed(Type,MPRED)-->PFC))).

:- multifile(pfcumt:pfcDatabaseTerm_DYN/1).
:- dynamic(pfcumt:pfcDatabaseTerm_DYN/1).

%:- throw(module(pfcumt,[umt/1])).

:- meta_predicate(pfcapi_to_mpred(*,*)).
pfcapi_to_mpred(I,I):-  I == end_of_file,fixup_exports,!.
% pfcapi_to_mpred(I,O):- var(I),!,O = umt(I).
pfcapi_to_mpred(I,I):- \+ compound(I),!.
pfcapi_to_mpred((:-dynamic(F/A)),(:- assert_if_new(pfcumt:pfcDatabaseTerm_DYN(FF/A)))):-  pfc_to_mp_f(F,A,FF),!.
pfcapi_to_mpred(PFC,MPRED):- \+ ground(PFC),must(mp2pfc(MPRED,PFC)),!.
pfcapi_to_mpred(PFC,MPRED):- must(mp2pfc(MPRED,PFC)),!.

:- multifile(pfcxform:pfc_file_api/1).
:- dynamic(pfcxform:pfc_file_api/1).

pfc_api_renamed(pfc,1,pfc_call).
pfc_api_renamed(add,_,pfc_add).
pfc_api_renamed(rem,_,pfc_rem).
pfc_api_renamed(rem2,_,pfc_rem2).
pfc_api_renamed(post,_,pfc_post).
pfc_api_renamed(post1,_,pfc_post1).
pfc_api_renamed(fc,1,pfc_fc).

pfc_api_renamed(pfcAtom,1,pfc_literal).
pfc_api_renamed(pfcNegatedAtom,1,pfc_negated_literal).
pfc_api_renamed(pfcPositiveAtom,1,pfc_positive_literal).

pfc_api_renamed(retract,_,retract_u).
pfc_api_renamed(retractall,_,retractall_u).
pfc_api_renamed(clause,_,clause_u).

pfc_api_renamed(assert,_,assert_u).
pfc_api_renamed(asserta,_,asserta_u).
pfc_api_renamed(assertz,_,assertz_u).

pfc_api_renamed(dynamic,1,kb_local).
pfc_api_renamed(multifile,1,kb_global).

pfc_api_renamed(call,1,umt).

pfc_api_renamed('not',_,'neg').
% pfc_api_renamed('~',_,'~').
pfc_api_renamed('=>',_,'==>').
pfc_api_renamed('<=>',_,'<==>').
pfc_api_renamed('<=',_,'<-').


pfc_to_mp_f(PFC,N,F):- pfc_api_renamed(PFC,N,F).
pfc_to_mp_f(F,_,F).


:- meta_predicate(mp2pfc(*,*)).
mp2pfc(I,O):- (atom(I);atom(O)),!, (pfc_api_renamed(I,atom,O)->true;I=O).
mp2pfc(I,I):- \+ compound(I),!.
mp2pfc('$VAR'(I),'$VAR'(I)):-!.
mp2pfc(M:A,M:AA):- !, mp2pfc(A,AA).
mp2pfc((A :- B),(AA :- BB)):- !,mp2pfc(A,AA),mp2pfc(B,BB).
mp2pfc(:-(A), :-(AA)):-  !, mp2pfc(A,AA). 
mp2pfc([A|B],[AA|BB]):- !, mp2pfc(A,AA),mp2pfc(B,BB).
mp2pfc(op(P,XFY,OP),op(P,XFY,OP)):- pfc_api_renamed(OP,op,OP2)->op(P,XFY,OP2),!.
mp2pfc(op(P,XFY,OP),op(P,XFY,OP)):- pfc_api_renamed(OP2,op,OP)->op(P,XFY,OP2),!.

mp2pfc(( \+ NV),( \+ NVA)):- (nonvar(NV);nonvar(NVA)), NV = '~'(A), NVA = not(AA), !, mp2pfc(A,AA).
% mp2pfc( '~'(NV),( \+ NVA)):- (nonvar(NV);nonvar(NVA)), NV = '~'(A), NVA = not(AA), !, mp2pfc(A,AA).

mp2pfc(PA,PAA):- compound(PAA),PAA=..[FF|AABB],!,functor(PAA,FF,A),pfc_to_mp_f(FF,A,F),mp2pfc_l(AABB,AB),PA=..[F|AB].
mp2pfc(CALL,CALL):- functor(CALL,call,_).
mp2pfc(PTA,PTAA):- PTA=..[t,F|Args],atom(F),mp2pfc_l(Args,ArgsO),PTAA=..[F|ArgsO].
mp2pfc(PA,PAA):- PA=..[F|AB],!, functor(PA,F,A),  pfc_to_mp_f(F,A,FF),mp2pfc_l(AB,AABB),PAA=..[FF|AABB].

mp2pfc_l(I,O):- must_maplist(mp2pfc,I,O).


reading_pfc_api_file(Type,I,O):- 
  source_location(File,_),
  pfcxform:pfc_file_api(File),
  pfcapi_to_mpred(I,O),
  !, 
  I\==O,
  show_if_changed(Type,I,O).



:- multifile(system:'term_expansion'/2).
system:term_expansion(I,O) :- reading_pfc_api_file(pfc_api_term_expansion,I,O).

%:- multifile(system:'goal_expansion'/2).
%system:goal_expansion(I,O) :- reading_pfc_api_file(pfc_api_goal_expansion,I,O).



