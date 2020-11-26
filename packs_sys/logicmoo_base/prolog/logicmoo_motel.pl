/* <module> logicmoo_plarkc - special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers 
% 
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(logicmoo_motel,[]).
/*
:- module(logicmoo_motel,[kif_to_motelog/2]).            

:- reexport(library(logicmoo/motel/mpred_motel)).


kif_to_motelog(Kif,MLog):- as_mlog(Kif,Motel), translate(Motel,MLog).

:- sanity(dehyphenize_const('a-b','aB')).
:- must(dehyphenize_const('a-2b','a_2b')).
:- must(dehyphenize_const('uitype-ProductDescriptionTemplate','uitypeProductDescriptionTemplate')).

as_mlog(X,FmlO):- as_dlog(X,Fml),
  subsT_each(Fml,['=>'='implies','v'='or','&'='and','~'='not','<=>'='equivalent','all'='forall'],FmlM),
  as_mlog_pt2(FmlM,FmlO).

as_mlog_pt2(Fml,Fml):- \+ compound(Fml),!.
as_mlog_pt2(Fml,Fml):- is_ftVar(Fml),!.
as_mlog_pt2(Fml,FmlO):- Fml =..[F|List],maplist(as_mlog_pt2,List,ListO)->as_mlog_pt3(F,ListO,FmlO),!.

as_mlog_pt3(or,List,or(List)).
as_mlog_pt3(and,List,and(List)).
as_mlog_pt3(F,List,Fml):- Fml=..[F|List].
  
      
:- fixup_exports.

*/
