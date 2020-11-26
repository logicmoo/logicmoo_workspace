
:- use_module(library(logicmoo_cg)).


cg_graph_value(N,V):- cg(N,Data),cg_formate(Data,V).

cg_formate(Data,V):- 
     findall(Member,cg_member(Member,Data),Members),
     unnumbervars(Members,UMembers),!,
     exclude(formed,UMembers,V).

formed(frame_var(_, _)).
formed(cg_type(_, _)).
formed(cg_name(Var, Value)):- =(Var, Value).
formed(cg_equal(Var, Value)):- =(Var, Value).
% formed(cg_values(E, List)):- member(E,List).
%formed(_).

cg_member(E,named_graph(_,Data)):- !, cg_member(E,Data).
cg_member(E,Data):- is_list(Data),!, member(ED,Data),cg_member(E,ED).
cg_member(E,E).


id_to_info(ID,(Label + List)):- cgc(ID,simple,Label,_,_),!,
   findall(Text,(cg(_,CGRS, _, _),member(cgr(Rel, Args, _),CGRS),
          member(ID,Args),id_to_text(cgr(Rel, Args, _),Text)),List).
id_to_info(ID,Label):- cg(ID, Rels, More, Data), maplist(id_to_text,Rels,Labels), append([Labels,[more=More],Data],Label),!.
id_to_info(ID,complex(Act,MoveBlock)):- 
   cgc(ID,complex,Act,_,[fs(name,MoveBlock)|_]),!.



id_to_info(ID,C):- id_to_info1(ID,C) *-> true ; id_to_info_all(ID,C).
   
id_to_info1(ID,C):-
        member(C,[cg(ID,_,_,_),reldef(ID,_,_),cgc(ID,_,_,_,_),typedef(ID,_,_), findrels(ID,_Cid,_,_)]), % ,findrels(_,ID,_,_)
        call(C),!.                                                                     
id_to_info_all(FID,C):-
        member(C,[cg(ID,_,_,_),reldef(ID,_,_),cgc(ID,_,_,_,_),typedef(ID,_,_), findrels(ID,_Cid,_,_)]), % ,findrels(_,ID,_,_)
        call(C),contains_var(FID,C).

% id_to_text(ID,cg(ID)):- cg(ID, _Rels, _More, _Data),!.


id_to_text(ID,Label):- cgc(ID, simple, Label,_,_),!.
id_to_text(ID,complex(Act,MoveBlock)):-  cgc(ID,complex,Act,_,[fs(name,MoveBlock)|_]),!.
id_to_text(ID,cg(Act,MoveBlock)):- cg(ID, _Rels, _More, _Data), cgc(_,complex,Act,[ID],[fs(name,MoveBlock)|_]),!.
id_to_text(ID,cg(ID)):- cg(ID, _Rels, _More, _Data),!.
id_to_text(cgr(Rel, Args, _),Label):-  maplist(id_to_text,Args,Labels),!, Label =.. [Rel|Labels].
id_to_text(ID,LabelOut):- id_to_info(ID,C), id_to_txtra(ID,C,Label), nonvar(Label),Label=LabelOut.

id_to_txtra(_ID,C,Label):-  sub_term(Sub,C),compound(Sub), Sub = fs(name, Label),!.
id_to_txtra(ID,C,Label):-  C = cgc(ID, _, Label, _, _),!.
id_to_txtra(ID,C,Label):-  C = cg(ID, [CGR|_], _, _),
  CGR = cgr(Rel, Args, _), maplist(id_to_text,Args,Labels), Label =.. [Rel|Labels].


id_to_infos(ID,Set):- findall(Label,id_to_text(ID,Label),Ls1),findall(Label,id_to_info(ID,Label),Ls2),findall(C,id_to_info_all(ID,C),_Ls3),
  append([Ls1,Ls2],Ls),!,list_to_set(Ls,Set).


id_type(ID,C):- 
       (isCG(ID),C=cg;
	isTypeDefGraph(ID),C=typedef;
	isRelDefGraph(ID),C=reldef;
        isConcept(ID),C=concept),!.


:- fixup_exports.



