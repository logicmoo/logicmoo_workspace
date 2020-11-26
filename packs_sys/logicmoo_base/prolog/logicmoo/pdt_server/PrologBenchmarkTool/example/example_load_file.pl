query1 :-
   methodT(Method, _, _, _, _, _, _),
   getFieldT(Id, _, Method, 'null', _, _).
   
query2 :-
   methodT(Method, _, _, _, _, _, _),
   getFieldT(Id, _, Method, _, _, _),
   getFieldT(Id, _, Method, 'null', _, _).
   
query3 :-
   methodT(Method, _, _, _, _, _, _),
   getFieldT(_Id, _, Method, Recv, _, _),
   Recv == 'null'.
   
src_class(ClassId) :-
	classT(ClassId, _, _, _),
	not(externT(ClassId)).
	
members_of_list(List, Member) :-
	member(Member, List).
