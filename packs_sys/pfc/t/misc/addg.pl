:- dynamic db/2.

% addg(P) adds P to database 0.

addg(P) :- addg(P,0).


% addg(+P,+DbName) adds P to a "global" database with name DbName.

addg(P,DbName) :-
  % the database exists already.
  pfc_get_database(DbName,DB),
  add(P,DB,NewDB),
  retract(pfc_db(DbName,DB)),
  assert(pfc_db(DbName,NewDB)).



% pfc_get_database(+Name,-DB) returns the term representing the
% database with name Name.  If it doesn't exisit, than an intial empty
% one is created.

pfc_get_database(Name,DB) :- 
  pfc_db(Name,DB),
  !.
pfc_get_database(Name,DB) :-
  % the databse does not yet exist.
  emptyDB(DB),
  assert(pfc_db(Name,DB)).


