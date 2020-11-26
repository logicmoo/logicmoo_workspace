%Resource interface file for GoDiS databases. 


:- multifile is_resource_type/1,resource_relation/2, resource_relation_type/2.
:- discontiguous resource_relation/2, resource_relation_type/2.

is_resource_type(database).
/*----------------------------------------------------------------------
     database
----------------------------------------------------------------------*/

resource_relation( consultDB, [Database, Query, PropSet, Answer] ) :-
	Database : consultDB( Query, PropSet, Answer ), !.
%resource_relation( consultDB, [_, X^Q, _, notexist(Q) ] ).
resource_relation( consultDB, [_, X^Q, _, notexist(X,Q) ] ).
resource_relation( consultDB, [_, Q, _, unknown(Q) ] ).
resource_relation_type( consultDB, [database, question, set(prop), answer]).

resource_relation( consultDBx, [Database, Query, PropSet, AnswerSet] ) :-
	Database : consultDBx( Query, PropSet, AnswerSet ), !.
resource_relation( consultDBx, [_, X^Q, _, set([notexist(X,Q)]) ] ).
resource_relation( consultDBx, [_, Q, _, set([unknown(Q)])] ).
resource_relation_type( consultDB, [database, question, set(prop), answer]).


resource_relation( validDBparameter, [Database, Prop ] ) :-
	Database : validDBparameter( Prop ), !.
resource_relation_type( validDBparameter, [database, prop]).
