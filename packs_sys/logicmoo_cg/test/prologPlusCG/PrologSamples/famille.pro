pere( martin, louis ).
pere( martin, julie ).
pere( martin, claire ).
pere( martin, marc ).
pere( louis, sebastien ).
pere( louis, pierre ).
pere( louis, isabelle ).
pere( jean_francois, pascal ).
pere( jean_francois, olivier ).
pere( paul, charles ).
pere( marc, patrick ).
pere( michel, steve ).

mere( nathalie , louis ).
mere( nathalie , julie ).
mere( nathalie , claire ).
mere( nathalie , marc ).
mere( helene, sebastien ).
mere( helene, pierre ).
mere( helene, isabelle ).
mere( julie , pascal ).
mere( julie , olivier ).
mere( isabelle , steve ).
mere( claire , charles).
mere( viviane, patrick ).

grandpere(X,Y) :-
	pere(X,Z),
	pere(Z,Y).

grandpere(X,Y) :-
	pere(X,Z),
	mere(Z,Y).


enfant(X,Y) :- pere(Y,X).

enfant(X,Y) :- mere(Y,X).

ascendant(X,Y) :- pere(X,Y).
ascendant(X,Y) :- mere(X,Y).

ascendant(X,Y) :- 
	pere(X,Z),
	ascendant(Z,Y).

ascendant(X,Y) :- 
	mere(X,Z),
	ascendant(Z,Y).

