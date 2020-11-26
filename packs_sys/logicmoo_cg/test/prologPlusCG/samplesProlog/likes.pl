
// Illustre le retour-arrière (backtracking)
// ?- likes(sam,dahl).
// ?- likes(sam,chop_suey).
// ?- likes(sam,pizza).
// ?- likes(sam,chips).
// ?- likes(sam,curry).

likes(sam,X_Food) :-
        indian(X_Food),
        mild(X_Food).
likes(sam,X_Food) :-
        chinese(X_Food).
likes(sam,X_Food) :-
        italian(X_Food).
likes(sam,chips).

indian(curry).
indian(dahl).
indian(tandoori).
indian(kurma).

mild(dahl).
mild(tandoori).
mild(kurma).

chinese(chow_mein).
chinese(chop_suey).
chinese(sweet_and_sour).

italian(pizza).
italian(spaghetti).
