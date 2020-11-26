/*----------------------------------------------------------------------
    sort_restr( +Prop )

    Prop fulfils the sortal restrictions on propositions
----------------------------------------------------------------------*/

% flavours
sem_sort( vanilla, flavour ).
sem_sort( strawberry, flavour ).
sem_sort( chocolate, flavour ).
sem_sort( mocha, flavour ).
sem_sort( hazelnut, flavour ).

% number

sem_sort( N, number ) :-
	integer(N).



/*--------------------
conceptual hierarichy
--------------------*/

isa( dummy, dummy ).