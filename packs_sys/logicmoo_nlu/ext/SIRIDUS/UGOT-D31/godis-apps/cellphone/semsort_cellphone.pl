/*************************************************************************

         name: semsort_cellphone.pl
      version: Feb, 2005
  description: Mini Mobile Phone semsort file
       author: Anna Olsson and Jessica Villing

*************************************************************************/
:- module( semsort_cellphone, [sem_sort/2, isa/2] ).

%%% Action
sem_sort( top, action ).
sem_sort( call, action).
sem_sort( call_name, action).
sem_sort( call_number, action).
sem_sort( phonebook, action ).
sem_sort( search_phonebook, action ).
sem_sort( add_new_entry, action ).
sem_sort( delete_entry, action ).
sem_sort( settings, action ).
sem_sort( telephone_settings, action).
sem_sort( change_language, action).
sem_sort( security_settings, action).
sem_sort( change_security_code, action).
sem_sort( reset, action).

%% Names
sem_sort(alex, name).
sem_sort(alida, name).
sem_sort(anna, name).
sem_sort(björn, name).
sem_sort(bo, name).
sem_sort(david, name).
sem_sort(edith, name).
sem_sort(elsa, name).
sem_sort(erik, name).
sem_sort(hildur, name).
sem_sort(hugo, name).
sem_sort(jan, name).
sem_sort(jessica, name).
sem_sort(john, name).
sem_sort(kalle, name). 
sem_sort(leif, name).
sem_sort(lisa, name).
sem_sort(maria, name).
sem_sort(mia, name).
sem_sort(olle, name).
sem_sort(oscar, name).
sem_sort(paul, name).
sem_sort(pelle, name). 
sem_sort(per, name).
sem_sort(peter, name).
sem_sort(robert, name).
sem_sort(robin, name). 
sem_sort(staffan, name). 
sem_sort(stina, name).
sem_sort(åsa, name).

%%% language lexicon
sem_sort(deutsch, language).
sem_sort(english, language).
sem_sort(espanol, language).
sem_sort(francais, language).
sem_sort(french, language).
sem_sort(gaeilge, language).
sem_sort(german, language).
sem_sort(irish, language).
sem_sort(spanish, language).
sem_sort(svenska, language).
sem_sort(swedish, language).

sem_sort('99', code).

isa( T0, T2 ) :-
	T0 \= T2,
	isa( T0, T1),
	isa( T1, T2 ).

%%% Numbers
sem_sort( N, number ) :-
	integer( N ).