sem_sort( telephone, domain ).
sem_sort( vcr, domain ).

				%actions
sem_sort( top, action ).
sem_sort( tp_phoneCall, action ).
sem_sort( tp_divertCall, action ).
sem_sort( tp_cancelDivert, action ).
sem_sort( tp_conferenceCall, action ).

sem_sort( change_language, action ).
sem_sort( change_domain, action ).

sem_sort( luis,name ).
sem_sort( juan,name ).	


isa(destination,name).
isa(divert_to,name).
isa(first_person,name).
isa(second_person,name).
