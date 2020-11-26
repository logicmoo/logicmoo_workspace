/*----------------------------------------------------------------------
    sort_restr( +Prop )

    Prop fulfils the sortal restrictions on propositions
----------------------------------------------------------------------*/

sem_sort( malaria, disease ).
sem_sort( coccidioidomycosis, disease ).
sem_sort( schistosomiasis, disease ).

sem_sort('weakness',symptom).
sem_sort('diarrhea',symptom).
sem_sort('vomiting',symptom).
sem_sort('myalgia',symptom).
sem_sort('weight loss',symptom).
sem_sort('nausea',symptom).
sem_sort('lack of appetite',symptom).
sem_sort('cough',symptom).
sem_sort('dry cough',symptom).
sem_sort('influenzalike illness',symptom).
sem_sort('malaise',symptom).
sem_sort('arthralgia',symptom).
sem_sort('abdominal pain',symptom).
sem_sort('tiredness',symptom).
sem_sort('headache',symptom).
sem_sort('chills',symptom).

sem_sort('travel to desert region',medicalHistory).
sem_sort('travel to tropical climate',medicalHistory).
sem_sort('AIDS',medicalHistory).
sem_sort('exposure to dust',medicalHistory).
sem_sort('swimming in fresh water',medicalHistory).
sem_sort('travel outside US',medicalHistory).

sem_sort( fever, test ).
sem_sort( shaking, test ).
sem_sort( anemia, test ).
sem_sort( jaundice, test ).
sem_sort( rash, test ).
sem_sort( hematuria, test ).


%sem_sort( D, info ):-.

sem_sort( top, action ).
/*--------------------
conceptual hierarichy
--------------------*/

isa( info_disease, disease ).

%isa( dummy, dummy ).
