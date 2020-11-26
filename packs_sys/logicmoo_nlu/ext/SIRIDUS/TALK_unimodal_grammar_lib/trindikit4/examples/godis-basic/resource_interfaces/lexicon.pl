%Resource interface file for GoDiS lexica 

:- multifile is_resource_type/1,resource_relation/2, resource_relation_type/2.
:- discontiguous resource_relation/2, resource_relation_type/2.
is_resource_type(lexicon).

resource_relation( input_form, [Lexicon, Phrase, Move] ) :-
	Lexicon : input_form( Phrase, Move ).
resource_relation_type( input_form, [ lexicon, string, dmove ] ).

resource_relation( output_form, [Lexicon, Phrase, Move] ) :-
	Lexicon : output_form( Phrase, Move ).
resource_relation_type( output_form, [ lexicon, string, dmove ] ).

resource_relation( output_formcom, [Lexicon,Move,set(Com),Phrase] ):-
        Lexicon : output_form(Move,Com,Phrase).
resource_relation_type( output_form, [ lexicon,dmove,set(prop),string ] ).

resource_relation( yn_answer, [Lexicon, A] ) :-
	Lexicon : yn_answer( A ).
resource_relation_type( yn_answer, [ lexicon, answer ] ).