%Resource interface file for asr grammars
%Used in older version of godis. Specifies what speech recognition grammar
%the speech recognition client should load. The relation language/1 was called
%when the TTS was started to know which language to run. 



:- multifile is_resource_type/1,resource_relation/2, resource_relation_type/2,.
:- discontiguous resource_relation/2, resource_relation_type/2.

is_resource_type(asr_grammar).

/*----------------------------------------------------------------------
     asr_grammar
----------------------------------------------------------------------*/


resource_relation( language, [Grammar, L] ):-
        Grammar:language(L).
resource_of_type( language, [asr_grammar, atom] ).
