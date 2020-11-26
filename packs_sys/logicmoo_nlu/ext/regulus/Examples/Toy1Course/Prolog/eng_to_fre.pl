
% translation rules for course grammar

%  regulus_config(transfer_rules, toy1_course_prolog('eng_to_fre.pl')).

transfer_lexicon([utterance_type,command], [utterance_type,command]).

transfer_lexicon([entity, hotels], [entity, hotels]).

transfer_lexicon([action,list], [action,lister]).

transfer_rule([[entity, hotels]],[[entity,hotels],[def,yes]]).
