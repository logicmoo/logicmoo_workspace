% File to determine whether verbs are transitive are not.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

% VERB USAGE PATTERNS

% fr(synset_id,f_num,w_num).

%      The fr operator specifies a generic sentence frame for one or all
%      words in a synset. The operator is defined only for verbs.

%    s(synset_id,w_num,'word',ss_type,sense_number,tag_state).

% transitivity(+Verb,+Synset_ID,-Transitivities_List)
% given a verb with the sense given return how it may be
% used as a list of three items:
% [Intransitivity,Transitivity,Ditransitivity]
% where each is either one of 'intransitive','transitive', 'ditransitive' or 'no' 
% according to whether the verb in that sense can be used that way.

% The difference between transitivity1 and transitivity is that the former
% expects multiple words to appear like this: 'dry_clean' whereas the latter expects them
% to appear with spaces instead of hyphens (e.g., 'dry clean'). Another
% difference is that transitivity1/3 returns a list of 0-3 items whereas transitivity/3
% always returns a list of 3 items.

transitivity(Verb,Synset_ID,Transitivity_List) :-
	concat_atom(List_Of_Words,' ',Verb),                 % Separate out words separated by spaces
	concat_atom(List_Of_Words,'_',Hyphenated_Verb),      % Put the word back together using hyphens instead
	% reformat verb from words like 'dry clean' to a hyphenated form like 'dry_clean' used by the s/6 clauses in WordNet.
	transitivity1(Hyphenated_Verb,Synset_ID,Transitivities),
	% reformat results from a list of 0-3 items into a list of 3 items:
	(memberchk(intransitive,Transitivities) -> Intransitivity = intransitive; Intransitivity = no),
	(memberchk(transitive,Transitivities) -> Transitivity = transitive; Transitivity = no),
	(memberchk(ditransitive,Transitivities) -> Ditransitivity = ditransitive; Ditransitivity = no),
	Transitivity_List = [Intransitivity,Transitivity,Ditransitivity].
	
% transitivity1(+Hyphenated_Verb,+Synset_ID,-Transitivities)
% given a verb with the sense given return how it may be used
% as list of 0-3 items such as [intransitive], [transitive,ditransitive]
% or [intransitive,transitive,ditransitive], etc. It can
% be empty if the verb can only be used in ways that CELT does
% not support.

transitivity1(Hyphenated_Verb,Synset_ID,Transitivities) :-
	setof(Transitivity,get_transitivity(Hyphenated_Verb,Synset_ID,Transitivity),All_Usages),
	delete(All_Usages,outside_celt,Transitivities).

% get_transitivity sets Transitivity to one of the ways the
% verb can be used, one of [intransitivite, transitive, ditransitive].
% Zero or multiple answers may be returned.

get_transitivity(Hyphenated_Verb,Synset_ID,Transitivity) :-
	get_frame(Hyphenated_Verb,Synset_ID,Frame_number),
	frame(Frame_number, Example, Transitivity, Args).

% get_frame sets Frame_number to one of the model sentence frame numbers
% for the way the verb can be used. One or multiple answers may be returned.

get_frame(Hyphenated_Verb,Synset_ID,Frame_number) :-
	fr(Synset_ID,Frame_number,0).

get_frame(Hyphenated_Verb,Synset_ID,Frame_number) :-
	s(Synset_ID,Word_number,Hyphenated_Verb,v,Sense_number,Tag_state),
	fr(Synset_ID,Frame_number,Word_number).

frame(1,'Something ----s',intransitive,[something]).
frame(2,'Somebody ----s',intransitive,[somebody]).
frame(3,'It is ----ing',outside_celt,[]).
frame(4,'Something is ----ing PP',outside_celt,[something]).
frame(5,'Something ----s something Adjective/Noun',outside_celt,[something,something]).
frame(6,'Something ----s Adjective/Noun',outside_celt,[something]).
frame(7,'Somebody ----s Adjective',outside_celt,[somebody]).
frame(8,'Somebody ----s something',transitive,[somebody,something]).
frame(9,'Somebody ----s somebody',transitive,[somebody,somebody]).
frame(10,'Something ----s somebody',transitive,[something,somebody]).
frame(11,'Something ----s something',transitive,[something,something]).
frame(12,'Something ----s to somebody',outside_celt,[something,somebody]).
frame(13,'Somebody ----s on something',outside_celt,[somebody,something]).
frame(14,'Somebody ----s somebody something',ditransitive,[somebody,somebody,something]).
frame(15,'Somebody ----s something to somebody',ditransitive,[somebody,something,somebody]).
frame(16,'Somebody ----s something from somebody',transitive,[something,somebody]).
frame(17,'Somebody ----s somebody with something',transitive,[somebody,something]).
frame(18,'Somebody ----s somebody of something',transitive,[somebody,somebody]).
frame(19,'Somebody ----s something on somebody',transitive,[somebody,something]).
frame(20,'Somebody ----s somebody PP',transitive,[somebody,somebody]).
frame(21,'Somebody ----s something PP',transitive,[somebody,something]).
frame(22,'Somebody ----s PP',intransitive,[somebody]).
frame(23,'Somebody\'s (body part) ----s',intransitive,[something]).
frame(24,'Somebody ----s somebody to INFINITIVE',outside_celt,[somebody,somebody]). % 'to' only precedes indirect obj
frame(25,'Somebody ----s somebody INFINITIVE',outside_celt,[somebody,somebody]).
frame(26,'Somebody ----s that CLAUSE',outside_celt,[somebody]).
frame(27,'Somebody ----s to somebody',outside_celt,[somebody,somebody]). % Missing direct object
frame(28,'Somebody ----s to INFINITIVE',outside_celt,[somebody,somebody]). % 'to' only precedes indirect obj
frame(29,'Somebody ----s whether INFINITIVE',outside_celt,[somebody]). % No infinitives in CELT
frame(30,'Somebody ----s somebody into V-ing something',outside_celt,[somebody,somebody,something]). % No participles
frame(31,'Somebody ----s something with something',transitive,[somebody,something,something]).
frame(32,'Somebody ----s INFINITIVE',outside_celt,[somebody]). % No infinitives in CELT
frame(33,'Somebody ----s VERB-ing',outside_celt,[somebody]). % No participles in CELT
frame(34,'Somebody ----s that CLAUSE',outside_celt,[somebody]). % Relative sentences only modify noun phrases in CELT.
