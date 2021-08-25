parse(S) :- phrase(sent(Features),S),writeln(S),display_feature_structure(Features).
generate :- phrase(sent(Features),S), writeln(S),fail.

sent(num<->X) --> np(case<->nom..num<->X), vp(num<->X).

vp(num<->X) --> verb(subcat<->1..num<->X). % for verbs without objects
vp(num<->X) --> verb(subcat<->2..num<->X),np(case<->acc). % for verbs with objects

np(num<->X) --> det(num<->X), noun(num<->X).
np(case<->C..number<->X) --> pronoun(case<->C..number<->X).

det(num<->sg) --> [a].
det(num<->pl) --> [two].
det(num<->X) --> [the]. % both singular and plural

pronoun(num<->sg) --> [it]. % both nom and acc case.
pronoun(case<->nom..num<->pl) --> [they].
pronoun(case<->acc..num<->pl) --> [them].

noun(num<->sg) --> [dog].
noun(num<->pl) --> [dogs].

% intransitive verbs
verb(num<->sg..subcat<->1) --> [barks].
verb(num<->pl..subcat<->1) --> [bark].

% transitive verbs
verb(num<->sg..subcat<->2) --> [scares].
verb(num<->pl..subcat<->2) --> [scare].
