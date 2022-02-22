%%%%
%%%% Random paranoid talk-radio generator
%%%%

:- public headline/1.

headline(String) :-
   randomize(headline(Words, [])),
   word_list(String, Words).

headline --> opt_intro, headline_body.

:- randomizable opt_intro/2, intro_phrase/2.

opt_intro --> [].
opt_intro --> intro_phrase, [':'].

intro_phrase --> [and, this, just, in].
intro_phrase --> [breaking, news, ':'].
intro_phrase --> [on, a, lighter, note].

:- randomizable headline_body/2, soundbite/2, said/3.

headline_body --> soundbite.

soundbite --> speaker(X), said(X), crazy_thing(X).

said(_) --> [said].
said(_) --> [decreed].
said(_) --> [opined].
said(_) --> [claimed].
said(X) --> [raised, eyebrows, when], she(X), said(X).

she(X) --> {male(X)}, [he].
she(X) --> {female(X)}, [she].
she(_) --> [it].

:- randomizable speaker/3, speaker_name/3, speaker_title/3.
speaker(X) --> speaker_title(X), speaker_name(X).

male(trump).
fox_commentator(palin).
speaker_name(trump) --> [donald, trump].
speaker_name(trump) --> [trump].
speaker_name(trump) --> [donald].

speaker_title(trump) --> ['god-emperor'].
speaker_title(trump) --> ['president-for-life'].
speaker_title(trump) --> [pope].

female(palin).
fox_commentator(palin).
speaker_name(palin) --> [sarah, palin].
speaker_title(palin) --> [former, governor].
speaker_title(palin) --> [animatronic, markov, generator].

speaker_title(X) --> {fox_commentator(X)}, [fox, commentator].

:- randomizable crazy_thing/3, badguys/4, unforgivable_sin/4, immigrant_group/1.

crazy_thing(Speaker) --> badguys(Speaker, Group), unforgivable_sin(Speaker, Group).

badguys(_, X) --> [X], { immigrant_group(X) }.

immigrant_group(mexicans).
immigrant_group(aliens).

badguys(_, isis) --> ['ISIS'].
organization(isis).

badguys(_, planned_parenthood) --> [planned, parenthood].
organization(planned_parenthood).

badguys(_, socialists) --> [socialists].
badguys(_, bankers) --> [bankers].
badguys(palin, media) --> [the, lamestream, media].
organization(media).

badguys(trump, losers) --> [losers].
badguys(palin, atheists) --> [atheists].
badguys(palin, those_people) --> ['THOSE', people].

unforgivable_sin(Speaker, Group) -->
   [are, really, secret],
   badguy_members(Speaker, OtherGroup),
   { Group \= OtherGroup }.
unforgivable_sin(_, _) --> [are, trying, to, steal, our, precious, bodily, fluids].


badguy_members(Speaker, Group) --> [members, of], badguys(Speaker, Group), { organization(Group) }.
badguy_members(Speaker, Group) --> badguys(Speaker, Group).