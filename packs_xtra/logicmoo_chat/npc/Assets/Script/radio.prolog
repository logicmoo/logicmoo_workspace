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

male(newcaster).
news_commentator(judy).
speaker_name(newcaster) --> [newcaster].
speaker_name(newcaster) --> [donald].


female(judy).
news_commentator(judy).
speaker_name(judy) --> [little, judy].
speaker_title(judy) --> [former, governor].
speaker_title(judy) --> [animatronic, markov, generator].

speaker_title(X) --> {news_commentator(X)}, [news, commentator].

:- randomizable crazy_thing/3, goodguys/4, heroic_act/4, sports_team/1.

crazy_thing(Speaker) --> goodguys(Speaker, Group), heroic_act(Speaker, Group).

goodguys(_, X) --> [X], { sports_team(X) }.

sports_team(seahawks).
sports_team(cowboys).

goodguys(_, scouts) --> ['SCOUTS'].
organization(scouts).

goodguys(_, parents) --> [the, parents].
organization(parents).

goodguys(_, doctors) --> [doctors].
goodguys(_, environmentalists) --> [environmentalists].
goodguys(judy, media) --> [the, news, media].
organization(media).

goodguys(newcaster, heros) --> [heros].
goodguys(judy, players) --> [players].
goodguys(judy, those_people) --> ['THOSE', people].

heroic_act(Speaker, Group) -->
   [are, really, secret],
   goodguy_members(Speaker, OtherGroup),
   { Group \= OtherGroup }.
heroic_act(_, _) --> [are, trying, to, help, us].


goodguy_members(Speaker, Group) --> [members, of], goodguys(Speaker, Group), { organization(Group) }.
goodguy_members(Speaker, Group) --> goodguys(Speaker, Group).