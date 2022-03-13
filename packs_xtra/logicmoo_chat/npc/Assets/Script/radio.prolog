%%%%
%%%% Random paranoid talk-radio generator
%%%%

:- public headline/1.



%=autodoc
%% headline( +String) is semidet.
%
% Headline.
%
headline(String) :-
   randomize(headline(Words, [])),
   word_list(String, Words).



%=autodoc
%% headline is semidet.
%
% Headline.
%
headline --> opt_intro, headline_body.

:- randomizable opt_intro/2, intro_phrase/2.



%=autodoc
%% opt_intro is semidet.
%
% Opt Intro.
%
opt_intro --> [].
opt_intro-->intro_phrase, theTextM1(:).



%=autodoc
%% intro_phrase is semidet.
%
% Intro Phrase.
%
intro_phrase-->theTextM([and, this, just, in]).
intro_phrase-->theTextM([breaking, news, :]).
intro_phrase-->theTextM([on, a, lighter, note]).

:- randomizable headline_body/2, soundbite/2, said/3.



%=autodoc
%% headline_body is semidet.
%
% Headline Body.
%
headline_body --> soundbite.



%=autodoc
%% soundbite is semidet.
%
% Soundbite.
%
soundbite --> speaker(X), said(X), crazy_thing(X).



%=autodoc
%% said( ?ARG1) is semidet.
%
% Said.
%
said(_)-->theTextM1(said).
said(_)-->theTextM1(decreed).
said(_)-->theTextM1(opined).
said(_)-->theTextM1(claimed).
said(X) -->  
  theTextM([raised, eyebrows, when]), she(X), said(X).



%=autodoc
%% she( ?ARG1) is semidet.
%
% She.
%
she(X)-->{male(X)}, theTextM1(he).
she(X)-->{female(X)}, theTextM1(she).
she(_)-->theTextM1(it).

:- randomizable speaker/3, speaker_name/3, speaker_title/3.


%=autodoc
%% speaker( ?ARG1) is semidet.
%
% Speaker.
%
speaker(X) --> speaker_title(X), speaker_name(X).



%=autodoc
%% male( ?Newcaster1) is semidet.
%
% Male.
%
male(newcaster).


%=autodoc
%% news_commentator( ?Judy1) is semidet.
%
% News Commentator.
%
news_commentator(little_sophia).


%=autodoc
%% speaker_name( ?ARG1) is semidet.
%
% Speaker Name.
%
speaker_name(newcaster)-->theTextM1(newcaster).
speaker_name(newcaster)-->theTextM1(donald).




%=autodoc
%% female( ?Judy1) is semidet.
%
% Female.
%
female(little_sophia).
news_commentator(little_sophia).
speaker_name(little_sophia)-->theTextM([little, sophia]).


%=autodoc
%% speaker_title( ?ARG1) is semidet.
%
% Speaker Title.
%
speaker_title(little_sophia)-->theTextM([former, infant]).
speaker_title(little_sophia)-->theTextM([animatronic, markov, generator]).

speaker_title(X)-->{news_commentator(X)}, theTextM([news, commentator]).

:- randomizable crazy_thing/3, goodguys/4, heroic_act/4, sports_team/1.



%=autodoc
%% crazy_thing( ?ARG1) is semidet.
%
% Crazy Thing.
%
crazy_thing(Speaker) --> goodguys(Speaker, Group), heroic_act(Speaker, Group).



%=autodoc
%% goodguys( ?ARG1, ?ARG2) is semidet.
%
% Goodguys.
%
goodguys(_, X)-->theTextM1(X), {sports_team(X)}.



%=autodoc
%% sports_team( ?Cowboys1) is semidet.
%
% Sports Team.
%
sports_team(seahawks).
sports_team(cowboys).

goodguys(_, scouts)-->theTextM1('SCOUTS').


%=autodoc
%% organization( ?Media1) is semidet.
%
% Organization.
%
organization(scouts).

goodguys(_, parents)-->theTextM([the, parents]).
organization(parents).

goodguys(_, doctors)-->theTextM1(doctors).
goodguys(_, environmentalists)-->theTextM1(environmentalists).
goodguys(little_sophia, media)-->theTextM([the, news, media]).
organization(media).

goodguys(newcaster, heros)-->theTextM1(heros).
goodguys(little_sophia, users)-->theTextM1(users).
goodguys(little_sophia, those_people)-->theTextM(['THOSE', people]).



%=autodoc
%% heroic_act( ?ARG1, ?ARG2) is semidet.
%
% Heroic Single Doer Action.
%
heroic_act(Speaker, Group) -->  
  ( theTextM([are, really, secret])  ,
    goodguy_members(Speaker, OtherGroup), 
    {Group\=OtherGroup}).
heroic_act(_, _) -->  
  theTextM([are, trying, to, help, us]).




%=autodoc
%% goodguy_members( ?ARG1, ?ARG2) is semidet.
%
% Goodguy Members.
%
goodguy_members(Speaker, Group) -->  
  ( theTextM([members, of])  ,
    goodguys(Speaker, Group), 
    {organization(Group)}).
goodguy_members(Speaker, Group) --> goodguys(Speaker, Group).