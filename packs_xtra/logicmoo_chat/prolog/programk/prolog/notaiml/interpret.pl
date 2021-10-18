:- module(interpret, [chatterbot/4]).

:- use_module(tokenize).
:- use_module(library(http/http_error)).

/*
catagory([], [], [], 'I have no response for that.').

%
%  pattern_match(+In, +Pattern, -Star)
%   determine if in matches pattern. If so, Star binds to the portion
%   matched by asterisks or [] (as a string)
pattern_match(In, Pattern, Star) :-
	name(In, Instring),
	name(Pattern, Pstring),
	pmatch(Instring, Pstring, [], Star).

%
%%   pmatch(+Instring, +PatternString,
% TODO this isn't handling setting the output
% % TODO think in detail about this whole thing
%

pmatch([] , [_], _, _) :- !,fail.
pmatch([] , [], _, _).
pmatch([] , [], _, _) :- !, fail.
pmatch([_], [], _, _) :- !,fail.
pmatch([HI|TI], [42|TP], StarSoFar, Out) :-
	pmatch(TI, TP, StarSoFar, Out).
pmatch(TI, [42|TP], StarSoFar, Out) :-
	pmatch(TI, [42|TP], StarSoFar, Out).
pmatch([HI|TI], [HP|TP], StarSoFar, Out) :-
	(HI is HP;
	 HI is HP+32;
	 HI+32 is HP),
	pmatch(TI, TP , StarSoFar, Out).
*/

% hard coded default catagory so we don't have to worry about match
% failing

category([nt('#$THAT'), star(0), nt('#$TOPIC'), star(0), nt('#$IN'), star(0)], [word('Replace'), word('the'),
		     word('default'), word('catagory'),special('.')], star(0)).

% match patterns per the spec
%
% Annie - note, NEVER write a header comment like that again!
% You've spent too much time debugging this because you weren't clear
% what it was supposed to do when you started.
% and the code's not clear
%
% Patt is the tokenized pattern
% In is the tokenized input
% PartStar is a reversed, partially accumulated * match
% InStars is a reversed list of stars
% Stars is the final list of * matches in reversed order
%
% Direct matches
pattern_match([word(A)|T], [word(A)|TA], [] , IS, Stars) :-
	pattern_match(T, TA, [], IS , Stars).
pattern_match([nt(A)|T], [nt(A)|TA], [] , IS, Stars) :-
	pattern_match(T, TA, [], IS , Stars).

% termination matches
pattern_match([], [], [], Stars, Stars).
pattern_match([], [], PartStar, [RP|Stars], Stars) :-
	reverse(PartStar, RP).

% star matches
% stop creeping
pattern_match([star(0)|T], Raw, PartStar, InStars, Stars)  :-
	reverse(PartStar, RP),
	pattern_match(T, Raw, [], [RP|InStars], Stars).
pattern_match(Pattern, [star(0)|TRaw], PartStar, InStars, Stars)  :-
	reverse(PartStar, RP),
	pattern_match(Pattern, TRaw, [], [RP|InStars], Stars).
% this handles star(0) on both sides without special case

% continue creeping
pattern_match([star(0)|T], [HA|TA], PartStar, InStars, Stars) :-
	pattern_match([star(0)|T], TA, [HA|PartStar], InStars, Stars).
pattern_match([word(_)|TP], [star(0)|TA], PartStar, InStars, Stars) :-
	pattern_match(TP, [star(0)|TA], PartStar, InStars, Stars).

expand_template([], _, []).
expand_template([star(0)|T], [Star|TStars], [Star|RT]) :-
	expand_template(T, TStars, RT).
expand_template([H|T], Stars, [H|RT]) :-
	expand_template(T, Stars, RT).

match(Tokens, memory(That, Topic), Response, NewTopic) :-
	append([nt('#$THAT')|That],
	       [nt('#$TOPIC')|Topic],A),
	append(A, [nt('#$IN')|Tokens], RawMatch),
	category(Pattern, Template, NewTopic),
	pattern_match(Pattern, RawMatch, [], [], Stars),
	reverse(Stars, OutStars),
	expand_template(Template, OutStars, Response).

chatterbot(memory(That, Topic), Intext, Response, NewTopic) :-
	token_stream_of(Intext, InTokens),
	% TODO preprocess token stream
	match(InTokens, memory(That, Topic), ResponseTokens, NewTopic),
       	detokenize(ResponseTokens , Response).
%	swritef(Response, 'InTokens is %q', [InTokens]),
%	NewTopic=Topic.  % temporary
/*	catagory(AnInPattern, AThatPattern, ATopicPattern, Template),
	pattern_match(Intext, AnInPattern, Star),
	pattern_match(That, AThatPattern),
	pattern_match(Topic, ATopicPattern),
	template_reduce(Template , Star, Response, NewTopic). */











