% Grammar with a Holding Stack

% S may or may not begin with 'what did'.
% In the latter case 'what' is added to the stack
% before the NP and VP are parsed.

s(S) --> np(NP), vp(VP),
	{ S  = hold<-> (in<->H1..out<->H3),
	  NP = hold<-> (in<->H1..out<->H2),
	  VP = hold<-> (in<->H2..out<->H3) }.

s(S) --> [what, did], np(NP), vp(VP),
	{ S  = hold<-> (in<->H1..out<->H3),
	  NP = hold<-> (in<->[what|H1]..out<->H2),
	  VP = hold<-> (in<->H2..out<->H3) }.


% NP is parsed by either accepting det and n,
% leaving the hold stack unchanged, or else
% by extracting 'what' from the stack without
% accepting anything from the input list.

np(NP) --> det, n, { NP = hold<-> (in<->H..out<->H) }.

np(NP) --> [], { NP = hold<-> (in<->[what|H1]..out<->H1) }.

% VP consists of V followed by NP or S.
% Both hold<->in and hold<->out are the same
% on the VP as on the S or NP, since the
% hold stack can only be altered while
% processing the S or NP, not the verb.

vp(VP) --> v, np(NP), { VP = hold<->H,
			NP = hold<->H }.

vp(VP) --> v, s(S),   { VP = hold<->H,
			 S = hold<->H }.

% Lexicon

det --> [the];[a];[an].
n   --> [dog];[cat];[boy].
v   --> [said];[say];[chase];[chased].

try(X) :- writeln([X]),
	S = hold<-> (in<->[]..out<->[]),
	phrase(s(S),X,[]).

test1 :- try([the,boy,said,the,dog,chased,the,cat]).

test2 :- try([what,did,the,boy,say,chased,the,cat]).

test3 :- try([what,did,the,boy,say,the,cat,chased]).

test4 :- try([what,did,the,boy,say,the,dog,chased,the,cat]). % should fail

