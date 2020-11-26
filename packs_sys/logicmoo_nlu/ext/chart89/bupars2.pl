% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% bupars2.pl [Chapter  5] The BUP bottom-up parser
% of Matsumoto, Tanaka, Hirakawa, Miyoshi, and Yasukawa 1983 and
% Matsumoto, Kiyono and Tanaka 1985 - note that the rule-set has
% been hand translated.
%
?- reconsult('examples.pl').
% ?- reconsult('lexicon.pl').
%
% the set of CF-PSG rules (DCG style format) used to derive BUP clauses,
% each rule immediately precedes its translation
%
% s   ---> np, vp.
%
np(Goal,NP,Parse,Words0,WordsN)  :-
	link(s,Goal),
	goal(vp,VP,Words0,Words1),
	s(Goal,s(NP,VP),Parse,Words1,WordsN).
%
% vp  ---> iv.
%
iv(Goal,IV,Parse,Words0,WordsN)  :-
	link(vp,Goal),
	vp(Goal,vp(IV),Parse,Words0,WordsN).
%
% np  ---> det, nb.
%
det(Goal,DET,Parse,Words0,WordsN)  :-
	link(np,Goal),
	goal(nb,NB,Words0,Words1),
	np(Goal,np(DET,NB),Parse,Words1,WordsN).
%
% nb  ---> nn.
%
nn(Goal,NN,Parse,Words0,WordsN)  :-
	link(nb,Goal),
	nb(Goal,nb(NN),Parse,Words0,WordsN).
%
% | nb  ---> nn, rel.
% |
% | nn(Goal,NN,Parse,Words0,WordsN)  :-
% |	link(nn,Goal),
% |	goal(rel,REL,Words0,Words1),
% |	nb(Goal,nb(NN,REL),Parse,Words1,WordsN).
% |
% | The rule and translation above are consistent with our standard
% | grammar, but the latter includes no left-recursion, and, since
% | this is a bottom-up parser, we wish to show its ability to handle
% | left recursion.  Hence the following recursive relative clause rule:
%
% np  ---> np, rel
%
np(Goal,NP,Parse,Words0,WordsN)  :-
	link(np,Goal),
	goal(rel,REL,Words0,Words1),
	np(Goal,np(NP,REL),Parse,Words1,WordsN).
%
% rel ---> wh, vp.
%
wh(Goal,WH,Parse,Words0,WordsN)  :-
	link(rel,Goal),
	goal(vp,VP,Words0,Words1),
	rel(Goal,rel(WH,VP),Parse,Words1,WordsN).
%
% vp  ---> tv, np.
%
tv(Goal,TV,Parse,Words0,WordsN)  :-
	link(vp,Goal),
	goal(np,NP,Words0,Words1),
	vp(Goal,vp(TV,NP),Parse,Words1,WordsN).
%
% vp  ---> dv, np, pp.
%
dv(Goal,DV,Parse,Words0,WordsN)  :-
	link(vp,Goal),
	goal(np,NP,Words0,Words1),
	goal(pp,PP,Words1,Words2),
	vp(Goal,vp(DV,NP,PP),Parse,Words2,WordsN).
%
% vp  ---> sv, s.
%
sv(Goal,SV,Parse,Words0,WordsN)  :-
	link(vp,Goal),
	goal(s,S,Words0,Words1),
	vp(Goal,vp(SV,S),Parse,Words1,WordsN).
%
% pp  ---> p, np.
%
p(Goal,P,Parse,Words0,WordsN)  :-
	link(pp,Goal),
	goal(np,NP,Words0,Words1),
	pp(Goal,pp(P,NP),Parse,Words1,WordsN).
%
%
% |goal(Goal,Parse2,[Word0|Words1],WordsN) :-
% |	word(Category,Word0),
% |	Parse1 =.. [Category,Word0],
% |	link(Category,Goal),
% |	Term =.. [Category,Goal,Parse1,Parse2,Words1,WordsN],
% |	call(Term).
% |
% | the predicate above is the simple version of 'goal' that does
% | not store intermediate results.  The following definition in
% | effect creates a "partial success/failure" table as it proceeds
%
goal(Goal,Parse2,Words0,WordsN) :-
	wf_goal(Goal,_,Words0,_), !,
	wf_goal(Goal,Parse2,Words0,WordsN);
	fail_goal(Goal,Words0), !,
	fail.
goal(Goal,Parse2,[Word0|Words1],WordsN) :-
	word(Category,Word0),
	Parse1 =.. [Category,Word0],
	link(Category,Goal),
	Term =.. [Category,Goal,Parse1,Parse2,Words1,WordsN],
	call(Term),
	assertz(wf_goal(Goal,Parse2,[Word0|Words1],WordsN)).
goal(Goal,Parse2,Words0,WordsN) :-
	(wf_goal(Goal,_,Words0,_);
	 assertz(fail_goal(Goal,Words0))),
	!,
	fail.
%
% the "terminate" clauses
%
s(s,Parse,Parse,String,String).
np(np,Parse,Parse,String,String).
nb(nb,Parse,Parse,String,String).
nn(nn,Parse,Parse,String,String).
det(det,Parse,Parse,String,String).
rel(rel,Parse,Parse,String,String).
wh(wh,Parse,Parse,String,String).
vp(vp,Parse,Parse,String,String).
iv(iv,Parse,Parse,String,String).
tv(tv,Parse,Parse,String,String).
dv(dv,Parse,Parse,String,String).
sv(sv,Parse,Parse,String,String).
pp(pp,Parse,Parse,String,String).
p(p,Parse,Parse,String,String).
%
% the "link" relation
link(np,s).
link(det,np).
link(det,s).				% link is transitive
link(nn,nb).
link(wh,rel).
link(iv,vp).
link(tv,vp).
link(dv,vp).
link(sv,vp).
link(p,pp).
link(Category,Category).		% link is reflexive

%
test(String) :-
	goal(s,Parse,String,[]),
	write(Parse),
	nl,
	listing(wf_goal),
	retractall(wf_goal(_,_,_,_)),
	listing(fail_goal),
	retractall(fail_goal(_,_)).
%
% An example lexicon
%
word(np,kim).
word(np,sandy).
word(np,lee).
word(np,bread).
word(det,a).
word(det,the).
word(det,her).
word(nn,consumer).
word(nn,duck).
word(nn,man).
word(nn,woman).
word(wh,who).
word(wh,that).
word(p,to).
word(iv,died).
word(iv,ate).
word(tv,ate).
word(tv,saw).
word(tv,gave).
word(dv,gave).
word(dv,handed).
word(sv,knew).
%
