% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% fsgrules.pl [Chapter  4] Feature augmented phrase structure rules
%
?- op(255,xfx,--->).
?- op(30,xfx,':').
%
initial(s).		% used by chart parsers
%
s	--->	np(person:P,number:N,sex:S,case:nominative),
		vp(person:P,number:N,sex:S,verb_form:tensed).
np(person:3,number:N,sex:S,case:_)  --->
		det(number:N),
		nb(number:N,sex:S).
np(person:3,number:plural,sex:S,case:_)  --->
		nb(number:plural,sex:S).
nb(number:N,sex:S)  --->
		nn(number:N,sex:S).
nb(number:N,sex:S)  --->
		nn(number:N,sex:S),
		rel(number:N,sex:S).
rel(number:N,sex:S)  --->
		wh(sex:S),
		vp(person:3,number:N,sex:S,verb_form:tensed).
vp(person:P,number:N,sex:_,verb_form:V)  --->
		iv(person:P,number:N,verb_form:V).
vp(person:P,number:N,sex:S,verb_form:V)  --->
		bv(person:P,number:N,verb_form:V),
		adj(number:N,sex:S).
vp(person:P,number:N,sex:S,verb_form:V)  --->
		bv(person:P,number:N,verb_form:V),
		np(person:_,number:N,sex:S,case:accusative).
vp(person:P,number:N,sex:_,verb_form:V)  --->
		tv(person:P,number:N,verb_form:V),
		np(person:_,number:_,sex:_,case:accusative).
vp(person:P1,number:N1,sex:S1,verb_form:V1)  --->
		xv(person:P1,number:N1,verb_form:V1),
		np(person:P2,number:N2,sex:S2,case:accusative),
		vp(person:P2,number:N2,sex:S2,verb_form:infinitive).
%
% Following rules handle unbounded dependencies in NP topicalization
% and relative clauses:
%
s	--->	np(person:P,number:N,sex:S,case:C),
		s(slash:np(person:P,number:N,sex:S,case:C)).
s(slash:XP)  --->
		np(person:P,number:N,sex:S,case:nominative),
		vp(person:P,number:N,sex:S,verb_form:tensed,slash:XP).
rel(number:N,sex:S)  --->
		wh(sex:S),
		s(slash:np(person:_,number:N,sex:S,case:_)).
vp(person:P,number:N,sex:S,verb_form:V,slash:np(person:_,
		number:N,sex:S,case:accusative))  --->
		bv(person:P,number:N,verb_form:V).
vp(person:P,number:N,sex:_,verb_form:V,slash:np(person:_,
		number:_,sex:_,case:accusative))  --->
		tv(person:P,number:N,verb_form:V).
vp(person:P1,number:N1,sex:S1,verb_form:V1,slash:np(person:P2,
		number:N2,sex:S2,case:accusative))  --->
		xv(person:P1,number:N1,verb_form:V1),
		vp(person:P2,number:N2,sex:S2,verb_form:infinitive).
