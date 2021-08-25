% Discourse Representation Theory (adapted from Johnson & Klein 1986)

% unique_integer(N)
%   instantiates N to a different integer each time called.

unique_integer(N) :-
	retract(unique_aux(N)),
	!,
	NewN is N+1,
	asserta(unique_aux(NewN)).

:-dynamic(unique_aux/1).
unique_aux(0).

% Nouns
%   Each noun generates a unique index and inserts it, along with
%   a condition, into the DRS that is passed to it.

n(N) --> [man],
	   { unique_integer(C),
	     N = syn<->index<->C ..
	         sem<-> (in<-> [Current|Super] ..
		        out<-> [[C,man(C)|Current]|Super]) }.

n(N) --> [donkey],
	   { unique_integer(C),
	     N = syn<->index<->C ..
	         sem<-> (in<-> [Current|Super] ..
		         out<-> [[C,donkey(C)|Current]|Super]) }.

% Verbs
%   Each verb is linked to indices of its arguments through syntactic features.
%   Using these indices, it adds appropriate predicate to semantics.

v(V) --> [saw],
	   { V = syn<-> (arg1<->Arg1 .. arg2<->Arg2) ..
	         sem<-> (in<-> [Current|Super] ..
		         out<-> [[saw(Arg1,Arg2)|Current]|Super]) }.

% Determiners
%   Determiners tie together the semantics of their scope and restrictor.
%   The simplest determiner, 'a', simply passes semantic material to its
%   restrictor and then to its scope. A more complex determiner such as
%   'every' passes an empty list to its scope and restrictor, collects
%   whatever semantic material they add, and then arranges it into an if-then structure.

det(Det) --> [a],
	     { Det = sem<->res<->in<->A,   Det = sem<->in<->A,
	       Det = sem<->scope<->in<->B, Det = sem<->res<->out<->B,
	       Det = sem<->out<->C,      Det = sem<->scope<->out<->C}.

det(Det) --> [every],
	     { Det = sem<->res<->in<->[[]|A], Det = sem<->in<->A,
	       Det = sem<->scope<->in<->[[]|B], Det = sem<->res<->out<->B,
	       Det = sem<->scope<->out<->[Scope,Res|[Current|Super]],
	       Det = sem<->out<->[[Res>Scope|Current]|Super] }.

% Noun phrase
%     Pass semantic material to determiner, which will specify logical structure.

np(NP) --> { NP=sem<->A,      Det = sem<->A,
	     Det=sem<->res<->B, N=sem<->B,
	     NP=syn<->C,      N=syn<->C }, det(Det),n(N).

% Verb phrase
%     Pass semantic material to the embedded NP (the direct object).

vp(VP) --> { VP = sem<->A,       NP = sem<->A,
	     NP = sem<->scope<->B, V = sem<->B,
	     VP = syn<->arg2<->C,  NP = syn<->index<->C,
	     VP = syn<->D,       V = syn<->D }, v(V), np(NP).

% Sentence
%    Pass semantic material to the subject NP.
%    Pass VP semantics to the subject NP as its scope.

s(S) --> { S = sem<->A,          NP = sem<->A,
	   S = syn<->B,          VP = syn<->B,
	   NP = sem<->scope<->C,   VP = sem<->C,
	   VP = syn<->arg1<->D,    NP = syn<->index<->D }, np(NP), vp(VP).

% Procedure to parse and display a sentence

try(String) :- write(String), nl,
	Features = sem<->in<->[[]],    /* start with empty structure */
	phrase(s(Features),String),
	Features = sem<->out<->SemOut, /* extract what was built */
	display_feature_structure(SemOut).

% Example sentences

test1 :- try([a,man,saw,a,donkey]).

test2 :- try([a,donkey,saw,a,man]).

test3 :- try([every,man,saw,a,donkey]).

test4 :- try([every,man,saw,every,donkey]).

test5 :- try([a,man,saw,every,donkey]).

test6 :- try([a,donkey,saw,every,man]).



