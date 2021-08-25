%% (c) 1992, 1993 by KIT-BACK, TU Berlin. All rights reserved.

:- dynamic b5tf_trans/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                      %%
%%                  T R A N S L A T E   F I L E S                       %%
%%                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Export:

%% translate_file( + Filename, + Filename)
%% translate_file( + Filename, + Filename, + FileName)
%% translate( + Term, + Term/+TermList)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translate_file(X,Y) :-
	b5tf_translate_file(X,Y).

translate_file(X,Y,Z) :-
	b5tf_translate_file(X,Y,Z).

%% translate_file( filename1, filename2) translates the tells and ask from
%% filename1 into another syntactical variant and writes it into filename2.
%% translate_file( filename1, filename2, filename3) translates the content
%% of filename1 into filename2 using the translationrules specified in
%% filename3.

translate(X,Y) :-
	b5tf_translate(X,Y).

%% With translate( term1, term2) you can read in a translation rule.
%% Translations rules are applied to the topmost funktor and to subterms.
%% e.g. translate(tboxask(subsumes(X,Y)),Y <<< X)
%% and  translate(X androle Y, X and Y)
%% leads to tboxtell(subsumes(r1 androle r2,r3)) --> r3 <<< r1 and r2

%% You have to insert translation rules in the same order you would insert
%% prolog rules, because only the first matching rule is applied.
%% e.g. first     translate(aboxtell(X = Y), X :: Y).
%%      and then  translate(aboxtell(new X = Y),X :: Y).
%% leads to aboxtell(o :: c) --> o :: c
%% and      aboxtell(new o = c) --> new o :: c  
%% Inserting the rules in the opposite order the result is 'o :: c' in both
%% cases.

%% With the translation rule translate(term, termlist) one tell is changed
%% into a set of tells.
%% e g. translate(C :< D and not(E),[C :< D, disjoint(C,E)]).

b5tf_translate_file(FileName1,FileName2,RuleFileName) :-
	b5tf_init,
	b5tf_read(RuleFileName),              %% AENDERN IN BACKREAD
	b5tf_translate_file(FileName1,FileName2),
	b5tf_init.

b5tf_translate_file(FileName1,FileName2) :-
	see(FileName1),
	tell(FileName2),
	repeat,
	b5tf_translate_terms,
	!,
	seen,
	told.


b5tf_translate_terms :-
	read(Term),
	(Term == end_of_file,
	 !,
	 true;
	    b5tf_trans_term(Term),
	    fail).



b5tf_trans_term([]) :-
	!.

b5tf_trans_term([Term|TList]) :-
	!,
	b5tf_trans_term(Term),
	b5tf_trans_term(TList).

b5tf_trans_term(Term) :-
	b5tf_trans(Term,Term1),
	Term \== Term1,
	!,
	b5tf_trans_term(Term1).


b5tf_trans_term(Term) :-
	Term =.. [NewFunktor|Arguments],
	b5tf_trans_argument_list(Arguments,NewArguments),
	NewTerm =.. [NewFunktor|NewArguments],
	writeq(NewTerm),
	write('.'),
	nl.

b5tf_trans_argument(X,X) :-
	var(X),
	!.

b5tf_trans_argument(Term,Term2) :-
	b5tf_trans(Term,Term1),
	Term \== Term1,
	!,
	b5tf_trans_argument(Term1,Term2).


b5tf_trans_argument(Term,NewTerm) :-
	Term =.. [NewFunktor|Arguments],
	b5tf_trans_argument_list(Arguments,NewArguments),
	NewTerm =.. [NewFunktor|NewArguments].


b5tf_trans_argument_list([],[]).

b5tf_trans_argument_list([Arg|ArgList],[NewArg|NewArgList]) :-
	b5tf_trans_argument(Arg,NewArg),
	b5tf_trans_argument_list(ArgList,NewArgList).


b5tf_translate(X,Y) :-
	retract(b5tf_trans(X,X)),
	assertz((b5tf_trans(X,Y) :- !)),
	assertz(b5tf_trans(Z,Z)).


b5tf_init :-
	retractall(b5tf_trans(_,_)),
	asserta(b5tf_trans(X,X)).


b5tf_trans(X,X).


b5tf_read(FileName) :-
	see(FileName),
	repeat,
	read(X),
	(X == end_of_file,
	 !,
	 seen;
	    call(X),
	    fail).
