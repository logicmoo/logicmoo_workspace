%% File: dnf2fof.pl  -  Version: 1.8  -  Date: 1 March 2012
%%
%% Purpose: The transformation of Malecop DNF files into TPTP FOF files for a further usage.
%%
%% Authors: Jiri Vyskocil
%%
%% Usage: dnf2fof(I,O).% transforms dnf Malecop file I into TPTP fof file O.
%%

dnf2fof(Input_File,Output_File) :-
	read_file_to_list(Input_File,Ls),
	split_conjectures_axioms(Ls,Cs,As),
	dnf_to_one_fof(Cs,C),
	eliminate_sharp(C,NC),
	list_dnf2fof(As,FAs),
	close_fof(FAs,CAs),
	list_to_file([NC|CAs],Output_File).

%%%%%%
close_fof([],[]).
close_fof([fof(Id,Type,FOF)|Ls],[fof(Id,Type,CFOF)|Cs]) :-
        close_formula(FOF,CFOF),
	close_fof(Ls,Cs).

close_formula(FOF,CFOF) :-
        term_variables(FOF,Vars),
	(
	Vars = [] ->
	  CFOF=FOF
	;
	  CFOF=':'('!'(Vars),FOF)
	).

%%%%%%
eliminate_sharp(fof(Id,Type,FOF),fof(Id,Type,EFOF)) :- !,
	eliminate_sharp(FOF,EFOF).
% eliminate_sharp('#'(_),'$'(false)) :- !.
eliminate_sharp('|'('#'(_),T), NT) :- !,
	eliminate_sharp(T,NT).
eliminate_sharp('|'(T,'#'(_)), NT) :- !,
	eliminate_sharp(T,NT).
eliminate_sharp('&'('#'(_),T), NT) :- !,
	eliminate_sharp(T,NT).
eliminate_sharp('&'(T,'#'(_)), NT) :- !,
	eliminate_sharp(T,NT).
eliminate_sharp('|'(A,B), '|'(NA,NB)) :- !,
	eliminate_sharp(A,NA),
	eliminate_sharp(B,NB).
eliminate_sharp('&'(A,B), '&'(NA,NB)) :- !,
	eliminate_sharp(A,NA),
	eliminate_sharp(B,NB).
eliminate_sharp('~'(T), '~'(NT)) :- !,
	eliminate_sharp(T,NT).
eliminate_sharp(':'(Q,T), ':'(Q,NT)) :- !,
	eliminate_sharp(T,NT).
eliminate_sharp(X,X).

%%%%%%
dnf_to_one_fof(Ls,fof(Id,Type,FOF)) :-
	dnf_to_one_fof_(Ls,fof(Id,Type,FOF)).	

%%%%%%

dnf_to_one_fof_([dnf(Id,Type,DNF,_G,_X)],fof(Id,Type,'~'(FOF))) :-
	!, negated_list_to_closed_conjunction(DNF,FOF).
dnf_to_one_fof_([dnf(Id,Type,DNF,_G,_X)|Ls],fof(Id,Type,'|'('~'(FOF),RFOF))) :-
	negated_list_to_closed_conjunction(DNF,FOF),
	dnf_to_one_fof_(Ls,fof(_,_,RFOF)).

dnf_to_one_fof_([dnf(Id,Type,DNF,_G)],fof(Id,Type,'~'(FOF))) :-
	!, negated_list_to_closed_conjunction(DNF,FOF).
dnf_to_one_fof_([dnf(Id,Type,DNF,_G)|Ls],fof(Id,Type,'|'('~'(FOF),RFOF))) :-
	negated_list_to_closed_conjunction(DNF,FOF),
	dnf_to_one_fof_(Ls,fof_(_,_,RFOF)).

%%%%%%

list_to_closed_conjunction(DNF,CFOF) :-
	list_to_conjunction(DNF,FOF),
	close_formula(FOF,CFOF).
		
negated_list_to_closed_conjunction(DNF,CFOF) :-
	list_to_conjunction(DNF,FOF),
	close_formula('~'(FOF),CFOF).
		
%%%%%%

list_dnf2fof([],[]).
list_dnf2fof([dnf(Id,Type,DNF,_G,_X)|Ls],[fof(Id,Type,'~'(FOF))|Fs]) :-
	list_to_conjunction(DNF,FOF),
	list_dnf2fof(Ls,Fs).	
list_dnf2fof([dnf(Id,Type,DNF,_G)|Ls],[fof(Id,Type,'~'(FOF))|Fs]) :-
	list_to_conjunction(DNF,FOF),
	list_dnf2fof(Ls,Fs).	

%%%%%%

list_to_disjunction([],'$'(false)) :- !.
list_to_disjunction([LIT],TPTPLIT) :- !,
	literal_to_tptp(LIT,TPTPLIT).
list_to_disjunction([L|Ls],'|'(TL,TLs)) :- !,
	literal_to_tptp(L,TL),
	list_to_disjunction(Ls,TLs).

list_to_conjunction([],'$'(true)) :- !.
list_to_conjunction([LIT],TPTPLIT) :- !,
	literal_to_tptp(LIT,TPTPLIT).
list_to_conjunction([L|Ls],'&'(TL,TLs)) :- !,
	literal_to_tptp(L,TL),
	list_to_conjunction(Ls,TLs).

literal_to_tptp(-(T),'~'(T)) :- !.
literal_to_tptp(T,T) :- !.

%%%%%%

split_conjectures_axioms([],[],[]).
split_conjectures_axioms([dnf(Id,Type,DNF,G,X)|Ls],Cs,As) :-
	(
	Type=conjecture ->
		Cs = [dnf(Id,Type,DNF,G,X)|NCs], As=NAs
	;
		As = [dnf(Id,Type,DNF,G,X)|NAs], Cs=NCs
	),!,
	split_conjectures_axioms(Ls,NCs,NAs).

split_conjectures_axioms([dnf(Id,Type,DNF,G)|Ls],Cs,As) :-
	(
	Type=conjecture ->
		Cs = [dnf(Id,Type,DNF,G)|NCs], As=NAs
	;
		As = [dnf(Id,Type,DNF,G)|NAs], Cs=NCs
	),!,
	split_conjectures_axioms(Ls,NCs,NAs).

%%%%%%

list_to_file(List,File) :-
    open(File,write,Stream), 
    ( forall(member(L,List),(copy_term(L,C), numbervars(C,0,_), print(Stream,C),write(Stream,'.'),nl(Stream)))
    -> close(Stream) ; close(Stream), fail )
    .

portray('&'(X,Y)) :-
        print('('),
        print(X),
	print(' & '),
        print(Y),
	print(')').

portray('|'(X,Y)) :-
        print('('),
        print(X),
	print(' | '),
        print(Y),
	print(')').

portray('='(X,Y)) :-
        print('('),
        print(X),
	print(' = '),
        print(Y),
	print(')').

portray(':'(X,Y)) :-
        print('('),
        print(X),
	print(' : '),
        print(Y),
	print(')').

portray('~'(X)) :-
        print('(~ '),
        print(X),
	print(')').
%%%%%%

declare_TPTP_operators:-
    op(99,fx,'$'),
    op(100,fx,++),
    op(100,fx,--),
    op(100,xf,'!'),
    op(405,xfx,'='),
    op(405,xfx,'~='),
    op(450,fy,~),
    op(502,xfy,'|'),
    op(502,xfy,'~|'),
    op(503,xfy,&),
    op(503,xfy,~&),
    op(504,xfy,=>),
    op(504,xfy,<=),
    op(505,xfy,<=>),
    op(505,xfy,<~>),
%----! and ? are of higher precedence than : so !X:p(X) is :(!(X),p(X))
%----Otherwise !X:!Y:p(X,Y) cannot be parsed.
    op(400,fx,!),
    op(400,fx,?),
%----Need : stronger than + for equality and otter in tptp2X
%----Need : weaker than quantifiers for !X : ~p
    op(450,xfy,:),
%---- .. used for range in tptp2X. Needs to be stronger than :
    op(400,xfx,'..').

read_file_to_list(File,Ls) :- see(File),read_file_to_list_(Ls),seen.
read_file_to_list_(Ls) :-
	findall(T,(repeat,read(T),(T \== end_of_file -> true ; (!,fail))),Ls).

:-	system_mode(on).
:-	declare_TPTP_operators.
:-	system_mode(off).


