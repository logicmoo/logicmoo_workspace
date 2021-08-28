% ===================================================================
% PURPOSE
% This File is the bootstrap for the Sigma Infence engine For B-Prolog
% So first is loads the proper files and then starts up the system
% ===================================================================

% This file will be invoked from [new_tests_for_doug], go.

:-set_prolog_flag(singleton,off).


% SWI-Ops (just in case)
:-op(600,xfy,':').
:-op(600,fy,'multifile').
:-op(600,fy,'index').
 

% Prolog specific code choices
if_prolog(bp,G):-call(G).  % Run B-Prolog Specifics
if_prolog(_,_):-!.  % Dont run SWI Specificd or others

% used like if_prolog(bp,do_bp_stuff),if_prolog(swi,do_swi_stuff) inline in Sigma code

% ============================================
% Clone SWI-Prolog (non-ISO) builtins
% (many predicates do not need to do anything)
% ============================================

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.37.html
% TODO Port
get_time(6667). 

convert_time(_,'Now is the time (TODO Get from B-Prolog)').

convert_time(T,1,2,3,4,5,6,7).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.20.html#sigma_numbervars/4

sigma_numbervars(X,_,Y,Z):-sigma_numbervars(X,Y,Z).
  
% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.28.html#arithmetic_function/1

arithmetic_function(_).

% Gets statistical time that CPU has been used by prolog (Each prolog has their own)
getCputime(Time):-statistics(runtime,[Time|_]).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.13.html#flag/3

flag(Name,Out,In):-
	flag_db(Name,Out),
	set_the_flag(Name,In).

:-dynamic(in_flag_db/2).

flag_db(Name,Out):-in_flag_db(Name,Out),!.
flag_db(Name,0).

set_the_flag(Name,In):-
	retractAllProlog(in_flag_db(Name,_)),
	set_the_flag_1(Name,In).
	
set_the_flag_1(Name,V):-var(V).
set_the_flag_1(Name,X + Y):-Z is X + Y,!,
	     set_the_flag_1(Name,Z).
set_the_flag_1(Name,X - Y):-Z is X - Y,!,
	     set_the_flag_1(Name,Z).
set_the_flag_1(Name,V):-asserta(in_flag_db(Name,V)).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.3.html#make/0

make.

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.14.html#index/1

index(_).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.21.html#concat_atom/2
% TODO find optimal solution
concat_atom([],''):-!.
concat_atom([A],A):-!.
concat_atom([A|L],ALA):-!,
	concat_atom(L,AL),
	atom_codes(A,AC),
	atom_codes(AL,ALC),
	append(AC,ALC,ALO),!,
	atom_codes(ALA,ALO).
	       
% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.42.html#style_check/1

style_check(_).  

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.8.html#ignore/1

ignore(X):-once((call(X);true)).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.38.html#absolute_file_name/2

absolute_file_name(X,X).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.5.html#string/1

string(VAR):-var(VAR),!,fail.
string([I|_]):-integer(I).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.30.html#list_to_set/2

list_to_set(List1,List2):-sort(List1,List2).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.29.html#memberchk/2

memberchk(X,Y):-once(member(X,Y)).

% Using retractAllProlog/1 (in Sigma code) since Eclipse prolog (another port for Sigma)  chokes on retractall/1

retractAllProlog(X):-retractall(X).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.6.html#unify_with_occurs_check/2

unify_with_occurs_check(X,X).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.21.html#atom_to_term/3

atom_to_term(Atom,Term,Vars):-parse_atom(Atom,Term,Vars).

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.30.html#subtract/3

subtract([], _, []).
subtract([Head|Tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        subtract(Tail, L2, L3).
subtract([Head|Tail1], L2, [Head|Tail3]) :-
        subtract(Tail1, L2, Tail3).

% Each prolog has a specific way it could unnumber the result of a sigma_numbervars
% TODO find optimal solution

unnumbervars(Numbered,Unnumbered):-
	getPrologVars(Numbered,List,_,_),  % Defined in sigma_utility.pl
	recopy_each_var(Numbered,List,Unnumbered).

recopy_each_var(Unnumbered,[],Unnumbered).
recopy_each_var(Numbered,[V|List],Unnumbered):-
	ok_subst(Numbered,V,_NewVar,M),   % Defined in sigma_utility.pl
	recopy_each_var(M,List,Unnumbered),!.
		
% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.21.html#term_to_atom/2

term_to_atom(T,A):-term2atom(T,A).


% Defined in C in SWI

getCleanCharsWhitespaceProper([X],[]):-is_whitespace_code(X).
getCleanCharsWhitespaceProper([F|X],W):-is_whitespace_code(F),getCleanCharsWhitespaceProper(X,W).
getCleanCharsWhitespaceProper(X,X):-!.
string_clean(X,X).
is_whitespace_code(32).
is_whitespace_code(X):-X<32.


/*
term_to_atom(T,A):-
	open(term_to_atom,write,W,[type(binary)]),
	writeq(W,T),
	close(W,[force(true)]),!,
	open(term_to_atom,read,R),
	get_all_codes(R,Codes),
	atom_codes(A,Codes),
	close(R,[force(true)]).!.
	
get_all_codes(R,[]):-at_end_of_stream(R),!.
get_all_codes(R,[C|Odes]):-
	get_code(R,C),
	get_all_codes(R,Odes),!.
	
*/

% ========================================================================================
% Some prologs have a printf() type predicate.. so I made up fmtString/writeFmt in the Sigma code that calls the per-prolog mechaism
% in SWI it's formzat/N and sformat/N  .. need to find B-Prolog's version
% ========================================================================================

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.35.html#format/2

writeFmt(Format,Args):-!,current_output(Stream),writeFmt(Stream,Format,Args).

%http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.35.html#format/3

writeFmt(Loc,[],Args):-!.
writeFmt(user_error,F,Args):-!,
	writeFmt(user_output,F,Args).
writeFmt(Loc,Atom,Args):-atom(Atom),atom_codes(Atom,String),!,
	writeFmt(Loc,String,Args).
writeFmt(Loc,[126,110|Rest],Args):-!,nl(Loc),
	writeFmt(Loc,Rest,Args),!.
writeFmt(Loc,[126,N|Rest],[A|Rgs]):-!,
	wformat(Loc,N,A),!,
	writeFmt(Loc,Rest,Rgs),!.
writeFmt(Loc,[N|Rest],Rgs):-!,
	put_code(Loc,N),
	writeFmt(Loc,Rest,Rgs),!.
writeFmt(Loc,Rest,Rgs):-!,throw('fomat_error'(writeFmt(Loc,Rest,Rgs))).	

% http://www.swi.plsy.uva.nl/projects/SWI-Prolog/Manual/sec-3.35.html#sformat/3

fmtString(Format,Args):-!,current_output(Stream),fmtString(Stream,Format,Args).

fmtString([],[],Args):-!.
fmtString(Loc,Atom,Args):-atom(Atom),atom_codes(Atom,String),!,
	fmtString(Loc,String,Args).
fmtString([10|Loc],[126,110|Rest],Args):-!,nl(Loc),
	fmtString(Loc,Rest,Args),!.
fmtString(OLOc,[126,N|Rest],[A|Rgs]):-!,
	swformat(LocN,N,A),!,
	fmtString(Loc,Rest,Rgs),!,
	append(LocN,Loc,OLOc).
fmtString([N|Loc],[N|Rest],Rgs):-!,
	fmtString(Loc,Rest,Rgs),!.
fmtString(Loc,Rest,Rgs):-!,throw('fomat_error'(fmtString(Loc,Rest,Rgs))).	

wformat(Loc,_,C):-string(C),!,atom_codes(A,C),write(Loc,A).
wformat(Loc,119,A):-!,write(Loc,A). % ~w
wformat(Loc,113,A):-!,writeq(Loc,A). % ~q
wformat(Loc,103,A):-!,write(Loc,A). % ~g
wformat(Loc,N,A):-!,put_code(Loc,N),put_code(Loc,A),!.

swformat(A,_,A):-string(A),!.
swformat(C,_,A):-atom(A),!,atom_codes(A,C),!.
swformat(String,119,A):-!,term2string(A,String). % ~w	write/N
swformat(String,113,A):-!,term2string(A,String). % ~q	 writeq/N
swformat(String,103,A):-!,term2string(A,String). % ~g	 writes a 'real'
swformat([N,A],N,A):-!.

% =============================================
% Load the Sigma header and Engine 
% =============================================

:-['sigma_bootstrap.pl'].

% =============================================
% Sets Sigma Defaults (After intialization)
% =============================================

:-initializeSigmaServerData.

:-setSigmaOptionDefaults.

