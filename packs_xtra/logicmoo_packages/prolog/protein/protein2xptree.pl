%********************  protein2xptree.pl ********************

% Beweistermaufbereitung fuer xptree
% Es enthaelt die Haupteinsprungspunkte :
% - protein2xptree/1: 
%   Uebersetzung des Protein Beweisterms aus Problem.tree in das xptree Format
%   in Problem-xptree.tree und Aufruf von xptree

%************************************************************

% ---------- Compileranweisungen ----------

% Unterdrueckung von "loading library" Meldungen
:- set_error_handler( 139, true/0).

% ----------

:- module_interface( protein2xptree).

:- global protein2xptree / 1.

:- begin_module( protein2xptree).

:- use_module( t_literal).

:- op( 800, xfx, .).
:- op(   1,  fx, ~).


%%% protein2xptree( Problem)
%%% Oeffnet Datei, liest Prologterm und uebersetzt diesen in das xptree Format,
%%% das in einer weiteren Datei abgelegt wird.

:- mode protein2xptree( ++).

protein2xptree( Problem) :-
	concat_atoms( Problem, '.tree', InF),
        concat_atoms( Problem, '-xptree.tree', OutF),
	open( InF, read, InS),
	read( InS, Term),
	close( InS),
	open( OutF, write, OutS),
	writeln( OutS, "["),
	l_translate( OutS, [Term], ""),
	writeln( OutS, "]."),
	close( OutS),
	printf( "protein2xptree: wrote %w", [OutF]),
        concat_string( ["xptree ",Problem,"-xptree &"], Call),
	sh(Call).


%%% l_translate( Stream, Term, Blanks)
%%% Eigentliche rekursive Uebersetzung

:- mode l_translate( ++, +, ++).

l_translate( _, [], _).

l_translate( S, [trc(InNode,Info,SubTrace)|TraceR], Blanks) :-
	l_translate_lit( InNode, Node, NegNode),
	concat_strings( Blanks, "   ", SubBlanks),
	l_translate_info( Info, KZ, [InfN,InfoX]),
	(KZ == n, SubTrace == [] ->
	    printf( S, "%w[%DQvw, [%w, %.w]]", [Blanks,NegNode,InfN,InfoX])
        ;
	    printf( S, "%w[%DQvw, [%w, %.w], [%n", [Blanks,NegNode,InfN,InfoX]),
	    (KZ == y -> printf( S, "%w[%DQvw]", [SubBlanks,Node]); true),
            (SubTrace \== [], KZ == y -> writeln( S, ","); nl(S)),
            l_translate( S, SubTrace, SubBlanks),
	    printf( S, "%w]]", [Blanks])),   
        (TraceR == [] -> writeln( S, "\n") ; writeln( S, ",\n")),
	l_translate( S, TraceR, Blanks).


%%% l_translate_lit( InNode, OutNode, OutNegNode)
%%% Uebersetzt Literale

:- mode l_translate_lit( +, -, -).

l_translate_lit( InNode, OutNode, OutNegNode) :-
	(lit_test_neg( InNode) -> 
	    lit_neg( InNode, NegNode),
	    Node = ~NegNode
        ; 
            Node = InNode,
	    NegNode = ~Node),
	l_fnc_convert( Node, OutNode),
	l_fnc_convert( NegNode, OutNegNode).


%%% l_fnc_convert( InNode, OutNode)
%%% Benennt xptree unangenehme Praedikate um

:- mode l_fnc_convert( +, -).

l_fnc_convert( X, X) :- var(X).
l_fnc_convert( X, X) :- atomic(X).
l_fnc_convert( ~In, ~Out) :- l_fnc_convert( In, Out).

l_fnc_convert( X=Y, eq(U,V)) :- 
	l_fnc_convert( X, U),
	l_fnc_convert( Y, V).

l_fnc_convert( X:Y, sort(U,V)) :-
	l_fnc_convert( X, U),
	l_fnc_convert( Y, V).

l_fnc_convert(In, Out) :-
	In =.. [Fnc|InArgL],
	maplist( l_fnc_convert, InArgL, OutArgL),
	Out =.. [Fnc|OutArgL].


%%% l_translate_info( InInfo, KZ, OutInfo)
%%% Uebersetzt Infoterm

:- mode l_translate_info( ++, -, ?).

l_translate_info( info(Inf,restart,ClausN,LitN),n,[Inf,ext__(0.0,ClausN.LitN)]).

l_translate_info( info(Inf,Type,ClausN,LitN), y, 
                  [Inf, ext__(0.0,ClausN.LitN)]) :-
        (Type == fact ; Type == ext ; Type == query).

l_translate_info( info(Inf,Type,ClausN,LitN), y, 
                  [Inf, ext__(1.0,ClausN.LitN)]) :-
         (Type == th_fact ; Type == th_start ; Type == th_start_1 ; 
          Type == th_ext).

l_translate_info( info(Inf,red,N,_),           n, [Inf, red__(0.0,N)]).
l_translate_info( info(Inf,red_cut,N,_),       n, [Inf, red__(0.1,N)]).
l_translate_info( info(Inf,th_red,_,_),        n, [Inf, red__(1.0,0)]).
l_translate_info( info(Inf,th_red_cut,_,_),    n, [Inf, red__(1.1,0)]).
l_translate_info( info(Inf,factor,N,_),        n, [Inf, fac__(0.0,N)]).
l_translate_info( info(Inf,factor_cut,N,_),    n, [Inf, fac__(0.1,N)]).
l_translate_info( info(Inf,th_factor,_,_),     n, [Inf, fac__(1.0,0)]).
l_translate_info( info(Inf,th_factor_cut,_,_), n, [Inf, fac__(1.1,0)]).
l_translate_info( info(Inf,_,_,_),             n, [Inf, built_in]).

