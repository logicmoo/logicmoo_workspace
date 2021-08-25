%********************  t_header_tree.pl  ********************

% Praedikate zur Verwaltung des Beweisterms.

%************************************************************

%%% p_trace( Type, ClausN, LitN, Node, SubTrace, Trace)
%%% Verzoegern, falls Variablen im SubTrace, sonst Fortschreibung des Traceterms

:- mode p_trace( ++, ++, ++, +, +, -).

p_trace( Type, ClausN, LitN, Node, SubTraceL, Trace) :-
	checklist( nonvar, SubTraceL),
	h_trace( Type, ClausN, LitN, Node, SubTraceL, Trace), !.

p_trace( Type, ClausN, LitN, Node, SubTraceL, Trace) :-
	u_delay( SubTraceL, p_trace(Type,ClausN,LitN,Node,SubTraceL,Trace)).


%%% h_trace( Type, ClausN, LitN, Node, SubTrace, Trace)
%%% Fortschreiben des Traceterms, dabei Abflachen des Baums, der aus der
%%% Verarbeitung der Praemissenliste entsteht.

:- mode h_trace( +, ++, ++, +, +, -).

% Theorieverarbeitung ohne Zwischenschritte: Loeschen des th_end Markers
h_trace( Type, ClausN, LitN, Node, [th_end|SubTrace], Trace) :-
	(Type == th_start ; Type == th_start_1 ; Type == th_fact ; 
         Type = sim(_,_)),
	h_get_node_info( Type, ClausN, LitN, Info),
	Trace = trc( Node, Info, SubTrace), !.

% Theorieverarbeitung mit Zwischenschritten:
% Loeschen des th_end Markers, Initialisierung des Liftterms
h_trace( Type, ClausN, LitN, Node, [th_end|SubTrace], Trace) :-
	h_get_node_info( Type, ClausN, LitN, Info),
        Trace = lift( trc( Node, Info, SubTrace), []), !.

% Theorieverarbeitung mit Zwischenschritten Abschluss:
% abgeflachten Baum zusammensetzen
h_trace( Type, ClausN, LitN, Node, [lift(Lift1,Lift2)|SubTrace], Trace):-	
	(Type == th_start ; Type == th_start_1 ; Type == th_fact ;
         Type == sim(_,_)),
	h_get_node_info( Type, ClausN, LitN, Info),
	flatten([[Lift1],Lift2,SubTrace], ThTrace),
	Trace = trc( Node, Info, ThTrace), !.

% Theorieverarbeitung Zwischenschritte Zwischenschritt:
% Markierung der zu hebenden Zweige mit lift(...)
h_trace( Type, ClausN, LitN, Node, [lift(Lift1,Lift2)|SubTrace], Trace):-
	h_get_node_info( Type, ClausN, LitN, Info),
	append( [Lift1], Lift2, Lift),
        Trace = lift( trc( Node, Info, SubTrace), Lift), !.

% Normalfall
h_trace( Type, ClausN, LitN, Node, SubTrace, Trace) :-
        h_get_node_info( Type, ClausN, LitN, Info),
        Trace = trc( Node, Info, SubTrace), !.

% Fehlerfall
h_trace( Type, ClausN, LitN, Node, SubTrace, _) :-
        h_get_node_info( Type, ClausN, LitN, Info),
	u_out( "\nWARNING: Trace incorrect! %w, node(%DQvw), subtrace(%w)",
                [Info,Node,SubTrace]), !.


%%% h_get_node_info( Type, ClausN, LitN, InfoTerm)
%%% Ermittelt den Nodeinfoterm

:- mode h_get_node_info( ++, +, +, -).

h_get_node_info( Type, ClausN, LitN, info(Inf,Type,ClausN,LitN)) :-
	getval( inf, Inf).


% ---------- Beweistermverarbeitung ----------

%%% h_write_tree( Trace)
%%% Schreibt den entsprechenden Beweisbaum

:- mode h_write_tree( +).

h_write_tree( Trace) :-
	def_const( problem, Problem),
	concat_atoms( Problem, '.tree', FileName),
	open( FileName, write, S),
	h_write_now( S, [Trace], ""),
	writeln( S, "."),
	close( S).

	
%%% h_write_now( Stream, Trace, Blanks)

:- mode h_write_now( ++, +, ++).
	
h_write_now( _, [], _).

h_write_now( S, [Term|TraceR], Blanks) :-
	Term = trc(Node,Info,SubTrace),
	(p_neg_shortlit( _, Node, NegNode) ; Node = NegNode),
	concat_strings( Blanks, "   ", SubBlanks),
	printf( S, "\n%wtrc(%DOQvw, %w, [", [Blanks,Node,Info]),
        h_write_now( S, SubTrace, SubBlanks),
        (TraceR = [] -> printf( S, "])", []); printf( S, "]),", [])),
	h_write_now( S, TraceR, Blanks).

% END t_header_tree.pl END

