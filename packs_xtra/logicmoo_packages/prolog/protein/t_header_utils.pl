%********************  t_header_utils.pl ********************

% Utility-Praedikate, die waehrend des Beweisens gebraucht werden. 

%************************************************************

%%% p_unifiable( Element, List)
%%% Element unifizierbar mit einem Element der Liste? 

:- mode p_unifiable( +, +).

p_unifiable( El, [El|_]).
p_unifiable( El, [_,El|_]).
p_unifiable( El, [_,_,El|_]).
p_unifiable( El, [_,_,_|R]) :- p_unifiable( El, R).


%%% p_unifiable_n_count( Element, List, Index)
%%% Element unifizierbat mit dem wievielten Element der Liste?

:- mode p_unifiable_n_count( +, +, ?).

p_unifiable_n_count( El, [El|_], 1).
p_unifiable_n_count( El, [_,El|_], 2).
p_unifiable_n_count( El, [_,_,El|_], 3).

p_unifiable_n_count( El, [_,_,_|R], N) :-
	p_unifiable_n_count( El, R, M),
	N is M + 3.


%%% p_identical( Element, List)
%%% Element identisch eines Elements der Liste?

:- mode p_identical( +, +).

p_identical( El, [X|_])     :- El == X, !.
p_identical( El, [_,X|_])   :- El == X, !.
p_identical( El, [_,_,X|_]) :- El == X, !.
p_identical( El, [_,_,_|R]) :- p_identical( El, R), !.


%%% p_identical_n_count( Element, Liste, Index)
%%% Element identisch des wievielten Elements der Liste?

:- mode p_identical_n_count( +, +, -).

p_identical_n_count( El, [X|_], 1)     :- El == X, !.
p_identical_n_count( El, [_,X|_], 2)   :- El == X, !.
p_identical_n_count( El, [_,_,X|_], 3) :- El == X, !.

p_identical_n_count( El, [_,_,_|R], N) :- 
	p_identical_n_count( El, R, M), 
	N is M + 3, !.


%%% p_identical_upto_restart( Element, List)
%%% Element identisch eines Elements der Liste bis zum letzten Restart?

:- mode p_identical_upto_restart( +, +).

p_identical_upto_restart( _, [restart|_])       :- !, fail.
p_identical_upto_restart( El, [X|_])            :- El == X, !.
p_identical_upto_restart( _, [_,restart|_])     :- !, fail.
p_identical_upto_restart( El, [_,X|_])          :- El == X, !.
p_identical_upto_restart( _, [_,_,restart|_])   :- !, fail.
p_identical_upto_restart( El, [_,_,X|_])        :- El == X, !. 
p_identical_upto_restart( El, [_,_,_|R]) :- p_identical_upto_restart( El, R), !.


%%% p_never_identical( Element, Liste)
%%% Element nie identisch eines Elements der Liste?

:- mode p_never_identical( +, +).

p_never_identical( _, []).

p_never_identical( El, [X|R]) :-
	!, El ~= X,
        p_never_identical( El, R).


%%% p_never_identical_upto_restart( Element, AncL)
%%% Elememt nie identisch eines Elements der Liste bis zum letzten Restart?

:- mode p_never_identical_upto_restart( +, +).

p_never_identical_upto_restart( _, []).
p_never_identical_upto_restart( _, [restart|_]) :- !.

p_never_identical_upto_restart( El, [X|R]) :-
	!, El ~= X,
        p_never_identical_upto_restart( El, R).


%%% u_rewrite_list( Flag, SimLit, RewriteL, InCondL, InLitL, OutLitL, OutCondL)
%%% Wendet p_rewrite auf jedes Literal der InLitL an

:- mode u_rewrite_list( ++, +, +, +, ?, -, -).

u_rewrite_list( _, _, _, _, [], [], []) :- !.

u_rewrite_list( Flag, SimTerm, RewriteL, InCondL, [InTerm|InTermR], 
           [OutTerm|OutTermR], [OutCondL|OutCondR]) :- !,
        p_rewrite( Flag, SimTerm, RewriteL, InCondL, InTerm, 
                   OutTerm, OutCondL),
        u_rewrite_list( Flag, SimTerm, RewriteL, InCondL, InTermR, 
                   OutTermR, OutCondR).		   


%%% p_rewrite( Flag, SimLit, RewriteL, InCondL, InLit, OutLitL, OutCondL)
%%% Rewrite matchender Literale durch RewriteL

:- mode p_rewrite( ++, +, +, +, ?, ?, -).

p_rewrite( _, _, _, _, Var, [Var], []) :- var( Var), !.

p_rewrite( con, SimLit, RewriteL, InCondL, InLit, [OutLit], OutCondL) :-
	instance( InLit, SimLit), 
	copy_term( (SimLit,RewriteL,InCondL), (CSimLit,CRewriteL,OutCondL)),
	CSimLit = InLit, !,
	member( OutLit, CRewriteL).
	
p_rewrite( dis, SimLit, RewriteL, InCondL, InLit, CRewriteL, OutCondL) :- 
	instance( InLit, SimLit), 
	copy_term( (SimLit,RewriteL,InCondL), (CSimLit,CRewriteL,OutCondL)),
	CSimLit = InLit, !.

p_rewrite( rew, SimTerm, RewriteL, InCondL, InTerm, OutTermL, OutCondL) :- 
	InTerm =.. [Fnc|InArgL],
	u_rewrite_list( rew, SimTerm, RewriteL,InCondL,InArgL,ZwiArgL,ZwiCondL),
	u_flatten( ZwiArgL, OutArgL),
	ZwiTerm =.. [Fnc|OutArgL],
	p_rewrite( dis, SimTerm, RewriteL, InCondL, ZwiTerm, OutTermL,OutCondH),
	flatten( [OutCondH|ZwiCondL], OutCondL), !.

p_rewrite( flat, SimTerm, [Rewrite], InCondL, InTerm, [OutTerm], OutCondL) :- 
	InTerm =.. [Fnc|InArgL],
	l_rewrite_some( SimTerm, Rewrite,InCondL,InArgL,OutArgL,OutCondL),
	OutTerm =.. [Fnc|OutArgL],
	!.

p_rewrite( _, _, _, _, X, [X], []) :- !.


l_rewrite_some( SimTerm, Rewrite, InCondL, 
	        [Term|Rest], [CRewrite|Rest], OutCondL) :-
	instance( Term, SimTerm), 
	copy_term( (SimTerm,Rewrite,InCondL), (CSimTerm,CRewrite,OutCondL)),
	CSimTerm = Term, 
%	printf('Rewrite %w to %w, cond %w%n',[Term, CRewrite, OutCondL]),
	!.

l_rewrite_some( SimTerm, RewriteL, InCondL, 
	        [Term|Rest], [Term|SimRest], OutCondL) :-
	l_rewrite_some( SimTerm, RewriteL, InCondL, Rest, SimRest, OutCondL).


%%% p_assert_bck( Term)
%%% Backtrackingfaehiges append

:- mode p_assert_bck( +).

p_assert_bck( Term) :- assert( Term), u_on_backtrack( retract(Term)).


%%% p_count( GlobaleVariablenname, Additor)
%%% Backtrackingfaehiger Zaehler

:- mode p_count( ++, ++).
	
p_count( Type, Add) :- 
	getval( Type, Value),
	plus( Value, Add, Sum),
	setval( Type, Sum),
	u_on_backtrack( setval( Type, Value)). 


%%% p_call( Goal)
%%% Prologaufrufe, die nicht im Trace erscheinen

:- mode p_call( +).

p_call( Goal) :- Goal.


%%% u_on_backtrack( Goal)
%%% Ausfuehrung des Goals beim Backtracking

:- mode u_on_backtrack( +).

u_on_backtrack( _).
u_on_backtrack( Goal) :- Goal, !, fail. 


%%% u_flatten( InLL, OutL)
%%% Wie Flatten, jedoch nur ein Level

:- mode u_flatten( +, -).

u_flatten( [], []).

u_flatten( [H|InR], L) :-
	u_flatten( InR, OutR), 
	append( H, OutR, L).


%%% u_delay( Variable, Goal)
%%% Delayen des Goals bis Variable instanziiert wurde
%%% pragma( debug). erforderlich (to be able to see the DELAY port)

u_delay(Term, Goal, Module) :-
	make_suspension(Goal, 2, Susp, Module),
	insert_suspension(Term, Susp, 1, suspend).


%%% u_open_trc( S)
%%% Oeffnen des Tracefiles

:- mode u_open_trc( -).

u_open_trc( S) :-
	def_const( problem, Problem),
	concat_atoms( Problem, '.trc', FileN),
	open( FileN, append, S).


% ---------- Bildschirmmeldungen ----------

%%% u_out( String)
%%% u_out( String, VarL)
%%% Gibt Message auf dem Bildschirm aus

:- mode u_out( ++).
:- mode u_out( ++, +).

u_out( String) :- u_out( String, []).

u_out( String, VarL) :-
	protein_flag( out_stream, S),
	printf( S, String, VarL),
	flush( S).
	

% ---------- Start und Ende ----------

%%% u_init( Problem, OutProblem)
%%% Initialisierungen, Ausgabe Hinweise an den Benutzer

%:- mode u_init( ++, -).

u_init( InProblem, Problem) :-  
	name( InProblem, InProblemL),
	(append( ProblemL, [46,116,109,101], InProblemL) -> true ;
	    ProblemL = InProblemL),
	name( Problem, ProblemL),  
        set_error_handler(139,true/0),
	statistics( runtime, [Start,_]),
	def_set_const( start, Start),
	def_set_const( problem, Problem),
	set_flag( occur_check, on), 
	set_flag( print_depth, 1000000).

	
%%% u_tini
%%% Zuruecksetzen der Initialisierungen

u_tini :- 
	((protein_flag( trace, dynamic(_)), protein_mode( proof)) ->
           close( tview), 
	   close( input),
	   delete( in),
	   delete( out); true),
        (def_const( header, HeaderF),
	 exists( HeaderF) ->
	   concat_atoms( 'rm ', HeaderF, Call),
           sh( Call); true),
	def_const( problem, Problem),
	def_const( start, Start),
	statistics( runtime, [End,_]),
	Time is End - Start,
	Seconds is Time * 0.001,
	u_out( "%n%w.tme   finished in %w seconds%n", [Problem,Seconds]), !.
    

%%% u_abort
%%% Abort mit Zuruecksetzen der Initialisierungen

u_abort :- 
	u_out( "%naborting...", []),
	u_tini, 
	(protein_flag( out_stream, user) ->
	    halt ; exit_block( protein)).

% END t_header_utils.pl END

