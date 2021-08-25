%********************     t_utils.pl     ********************

% Utility-Praedikate 

%************************************************************

:- module_interface( t_utils).

:- export u_init / 2,
	  u_tini / 0,
	  u_abort / 0,
	  u_connect_lists / 3,
          u_copy_file / 2,
	  u_checklist / 2,
	  u_close_file / 1,
	  u_flatten / 2,
	  u_get_stream / 2,
	  u_init_streams / 1,
	  u_makelist / 2,
          u_makelist_comma / 2,
	  u_makelist_semicolon / 2,
	  u_maplist / 3,
	  u_nth / 5,
	  u_on_backtrack / 1,
	  u_open_file / 3,
	  u_out / 1, u_out / 2,
	  u_remove_dups / 2,
	  p_rewrite / 7, 
	  u_rewrite_list / 7,
	  u_set_stream / 1,
	  u_splitlist / 4,
	  u_var_name / 2,
	  u_writeclause / 2.

:- global u_delay / 2.

:- begin_module( t_utils).

:- lib( numbervars).

:- use_module( t_definitions).

:- dynamic stream / 2.

:- [t_header_utils].


% ---------- Metapraedikate ----------

:- tool( u_splitlist / 4, u_splitlist / 5).
:- tool( u_checklist / 2, u_checklist / 3).
:- tool( u_maplist / 3, u_maplist / 4).
:- tool( u_delay / 2, u_delay / 3).


%%% u_splitlist( Pred, InL, TrueL, FalseL)
%%% teilt jedes Element von InL in TrueL oder FalseL ein, je nachdem, ob 
%%% Pred == true oder false, wenn das Element von InL fuer El eingesetzt wird
%%% hat als tool ein zusaetzliches CallerModule-Argument

:- mode u_splitlist( +, +, -, -).

u_splitlist( Pred, [H|R], TrueL, FalseL, M) :-
        Pred =.. PL,
	append( PL, [H], CallL),
	Call =.. CallL,
	(call(Call, M) ->
	    TrueL = [H|TrueL1],
            FalseL = FalseL1
        ;
	    TrueL = TrueL1, 
            FalseL = [H|FalseL1]),
	u_splitlist( Pred, R, TrueL1, FalseL1, M).

u_splitlist( _, [], [], [], _).


%%% u_checklist(Pred, L) 
%%% tracebares checklist

u_checklist(_, [], _).

u_checklist(Pred, [H|T], M) :-
    Pred =.. PL,
    append(PL, [H], CallL),
    Call =.. CallL,
    call(Call, M),
    u_checklist(Pred, T, M).


%%% u_maplist(Pred, InL, OutL) 
%%% tracebares maplist

u_maplist(_, [], [], _).

u_maplist(Pred, [H1|T1], [H2|T2], M) :-
    Pred =.. PL,
    append(PL, [H1, H2], NewPred),
    Call =.. NewPred,
    call(Call, M),
    u_maplist(Pred, T1, T2, M).


% ---------- Listenmanipulation ----------

%%% u_flatten( InLL, OutL)
%%% Wie Flatten, jedoch nur ein Level

:- mode u_flatten( +, -).

u_flatten( [], []).

u_flatten( [H|InR], L) :-
	u_flatten( InR, OutR), 
	append( H, OutR, L).


%%% u_nth( InL, Start, N, X, OutL)
%%% wie nth1, aber es muss nur die Gesamtliste mitgegeben sein.
%%% Ausserdem ist der Start der Zaehlung mitzugeben.

:- mode u_nth( +, ++, -, -, -).

u_nth( [X|R], N, N, X, R).

u_nth( [H|InR], InN, OutN, X, [H|OutR]) :-
	ZwiN is InN + 1,
	u_nth( InR, ZwiN, OutN, X, OutR).


%%% u_remove_dups( InL, OutL)
%%% Loeschen doppelter Literale

:- mode u_remove_dups( +, -).

u_remove_dups( [], []) :- !.

u_remove_dups( [H|InR], [H|OutR]) :-
	nonground( H),
	u_remove_dups( InR, OutR).

u_remove_dups( [H|InR], OutR) :- 
	memberchk( H, InR), !, 
	u_remove_dups( InR, OutR).

u_remove_dups( [H|InR], [H|OutR]) :- 
	u_remove_dups( InR, OutR).


%%% u_connect_lists( L1, L2, L3)
%%% Bildet aus beiden Listen eine Liste mit Paerchen

u_connect_lists( [], [], []).

u_connect_lists( [H1|R1], [H2|R2], [(H1,H2)|R3]) :-
	u_connect_lists( R1, R2, R3).


% ---------- Varaiblenmanipulation ----------

%%% u_var_name( Var, (Var,Name))
%%% Ermittelt Name eine Variablen

u_var_name( Var, (Var,Name)) :- get_var_info( Var, name, Name).


% ---------- Termmanipulation ----------

%%% u_makelist( Term, Liste)
%%% u_makelist_comma oder u_makelist_semicolon

u_makelist( Term, Liste) :- u_makelist_comma( Term, Liste), !.
u_makelist( Term, Liste) :- u_makelist_semicolon( Term, Liste), !.


%%% u_makelist_comma( Term, Liste)
%%% Erzeugung einer Liste aus (A1, ..., An)

u_makelist_comma( (_ ; _), V) :- var( V), !, fail.

u_makelist_comma( (Lit , TermR), [Lit|LitR]) :- 
	u_makelist_comma( TermR, LitR), !.

u_makelist_comma( Lit, [Lit]).


%%% u_makelist_semicolon( Term, Liste)
%%% Erzeugung einer Liste aus (A1; ...;An)

u_makelist_semicolon( (_ , _), V) :- var( V), !, fail.

u_makelist_semicolon( (Lit ; TermR), [Lit|LitR]) :-
	u_makelist_semicolon( TermR, LitR), !.

u_makelist_semicolon( Lit, [Lit]).


% ---------- File und Stream Managing ----------

%%% u_init_streams( Part)
%%% Loeschen aller stream-Praedikate einer Partition

:- mode u_init_streams( ++).

u_init_streams( Part) :- retract_all( stream(Part,_)).


%%% u_get_stream( Part, S)
%%% Ermittelt den naechsten Stream der Partition.
%%% Immer wieder erfuellbar.

:- mode u_get_stream( ++, -).

u_get_stream( Part, S) :- (stream( Part, S) -> true ; !, fail).
u_get_stream( Part, S) :- u_get_stream( Part, S).


%%% u_open_file( Part, Read/Write/Append, File)
%%% Oeffnet Dateien und vermerkt Stream in der entsprechenden Partition

:- mode u_open_file( ++, ++, ++).

u_open_file( Part, write, File) :-
	open( File, write, S),
	asserta( stream(Part,S)), !.

u_open_file( Part, Flag, File) :-
	exists( File),
	open( File, Flag, S),
	asserta( stream(Part,S)), !.

u_open_file( _, _,File) :- def_message( error, file, File).


%%% u_close_file( Part)
%%% Schliessen der Datei mit erstem Stream aus der Partition

:- mode u_close_file( ++).

u_close_file( Part) :- retract( stream(Part,S)), close( S), !.
	

%%% u_copy_file( S, FName)
%%% kopiert den Inhalt der Datei 'FName' in den Stream S

:- mode u_copy_file( ++, ++).

u_copy_file( WS, FName) :-
	(open( FName, read, RS) ; close( WS), u_abort),
	repeat,
	   get( RS, C),
	   (C == -1 -> true; put(WS, C), fail),
	close( RS), !.


%%% u_writeclause( Part, Clause)
%%% writeclause und numbervars und Steamermittlung

:- mode u_writeclause( ++, +).

u_writeclause( Part, Clause) :-
	u_get_stream( Part, S),
	numbervars( Clause, 0, _),
	writeclause( S, Clause), !.
	
