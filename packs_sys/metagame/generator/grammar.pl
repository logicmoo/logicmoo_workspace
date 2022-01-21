%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% grammar.pl
%%% This file defines the grammar for symmetric chess-like games.
%%% This grammar is used bidirectionally, to parse game definitions
%%% into an internal representation, and to generate definitions
%%% from an internal representation.
%%%
%%% The grammar is described in the paper:
%%% Metagame in Symmetric Chess-Like Games
%%%
%%% The line and tab symbols in the rules are used for pretty-printing
%%% purposes, and are ignored when parameter parsing_mode is parsing.

game(Game) --> {game(Game,Name,Board,Pieces,Goals,Constraints)},
        [game], game_name(Name), line,
	goal_defs(Goals), line,
	board(Board), line,
	opt_constraints(Constraints), line, 
        piece_defs(Pieces), 
        [end,game], period.


game_name(Game) --> [Game], {atom(Game)}.

board(B) --> {board(B), 
	      board_size(B,XMax,YMax), 
	      board_type(B,Type), 
	      board_promote_rows(B,Promote),
	      board_inversion(B,Inversion),
	      board_assignments(B,Assignments)},
     [board_size],
     number(XMax), [by], number(YMax), line,
     [board_type], board_type(Type), line,
      opt_inversion(Inversion), 
     [promote_rank], number(Promote), line,
      [setup], line, assignment_list(Assignments).

board_type(planar) --> [planar].
board_type(vertical_cylinder) --> [vertical_cylinder].

inversion_type(forward).
inversion_type(diagonal).



assignment_list(A) --> assignment_decision(A)
                      | assignments(A).


assignments([A]) --> assignment(A).
assignments([A|As]) --> assignment(A), line, assignments(As).

assignment(A=S) --> tab(5), piece_name(A), [at], square_list(S).

assignment_decision(D) --> {decision(D), decision_chooser(D,C),
	                           decision_options(D,O), 
				   decision_constraints(D,Con)},
        tab(3), [decision], assigner(C), [assigns], piece_names(O), line,
        tab(5), [to], square_list(Con), line, tab(3), [end,decision].

assigner(C) --> player(C).
assigner(random) --> [random].


% 0 or more piece definitions. 
piece_defs([]) --> [].
piece_defs([D|Defs]) --> piece_def(D), line, piece_defs(Defs).


piece_def(Def) --> {piece_definition(Def,Name,Movement,Capture,Promote,Con)},
	[define], piece_name(Name), line, 
	 opt_moving(Movement),
	 opt_capturing(Capture),
	 opt_promoting(Name,Promote),
	 opt_constraints(Con),
        [end,define], line.


opt_moving([]) --> [].
opt_moving(Movement) --> 
	tab(3), [moving], line, 
	        movement_def(Movement), line, 
	tab(3),  [end,moving], line. 

opt_capturing([]) --> [].
opt_capturing(Capture) --> 
	tab(3), [capturing], line, 
	        capture_def(Capture), line,
	tab(3), [end,capturing], line. 

% If no promote name is given defaults to promoting to same piece.
opt_promoting(Name,promote(Name)) --> [].
opt_promoting(_,Promote) --> 
	tab(3), [promoting], promote_def(Promote), line,
	tab(3), [end,promoting], line. 



opt_constraints(Constraint) --> 
	{constraint(Constraint),
	 constraint_must_capture(Constraint,no),
	 constraint_continue_captures(Constraint,no)},
	 [].	
opt_constraints(Constraint) --> 
	tab(3), [constraints], constraint_def(Constraint), line. 


% Defaults to diagonal inversion. 
opt_inversion(diagonal) --> 
	 [].	
opt_inversion(Inversion) --> 
	[inversion], inversion_def(Inversion), line. 


inversion_def(Inversion) -->  {inversion_type(Inversion)}, 
	[Inversion].



movement_def([M]) --> simple_movement(M).
movement_def([M|Ms]) --> simple_movement(M), line, line, movement_def(Ms).

simple_movement(M) --> {movement(M), movement_type(M,T), movement_dir(M,D),
	                movement_sym(M,S)},
	tab(15), [movement], line,
	           movement_type(T), line, 
	           gdirection(D), syms(S), line, 
	tab(15), [end,movement].


movement_type(T) --> leaper(T).
movement_type(T) --> rider(T).
movement_type(T) --> hopper(T).

leaper(L) --> {leaper(L)}, tab(20), [leap].

rider(R) --> {rider(R),rider_must(R,Must),rider_min(R,Min),rider_max(R,Max)},
	 tab(20), [ride], min_dist(Min), max_dist(Max), longest(Must).

min_dist(1) --> [].
min_dist(Min) --> [min], number(Min).

max_dist(any) --> []. 
max_dist(N) --> [max], number(N).

longest(no) --> [].
longest(yes) --> [longest].


hopper(H) --> {hopper(H), hopper_before(H,B), hopper_over(H,O), 
	      hopper_after(H,A), hopper_type(H,R)},
	  tab(20), [hop], 
                     [before], compare_eq(B), [over], compare_eq(O),
		      [after], compare_eq(A), line, 
	 tab(20),  [hop_over], description(R).


%--------------
% Symmetries
%--------------

syms(Sym) --> {symmetry(Sym)}, [symmetry], symmetry_set(Sym).

% Not produced all_sym.
symmetry_set(Sym) --> {sym_forward(Sym,yes),
	               sym_side(Sym,yes),
		       sym_rotation(Sym,yes)},
		       [all_symmetry].
symmetry_set(Sym) --> openbrace, sym_set(Sym), closebrace.

sym_set(Sym) --> forward(Sym), side(Sym), rotation(Sym).


forward(Sym) --> [], {sym_forward(Sym,no)}.
forward(Sym) --> [forward], {sym_forward(Sym,yes)}.

side(Sym) --> [], {sym_side(Sym,no)}.
side(Sym) --> [side], {sym_side(Sym,yes)}.

rotation(Sym) --> [rotation], {sym_rotation(Sym,yes)}.
rotation(Sym) --> {sym_rotation(Sym,no)}, []. 

%--------------
% Equations
%--------------

% Change from eq to = in generator.

compare_eq(C) --> {comparison(C,Comp,Number), comparison_comp(C,Comp),
	           comparison_num(C,Number)},
	openb, [x], comparative(Comp), delta(Number), closeb.

%comparative(Comp) --> [Comp], {comparative(Comp)}.

comparative(geq) --> ['>='].
comparative(eq) --> ['='].
comparative(leq) --> ['<='].


%-----------------------------
% Directions, Square_List, Deltas
%----------------------------

gdirection(Dir) --> {direction(Dir,X,Y)}, tab(20), ['<'], delta(X), comma,
                    delta(Y), ['>'].  

square_list(Squares) --> openbrace, squares(Squares), closebrace.

squares([H]) --> gsquare(H).
squares([H|T]) --> gsquare(H), squares(T).

gsquare(Sq) --> {square(Sq,X,Y), alpha_squares_mode(on)}, 
	['('], [Col], {nth_letter(X,Col)}, 
	 comma, number(Y), [')']. 

gsquare(Sq) --> {square(Sq,X,Y), alpha_squares_mode(off)},
	['('], number(X), comma, number(Y), [')']. 


delta(Delta) --> number(Delta).

number(N) --> [N], {number(N)}.


%--------------
% CAPTURES
%--------------

capture_def([C]) --> simple_capture(C).
capture_def([C|Cs]) --> simple_capture(C), line, line, capture_def(Cs).

simple_capture(C) --> {capture(C), capture_movement(C,M),capture_methods(C,T), 
	               capture_type(C,R),
			capture_effect(C,E)},
	tab(5), [capture,by],  capture_methods(T), line, 
	tab(14),         [type], description(R),  line, 
        tab(14),         [effect], effect(E), line, 
	            movement_def(M), line, 
	tab(5), [end,capture].

capture_methods(M) --> {method(M)}, openbrace, retrieve(M),
                       clobber(M), hop(M), closebrace.


retrieve(Method) --> [], {method_retrieve(Method,no)}.
retrieve(Method) --> [retrieve], {method_retrieve(Method,yes)}.

clobber(Method) --> [], {method_clobber(Method,no)}.
clobber(Method) --> [clobber], {method_clobber(Method,yes)}.

hop(Method) --> [hop], {method_hop(Method,yes)}.
hop(Method) --> {method_hop(Method,no)}, []. 


% Simplified as in paper grammar.
% Removed displacement
effect(remove) --> [remove].
effect(possess(Player)) --> [Player,possesses].
%effect(give) --> [opponent,possesses].
%effect(displace(Player)) --> [Player, displaces].
%effect(displace_enemy) --> [opponent, displaces].



%--------------
% GOALS 
%--------------
% The generator always produces 1 stalemate goal, 
% then adds the other 2 types of goals.  But this 
% grammar is actually more general than the games
% generated, and programs must be able to read anything
% in the class defined by this grammar.

goal_defs(Goals) --> [goals], goals(Goals).

goals([]) --> [].
goals([G|Goals]) --> simple_goal(G), line, tab(5), goals(Goals).

simple_goal(Arrive) --> {arrive_goal(Arrive,Desc,Squares)},
	                [arrive], description(Desc), [at], square_list(Squares).

simple_goal(Eradicate) --> {eradicate_goal(Eradicate,Desc)},
	                [eradicate], description(Desc).
	               
simple_goal(Stalemate) --> {stalemate_goal(Stalemate,Player)},
	                [stalemate], player(Player).

%--------------------
% Descriptions
%--------------------

description(Desc) --> {piece_description(Desc,Player,Pieces)},
	openb, player_gen(Player), piece_names(Pieces), closeb.

piece_names(any_piece) --> [any_piece].
piece_names(Pieces) --> openbrace, identifiers(Pieces), closebrace.

identifiers([Piece]) --> piece_name(Piece).
identifiers([P|Pieces]) --> piece_name(P), identifiers(Pieces).


piece_name(Piece) --> [Piece], {atom(Piece)}.


player_gen(Player) --> openbrace, player(Player), closebrace.
player_gen(any_player) --> [any_player].

player(player) -->  [player].
player(opponent) --> [opponent].

player(player).
player(opponent).


%--------------
% Piece Movement Constraints
%--------------

constraint_defs(Constraint) --> {constraint(Constraint),
			  constraint_must_capture(Constraint,no),
			  constraint_continue_captures(Constraint,no)},
	[].	
constraint_defs(Constraint) --> [constraints], constraint_def(Constraint).



constraint_def(Constraint) --> {constraint(Constraint)},
	must_capture(Constraint), continue_captures(Constraint).

must_capture(Constraint) --> {constraint_must_capture(Constraint,no)}, []. 
must_capture(Constraint) --> [must_capture], {constraint_must_capture(Constraint,yes)}.

continue_captures(Constraint) --> [], {constraint_continue_captures(Constraint,no)}.
continue_captures(Constraint) -->  {constraint_continue_captures(Constraint,yes)},
	[continue_captures].



%promote_def(Special) --> {promotion(Special,Prom)},
%	promotion(Prom).

promote_def(Prom) --> {decision(Prom)}, promotion_decision(Prom).
promote_def(promote(Prom)) --> [promote_to], piece_name(Prom).


% Added chooser to promote within his own, other guys, or anyones 
% piece_names.  Must make sure this works, and generator uses it!

promotion_decision(D) --> {decision(D), decision_chooser(D,C),
	                           decision_options(D,O)},
        [decision], player(C), line, tab(10), [options], description(O).



line --> {parsing_mode(printing)}, [line].
line --> {parsing_mode(parsing)}, [].

tab(T) --> {parsing_mode(printing)}, [tab(T)].
tab(_T) --> {parsing_mode(parsing)}, [].

true(any) --> [].


semi --> [';'].

colon --> [':'].

comma --> [','].

openp --> ['('].

closep --> [')'].

openb --> ['['].

closeb --> [']'].

openbrace --> ['{'].

closebrace --> ['}'].

period --> ['.'].


identifier --> [X], {atom(X)}.


%================================================================================
% Printing Assignments to strings
%================================================================================
% Along with the move and game grammars, the standard form for assignments
% in the grammar above is used to transmit initial random assignments to the 
% players at play-time.  
% The following routines convert between the grammatical (token) 
% representation, my system's  internal representation, and a character string
% encoding of these tokens for communication purposes.
% Note the period is added as in all other string representations I use,
% to determine end of strings. 

assignments_string(Assignment,String) :-
	var(String), !,
	assignments(Assignment,Tokens,['.'|_Rest]),
	print_tokens_to_string(Tokens,String).
assignments_string(Assignment,String) :-
	var(Assignment),
	read_tokens_from_string(String,Tokens),
	assignments(Assignment,Tokens,['.'|_Rest]).

%================================================================================
% PORTRAY functions to print certain grammatical constructs
% for interface purposes.
%================================================================================

portray_square(square(X,Y)) :- 
	with_alpha_squares(gsquare(square(X,Y),S,[])),
	print_tokens(S).

portray_player(player) :- write(white).
portray_player(opponent) :- write(black).

portray_piece(piece(A,B)) :- piece(piece(A,B),S,[]), print_tokens(S).

portray_moving(move(Piece,Player,From,To)) :- 
	moving(move(Piece,Player,From,To),S,[]), print_tokens(S).
	
portray_game(game(Name,_Board,_Pieces,_Goals,_Constraints)) :- 
	format("<Game: ~w>",[Name]).

%================================================================================
%Printing games
%================================================================================


% GEN(L): generates an internal game representation,
% and then returns the list which is its  grammatical representation
% using the grammar above.
%
gen(L) :- generate_game(G), game(G,L,_).


% RANDOM_GAME_TO_FILE(+File)
% -------------------
% The predicate used most commonly for generating new games.
% Outputs the game to File.
%
random_game_to_file(File) :-
	print_gen_game_to_file(File).
	

print_gen_game :- record_seed,
	 gen(G),
	 print_tokens(G).

print_gen_game_to_file(File) :- 
	record_seed,
	generate_game(G),
	set_printing_mode,
	game(G,GameList,_),
	print_game_to_file(GameList,File),
	set_parsing_mode.

print_game_struct(G) :-
	set_printing_mode,
	game(G,GameList,_),
	set_parsing_mode,
	print_tokens(GameList).
	

prettify_game(GameList,Pretty) :-
	set_parsing_mode,
	game(G,GameList,_),
	set_printing_mode,
	game(G,Pretty),
	set_parsing_mode.
	
pretty_print_game_to_file(GameList,File) :-
	prettify_game(GameList,Pretty),
	print_game_to_file(Pretty,File).


% PRINT_GAME_TO_FILE(+Game,+File)
% Outputs a game to file File.game.
print_game_to_file(Game,File) :-
	sys_suffixed_filename(File,game,GameFile),
	format("~nWriting game to file: ~w~n",[GameFile]),
	tell(GameFile),
	write_old_seed,
	print_tokens(Game),
	told.

% If we've just generated a game, print its
% seed as a comment. 
write_old_seed :- 
	old_seed(Seed), !,
	nl,
	write('% seed: '),
	write(Seed),
	nl.
write_old_seed.

	
%================================================================================
% Reading games from files, printing back.
%================================================================================

%%% Read from pascal-like game output, into list.
%%% Reading is CASE-INSENSITIVE:  all alpha characters
%%% are converted to lower case when read in.   
%%% Also ignores extra blanks, tabs, and linefeeds.
%%% Comments occur from some point in a line started by %,
%%% and will be ignored to end of line.
%%% Can read games without spaces between operators and atoms,
%%% so squares can be written (X,Y) instead of ( X , Y ).  
%%%
% read_game_from_file_to_list('~/prolog/play/game.2.5.92',Game).
%print_read_game('~/prolog/play/game.2.5.92').

print_read_game(File) :- 
	format("~nReading game from file~n",[]),
	read_game_from_file_to_list(File,Game),
	format("~nRead game from file~n",[]),
	set_parsing_mode,
	format("~nParsing game in parse mode~n",[]),
	game(G,Game,[]),
	set_printing_mode,
	format("~nParsing game in print mode~n",[]),
	game(G,Game1,[]),
	format("~nPrinting game~n",[]),
	print_tokens(Game1).


read_game_from_file_to_list(File,Game) :- 
	format("~nReading game from file: ~w~n",[File]),
	read_tokens_from_file(File,Game).

read_game_from_string_to_list(String,Game) :- 
	format("~nReading game from string. ~n",[]),
	read_tokens_from_string(String,Game).


read_game_to_list(Game) :- 
	read_tokens(Game).
	

