%******************** t_header_dynamic.pl *******************

% Praedikate zum parallelen Beweisbaumaufbau.

%************************************************************

%%% h_init_dynamic
%%% Initialisiert Treeviewer

h_init_dynamic :-
	u_open_trc( S),
	close( S),
	setval( node, 0),
	sh( 'tview -c in out &'),
        sh( 'sleep 2'),
	(h_check_pipes( in, out) ; u_abort),
	open( in, write, tview),
	open( out, read, input),
	writeln( tview, call('GeometryScale','10:10')),
	writeln( tview, call('GeometryRootLeft','')),
	writeln( tview, fill_pulldown(graphic,'PROTEIN', [])),
	writeln( tview, add_pulldown('PROTEIN',
                 [[toggle,pause,off,protein(pause),protein(go)],
                  [noconfirm,ende,protein(ende)]])),
        writeln( tview, root(node(0))),    
	writeln( tview, update),
	flush( tview), !.


%%% h_check_pipes( Pipe1, Pipe2)
%%% Testet die Pipes

:- mode h_check_pipes( ++, ++).

h_check_pipes( P1, P2) :-
	repeat,
	  exists( P1),
	  exists( P2).


%%% p_dynamic(Blatt,Text,Shape,Color,Node,NegNode,Father,Son,Speed)
%%% Dynamische Baumschreibung
%%% +++ Crash bei DRME

:- mode p_dynamic( ++, ++, ++, ++, +, +, ++, -, ++).

% Blaetter
p_dynamic( y, Text, Shape, Color, _, NegNode, Father, _, Speed) :-
	h_get_node( Son),
	h_add_node( node(Father), node(Son), NegNode, Text, Shape, Color),
	sleep( Speed).

% Knoten
p_dynamic( n, Text, Shape, Color, Node, NegNode, Father, Son, Speed) :-
	h_get_node( Son),
	h_add_node( node(Father), node(Son), NegNode, Text, Shape, Color),
	h_add_node( node(Son), leaf(Son), Node, 'closed path', 4, seagreen),
	sleep( Speed).


%%% h_get_node( NodeN)
%%% Liefert neue NodeN. Backtrackingfaehig !!!

:- mode h_get_node( -).

h_get_node( NodeN) :- 
	getval( node, NodeN),
	incval( node), 
	u_on_backtrack( decval( node)).


%%% h_add_node( Father, Son, Node, Text, Shape, Color)
%%% Fuegt Knoten hinzu. Backtrackingfaehig !!!

:- mode h_add_node( ++, -, +, ++, ++, ++).

h_add_node( Father, Son, Node, Text, Shape, Color) :-
	(Son == node(0) -> true; writeln( tview, new_edge(Father,Son))),
        writeln( tview, set_info(Son,Node)),
	writeln( tview, set_contents(Son,Text)),
	writeln( tview, set_shape(Son,Shape)),
	writeln( tview, set_color(Son,Color)),
	writeln( tview, update),
	writeln( tview, position(Son)),
        flush( tview),  
	u_on_backtrack( (Son == node(0) -> true;   
                           writeln( tview, del_edge(Father,Son)))).

% END t_header_dynamic.pl END

