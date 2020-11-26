:-module(graphviz,
     [add_arc_graph/2,
     init_graph/2,
     status/2,
     draw_graph/3,
     close_graph/0]).

:- ensure_loaded(sciff_options).

:- dynamic proof_status/2.


% A is the transition name
% B is the content of the new node
add_arc_graph(_,_) :-
	get_option(graphviz, off), !.
add_arc_graph(A,B):-
    status(S,_),
    draw_graph(S,A,B).


init_graph(_, _):-
		get_option(graphviz, off), !.
init_graph(FileName,Stream):-
    open(FileName,write,Stream),
    write(Stream,'digraph G {\n'),
    assert(proof_status(0,Stream)).

status(_,_):-
		get_option(graphviz, off), !.
status(S,F):-
    proof_status(S,F).

% draw_graph(+Sin,+Transition,+NewNode)
% Sin is the starting node identifier 
% Transition is the applied transition (whatever you want, goes to the label of the arc)
% NewNode is the representation of the new node (whatever you want, goes in the new node)
draw_graph(_,_,_):-
		get_option(graphviz, off), !.
draw_graph(Sin,Transition,NewNode):-
    proof_status(Stemp,Stream),
    retract(proof_status(Stemp,Stream)),
    Sout is Stemp + 1,
    assert(proof_status(Sout,Stream)),
    % Write the new node
    write(Stream,Sout), write(Stream,' [label="'),
    write(Stream,NewNode), write(Stream,'"];\n'),
    % write the arc
    write(Stream,Sin),
    write(Stream,' -> '),
    write(Stream,Sout),
    write(Stream,' [label="'),
    write(Stream,Transition),
    write(Stream,'"];\n').

close_graph :-
		get_option(graphviz, off), !.
close_graph :-
    status(_,F),
    write(F,'\n}\n'),
    close(F).
