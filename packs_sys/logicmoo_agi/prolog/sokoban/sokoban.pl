% Author: Jan Klinkosz, id number: 394 342

:-include(game).

  solve(Problem, Solution):-
Problem = [Tops, Rights, Boxes, Solutions, sokoban(Sokoban)],
abolish_all_tables,
retractall(top(_,_)),
findall(_, ( member(P, Tops), assert(P) ), _),
retractall(right(_,_)),
findall(_, ( member(P, Rights), assert(P) ), _),
retractall(solution(_)),
findall(_, ( member(P, Solutions), assert(P) ), _),
retractall(initial_state(_)),
findall(Box, member(box(Box), Boxes), BoxLocs),
assert(initial_state(state(Sokoban, BoxLocs))),
solve_problem(Problem, Solution).


solve_problem(_Problem, Solution) :-
    initial_state(Initial),
    Initial = state(_Sokoban, BoxLocs),
    dfs(Initial, [BoxLocs], BoxesMoves), !,
    transform(Initial, BoxesMoves, Solution).

/* checks if state is final, if not then pick
box to move and direction, try to move box in
that direction */
dfs(State, _History, []) :-
    final_state(State), !.


dfs(State, History, [Move | Moves]) :-
    movement(State, Move, _SokobanMoves),
    update(State, Move, NewState),
    NewState = state(_Sokoban, BoxLocs),
    \+ member(BoxLocs, History),
    dfs(NewState, [BoxLocs | History], Moves), !.

/* transforms box moves into sokoban moves, wich results
in that box moves */
transform(_State, [], []).

transform(state(Sokoban, Boxes), [push(Box, Dir) | BoxesMoves], SokobanMoves) :-
    neib(PushPosition, Box, Dir),
    can_reach(Sokoban, PushPosition, Boxes, [], Moves),
    update(state(Sokoban, Boxes), push(Box, Dir), NewState),
    transform(NewState, BoxesMoves, SokobanMovesRest),
    append(Moves, [move(PushPosition, Dir)], MovesWithLastMove),
    append(MovesWithLastMove, SokobanMovesRest, SokobanMoves).
