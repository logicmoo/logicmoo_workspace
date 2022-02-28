% This is a heuristic for a-star that ONLY WORKS FOR the sliding domain.
% For each tile number n, it calculates how far the piece number n is
% from the tile, with respect to the manhattan distance. This is the 
% direct distance horisontally and vertically. All of the tiles are added
% together, and returned as the heuristic value.
% This heuristic is admissible. 

h(S, D) :-
        bb_get(fictiveGoal, G),
        length(G, L),
        total_distance(S, L, D).

% total_distance(+State, +Length, -Distance)
total_distance(S, L, D) :-
        % Calculates the size of the board, according to the
        % amount of predicates in the goal
        Size is integer(sqrt(L + 1)),
        distance(S, D, 0, Size).

% distance(+State, -Distance, +AccDist, +Size)
distance([], D, D, _S).
distance([H|T], D, Acc, Size) :-
        H = at(Block, Position), 
        get_number(Block, BN),
        get_number(Position, PN),  
        manhattan_distance(BN, PN, Size, Dist),
        Acc1 is Acc + Dist,
        distance(T, D, Acc1, Size).
distance([H|T], D, Acc, Size) :- % Uninteresting predicate
        \+ H = at(_, _),
        distance(T, D, Acc, Size).

absolute_distance(N1, N2, _Size, Dist) :-
        Diff is N1 - N2,
        Dist is abs(Diff). 

% Calculates the manhattan distance from Number1 to Number2
% with respect to a grid that is Size x Size 
% manhattan_distance(+Number1, +Number2, +Size, -Distance)
manhattan_distance(N1, N2, Size, Dist) :-
        D1 is ((N1 - 1) mod Size) - ((N2 - 1) mod Size), 
        Horizontal is abs(D1),
        D2 is ((N1 - 1) // Size) - ((N2 - 1) // Size),
        Vertical is abs(D2),
        Dist is Horizontal + Vertical.
        
% Turns the predicate from the sliding domain into numeric values
get_number(Pred, Number) :-
        atom_chars(Pred, PredList),
        PredList = [_|SmallPredList], 
        number_chars(Number, SmallPredList).