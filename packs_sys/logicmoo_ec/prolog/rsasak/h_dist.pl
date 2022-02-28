% This heuristic is specifically for the shuffling problem.
% It caclulates the distance a block is from its goal, and 
% sums up the distances as the heuristic value. 
% The state should only consist of "at" and "left".
% The total value is divided by 4 to make the heuristic admissible.
% A 3shuffle will change the positions with 4. 
                                           
h(S, D) :- 
        bb_get(fictiveGoal, G), 
        total_distance(S, G, Dist),
        D is round(Dist/4).

total_distance(S, G, D) :-
        sort(S, S1),
        sort(G, G1),
        distance(S1, G1, D, 0).

% Reccursivly determine the distance
distance([], _, D, D).
distance([H|T], G, D, Acc) :-
        H = left(_, _),
        distance(T, G, D, Acc).
distance([H_S|T_S], [H_G|T_G], D, Acc) :-
        % Since S and G is sorted, the blocks should appear at the same time
        H_S = at(B, P_S), 
        H_G = at(B, P_G),
        get_number(P_S, N1),
        get_number(P_G, N2),
        Dist is N1 - N2,
        Acc1 is Acc + abs(Dist), 
        distance(T_S, T_G, D, Acc1).
        
% Turns the predicate from the sliding domain into numeric values
% Pred should be "pos1, pos3, ..."
get_number(Pred, Number) :-
        atom_chars(Pred, PredList),
        PredList = [_, _, _|SmallPredList], 
        number_chars(Number, SmallPredList).