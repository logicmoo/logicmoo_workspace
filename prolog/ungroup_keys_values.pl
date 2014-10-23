:- module(ungroup_keys_values,
	  [ungroup_keys_values/2,
	   ungroup_keys_values/3
	  ]).

% :- use_module(library(maplist_dcg)).

% ungroup_keys_values(Groups) -->
%     maplist_dcg(ungroup_key_values, Groups).

% ungroup_key_values(K-VL) -->
%     maplist_dcg(ungroup_key_value(K), VL).

% ungroup_key_value(K, V) --> [K-V].

ungroup_keys_values(Groups, Pairs) :-
    ungroup_keys_values(Groups, Pairs, []).

ungroup_keys_values([]) --> [].
ungroup_keys_values([M-[N|TN]|T]) -->
    [M-N],
    same_key(TN, M),
    ungroup_keys_values(T).

same_key([N|TN], M) -->
    [M-N],
    same_key(TN, M).
same_key([], _) --> [].
