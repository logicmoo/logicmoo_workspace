:- module(group_pairs_or_sort, [group_pairs_or_sort/2]).

group_pairs_or_sort(Pairs, Grouped) :-
    Pairs = [_-_|_],
    !,
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, UnGrouped),
    maplist(group_pairs_or_sort_into, UnGrouped, Grouped).
group_pairs_or_sort(Unsorted, Sorted) :-
    sort(Unsorted, Sorted).

group_pairs_or_sort_into(K-U, K-S) :- group_pairs_or_sort(U, S).
