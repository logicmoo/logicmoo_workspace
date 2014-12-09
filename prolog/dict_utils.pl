:- module(dict_utils, [dict_subtract/3, dict_intersect/3]).

dict_subtract(Dict1, Dict2, Dict) :-
    put_dict(Dict2, Dict1, Dict3),
    select_dict(Dict1, Dict3, Dict).

dict_intersect(Dict1, Dict2, Dict) :-
    put_dict(Dict2, Dict1, Dict3),
    select_dict(Dict1, Dict3, Rest1),
    select_dict(Rest1, Dict2, Dict).
