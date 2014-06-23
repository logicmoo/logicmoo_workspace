:- module(referenced_by, [referenced_by/3]).

referenced_by([]) --> [].
referenced_by([Loc/CI|T]) -->
    ['\t'], Loc, CI,
    [nl],
    referenced_by(T).
