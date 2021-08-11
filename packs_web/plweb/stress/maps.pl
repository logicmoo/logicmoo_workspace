:- module(procps_maps,
          [ maps/2,                     % +File, -Records
            map_size/2,
            prot/1,
            anon/1
          ]).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

maps(PID, Records) :-
    integer(PID),
    !,
    format(string(File), "/proc/~w/maps", [PID]),
    maps(File, Records).
maps(File, Records) :-
    phrase_from_file(maps(Records), File).

maps([H|T]) --> map(H), !, maps(T).
maps([]) --> [].

map(map{start:From,
        end:To,
        access:Access,
        flags: f{f1:F1,f2:F2},
        inode: INode,
        file: File
       }) -->
    xinteger(From), "-", xinteger(To), " ",
    access(Access), " ",
    xinteger(_Unknown), " ",
    xinteger(F1), ":", xinteger(F2), " ",
    integer(INode),
    (   {INode == 0}
    ->  (   blanks_to_nl
        ->  {File = (-)}
        ;   blanks,
            "[", string(Res), "]", blanks_to_nl
        ->  { atom_codes(File, Res) }
        )
    ;   blanks,
        string(S),
        blanks_to_nl
    ->  { atom_codes(File, S) }
    ).

access([R,W,X,P]) --> r(R), w(W), x(X), p(P).

r(r) --> "r".
r(-) --> "-".
w(w) --> "w".
w(-) --> "-".
x(x) --> "x".
x(-) --> "-".
p(p) --> "p".
p(s) --> "s".


map_size(Map, Size) :-
    Size is Map.end - Map.start.

prot(Map) :-
    Map.access = [-,-,-,p].

anon(Map) :-
    Map.access = [r,w,-,p],
    Map.inode == 0.
