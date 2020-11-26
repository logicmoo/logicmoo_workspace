%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2003-5, Renate Schmidt, University of Manchester
%
% Adaptation of normalise.pl of ml2dfg translator
% The primistive operators are assumed to be
%     or, not, dia, comp, test, conv
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

normalise([], []).
normalise([Fml|FmlList], [NFFml|NFList]) :-
    normalise(Fml, [], NFFml, Paths),
    write('             '), print(NFFml), nl, 
    write('Paths = '), print(Paths), nl,
    normalise(FmlList, NFList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Elimination of implies, equiv, dia, plus
% Simplification &
% Transformation into normal form
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Eliminating implies

normalise(implies(A, B), FreeV, NF, Paths) :- !,
    normalise(or(not(A), B), FreeV, NF, Paths).

normalise(not(implies(A, B)), FreeV, NF, Paths) :- !,
    normalise(not(or(not(A), B)), FreeV, NF, Paths).

normalise(conv(implies(A, B)), FreeV, NF, Paths) :- !,
    normalise(or(not(conv(A)), conv(B)), FreeV, NF, Paths).

% Eliminating implied

normalise(implied(B, A), FreeV, NF, Paths) :- !,
    normalise(or(not(A), B), FreeV, NF, Paths).

normalise(not(implied(B, A)), FreeV, NF, Paths) :- !,
    normalise(not(or(not(A), B)), FreeV, NF, Paths).

normalise(conv(implied(B, A)), FreeV, NF, Paths) :- !,
    normalise(or(not(conv(A)), conv(B)), FreeV, NF, Paths).

% Eliminating equiv

normalise(equiv(A, B), FreeV, NF, Paths) :- !,
    normalise(not(or(not(or(not(A), B)), not(or(not(B), A)))), FreeV, NF, Paths).

normalise(not(equiv(A, B)), FreeV, NF, Paths) :- !,
    normalise(or(not(or(not(A), B)), not(or(not(B), A))), FreeV, NF, Paths).

normalise(conv(equiv(A, B)), FreeV, NF, Paths) :- !,
    normalise(not(or(not(or(not(conv(A)), conv(B))), not(or(not(conv(A)), conv(B))))), FreeV, NF, Paths).

% Eliminating box

normalise(box(R,A), FreeV, NF, Paths) :- !,
    normalise(not(dia(R,not(A))), FreeV, NF, Paths).

normalise(not(box(R,A)), FreeV, NF, Paths) :- !,
    normalise(dia(R,not(A)), FreeV, NF, Paths).

% Eliminating and

normalise(and(A,B), FreeV, NF, Paths) :- !,
    normalise(not(or(not(A),not(B))), FreeV, NF, Paths).

normalise(not(and(A,B)), FreeV, NF, Paths) :- !,
    normalise(or(not(A),not(B)), FreeV, NF, Paths).

% Eliminating false

normalise(false, FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

normalise(not(false), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(conv(false), FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

% Eliminating id
% model with p? | -p?

normalise(id, FreeV, NF, Paths) :- !,
    normalise(or(test('ID'),test(not('ID'))), FreeV, NF, Paths).

normalise(dia(id,A), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF, Paths).

normalise(dia(conv(id),A), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF, Paths).

normalise(not(dia(id,A)), FreeV, NF, Paths) :- !,
    normalise(not(A), FreeV, NF, Paths).

normalise(not(dia(conv(id),A)), FreeV, NF, Paths) :- !,
    normalise(not(A), FreeV, NF, Paths).

normalise(comp(id,R), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF, Paths).

normalise(comp(R,id), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF, Paths).

normalise(comp(conv(id),R), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF, Paths).

normalise(comp(R,conv(id)), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF, Paths).

normalise(or(conv(id),R), FreeV, NF, Paths) :- !,
    normalise(or(id,R), FreeV, NF, Paths).

normalise(or(R,conv(id)), FreeV, NF, Paths) :- !,
    normalise(or(R,id), FreeV, NF, Paths).

normalise(star(id), FreeV, NF, Paths) :- !,
    normalise(id, FreeV, NF, Paths).

normalise(star(conv(id)), FreeV, NF, Paths) :- !,
    normalise(id, FreeV, NF, Paths).

normalise(conv(id), FreeV, NF, Paths) :- !,
    normalise(id, FreeV, NF, Paths).

% Eliminating plus

normalise(plus(R), FreeV, NF, Paths) :- !,
    normalise(comp(R,star(R)), FreeV, NF, Paths).

normalise(conv(plus(R)), FreeV, NF, Paths) :- !,
    normalise(conv(comp(R,star(R))), FreeV, NF, Paths).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simplification & pushing not inwards

% Negation

normalise(not(not(A)), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF, Paths).

normalise(not(test(A)), FreeV, NF, Paths) :- !,
    normalise(test(not(A)), FreeV, NF, Paths).

% Disjunction

normalise(or(A, A), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF, Paths).

normalise(or(not(A), A), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(or(A, not(A)), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(or(_,true), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(or(true,_), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(or(A, not(true)), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF, Paths).

normalise(or(not(true), A), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF, Paths).

normalise(or(test(not(A)), test(A)), FreeV, NF, Paths) :- !,
    normalise(test(true), FreeV, NF, Paths).

normalise(or(test(A), test(not(A))), FreeV, NF, Paths) :- !,
    normalise(test(true), FreeV, NF, Paths).

normalise(or(A, test(not(true))), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF, Paths).

normalise(or(test(not(true)), A), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF, Paths).

normalise(or(A, B), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF1, Paths1),
    normalise(B, FreeV, NF2, Paths2),
    (Paths1 > Paths2  ->  
         OrdNF = or(NF2, NF1);
         OrdNF = or(NF1, NF2)),
    (A = NF1, B = NF2 ->
         (NF = OrdNF,
          Paths is Paths1 + Paths2);
         normalise(OrdNF, FreeV, NF, Paths)).

% not or, i.e. Conjunction

normalise(not(or(A, A)), FreeV, NF, Paths) :- !,
    normalise(not(A), FreeV, NF, Paths).

normalise(not(or(not(A), A)), FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

normalise(not(or(A, not(A))), FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

normalise(not(or(A,not(true))), FreeV, NF, Paths) :- !,
    normalise(not(A), FreeV, NF, Paths).

normalise(not(or(not(true),A)), FreeV, NF, Paths) :- !,
    normalise(not(A), FreeV, NF, Paths).

normalise(not(or(_, true)), FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

normalise(not(or(true, _)), FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

normalise(not(or(A,test(not(true)))), FreeV, NF, Paths) :- !,
    normalise(not(A), FreeV, NF, Paths).

normalise(not(or(test(not(true)),A)), FreeV, NF, Paths) :- !,
    normalise(not(A), FreeV, NF, Paths).

normalise(not(or(A, B)), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF1, Paths1),
    normalise(B, FreeV, NF2, Paths2),
    (Paths1 > Paths2  ->  
         OrdNF = not(or(NF2, NF1));
         OrdNF = not(or(NF1, NF2))),
    (A = NF1, B = NF2 ->
         (NF = OrdNF,
          Paths is Paths1 + Paths2);
         normalise(OrdNF, FreeV, NF, Paths)).

% dia

normalise(dia(_,not(true)), FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

normalise(dia(test(not(true)),_), FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

normalise(dia(test(true),A), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF, Paths).

normalise(dia(comp(R,S),A), FreeV, NF, Paths) :- !,
    normalise(dia(R,dia(S,A)), FreeV, NF, Paths).

normalise(dia(or(R,S),A), FreeV, NF, Paths) :- !,
    normalise(or(dia(R,A),dia(S,A)), FreeV, NF, Paths).

normalise(dia(R,A), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF1, Paths1),
    normalise(A, FreeV, NF2, Paths2),
    (R = NF1, A = NF2 ->
        (NF = dia(NF1, NF2),
            Paths is Paths1 * Paths2);
        normalise(dia(NF1, NF2), FreeV, NF, Paths)).

% not box, i.e. dia

normalise(not(dia(_,not(true))), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(not(dia(not(true),_)), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(not(dia(test(true),A)), FreeV, NF, Paths) :- !,
    normalise(not(A), FreeV, NF, Paths).

normalise(not(dia(comp(R,S),A)), FreeV, NF, Paths) :- !,
    normalise(not(dia(R,dia(S,A))), FreeV, NF, Paths).

normalise(not(dia(or(R,S),A)), FreeV, NF, Paths) :- !,
    normalise(not(or(dia(R,A),dia(S,A))), FreeV, NF, Paths).

normalise(not(dia(R,A)), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF1, Paths1),
    normalise(A, FreeV, NF2, Paths2),
    (R = NF1, A = NF2 ->
        (NF = not(dia(NF1, NF2)),
            Paths is Paths1 * Paths2);
        normalise(not(dia(NF1, NF2)), FreeV, NF, Paths)).

% test

%normalise(test(not(true)), FreeV, NF, Paths) :- !,
%    normalise(not(true), FreeV, NF, Paths).

normalise(test(A), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF1, Paths1),
    (A = NF1 ->
        (NF = test(NF1),
         Paths = Paths1);    % not sure if this is necessary
        normalise(test(NF1), FreeV, NF, Paths)).

% not test

normalise(not(test(not(true))), FreeV, NF, Paths) :- !,
    normalise(test(true), FreeV, NF, Paths).

normalise(not(test(A)), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF1, Paths1),
    (A = NF1 ->
        (NF = not(test(NF1)),
         Paths = Paths1);    % not sure if this is necessary
        normalise(not(test(NF1)), FreeV, NF, Paths)).

% comp

normalise(comp(true,true), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(comp(_,not(true)), FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

normalise(comp(not(true),_), FreeV, NF, Paths) :- !,
    normalise(not(true), FreeV, NF, Paths).

normalise(comp(test(true),R), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF, Paths).

normalise(comp(R,test(true)), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF, Paths).

normalise(comp(_,test(not(true))), FreeV, NF, Paths) :- !,
    normalise(test(not(true)), FreeV, NF, Paths).

normalise(comp(test(not(true)),_), FreeV, NF, Paths) :- !,
    normalise(test(not(true)), FreeV, NF, Paths).

normalise(comp(R,S), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF1, Paths1),
    normalise(S, FreeV, NF2, Paths2),
    (R = NF1, S = NF2 ->
        (NF = comp(NF1, NF2),
            Paths is Paths1 * Paths2);
        normalise(comp(NF1, NF2), FreeV, NF, Paths)).

% not comp

normalise(not(comp(true,true)), FreeV, NF, Paths) :- !,
    normalise(test(not(true)), FreeV, NF, Paths).

normalise(not(comp(_,not(true))), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(not(comp(not(true),_)), FreeV, NF, Paths) :- !,
    normalise(true, FreeV, NF, Paths).

normalise(not(comp(test(true),R)), FreeV, NF, Paths) :- !,
    normalise(not(R), FreeV, NF, Paths).

normalise(not(comp(R,test(true))), FreeV, NF, Paths) :- !,
    normalise(not(R), FreeV, NF, Paths).

normalise(not(comp(_,test(not(true)))), FreeV, NF, Paths) :- !,
    normalise(test(true), FreeV, NF, Paths).

normalise(not(comp(test(not(true)),_)), FreeV, NF, Paths) :- !,
    normalise(test(true), FreeV, NF, Paths).

normalise(not(comp(R,S)), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF1, Paths1),
    normalise(S, FreeV, NF2, Paths2),
    (R = NF1, S = NF2 ->
        (NF = not(comp(NF1, NF2)),
            Paths is Paths1 + Paths2);
        normalise(not(comp(NF1, NF2)), FreeV, NF, Paths)).

% star

normalise(star(not(true)), FreeV, NF, Paths) :- !,
    normalise(test(not(true)), FreeV, NF, Paths).

normalise(star(true), FreeV, NF, Paths) :- !,
    normalise(test(true), FreeV, NF, Paths).

normalise(star(test(not(true))), FreeV, NF, Paths) :- !,
    normalise(test(not(true)), FreeV, NF, Paths).

normalise(star(test(true)), FreeV, NF, Paths) :- !,
    normalise(test(true), FreeV, NF, Paths).

normalise(star(A), FreeV, NF, Paths) :- !,
    normalise(A, FreeV, NF1, Paths1),
    (A = NF1 ->
        (NF = star(NF1),
         Paths = Paths1);    % not sure if this is necessary
        normalise(star(NF1), FreeV, NF, Paths)).

% conv

normalise(conv(R), FreeV, NF, Paths) :- !,
    normalise(R, FreeV, NF1, Paths1),
    (R = NF1 ->
        (NF = conv(NF1),
         Paths = Paths1);    % not sure if this is necessary
        normalise(conv(NF1), FreeV, NF, Paths)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Literals

normalise(Lit, _, Lit, 1). 
