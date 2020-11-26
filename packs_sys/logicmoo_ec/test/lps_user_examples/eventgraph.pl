:- expects_dialect(lps).

% eventgraph.pl
% 
:- use_rendering(graphviz).

base([
event('CD8',bind,'CD4'),
event('CD4',regulate,'SARS-CoV-2'),
event('CD8',regulate,'SARS-CoV-2'),
event('CD8',inhibit,'SARS-CoV-2'),
event('CD4',inhibit,'SARS-CoV-2'),
event('CD4',regulate,'JAK3'),
event('CD8',regulate,'JAK3'),
event('CD8',inhibit,'JAK3'),
event('CD4',bind,'JAK3'),
event('CD4',inhibit,'JAK3'),
event('CD4',bind,'CD8'),
event('CD8',bind,'SARS-CoV-2'),
event('CD8',bind,'JAK3'),
event('CD4',associate,'JAK3'),
event('JAK3',regulate,'JAK3'),
event('JAK3',bind,'JAK3'),
event('JAK3',inhibit,'JAK3'),
event('JAK3',associate,'STAT'),
event('STAT',regulate,'SARS-CoV-2'),
event('STAT',inhibit,'SARS-CoV-2'),
event('STAT',associate,'MHC'),
event('MHC',bind,'SARS-CoV-2'),
event('MHC',inhibit,'SARS-CoV-2'),
event('MHC',regulate,'SARS-CoV-2'),
event('MHC',associate,'importin'),
event('importin',inhibit,'SARS-CoV-2'),
event('importin',regulate,'SARS-CoV-2'),
event('importin',bind,'SARS-CoV-2'),
event('JAK3',associate,'MHC'),
event('MHC',associate,'STAT'),
event('importin',associate,'STAT'),
event('JAK3',associate,'importin'),
event('importin',associate,'MHC'),
event('CD4',associate,'STAT'),
event('STAT',regulate,'JAK3'),
event('STAT',bind,'JAK3'),
event('STAT',inhibit,'JAK3'),
event('STAT',associate,'JAK3'),
event('MHC',bind,'JAK3'),
event('MHC',regulate,'JAK3'),
event('MHC',inhibit,'JAK3'),
event('MHC',associate,'JAK3'),
event('importin',regulate,'JAK3'),
event('importin',inhibit,'JAK3'),
event('importin',bind,'JAK3'),
event('CD4',associate,'MHC'),
event('CD8',associate,'JAK3'),
event('CD8',associate,'STAT'),
event('CD8',associate,'MHC')
]).

event2edge(event(A,Rel,B), subgraph(RelID,
             [ %node([style=filled, color=red]),
               node([style=filled]),
               edge([label = Rel]), 
               % A -> RelID, RelID -> B, 
               % (A,Rel) -> (Rel,B), 
               A -> B 
             ]) ) :- 
     gensym(Rel, RelID), not(A=B).

test1(SG) :-
    event2edge(event('CD4',increase,'RUNX1'),SG).

alledges(L) :- 
    findall(X, (base(B), member(Event, B), event2edge(Event,X)), L).

draw_graph(N, M, digraph(F)) :-
    alledges(L), copyfromto(1,N,M, L, F).  

copyfromto(_C,_N,_M,[],[]).
copyfromto(C,N,M,[H|R1],[H|R2]) :-
    N=<C, C<=M, !, CC is C+1, copyfromto(CC,N,M,R1,R2).
copyfromto(C,N,M,[_H|R1],R2) :-
    CC is C+1, copyfromto(CC,N,M,R1,R2).

/** <examples>
?- X = dot("digraph G { Hello->World }").
?- draw_graph(1, 100, X), Y = dot(X).
?- copyfromto(0, 3, 5, [1,2,3,4,5,6,7,8,9], L).
?- draw_graph(1, 100, X), Y = circo(X).
?- draw_graph(1, 100, X), Y = sfdp(X).
?- draw_graph(1, 100, X), Y = twopi(X).
*/
