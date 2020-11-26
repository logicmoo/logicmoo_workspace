/* $Id: mapcoloring.pl,v 321.4 2002-02-25 17:37:10-08 - - $ */

/*
* Map coloring.
*
* Given an adjacency matrix, find a coloring of the map such
* that no two adjacent nodes have the same color.  The four
* color theorem says this is always possible with four colors.
*/

/*
* Specification of the nodes in the graph and the paths.
*/

graph( [1,2,3,4,5] ).
graph([a,b,c,d,e,f]).

path( 1, 2 ).
path( 1, 3 ).
path( 1, 4 ).
path( 2, 3 ).
path( 2, 4 ).
path( 3, 4 ).
path( 4, 5 ).
path(a,b).
path(a,c).
path(a,d).
path(a,e).
path(b,c).
path(b,d).
path(b,e).
path(c,d).
path(c,e).
path(d,e).
path(e,f).

/*
* Undirected graph, ajacency is bidirectional.
*/

adjacent( Node1, Node2 ) :- path( Node1, Node2 ).
adjacent( Node1, Node2 ) :- path( Node2, Node1 ).

/*
* Specifications of possible colors for the nodes.
*/

color( red ).
color( green ).
color( blue ).
color( white ).
coloe( yellow ).

/*
* Find a coloring with no conflicts.
*/

findcoloring( [], [] ).
findcoloring( [Node | Nodes], [Coloring | Colorings] ) :-
	findcoloring( Nodes, Colorings ),
	Coloring = color( Node, Color ),
	color( Color ),
	noconflict( Coloring, Colorings ).

noconflict( _, [] ).
noconflict( Coloring1, [Coloring2 | Colorings] ) :-
	not( conflict( Coloring1, Coloring2 )),
	noconflict( Coloring1, Colorings ).

conflict( color( Node1, Color ), color( Node2, Color )) :-
	adjacent( Node1, Node2 ).

/*
* Trace the relevant relations.
*/

traceon :-
	trace( adjacent ),
	trace( color ),
	trace( findcoloring ),
	trace( noconflict ),
	trace( conflict ).

writeallcolorings :-
	writeanycoloring,
	fail.

writeanycoloring :-
	findanycoloring( Graph, Coloring ),
	write( Coloring ), nl.

findanycoloring( Graph, Coloring ) :-
	graph( Graph ),
	findcoloring( Graph, Coloring ).


/* prolog  tutorial 2.9 Map coloring redux 

We take another look at the map coloring problem introduced in Section 2.1. This time, the data representing region adjacency is stored in a list, colors are supplied in a list, and the program generates colorings which are then checked for correctness.
*/

adjacent(X,Y,Map) :-  member([X,Y],Map) ; member([Y,X],Map). 


find_regions([],R,R). 
find_regions([[X,Y]|S], R,A) :- 
 (member(X,R) ->  
  (member(Y,R) -> find_regions(S,R,A) ; find_regions(S,[Y|R],A)) ; 
   (member(Y,R) -> find_regions(S,[X|R],A) ; find_regions(S,[X,Y|R],A) ) ). 


color(Map,Colors,Coloring) :-
        find_regions(Map,[],Regions), 
        color_all(Regions,Colors,Coloring), 
        \+ conflict(Map,Coloring). 
color_all([R|Rs],Colors,[[R,C]|A]) :- 
        member(C,Colors), 
        color_all(Rs,Colors,A). 
color_all([],_,[]). 


conflict(Map,Coloring) :- 
        member([R1,C],Coloring), 
        member([R2,C],Coloring), 
        adjacent(R1,R2,Map). 

map1([[1,2],[1,3],[1,4],[1,5],[2,3],[2,4],[3,4],[4,5]] ).




/* prolog tutorial  2.1 Map Colorings 

This section uses a famous mathematical problem -- that of coloring planar maps -- to motivate logical representations of facts and rules in Prolog. The prolog program developed provides a representation for adjacent regions in a map, and shows a way to represent colorings, and a definition of when the colorings in conflict; that is, when two adjacent regions have the same coloring. The section introduces the concept of a semantic program clause tree -- to motivate the issue of semantics for logic-based programming.
*/

adjacent(1,2).         adjacent(2,1). 
adjacent(1,3).         adjacent(3,1). 
adjacent(1,4).         adjacent(4,1). 
adjacent(1,5).         adjacent(5,1). 
adjacent(2,3).         adjacent(3,2). 
adjacent(2,4).         adjacent(4,2). 
adjacent(3,4).         adjacent(4,3). 
adjacent(4,5).         adjacent(5,4). 

/*------------------------------------*/

color(1,red,a).    color(1,red,b). 
color(2,blue,a).   color(2,blue,b). 
color(3,green,a).  color(3,green,b). 
color(4,yellow,a). color(4,blue,b). 
color(5,blue,a).   color(5,green,b). 

/*------------------------------------*/

conflict(Coloring) :- 
   adjacent(X,Y), 
   color(X,Color,Coloring), 
   color(Y,Color,Coloring). 

/*-------------------------------------*/

conflict(R1,R2,Coloring) :- 
   adjacent(R1,R2), 
   color(R1,Color,Coloring), 
   color(R2,Color,Coloring). 

