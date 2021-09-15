/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Günter Kniesel (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 * This file implements generic graph algorithms in Prolog.
 *  - Tarjan's O(M+N) computation of strongly connected components (SCCs)
 *    extended to deal also with nested SCCs. Nested SCCs are collapsed 
 *    into a single one.
 *  - Depth first path search (also O(M+N))
 *
 * To use the algorithms in the context of module M do the following:
 * - import this module into M via ":-use_module(THISMODULENAME)."
 * - define the graph on which the algorithms should operate by 
 *   providing in M clauses for the predicates 
 *     - graph_node(Node) 
 *     - graph_edge(FromNode,ToNode)
 * By adhering to this convention, different modules can work on different
 * locally defined graphs without interfering. 
 * DO not even think of patching 
 * Autor: Günter Kniesel
 * Date: September 26, 2005
 * Date: October 2, 2009: Made "public" predicates module transparent
 */
:- module('condor.graph.cycle.scc',  
         [ strongly_connected/1  % Arg1 = List of node lists representing SCCs
         , dfs/3   % Called locally in metapredicates 
         , node/1  % Called locally in metapredicates
         , edge/2  % Called locally in metapredicates
         ] ).

    



/*
 * Global data structures for depth first traversal and SCC.
 */
:- dynamic discovery_time/2. % args = node, time
:- dynamic finishing_time/2. % args = node, time
:- dynamic global_time/1.
:- dynamic scc_counter/1.
:- dynamic current_scc_nr/1.
:- dynamic scc_nr_for/2.
:- dynamic graph_orientation/1.


  
/*
 * Define graph structure based on predicates graph_edge/2 
 * and graph_node/1 to be defined in the calling module.
 */ 
:- module_transparent node/1.
node(N) :- 
    context_module(M), 
    M:graph_node(N).

:- module_transparent edge/2.
edge(N1,N2) :- 
    context_module(M), 
    ( graph_orientation(forward)
      -> M:graph_edge(N1,N2) 
       ; M:graph_edge(N2,N1)
    ).


/*
 * For the graph defined by node/1 and edge/2 calculate the 
 * strongly connected components (SCCs) using Tarjan's algorithm:
 *  1. Perform a depth first toplogical sort that 
 *     assigns start and finishing times to the nodes.
 *  2. Order nodes by reverse finishing time.
 *  3. Inverse the direction of graph edges.
 *  4. Perform a depth first topological sort of the reversed
 *     graph processing the nodes in the order of reverse 
 *     finishing time determined in step 2.
 *  5. Each path determined in this second toposort is a SCC.  
 */  
:- module_transparent( strongly_connected/1 ).
 
strongly_connected(SortedUniqueSCCs) :-
    clean, % Initialize local data for first DF traversal
    visit_all_nodes_randomly(_NumberedPaths),  
   % show_internals,
    sort_by_reverse_finishing_times(Nodes),
    clean, % Reinitialize local data for second DF traversal 
           % Must come AFTER sort_by_reverse_finishing_times!
    set_graph_orientation(reverse), % invert graph edges
    visit_all_nodes_ordered(Nodes,NumberedSccCandidatess),
   % show_internals,
    combine_nested_cycles(NumberedSccCandidatess,NumberedSCCs),
    findall(Unique, 
            ( member((_,Path),NumberedSCCs), 
              sort(Path,Sorted),
              removeDuplicates(Sorted,Unique)
            ),
            SortedUniqueSCCs),
    true.  



combine_nested_cycles(SccCandidates,SCCs):-
    length(SccCandidates,L),
    scc_counter(L),
    !,
    SccCandidates = SCCs.
combine_nested_cycles(SccCandidates,SCCs):-
    % findall( (Cnt,Path1), scc_candidate(Cnt,Path1), All),
    patch(SccCandidates,SCCs).

/** 
 * All SCC candidates with the same number are concatenated.
 * Arg2 is the list of remaining true SCCs. 
 */
patch( [],[] ).
patch( [(Nr,List1)|Tail],Patch) :-
    member_remove_first((Nr,List2),Tail,Rest),
    !,
    append(List2,List1,L21),
    patch( [(Nr,L21)|Rest],Patch).
patch( [Elem|Tail],[Elem|Patch]) :-
    patch( Tail,Patch).

/** 
 * Like member/2 but remove the found element from 
 * the list. The shortened list is returned in arg3.
 */
member_remove_first(Elem,[Elem|Rest],Rest) :- !.
member_remove_first(Elem,[H|Tail],[H|Rest]):-
   member_remove_first(Elem,Tail,Rest). 
    
/*
 * Topological sorting based on depth first search returns a list
 * of depths first paths through the graph, starting randomly.
 * This variant is used in the first phase of Tarjan's algorithm
 * for computing strongly connected components.
 */
:- module_transparent( visit_all_nodes_randomly/1 ).

visit_all_nodes_randomly(Paths) :-
    findall((Cnt,Path), ( % dfs_for_node(Cnt,Path)
                          node(N), dfs(N,Cnt,Path) 
                         ), Paths).



/*
 * Topological sorting based on depth first search returns a list
 * of depths first paths through the graph, proceessing the nodes
 * of the graph in the order indicated by NodeList.
 * This variant is used in the first phase of Tarjan's algorithm
 * for computing strongly connected components. When used that 
 * way the computed list of paths corresponds to the set of 
 * strongly connected components of the graph.
 */
:- module_transparent( visit_all_nodes_ordered/2 ).
visit_all_nodes_ordered(NodeList,Paths) :-
    findall((Cnt,Path), ( % dfs_for_node_from_list(NodeList,Cnt,Path)
                         member(N,NodeList),dfs(N,Cnt,Path)
                        ), Paths).


:- module_transparent( dfs_for_node/2 ).
:- module_transparent( dfs_for_node_from_list/3 ).
dfs_for_node(                   Cnt,Path) :- node(N),           dfs(N,Cnt,Path).
dfs_for_node_from_list(NodeList,Cnt,Path) :- member(N,NodeList),dfs(N,Cnt,Path).


:- module_transparent( dfs/3 ).
dfs(N,Cnt,Path) :-
    visit_first_node(N,Path),   % Increments scc_counter only once
    clause('condor.graph.cycle.scc':scc_counter(Cnt),_). % New value for every FIRST node!

:- module_transparent( visit_first_node/2 ).
visit_first_node(N,Path) :-
    not('condor.graph.cycle.scc':visited(N)),
    set_time(N, discovery_time),  % assert discovery_time(N,...)
    increment_scc_counter,
    visit_neighbours(N,Path).

/*
 * Depth first traversal starting at node N returns path Path.
 */
:- module_transparent( visit_node/2 ). 
visit_node(N,Path) :-
    not('condor.graph.cycle.scc':visited(N)),
    set_time(N, discovery_time),  % assert discovery_time(N,...)
    visit_neighbours(N,Path).


/*
 * A node has already been visited if its discovery time is set.
 * The node might still be visited ('grey' node) or its visit might
 * already be finished ('black' node). Use finished/1 to find out
 * the difference. 
 */
visited(N) :-
    discovery_time(N,_), 
    !.
    
    
/*
 * Visit one neighbour at one time. Visit them all upon backtracking.
 * Uses the graph_edge/2 definition from the calling context module.
 */
:- module_transparent( visit_neighbours/2 ).

visit_neighbours(N,Path) :-     % no unvisited neighbours
    dead_end(N),
    !,
    Path = [N],
    set_time(N, finishing_time).  % assert finishing_time(N,...)
visit_neighbours(N,Path) :-     % visit all unvisited neighbours
    context_module(M),
    M:edge(N,Other),              % ... by backtracking over M:edge/2
    not( 'condor.graph.cycle.scc':visited(Other) ),
    Path = [N|Rest],
    visit_node(Other,Rest).
visit_neighbours(N,_) :-        % all neighbours visited
    set_time(N, finishing_time),  % assert finishing_time(N,...)
    fail.


/*
 * The node N either has no neighbours at all or it has only
 * visited ones.
 */
:- module_transparent dead_end/1. 

dead_end(N) :-
    not( edge(N,_) ),
    !.
dead_end(N) :-
    forall( edge(N,N2), 'condor.graph.cycle.scc':visited(N2) ). 
   


/*
 * Sort nodes in reverse finishing time assigned by first 
 * toposort pass through the graph.
 */
sort_by_reverse_finishing_times(Nodes) :-
    findall( (Time,N), finishing_time(N,Time), All),
    sort(All,Sorted),
    reverse(Sorted,Rev),
    findall( Node, member((_,Node), Rev), Nodes).

/*
test(sort_by_reverse_finishing_times(Nodes),Expected) :-
     sort_by_reverse_finishing_times(Nodes),
     Expected = [a,b,e,d,c,f].
         
finishing_time(a, 18).
finishing_time(b, 17).
finishing_time(c, 6).
finishing_time(d, 7).
finishing_time(e, 8).
finishing_time(f, 3).   
*/


/* --------- Helper Predicates -------------------------- */
   

/*
 * Show snapshot of helper data structures.
 */   
show_internals :-
    listing_if_defined(discovery_time),
    listing_if_defined(finishing_time),
    listing_if_defined(global_time),
    listing_if_defined(scc_counter),
    listing_if_defined(graph_orientation).

    
/*
 * (Re)Initialize helper data structures.
 */
clean :-
  retractall(discovery_time(_,_)),
  retractall(finishing_time(_,_)),
  reset_time,
  reset_scc_counter,
  set_graph_orientation(forward).

 
/*
 * Assert discovery_time(N,currentTime) or finishing_time(N,currentTime)
 * and increment currentTime.
 */
set_time(N, Which) :-
    retract(global_time(T)),
    T1 is T+1, 
    assert(global_time(T1)),
    Fact =.. [Which, N, T],  % discovery_time(N,T) or finishing_time(N,T)
    assert(Fact).  


/** 
 * Reset time to 1.
 */
reset_time :-
   retractall(global_time(_)),
   assert(global_time(1)). 


/** 
 * Operations on counter of strongly connected components.
 */
   
reset_scc_counter :-
   set_scc_counter(0). 
       
set_scc_counter(New) :- 
   retractall(scc_counter(_)),
   assert(scc_counter(New)).
   % format('asserted scc_counter(~a).~n',[New]).  
   
increment_scc_counter :- 
   retract(scc_counter(Old)),
   New is Old+1,
   assert(scc_counter(New)).
   % format('asserted scc_counter(~a).~n',[New]).  



/*
 * Set graph orientation. Legal values are 'forward' and
 * 'reverse'. (Actually, anything different from 'forward'
 * is treated as 'reverse'. This flag controls the direction
 * of arcs in the graph. In the second phase it allows to
 * virtually invert the arcs without physically copying the
 * graph.
 */
set_graph_orientation(X) :-
   retractall(graph_orientation(_)),
   assert(graph_orientation(X)).     	
    
    

