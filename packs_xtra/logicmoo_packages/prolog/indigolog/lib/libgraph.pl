%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/libgraph.pl
%
%  Author    : Sebastian Sardina
%  Time-stamp: <03/12/26 20:05:04 ssardina>
%  email     : ssardina@cs.toronto.edu
%  WWW       : www.cs.toronto.edu/~ssardina
%  TESTED    : SWI Prolog 5.0.10 http://www.swi-prolog.org
%	       ECLiPSe 5.3 on RedHat Linux 6.2-7.2
%  TYPE CODE : system independent predicates
%
% DESCRIPTION: Library for implementing graphs
%
% This file provides primitive 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             December 26, 2003
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
%
%                          All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Primitive actions are called using perform/3:
%
% -- perform(CommandList)
%       CommandL is a list-char codifying the command to be executed
%
%  OBS: If ActionT is the term containing the primitive action to be
%       called, your code using internet.pl should do something like this:
%
%       term_to_atom(perform(ActionT), ActionA), % Convert into a list-chars
%       concat_atom(['eclipse -b ', Dir, 'lib/internet.pl -e ','\'', 
%                     ActionA,'\''], Command),
%       exec(Command). 
%
% or issue the shell comamnd:
%
%     eclipse -b <path>/internet.pl -e 'perform(<ActionAsListChar>'
%
%
% The following low-level internet/system primitive actions are provided
% (i.e., they are legal commands to be used with perform/1)
%
% -- browser_new(+ID) 
%        create a new web browser called IdWeb
% -- browser_close(+ID)
%        remove web browser IdWeb
% -- browser_refresh(+ID)
%        refresh content of URL
% -- browser_open(+ID, +URL)
%        set browser IdWeb to URL
%
%  -- check_string_after(+URL, +S, +PAfter) 
%        sense whether there is a string S in in address A after pos PAfter
%  -- check_pos_string(+A, +S, +PAfter) 
%        sense the position in address A of string S after position PAfter
%  -- read_string_between(+A, +D1, +D2, +PAfter): 
%        sense the string between D1 and D2 in address A after pos PAfter
%  -- read_string_length(+URL, +D, +L, +PAfter): 
%        sense the string in address A that starts with string D for 
%        a length of L and after position PAfter
%  -- read_html_field(+URL, +FieldName, +Cont, +PAfter): 
%        sense the next string value of field FieldName after position PAfter
%        that includes string Cont
%  -- download(+URL, +File) 
%        download address URL to file File  sense the process id number
%  -- check_web_file(+URL)
%        senses whether WebFile exists
%
%  -- sense_proc_term(+Pid)
%        senses whether process Pid is finished
%  -- kill_proc(+Pid)
%        kills process Pid
%  -- wait_proc(+Pid)
%        waits for process Pid to finish
%  -- sense_proc_exists(+Pid) 
%        sesnes whether process Pid exists
%  -- sense_file_exists(+File) 
%        senses whether file File exists
%  -- say(+Phrase, +Language)
%        speak Phrase in Language (requires a voice synthesis 
%        like festival)
%
% The following actions may generate the following exogenous actions:
%     
%  -- int_bool_after(URL, String, PAfter, Bool) :
%         Bool=there is String after PAfter in URL
%  -- int_pos(URL, String, PAfter, Pos) :
%         Pos=the next String after PAfter in URL is at position Pos
%  -- int_string_between(URL, Del1, Del2, PAfter, String) :
%         String=string between Del1 and Del2 in URL after PAfter
%  -- int_string_length(URL, Del, Lenght, PAfter, String) :
%         String=string after Del1 of length Length in URL after PAfter
%  -- int_html_field(URL, FieldName, PAfter, String, Pos) :
%         String=value of field FieldName containing Cont in URL after PAfter
%         Pos=starting postition of String
%  -- int_bool_download(URL, File, Status) :
%         Downloading of URL to File finished with Status (ok/failed)
%  -- int_bool_urlexists(URL, Bool) :
%         Bool= URL exists
%  -- int_proc_waited(Pid, Status)
%         Status = result of waiting process Pid (may be failed)
%
%
% REQUIRED:
%
% -- compat_swi/compat_ecl compatibility libraries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(libgraph,
	[getNode/2,
	 getNEdge/4,
	 getEdge/4,
	 %
	 add_node/3,
	 add_edges/3,
	 add_edge/3,
	 add_nedges/3,
	 remove_node/3,
	 remove_edges/3,
	 remove_nedges/3,
	 combine_nodes/4,
	 %
	 path_graph_short/5,
	 path_graph/5,
	 %
	 sub_graph/3
 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GRAPH REPRESENTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A GRAPH HAS THE FOLLOWING STRUCTURE:
%    graph(Nodes, Edges) or graph(Nodes, Edges, NEdges) WHERE:
%  
% - Nodes is a list of nodes, e.g., Nodes=[station(1),station(2),station(3)]
%
% - Edges is a list of edges of the form "edge(Node1,Node2,Annot)"
%   where Node1, Node2 are nodes and Annot is any anottation 
%          e.g., edge(station(1),station(2),north)          
%
% - NEdges describes the edges that are NOT present in the graph
%   (this is used to specify incompletely defined graphs)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% QUERY TOOLS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% There is an edge in the graph from X to Y with annotation A
getEdge(X,Y,A,graph(Nodes,Edges)):- getEdge(X,Y,A,graph(Nodes,Edges,[])).
getEdge(X,Y,A,graph(_,Edges,_))  :- member(edge(X,Y,A), Edges).

% There is an nedge in the graph from X to Y with annotation A
getNEdge(X,Y,A,graph(_,_,NEdges))  :- member(edge(X,Y,A), NEdges).

% N is a node in the graph
getNode(N,graph(Nodes,_))   :- getNode(N,graph(Nodes,_,_,[])).
getNode(N,graph(Nodes,_,_)) :- member(N,Nodes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIMITIVE DYNAMIC OPERATIONS ON A GRAPH (ADD AND REMOVE NODES/EDGES)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Add a node N to a graph 
%
add_node(N, graph(Nodes,Edges,NEdges), graph([N|Nodes],Edges,NEdges)).
add_node(N, graph(Nodes,Edges), graph([N|Nodes],Edges)).

%
% Add a list of edges to a graph
%
add_edges([], G, G) :- !.
add_edges([edge(X,Y,D)|Tail], G, GN) :- 
	\+ getEdge(X,Y,D,G), !,
	add_edge(edge(X,Y,D),G,G2),   % Add this single edge
	add_edges(Tail, G2, GN).
add_edges([_|Tail], G, GN) :- !, add_edges(Tail, G, GN).

% Add one edge to a graph
add_edge(E, graph(Nodes,Edges), graph(Nodes,[E|Edges])).
add_edge(E, graph(Nodes,Edges,NEdges), graph(Nodes,[E|Edges],NEdges)).

add_nedges([], G, G) :- !.
add_nedges([edge(X,Y,D)|Tail], G, GN) :- 
	\+ getNEdge(X,Y,D,G), !,
	add_nedge(edge(X,Y,D),G,G2),  % Add this signle edge
	add_nedges(Tail, G2, GN).
add_nedge([_|Tail], G, GN) :- !, add_nedges(Tail, G, GN).

% Add one nedge to a graph
add_nedge(NE, graph(Nodes,Edges,NEdges), graph(Nodes,Edges,[NE|NEdges])).


%
% Remove node N from a graph (i.e., remove node and all its edges)
%
remove_node(N, G, GN) :-
	remove_edges([edge(N,_,_),edge(_,N,_)], G, G2),
	remove_nedges([edge(N,_,_),edge(_,N,_)], G2, G3),
	delete_node(N, G3, GN).
	
delete_node(N,graph(Nodes,Edges), graph(Nodes2, Edges)) :-
delete_node(N,graph(Nodes,Edges,[]), graph(Nodes2, Edges,_)).
delete_node(N,graph(Nodes,Edges, NEdges), graph(Nodes2, Edges, NEdges)) :-
	remove_element(Nodes, N, Nodes2).


%
% Remove a list of edges E from a graph
%
remove_edges(E, graph(Nodes, Edges), graph(Nodes, Edges2)) :- 
	remove_edges(E, graph(Nodes, Edges, []), graph(Nodes, Edges2, _)).
remove_edges(E, graph(Nodes, Edges, NEdges), graph(Nodes, Edges2, NEdges)) :- 
	subtractvar(Edges, E, Edges2).
remove_nedges(E, graph(Nodes, Edges, NEdges), graph(Nodes, Edges, NEdges2)) :- 
	subtractvar(NEdges, E, NEdges2).





%
% Combine nodes N1 and N2 in G into a single node N2. GNew is new combined graph
%
combine_nodes(N1,N2,G,GNew) :-
	allSolutions(edge(N2,X,D), getEdge(N1,X,D,G), L1),
	allSolutions(edge(X,N2,D), getEdge(X,N1,D,G), L2),
	append(L1,L2,LE),  % Edges that may have to be incorporated to node N2
	%
	allSolutions(edge(N2,X,D), getNEdge(N1,X,D,G), LN1),
	allSolutions(edge(X,N2,D), getNEdge(X,N1,D,G), LN2),
	append(LN1,LN2,LNE),  % NEdges that may have to be incorporated to node N2
	%
	remove_node(N1,G,G2),  % Remove node N1
	add_edge(LE, G2, G3), 
	add_nedge(LNE, G3, GNew).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PATH COMPUTATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A PATH is a sequence (list) of node names.
% e.g., [station(1),station(5),station(6),station(2)] is a path 
%       from station(1) to station(2)

% P is the shortest path in G from X to Y (and with length less than Limit)
path_graph_short(X, Y, G, Limit, P) :- path_graph_short(X,Y,G,0,Limit, P).

path_graph_short(X,X,_,0,[])  :- fail, !.
path_graph_short(X,Y,G,N,_,P) :- path_graph(X,Y,G,N,P).
path_graph_short(X,Y,G,N,L,P) :- 
	L2 is L-1, N2 is N+1, path_graph_short(X,Y,G,N2,L2,P).

% path_graph(X, Y, G, L, P): P is a path of length L from X to Y in graph G
path_graph(X, Y, graph(_,E), L, Path) :-
	path_graph(X, Y, graph(_,E,[]), L, Path).
path_graph(Y, X, graph(_,E,_), L, [Y|LV]) :- 
	length(LV, L), % Build a list LV of variables of length L
	path1_graph(E, X, [Y|LV]). 

path1_graph(_, X, [X]).
path1_graph(E, X, [Y | Path1]) :- 
	member(edge(Z,Y,_), E),  % We can go from Y to Z with an edge in E
%	append(GList, VList, Path1),
%	ground(GList), 
%	\+ member(Z, Path1),
	Path1=[Z|_],
	path1_graph(E, X, Path1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUBGRAPH COMPUTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sub_graph(G1, G2, Map): 
%   the (possibly) incomplete graph G2 is an exact subgraph of G1 under mapping Map
sub_graph(graph(_, Edges1), G2, Map) :-
	abstract_graph(G2, graph(_,AEdges2, ANEdges2), Map),
	sublist(AEdges2, Edges1),
	\+ (member(E, ANEdges2), member(E, Edges1)).


% abstract_graph(G, AG, Map):
%       AG is a (variable) abstraction graph of a concrete graph G. 
%   An abstraction graph results from replacing each node with a unique variable
%   Map = list of (X,N) where X is a node in AG and N is a node in G, X-->N
abstract_graph(graph(Nodes,Edges), graph(ANodes,AEdges,ANEdges), Map) :-
	abstract_graph(graph(Nodes,Edges,[]), graph(ANodes,AEdges,ANEdges), Map).
abstract_graph(graph([],Edges,NEdges), graph([],Edges,NEdges), []).
abstract_graph(graph([N|R],Edges,NEdges),graph([X|RA],EdgesA,NEdgesA),[(X,N)|Map]):-
	abstract_node(N,X,Edges,Edges2),
	abstract_node(N,X,NEdges,NEdges2),
	abstract_graph(graph(R,Edges2,NEdges2), graph(RA, EdgesA,NEdgesA), Map).

% abstract_node(+N,-X,+Edges,-EdgesA): 
%   EdgesA is Edges with all ground node N replaced by variable X
abstract_node(_,_,[],[]).
abstract_node(N,X,[edge(S,D,A)|R],[edge(X,D,A)|RA]) :- 
	S==N, !,
	abstract_node(N,X,R,RA).
abstract_node(N,X,[edge(S,D,A)|R],[edge(S,X,A)|RA]) :- 
	D==N, !,
	abstract_node(N,X,R,RA).
abstract_node(N,X,[edge(S,D,A)|R],[edge(S,D,A)|RA]) :- 
	abstract_node(N,X,R,RA).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OTHER TOOLS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Like setof/3 but returns an empty list of G fails
allSolutions(T, G, L) :- setof(T,G,L) -> true ; L=[].


% A version of substract/3 in SWI that deals with variables as elements
substractvar(L1,[],L1).
substractvar(L1,[E|Tail],L2) :-
	remove_element(L1,E,L3),
	substractvar(L3,Tail,L2).

% Remove all elements E from a list (handle variables in the list)
remove_element([],_,[]).
remove_element([X|L1],E,L2) :- 
	E==X, !,
	remove_element(L1,E,L2).
remove_element([X|L1],E,[X|L2]) :-
	remove_element(L1,E,L2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: lib/libgraph.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
