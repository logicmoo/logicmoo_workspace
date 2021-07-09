:- module(tree, 
	[	print_tree/1
	,	print_tree/2
	,	empty_tree/2
	,	tree_node/2
	,	tree_canonical/2
	,	maptree/2
	,	maptree/3
	,	node_label/2
	,	node_children/2
	,	tree_cursor/2
	,	tree_depth/2
	,	cursor_node/2
	,	cursor_move/3
	,	cursor_add_sibling/3
	,	cursor_ins_sibling/3
	,	cursor_ins_child/3
	,	get_node/3
	,	set_node/3
	]).

:- meta_predicate maptree(1,?), maptree(2,?,?).

/** <module> Trees

This module provides predicates for manipulated trees. The tree data-type is
defined polymorphically over payload data types A as:

	tree(A) ---> node(A, list(tree(A))).

Thus, in this representation, data of type A is associated with each node
including the root node, and every node has list of child nodes, which will
be empty for leaf nodes.

*/

%% empty_tree(?N:A, ?T:tree(A)) is det.
%
%  Unify T with a root node tree containing N.
empty_tree(A,node(A,[])).

%% tree_node( +T:tree(A), -N:tree(A)) is nondet.
%
%  Unifies N with all the nodes in a tree.
tree_node(T,T).
tree_node(node(_,NX),N) :- member(T,NX), tree_node(T,N).

%% node_label( +N:tree(A), -X:A) is det.
node_label(node(A,_),A).

%% node_children( +N:tree(A), -C:list(tree(A))) is det.
node_children(node(_,C),C).


%% maptree(P:pred(?X:A),?TA:tree(A)) is nondet.
%% maptree(P:pred(?X:A,?Y:B),?TA:tree(A),?TB:tree(B)) is nondet.
%
% Map over tree using unary or binary predicate P.
maptree(P,node(A,AX)) :- call(P,A), maplist(maptree(P),AX).
maptree(P,node(A,AX),node(B,BX)) :- call(P,A,B), maplist(maptree(P),AX,BX).


%% tree_depth( +T1:tree(_), -D:natural) is det.
%
%  Maximum depth of tree.
tree_depth(node(_,[]),0).
tree_depth(node(_,[C1|CX]),N) :-
	maplist(tree_depth,[C1|CX],DX),
	foldl(max,DX,0,M),
	succ(M,N).


max(L,M,N) :- N is max(L,M).
%% tree_canonical( +T1:tree(A), -T2:tree(A)) is det.
%
%  Construct canonical form of a given trie by sorting all children into standard order.
tree_canonical(node(D,CX1),node(D,CX3)) :-
	sort(CX1,CX2), maplist(tree_canonical,CX2,CX3).


print_node(NA,T,T2) :-
	with_output_to(string(AA),print(node(NA))), string_length(AA,L),
	with_output_to(string(SA),tab(L)), string_concat(T,SA,T2),
	write(AA). 

%% print_tree( +T:tree(A)) is det.
%% print_tree( +Pre:atom, +T:tree(A)) is det.
%
%  Prints a drawing of the tree. It uses unicode box-drawing characters to
%  draw the edges so your terminal must be set-up correctly to show these
%  properly. A node with data X is labeled with whatever print(node(X)) 
%  produces and so can be customised by declaring clauses of  user:portray/1.
%  If the prefix Pre is supplied, the tree is started at the current position
%  in the output stream, but subsequent new lines are prefixed with Pre,
%  to allow arbitrary indenting.
%
%  If the child list for any node is a frozen variable, the variable is
%  unfrozen.

print_tree(T) :- write(' '), print_tree(' ',T), nl. 
print_tree(_,V) :- var(V), !, write('_').
print_tree(T,node(NA,CX)) :-
	print_node(NA,T,T2),
	unfreeze_list(CX),
	print_subtree(T2,first,CX).

print_subtree(_,first,CX) :- var(CX), !, write('\u2500').
print_subtree(_,_,Z) :- (var(Z);Z=[]), !.

print_subtree(T,first,[C1|CX]) :- 
	write_symbols(first,CX),
	print_subtree_x(T,C1,CX).

print_subtree(T,rest,[C1|CX]) :- 
	nl, write(T), write_symbols(rest,CX),
	print_subtree_x(T,C1,CX).

print_subtree_x(T,C1,CX) :-
	list_prefix(CX,T,T2),
	print_tree(T2,C1), 
	print_subtree(T,rest,CX).

write_symbols(first,V) :- nonvar(V), V=[], !, write('\u2500\u2500\u2500').
write_symbols(rest,V)  :- nonvar(V), V=[], !, write(' \u2514\u2500').
write_symbols(first,_) :- write('\u2500\u252C\u2500').
write_symbols(rest,_)  :- write(' \u251C\u2500').

list_prefix(V,T,T2) :- (var(V);V=[]), !, atom_concat(T,'   ',T2).
list_prefix(_,T,T2) :- atom_concat(T,' \u2502 ',T2).

unfreeze_list(X) :- 
	frozen(X,G), 
	(G=true->true;(X=[_|_];X=[])).

%% tree_cursor( +T:tree(A), -C:cursor(A)) is det.
%
%  Constructs a cursor representing the given tree and a current position
%  within that tree. Uses a zipper-like data type, as described in the functional
%  programming literature. Initial position is the root node.
tree_cursor(Root,  cursor(Root,top)).


%% get_node(-N:tree(A))// is det.
%  Gets the subtree N at the current cursor position.
get_node(N,cursor(N,P),cursor(N,P)).

%% set_node(+N:tree(A))// is det.
%  Replaces the subtree at the current cursor with N.
set_node(N,cursor(_,P),cursor(N,P)).

%% cursor_node( +C:cursor(A), -N:tree(A)) is det.
%
%  Relation between a cursor and the subtree at the current position.
cursor_node(cursor(N,_),N).

%% cursor_move( +Dir:oneof([down,up,left,right]), +C1:cursor(A), -C2:cursor(A)) is semidet.
%
%  Move a cursor around the tree. Dir can be one of:
%  * up - move to the parent of the current node if not at the root already.
%  * down - move to the current node's first child if it exists.
%  * right - move to the current node's next sibling if there is one.
%  * left - move to the previous sibling if not already the first.

cursor_move(down,  cursor(node(D,[C1|CX]),Path),     cursor(C1,point(Path,D,[],CX))).
cursor_move(right, cursor(N,point(Up,D,LX,[R|RX])),  cursor(R,point(Up,D,[N|LX],RX))).
cursor_move(left,  cursor(N,point(Up,D,[L|LX],RX)),  cursor(L,point(Up,D,LX,[N|RX]))).
cursor_move(up,    cursor(N,point(Up,D,Left,Right)), cursor(node(D,CX),Up)) :- 
	rev_append(Left,[N|Right],CX).

%% cursor_add_sibling( ?N:tree(A), +C1:cursor(A), -C2:cursor(A)) is det.
%
%  Add a N sibling after the current node and move the cursor to it.
cursor_add_sibling(M, cursor(N,point(Up,D,LX,RX)), cursor(M,point(Up,D,[N|LX],RX))).


%% cursor_ins_sibling( ?N:tree(A), +C1:cursor(A), -C2:cursor(A)) is det.
%
%  Insert a sibling N before the current node and move the cursor to it.
cursor_ins_sibling(M, cursor(N,point(Up,D,LX,RX)), cursor(M,point(Up,D,LX,[N|RX]))).

%% cursor_ins_child( ?N:tree(A), +C1:cursor(A), -C2:cursor(A)) is det.
%
%  Insert a child of the current node as first in the list of children and 
%  move down to the newly inserted node.
cursor_ins_child(M,   cursor(node(X,CX),Path),     cursor(M,point(Path,X,[],CX))).

rev_append([],RX,RX).
rev_append([L|LX],RX,LR) :- rev_append(LX,[L|RX],LR).

