
% DCTG based GP reproduction operators: crossover & mutation
% Brian Ross
% January 25, 1999

% crossover(Parent1, Parent2, Child1, Child2):
%	Parent1, Parent2 - parent trees to reproduce
%	Child1, Child2 - resulting children 
% Performs grammar tree expression crossover on two parents.
% If internal crossover probability set, then nodes of the specified type
% are selected; else all nodes initially counted.
% The rules for crossover are:
%	- only nodes of same rule name from each parent are crossed 
%	- crossover is attempted a max N number of times until successful
%	  (user-specified parameter)
% 	- an attempt fails if the offspring exceed max depth parameter
% 	- if no internal/leaf counting, then counts on all node names done.
% 	- if internal/leaf counting to be done (case 1), then it is done only
%	  for one parent. (If it fails, then 2nd parent tried; if that fails,
%	  then all nodes counted from first parent). Other parent just uses 
%	  terminal name count (increases odds that a crossover will be 
%	  possible).

crossover(P1, P2, C1, C2) :-    % case 2
	prob_internal_crossover_P(PI),
	\+ (P1 == no),
	(maybe(PI) -> Type=internal ; Type=leaf),
	reprod_P(Tries),
	(once(count_nodes(P1, Type, N1)),	
		(Parent1, Parent2) = (P1, P2)
		;
		once(count_nodes(P2, Type, N1)),
		(Parent1, Parent2) = (P2, P1)),
	do_crossover(Tries, Parent1, N1, Parent2, C1, C2),
	!. 
crossover(P1, P2, C1, C2) :-    % case 1
	reprod_P(Tries),
	once(count_nodes(P1, all, N1)),
	do_crossover(Tries, P1, N1, P2, C1, C2),
	!. 

do_crossover(0, _, _, _, _, _) :- 
	!, 
	fail.
do_crossover(_, Parent1, N1, Parent2, Child1, Child2) :-
	my_random(N1, K1),
	%writel(['A:rand pick ', K1, ' from ', N1, '.', nl]),
	select_subtree(Parent1, K1, _, Child1, Subtree1, Subtree2, NodeName),
	count_nodes(Parent2, NodeName, N2),
	my_random(N2, K2),
	%writel(['B:rand pick ', K2, ' from ', N2, ' ', NodeName, ' nodes.', nl]),
	select_subtree(Parent2, K2, _, Child2, Subtree2, Subtree1, NodeName),
	tree_verification(Child1),
	tree_verification(Child2),
	!.
do_crossover(Tries, Parent1, N1, Parent2, Child1, Child2) :-
	Tries2 is Tries - 1,
	%writel(['Try ', Tries2, nl]),
	do_crossover(Tries2, Parent1, N1, Parent2, Child1, Child2),
	!.

% check that a new Tree doesn't fail due to
% failed embedded code in DCTG rules.

tree_verification(Child) :-
	%writel(['tree_verif: testing child:',nl]),
	%prettyprint(Child),
	(reprod_verif_P(yes) ->
		user_args_P(Args),
		verification(Child, Args, _)
		%writel(['tree_verif: verification succeeded.',nl])
		;
		true),
	!.

% count_nodes(Tree, NodeName, NumNodes):
%	Tree - DCTG expression structure 
%	NodeName - name of node to count (otherwise: all nodes = 'all'; 
%			all internal = 'internal'; all leaf = 'leaf')
%	NumNodes - number of nodes in Tree
% Scans Tree and counts number of nodes.

count_nodes(node(_, Children, _), all, NumNodes) :-
	!,
	count_children_nodes(Children, all, NumNodes2),
	NumNodes is NumNodes2 + 1.
count_nodes(node(_, Children, ID), Type, NumNodes) :-
	Type == internal,
	fast:dctg_rule_info(_, ID, _, _, nonterminal),
	!,
	count_children_nodes(Children, Type, NumNodes2),
	NumNodes is NumNodes2 + 1.
count_nodes(node(_, Children, ID), Type, NumNodes) :-
	Type == leaf,
	fast:dctg_rule_info(_, ID, _, _, terminal),
	!,
	count_children_nodes(Children, Type, NumNodes2),
	NumNodes is NumNodes2 + 1.
count_nodes(node(_, Children, ID), NodeName, NumNodes) :-
	fast:dctg_rule_info(NodeName, ID, _, _, _),
	!,
	count_children_nodes(Children, NodeName, NumNodes2),
	NumNodes is NumNodes2 + 1.
count_nodes(node(_, Children, _), NodeName, NumNodes) :-
	!,
	count_children_nodes(Children, NodeName, NumNodes).
count_nodes(_, _, 0).

count_children_nodes([], _, 0).
count_children_nodes([Node|Rest], NodeName, NumNodes) :-
	count_nodes(Node, NodeName, NumNodes2),
	count_children_nodes(Rest, NodeName, NumNodes3),
	NumNodes is NumNodes2 + NumNodes3,
	!.

% select_subtree(Parent, K, K2, NewParent, SubTree, Hole, NodeName):
%	Parent - parent tree structure
%	K - Kth node to select in Parent; must be < number nodes in Parent.
%	K2 - final K during structure traversal
%	NewParent - Parent structure with variable Hole in place of removed
%		subtree Subtree
%	Subtree - subtree to swap
%	Hole - location of hole in ParentWithHole (variable)
%	NodeName - node name of Subtree to select from; if variable, then 
%		select from all nodes
% Selects a Kth node in tree for crossover of type NodeName (or all, if 
% NodeName not set). Sets up the new tree with Hole placeholder for selected 
% subtree. Hole may be already unified with other parent's subtree. 
% Cases:
%	1. Count = 0, var name --> use that node
%	2. Count = 0, name matches given --> use that node
% 	3. Count > 0, var name or name match -> count and continue
%	4. name doesn't match given --> skip and continue
%	5. else stop at given count (we've exhausted tree, and we're at 
%	   non-node component)

select_subtree(node(_, Kids, ID), 1, 0, NewParent, 
		node(NodeName, Kids, ID), NewParent, NodeName) :-  % cases 1, 2
	(var(NodeName) ; fast:dctg_rule_info(NodeName,ID,_,_,_)),
	!,
	fast:dctg_rule_info(NodeName,ID,_,_,_).
select_subtree(node(Name, Kids, ID), K, K2, node(Name, Kids2, ID), 
		Subtree, Hole, NodeName) :- % case 3
	(var(NodeName) ; fast:dctg_rule_info(NodeName,ID,_,_,_)),
	!,
	K3 is K-1,
	select_subtree_children(Kids, K3, K2, Kids2, Subtree, Hole, NodeName).
select_subtree(node(Name, Kids, ID), K, K2, node(Name, Kids2, ID), 
		Subtree, Hole, NodeName) :- % case 4
	!,
	select_subtree_children(Kids, K, K2, Kids2, Subtree, Hole, NodeName).
select_subtree(Node, K, K, Node, _, _, _). % case 5

% select_subtree_children applies select_subtree to list of nodes.

select_subtree_children([], K, K, [], _, _, _) :- !.
select_subtree_children([Node|T], K, K2, [Node2|T2], Subtree, Hole, Name) :- 
	select_subtree(Node, K, K3, Node2, Subtree, Hole, Name),
	(K3 == 0 ->
		T=T2, 
		K3=K2
		;
		select_subtree_children(T, K3, K2, T2, Subtree, Hole, Name)).

debug_crossover :-
	dctg_root_P(Root),
	writel(['Generate tree 1...', nl]),
	generate_tree(Root, full, 6, _, P1, _),
	writel(['Generate tree 2...', nl]),
	generate_tree(Root, full, 6, _, P2, _),
	writel(['Parent1...', nl]),
	prettyprint(P1),
	writel(['Parent2...', nl]),
	prettyprint(P2),
	writel(['Do the crossover...', nl]),
	crossover(P1, P2, C1, C2),
	writel(['Child1...', nl]),
	prettyprint(C1),
	writel(['Child2...', nl]),
	prettyprint(C2).

debug_crossover2 :-
	generate_tree(sentence, grow, 10, _, P1, _),
	generate_tree(sentence, grow, 10, _, P2, _),
	crossover(P1, P2, C1, C2),
	writel(['Parent1...', nl]),
	prettyprint(P1),
	writel(['Parent2...', nl]),
	prettyprint(P2),
	writel(['Child1...', nl]),
	prettyprint(C1),
	writel(['Child2...', nl]),
	prettyprint(C2).

% ---------------------------

% mutation(Parent, Child):
%	Parent - tree to mutate
% 	Child - mutated result
% Performs mutation on a tree. A subtree is randomly selected. Then a
% new subtree of the same type as selected one is generated using grow 
% generation, and it replaces the selected subtree. If the resulting tree 
% is too deep, then it is repeated a maximum number of user-specified times.
% If the user is using terminal mutation probability (Case 1) then all nodes
% of that type (if it succeeds statisticall) are counted. If none exist, then
% all nodes counted (case 2).

mutation(Parent, Child) :-
	reprod_P(Tries),
	do_mutation(Tries, Parent, Child),
	!.

do_mutation(0, _, _) :-
	!,
	fail.
do_mutation(_, Parent, Child) :-   % case 1
	prob_terminal_mutation_P(PT),
	\+ (PT==no),
	(maybe(PT) -> Type=leaf ; Type=internal),
	count_nodes(Parent, Type, N),
	max_depth_P(_, MaxDepth),
	my_random(N, K),
	%writel(['rand pick ', K, ' from ', N, '.', nl]),
	select_subtree(Parent, K, _, Child, _, NewTree, NodeName),
	NewDepth is MaxDepth - 2, % a subtree with a node type has depth > 1
	generate_tree(NodeName, grow, NewDepth, _, NewTree, _),
	tree_verification(Child),
	!.
do_mutation(_, Parent, Child) :-   % case 2
	max_depth_P(_, MaxDepth),
	count_nodes(Parent, all, N),
	my_random(N, K),
	%writel(['rand pick ', K, ' from ', N, '.', nl]),
	select_subtree(Parent, K, _, Child, _, NewTree, NodeName),
	NewDepth is MaxDepth - 2, % a subtree with a node type has depth > 1
	generate_tree(NodeName, grow, NewDepth, _, NewTree, _),
	tree_verification(Child),
	!.
do_mutation(Tries, Parent, Child) :-
	Tries2 is Tries - 1,
	%writel(['Try countdown... ', Tries2, nl]),
	do_mutation(Tries2, Parent, Child),
	!.

debug_mutation :-
	dctg_root_P(Root),
	generate_tree(Root, full, 6, _, Parent, _),
	mutation(Parent, Child),	
	writel(['Parent...', nl]),
	prettyprint(Parent),
	writel(['Child...', nl]),
	prettyprint(Child).

% ---------------------------

% verification(Tree, UserArgs, Expr):
%	Tree - DCTG tree to verify
%	UserArgs - Argument list to pass to DCTG rules 
%	Expr - list expression for Tree
% The DCTG tree is verified by interpreting the Prolog DCTG rules
% in concert with the Tree structure. The purpose of this is to
% execute any embedded Prolog in the rules, which are 
% not retained in the tree data structure itself. User args as set by user_args
% parameter are also used (those embedded in Prolog structure are irrelevant).
% This routine may cause a tree to fail, in that embedded Prolog goals or
% user args fail.

% verification embeds user args into initial call of tree.

verification(node(Name, Kids, ID), UserArgs, Expr) :-
	fast:dctg_rule_info(_, ID, Call, _, _),
	Call =.. [Name|Args],
	append(_, [node(X,Y,Z),Expr,_], Args),
	append(UserArgs, [node(X,Y,Z),Expr,[]], Args2), 
	RuleHead2 =.. [Name|Args2],     % embed user args, empty diff list
	!,
	verify_tree(RuleHead2, node(Name, Kids, ID)).

verify_tree(Call, node(_, Kids, ID)) :-
	clause(Call, Body),
	same_id(Call, ID),
	!,
	%writel(['verify_tree: Call=', Call, 'node = ', N, ID, nl]),
	%writel(['verify_tree: Body= ', Body, 'Kids=', Kids,nl]),
	verify_kids(Body, Kids, _).
verify_tree(_, _) :- 
	%writel(['verify_tree: failed', nl]),
	!,
	fail.
	
verify_kids((A,B), Kids, Kids3) :-
	!,
	verify_kids(A, Kids, Kids2),
	verify_kids(B, Kids2, Kids3).
verify_kids(A, [node(_, Kids, ID)|Rest], Rest) :-
	is_a_rule_call(A),
	!,
	%writel(['v_k 2: Call=', A, 'Node name = ', N, ID, nl]),
	verify_tree(A, node(_, Kids, ID)).
verify_kids(c(A,X,B), [[H]|T], T) :-  % single constant
 	!,
 	% X == H,
 	X = H,
 	%writel(['v_k 3: Call=', c(A,X,B), 'List=', [[H]|T], nl]),
 	c(A,X,B).
verify_kids(c(A,X,B), [[H|T2]|T], [T2|T]) :-  % multiple constants
	!,
	% X == H,
	X = H,
	%writel(['v_k 4: Call=', c(A,X,B), 'List=', [[H|T2]|T], nl]),
	c(A,X,B).
verify_kids(A, Kids, Kids) :-
	!,
	%writel(['v_k 5: Call=', A, 'Kids=', Kids, nl]),
	call(A).

% Warning: user cannot use node/3 structure in their user arg fields!

same_id(Call, ID) :-
	Call =.. [_|Args],
	member(node(_, _, ID2), Args),
	% append(_, [node(_, _, ID)|_], Args),
	!,
	ID==ID2.

