
% Misc DCTG utilities.
% B. Ross
% January 1999

% Pretty-printer...

prettyprint(Tree) :-
	pretty(Tree, 0),
	!.

pretty(node(Name, Kids, ID), Tab) :-
	T is Tab*4,
	tab(T), 
	writel2([Name, ' (#', ID, ',d ', Tab,')', nl]),
	Tab2 is Tab + 1,	
	prettykids(Kids, Tab2),
	!.
pretty(Value, Tab) :-
	T is Tab*4,
	tab(T), 
	writel([Value, nl]),
	!.

prettykids([], _) :- !.
prettykids([Node|Rest], Tab) :-
	pretty(Node, Tab),
	prettykids(Rest, Tab),
	!.

% DCTG tree depth measurer...

tree_depth(node(_, Kids, _), D) :-
	tree_depth_kids(Kids, D2),
	D is D2 + 1,
	!.
tree_depth(_, 1) :- !.

tree_depth_kids([], 0) :- !.
tree_depth_kids([Node|Rest], D) :-
	tree_depth(Node, D2),
	tree_depth_kids(Rest, D3),
	D is max(D2, D3),
	!.

% listprint converts tree to list, using DCTG verification

listprint(Tree) :-
	user_args_P(UserArgs),
	verification(Tree, UserArgs, List),
	writel2(List),
	nl,
	!.

