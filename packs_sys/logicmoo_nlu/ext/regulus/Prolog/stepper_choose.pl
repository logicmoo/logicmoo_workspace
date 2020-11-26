
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(stepper_choose,
	  [choose_item_from_menu/2,
	   choose_rule_from_menu/2,
	   choose_cut_node_from_menu/3]
	 ).

%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/stepper_read').
%:- use_module('$REGULUS/Prolog/stepper_rule_db').
:- use_module('$REGULUS/Prolog/stepper_item_db').

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%----------------------------------------------------------------------

choose_item_from_menu([], _ChosenItem) :-
	!,
	format('~NCan\'t find anything~n~n', []),
	fail.
choose_item_from_menu([ChosenItem], ChosenItem) :-
	!.
choose_item_from_menu(PossibleItems, ChosenItem) :-
	items_to_menu_alist(PossibleItems, MenuAlist),
	Question = 'Select item:',
	choose_from_stepper_text_menu(Question, MenuAlist, ChosenItem).

choose_rule_from_menu(PossibleRules, ChosenRule) :-
	choose_item_from_menu(PossibleRules, ChosenRule).

items_to_menu_alist(Items, MenuAlist) :-
	items_to_item_feats_alist(Items, ItemFeatsAlist),
	keep_only_distinguishing_feats(ItemFeatsAlist, ItemFeatsAlist1),
	item_feats_alist_to_menu_alist(ItemFeatsAlist1, MenuAlist),
	!.
items_to_menu_alist(Items, MenuAlist) :-
	format2error('~N*** Error: bad call: ~w~n', [items_to_menu_alist(Items, MenuAlist)]),
	fail.

%----------------------------------------------------------------------

items_to_item_feats_alist(Items, ItemFeatsAlist) :-
	items_to_item_feats_alist1(Items, 1, ItemFeatsAlist).

items_to_item_feats_alist1([], _I, []).
items_to_item_feats_alist1([F | R], I, [F-Feats | R1]) :-
	item_to_feats(F, I, Feats),
	I1 is I + 1,
	!,
	items_to_item_feats_alist1(R, I1, R1).

item_to_feats(Item, I, OrderedFeats) :-
	findall(GroundedFeat,
		(   feat_for_item(Item, Feat),
		    copy_term(Feat, GroundedFeat),
		    make_ground(GroundedFeat)
		),
		GroundedFeats0),
	GroundedFeats = [id=I | GroundedFeats0],
	order_feats(GroundedFeats, OrderedFeats),
	!.

order_feats(Feats, OrderedFeats) :-
	tag_feats_by_priority(Feats, TaggedFeats),
	keysort(TaggedFeats, OrderedTaggedFeats),
	unkey_list(OrderedTaggedFeats, OrderedFeats),
	!.
order_feats(Feats, OrderedFeats) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [order_feats(Feats, OrderedFeats)]),
	fail.

tag_feats_by_priority([], []).
tag_feats_by_priority([(Feat=Value) | R], [Priority-(Feat=Value) | R1]) :-
	priority_for_feat_and_value(Feat, Value, Priority),
	!,
	tag_feats_by_priority(R, R1).

priority_for_feat_and_value(cat, _Val, 1.0) :- !.
priority_for_feat_and_value(subcat, Val, 2.0) :-
	\+ var_or_grounded_var(Val),
	!.
priority_for_feat_and_value(sem, Val, 3.0) :-
	\+ term_contains_functor(Val, concat/2),
	\+ var_or_grounded_var(Val),
	!.
priority_for_feat_and_value(rule, _Val, 6.0) :- !.
priority_for_feat_and_value(line_info, _Val, 7.0) :- !.
priority_for_feat_and_value(sem, Val, 8.0) :-
	term_contains_functor(Val, concat/2),
	!.
priority_for_feat_and_value(tree, _Val, 9.0) :- !.
priority_for_feat_and_value(sem, Val, 10.0) :-
	var_or_grounded_var(Val),
	!.
priority_for_feat_and_value(id, _Val, 12.0) :- !.
priority_for_feat_and_value(_Other, _Val, 8.0) :- !.
 
%----------------------------------------------------------------------

keep_only_distinguishing_feats(ItemFeatsAlistIn, ItemFeatsAlistOut) :-
	remove_feats_common_to_all_items(ItemFeatsAlistIn, ItemFeatsAlistNext),
	remove_unnecessary_feats(ItemFeatsAlistNext, ItemFeatsAlistOut).

remove_feats_common_to_all_items(ItemFeatsAlistIn, ItemFeatsAlistOut) :-
	remove_feat_common_to_all_items(ItemFeatsAlistIn, ItemFeatsAlistNext),
	!,
	remove_feats_common_to_all_items(ItemFeatsAlistNext, ItemFeatsAlistOut).
remove_feats_common_to_all_items(ItemFeatsAlistIn, ItemFeatsAlistIn).

remove_feat_common_to_all_items(ItemFeatsAlistIn, ItemFeatsAlistOut) :-
	ItemFeatsAlistIn = [_Item-Feats | Rest],
	member(Feat=Val, Feats),
	feat_val_pair_is_in_all_items(Feat=Val, Rest),
	remove_feat_val_pair_from_all_items(ItemFeatsAlistIn, Feat=Val, ItemFeatsAlistOut),
	!.

feat_val_pair_is_in_all_items(_Feat=_Val, []).
feat_val_pair_is_in_all_items(Feat=Val, [F | R]) :-
	feat_val_pair_is_in_item(Feat=Val, F),
	!,
	feat_val_pair_is_in_all_items(Feat=Val, R).

feat_val_pair_is_in_item(Feat=Val, _Item-Pairs) :-
	member(Feat=Val1, Pairs),
	Val == Val1,
	!.

remove_feat_val_pair_from_all_items([], _Feat=_Val, []).
remove_feat_val_pair_from_all_items([F | R], Feat=Val, [F1 | R1]) :-
	remove_feat_val_pair_from_item(F, Feat=Val, F1),
	!,
	remove_feat_val_pair_from_all_items(R, Feat=Val, R1).

remove_feat_val_pair_from_item(Item-Pairs, Feat=Val, Item-Pairs1) :-
	remove_feat_val_pair_from_pairs(Pairs, Feat=Val, Pairs1),
	!.

remove_feat_val_pair_from_pairs([Feat=Val1 | R], Feat=Val, R) :-
	Val == Val1,
	!.
remove_feat_val_pair_from_pairs([F | R], Feat=Val, [F | R1]) :-
	remove_feat_val_pair_from_pairs(R, Feat=Val, R1).

%----------------------------------------------------------------------

remove_unnecessary_feats(ItemFeatsAlistIn, ItemFeatsAlistOut) :-
	remove_unnecessary_feats1(ItemFeatsAlistIn, DifferentiatedFeatsAlist),
	collapse_differentiated_feats_alist(DifferentiatedFeatsAlist, ItemFeatsAlistOut).

remove_unnecessary_feats1([Item-_Feats], item(Item)) :-
	!.
remove_unnecessary_feats1(ItemFeatsAlistIn, DifferentiatedFeatsAlist) :-
	split_by_first_feature(ItemFeatsAlistIn, DifferentiatedFeatsAlist0),
	!,
	(   DifferentiatedFeatsAlist0 = [_KeyVal-ItemFeatsAlistNext] ->
	    remove_unnecessary_feats1(ItemFeatsAlistNext, DifferentiatedFeatsAlist) ;
	    
	    remove_unnecessary_feats2(DifferentiatedFeatsAlist0, DifferentiatedFeatsAlist)
	).

split_by_first_feature(ItemFeatsAlistIn, DifferentiatedFeatsAlist) :-
	empty_assoc_generic(AssocIn),
	split_by_first_feature1(ItemFeatsAlistIn, AssocIn-AssocOut),
	assoc_generic_to_list(AssocOut, DifferentiatedFeatsAlist),
	!.
	
split_by_first_feature1([], AssocIn-AssocIn).
split_by_first_feature1([F | R], AssocIn-AssocOut) :-
	F = Item-[Feat=Val | RestFeats],
	get_assoc_generic_or_empty_list(Feat=Val, AssocIn, InList),
	NextList = [Item-RestFeats | InList],
	put_assoc_generic(Feat=Val, AssocIn, NextList, AssocNext),
	!,
	split_by_first_feature1(R, AssocNext-AssocOut).

remove_unnecessary_feats2([], []).
remove_unnecessary_feats2([F | R], [F1 | R1]) :-
	remove_unnecessary_feats3(F, F1),
	!,
	remove_unnecessary_feats2(R, R1).

remove_unnecessary_feats3((Feat=Val)-ItemFeatsAlist, (Feat=Val)-DifferentiatedFeatsAlist) :-
	remove_unnecessary_feats1(ItemFeatsAlist, DifferentiatedFeatsAlist).

collapse_differentiated_feats_alist(DifferentiatedFeatsAlist, ItemFeatsAlist) :-
	findall(Item-FeatValPairs,
		path_through_differentiated_feats_alist(DifferentiatedFeatsAlist, FeatValPairs, Item),
		ItemFeatsAlist).

path_through_differentiated_feats_alist(Pair-item(Item), [Pair], Item) :-
	!.
path_through_differentiated_feats_alist(Pair-List, [Pair | RestPairs], Item) :-
	!,
	member(DifferentiatedFeatsAlist, List),
	path_through_differentiated_feats_alist(DifferentiatedFeatsAlist, RestPairs, Item).
path_through_differentiated_feats_alist(List, Pairs, Item) :-
	is_list(List),
	!,
	member(DifferentiatedFeatsAlist, List),
	path_through_differentiated_feats_alist(DifferentiatedFeatsAlist, Pairs, Item).

get_assoc_generic_or_empty_list(Key, Assoc, Val) :-
	get_assoc_generic(Key, Assoc, Val),
	!.
get_assoc_generic_or_empty_list(_Key, _Assoc, []).

%----------------------------------------------------------------------

item_feats_alist_to_menu_alist([], []).
item_feats_alist_to_menu_alist([Item-Feats | R], [Item-Text | R1]) :-
	feats_to_text(Feats, Text),
	!,
	item_feats_alist_to_menu_alist(R, R1).

feats_to_text([], '').
feats_to_text([F], FText) :-
	feat_to_text(F, FText),
	!.
feats_to_text([F | R], Result) :-
	feat_to_text(F, FText),
	feats_to_text(R, RText),
	format_to_atom('~w, ~w', [FText, RText], Result).

feat_to_text(rule=rule(RuleSummary, line_info(_ItemNumber, _Cut, LineFrom-LineTo, File)), Text) :-
	(   LineFrom = LineTo ->
	    format_to_atom('~w~n    line ~d~n    ~w', [RuleSummary, LineFrom, File], Text) ;
	    format_to_atom('~w~n    lines ~d to ~d~n    ~w', [RuleSummary, LineFrom, LineTo, File], Text)
	),
	!.
feat_to_text(line_info=line_info(_ItemNumber, _Cut, LineFrom-LineTo, File), Text) :-
	(   LineFrom = LineTo ->
	    format_to_atom('line ~d in ~w', [LineFrom, File], Text) ;
	    format_to_atom('lines ~d to ~d in ~w', [LineFrom, LineTo, File], Text)
	),
	!.
feat_to_text(tree=Tree, Text) :-
	with_output_to_chars(prettyprint_parse_tree(Tree, dont_print_line_info),
			     TreeString0),
	append("\n", TreeString0, TreeString),
	atom_codes(Text, TreeString),
	!.
feat_to_text(Feat, Text) :-
	format_to_atom('~w', [Feat], Text).	

%----------------------------------------------------------------------

choose_cut_node_from_menu([], _Item, _NodeID) :-
	!,
	format('~N~nNo cut nodes found in upper item~n~n', []),
	fail.
choose_cut_node_from_menu([SingleCutNode], _Item, SingleCutNode) :-
	!.
choose_cut_node_from_menu(CutNodes, Item, NodeID) :-
	format('~N~n', []),
	feat_for_item(Item, tree=Tree),
	prettyprint_parse_tree(Tree, print_line_info_and_numbers),
	choose_number_from_list('~N~nChoose one of the following nodes to join on: ~w ',
				'~N"~w" is not an available cut node',
				CutNodes,
				NodeID).

 
