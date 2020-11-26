
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(stepper_item_db,
	  [get_stepper_mode/1,
	   set_stepper_mode/1,

	   init_stepper_item_database_if_necessary/0,
	   init_stepper_item_database/0,
	   renumber_stepper_item_database/2,
	   change_info_for_delete_all/1,
	   
	   get_item/2,
	   get_list_of_items/2,
	   add_new_item/2,
	   add_new_item_list/2,
	   delete_item/1,
	   delete_list_of_items/1,
	   move_item_first/1,
	   move_item_last/1,
	   replace_item/2,
	   mark_out_of_date_items/1,
	   
	   display_item/1,
	   display_cut_node/2,
	   package_item_for_java_gui/2,
	   package_cut_node_for_java_gui/3,
	   display_rule_for_item/1,
	   rule_string_for_item/2,
	   
	   check_is_item_id/1,
	   check_is_list_of_item_ids/1,
	   
	   make_item_canonical/2,
	   mark_cut_at_node_in_item/4,
	   
	   print_summary/0,
	   print_summary_to_list/1,
	   print_summary_for_item/2,
	   print_summary_for_item/3,
	   
	   feat_for_item/2,
	   tree_for_item/2,
	   syn_feats_for_item/2,

	   summarise_item_list/2,
	   summarise_lhs/2,
	   summarise_rhs/2,
	   summarise_goal_list/2]
	 ).

%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/java_gui_utils').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_eval').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%----------------------------------------------------------------------

line_length_for_gui(50).

%----------------------------------------------------------------------

:- dynamic stepper_mode/1.

get_stepper_mode(Mode) :-
	stepper_mode(Mode),
	!.
get_stepper_mode(debug).

set_stepper_mode(Mode) :-
	retractall(stepper_mode(_)),
	assertz(stepper_mode(Mode)).

%----------------------------------------------------------------------

% stepper_item(ItemID, Item).

:- dynamic stepper_item/2.

init_stepper_item_database_if_necessary :-
	\+ next_item_id(_),
	init_stepper_item_database,
	!.
init_stepper_item_database_if_necessary.

init_stepper_item_database :-
	retractall(stepper_item(_, _)),
	init_next_item_id_counter,
	!.

renumber_stepper_item_database(DeletedIDs, ChangeInfo) :-
	findall(change(DeletedNum, 0),
		member(DeletedNum, DeletedIDs),
		DeletedList),
	findall(ItemNum-Item,
		stepper_item(ItemNum, Item),
		ChangedPairs),
	init_stepper_item_database,
	add_items_list_after_renumbering(ChangedPairs, ChangeList0),
	append(DeletedList, ChangeList0, ChangeList),
	ChangeInfo =.. [changes | ChangeList],
	!.
renumber_stepper_item_database(DeletedIDs, ChangeInfo) :-
	format2error('~N*** Error: bad call: ~w~n', [renumber_stepper_item_database(DeletedIDs, ChangeInfo)]),
	fail.

add_items_list_after_renumbering([], []).
add_items_list_after_renumbering([F | R], [F1 | R1]) :-
	add_item_after_renumbering(F, F1),
	!,
	add_items_list_after_renumbering(R, R1).

add_item_after_renumbering(OldID-Item, change(OldID, NewID)) :-
	get_next_item_id(NewID),
	assertz(stepper_item(NewID, Item)),
	!.

change_info_for_delete_all(ChangeInfo) :-
	findall(change(Id, 0),
		stepper_item(Id, _),
		ChangeList),
	ChangeInfo =.. [changes | ChangeList],
	!.

get_list_of_items([], []) :-
	!.
get_list_of_items([F | R], [F1 | R1]) :-
	get_item(F, F1),
	get_list_of_items(R, R1),
	!.
get_list_of_items(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [get_list_of_items(X, Y)]),
	fail.

add_new_item_list([], _Tag).
add_new_item_list([F | R], Tag) :-
	add_new_item(F, Tag),
	!,
	add_new_item_list(R, Tag).

add_new_item(Item, Tag) :-
	make_item_canonical(Item, Item1),
	set_item_tag(Item1, Tag),
	get_next_item_id(Id),
	assertz(stepper_item(Id, Item1)),
	print_summary_for_item(Id, Item1, 'Added item '),
	!.

set_item_tag(Item, Tag) :-
	Item = item(_LHS, _RHS, Tag),
	!.
set_item_tag(Item, Tag) :-
	format2error('~N*** Error: bad call: ~w~n', [set_item_tag(Item, Tag)]),
	fail.

get_item(ItemID, Item) :-
	stepper_item(ItemID, Item),
	!.
get_item(ItemID, _Item) :-
	integer(ItemId),
	ItemId > 0,
	!,
	format('~NThere is no item number ~d~n', [ItemID]),
	fail.
get_item(_ItemID, _Item) :-
	!,
	format('~NItem IDs must be positive integers~n', []),
	fail.

delete_item(ItemID) :-
	check_is_item_id(ItemID),
	get_item(ItemID, Item),
	retract(stepper_item(ItemID, Item)),
	!.

delete_list_of_items(List) :-
	check_is_list_of_item_ids(List),
	delete_list_of_items1(List).
	
delete_list_of_items1([]).
delete_list_of_items1([F | R]) :-
	delete_item(F),
	!,
	delete_list_of_items1(R).

replace_item(ItemID, NewItem) :-
	retract(stepper_item(ItemID, _)),
	assertz(stepper_item(ItemID, NewItem)).

mark_out_of_date_items(AtomsToMatch) :-
	findall(Id, stepper_item(Id, _), Ids),
	mark_out_of_date_items1(Ids, AtomsToMatch),
	!.
mark_out_of_date_items(AtomsToMatch) :-
	format2error('*** Bad call: ~w~n', [mark_out_of_date_items(AtomsToMatch)]),
	fail.

mark_out_of_date_items1([], _AtomsToMatch).
mark_out_of_date_items1([F | R], AtomsToMatch) :-
	mark_out_of_date_item(F, AtomsToMatch),
	!,
	mark_out_of_date_items1(R, AtomsToMatch).

mark_out_of_date_item(ItemID, AtomsToMatch) :-
	stepper_item(ItemID, item(LHS, RHS, Tag)),
	(   tag_matches_atom_in_list([old], Tag) ->
	    true
	;
	    tag_matches_atom_in_list(AtomsToMatch, Tag) ->
	    format_to_atom('~w - old', [Tag], TagWithOld),
	    NewItem = item(LHS, RHS, TagWithOld),
	    replace_item(ItemID, NewItem)
	;
	    true
	).
 
tag_matches_atom_in_list([F | _R], Tag) :-
	atom_name_contains_string(Tag, F),
	!.
tag_matches_atom_in_list([_F | R], Tag) :-
	tag_matches_atom_in_list(R, Tag),
	!.

%----------------------------------------------------------------------

check_is_list_of_item_ids(ListOfItemIDs) :-
	ground(ListOfItemIDs),
	check_is_list_of_item_ids1(ListOfItemIDs).

check_is_list_of_item_ids1([]).
check_is_list_of_item_ids1([F | R]) :-
	check_is_item_id(F),
	!,
	check_is_list_of_item_ids1(R).

check_is_item_id(ItemID) :-
	stepper_item(ItemID, _Item).
check_is_item_id(ItemID) :-
	format2error('~N*** Error: "~w" is not a current item ID~n', [ItemID]),
	fail.
	
%----------------------------------------------------------------------

:- dynamic next_item_id/1.

init_next_item_id_counter :-
	retractall(next_item_id(_)),
	assertz(next_item_id(1)),
	!.

get_next_item_id(Id) :-
	next_item_id(Id),
	retractall(next_item_id(_)),
	Id1 is Id + 1,
	assertz(next_item_id(Id1)),
	!.
get_next_item_id(Id) :-
	format2error('~N*** Error: bad call: ~w~n', [get_next_item_id(Id)]),
	fail.

%----------------------------------------------------------------------

make_item_canonical(item(LHS, RHS, Tag), item(LHS1, RHS1, Tag)) :-
	LHS =.. [Cat, Tree | Rest],
	renumber_parse_tree(Tree, Tree1, LeafGoals),
	LHS1 =.. [Cat, Tree1 | Rest],
	renumber_rhs(RHS, LeafGoals-[], RHS1),
	!.

/*
- renumber_parse_tree(Tree, Tree1, LeafGoalsIn)

Put in numerical IDs in Tree to make Tree1. Store leaf goals.
*/

renumber_parse_tree(Tree, Tree1, LeafGoals) :-
	renumber_parse_tree1(Tree, Tree1, LeafGoals-[], 1-_Out),
	!.
renumber_parse_tree(Tree, Tree1, LeafGoals) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [renumber_parse_tree(Tree, Tree1, LeafGoals)]),
	fail.

renumber_parse_tree1(Var, Var, LeafGoalsIn-LeafGoalsIn, InN-InN) :-
	var(Var),
	!.
renumber_parse_tree1(phrase(Cat, line_info(_, Cut, Lines, File), Daughters),
		     OutTree,
		     LeafGoalsIn-LeafGoalsOut,
		     InN-OutN) :-
	NextN is InN + 1,
	OutTree = phrase(Cat, line_info(InN, Cut, Lines, File), Daughters1),
	(   var(Lines) ->
	    LeafGoalsIn = [OutTree | LeafGoalsNext] ;
	    LeafGoalsIn = LeafGoalsNext
	),
	renumber_parse_tree_daughters(Daughters, Daughters1,
				      LeafGoalsNext-LeafGoalsOut, NextN-OutN),
	!.
renumber_parse_tree1(Other, Other, LeafGoalsIn-LeafGoalsIn, InN-InN).

renumber_parse_tree_daughters(Var, Var, LeafGoalsIn-LeafGoalsIn, InN-InN) :-
	var(Var),
	!.
renumber_parse_tree_daughters((P, Q), (P1, Q1), LeafGoalsIn-LeafGoalsOut, InN-OutN) :-
	!,
	renumber_parse_tree1(P, P1, LeafGoalsIn-LeafGoalsNext, InN-NextN),
	renumber_parse_tree_daughters(Q, Q1, LeafGoalsNext-LeafGoalsOut, NextN-OutN).
renumber_parse_tree_daughters(P, P1, LeafGoalsIn-LeafGoalsOut, InN-OutN) :-
	renumber_parse_tree1(P, P1, LeafGoalsIn-LeafGoalsOut, InN-OutN).

%---------------------------------------------------------------------

mark_cut_at_node_in_item(item(LHS, RHS, Tag), NodeID, CutMarking, item(LHS1, RHS, Tag)) :-
	LHS =.. [Cat, Tree | Rest],
	LHS1 =.. [Cat, Tree1 | Rest],
	mark_cut_at_node_in_parse_tree(Tree, NodeID, CutMarking, Tree1),
	!.
mark_cut_at_node_in_item(Item, NodeID, CutMarking, Item1) :-
	format2error('~N*** Error: bad call: ~w~n', [mark_cut_at_node_in_item(Item, NodeID, CutMarking, Item1)]),
	fail.

mark_cut_at_node_in_parse_tree(Tree, NodeID, CutMarking, Tree1) :-
	Tree = phrase(Cat, line_info(NodeID1, _OldCutMarking, Lines, File), Daughters),
	NodeID == NodeID1,
	Tree1 = phrase(Cat, line_info(NodeID, CutMarking, Lines, File), Daughters),
	!.
mark_cut_at_node_in_parse_tree(Tree, NodeID, CutMarking, Tree1) :-
	Tree = phrase(Cat, LineInfo, Daughters),
	nonvar(Daughters),
	mark_cut_at_node_in_parse_tree_daughters(Daughters, NodeID, CutMarking, Daughters1),
	Tree1 = phrase(Cat, LineInfo, Daughters1),
	!.

mark_cut_at_node_in_parse_tree_daughters((First, Rest), NodeID, CutMarking, (First1, Rest)) :-
	mark_cut_at_node_in_parse_tree(First, NodeID, CutMarking, First1),
	!.
mark_cut_at_node_in_parse_tree_daughters((First, Rest), NodeID, CutMarking, (First, Rest1)) :-
	mark_cut_at_node_in_parse_tree_daughters(Rest, NodeID, CutMarking, Rest1),
	!.
mark_cut_at_node_in_parse_tree_daughters(Tree, NodeID, CutMarking, Tree1) :-
	Tree = phrase(_, _, _),
	mark_cut_at_node_in_parse_tree(Tree, NodeID, CutMarking, Tree1).

%---------------------------------------------------------------------

renumber_rhs(true, LeafGoalsIn-LeafGoalsIn, true) :-
	!.
renumber_rhs(LexGoal, LeafGoalsIn-LeafGoalsIn, LexGoal) :-
	nonvar(LexGoal),
	functor(LexGoal, 'C', 3),
	!.
renumber_rhs((F, R), LeafGoalsIn-LeafGoalsOut, (F1, R1)) :-
	renumber_rhs(F, LeafGoalsIn-LeafGoalsNext, F1),
	renumber_rhs(R, LeafGoalsNext-LeafGoalsOut, R1),
	!.
renumber_rhs(Goal, [LeafGoalTree | LeafGoalsOut]-LeafGoalsOut, Goal1) :-
	Goal =.. [Cat, _OldTree | Rest],
	Goal1 =.. [Cat, LeafGoalTree | Rest],
	!.
renumber_rhs(RHS, LeafGoals, RHS1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [renumber_rhs(RHS, LeafGoals, RHS1)]),
	fail.

%----------------------------------------------------------------------

print_summary_to_list(List) :-
	get_sorted_id_item_pairs(Pairs),
	findall(SummaryString,
		(   member(ItemID-Item, Pairs),
		    print_summary_for_item_to_string(ItemID, Item, SummaryString)
		),
		List).

get_sorted_id_item_pairs(SortedPairs) :-
	findall(ItemID-Item,
		stepper_item(ItemID, Item),
		Pairs),
	keysort(Pairs, SortedPairs).

print_summary_for_item_to_string(ItemID, Item, SummaryString) :-
	with_output_to_chars(print_summary_for_item(ItemID, Item),
			     SummaryString0),
	remove_final_newline_char(SummaryString0, SummaryString).

remove_final_newline_char([], []) :-
	!.
remove_final_newline_char([0'\n], []) :-
	!.
remove_final_newline_char([F | R], [F | R1]) :-
	remove_final_newline_char(R, R1).

%----------------------------------------------------------------------

print_summary :-
	\+ stepper_item(_ItemID, _Item),
	format('~NNo items~n', []),
	!.
print_summary :-
	get_sorted_id_item_pairs(Pairs),
	member(ItemID-Item, Pairs),
	print_summary_for_item(ItemID, Item),
	fail.
print_summary.

print_summary_for_item(ItemID, Item) :-
	print_summary_for_item(ItemID, Item, '').

print_summary_for_item(ItemID, Item, Intro) :-
	feat_for_item(Item, form=Form),
	feat_for_item(Item, tag=Tag),
	format('~N~w~d: ~w (~w)~n', [Intro, ItemID, Form, Tag]),
	!.
print_summary_for_item(Intro, ItemID, Item) :-
	format2error('~N*** Error: bad call: ~w~n', [print_summary_for_item(Intro, ItemID, Item)]),
	fail.

%----------------------------------------------------------------------

tree_for_item(Item, Tree) :-
	Item = item(LHS, _RHS, _Tag),
	arg(1, LHS, Tree).

syn_feats_for_item(Item, SynFeats) :-
	Item = item(LHS, _RHS, _Tag),
	arg(2, LHS, SynFeats).

%----------------------------------------------------------------------

feat_for_item(item(LHS, RHS, _Tag), form=(LHSSummary --> RHSSummary)) :-
	summarise_lhs(LHS, LHSSummary),
	summarise_rhs(RHS, RHSSummary).
feat_for_item(item(LHS, _RHS, _Tag), Feat) :-
	feat_for_item_lhs(LHS, Feat).
feat_for_item(item(_LHS, RHS, _Tag), Feat) :-
	feat_for_item_rhs(RHS, Feat).
feat_for_item(item(_LHS, _RHS, Tag), Feat) :-
	feat_for_item_tag(Tag, Feat).

feat_for_item_lhs(LHS, cat=Cat) :-
	functor(LHS, Cat, _).
feat_for_item_lhs(LHS, line_info=LineInfo) :-
	arg(1, LHS, Tree),
	Tree = phrase(_, LineInfo, _).
feat_for_item_lhs(LHS, sem=Sem) :-
	arg(3, LHS, Sem).
feat_for_item_lhs(LHS, SynFeat) :-
	arg(2, LHS, SynFeats),
	print_form_for_syn_feats_grounded(SynFeats, GroundedPrintFeats),
	member(SynFeat, GroundedPrintFeats).
feat_for_item_lhs(LHS, tree=Tree) :-
	arg(1, LHS, Tree).
feat_for_item_lhs(LHS, rule=Rule) :-
	arg(1, LHS, Tree),
	rule_in_tree(Tree, Rule).

% No RHS feats yet.

feat_for_item_rhs(_RHS, _Feat) :-
	fail.

% Just use the tag itself as a feature for now

feat_for_item_tag(Tag, tag=Tag).

%----------------------------------------------------------------------

summarise_lhs(LHS, LHSSummary) :-
	summarise_cat(LHS, LHSSummary).

summarise_rhs(true, []) :-
	!.
summarise_rhs((P, Q), (PSummary, QSummary)) :-
	!,
	summarise_rhs(P, PSummary),
	summarise_rhs(Q, QSummary).
summarise_rhs('C'(_, Word, _), Word) :-
	!.
summarise_rhs(CatGoal, Cat) :-
	summarise_cat(CatGoal, Cat).

summarise_cat(CatGoal, Cat) :-
	functor(CatGoal, Cat, _).

summarise_item_list([], []).
summarise_item_list([F | R], [F1 | R1]) :-
	F = item(LHS, _RHS, _Tag),
	summarise_lhs(LHS, F1),
	!,
	summarise_item_list(R, R1).

summarise_goal_list([], []).
summarise_goal_list([F | R], [F1 | R1]) :-
	summarise_cat(F, F1),
	!,
	summarise_goal_list(R, R1).

%----------------------------------------------------------------------

display_cut_node(_NodeID, SynFeats) :-
	print_form_for_syn_feats_grounded(SynFeats, PrintSynFeats),
	format('~NForm:  ~w~n', ['CUT NODE']),
	%format('~NSem:   ~w~n', ['(no semantics)']),
	format('~NFeats: ~w~n', [PrintSynFeats]),
	%format('~NTree:  ~w', ['(no tree)']),
	!.

%----------------------------------------------------------------------

display_item(Item) :-
	feat_for_item(Item, form=Form),
	feat_for_item(Item, tag=Tag),
	copy_term(Item, Item1),
	mark_sem_values_in_rhs(Item1),
	feat_for_item(Item1, sem=Sem0),
	regulus_eval_text(Sem0, Sem),
	tree_for_item(Item, Tree),
	syn_feats_for_item(Item, SynFeats),
	print_form_for_syn_feats_grounded(SynFeats, PrintSynFeats),
	format('~NForm:  ~w~n', [Form]),
	(   nonvar(Tag) ->
	    format('~NTag:   ~w~n', [Tag])
	;
	    otherwise ->
	    true
	),
	format('~NSem:   ~w~n', [Sem]),
	format('~NFeats: ~w~n', [PrintSynFeats]),
	format('~NTree:~n~n', []),
	prettyprint_parse_tree(Tree, print_line_info_and_numbers),
	!.
display_item(Item) :-
	format2error('~N*** Error: bad call: ~w~n', [display_item(Item)]),
	fail.

mark_sem_values_in_rhs(Item) :-
	Item = item(_LHS, RHS, _Tag),
	mark_sem_values_in_rhs1(RHS).

mark_sem_values_in_rhs1((F, R)) :-
	!,
	mark_sem_values_in_rhs1(F),
	mark_sem_values_in_rhs1(R).
mark_sem_values_in_rhs1(CatGoal) :-
	functor(CatGoal, _Cat, 6),
	arg(1, CatGoal, Tree),
	Tree = phrase(_, line_info(NodeID, _, _, _), _),
	number(NodeID),
	arg(3, CatGoal, Sem),
	var(Sem),
	format_to_atom('(Sem for node ~d)', [NodeID], Sem),
	!.
mark_sem_values_in_rhs1(_Other).

%----------------------------------------------------------------------

display_rule_for_item(Item) :-
	feat_for_item(Item, line_info=LineInfo),
	LineInfo = line_info(_, _, From-To, File),
	From1 is From + 1,
	To1 is To + 1,
	print_lines_from_file(File, From1, To1),
	!.
display_rule_for_item(Item) :-
	format2error('~N*** Error: bad call: ~w~n', [display_rule_for_item(Item)]),
	fail.

/*
rule_string_for_item(Item, RuleString) :-
	with_output_to_chars(display_rule_for_item(Item),
			     CommentAndRuleString),
	read_term_from_chars(CommentAndRuleString, Rule, [variable_names(NameVarPairs)]),
	instantiate_vars_to_names(NameVarPairs),
	make_ground(Rule),
	with_output_to_chars(prettyprint(Rule),
			     RuleString),
	!.
*/
rule_string_for_item(Item, RuleString) :-
	with_output_to_chars(display_rule_for_item(Item),
			     RuleString),
	!.

rule_string_for_item(_Item, RuleString) :-
	RuleString = "*** Unable to format rule ***".

instantiate_vars_to_names([]).
instantiate_vars_to_names([Name=Var | R]) :-
	Name = Var,
	!,
	instantiate_vars_to_names(R).
	
%----------------------------------------------------------------------

rule_in_tree(Tree, _Rule) :-
	(   var(Tree) ;
	    Tree = empty_constituent ;
	    Tree = lex(_LexItem)
	),
	!,
	fail.
rule_in_tree(Tree, Rule) :-
	Tree = phrase(Cat, LineInfo, Daughters),
	nonvar(Daughters),
	summarise_daughters_in_rule(Daughters, DaughtersSummary),
	Rule = rule((Cat --> DaughtersSummary), LineInfo).
rule_in_tree(Tree, Rule) :-
	Tree = phrase(_Cat, _LineInfo, Daughters),
	comma_list_to_list(Daughters, DaughtersList),
	member(Daughter, DaughtersList),
	rule_in_tree(Daughter, Rule).

summarise_daughters_in_rule(Var, Var) :-
	var(Var),
	!.
summarise_daughters_in_rule((F, R), (F1, R1)) :-
	!,
	summarise_daughter_in_rule(F, F1),
	summarise_daughters_in_rule(R, R1).
summarise_daughters_in_rule(F, F1) :-
	summarise_daughter_in_rule(F, F1).

summarise_daughter_in_rule(Tree, Cat) :-
	Tree = phrase(Cat, _LineInfo, _Daughters),
	!.
summarise_daughter_in_rule(Tree, LexItem) :-
	Tree = lex(LexItem),
	!.

%----------------------------------------------------------------------

package_item_for_java_gui(Item, PackagedItem) :-
	line_length_for_gui(LineLength),
	
	feat_for_item(Item, cat=Cat),
	format_to_chars('~w', [Cat], CatString),

	feat_for_item(Item, tag=Tag),
	format_to_chars('~w', [Tag], TagString),

	feat_for_item(Item, sem=Sem0),
	regulus_eval_text(Sem0, Sem),
	%format_to_chars('~w', [Sem], SemString),
	prettyprint_to_string_for_gui(Sem, SemString, LineLength),

	Item = item(LHS, _RHS, _Tag),
	arg(1, LHS, Tree),
	arg(2, LHS, SynFeats),
	print_form_for_syn_feats_grounded(SynFeats, PrintSynFeats),
	%format_to_chars('~w', [PrintSynFeats], FeatsString),
	prettyprint_to_string_for_gui(PrintSynFeats, FeatsString, LineLength),

	feat_for_item(Item, form=Form),
	format_to_chars('~w', [Form], FormString),

	package_tree_for_java_gui(Tree, GUITree),
	
	PackagedItem = item(TagString, CatString, SemString, FeatsString, FormString, GUITree),
	!.
package_item_for_java_gui(Item, PackagedItem) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [package_item_for_java_gui(Item, PackagedItem)]),
	fail.

%----------------------------------------------------------------------

package_cut_node_for_java_gui(Cat, SynFeats, PackagedItem) :-
	line_length_for_gui(LineLength),

	format_to_chars('~w', [Cat], CatString),

	TagString = "(no tag)",

	SemString = "(no semantics)",

	FormString = "(no form)",

	GUITree = "(no tree)",
	
	print_form_for_syn_feats_grounded(SynFeats, PrintSynFeats),
	%format_to_chars('~w', [PrintSynFeats], FeatsString),
	prettyprint_to_string_for_gui(PrintSynFeats, FeatsString, LineLength),

	PackagedItem = item(TagString, CatString, SemString, FeatsString, FormString, GUITree),
	!.

%----------------------------------------------------------------------

package_tree_for_java_gui(Tree, GUITree) :-
	Tree = empty_constituent,
	CatString = "n/a",
	NodeNumber = 0,
	CutString = "no_cut",
	FileString = "n/a",
	StartLine = 0,
	EndLine = 0,
	LexString = "empty",
	NDaughters = 0,
	GUIDaughters = "n/a",
	
	GUITree = tree(CatString, NodeNumber, CutString, FileString, StartLine, EndLine, LexString,
		       NDaughters, GUIDaughters),
	!.
package_tree_for_java_gui(Tree, GUITree) :-
	Tree = lex(LexItem),
	CatString = "n/a",
	NodeNumber = 0,
	CutString = "no_cut",
	FileString = "n/a",
	StartLine = 0,
	EndLine = 0,
	format_to_chars('~w', [LexItem], LexString),
	NDaughters = 0,
	GUIDaughters = "n/a",
	
	GUITree = tree(CatString, NodeNumber, CutString, FileString, StartLine, EndLine, LexString,
		       NDaughters, GUIDaughters),
	!.
% "Cut" node has uninstantiated daughters. Package them as a lex for the word "CUT".
package_tree_for_java_gui(Tree, GUITree) :-	
	Tree = phrase(Cat, line_info(NodeNumber, _Cut, _Lines, _File), Daughters),
	var(Daughters),

	format_to_chars('~w', [Cat], CatString),
	CutString = "cut",
	FileString = "n/a",
	Lex = "n/a",
	StartLine = 0,
	EndLine = 0,
	DaughtersList = [lex('CUT')],
	NDaughters = 1,
	package_tree_list_for_java_gui(DaughtersList, GUIDaughtersList),
	GUIDaughters =.. [daughters | GUIDaughtersList],
	
	GUITree = tree(CatString, NodeNumber, CutString, FileString, StartLine, EndLine, Lex,
		       NDaughters, GUIDaughters),
	!.
package_tree_for_java_gui(Tree, GUITree) :-	
	Tree = phrase(Cat, line_info(NodeNumber, Cut, StartLine-EndLine, File), Daughters),

	format_to_chars('~w', [Cat], CatString),
	format_to_chars('~w', [File], FileString),
	format_to_chars('~w', [Cut], CutString),
	Lex = "n/a",
	comma_list_to_list(Daughters, DaughtersList),
	length(DaughtersList, NDaughters),
	package_tree_list_for_java_gui(DaughtersList, GUIDaughtersList),
	GUIDaughters =.. [daughters | GUIDaughtersList],
	
	GUITree = tree(CatString, NodeNumber, CutString, FileString, StartLine, EndLine, Lex,
		       NDaughters, GUIDaughters),
	!.
package_tree_for_java_gui(Tree, GUITree) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [package_tree_for_java_gui(Tree, GUITree)]),
	fail.

package_tree_list_for_java_gui([], []).
package_tree_list_for_java_gui([F | R], [F1 | R1]) :-
	package_tree_for_java_gui(F, F1),
	!,
	package_tree_list_for_java_gui(R, R1).

%---------------------------------------------------------------

%regulus_eval_text(In, Out) :-
%	(   ( current_predicate(user:regulus_config/2), user:regulus_config(lf_postproc_pred, PostProcPred) ) ->
%	    true ;
%	    PostProcPred = no_post_proc_pred
%	),
%	regulus_eval_text(In, Out, PostProcPred),
%	!.
%regulus_eval_text(In, In).


