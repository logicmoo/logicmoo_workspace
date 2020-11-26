
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(stepper,
	  [stepper_loop/0,
	   init_stepper_loop/0,
	   init_stepper_loop_with_output_to_string/1,
	   process_stepper_command_for_java_gui/8,
	   get_stepper_summary_for_java_gui/2,
	   show_item_for_java_gui/3,
	   show_item_for_java_gui/4,
	   get_rule_string_for_item_for_java_gui/4,
	   tree_to_item/2]
	 ).
 
%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/stepper_read').
:- use_module('$REGULUS/Prolog/stepper_rule_db').
:- use_module('$REGULUS/Prolog/stepper_item_db').
:- use_module('$REGULUS/Prolog/stepper_choose').

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_eval').
:- use_module('$REGULUS/Prolog/logging').
:- use_module('$REGULUS/Prolog/java_gui_utils').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%----------------------------------------------------------------------
 
stepper_loop :-
	init_stepper_loop,
	!,
	stepper_loop1.

stepper_loop1 :-
	read_stepper_command0(Command, LiteralCommand),
	process_stepper_command0(Command, "*null_comment*", LiteralCommand),
	!,
	(   Command == exit ->
	    true ;
	    stepper_loop1
	).

%----------------------------------------------------------------------

process_stepper_command_for_java_gui(InChars, CommentChars, AnswerChars, OutChars, Status, ChangeInfo, MenuInfo, ErrorString) :-
	format('~N--- Execute stepper command for GUI: "~s"~n', [InChars]),
	%format('~N~n-- Executing: ~q~n~n',
	%       [process_stepper_command_for_java_gui(InChars, AnswerChars, OutChars, MenuInfo)]),
	%trace,
	sleep_if_sicstus4_and_using_gui,
	set_answer_string(AnswerChars),
	init_stored_errors,
	with_output_to_chars(process_stepper_command_for_java_gui1(InChars, CommentChars, Status, ChangeInfo, MenuInfo),
			     OutChars),
	get_stored_errors(ErrorString),
	format('~NStatus: ~w~n~n', [Status]),
	format('~NError string:~n"~s"~n', [ErrorString]),	
	%format('~N~n-- Executed: ~q~n~n',
	%       [process_stepper_command_for_java_gui(InChars, AnswerChars, OutChars, MenuInfo)]),
	format('~N--- Executed command~n', []),
	!.
 
process_stepper_command_for_java_gui1(Chars, CommentChars, Status, ChangeInfo, MenuInfo) :-
	atom_codes(LiteralCommand, Chars),
	(   interpret_stepper_command_chars(Chars, Command) ->
	    true ;

	    Command = unknown_command(LiteralCommand)
	),
	process_stepper_command_for_java_gui2(Command, CommentChars, LiteralCommand, Status, ChangeInfo, MenuInfo).

process_stepper_command_for_java_gui2(Command, CommentChars, LiteralCommand, Status, ChangeInfo, MenuInfo) :-
	on_exception(
	Exception, 
	( process_stepper_command_for_java_gui3(Command, CommentChars, Status, ChangeInfo, LiteralCommand), MenuInfo = no_menu ),
	( ChangeInfo = no_change, handle_exception_for_java_gui(Exception, Status, MenuInfo) )
    ),
	!.
process_stepper_command_for_java_gui2(_Command, _CommentChars, LiteralCommand, Status, ChangeInfo, MenuInfo) :-
	format2error('~NError processing command: ~s~n', [LiteralCommand]),
	MenuInfo = no_menu,
	ChangeInfo = no_change,
	Status = error.

process_stepper_command_for_java_gui3(unknown_command(Command), _CommentChars, Status, ChangeInfo, _LiteralCommand) :-
	!,
	format2error('~NUnknown stepper command "~w"~n', [Command]),
	Status = error,
	ChangeInfo = no_change,
        !.
process_stepper_command_for_java_gui3(Command, CommentChars, Status, ChangeInfo, _LiteralCommand) :-
	process_stepper_command(Command, CommentChars, ChangeInfo),
	Status = ok,
	!.
process_stepper_command_for_java_gui3(_Command, _CommentChars, Status, ChangeInfo, LiteralCommand) :-
	format2error('~N*** Error processing stepper command: "~w"~n', [LiteralCommand]),
	Status = error,
	ChangeInfo = no_change.

%----------------------------------------------------------------------

init_stepper_loop_with_output_to_string(OutChars) :-
	format('~N--- Initialising stepper for GUI~n', []),
	with_output_to_chars(init_stepper_loop,
			     OutChars),
	format('~N--- Stepper initialised for GUI~n', []).

init_stepper_loop :-
	on_exception(
	Exception, 
	init_stepper_loop_main,
	(   inform_about_top_level_regulus_error(Exception),
	    fail
	)
    ),
	!,
	format('~N--- Stepper initialised~n', []).
init_stepper_loop :-
	format('~N*** Warning: stepper initialisation failed~n', []).

init_stepper_loop_main :-	
	init_stepper_item_database_if_necessary,
	make_expanded_dcg_clause_database.

%----------------------------------------------------------------------

get_stepper_summary_for_java_gui(Comment, SummaryTerm) :-
	log_stepper_command('SUMMARY', Comment, []),
	format('~N--- Get stepper summary for GUI~n', []),
	print_summary_to_list(List),
	SummaryTerm =.. [summary | List],
	format('~N--- Found stepper summary~n', []),
	!.
get_stepper_summary_for_java_gui(_Comment, error) :-
	format('~N--- Unable to find stepper summary~n', []).

%----------------------------------------------------------------------
 
read_stepper_command0(Command, LiteralCommand) :-
	on_exception(
	Exception, 
	read_stepper_command(Command, LiteralCommand),
	(   inform_about_top_level_regulus_error(Exception),
	    Command = bad_command
	)
    ),
	!.

%----------------------------------------------------------------------

process_stepper_command0(unknown_command(Command), CommentChars, LiteralCommand) :-
	!,
	log_stepper_command('UNKNOWN_COMMAND', CommentChars, [LiteralCommand]),
	format('~NUnknown stepper command "~w"~n', [Command]),
        !.
process_stepper_command0(Command, CommentChars, _LiteralCommand) :-
	on_exception(Exception,
		     process_stepper_command(Command, CommentChars, _ChangeInfo),
		     ( inform_about_top_level_regulus_error(Exception) )
		    ),
	!.
process_stepper_command0(_Command, _CommentChars, LiteralCommand) :-
	format2error('~N*** Error processing stepper command: "~w"~n', [LiteralCommand]).
 
%----------------------------------------------------------------------

process_stepper_command(exit, CommentChars, no_change) :-
	log_stepper_command('EXIT', CommentChars, []),
	!.
 
process_stepper_command(help, CommentChars, no_change) :-
	log_stepper_command('HELP', CommentChars, []),
	format('~N', []),
	format('~NAvailable stepper commands:~n~n', []),
	print_file('$REGULUS/Prolog/stepper_help.txt').

/*
- lex(LexExpression)

Add lexical items for word or multi-word expression

Examples: lex(pain), lex((bright, light))

Present a menu of different lexical items for word/phrase, and add
chosen one as numbered items. The content of each item is a numbered
tree dominating the word/phrase.

Menu choice based on line-info, logical form, features. Show enough to give unique
description.
*/

process_stepper_command(LexCommand, CommentChars, ChangeInfo) :-
	functor(LexCommand, lex, N),
	N > 1,
	LexCommand =.. [lex | Args],
	log_stepper_command('LEX', CommentChars, [Args]),
	list_to_comma_list(Args, LexExpression),
	process_stepper_command(lex(LexExpression), CommentChars, ChangeInfo),
	!.

process_stepper_command(lex(LexExpression), CommentChars, no_change) :-
	(   atomic(LexExpression) ->
	    log_stepper_command('LEX', CommentChars, [LexExpression])
	;
	    true
	),
	find_items_for_lex_expression(LexExpression, PossibleItems),
	(   PossibleItems = [] ->
	    comma_list_to_list(LexExpression, LexExpressionWords),
	    warn_about_missing_vocabulary_for_words_list(LexExpressionWords),
	    format2error("~NNo lex items found~n~n", []),
	    fail ;
	    
	    choose_item_from_menu(PossibleItems, ChosenItem),
	    add_new_item(ChosenItem, lexicon)
	),
	!.

/*

- debug

Set debug mode

*/

process_stepper_command(debug, CommentChars, no_change) :-
	set_stepper_mode(debug),
	log_stepper_command('DEBUG', CommentChars, []),
	format('~NStepper now in debug mode~n', []).

/*

- edit

Set edit mode

*/

process_stepper_command(edit, CommentChars, no_change) :-
	set_stepper_mode(edit),
	log_stepper_command('EDIT', CommentChars, []),
	format('~NStepper now in edit mode~n', []).

/*

- load

Execute LOAD command and recompute stepper grammar info

*/

process_stepper_command(load, _CommentChars, no_change) :-
	format('~NExecuting Regulus command: "LOAD"~n', []),
	user:process_regulus_loop_command(['LOAD']),
	make_expanded_dcg_clause_database,
	mark_out_of_date_items([parse, lexicon]),
	format('~NCommand executed~n', []).

/*

/*

- load_generation

Execute LOAD_GENERATION command and recompute stepper grammar info

*/

process_stepper_command(load_generation, _CommentChars, no_change) :-
	format('~NExecuting Regulus command: "LOAD_GENERATION"~n', []),
	user:process_regulus_loop_command(['LOAD_GENERATION']),
	make_expanded_dcg_clause_database,
	mark_out_of_date_items([generate]),
	format('~NCommand executed~n', []).

/*

- ebl_load

Execute EBL_LOAD command and recompute stepper grammar info

*/

process_stepper_command(ebl_load, _CommentChars, no_change) :-
	format('~NExecuting Regulus command: "EBL_LOAD"~n', []),
	user:process_regulus_loop_command(['EBL_LOAD']),
	make_expanded_dcg_clause_database,
	mark_out_of_date_items([parse, lexicon]),
	format('~NCommand executed~n', []).
 
/*

- ebl_load_generation

Execute EBL_LOAD_GENERATION command and recompute stepper grammar info

*/
 
process_stepper_command(ebl_load_generation, _CommentChars, no_change) :-
	format('~NExecuting Regulus command: "EBL_LOAD_GENERATION"~n', []),
	user:process_regulus_loop_command(['EBL_LOAD_GENERATION']),
	make_expanded_dcg_clause_database,
	mark_out_of_date_items([generate]),
	format('~NCommand executed~n', []).

/*
- parse(Atom)

Add items for parses of atom

Examples: parse('where is the pain'), parse('the headaches')

Present a menu of different parses for atom, and add chosen one as
numbered items. The content of each item is a numbered tree dominating
the word/phrase.

Menu choice based on logical form, rules used in tree, subtrees. Show
enough to give unique description.
*/

process_stepper_command(parse(Atom), CommentChars, no_change) :-
	(   \+ atom(Atom) ->
	    format2error('~N*** Error: argument to \'parse\' must be an atom~n', []),
	    fail
	;

	    log_stepper_command('PARSE', CommentChars, [Atom])
	),
	find_items_for_utterance_atom(Atom, PossibleItems),
	(   PossibleItems = [] ->
	    warn_about_missing_vocabulary_for_atom(Atom),
	    format2error("~NNo parse found~n~n", []),
	    item_for_bad_parse(Atom, BadItem),
	    add_new_item(BadItem, 'failed parse')
	;
	    choose_item_from_menu(PossibleItems, ChosenItem),
	    add_new_item(ChosenItem, parse)
	),
	!.

/*

- generate(Item)

Add items for generations from logical form in Item

Example: generate(1)

*/

process_stepper_command(generate(ItemID), CommentChars, no_change) :-
	log_stepper_command('GENERATE', CommentChars, [ItemID]),
	get_item(ItemID, Item),
	generate_from_item(Item, GeneratedItems),
	(   GeneratedItems = [] ->

	    format('~NNothing generated~n~n', []) ;

	    otherwise ->
	    length(GeneratedItems, NItems),
	    format('~NGenerated ~d items~n~n', [NItems]),
	    add_new_item_list(GeneratedItems, generated)
	),
	!.

/*
- combine(Items)

Examples: combine([3,4]), combine([5]), combine([])

Present a menu of different rules that could be used to combine items
(i.e. dominate them). Try to apply chosen rule. If successful, add new
item. If unsuccessful, present informative unification failure
message.

Menu choice based on line-info
*/

process_stepper_command(CombineCommand, CommentChars, no_change) :-
	functor(CombineCommand, combine, N),
	CombineCommand =.. [combine | Args],
	(   N > 1 ;
	    
	    ( Args = [Arg], \+ is_list(Arg) )
	),
	process_stepper_command(combine(Args), CommentChars, no_change),
	!.

process_stepper_command(combine(ListOfItemIDs), CommentChars, no_change) :-
	log_stepper_command('COMBINE', CommentChars, ListOfItemIDs),
	check_is_list_of_item_ids(ListOfItemIDs),
	ListOfItemIDs \== [],
	get_list_of_items(ListOfItemIDs, ListOfItems),
	find_rules_for_item_list(ListOfItems, PossibleRules),
	choose_rule_from_menu(PossibleRules, ChosenRule),
	combine_items_using_rule(ChosenRule, ListOfItems, NewItem),
	add_new_item(NewItem, combined),
	!.

process_stepper_command(gap, CommentChars, no_change) :-
	log_stepper_command('GAP', CommentChars, []),
	find_rules_for_item_list([], PossibleRules),
	choose_rule_from_menu(PossibleRules, NewItem),
	add_new_item(NewItem, gap),
	!.

/*
- cut(ItemID, NodeID)

In debug mode, create two new items by cutting Item above NodeID. Both subtrees are
renumbered. Leaves in the upper subtree corresponding to removed nodes
are given an ID.

In edit mode, add a 'cut' annotation at NodeID
*/

process_stepper_command(cut(ItemID, NodeID), CommentChars, no_change) :-
	log_stepper_command('CUT', CommentChars, [ItemID, NodeID]),
	get_stepper_mode(Mode),
	get_item(ItemID, Item),
	(   Mode = debug ->
	    cut_item_at_node(Item, NodeID, UpperItem, LowerItem),
	    add_new_item(UpperItem, cut),
	    add_new_item(LowerItem, cut)
	;
	    mark_cut_at_node_in_item(Item, NodeID, cut, NewItem),
	    replace_item(ItemID, NewItem)
	),
	!.

/*
- uncut(ItemID, NodeID)

In edit mode, remove 'cut' annotation at NodeID

*/

process_stepper_command(uncut(ItemID, NodeID), CommentChars, no_change) :-
	log_stepper_command('UNCUT', CommentChars, [ItemID, NodeID]),
	get_stepper_mode(Mode),
	get_item(ItemID, Item),
	(   Mode = debug ->
	    format2error('~N*** Error: UNCUT command is only meaningful in edit mode~n', []),
	    fail
	;
	    mark_cut_at_node_in_item(Item, NodeID, no_cut, NewItem),
	    replace_item(ItemID, NewItem)
	),
	!.

/*
- join(ItemID1, NodeID, ItemID2)

Create new item by joining Item2 to NodeID of Item1. NodeID must be the
ID of a "removed" node in Item1. If successful, add new
item. If unsuccessful, present informative unification failure
message.
*/

process_stepper_command(join(ItemID1, NodeID, ItemID2), CommentChars, no_change) :-
	log_stepper_command('JOIN', CommentChars, [ItemID1, NodeID, ItemID2]),
	get_item(ItemID1, Item1),
	get_item(ItemID2, Item2),
	join_items_at_node(Item1, NodeID, Item2, NewItem),
	add_new_item(NewItem, joined),
	!.

process_stepper_command(join(ItemID1, ItemID2), CommentChars, no_change) :-
	log_stepper_command('JOIN', CommentChars, [ItemID1, ItemID2]),
	get_item(ItemID1, Item1),
	get_item(ItemID2, Item2),
	find_cut_nodes_in_item(Item1, CutNodes),
	choose_cut_node_from_menu(CutNodes, Item1, NodeID),
	join_items_at_node(Item1, NodeID, Item2, NewItem),
	add_new_item(NewItem, joined),
	!.

/*
- show(Item)

Print information for Item. Tree and logical form.
*/

process_stepper_command(show(ItemID), CommentChars, no_change) :-
	log_stepper_command('SHOW', CommentChars, [ItemID]),
	get_item(ItemID, Item),
	display_item(Item),
	!.

/*
- show(Item, NodeID)

Print information for subtree of Tree rooted in NodeID. 
*/
 
process_stepper_command(show(ItemID, NodeID), CommentChars, no_change) :-
	log_stepper_command('SHOW', CommentChars, [ItemID, NodeID]),
	get_item(ItemID, Item),
	(   NodeID = 0 ->
	    format2error('~NNothing to show at leaf/cut nodes.', []),
	    fail
	;
	    is_cut_node_in_item(NodeID, Item, _Cat, SynFeats) ->
	    display_cut_node(NodeID, SynFeats)
	;
	    otherwise ->
	    cut_item_at_node(Item, NodeID, _UpperItem, LowerItem),
	    make_item_canonical(LowerItem, LowerItem1),
	    display_item(LowerItem1)
	),
	%format('~N~nRule:~n', []),
	%display_rule_for_item(LowerItem1),
	!.

/*
- rule(Item, NodeID)

Print rule at NodeID. 
*/

process_stepper_command(rule(ItemID, NodeID), CommentChars, no_change) :-
	log_stepper_command('RULE', CommentChars, [ItemID, NodeID]),
	(   NodeID = 0 ->
	    format2error('~NNo rule to show at leaf/cut nodes.', []),
	    fail
	;
	    otherwise ->
	    get_item(ItemID, Item),
	    cut_item_at_node(Item, NodeID, _UpperItem, LowerItem),
	    display_rule_for_item(LowerItem)
	),
	!.
 
/*
- summary

Print summary info for all items. Logical form, short form of tree.
*/

process_stepper_command(summary, CommentChars, no_change) :-
	log_stepper_command('SUMMARY', CommentChars, []),
	print_summary,
	!.

/*
- delete(Item)

Remove Item
*/
 
process_stepper_command(delete(ItemID), CommentChars, ChangeInfo) :-
	log_stepper_command('DELETE', CommentChars, [ItemID]),
	number(ItemID),
	(   get_stepper_confirmation('~N~nDelete item ~d? ', [ItemID]) ->
	    
	    delete_item(ItemID),
	    renumber_stepper_item_database([ItemID], ChangeInfo),
	    format('~NItem ~d deleted~n', [ItemID])
	),
	!.

/*
- delete(ItemList)

Remove items in ItemList
*/
 
process_stepper_command(DeleteCommand, CommentChars, ChangeInfo) :-
	functor(DeleteCommand, delete, N),
	DeleteCommand =.. [delete | Args],
	length(Args, N),
	N > 1,
	process_stepper_command(delete(Args), CommentChars, ChangeInfo),
	!.

process_stepper_command(delete(ListOfItemIDs), CommentChars, ChangeInfo) :-
	is_list(ListOfItemIDs),
	log_stepper_command('DELETE', CommentChars, ListOfItemIDs),
	(   get_stepper_confirmation('~N~nDelete items ~w? ', [ListOfItemIDs]) ->
	    
	    delete_list_of_items(ListOfItemIDs),
	    renumber_stepper_item_database(ListOfItemIDs, ChangeInfo),
	    format('~NItems ~w deleted~n', [ListOfItemIDs])
	),
	!.

process_stepper_command(delete_all, CommentChars, ChangeInfo) :-
	log_stepper_command('DELETE_ALL', CommentChars, []),
	(   get_stepper_confirmation('~N~nDelete all items? ', []) ->

	    change_info_for_delete_all(ChangeInfo),
	    init_stepper_item_database,
	    format('~NAll items deleted~n', [])
	),
	!.

%----------------------------------------------------------------------

% Like process_stepper_command(show(ItemId))

show_item_for_java_gui(Comment, ItemId, PackagedItem) :-
	format('~N--- Received call: ~q~n', [show_item_for_java_gui(Comment, ItemId, PackagedItem)]),
	%trace,
	log_stepper_command('SHOW', Comment, [ItemId]),
	get_item(ItemId, Item),
	package_item_for_java_gui(Item, PackagedItem),
	format('~N--- Succeeded: ~q~n', [show_item_for_java_gui(Comment, ItemId, Item)]),
	!.
show_item_for_java_gui(Comment, ItemId, PackagedItem) :-
	format('~N--- Failed: ~q~n', [show_item_for_java_gui(Comment, ItemId, PackagedItem)]),
	fail.

% Like process_stepper_command(show(ItemId, NodeID))
	
show_item_for_java_gui(Comment, ItemId, NodeID, PackagedItem) :-
	format('~N--- Received call: ~q~n', [show_item_for_java_gui(Comment, ItemId, NodeID, PackagedItem)]),
	log_stepper_command('SHOW', Comment, [ItemId, NodeID]),
	get_item(ItemId, Item),
	(   is_cut_node_in_item(NodeID, Item, Cat, SynFeats) ->

	    package_cut_node_for_java_gui(Cat, SynFeats, PackagedItem) ;
	    
	    cut_item_at_node(Item, NodeID, _UpperItem, LowerItem),
	    make_item_canonical(LowerItem, LowerItem1),
	    package_item_for_java_gui(LowerItem1, PackagedItem)
	),
	format('~N--- Succeeded: ~q~n', [show_item_for_java_gui(Comment, ItemId, NodeID, PackagedItem)]),
	!.
show_item_for_java_gui(_Comment, ItemId, NodeID, PackagedItem) :-
	format('~N--- Failed: ~q~n', [show_item_for_java_gui(ItemId, NodeID, PackagedItem)]),
	fail.

% Like process_stepper_command(rule(ItemID, NodeID))

get_rule_string_for_item_for_java_gui(Comment, ItemID, NodeID, RuleString) :-
	log_stepper_command('RULE', Comment, [ItemID, NodeID]),
	get_item(ItemID, Item),
	cut_item_at_node(Item, NodeID, _UpperItem, LowerItem),
	rule_string_for_item(LowerItem, CommentAndRuleString),
	truncate_comment_lines_in_string_for_java_gui(CommentAndRuleString, CommentAndRuleString1),
	wrap_lines_in_string_for_java_gui(CommentAndRuleString1, RuleString),	
	!.

%----------------------------------------------------------------------

item_for_bad_parse(Atom, BadItem) :-
	split_atom_into_words(Atom, WordsList),
	list_to_comma_list(WordsList, CommaList),
	bad_parse_lhs(LHS),
	lex_expression_to_dcg_form(CommaList, RHS),
	BadItem = item(LHS, RHS, 'failed parse'),
	!.
item_for_bad_parse(Atom, BadItem) :-
	format2atom('~N*** Error: bad call: ~w~n', [item_for_bad_parse(Atom, BadItem)]),
	fail.

bad_parse_lhs(LHS) :-
	LHS = 'FAILED_PARSE'(no_tree, [], [], _, _, _).
	
%----------------------------------------------------------------------
 
find_items_for_lex_expression(LexExpression, PossibleItems) :-
	findall(Item,
		item_for_lex_expression(LexExpression, Item),
		PossibleItems),
	!.

item_for_lex_expression(LexExpression, item(DCGCat, DCGLexExpression, _Tag)) :-
	check_term_is_lex_expression(LexExpression),
	lex_expression_to_dcg_form(LexExpression, DCGLexExpression),
	expanded_dcg_clause(DCGCat, DCGLexExpression, normal).

check_term_is_lex_expression(LexExpression) :-
	term_is_lex_expression(LexExpression),
	!.
check_term_is_lex_expression(LexExpression) :-
	format2error('~N*** Error: ~w is not a lexical expression.~n', [LexExpression]),
	fail.
 
term_is_lex_expression(Atom) :-
	atom(Atom),
	Atom \== [],
	!.
term_is_lex_expression(Compound) :-
	nonvar(Compound),
	Compound = (L, R),
	term_is_lex_expression(L),
	term_is_lex_expression(R).

lex_expression_to_dcg_form(LexExpression, DCGForm) :-
	lex_expression_to_dcg_form1(LexExpression, DCGForm, _In, _Out).

lex_expression_to_dcg_form1(LexExpression, 'C'(In, LexExpression, Out), In, Out) :-
	atom(LexExpression),
	!.
lex_expression_to_dcg_form1((L, R), (L1, R1), In, Out) :-
	lex_expression_to_dcg_form1(L, L1, In, Next),
	lex_expression_to_dcg_form1(R, R1, Next, Out).

%----------------------------------------------------------------------

generate_from_item(Item, GeneratedItems) :-
	feat_for_item(Item, cat=Cat),
	feat_for_item(Item, sem=Sem),
	regulus_eval_text(Sem, Sem1),
	generate_from_cat_and_sem(Cat, Sem1, GeneratedItems).

generate_from_cat_and_sem(Cat, _Sem, GeneratedItems) :-
	Cat \== '.MAIN',
	format('~NCategory = ~w. Only allowed to generate from .MAIN category~n', [Cat]),
	GeneratedItems = [],
	!.
generate_from_cat_and_sem(_Cat, Sem, GeneratedItems) :-
	(   user:regulus_config(top_level_generation_pred, GenerationPred) ->
	    true ;
	    GenerationPred = generate
	),
	(   user:regulus_config(generation_module_name, GenerationModule) ->
	    true ;
	    GenerationModule = generator
	),
	Call =.. [GenerationPred, Sem, GeneratedTree, _GeneratedWords],
	findall(GeneratedTree, call(GenerationModule:Call), GeneratedTrees),
	generation_trees_to_items(GeneratedTrees, GeneratedItems),
	!.
generate_from_cat_and_sem(_Cat, _Sem, GeneratedItems) :-
	GeneratedItems = [].

generation_trees_to_items([], []).
generation_trees_to_items([F | R], [F1 | R1]) :-
	generation_tree_to_item(F, F1),
	!,
	generation_trees_to_items(R, R1).

%----------------------------------------------------------------------

find_items_for_utterance_atom(Atom, PossibleItems) :-
	split_atom_into_words(Atom, WordList),
	user:single_top_level_grammar(GrammarAtom),
	findall([Tree, SynFeats, Local],
		user:parse_with_current_parser(GrammarAtom, WordList, Tree, SynFeats, Local, _Global),
		Tuples0),
	safe_remove_duplicates(Tuples0, Tuples),
	word_list_to_item_rhs(WordList, RHS),
	tuples_and_rhs_to_items(Tuples, RHS, PossibleItems).

word_list_to_item_rhs(WordList, RHS) :-
	list_to_comma_list(WordList, WordCommaList),
	lex_expression_to_dcg_form(WordCommaList, RHS).

tuples_and_rhs_to_items([], _RHS, []).
tuples_and_rhs_to_items([F | R], RHS, [F1 | R1]) :-
	tuple_and_rhs_to_item(F, RHS, F1),
	!,
	tuples_and_rhs_to_items(R, RHS, R1).

%tuple_and_rhs_to_item([Tree, SynFeats, Sem], RHS, Item) :-
%	get_top_cat_from_tree(Tree, Cat),
%	get_in_and_out_from_rhs(RHS, In, Out),
%	LHS =.. [Cat, Tree, SynFeats, Sem, _Globals, In, Out],
%	Item = item(LHS, RHS).
tuple_and_rhs_to_item([Tree, _SynFeats, _Sem], _RHS, Item) :-
	tree_to_item(Tree, Item),
	!.

get_top_cat_from_tree(Tree, Cat) :-
	Tree = phrase(Cat, _LineInfo, _Daughters).

get_in_and_out_from_rhs((P, Q), In, Out) :-
	get_in_and_out_from_rhs(P, In, _),
	get_in_and_out_from_rhs(Q, _, Out).
get_in_and_out_from_rhs('C'(In, _Word, Out), In, Out).

%----------------------------------------------------------------------

find_rules_for_item_list(ListOfItems, PossibleRules) :-
	summarise_item_list(ListOfItems, ListSummary),
	findall(Rule,
		rule_for_list_summary(ListSummary, Rule),
		PossibleRules).

rule_for_list_summary([], Rule) :-
	expanded_dcg_clause(Head, true, normal),
	Rule = item(Head, true, _Tag).
rule_for_list_summary(ListSummary, Rule) :-
	ListSummary \== [],
	expanded_dcg_clause(Head, Body, normal),
	comma_list_to_list(Body, BodyList),
	remove_terminals_from_goal_list(BodyList, BodyListWithoutTerminals),
	summarise_goal_list(BodyListWithoutTerminals, BodyListWithoutTerminalsSummary),
	BodyListWithoutTerminalsSummary = ListSummary,
	Rule = item(Head, Body, _Tag).

remove_terminals_from_goal_list([], []).
remove_terminals_from_goal_list([F | R], R1) :-
	functor(F, 'C', 3),
	!,
	remove_terminals_from_goal_list(R, R1).
remove_terminals_from_goal_list([F | R], [F | R1]) :-
	!,
	remove_terminals_from_goal_list(R, R1).

%----------------------------------------------------------------------

combine_items_using_rule(Rule, [], NewItem) :-
	inform_about_attempt_to_use_rule(Rule),
	NewItem = Rule.
combine_items_using_rule(Rule, ListOfItems, NewItem) :-
	ListOfItems \== [],
	inform_about_attempt_to_use_rule(Rule),
	Rule = item(LHS0, RHS0, _Tag),
	copy_term(item(LHS0, RHS0), item(LHS, RHS)),
	attempt_to_resolve_body_against_list_of_items(RHS, ListOfItems-[], ExpandedRHS),
	flatten_dcg_clause_body(ExpandedRHS, FlattenedExpandedRHS),
	NewItem = item(LHS, FlattenedExpandedRHS, _Tag).

inform_about_attempt_to_use_rule(Rule) :-
	feat_for_item(Rule, line_info=LineInfo),
	format('~NUsing rule ', []),
	inform_about_line_info(LineInfo),
	!.
inform_about_attempt_to_use_rule(Rule) :-
	format2error('~N*** Error: bad call: ~w~n', [inform_about_attempt_to_use_rule(Rule)]),
	fail.

attempt_to_resolve_body_against_list_of_items((P, Q), ListIn-ListOut, (P1, Q1)) :-
	attempt_to_resolve_goal_against_list_of_items(P, ListIn-ListNext, P1),
	!,
	attempt_to_resolve_body_against_list_of_items(Q, ListNext-ListOut, Q1).
attempt_to_resolve_body_against_list_of_items(P, ListIn-ListOut, P1) :-
	attempt_to_resolve_goal_against_list_of_items(P, ListIn-ListOut, P1).

attempt_to_resolve_goal_against_list_of_items(P, ListIn-ListIn, P) :-
	functor(P, 'C', 3),
	!.
attempt_to_resolve_goal_against_list_of_items(P, [item(LHS, RHS, _Tag) | ListOut]-ListOut, RHS) :-
	attempt_to_unify_cat_goals(P, LHS).

%----------------------------------------------------------------------

attempt_to_unify_cat_goals(P, P) :-
	!.
attempt_to_unify_cat_goals(Goal1, Goal2) :-
	Goal1 =.. [Cat1, Tree1, Feats1, Sem1, Global1, In1, Out1],
	Goal2 =.. [Cat2, Tree2, Feats2, Sem2, Global2, In2, Out2],
	attempt_to_unify_cats(Cat1, Cat2),
	attempt_to_unify_trees(Tree1, Tree2),
	attempt_to_unify_sems(Sem1, Sem2),
	attempt_to_unify_feats(Feats1, Feats2, Cat1),
	attempt_to_unify_globals(Global1, Global2),
	attempt_to_unify_in(In1, In2),
	attempt_to_unify_out(Out1, Out2).

attempt_to_unify_cats(Cat, Cat) :-
	!.
attempt_to_unify_cats(Cat1, Cat2) :-
	format2error('~N~nIncompatible categories: "~w" and "~w"~n', [Cat1, Cat2]),
	fail.

attempt_to_unify_trees(Tree, Tree) :-
	!.
attempt_to_unify_trees(Tree1, Tree2) :-
	format2error('~N~nIncompatible trees: "~w" and "~w"~n', [Tree1, Tree2]),
	fail.

attempt_to_unify_sems(Sem, Sem) :-
	!.
attempt_to_unify_sems(Sem1, Sem2) :-
	format2error('~N~nIncompatible semantic values: "~w" and "~w"~n', [Sem1, Sem2]),
	fail.

attempt_to_unify_feats(Feats, Feats) :-
	!.
attempt_to_unify_feats(Feats1, Feats2, Cat) :-
	print_form_for_syn_feats_grounded(Feats1, PrintFeats1),
	print_form_for_syn_feats_grounded(Feats2, PrintFeats2),
	%format2error('~N~nIncompatible syntactic feats in categories:~n~n~w~n~n~w~n~n',
	%	     [Cat:PrintFeats1, Cat:PrintFeats2]),
	with_output_to_chars((   prettyprint(Cat:PrintFeats1),
				 format('~N~n', []),
				 prettyprint(Cat:PrintFeats2)),
			     PrettyprintChars),
	format2error('~N~nIncompatible syntactic feats in categories:~n~n~s~n~n', [PrettyprintChars]),
	find_feature_clash(Feats1, Feats2, Feat, Val1, Val2),
	print_form_for_syn_feats_grounded([Feat=Val1], [Feat=BadPrintFeat1]),
	print_form_for_syn_feats_grounded([Feat=Val2], [Feat=BadPrintFeat2]),
	format2error('~NFeature clash: ~w, ~w~n~n', [Feat=BadPrintFeat1, Feat=BadPrintFeat2]),
	fail.

attempt_to_unify_globals(G, G) :-
	!.
attempt_to_unify_globals(G1, G2) :-
	format2error('~N~nIncompatible global sem values: "~w" and "~w"~n', [G1, G2]),
	format2error('~NProbably an internal error', []),
	fail.

attempt_to_unify_in(G, G) :-
	!.
attempt_to_unify_in(G1, G2) :-
	format2error('~N~nIncompatible in-list values: "~w" and "~w"~n', [G1, G2]),
	format2error('~NProbably an internal error', []),
	fail.

attempt_to_unify_out(G, G) :-
	!.
attempt_to_unify_out(G1, G2) :-
	format2error('~N~nIncompatible out-list values: "~w" and "~w"~n', [G1, G2]),
	format2error('~NProbably an internal error', []),
	fail.

%find_feature_clash(Feats1, Feats2, Feat, Val1, Val2)

find_feature_clash([Feat=Val1 | _], [Feat=Val2 | _], Feat, Val1, Val2) :-
	\+ ((Val1 = Val2)),
	!.
find_feature_clash([F | R], [F | R1], Feat, Val1, Val2) :-
	find_feature_clash(R, R1, Feat, Val1, Val2).

%----------------------------------------------------------------------

cut_item_at_node(_Item, NodeID, _UpperItem, _LowerItem) :-
	NodeID = 0,
	format2error('~NNot allowed to cut on leaf nodes.', []),
	!,
	fail.
cut_item_at_node(Item, NodeID, UpperItem, LowerItem) :-
	feat_for_item(Item, tree=Tree),
	cut_tree_at_node(Tree, NodeID, UpperTree, LowerTree),
	tree_to_item(UpperTree, UpperItem),
	tree_to_item(LowerTree, LowerItem),
	!.
cut_item_at_node(Item, NodeID, UpperItem, LowerItem) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [cut_item_at_node(Item, NodeID, UpperItem, LowerItem)]),
	fail.

cut_tree_at_node(Tree, NodeID, UpperTree, LowerTree) :-
	Tree = phrase(Cat, line_info(NodeID1, Cut, Lines, File), Daughters),
	NodeID == NodeID1,
	UpperTree = phrase(Cat, line_info(NodeID, Cut, _AnyLines, _AnyFile), _AnyDaughters),
	LowerTree = phrase(Cat, line_info(NodeID, Cut, Lines, File), Daughters),
	!.
cut_tree_at_node(Tree, NodeID, UpperTree, LowerTree) :-
	Tree = phrase(Cat, LineInfo, Daughters),
	nonvar(Daughters),
	cut_daughters_at_node(Daughters, NodeID, CutDaughters, LowerTree),
	UpperTree = phrase(Cat, LineInfo, CutDaughters),
	!.

cut_daughters_at_node((First, Rest), NodeID, (UpperFirst, Rest), LowerTree) :-
	cut_tree_at_node(First, NodeID, UpperFirst, LowerTree),
	!.
cut_daughters_at_node((First, Rest), NodeID, (First, CutRest), LowerTree) :-
	cut_daughters_at_node(Rest, NodeID, CutRest, LowerTree),
	!.
cut_daughters_at_node(Tree, NodeID, UpperTree, LowerTree) :-
	Tree = phrase(_, _, _),
	cut_tree_at_node(Tree, NodeID, UpperTree, LowerTree).

%----------------------------------------------------------------------
 
tree_to_item(Tree, Item) :-
	tree_to_item(Tree, Item, normal).

generation_tree_to_item(Tree, Item) :-
	tree_to_item(Tree, Item, generation).

tree_to_item(Tree, Item, NormalOrGeneration) :-
	remove_dummy_top_level_rules_from_tree(Tree, RealTree),
	!,
	tree_to_item(RealTree, Item, NormalOrGeneration).
tree_to_item(Tree, Item, NormalOrGeneration) :-
	Tree = phrase(Cat, _LineInfo, Daughters),
	flatten_comma_list(Daughters, FlattenedDaughters),
	remove_line_info_node_from_tree(Tree, Tree1),
	remove_empty_constituents_from_daughters(FlattenedDaughters, DaughtersWithoutEmpties),
	Head =.. [Cat, Tree1, _Feats, _Sem, _Global, In, Out],
	expanded_dcg_clause(Head, Body, NormalOrGeneration),
	daughters_to_body(DaughtersWithoutEmpties, Body, ExpandedBody, In, Out, NormalOrGeneration),
	flatten_dcg_clause_body(ExpandedBody, ExpandedFlattenedBody),
	Item = item(Head, ExpandedFlattenedBody, _Tag),
	!.
tree_to_item(Tree, Item, NormalOrGeneration) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [tree_to_item(Tree, Item, NormalOrGeneration)]),
	fail.

remove_dummy_top_level_rules_from_tree(Tree, RealTree) :-
	nonvar(Tree),
	Tree = phrase(_TopCat, _LineInfo1,
		      phrase(dummy_top, _LineInfo2,
			     (lex(_), RealTree))),
	!.				

daughters_to_body((F, R), (F1, R1), (F2, R2), In, Out, NormalOrGeneration) :-
	!,
	daughters_to_body(F, F1, F2, In, Next, NormalOrGeneration),
	daughters_to_body(R, R1, R2, Next, Out, NormalOrGeneration).
daughters_to_body(Daughter, Body, ExpandedBody, In, Out, NormalOrGeneration) :-
	tree_to_body(Daughter, Body, ExpandedBody, In, Out, NormalOrGeneration).

% Lexical node
tree_to_body(lex(Word), 'C'(_, Word, _), 'C'(In, Word, Out), In, Out, _NormalOrGeneration) :-
	!.
% Empty node
tree_to_body(empty_constituent, _, true, In, In, _NormalOrGeneration) :-
	!.
% Leaf node left after cutting. No further constraints to be added.
tree_to_body(Tree, Body, Body, In, Out, _NormalOrGeneration) :-
	Tree = phrase(Cat, _LineInfo, Daughters),
	var(Daughters),
	Body =.. [Cat, Tree, _Feats, _Sem, _Global, In, Out],
	!.
% Normal non-terminal node
tree_to_body(Tree, Head, ExpandedBody, In, Out, NormalOrGeneration) :-
	Tree = phrase(Cat, _LineInfo, Daughters),
	remove_line_info_node_from_tree(Tree, Tree1),
	flatten_comma_list(Daughters, FlattenedDaughters),
	remove_empty_constituents_from_daughters(FlattenedDaughters, DaughtersWithoutEmpties),
	Head =.. [Cat, Tree1, _Feats, _Sem, _Global, In, Out],
	expanded_dcg_clause(Head, Body, NormalOrGeneration),
	daughters_to_body(DaughtersWithoutEmpties, Body, ExpandedBody, In, Out, NormalOrGeneration),
	!.

remove_line_info_node_from_tree(Tree0, Tree1) :-
	Tree0 = phrase(Cat, line_info(_, Cut, Lines, File), _),
	Tree1 = phrase(Cat, line_info(_, Cut, Lines, File), _),
	!.
remove_line_info_node_from_tree(Tree0, Tree1) :-
	Tree0 = phrase(Cat, line_info(_, Lines, File), _),
	Tree1 = phrase(Cat, line_info(_, no_cut, Lines, File), _),
	!.
 
%----------------------------------------------------------------------

find_cut_nodes_in_item(Item, CutNodes) :-
	feat_for_item(Item, tree=Tree),
	findall(CutNode,
		cut_node_in_tree(Tree, CutNode),
		CutNodes),
	!.
find_cut_nodes_in_item(Item, CutNodes) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [find_cut_nodes_in_item(Item, CutNodes)]),
	fail.

cut_node_in_tree(Tree, CutNode) :-
	nonvar(Tree),
	Tree = phrase(_Cat, line_info(CutNode, _, _, _), Daughters),
	var(Daughters),
	number(CutNode),
	!.
cut_node_in_tree(Tree, CutNode) :-
	nonvar(Tree),
	Tree = phrase(_Cat, _LineInfo, Daughters),
	nonvar(Daughters),
	cut_node_in_daughters(Daughters, CutNode).

cut_node_in_daughters((F, R), CutNode) :-
	!,
	(   cut_node_in_daughters(F, CutNode) ;
	    cut_node_in_daughters(R, CutNode)
	).
cut_node_in_daughters(Tree, CutNode) :-
	cut_node_in_tree(Tree, CutNode).

%----------------------------------------------------------------------

is_cut_node_in_item(NodeID, Item, Cat, Feats) :-
	Item = item(_LHS, RHS, _Tag),
	is_cut_node_in_rhs(RHS, NodeID, Cat, Feats).

is_cut_node_in_rhs((F, R), NodeID, Cat, Feats) :-
	!,
	(   is_cut_node_in_rhs(F, NodeID, Cat, Feats) ;
	    is_cut_node_in_rhs(R, NodeID, Cat, Feats)
	).
is_cut_node_in_rhs(Goal, NodeID, Cat, Feats) :-
	arg(1, Goal, phrase(Cat, line_info(NodeID1, _, _, _), _Daughters)),
	NodeID == NodeID1,
	arg(2, Goal, Feats).

%----------------------------------------------------------------------

join_items_at_node(UpperItem, NodeID, LowerItem, NewItem) :-
	copy_term(UpperItem, UpperItem1),
	UpperItem1 = item(UpperLHS, UpperRHS, _UpperTag),
	copy_term(LowerItem, LowerItem1),
	remove_root_node_number_in_item(LowerItem1, LowerItem2),
	LowerItem2 = item(LowerLHS, LowerRHS, _LowerTag),
	join_items_at_node1(UpperRHS, NodeID, LowerLHS, LowerRHS, ExpandedUpperRHS),
	flatten_dcg_clause_body(ExpandedUpperRHS, ExpandedFlattenedUpperRHS),
	NewItem = item(UpperLHS, ExpandedFlattenedUpperRHS, _Tag),
	!.

remove_root_node_number_in_item(ItemIn, ItemOut) :-
	ItemIn = item(LHS, RHS, Tag),
	remove_root_node_number_in_goal(LHS, LHS1),
	ItemOut = item(LHS1, RHS, Tag),
	!.
remove_root_node_number_in_item(ItemIn, ItemOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [remove_root_node_number_in_item(ItemIn, ItemOut)]),
	fail.

remove_root_node_number_in_goal(LHS1, LHS2) :-
	LHS1 =.. [Cat, Tree1 | Rest],
	LHS2 =.. [Cat, Tree2 | Rest],
	remove_root_node_number_in_tree(Tree1, Tree2).

remove_root_node_number_in_tree(Tree1, Tree2) :-
	Tree1 = phrase(Cat, line_info(_, Cut, Lines, File), Daughters),
	Tree2 = phrase(Cat, line_info(_, Cut, Lines, File), Daughters).

join_items_at_node1((F, R), NodeID, LowerLHS, LowerRHS, (F1, R)) :-
	join_items_at_node1(F, NodeID, LowerLHS, LowerRHS, F1),
	!.
join_items_at_node1((F, R), NodeID, LowerLHS, LowerRHS, (F, R1)) :-
	join_items_at_node1(R, NodeID, LowerLHS, LowerRHS, R1),
	!.
join_items_at_node1(Goal, NodeID, LowerLHS, LowerRHS, LowerRHS) :-
	functor(Goal, Cat, 6),
	Goal =.. [Cat, Tree | _Rest],
	Tree = phrase(Cat, line_info(NodeID, _, _Lines, _File), _Daughters),
	attempt_to_unify_cat_goals(Goal, LowerLHS),
	!.
 
%----------------------------------------------------------------------

warn_about_missing_vocabulary_for_atom(Atom) :-
	split_atom_into_words(Atom, Words),
	warn_about_missing_vocabulary_for_words_list(Words).

warn_about_missing_vocabulary_for_words_list(Words) :-
	unknown_words_in_word_list(Words, MissingWords),
	(   MissingWords = [] ->
	    true
	;
	    format2error('~NWords not in current vocabulary: ~w~n', [MissingWords])
	).

unknown_words_in_word_list([], []).
unknown_words_in_word_list([F | R], [F | R1]) :-
	\+ regulus_preds:vocabulary_item(F),
	!,
	unknown_words_in_word_list(R, R1).
unknown_words_in_word_list([_F | R], R1) :-
	!,
	unknown_words_in_word_list(R, R1).

%----------------------------------------------------------------------

log_stepper_command(Command, Args) :-
	log_event('STEPPER_COMMAND', "*null_comment*", [Command | Args]),
	!.

log_stepper_command(Command, CommentChars, Args) :-
	log_event('STEPPER_COMMAND', CommentChars, [Command | Args]),
	!.
log_stepper_command(Command, CommentChars, Args) :-
	format2error('~N*** Error: bad call: ~w~n', [log_stepper_command(Command, CommentChars, Args)]),
	fail.

