% ===================================================================
% File 'parser_ape.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_ape.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================

% :-module(parser_ape,[]).

% ==============================================================================
:- use_module(library(logicmoo_nlu/parser_sharing)).
:- shared_parser_data(talkdb:talk_db/3).

:- absolute_file_name(logicmoo_nlu_ext('ape'),Dir,[file_type(directory)]),
   assertz(user:file_search_path(ape,Dir)).
:- absolute_file_name(logicmoo_nlu_ext('ape/prolog'),Dir,[file_type(directory)]),
   asserta(user:file_search_path(ape,Dir)).




%warning(F,A):- sformat(S,F,A), dmsg(warning(S)).
%error(C1,C2):- trace_or_throw(error(C1,C2)).

:- reexport(ape(parser/ace_to_drs)).
:- reexport(ape(get_ape_results)).
:- reexport(ape(utils/drs_to_drslist)).
:- reexport(ape(utils/drs_to_sdrs)).
% sorts, grammar, grammar_functionwords, grammar_contentwords
:- reexport(ape('parser/grammar.plp')).
:- reexport(ape('parser/grammar_words'),[reset_progress_record/1]).
:- reexport(ape(parser/ape_utils)).
:- reexport(ape(parser/tokenizer)).
:- reexport(ape(utils/morphgen), [
	acesentencelist_pp/2
	]).
:- reexport(ape(utils/is_wellformed), [
	is_wellformed/1
	]).
:- reexport(ape(utils/drs_to_ascii)).
:- reexport(ape(utils/trees_to_ascii)).
:- reexport(ape(utils/drs_to_ace), [
	drs_to_ace/2
	]).
:- reexport(ape(logger/error_logger), [
	clear_messages/0,
	get_messages/1,
	is_error_message/4
	]).

:- set_prolog_flag(float_format, '%.11g' ).

% Import the lexicons
:- style_check(-(singleton)).
:- style_check(-(discontiguous)).
:- reexport(ape(lexicon/clex)).
:- reexport(ape(lexicon/ulex)).
:- style_check(+discontiguous).
:- style_check(+singleton).

:- reexport(ape('utils/morphgen')).

:- reexport(ape('utils/ace_niceace')).

:- reexport(ape('utils/drs_to_xml')).
:- reexport(ape('utils/drs_to_fol_to_prenex')).
:- reexport(ape('utils/drs_to_ascii')).
:- reexport(ape('utils/drs_to_ace')).
:- reexport(ape('utils/drs_to_coreace')).
:- reexport(ape('utils/drs_to_npace')).
:- reexport(ape('utils/drs_to_html')).
:- reexport(ape('utils/drs_to_ruleml')).
:- reexport(ape('utils/tree_utils')).
:- reexport(ape('utils/trees_to_ascii')).
:- reexport(ape('utils/drs_to_tptp')).
:- reexport(ape('lexicon/clex')).
:- reexport(ape('lexicon/ulex')).
:- reexport(ape('parser/ace_to_drs')).
:- reexport(ape('logger/error_logger')).

:- reexport(ape('utils/xmlterm_to_xmlatom')).

:- reexport(ape('utils/serialize_term')).


% See T:\opt\logicmoo_workspace\packs_sys\logicmoo_nlu\ext\ape\examples\output_tests.pl
% See T:/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/ape/tests/acetexts.nldata
:- ensure_loaded(ape('tests/acetexts')).
% See T:/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/ape/tests/test_owlswrl.pl
:- ensure_loaded(ape('tests/test_owlswrl')).


:- ensure_loaded(ape('../AceRules/engine/run_acerules')).
:- ensure_loaded(ape('../AceRules/engine/run_testcases')).
:- ensure_loaded(ape('../AceRules/engine/acerules_processor')).


%:- reexport(ape('utils/owlswrl/get_owl_output')).


:- export(any_to_ace_str/2).
any_to_ace_str(I,S):- words_of(I,M),
  tokenizer:expand_contracted_forms(_All,M,MS),
 % ignore((M\==MS,dmsg(expand_contracted_forms(M,MS)))),
  any_to_str(MS,SS), replace_in_string(['. .'='.'],SS,S).

try_ace_drs(I,O):- any_to_ace_str(I,S),!,
   ace_to_drs:aceparagraph_to_drs(S,on,off,1,_Sentences,_Trees,UnresolvedDrs,O,Messages,_Time),
   \+ \+ nop(wdmsg(UnresolvedDrs=Messages)),
   \+ \+ should_learn(O).

try_ace_drs(I):- make, try_ace_drs(I,O), \+ \+ exec_fol(I=O).

exec_fol(X=I):- !, nonvar(I),
  my_drs_to_fol_kif(I,O), 
  try_ace_fol(O,M), format('~N~n'), print_tree_nl(m=M),  
  format('~N~n'),
  print_tree_nl(X),write(' == '),
  print_tree_nl(I).
exec_fol(FOL):- exec_fol(exec_fol=FOL),!.


my_drs_to_fol_kif(DRS1,DRS5):- drs_to_sk(DRS1,DRS5).

try_ace_fol(I,O):- my_drs_to_fol(I,M),my_fol_kif(M,O),!.
my_drs_to_fol(I,O):- on_x_fail(drs_fol(I,O)),!.
my_drs_to_fol(OI,OI).


their_drs_to_coreace(DRS1,X):- drs_to_coreace:drs_to_coreace(DRS1,X),good_coreace(X).

good_coreace(X):- nonvar(X), X\==[],X\==['ERROR'].
try_ace_eng(O,unk(O)):- \+ compound(O),!.
try_ace_eng(DRS1,O):- their_drs_to_coreace(DRS1,O),good_coreace(O),!. 
try_ace_eng(IsList,O):- is_list(IsList),!,maplist(try_ace_eng,IsList,O).
try_ace_eng(drs([],Y),O):- term_variables(Y,[V|_]),try_ace_eng(drs(V,Y),O),!.
try_ace_eng(drs(X,Y),acedrs(X,O)):- !, try_ace_eng(Y,O),!.
try_ace_eng(NV,O):- NV=..[X,Y], try_ace_eng(Y,M),!,O=..[X,M].
try_ace_eng(T-_,aceo(O)):- term_variables(T,[V|_]), their_drs_to_coreace(drs([V],[T-1/2]),O),!.
try_ace_eng(T,O):- functor(T,_,A),A>2,!,try_ace_eng((T - (0/1)), O).
try_ace_eng(O,unk(O)):-!.

/*
object(B,reason,countable,na,eq,1)
predicate(Wait_In_Usually_For,wait,named('John'))
 predicate/3
 modifier_pp/4
 modifier_pp
*/

% John in no park usually for every reason waits.
% 'If there is a park X1 
%   then it is false that
%     if there is a reason X2 
%       then John waits usually for the reason X2 in the park X1.'
drs_to_sk(DRS1,DRS1):- copy_term(DRS1,DRS5),numbervars(DRS5,0,_),!,
  drs_to_sk1(DRS5,_DRS7),ignore((try_ace_eng(DRS1,List),maplist(wdmsg,List))).

drs_to_sk1(DRS1,DRS5):- notrace(catch(drs_to_sk11(DRS1,DRS5),_,fail)),!.
drs_to_sk1(DRS1,DRS1).
drs_to_sk11(DRS1,DRS5):-
 parser:(
  %generate_drs:generate_drs(
  %meta_preprocess(InputCodes, PlainText, LabelMap, OverridesPre),
	log('parser.priority_handler'),
	%priority_handler(OverridesPre, Overrides),
	%debug(list, 'Overrides Statements:', Overrides),
	% --- STEP 2 ---
	% Using APE to parse the ACE rules and facts.
	%log('parser.generate-drs'),
	%generate_drs(PlainText, Guess, DRS1),
	debug(drs, 'DRS Original:', DRS1),
	% --- STEP 3 ---
	% First check of the structure of the DRS (Level 1).
	%log('parser.check-drs-1'),
	%check_drs(DRS1, 1),
	% --- STEP 4 ---
	% Transformation of double implications.
	%log('parser.double_implication'),
	transform_double_implication(DRS1, DRS2),
	% --- STEP 5 ---
	% Condense the DRS: reduction of the number of predicates and other transformations.
	%log('parser.condense-drs'),
	condense_drs(DRS2, DRS3),
	debug(drs, 'DRS Condensed:', DRS3),
	% --- STEP 6 ---
	% Collection of group templates.
	log('parser.collect-templates'),
	clear_templates,
	collect_templates(DRS3),
	findall(Template, acetmp:group_template(Template), Templates),
	debug(list, 'Group Templates:', Templates),
	% --- STEP 7 ---
	% Group predicates.
	log('parser.group-predicates'),
	copy_term(DRS3, DRS4),
	group_predicates(DRS4, DRS5),
	debug(drs, 'DRS Grouped:', DRS5),
	% --- STEP 8 ---
	% Check if the atom-restriction was violated.
	log('parser.check-grouping'),
	check_grouping(DRS5),
	% --- STEP 9 ---
	% Second check of the structure of the DRS (Level 2).
	log('parser.check-drs-2')
	% check_drs(DRS5, 2),
	% --- STEP 10 ---
  ).

drs_to_sk2(DRS5,LabelMap,Overrides,Rules):-
 parser:(
	% Skolemization of the variables.
	log('parser.skolemize-drs'),
	skolemize_drs(DRS5),
	debug(drs, 'DRS Grouped & Skolemized:', DRS5),
  in_cmt(block,print_tree_nl(DRS5)),
	% --- STEP 11 ---
	% Third check of the structure of the DRS (Level 3).
	log('parser.check-drs-3'),
	%check_drs(DRS5, 3),
	% --- STEP 12 ---
	% Creation of rules from the labeled DRS.
	log('parser.create-rules'),
 (
	create_rules(DRS5, LabelMap, Rules1)),
 must_or_rtrace(
	get_setof(Rule, member(Rule, Rules1), Rules2)),
	append(Rules2, Overrides, Rules),
	debug(rules, 'Rules:', Rules),
	log('parser.finished')).

my_fol_kif(Var,O):- var(Var),!,Var=O,!. %freeze(Var,my_fol_kif(Var,O)).
my_fol_kif(I,O):- is_list(I),!,maplist(my_fol_kif,I,O).
my_fol_kif(I-_,O):-!,my_fol_kif(I,O).
my_fol_kif(I,O):- compound(I), compound_name_arguments(I,N,A),maplist(my_fol_kif,A,AO),compound_name_arguments(O,N,AO),!.
my_fol_kif(I,O):- I=O.

test_aceese("They know everything that he owns.").
test_aceese("John sees two books that sit on the shelf by the fire").
test_aceese("John sees two books that sit on the shelf that is burning").
test_aceese("John sees two books that sit on the shelf and they are burning").

test_aceese('A curiously carved table is a supporter in the Library.').
test_aceese('A rosewood bench is a supporter in the laboratory. "A rosewood bench, scarred and burned by years of incantations, fills half of the room."').
test_aceese('An iron crucible is an open container on the rosewood bench. "Bob''s crucible bubbles merrily on the bench."').
test_aceese('A cupboard is a closed openable container in the laboratory. "A cupboard of great antiquity quietly blends into the corner shadows."').
test_aceese('A plastic bag is a closed transparent portable openable container on the carved table.').
test_aceese('A Chinese puzzle box is a locked lockable closed openable portable container in the plastic bag.').
test_aceese('A pinch of ginger is a portable thing in the puzzle box.').

:- fixup_exports.

