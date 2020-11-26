/* * module * 
% last file loaded per file (loses at any module side effects
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
%
*/


%:- loading_module_h(CM), (registered_module_type(utility,CM)->export_all_preds;true).

% :- file_end(moo).

% :- loading_module_h(CM), (context_module(CM) -> retract(loading_module_h(CM)) ; true).
% :- context_module(CM),(registered_module_type(utility,CM))->module_predicates_are_exported(CM);module_predicates_are_exported(CM).
% :- retract(loading_module_h(_)).

:- all_source_file_predicates_are_transparent.

:- fixup_exports.



