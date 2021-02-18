
:- module(logicmoo_autodoc, [autodoc_sanity_tests/0]).

:- use_module(library(instance_prolog_docs)).

%! autodoc_sanity_tests is semidet.
%
% Autodoc Sanity Tests.
%
autodoc_sanity_tests:-
  autodoc_file(library(episodic_memory/'adv_action.pl')),
  autodoc_file(library(episodic_memory/'*.pl')),
  autodoc_file(library(instant_prolog_docs)),
  %autodoc_file(library(logicmoo/'*.pl')),
  !.



:- fixup_exports.

