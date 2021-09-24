
:- include(library(logicmoo_test_header)).

test_here(X):- notrace(mpred_test(e2c(X))).

test_convo_file(File):- atom_contains(File,'0'),!.
test_convo_file(File):-
  open(File,read,IS),
  repeat,
  catch(read_term(IS,Term,[syntax_errors(error)]),_,fail),
   (Term == end_of_file -> close(IS) ;  
    (ignore(test_convo_term(Term)),fail)).

test_convo_term(X):- 
 forall((sub_term(E,X),atomic(E),atom_contains(E,' '), 
   %dont parse end comment (Yet)
   \+ atom_contains(E,'CasAm')),
  test_here(E)).

:- prolog_load_context(directory,X), cd(X), 
   expand_file_name('*.plt',Files),
   maplist(test_convo_file,Files).


% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_agi/t/agi_tests/comprehension/comprehension_01.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.agi.agi_test.comprehension/COMPREHENSION_01/logicmoo_agi_agi_test_comprehension_COMPREHENSION_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ACOMPREHENSION_01 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/645
