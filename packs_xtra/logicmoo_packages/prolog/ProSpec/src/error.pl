%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% error module for prospec
%% -------------------------------------

%% Author: bernd thomas 
%% E-mail: bthomas@informatik.uni-koblenz.de
%% --------------------------------------------------
%% $Id: error.pl,v 1.1 1998/02/10 18:01:01 bthomas Exp $
%% $Log: error.pl,v $
%% Revision 1.1  1998/02/10 18:01:01  bthomas
%% Initial revision
%%

% ----------------------------------------
% prospec error codes
% ----------------------------------------

error_code(fileex(File), ["\nERROR: no file named ",File]).

error_code(no_spec, ["\nERROR: no specification found ! "]).
error_code(double_spec, ["\nERROR: more than one specification found ! "]).

error_code(spec,["\nERROR: specification not correct !"]).

error_code(specstart,['\nERROR: ProSpec expects a specification at the \
beginning of the file.\nThe specification has the following format:\n\n\
\tbegin(prospec).\n\n\t\
% a list of used sorts\n\t\
sorts : (human,man,woman).\n\n\t\
% the sorts hierachy \n\t\
subsorts : (man subsort_of human, woman subsort_of human).\n\n\t\
% the relations/predicates and the sort of their arguments \n\t\
relations : ( loves : (man,woman), dead : (human) ).\n\n\t\
% the functions and the sort of arguments and the result \n\t\
functions : ( xxx : (man,woman) => human ).\n\n\t\
% functions or relations that have to be ignored (not sorted) \n\t\
ignore : ( printf, is ).\n\n\t\
end(prospec).\n']).

error_code(specend,['\nERROR: specification terminated incorrectly. \
\nThe specification has to be terminated by: end(prospec).\n']).

error_code(relfc_def(Term),["\nERROR: relation/function not specified \n** ",Term," **"]).

error_code(sortconf(TTerm,Proto),["\nERROR: sorts conflict \n** Input: ",TTerm,"\n** Expected: ",Proto]).

error_code(sortunknown(Sort),["\nERROR: unknown sort ** ",Sort," ** in parameter list"]).

error_code(sortconf2(Sort,Term,FT),["\nERROR: sorts conflict you expect sort ** ",Sort," ** but ** ",Term," ** is of sort ** ",FT," **"]).

error_code(specsort(Sort),["\nERROR: none defined sort in specification ** ",Sort," **\n"]).

error_code(doublesort(Sort),["\nERROR: double definition of subsort ** ",Sort," **\n"]).

error_code(ignore,["\nERROR: in ignore definition\n"]).

error_code(double(Type,Ele),['\nError: ',Type,' double definition of  **  ',Ele,' **']).

% Peter:
error_code(inconsistent(Var,Sort),["\nERROR: Variable ** ",Var," ** inconsistently sorted by ** ", Sort, " **\n"]).
