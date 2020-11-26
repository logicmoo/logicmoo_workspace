:- encoding(iso_latin_1).
/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Günter Kniesel (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

% Date: 25.06.2006

/*
 * This file contains predicates for locating definitions (declarations and 
 * clauses) in SWI-Prolog modules and testing visibility of predicates in 
 * modules.
 *
 * Used in pdt/pl/{pdt_search,pdt_xref}.pl and in
 * ** pdt.runtime.builder/prolog-src/analyzer/metafile_referencer.pl
 */
:- module( utils4modules_visibility, 
         [
           module_of_file/2,               % File, Module
           
           empty_module/1,                 % (Module)
           pdt_hidden_module/1,                % Module
           
           visible_in_module/3,	           % Module, Name, Arity
           declared_in_module/2,           % Module, Head
           declared_in_module/3,		   % Module, Head, DeclaringModule (not generating)
           declared_in_module/4,           % Module, Name, Arity, DeclaringModule 
           referenced_but_undeclared/3,    % Module, Name, Arity 
           declared_but_undefined/3,       % Module, Name, Arity 
           defined_in_module/2,            % Module, Head
           defined_in_module/3,            % Module, Name, Arity
           defined_in_module/4,            % Module, Name, Arity, DefiningModule
           defined_in_files/4,             % Module, Name, Arity, Locations
           defined_in_file/6,              % Module, Name, Arity, Nth,File,StartingLine
           defined_in_file/7,              % Module, Name, Arity, Ref, Nth,File,StartingLine
           declared_in_file/4              % Module, Name, Arity, Location=[File-[Line]]          
           
           ]
 ).
 
:- use_module(pdt_support, [pdt_support/1]).
:- use_module(library(lists), [member/2]).

%% pdt_hidden_module(M)
%
pdt_hidden_module(M) :- starts_with_dollar(M).

starts_with_dollar(Atom) :- sub_atom( Atom, 0, 1, _, '$'). 
        
    
%% empty_module(?Module)
%
% Generate or test empty modules (which contain no own 
% predicate declaration, let alone any clauses).
empty_module(Module) :- 
    (  var(Module)                         % generating mode
    -> ( setof( M, empty_module__(M), Set),% avoid multiply returning
         member(Module,Set)                % ... the same module
       )
    ;  empty_module__(Module), !           % checking mode
    ). 
    
% A module is empty if all its predicates are imported
% (put differently: none of its predicates is not imported).    
empty_module__(M) :-                       % generator skips system Modules :-(
    current_predicate(M:_/_),
    \+ ( current_predicate(M:N/A),
         functor(H,N,A),
         \+ predicate_property(M:H, imported_from(_))
    ).

        
%% module_of_file(?File,?Module)
%
% Module is the module defined in File (or 'user' 
% if the file defines no explicit module or the  
% defined module is a hidden system module.
module_of_file(File,Module):-
    module_property(Module,file(File)).    % Fails if File defines no module
                                           % or if it defines a hidden module
                                           % whose name starts with $
module_of_file(File,Module):-
    atom(File),                            % If File is provided as input
    \+ module_property(_,file(File)),      % and there is no module in that file
    ( Module=user                          % the default module is 'user'
    ; Module=system                        % but it could also be 'system'
    ).

%% visible_in_module(?Module,?Name,?Arity) is nondet.
%
% Suceed if the predicate Name/Arity is visible in Module
% either via a local declaration or import. 
% 
% The used predicate_property(Head,visible) is documented as:
%    True when predicate can  be called without raising a  predicate
%    existence  error.    This  means  that  the  predicate  is  (1)
%    defined, (2) can be  inherited from one of the  default modules
%    (see default_module/2) or (3) can be autoloaded.  The behaviour
%    is logically  consistent iff  the propery  visible is  provided
%    explicitly.   If  the property  is left  unbound, only  defined
%    predicates are enumerated.
          
visible_in_module(Module,Name,Arity) :-
%   (ground(functor(Head,Name,Arity),
%   predicate_property(Module:Head,visible).
% The above was created by Jan Wielemaker during his visit to our group 
% in Nov. 2011. It is intended as a better behaved alternative to the
% strangely inconsistent versions of current_predicate/1 and /2.
	current_module(Module),
	setof(
		Name/Arity,
		Module^Name^Arity^(Module:current_predicate(Name/Arity)),
		PIs
	),
	member(Name/Arity, PIs).
%  
% <-- Beware of current_predicate/2: It hides system modules! 
% Only current_predicate/1 returns ALL modules that see Name/Arity,
% including (hidden) system modules, such as '$syspreds'. 
% BUT it does it only if Name AND Arity are instantiated.
% If only Name is instantiated, current_predicate/1 only returns  
% non-system modules that actually CALL Name/_. 

/*
Inconsistent behaviour of current_predicate(M:F/N):

3 ?- F=visible,current_predicate(M:F/N).
F = visible,
M = system,
N = 1 ;

F = visible,
M = gui_tracer,
N = 1 ;

false.

4 ?- F=visible,M='$syspreds', current_predicate(M:F/N).
M = '$syspreds',
F = visible,
N = 1 ;

false.

5 ?- F=visible,N=1,findall(M:F/N, current_predicate(M:F/N), All), length(All,L).
F = visible,
N = 1,
All = [emacs_buffer:visible/1, pce_hyper:visible/1, make:visible/1, 'condor.depend.expand.dnf':visible/1, targetModule:visible/1, prolog_break:visible/1,
utils4modules:visible/1, emacs_fundamental_mode:... / ..., ... : ...|...],
L = 203.
*/


%%
% declared_in_module(+Module, +Head) is det.
% declared_in_module(+Module, ?Head) is nondet.
% declared_in_module(?Module, ?Head) is nondet.
%  
% Determine whether the predicate indicated by Head has a *local*
% (non-imported) declaration in the module Module. The predicate 
% succeeds if there is at least a declaration (but possibly no  
% single clause) of the predicate in the module. If you want it to  
% fail you must abolish the predicate declaration, not just  
% retract all its clauses. See the built-ins Prolog predicates   
% abolish/1, abolish/1, retractall/1, retract/1
%  
declared_in_module(Module, Head) :-
   ( true ; Module = '$syspreds'),                     % Try also hidden module
   current_predicate(_, Module:Head),                  % Head is declared
   \+ predicate_property(Module:Head, imported_from(_)). % but not imported


%% declared_in_module(?Module,?Name,?Arity,?DeclaringModule) is nondet.
%
% Succeed if the predicate Name/Arity visible in Module is declared in 
% DeclaringModule. Module = DeclaringModule holds if Module contains
% a local (non-imported) declaration. Otherwise, DeclaringModule is the
% module from which the declaration is imported. 
% Note that predicate suceeds even if there is no defining clause for 
% Name/Arity in DeclaringModule (definition implies declaration but 
% not vice versa). 

declared_in_module(Module,Name,Arity,DeclaringModule) :-
    visible_in_module(Module,Name,Arity),                % Name/arity is visible in Module    
	functor(Head,Name,Arity), 
    (  predicate_property(Module:Head, imported_from(M)) % by being imported
    -> DeclaringModule = M
    ;  DeclaringModule = Module                          % by being declared locally
    ).
 
 
%% declared_in_module(?Module,+Head,?DeclaringModule) is nondet.
%
% Unlike 
%   declared_in_module(Module,Name,Arity,DeclaringModule),
%   functor(Head,Name,Arity) 
% this call does not generate, it assumes Head is instantiated!
%
declared_in_module(Module,Head,DeclaringModule) :-   
    (  predicate_property(Module:Head, imported_from(M)) % imported
    -> DeclaringModule = M
    ;  DeclaringModule = Module                          % declared locally
    ),
    functor(Head,Name,Arity), 
    visible_in_module(Module,Name,Arity). % Name/arity is visible in Module   
    
%% defined_in_module(?Module,+Head) is nondet.
%
% There is at least one clause for Head that is physically
% contained in Module (not just visible by import). 
% The clause(es) in the module can come from different files
% (because of multifile predicates).

defined_in_module(Module,Head) :- 
    functor(Head,Name,Arity),
    defined_in_module(Module,Name,Arity).

%% defined_in_module(?Module,?Name,?Arity) is nondet.
%
% There is at least one clause for Name/Arity that is physically
% contained in Module (not just visible by import). 
% The clause(es) in the module can come from different files
% (because of multifile predicates).

defined_in_module(Module,Name,Arity) :- % <<< deleted 1 argument
    defined_in_module(Module,Name,Arity,Module).

%% defined_in_module(?ReferencedModule,?Name,?Arity,?DefiningModule) is nondet.
%
% The predicate Name/Arity visible in ReferencedModule is defined in 
% DefiningModule. That is, there is at least one clause for Name/Arity 
% that is physically contained in DefiningModule (not just visible by import). 
% The clause(es) in DefiningModule can come from different files
% (because of multifile predicates).
defined_in_module(ReferencedModule,Name,Arity,DefiningModule) :- 
    declared_in_module(ReferencedModule,Name,Arity,DefiningModule),
    functor(Head,Name,Arity),
    \+ predicate_property(DefiningModule:Head, imported(_)).

%% list_module(+Module)
% List all predicates DEFINED in Module.
list_module(Module) :-
    nonvar(Module), 
    forall( defined_in_module(Module,F,A), listing(Module:F/A) ).
 
%% declared_but_undefined(-Module,-Name,-Arity,?DeclaringModule) is nondet
% 
% Succeed if the predicate Name/Arity visible in Module is declared in 
% DeclaringModule but not defined by any clause. 
declared_but_undefined(Module,Name,Arity) :- % <<< deleted 1 argument
    declared_in_module(Module,Name,Arity,Module),
    functor(Head,Name,Arity),
    \+ (predicate_property(Module:Head, number_of_clauses(X)), X>0).



%% referenced_but_undeclared(?Module,?Name,?Arity) is nondet
% 
% Succeed if the predicate Name/Arity is called in Module but is not 
% visible there. 
referenced_but_undeclared(Module,Name,Arity) :-
    predicate_property(Module:Head,undefined),
    functor(Head,Name,Arity).   

%% defined_in_file(-Module,-Name,-Arity,-N,?File,?Line) is nondet
%  defined_in_file(+Module,+Name,+Arity,+N,?File,?Line) is det
%
%  Get the source locations (File and Line) of all clauses
%  that define Module:Name/Arity.
 
defined_in_file(Module,Name,Arity, N,File,Line) :-
	defined_in_file(Module,Name,Arity, _Ref, N,File,Line).

defined_in_file(Module,Name,Arity, Ref, N,File,Line) :-
    declared_in_module(Module,Name,Arity,Module),
    functor(Head,Name,Arity),
    nth_clause(Module:Head,N,Ref),
    clause_property(Ref,file(File)),      
    clause_property(Ref,line_count(Line)).


%% declared_in_file(?Module, Head, ?File, ?Line) is nondet
%
% File is the file containing the declaration for the predicate Head, 
% which is visible in Module. 
% Line = 1 (approximating the line number information missing for declarations). 

declared_in_file(Module,Name,Arity,[File-[location(Line, null)]]) :-
    functor(Head,Name,Arity),
    predicate_property(Module:Head,foreign),
    !,
    File = foreign,
    Line = 0.
declared_in_file(Module,_Name,_Arity,[File-[location(Line, null)]]) :-
    module_property(Module, file(File)),           % declaration in known file
    !,
    Line=1.                                        % guess the unknown line nr
declared_in_file(Module,Name,Arity,[File-[location(Line, null)]]) :-
    functor(Head,Name,Arity),
    predicate_property(Module:Head,dynamic),
    File = (dynamic) ,  
    Line = 0.

%% 
% defined_in_files(+Module,+Name,+Arity, Locations) is semidet
% 
% Locations is a list of File-Lines terms. 
% Lines is a list of numbers indicating the starting lines
% of the clauses for Module:Name/Arity contained in File. 
defined_in_files(Module,Name,Arity,Locations) :-
    ( var(Module)
    ; var(Name)
    ; var(Arity)
    ),
    throw( input_argument_free(defined_in_files(Module,Name,Arity,Locations)) ).
     
defined_in_files(Module,Name,Arity,Locations) :-
    findall( File-Lines,
             setof( location(Line, Ref), Module^Name^Arity^N^
                    defined_in_file(Module,Name,Arity, Ref, N, File,Line),
                    Lines
             ),
             Locations
%    ),
%    (  SrcLocations == []
%    -> ( declared_in_module(Module,Name,Arity,DeclaringModule),
%         declared_in_file(DeclaringModule,Name,Arity,DeclLocation),
%         Locations = DeclLocation
%       ) 
%    ;  Locations = SrcLocations
    ).



