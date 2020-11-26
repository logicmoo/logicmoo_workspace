/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Günter Kniesel
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 * This file contains predicates for working with SWI-Prolog modules.
 */
:- module( utils4modules, [

           module_of_file/2,               % File, Module
           
           empty_module/1,                 % (Module)
           hidden_module/1,                % Module
           
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
           declared_in_file/4,             % Module, Name, Arity, Location=[File-[Line]]          

           assert_in_module/2,             % Module, Head
           assert_in_module/3,             % Module, Head, Body
           clause_in_module/2,             % Module, Head
           clause_in_module/3,             % Module, Head, Body
           retract_in_module/2,            % Module, Head
           retract_in_module/3,            % Module, Head, Body
           retractall_in_module/2,         % Module, Head
           call_in_module/2,               % Module, Goal
           call_and_report_contex_module/1,% Goal
           report_contex_module/1,         % Module        
           listing_in_module/2,            % Module, FunctorOrHeadOrFkt/Arity
           
           copy_module_predicate/3,        % SrcMod, TargetMod, Head
           move_module_predicate/3         % SrcMod, TargetMod, Head
           ]
 ).
 
:- module_transparent(call_and_report_contex_module/1).
call_and_report_contex_module(Goal) :- 
    context_module(M),
    log_on_stdout('Calling ~w.~n',[M:Goal]),
    call(M:Goal).

:- module_transparent(report_contex_module/1).
report_contex_module(M) :- context_module(M),
    log_on_stdout('Context module = ~w.~n',[M]).
    

%% hidden_module(M)
%
hidden_module(M) :- starts_with_dollar(M).

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
    \+ module_property(Module,file(File)), % and there is no module in that file
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
    
    current_predicate(Module:Name/Arity).
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
% does not generate!
%
declared_in_module(Module,Head,DeclaringModule) :-   
    (  predicate_property(Module:Head, imported_from(M)) % imported
    -> DeclaringModule = M
    ;  DeclaringModule = Module                          % declared locally
    ),
    functor(Head,Name,Arity), 
    visible_in_module(Module,Name,Arity). % Name/arity is visible in Module   
    
% Defined = There is at least one clause in the declaring module.
% Then the declaring module is also a defining module.
% Note that the clause(es) in the module can come from different files.

defined_in_module(Module,Head) :- 
    functor(Head,Name,Arity),
    defined_in_module(Module,Name,Arity).

%% defined_in_module(Module,Name,Arity) is nondet.
%
defined_in_module(Module,Name,Arity) :- % <<< deleted 1 argument
    defined_in_module(Module,Name,Arity,Module).

defined_in_module(ReferencedModule,Name,Arity,DefiningModule) :- 
    declared_in_module(ReferencedModule,Name,Arity,DefiningModule),
    functor(Head,Name,Arity),
    \+ predicate_property(DefiningModule:Head, imported(_)).

 
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
% Succeed if the predicate Name/Arity is called in Module is but not 
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
    declared_in_module(Module,Name,Arity,Module),
    functor(Head,Name,Arity),
    nth_clause(Module:Head,N,Ref),
    clause_property(Ref,file(File)),      
    clause_property(Ref,line_count(Line)).
%    ( module_property(M, file(F))
%    -> DDDeclaringModule=M
%    ;  DDDeclaringModule=unknown
%    ).

%% declared_in_file(?Module, Head, ?File, ?Line) is nondet
%
% File is the file containing the declaration for the predicate Head, 
% which is visible in Module. 
% Line = 1 (approximating the line number information missing for declarations). 

declared_in_file(Module,Name,Arity,[File-[Line]]) :-
    functor(Head,Name,Arity),
    predicate_property(Module:Head,foreign),
    !,
    File = 'No Prolog source code (only compiled external language code)',
    Line = 0.
declared_in_file(Module,_Name,_Arity,[File-[Line]]) :-
    module_property(Module, file(File)),  % declaration in known file
    !,
    Line=1.                                        % guess the unknown line nr
declared_in_file(Module,Name,Arity,[File-[Line]]) :-
    functor(Head,Name,Arity),
    predicate_property(Module:Head,foreign),
    File = 'Dynamic predicate, no Prolog source code (only dynamic code)' ,  
    Line = 0.

%% 
% declared_or_defined_in_files(+Module,+Name,+Arity, Locations) is semidet
% 
% Locations is a list of File-Lines terms whose Lines
% is a list of numbers indicating the starting lines of
% the clauses for Module:Name/Arity contained in File. 
defined_in_files(Module,Name,Arity,Locations) :-
    ( var(Module)
    ; var(Name)
    ; var(Arity)
    ),
    throw( input_argument_free(defined_in_files(Module,Name,Arity,Locations)) ).
     
defined_in_files(Module,Name,Arity,Locations) :-
    findall( File-Lines,
             setof( Line, Module^Name^Arity^N^
                    defined_in_file(Module,Name,Arity, N,File,Line),
                    Lines
             ),
             SrcLocations
    ),
    (  SrcLocations == []
    -> ( declared_in_module(Module,Name,Arity,DeclaringModule),
         declared_in_file(DeclaringModule,Name,Arity,DeclLocation),
         Locations = DeclLocation
       ) 
    ;  Locations = SrcLocations
    ).

/*
 * call_local_pred_of_module(+Module, +Head) is nondet
 * 
 * Call Head in Module only if Module contains a local definition 
 * (that is, does not import the predicate from another module).
 */  
call_local_pred_of_module(Module, Head) :-
    functor(Head,F,N),
    defined_in_module(Module, F,N,Module),   
    call(Module:Head).
    
/*
 * call_in_module(+Module, +Head) is nondet
 * 
 * Call Head in Module regardless whether Head is locally defined or
 * imported from another module. 
 */
call_in_module(Module,Goal) :- 
   nonvar(Module)
   -> call( Module:Goal )
    ; ctc_error('Goal called in variable module: ~w:~w.', 
                 [Module,Goal]
   ).

   
/** 
 * assert_in_module(?Mod,?Head      ) is det
 *
 * Assert clauses in an explicitly specified module. 
 * 
 * CAUTION: Due to the semantics of modules in SWI-Prolog, the  
 * clause ends up in the module from which the explicitly specified 
 * module imports the declaration of the predicate to be asserted.
 * 
 * assert_in_module/2,3 differs from a normal assert called in a 
 * module, which would assert the fact into the module containing
 * the invocation of assert (unless the predicate containing the 
 * invocation and all its parents on the stack were "module_transparent"
 * and the invoking module was loaded via use_module ...). 
 */
assert_in_module(Mod,Head      ) :- assert( :(Mod,Head)            ).
assert_in_module(Mod,Head,[]   ) :- assert( :(Mod,Head)            ).
assert_in_module(Mod,Head,Body ) :- not(is_list(Body)), !, assert( :(Mod,':-'(Head,Body)) ).

assert_in_module(Mod,Head, []) :-
    assert_in_module(Mod,Head ).
    
assert_in_module(Mod,Head,      [Opt]) :- 
   (  Opt == unique
   -> assert_unique( :(Mod,Head))
   ;  assert(        :(Mod,Head))
   ).
assert_in_module(Mod,Head,Body, [Opt]) :- 
   (  Opt == unique
   -> assert_unique( :(Mod,':-'(Head,Body)))
   ;  assert(        :(Mod,':-'(Head,Body)))
   ).


/*
 * Get or retract clauses from an explicitly specified module.
 * The Module argument must not be a variable! 
 * These predicates never access clauses imported from other modules.
 * They only get or delete clauses that are actually asserted in the
 * specified module. This appears to be standard SWI Prolog behaviour
 * meanwhile but it sometimes changed, so for safety we prefer to 
 * enforce it ourselves. 
 */
clause_in_module(Mod,Head	)   :- defined_in_module(Mod, Head), clause( :(Mod,Head),_ ) .
clause_in_module(Mod,Head,Body) :- defined_in_module(Mod, Head), clause( :(Mod,Head),Body ) .

retract_in_module(Mod,Head   )  :- defined_in_module(Mod, Head), retract( :(Mod,Head) ) .
retract_in_module(Mod,Head,Body):- defined_in_module(Mod, Head), retract( :(Mod,':-'(Head,Body)) ) .

retractall_in_module(Mod,Head)  :- defined_in_module(Mod, Head) -> retractall( :(Mod,Head) ) ; true.

listing_in_module(Module,Goal)  :- listing( Module:Goal ).
   
/*
 * Copy all clauses whose head unifies Arg3 from module Arg1 to 
 * module Arg2 without deleting the original clauses.
 */   
copy_module_predicate(InpMod, OutMod, Head) :-
   all( copy_module_clause(InpMod, OutMod, Head) ).
   

copy_module_clause(InpMod, OutMod, Head) :-
   clause_in_module(InpMod,Head,Body),
   assert_in_module(OutMod,Head,Body).  
   
/*
 * Move all clauses whose head unifies Arg3 from module Arg1 to 
 * module Arg2, deleting the original clauses.
 */   
move_module_predicate(FromModule, ToModule,Head) :-
   copy_module_predicate(FromModule, ToModule, Head),
   retractall_in_module(FromModule,Head).

/*
 * Replace all clauses whose old head unifies Arg2 from module Arg1  
 * and whose head unifies Arg2 .
 */       
replace_module_predicate(Module, Old, New) :-
	retract_in_module(Module, Old),
	assert_in_module(Module, New).    
	
/*
 * Tests:
 
assert_in_module(Mod,Head     , Goal) :- assert( :(Mod,Head)      ), call(Goal).
assert_in_module(Mod,Head,Body, Goal) :- assert( :(Mod,Head,Body) ), call(Goal).

% :- Dynamically created contents of user module globally visible (without module prefix): 
%       Mod = user, Head=uuu(1), Goal=uuu(X), assert_in_module(Mod,Head, Goal).

% :- Contents of other modules not visible without module prefix: 
%       Mod = mmmm, Head=uuu(2), Goal=uuu(X), assert_in_module(Mod,Head, Goal).

% :- Contents of other modules visible with explicit module prefix: 
%       Mod = mmmm, Head=uuu(3), Goal=mmmm:uuu(X), assert_in_module(Mod,Head, Goal).

% :- Dynamic creation of explicit module prefix: 
%       Mod = mmmm, Head=uuu(4), Goal=':'(mmmm,uuu(X)), assert_in_module(Mod,Head, Goal).

*/
