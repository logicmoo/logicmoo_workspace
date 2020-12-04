% Program: The Causal Calculator
% File: ccalc.pl
% Version: 2.0
% Language: SISCtus Prolog 3.7.1, SWI Prolog 5.2.3
% Date: 12/6/1997
% Last Revised: 4/7/2003

% Determine which version of Prolog is being used and load the appropriate
% include file for CCalc.  Each supported version of Prolog implements either
% the current_prolog_flag/2 or prolog_flag/2 predicate, but not both, so both
% must be checked to ensure compatibility.
/*
:- ( predicate_property(current_prolog_flag(_,_),built_in)
     -> current_prolog_flag(version,Ver)
   ; predicate_property(prolog_flag(_,_),built_in)
     -> prolog_flag(version,Ver)
   ; format("Error: CCalc must be run under SICStus Prolog 3.7.1 or~n",[]),
     format("   later, or SWI-Prolog 3.3.4 or later.~n",[]),
     close_abort ),
   name(Ver,VerName),
   % test whether version name starts with "SICStus"
   ( VerName = [0'S,0'I,0'C,0'S,0't,0'u,0's|_]
     -> ensure_loaded('sicstus.pl')
   ; ensure_loaded('swi.pl') ).
*/
%:- module(ccalc,[]).
is_sicstus:- \+ current_prolog_flag(version_data,swi(_,_,_,_)).

%?- is_sicstus -> prolog_flag(single_var_warnings,_,off) ; true.

?- is_sicstus -> ensure_loaded('sicstus.pl') ; ensure_loaded('swi.pl') .

% dynamic predicates differ from the others in that they can be modified while
%  the program is running. We use dynamic predicates to store some 
%  information in the database.

:- dynamic((
   macro1/2, macro/3, ccsorts/1, domain_schema/2, domain/2, var_sort/2, 
   (<=)/2, (<-)/2, rule_schema/1, rule_schema1/2, query_rule_schema/1, 
   query_problem/3, nmquery_problem/3 ,
   atom_integer/3, atom_integer_extension/3, saved_atom_integer/3,
%   integer_atom/2, integer_atom_extension/2, saved_integer_atom/2, 
   clause/1, clause0/1, clause1/1, query_clause/1,
   rule_body/2, query_rule_body/2, show_spec/1,
   test/1, attribute0/2, ab/1, ab_cwa_1/1, objs/2,
   consts/2, rigids/2, boolean_constants/2,
   value/2 )).  % value is not totally retracted during initialization


% <= is for storing rules
% <- is for storing query rules 

:- ccalcops:compile('precedences.std').
:- compile('./cchelp.pl').

safe_tell(FileName) :- tell(FileName), told, tell(FileName).
safe_see(FileName) :- see(FileName), seen, see(FileName).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% loadf(+File) : main routine for reading input files and grounding them. 
%%     File : either a single file name in atom, or multiple files in list 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loadf(Files) :-
   initialize,
   include_file('macros.std',0),  

   common_statistics(_),

   read_file(Files), 
   loadf_Files2, !.

loadf_Files2:- 
   instantiate_sorts,   % construct domain/2, which is a database of 
                        %  sort vs ground instances 
   ( show_spec(S) 
     -> \+ \+ valid_spec(S)
   ; true ),

   enumerate_atoms,     % construct hash table for atoms
   
   common_statistics(T1),
   T1S is T1/1000, 

   write('%'),
   value(original_atom_count,OC), format(" ~w atoms",[OC]), 
   flush_output,

   insert_inertia_rules,

   process_rule_schemas,  % generate grounded rules

   common_statistics(T2),
   T2S is T2/1000, 
   value(rule_count,RC), format(", ~w rules",[RC]), 
   flush_output,

   generate_completion_clauses(domain),   % this also involves translation of 
                                          %  multi-valued logic to 
                                          %  propositional logic

   value(clause_count,CC), format(", ~w clauses",[CC]), 
   value(aux_atom_count,AAC),
   ( AAC > 0
     -> format(" (~w new atoms)",[AAC]),
        ( ( value(mode,transition), AAC > 0 )
          -> flush_output,
	     renumber_atoms
        ; true )
   ; true ),
   common_statistics(T3),
   T3S is T3/1000,
   nl,
   ( value(timed,true)
     -> T12S is T1S + T2S,
	format("% Grounding time: ~2f seconds~n",[T12S]),
	format("% Completion time: ~2f seconds~n",[T3S]),  
	( value(verbose,true)
	  -> T is T1S + T2S + T3S,
	     format("% Total time: ~2f seconds~n",[T])
	; true )
   ; true ).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% include_file(File,IncludeDir,RuleSchema)
%% (second argument is optional)
%%
%% File : either a single file name in atom, or multiple files in list format.
%% IncludeDir: directory to include in the search path (used when one input
%%   file includes another)
%% RuleSchema: 0 => read static laws at time 0
%%             1 => read static laws at time 1 & dynamic laws
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

include_file(File,RuleSchema) :-
   include_file(File,_,RuleSchema).

include_file([],_,_) :- !.

include_file([File|Files],IncludeDir,RuleSchema) :-
   !,    
   include_file(File,IncludeDir,RuleSchema),
   include_file(Files,IncludeDir,RuleSchema).

%^ dis: should disallow a list of list.

% single file name
include_file(File,IncludeDir,RuleSchema) :- 
   \+ list(File),   % instead of atom(File), for swi compatibility on []
   !,
   seeing(OldInput),
   ( determine_full_filename(File,IncludeDir,AbsFile)
     -> true
   ; fatal_error("File \'~w\' not found.",[File]) ),
   ( (RuleSchema==1 ; 
      member(File,['static0.std']))
     -> true
   ; write('% loading file '),
     write(AbsFile), nl ), 
   flush_output,
   safe_see(AbsFile),
   read_pass(RuleSchema),
   seen, 
   see(OldInput).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% determine_full_filename(File,IncludeDir,FullName) :
%% (second argument is optional)
%%    When a file is loaded, try these directories in order:
%%       IncludeDir, which is the directory containing the file currently
%%          being read in the case of an include statement inside another file
%%       None (i.e. take File as an absolute filename)
%%       The current working directory in the Unix shell
%%       The value of the user-specified 'dir' option
%%       The directory from which CCalc was loaded
%%    If the file doesn't exist in any of these locations, return an error.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

determine_full_filename(File,FullName) :-
   determine_full_filename(File,_,FullName).

determine_full_filename(File,IncludeDir,FullName) :-
   ( nonvar(IncludeDir),
     format_to_atom(FullName,"~w~w",[IncludeDir,File])
   ; expand_filename(File,FullName)
   ; environ('PWD',PWD),
     format_to_atom(FullName,"~w/~w",[PWD,File])
   ; value(dir,Dir),
     format_to_atom(File0,"~w~w",[Dir,File]),
     expand_filename(File0,FullName)
   ; value(ccalc_dir,CCalcPath),
     format_to_atom(FullName,"~w~w",[CCalcPath,File])
   ; fatal_error("File \'~w\' not found.",[File]) ),
   common_file_exists(FullName),
   !.


% determine_path(Filename,Path) takes a Filename, and returns the Path to
% that filename, which is the whole filename up to and including the final
% slash (but not anything after that)

determine_path(Filename,Path) :-
   format_to_chars("~w",[Filename],FilenameStr),
   determine_path_recur(FilenameStr,PathStr),
   ( ground(PathStr)
     -> format_to_atom(Path,"~s",[PathStr])
   ; Path = '' ).

determine_path_recur([],[]).

determine_path_recur([47|Cs],Path) :-  % 47 is code for slash
   !,
   determine_path_recur(Cs,Path2),
   Path = [47|Path2].

determine_path_recur([C|Cs],Path) :-
   determine_path_recur(Cs,Path2),
   ( Path2 = []
     -> Path = []
   ; Path = [C|Path2] ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% read_file(+Files) : read files 
%% 
%%   The mode is set to the basic mode by default. First read 'static0.std'.
%%   This declares basic objects and an exogenieity assumption for initial
%%   states. Then read the main Files. The rules (schema) will be stored in 
%%   rule_schema/1. While reading them, as soon as history mode condition 
%%   is detected, CCalc will turn into history mode. If dynamic laws are 
%%   detected in the middle, they'll be skipped in this first reading.
%%   But after then, CCalc will turn into the transition mode and read 
%%   Files again for the dynamic laws to be stored in rule_schema1/1 for 
%%   later shifting. If additive constants are used, CCalc will process 
%%   additive files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_file(Files) :-
   include_file('static0.std',0), 
   iset(dynamic_detected,false),  % static0 contains sdFluent
   include_file(Files,0), % the mode will turn to history mode if 
                              % the files contain macros maxstep outside
                              % query 
   ( (value(dynamic_detected,true), \+ value(mode,history))
     -> iset(mode,transition), 
        filter_static1_dynamic(Files)  % this will store in rule_schema1
   ; true ),

   ( value(additive_detected,true)  % if additive fluent or action is detected
     -> process_additive
   ; true ),

   value(mode,Mode),
   write('%'), 
   format(" in ~w mode...~n",[Mode]), 
   iset(dynamic_detected,false).


%-----------------------------------------------------------------------------%
% filter_static1_dynamic(+Files) : Used in transition mode
%    read the rules only in Files and store them in
%    rule_schema1/1 for shifting
%-----------------------------------------------------------------------------%

filter_static1_dynamic(Files) :-
   V_EA = var(exogenousActionAtomicFormula,-1), 
   Term = (0: V_EA <= 0: V_EA),
   insert_rule_schema(Term,1),

   V_AA = var(attributeAtomicFormula,-1), 
   Term1 = (0: V_AA <= 0: V_AA), 
   insert_rule_schema(Term1,1),

   include_file(Files,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% read and dispatch 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_op(B) :- 
   functor(B,F,_), 
   member_check(F, [include, macros, sorts, variables, objects, constants, of,
                 tests, maxstep, compile, show, query, nmquery, op]).

read_pass(RuleSchema) :-
   read_fail_loop(RuleSchema).
read_pass(_RuleSchema).  


read_fail_loop(RuleSchema) :-
   repeat,
   read_and_expand(Term0,Term),

   ( (Term == end_of_file)
     -> !, fail

   ; (Term = (:- maxstep :: N))
     -> ( RuleSchema==0
          -> process_maxstep(N) )

   ; (Term = (:- maxAdditive :: N))
     -> ( RuleSchema==0
          -> iset(additive_detected,true),
             process_macros((maxAdditive -> N)), 
             process_include('additive') )

   ; (Term == dynamic; Term = ((dynamic) where C))
     -> iset(dynamic_detected,true)

   ; Term = (side_effect(SideEffect) where _Condition)
     -> call(SideEffect)

   ; Term = side_effect(SideEffect)
     -> call(SideEffect)  

   ; ( (Term = (((B=>H) where C)); Term = (((H<-B) where C)); 
        Term = (((H<=B) where C)); 
        Term = (((<- B) where C)), H = false; 
        Term = (((<= B) where C)), H = false ) )
     -> ( negative_nonboolean_atom(H) % rather check definiteness here
          -> fatal_error("The rule is not definite: ~w",Term0)
        ; true ),
        ( \+ ( (RuleSchema ==1), rigid_atom(H))
          -> insert_rule_schema(((H<=B) where C),RuleSchema ) )

   ; ( (Term = (B=>H); Term = (H<-B); Term = (H<=B);
        Term = (<- B), H = false; Term = (<= B), H = false) )
     -> ( negative_nonboolean_atom(H)
          -> fatal_error("The rule is not definite: ~w",Term0)
        ; true ),
        ( \+ ( (RuleSchema ==1), rigid_atom(H))
          -> insert_rule_schema((H<=B),RuleSchema ) )

   ; ( (Term = (H :- B); Term = (:- B), \+ is_op(B), H = false), 
       value(mode,basic) )
     -> ( negative_nonboolean_atom(H)
          -> fatal_error("The rule is not definite: ~w",Term0)
        ; true ),
        replace_comma_with_and(B,B1),
        insert_rule_schema((H<=B1),RuleSchema)

   ; (Term = (:- include(Arg)))     % required for sicstus 3.8
      -> process_include(Arg)

   ; (Term = (:- Command))       
     -> ( RuleSchema==0
           -> call(Command))     % other commands such as declarations of
                                 % sorts, objects, etc

   ; ( (Term = (F where C))
     -> ( (RuleSchema==1, \+ rigid_atom(Term))
          -> true
        ; insert_rule_schema(((F <= true) where C),RuleSchema) ) )

   ; ( ( (RuleSchema == 1), \+ rigid_atom(Term) )
       -> true
     ; insert_rule_schema((Term <= true), RuleSchema) ) ),
   fail.


%-----------------------------------------------------------------------------%
% negative_nonboolean_atom(+Head): 
%     Checks whether Head is a negation of nonboolean atoms, which makes
%     the rule nondefinite. -(a=1) (a \in {1,2,3}) for example.
%-----------------------------------------------------------------------------%

negative_nonboolean_atom(Head) :-
   ( Head = -((_: C eq Val)) ; Head = -(C eq Val)),
   \+ boolean_constants(C,_). 


/*
include_file0(File,IncludeDir) :- 
   \+ list(File),   % instead of atom(File), for swi compatibility on []
   !,
   seeing(OldInput),
   ( determine_full_filename(File,IncludeDir,AbsFile)
     -> true
   ; fatal_error("File \'~w\' not found.",[File]) ),
   ( (RuleSchema==1 ; 
      member(File,['static0.std']))
     -> true
   ; write('% loading file '),
     write(AbsFile), nl ), 
   flush_output,
   safe_see(AbsFile),
   read_pass(RuleSchema),
   seen, 
   see(OldInput).
*/

     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dispatch to declaration handlers 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

macros(B)     :- process_macros(B), !. 
sorts(B)      :- process_sorts(B), declare_composite_sorts(B), !.
variables(B)  :- process_variables(B), !.
objects(B)    :- process_objects(B), !.
constants(B)  :- process_constants(B), !.
include(B)    :- process_include(B), !.
show(B)       :- process_show(B), !.
tests(B)      :- process_tests2(B), !.
maxstep(B)    :- process_maxstep(B), !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initialize :-
   garbage_collect,
%   close_all_streams,

   retractall(macro(_,_,_)), 
   retractall(meta_macro(_,_,_)), 
   retractall(ccsorts(_)),
   retractall(domain_schema(_,_)),
   retractall(domain(_,_)),
   retractall(var_sort(_,_)),        

   retractall((_ <= _)),
   retractall((_ <- _)),
   retractall(rule_schema(_)),
   retractall(rule_schema1(_)),
   retractall(query_rule_schema(_)),

   retractall(query_problem(_,_,_)),
   retractall(nmquery_problem(_,_,_)),

   retractall(atom_integer(_,_)),
   retractall(atom_integer_extension(_,_)),
   retractall(saved_atom_integer(_,_)),

   retractall(clause(_)),
   retractall(clause0(_)),
   retractall(clause1(_)),
   retractall(query_clause(_)),
   assertz((clause(C) :- clause0(C))),
   assertz((clause(C) :- clause1(C))),

   retractall(rule_body(_,_)),
   retractall(query_rule_body(_,_)),

   retractall(show_spec(_)),

   retractall(test(_)),
   retractall(attribute0(_,_)),
   retractall(ab(_)), 
   retractall(ab_cwa_1(_)), 

   retractall(consts(_,_)),
   retractall(rigids(_,_)),
   retractall(boolean_constants(_,_)),
   retractall(objs(_,_)),

   assertz(ccsorts([node(atomicFormula,[])])),
   
%   process_sorts(((boolean*) >> boolean)),
%   assert_objects(true,boolean),
%   assert_objects(false,boolean),

   assertz(test(true)),
   assertz(test(false)),
   assertz(test(next(_,_))),
   assertz(test(_ = _)),
   assertz(test(_ < _)),
   assertz(test(_ > _)),
   assertz(test(_ =< _)),
   assertz(test(_ >= _)),

   assertz(test(_ == _)),
   assertz(test(_ @< _)),
   assertz(test(_ @> _)),
   assertz(test(_ @=< _)),
   assertz(test(_ @>= _)),
   assertz(test(_ is _)),
   assertz(test(_ =:= _)),
   assertz(test(_ =\= _)),

   retractall(value(atom_count,_)),
   retractall(value(rigid_count,_)),
   retractall(value(atom_count_0,_)),
   retractall(value(aux_atom_count_from_rules_0,_)),
   retractall(value(aux_atom_count_from_rules,_)),
   retractall(value(aux_atom_count_from_completion_0,_)),
   retractall(value(aux_atom_count_from_completion,_)),
   retractall(value(aux_atom_count,_)),
   retractall(value(original_atom_count,_)),
   retractall(value(extended_atom_count,_)),
   retractall(value(fluent_count,_)),
   retractall(value(action_count,_)),
   retractall(value(rule_count,_)),
   retractall(value(rule_count_0,_)),
   retractall(value(query_rule_count,_)),
   retractall(value(clause_count,_)),
   retractall(value(clause_count_0,_)),
   retractall(value(query_clause_count,_)),
   retractall(value(extended_clause_count,_)),
   retractall(value(compact_ans,_)),
   retractall(value(dynamic_detected,_)),

   iset(mode,basic),
   iset(dynamic_detected,false),
   iset(additive_detected,false),

   db_init_external,
   db_init_query_external.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% declaration handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_macros: Read :- macro section and store them in macro/3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_macros((A;B)) :-
   !, 
   process_macros(A), 
   process_macros(B).

% for compatibility
process_macros(((maxstep) -> N)) :-  
   !,
   fatal_error("In history mode, maxstep should now be specified by ~n
  :- maxstep ~w. ~n",N). 

process_macros((Left -> Right)) :-
   !,
   subst_vars_for_macro_vars((Left->Right),(T1->T2),_Vs),
   ( T1 == T2  % doesn't need to include this as a macro
     -> true
   ; assertz(macro(T1,true,T2)) ).

process_macros((Left -> Right where Call)) :-
   !,
   subst_vars_for_macro_vars((Left->Right where Call),
                             (T1->T2 where C),_Vs),
   ( T1 == T2
     -> true
   ; assertz(macro(T1,C,T2)) ).

process_macros(Macro) :-
   fatal_error("Invalid macro declaration (~w).",[Macro]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subst_vars_for_macro_vars(C,D,Vs) : substitute prolog 
%    variables (e.g., _29599) for macro variables (e.g., #1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subst_vars_for_macro_vars(#(N),V,Vs) :-
   member(=(N,V),Vs), !.
subst_vars_for_macro_vars(C,D,Vs) :-
   functor(C,F,N),
   functor(D,F,N),
   subst_vars_for_macro_vars_arg(C,0,N,D,Vs).

subst_vars_for_macro_vars_arg(_C,N,N,_D,_Vs) :- !.
subst_vars_for_macro_vars_arg(C,M,N,D,Vs) :-
   M1 is M+1,
   arg(M1,C,A),
   subst_vars_for_macro_vars(A,B,Vs),
   arg(M1,D,B),
   subst_vars_for_macro_vars_arg(C,M1,N,D,Vs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subst_free
% substitute free variables in Term into Prolog variable. 
% Doesn't do anything with bound variables.
% substitution is done with respect to substitution list Sub.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subst_free(Term,Term,_Sub) :-
   var(Term),
   !.
subst_free(Term,NewTerm,Sub) :-
   atom(Term),
   !,
   ( member(=(Term,NewTerm),Sub)
     -> true
   ; NewTerm = Term ).
subst_free([/\X|A],[/\X|A1],Sub) :-
   !,
   ( common_select(=(X,_),Sub,Sub1) 
     -> true
   ; Sub1 = Sub ),
   subst_free(A,A1,Sub1).
subst_free([\/X|A],[\/X|A1],Sub) :-
   !,
   ( common_select(=(X,_),Sub,Sub1) 
     -> true
   ; Sub1 = Sub ),
   subst_free(A,A1,Sub1).
subst_free(Term,NewTerm,Sub) :-
   functor(Term,F,N),
   functor(NewTerm,F,N),
   subst_free_arg(Term,0,N,NewTerm,Sub).

subst_free_arg(_Term,N,N,_NewTerm,_Sub) :-
   !.
subst_free_arg(Term,M,N,NewTerm,Sub) :-
   M1 is M+1,
   arg(M1,Term,A),
   subst_free(A,B,Sub),
   arg(M1,NewTerm,B),
   subst_free_arg(Term,M1,N,NewTerm,Sub).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subst_functor(F,G,T,NT) : substitute functor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subst_functor(F,G,T,NT) :-
   functor(T,F,N), !,
   functor(NT,G,N),
   subst_functor_arg(F,G,T,0,N,NT).

subst_functor(F,G,T,NT) :-
   functor(T,F1,N), F1 \== F, !,
   functor(NT,F1,N),
   subst_functor_arg(F,G,T,0,N,NT).

subst_functor_arg(_F,_G,_T,N,N,_NT) :-  !.

subst_functor_arg(F,G,T,M,N,NT) :-
   M1 is M+1,
   arg(M1,T,A),
   subst_functor(F,G,A,B),
   arg(M1,NT,B),
   subst_functor_arg(F,G,T,M1,N,NT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% type checking procedures : may be used in where condition 
%%  in macro expansion 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------%
% composite_sort(+S) : check whether sort name S is a composite sort
%-----------------------------------------------------------------------------%

complex_sort(S) :-  
   functor(S,_,1), !. 

%-----------------------------------------------------------------------------%
% composite_sort(+S) : check whether sort name S is a composite sort
%-----------------------------------------------------------------------------%

composite_sort(simpleFluent(_)).
composite_sort(inertialFluent(_)).
composite_sort(additiveFluent(_)). 

composite_sort(sdFluent(_)).
composite_sort(rigid(_)).

composite_sort(action(_)).
composite_sort(exogenousAction(_)).
composite_sort(attribute(_)).
composite_sort(additiveAction(_)). 
composite_sort(abAction(_)). 


%-----------------------------------------------------------------------------%
% constant(+C) : check whether C is a constant
%-----------------------------------------------------------------------------%

% macro
is_constant(var(Sort,_)) :- 
   composite_sort(Sort), !. 

% variable
is_constant(C) :-  
   var_sort(C,Sort), composite_sort(Sort), !.

% name
% To speed up, we only check the name of functor and arity, but 
%  don't check if the arguments belong to appropriate sorts. 
%  This doesn't allow distinguishing two constants with the same 
%  functor and arity, but it doesn't seem to be much problem
is_constant(C) :- 
   !,
   functor(C,F,N),
   consts(F,N).   % refers to constants database

%-----------------------------------------------------------------------------%
% object(+C) : check whether C is an object
%-----------------------------------------------------------------------------%

object(O) :-
   functor(O,F,N), 
   objs(F,N).

%-----------------------------------------------------------------------------%
% expr(+C) : 
%    check whether C is an expression that wil
%    It's also used to substitute values for constants where constants
%    are used in place of values. (e.g., C1+C2 )
%-----------------------------------------------------------------------------%

expr(-(Expr)) :- expr(Expr), !.

expr(Expr) :-
  functor(Expr,F,2),      % binary operator
  !,
  member_check(F,['+','-','*','/','//','rem','mod','gcd','min','max','**',
            '>','<','=<','>=','=']).

expr(Expr) :-          
   functor(Expr,F,1),     % unary operator
   !,
   member_check(F,['abs']).


%-----------------------------------------------------------------------------%
% rigid(+R) : check whether R is a rigid constan
%-----------------------------------------------------------------------------%

% macro
rigid(var(rigid(_S),_)) :- !.  

% variable
rigid(R) :- % variable  %^jo- problem?
   var_sort(R, rigid(_)), !.

% name - similar to constant
rigid(R) :-
   !,
   functor(R,F,N),
   rigids(F,N).


%-----------------------------------------------------------------------------%
% rigid_atom(+R) : check whether R is a rigid atom. 
%                   used to skip putting the rule with a rigid atom in the 
%                   head in rule_schema1 in transition mode
%-----------------------------------------------------------------------------%

rigid_atom(R) :-   % boolean
   rigid(R), !.
rigid_atom(-R) :-  % boolean
   rigid(R), !.    
rigid_atom(R eq _) :-
   rigid(R), !.
rigid_atom(R = _) :-
   rigid(R), !.


%-----------------------------------------------------------------------------%
% composite_var(+V) : check whether V is a variable for composite sor
%-----------------------------------------------------------------------------%

composite_var(V) :-
   var_sort(V,Sort),
   composite_sort(Sort), !.


%-----------------------------------------------------------------------------%
% is_var(+V) : check whether V is a variable
%-----------------------------------------------------------------------------%

is_var(V) :-
   var_sort(V,_), !.

is_atom((_:V)) :-
  is_var(V).
is_atom(V) :-
  is_var(V).
is_atom(C=_) :- 
  is_constant(C).
is_atom(C) :-
  boolean_constant(C).
is_atom(-C) :-
  boolean_constant(C).
is_atom(A=B) :-
  ( evaluable_expr(A) ; evaluable_expr(B) ). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------%
% simpleFluent_constant(+C) : 
%    Checks whether C is a simple fluent constant.
%-----------------------------------------------------------------------------%

simpleFluent_constant(var(simpleFluent(_),_)) :- !. 
simpleFluent_constant(var(inertialFluent(_),_)) :- !.

simpleFluent_constant(C) :-
   ( var_sort(C,simpleFluent(_));
     var_sort(C,inertialFluent(_)) ), !.

simpleFluent_constant(C) :-
   functor(C,F,N),
   ( domain_schema(simpleFluent(_),Ls);
     domain_schema(inertialFluent(_),Ls) ),
   member(L,Ls), 
   functor(L,F,N).   


%-----------------------------------------------------------------------------%
% sdFluent_constant(+C) : 
%    Checks whether C is a statically determined fluent constant.
%-----------------------------------------------------------------------------%

sdFluent_constant(var(sdFluent(_),_)) :- !. 
sdFluent_constant(var(rigid(_),_)) :- !.

sdFluent_constant(C) :-
   ( var_sort(C,sdFluent(_));
     var_sort(C,rigid(_)) ), !.

sdFluent_constant(C) :-
   functor(C,F,N),
   ( domain_schema(sdFluent(_),Ls);
     domain_schema(rigid(_),Ls) ),
   member(L,Ls), 
   functor(L,F,N).


%-----------------------------------------------------------------------------%
% fluent_constant(+C) : 
%    Checks whether C is a statically determined fluent constant.
%-----------------------------------------------------------------------------%

fluent_constant(C) :-
   ( simpleFluent_constant(C); sdFluent_constant(C) ). 


%-----------------------------------------------------------------------------%
% action constants
%-----------------------------------------------------------------------------%

action_constant(var(action(_),_)) :- !.
action_constant(var(abAction(_),_)) :- !.
action_constant(var(exogenousAction(_),_)) :- !.
action_constant(var(attribute(_),_)) :- !.
action_constant(var(additiveAction(_),_)) :- !.

action_constant(C) :-
   ( var_sort(C,action(_));
     var_sort(C,abAction(_));
     var_sort(C,exogenousAction(_));
     var_sort(C,attribute(_));
     var_sort(C,additiveAction(_)) ), !.

action_constant(C) :-
   functor(C,F,N),
   functor(L,F,N),
   ( domain_schema(action(_),Ls);
     domain_schema(abAction(_),Ls);
     domain_schema(exogenousAction(_),Ls);
     domain_schema(attribute(_),Ls);
     domain_schema(additiveAction(_),Ls) ),
   member(L,Ls).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% atoms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simpleFluent_atom(C=_) :-
  simpleFluent_constant(C). 
simpleFluent_atom(C) :-
  simpleFluent_constant(C). 
simpleFluent_atom(-C) :-
  simpleFluent_constant(C). 

sdFluent_atom(C=_) :-
  sdFluent_constant(C).
sdFluent_atom(C) :-
  sdFluent_constant(C).
sdFluent_atom(-C) :-
  sdFluent_constant(C).

fluent_atom(C=_) :-
  (simpleFluent_constant(C) ; sdFluent_constant(C)). 
fluent_atom(C) :-
  (simpleFluent_constant(C) ; sdFluent_constant(C)). 
fluent_atom(-C) :-
  (simpleFluent_constant(C) ; sdFluent_constant(C)). 

action_atom(C=_) :-
  action_constant(C).
action_atom(C) :-
  action_constant(C).
action_atom(-C) :-
  action_constant(C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% formulas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------%
% simpleFluent_formula
%-----------------------------------------------------------------------------%

simpleFluent_formula(F) :- 
   simpleFluent_atom(F). 
simpleFluent_formula(F) :-
   ( action_atom(F); 
     sdFluent_atom(F) ),
   !, fail.
simpleFluent_formula(F) :-
   functor(F,_,N), 
   simpleFluent_formula_arg(F,1,N).
simpleFluent_formula_arg(_F,M,N) :-
   M > N,
   !.
simpleFluent_formula_arg(F,M,N) :-
   M1 is M+1,
   simpleFluent_formula_arg(F,M1,N).


%-----------------------------------------------------------------------------%
% fluent_formula
%-----------------------------------------------------------------------------%

fluent_formula(F) :- 
   fluent_atom(F). 
fluent_formula(F) :-
   action_atom(F), 
   !, fail.
fluent_formula(F) :-
   functor(F,_,N), 
   fluent_formula_arg(F,1,N).
fluent_formula_arg(_F,M,N) :-
   M > N,
   !.
fluent_formula_arg(F,M,N) :-
   arg(M,F,A),
   fluent_formula(A),
   M1 is M+1,
   fluent_formula_arg(F,M1,N).


%-----------------------------------------------------------------------------%
% action formula
%-----------------------------------------------------------------------------%

action_formula(F) :-
   action_formula_1(F) , \+ fluent_formula(F).

action_formula_1(F) :- 
   action_atom(F). 
action_formula_1(F) :-
   fluent_atom(F), 
   !, fail.
action_formula_1(F) :-
   functor(F,_,N), 
   action_formula_1_arg(F,1,N).
action_formula_1_arg(_F,M,N) :-
   M > N,
   !.
action_formula_1_arg(F,M,N) :-
   arg(M,F,A), 
   action_formula_1(A),
   M1 is M+1,
   action_formula_1_arg(F,M1,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% has_no_constants(Formula) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_no_constants((_A@<_B)) :- !.
has_no_constants((_A==_B)) :- !.
has_no_constants(abby(_)) :- !.

has_no_constants(Formula) :-
   is_constant(Formula), !, fail.
has_no_constants(Formula) :-
   functor(Formula,_,N),
   has_no_constants_arg(Formula,1,N).

has_no_constants_arg(_Formula,M,N) :-
   M > N,
   !.
has_no_constants_arg(Formula,M,N) :-
   arg(M,Formula,A),
   has_no_constants(A), 
   M1 is M+1, 
   has_no_constants_arg(Formula,M1,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_sorts((A;B)) :-
   !, 
   process_sorts(A), 
   process_sorts(B).

process_sorts((S>>(S1>>S2))) :-
   !, 
   process_sorts((S>>S1)),
   process_sorts((S1>>S2)).

process_sorts((S>>(S1;S2))) :-
   !, 
   process_sorts((S>>S1)),
   process_sorts((S>>S2)).

process_sorts((S>>S1)) :-
   !,
   ( find_sort(S,_As)
     -> ( find_sort(S1,_Bs)
          -> ( true % compatible([S|As],[S|Bs]) 
                  %^jo- multiple inheritance without common supersort 
                  %     is now allowed. - potential error?
               -> add_subsort(S,S1)
               ; fatal_error("Sorts ~q and ~q must have a common supersort.",
                             [S,S1]) )
        ; add_subsort(S,S1) )
   ; process_sorts(S),
     process_sorts((S>>S1)) ).

process_sorts(S) :-  % skip if already declared
   ( atom(S) ; composite_sort(S) ; S = _* ),
   !, 
   ( is_sort(S) 
     -> true
   ; ( find_sort(S,As)
        -> ( As = []
              -> format("~nRedundant sort declaration: ~w.",[S])
            ; fatal_error("Invalid sort1 declaration (~w).",[S]) )
     ; get_ccsorts(Nodes),
       append(Nodes,[node(S,[])],NewNodes),
       put_ccsorts(NewNodes) ) ).

process_sorts(Spec) :-
   fatal_error("Invalid sort declaration (~w).",[Spec]).

%^jo common ancestor check. Why was it necessary?
compatible(As,Bs) :-
	append(_,[A],As), append(_,[A],Bs).

get_ccsorts(Nodes) :- ccsorts(Nodes).

put_ccsorts(Nodes) :-
   retractall(ccsorts(_)), assertz(ccsorts(Nodes)).

% check whether Sort is predeclared as a sort

is_sort(Sort) :-
  ( Sort = atomicFormula
    -> true 
  ; find_sort(Sort,_) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_sort(+Sort,?Ancestors) Returns the list of ancestors of Sort if found.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_sort(Sort,As) :-
   get_ccsorts(Nodes),  
   find_sort(Nodes,Sort,[],As).

find_sort([node(S,Subs)|Nodes],Sort,As1,As) :-
   ( S = Sort, As = As1
   ; find_sort(Subs,Sort,[S|As1],As) % recursively find
   ; find_sort(Nodes,Sort,As1,As) ).

%descendant_sort(Child,Child).

descendant_sort(Child,Ancestor) :-
   find_sort(Child,Ancestor_List), member_check(Ancestor, Ancestor_List).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add_subsort(+Sort,+SubSort) : add subsort SubSort to sort Sort
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_subsort(Sort,Sub) :-
   get_ccsorts(Nodes),
   add_subsort(Nodes,Sort,Sub,[],NewNodes),
   put_ccsorts(NewNodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add_subsort(OldNodes,Sort,Sub,Ancestors,NewNodes1) :
%%   replace OldNodes by NewNodes1 whose Sort node includes Sub in its subsort list
%    Ancestors used to keep record to check cycle in sorts declaration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_subsort([],_Sort,_Sub,_Ancestors,[]).
add_subsort([node(S,Subs)|Nodes],Sort,Sub,Ancestors,NewNodes1) :-
   add_subsort(Subs,Sort,Sub,[S|Ancestors],NewSubs),
   add_subsort(Nodes,Sort,Sub,Ancestors,NewNodes),
   ( S == Sort      % if Sort is the right super sort to add the subsort Sub
     -> ( member(Sub,[S|Ancestors])
          -> report_cycle([S|Ancestors],Sub)
        ; ( member(node(Sub,_),NewSubs)  % if the subsort is already 
                                         % in the subsort list of the node
            -> NewSubs1 = NewSubs        % no change in the subsort list
            ;  ( find_sort(Sub,_), find_nodes([Sub],SubNodes)
                 -> append(NewSubs,SubNodes,NewSubs1)
                 ;  append(NewSubs,[node(Sub,[])],NewSubs1) ) ), 
                    % otherwise add the subsort Sub
                    % to the subsort list

   	  NewNodes1 = [node(S,NewSubs1)|NewNodes] )  % construct new node which include
                                                     % the new subsort
   ; NewNodes1 = [node(S,NewSubs)|NewNodes] ).

report_cycle(Ancestors,Sub) :-
	format("~nCycle in sorts: ",[]),
	reverse(Ancestors,As),
	( member(A,As), 
	  format("~w >> ",   [A]), fail; 
	format("~w.~n",[Sub]) ),
	close_abort.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% declare_composite_sorts : declare composite sorts for each sort 
%%             introduced. processed just after process_sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declare_composite_sorts((A;B)) :-
   !,
   declare_composite_sorts(A), 
   declare_composite_sorts(B).

declare_composite_sorts((S>>(S1>>S2))) :-
   !, 
   declare_composite_sorts((S>>S1)),
   declare_composite_sorts((S1>>S2)).

declare_composite_sorts((S>>(S1;S2))) :-
   !, 
   declare_composite_sorts((S>>S1)),
   declare_composite_sorts((S>>S2)).

declare_composite_sorts((S>>S1)) :-
   !, 
   declare_composite_sorts(S),
   declare_composite_sorts(S1).

declare_composite_sorts(S) :-
   !,
   ( skip_declare_composite_sorts(S)
     -> true
   ; 
     process_sorts(((S*) >> S)),

     process_sorts((simpleFluent(S) >> inertialFluent(S))),
     process_sorts(sdFluent(S)),
     process_sorts((action(S) >> exogenousAction(S))),
     process_sorts(attribute(S)),
     process_sorts(abAction(S)),
     process_sorts(rigid(S)),

     process_sorts((simpleFluent(S*) >> inertialFluent(S*))),
     process_sorts(sdFluent(S*)),
     process_sorts((action(S*) >> exogenousAction(S*))),
     process_sorts(attribute(S*)),
     process_sorts(rigid(S*)),

     assert_objects((simpleFluent(S) eq S),   simpleFluentAtomicFormula),
     assert_objects((sdFluent(S) eq S), sdFluentAtomicFormula),

     assert_objects((action(S) eq S),   actionAtomicFormula),
     assert_objects((exogenousAction(S) eq S), exogenousActionAtomicFormula),
     assert_objects((attribute(S) eq S*), attributeAtomicFormula),
     assert_objects((abAction(S) eq S),   abActionAtomicFormula),
     assert_objects((rigid(S) eq S),    rigidAtomicFormula),

     assert_objects((simpleFluent(S*) eq S*),   simpleFluentAtomicFormula),
     assert_objects((sdFluent(S*) eq S*), sdFluentAtomicFormula),
     assert_objects((action(S*) eq S*),   actionAtomicFormula),
     assert_objects((exogenousAction(S*) eq S*), exogenousActionAtomicFormula),
     assert_objects((rigid(S*) eq S*),    rigidAtomicFormula),

%^ what if attribute(S*)? - no there is no such
     assert_objects(none,S*) ).


skip_declare_composite_sorts(S) :-
   ( composite_sort(S) ; S = _* ; 
      member_check(S,[step, astep,
                simpleFluentAtomicFormula,inertialFluentAtomicFormula,
                sdFluentAtomicFormula, 
                actionAtomicFormula, 
                abActionAtomicFormula, 
                exogenousActionAtomicFormula, 
                attributeAtomicFormula, 
                rigidAtomicFormula, 
                atomicFormula]) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Note: is_sort(S) is to check that sort declarations
% precede var declarations in included problem files.
assert_variables((A,B),S) :-
   !, 
   assert_variables(A,S), 
   assert_variables(B,S).
assert_variables(V,S) :-
   atom(V), 
   ( S == computed
     -> true
   ; is_sort(S) ),
   !,                          
   ( var_sort(V,S1)   % if V is already declared as a variable of S1
     -> ( S = S1
          -> true
        ; fatal_error("Variable (~w) has multiple declarations.",[V]) )
   ; assertz(var_sort(V,S)) ).
assert_variables(V,S) :-
   fatal_error("Invalid variable declaration (~w).",[(V::S)]).

process_variables((A;B)) :-
   !, 
   process_variables(A), 
   process_variables(B).

process_variables((Vs::M..N)) :-
   !,
   eval_expr(M,M1), 
   eval_expr(N,N1),
   generate_range_sort(M1,N1,S1),
   assert_variables(Vs,S1).

process_variables((Vs::(S+none))) :-
   is_sort(S),
   !, 
   assert_variables(Vs,(S*)).

process_variables((Vs::S)) :-
   is_sort(S),
   !,
   assert_variables(Vs,S).

process_variables((Vs::S)) :-
   member(S,[simpleFluent,inertialFluent,sdFluent,action,exogenousAction,
             attribute,rigid]),
   !,
   S1 =.. [S, boolean],
   assert_variables(Vs,S1).

process_variables(Spec) :-
   fatal_error("Invalid variable declaration (~w).",[Spec]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_objects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assert_objects((A,B),S) :-
   !, 
   assert_objects(A,S), 
   assert_objects(B,S).

assert_objects(M..N,S) :-        % a hack to allow convenient time specs
   !,                         
   expand_sequence(M,N,Ns),
   ( retract(domain_schema(S,Cs))
     -> append(Cs,Ns,NewCs), 
        assertz(domain_schema(S,NewCs))
   ; assertz(domain_schema(S,Ns)) ).

assert_objects(C,S) :-
   functor(C,F,N),
   ( objs(F,N)
     -> true
   ; assertz(objs(F,N)) ),
 
   ( (domain_schema(S,Cs), member(C,Cs))  % if C is already in domain_schema
      -> true                             % just skip
   ; ( retract(domain_schema(S,Cs))       % otherwise insert C
       -> append(Cs,[C],NewCs),           
          assertz(domain_schema(S,NewCs))
     ; assertz(domain_schema(S,[C])))).

process_objects((A;B)) :-
   !, 
   process_objects(A), 
   process_objects(B).

process_objects((Cs::S)) :-
   is_sort(S), %^%^ (S \== boolean),
   !,
   assert_objects(Cs,S).

process_objects((Cs::(S+none))) :-
   atom(S),
   is_sort(S),
   !, 
%   introduce_star_sort(S),
   assert_objects(Cs,(S*)).

process_objects(Spec) :-
   fatal_error("Invalid object declaration (~w).",[Spec]).

introduce_star_sort(S) :-
   process_sorts((simpleFluent(S*) >> inertialFluent(S*))),
   process_sorts(sdFluent(S*)),
   process_sorts((action(S*) >> exogenousAction(S*))),
   process_sorts(rigid(S*)),

   assert_objects((simpleFluent(S*) eq S*),   simpleFluentAtomicFormula),
   assert_objects((sdFluent(S*) eq S*), sdFluentAtomicFormula), 
   assert_objects((action(S*) eq S*),   actionAtomicFormula),
   assert_objects((exogenousAction(S*) eq S*), exogenousActionAtomicFormula),
   assert_objects((rigid(S*) eq S*),    rigidAtomicFormula).

% generate_range_sort(+M,+N,-RangeSort) 
generate_range_sort(M,N,RangeSortM_N) :-  
   expand_sequence(M,N,Ns), 
   name(rangeSort,ListRangeSort),
   name(M,ListM),
   name(N,ListN),
   name('_',ListUL),
   append(ListRangeSort,ListM,ListRangeSortM),
   append(ListRangeSortM,ListUL,ListRangeSortM_),
   append(ListRangeSortM_,ListN,ListRangeSortM_N),
   atom_chars(RangeSortM_N,ListRangeSortM_N),
   sorts((integer >> RangeSortM_N)),  % integer >> RangeSort? 
   list_to_tuple(Ns,T),
   process_objects((T :: RangeSortM_N)).

check_domain_for_additive(S,M,N) :-
   ( (M =< 0, 0 =< N)
     -> true
   ; atom_chars(S,S1),
     ( prefix("rangeSort",S1)
       -> fatal_error("The domain of additiveFluent(~w..~w) should include the neutral element 0",[M,N])
     ; fatal_error("The domain of additiveFluent(~w) should include the neutral element 0",S) ) ). 


%-----------------------------------------------------------------------------%
% process additive
%-----------------------------------------------------------------------------%

% assert rule schemas for every additive fluent and action
process_additive :-
   domain_schema(additiveFluent(S),_), 
   domain_schema(S,[M|Ns]), 
   common_last([M|Ns],N), 
   check_domain_for_additive(S,M,N),
   insert_additiveFluent_rules(additiveFluent(S),M,N), % M and N is the range
   fail.
process_additive :- 
   domain_schema(additiveAction(S),_), 
   domain_schema(S,[M|Ns]), 
   common_last([M|Ns],N), 
   check_domain_for_additive(S,M,N),
   insert_additiveAction_rules(additiveAction(S),M,N), % M and N is the range
   fail.
process_additive.


%-----------------------------------------------------------------------------%
% additive fluents and actions
%-----------------------------------------------------------------------------%
  
additive_fluent(AF) :- 
   functor(AF,F,N), 
   functor(AF1,F,N), 
   domain_schema(additiveFluent(_),AFs), 
   member_check(AF1,AFs). 

additive_action(AF) :-
   functor(AF,F,N), 
   functor(AF1,F,N), 
   domain_schema(additiveAction(_),AFs), 
   member_check(AF1,AFs). 

% check duplicates

process_static_abLabel(Ab) :-
   Ab =.. [F|VArgs],
   vars_to_sorts(VArgs,SArgs), 
   Ab2 =.. [F|SArgs],
   process_constants((Ab2 :: sdFluent)),
   register_ab(Ab).

process_dynamic_abLabel(Ab) :-
   Ab =.. [F|VArgs],
   vars_to_sorts(VArgs,SArgs), 
   Ab2 =.. [F|SArgs],
   process_constants((Ab2 :: abAction(boolean))),
   register_ab(Ab).

register_ab(Ab) :-
   assertz(ab(Ab)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------%
% register_constants(C): construct consts/2 to speed up deciding
%                         if something is a constant
%-----------------------------------------------------------------------------%
register_constants((A,B)) :-
   !, 
   register_constants(A),
   register_constants(B).

register_constants(C) :-
   functor(C,F,N),
   assertz(consts(F,N)).

%-----------------------------------------------------------------------------%
% register_rigids(C): construct rigids/2 to speed up deciding
%                         if something is a rigid constant
%-----------------------------------------------------------------------------%
register_rigids((A,B)) :-
   !, 
   register_rigids(A),
   register_rigids(B).

register_rigids(C) :-
   functor(C,F,N),
   assertz(rigids(F,N)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% register_boolean_constants(C): construct boolean_constants/2 
%  to speed up deciding if something is a boolean constant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_boolean_constants((A,B)) :-
   !, 
   register_boolean_constants(A),
   register_boolean_constants(B).

register_boolean_constants(C) :-
   functor(C,F,N),
   assertz(boolean_constants(F,N)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_constant : process constant declaration section
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_constants((A;B)) :-
   !,
   process_constants(A), 
   process_constants(B).

process_constants((Cs::attribute(S*) of Action)) :-
   fatal_error("Invalid constant declaration: ~w. Use ~w instead of ~w.",[(Cs::attribute(S*) of Action), S, S*]).

process_constants((Cs::attribute(S) of Action)) :-
%  (S \= _*), 
%   introduce_star_sort(S),
   ( S = (M..N)
     -> eval_expr(M,M1),
        eval_expr(N,N1),
        generate_range_sort(M1,N1,S1)
   ; S1 = S ),
   is_sort(attribute(S1)),
   !, 
   iset(dynamic_detected,true), 
   ( S1 == boolean
     -> register_boolean_constants(Cs)
   ; true ),
   register_constants(Cs),
   assert_objects(Cs,attribute(S1)), 
   insert_attribute_rules(Cs,Action). 

process_constants((Cs::additiveFluent(S))) :-
   !,
   iset(dynamic_detected,true),
   iset(additive_detected,true), 
   ( S = (M..N) 
     -> eval_expr(M,M1),
        eval_expr(N,N1),
        generate_range_sort(M1,N1,S1)
   ; S1 = S ),
   process_sorts((simpleFluent(S1) >> additiveFluent(S1))),
   register_constants(Cs),
   assert_objects(Cs,additiveFluent(S1)),
   process_constants(  
        ( contribution(action(boolean),additiveFluent(S1)), 
          accumulatedContribution(action(boolean),additiveFluent(S1))
                                  ::    action(additiveInteger) ) ).

process_constants((Cs::additiveAction(S))) :-
   !,
   iset(dynamic_detected,true),
   iset(additive_detected,true),
   ( S = (M..N) 
     -> eval_expr(M,M1),
        eval_expr(N,N1),
        generate_range_sort(M1,N1,S1)
   ; S1 = S ),
   process_sorts((action(S1) >> additiveAction(S1))),
   register_constants(Cs),
   assert_objects(Cs,additiveAction(S1)),
   process_constants(  
        ( contribution(action(boolean),additiveAction(S1)), 
          accumulatedContribution(action(boolean),additiveAction(S1))
                                  ::    action(additiveInteger) ) ).

process_constants((Cs::M..N)) :-
   !,
   eval_expr(M,M1), 
   eval_expr(N,N1),
   generate_range_sort(M1,N1,S1),
   
   register_constants(Cs),
   register_rigids(Cs),
   assert_objects(Cs,rigid(S1)).

process_constants((Cs::((M..N)+none))) :-
   !,
   eval_expr(M,M1), 
   eval_expr(N,N1),
   generate_range_sort(M1,N1,S1),
   
   register_constants(Cs),
   register_rigids(Cs),
   assert_objects(Cs,rigid(S1*)).

process_constants((Cs::(S+none))) :-
   atom(S),
   is_sort(S),
   !, 
   ( S == boolean
     -> register_boolean_constants(Cs)
   ; true),
   register_constants(Cs),
   register_rigids(Cs),
   assert_objects(Cs,rigid(S*)). 

process_constants((Cs::S)) :-
   atom(S), 
   is_sort(S),
   !, 
   ( S == boolean
     -> register_boolean_constants(Cs)
   ; true),
   register_constants(Cs),
   register_rigids(Cs),
   assert_objects(Cs,rigid(S)). 

% composite sort except rigid constants
process_constants((Cs::CompositeSort)) :-
   ( CompositeSort =.. [rigid,S0]
     -> fatal_error("Invalid constant declaration (~w): Use ~w instead of
~w",[(Cs::CompositeSort),S0,CompositeSort])
   ; true ),
   composite_sort(CompositeSort), 
   !,
   iset(dynamic_detected,true),
   ( CompositeSort =.. [F,(M..N)]
     -> eval_expr(M,M1),
        eval_expr(N,N1), 
        generate_range_sort(M1,N1,RangeSort), 
        CompositeSort1 =.. [F,RangeSort]
   ; CompositeSort =.. [F,(S+none)]
     -> CompositeSort1 =.. [F,S*]
   ; CompositeSort1 = CompositeSort ),
   ( arg(1,CompositeSort,boolean)
     -> register_boolean_constants(Cs)
   ; true ),
   register_constants(Cs),
   assert_objects(Cs,CompositeSort1). 

process_constants((Cs::S)) :-
   (S = simpleFluent; S = inertialFluent),
   !,
   iset(dynamic_detected,true), 
   S1 =.. [S, boolean],
   process_constants((Cs::S1)). 

process_constants((Cs::sdFluent)) :-
   !,
   iset(dynamic_detected,true), 
   process_constants((Cs::sdFluent(boolean))).

process_constants((Cs::action)) :-
   !,
   iset(dynamic_detected,true), 
   process_constants((Cs::action(boolean))).

process_constants((Cs::exogenousAction)) :-
   !,
   iset(dynamic_detected,true), 
   process_constants((Cs::exogenousAction(boolean))).

process_constants((Cs::attribute of Action)) :-
   process_constants((Cs::attribute(boolean) of Action)). 

process_constants(Spec) :-
   fatal_error("Invalid constant declaration (~w).",[Spec]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------%
%  sorts_to_vars(+Sorts,-Vars) : Transform a list of sort names to a list of
%      corresponding distinct variables.
%      tail recursion
%-----------------------------------------------------------------------------%

sorts_to_vars(Sorts,Vars) :- 
   length(Sorts,N), 
   sorts_to_vars(Sorts,[],N,Vars).

sorts_to_vars([Sort|Sorts],Acc,N,Vars) :-
   get_var(Sort,N,Var), 
   N1 is N-1, 
   append(Acc,[Var],Acc1), 
   sorts_to_vars(Sorts,Acc1,N1,Vars).

sorts_to_vars([],Vars,_,Vars).   


%-----------------------------------------------------------------------------%
%  vars_to_sorts(+Vars,-Sorts) : Transform a list of variables to a list of
%      corresponding sort names in a tail recursive way
%-----------------------------------------------------------------------------%

vars_to_sorts(Vars,Sorts) :- 
   vars_to_sorts(Vars,[],Sorts).

vars_to_sorts([Var|Vars],Acc,Sorts) :-
   var_sort(Var,Sort),
   append(Acc,[Sort],Acc1), 
   vars_to_sorts(Vars,Acc1,Sorts).

vars_to_sorts([],Sorts,Sorts).   


%-----------------------------------------------------------------------------%
%  tuples_to_conjs(+Tuples,-Conjs) : Transform a list of Tuple to a list of
%      corresponding conjunction in a tail recursive way
%-----------------------------------------------------------------------------%

tuples_to_conjs(Tuples,Conjs) :- 
   tuples_to_conjs(Tuples,[],Conjs).

tuples_to_conjs([T|Ts],Acc,Conjs) :-
   conjoin_tuple(T,C), 
   append(Acc,[C],Acc1), 
   tuples_to_conjs(Ts,Acc1,Conjs).

tuples_to_conjs([],Conjs,Conjs).   


%-----------------------------------------------------------------------------%
%  consts_to_sorts(+Consts,-Sorts) : Transform a list of Constants to a list of
%      corresponding sort names in a tail recursive way
%-----------------------------------------------------------------------------%

% const_to_sort(C,S) : convert const into sort . inefficient

const_to_sort(C,S) :- domain(S,Cs), member(C,Cs).

%-------

consts_to_sorts([Const|Consts],[Sort|Sorts]) :-
   const_to_sort(Const,Sort), 
   consts_to_sorts(Consts,Sorts).
consts_to_sorts([],[]).


/*  Can't use tail recursion in find_all used in get_all_attributes
consts_to_sorts(Consts,Sorts) :- 
   consts_to_sorts1(Consts,[],Sorts).

consts_to_sorts1([C|Cs],Acc,Sorts) :-
   const_to_sort(C,S), !,
%   domain(S,Ds), member(C,Ds),  % inefficient?
   append(Acc,[S],Acc1), 
   consts_to_sorts1(Cs,Acc1,Sorts).

consts_to_sorts1([],Sorts,Sorts).   
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------%
% expand_sequence(+M,+N,?Ns) : 
%    used for expanding a range of integers into a list of integers
%    e.g. | ?- expand_sequence(1,5,Ns).
%         Ns = [1,2,3,4,5] ? ;
%-----------------------------------------------------------------------------%

expand_sequence(M0,N0,Ns) :-
   M is M0, N is N0,
   ( integer(M), integer(N)
      -> ( M =< N
           -> expand_sequence_aux(M,N,Ns)
         ; Ns = [] )
   ; fatal_error("Invalid sequence (~w)",[M..N]) ).


expand_sequence_aux(M,N,Ns) :-
   expand_sequence_aux(M,N,[],Ns). 

expand_sequence_aux(M,N,Acc,Ns) :-
   M<N,!,
   append(Acc,[M],Acc1), 
   M1 is M+1,
   expand_sequence_aux(M1,N,Acc1,Ns). 
expand_sequence_aux(N,N,Acc,Ns) :-
   append(Acc,[N],Ns).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_include
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_include((A;B)) :-
   !,
   process_include(A), 
   process_include(B).

/*
process_include((A:[(S->S1)|Ls])) :-
   !,
%   ( determine_full_filename(A,_IncludeDir,AbsFile)
%     -> true
%   ; fatal_error("File \'~w\' not found.",[A]) ),
%   format_to_atom(RenCmd,"mv ~w ~w", [A,tmp]),
%   system(RenCmd),
   format_to_atom(ReplaceCmd, "sed 's/~w/~w/g' ~w > ~w.~w",[S,S1,A,A,S1]),
   system(ReplaceCmd),
   append_name(A,'.', Adot),
   append_name(Adot,S1,A1),
   process_include((A1:Ls)).

process_include((A:[])) :-
   process_include(A).
*/

process_include((A: Ls)) :-
   !,
   process_include1((A: Ls)),  
   retract_macro1(Ls). 

process_include(A) :-
   !,
   process_include1(A).

retract_macro1((L->R),Ls) :-  
   retract(macro1(L,R)), 
   retract_macro1(Ls).

retract_macro1((L->R)) :-
   retract(macro1(L,R)).

process_include1((A: (L, Ls))) :-
   !,
   process_include1((A: L)), 
   process_include1((A: Ls)).

process_include1((A: (Left->Right))) :- 
   !,
   subst_vars_for_macro_vars((Left->Right),(T1->T2),_Vs),
   ( T1 == T2  % doesn't need to include this as a macro
     -> true
   ; assertz(macro1(T1,T2)) ), 
   process_include1(A). 

process_include1(A) :-
   !,
   seeing_filename(CurrInput),
   determine_path(CurrInput,CurrPath),
   ( (value(mode,transition), value(dynamic_detected,true))
     -> include_file(A,CurrPath,1)
   ; include_file(A,CurrPath,0) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_show
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_all :-
  forall(valid_show_spec(B), process_show(B)).

process_show(B) :-
   retractall(show_spec(_)),
   process_show_internal(B).

process_show_internal((A;B)) :-
   !,
   process_show_internal(A),
   process_show_internal(B).

process_show_internal((A;B)) :-
   !,
   process_show_internal(A), 
   process_show_internal(B).

process_show_internal(C0) :-
   decode_macro(C0,C),
   ( C = (T: F = V)
     -> C1 = (T: F eq V)
   ; C = (F = V) 
     -> C1 = (F eq V)
   ; ( C = -((T: F = V)) ; C = (T: F \= V) ; C = (T: F <> V) )
     -> C1 = -((T: F eq V))
   ; ( C = -(F = V) ; C = (F \= V) ; C = (F <> V) ) 
     -> C1 = -(F eq V)
   ; C1 = C ),

   ( domain(_,_)
     -> \+ \+ valid_spec([C1])
   ; valid_show_spec(C1)),        
     !,
     ( retract(show_spec(Cs))
       -> append(Cs,[C1],NewCs), 
          assertz(show_spec(NewCs))
   ; assertz(show_spec([C1])) ).

process_show_internal(Spec) :-	
   nonfatal_error("Invalid show spec (~w) ignored.",[Spec]).

% valid_show_spec appears to perform a task similar to valid_spec, except
% that valid_show_spec is called when no objects have been defined yet, in
% which case terms can't be grounded to determine if they're valid specs

valid_show_spec(positive).
valid_show_spec(negative).
valid_show_spec(all).
valid_show_spec(ab).
valid_show_spec(none).
valid_show_spec(Term) :- \+ callable(Term), !, fail.
valid_show_spec(Term) :-
	functor(Term,_F,_N).
valid_show_spec(-Term) :- 
	functor(Term,_F,_N).

(show) :- format("enter specs (then ctrl-d)",[]),
        nl,
 	read_specs([],Specs),
	(Specs = []
	  -> Specs0 = positive
	; list_to_semituple(Specs, Specs0)),
	process_show(Specs0).


read_specs(SoFar, Specs) :-
	read(Spec),
	(Spec == end_of_file
	  -> Specs = SoFar
	; semituple_to_list(Spec,Spec0),
	  \+ \+ valid_spec(Spec0),
	  !,
	  append(Spec0,SoFar,SoFar0),
	  read_specs(SoFar0,Specs)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_tests((A;B)) :-
	!,
	process_tests(A), 
	process_tests(B).
process_tests(P/N) :-
	functor_test_spec(P/N),
	!,
        functor(Gen,P,N),
	assertz(test(Gen)).
process_tests(Spec) :-
	fatal_error("Invalid test declaration (~w).",[Spec]).

process_tests2((A;B)) :-
	!,
	process_tests2(A), 
	process_tests2(B).
process_tests2(((P/N,B):: File)) :-
        functor_test_spec(P/N),
        !,
        functor(Gen,P,N),
        assertz(test(Gen)), 
        process_tests2((B :: File)).
process_tests2((P/N :: File)) :-
	functor_test_spec(P/N),
	!,
        functor(Gen,P,N),
	assertz(test(Gen)),
        ( determine_full_filename(File,AbsFile)
          -> true
        ; fatal_error("File \'~w\' not found.",[File]) ),
        compile(AbsFile).
process_tests2(Spec) :-
	fatal_error("Invalid test declaration (~w).",[Spec]).

functor_test_spec(Spec) :-
        Spec = F/N, atom(F), integer(N), N >= 0.

process_maxstep(MaxStep) :-
   !,
   iset(mode,history),
   retractall(macro(maxstep,_,_)),
   eval_expr(MaxStep,MaxStep1),
   assertz(macro(maxstep,true,MaxStep1)),
   include_file('history.std',0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_query 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_query(Fields,QueryType) :-                      
   semituple_to_list(Fields,FieldList),
   ( member_check((label::Label),FieldList)
      ->  ( atomic(Label) 
            -> true 
          ; fatal_error("Invalid label field (~w).",[Label]) )
   ; Label =  0 ),
   
   ( member_check(((maxstep)::any),FieldList)
     -> process_unsolvable_query(Label,FieldList)
   ; true),

   ( member_check(((maxstep)::MaxStep),FieldList)
     -> do_term_expansion(MaxStep,MaxStep1),
        eval_expr(MaxStep1,MaxStep2),
        ( value(mode,history)
          -> nonfatal_error("query ~w - maxstep is already declared in history mode.~n",[Label])
        ; ( ((integer(MaxStep2), MaxStep2 >=0) ; (MaxStep2 == 'any') ; 
            (MaxStep2 = (T1..T2), check_bounds(T1,T2,Label)))
            -> true 
            ; fatal_error("Invalid maxstep in Query ~w", [Label]) ))
   ; ( value(mode,history)
       -> macro((maxstep),_,MaxStep2)
     ; MaxStep2 = 0 ) ),

   delete(FieldList, (label::Label), FieldList2),
   delete(FieldList2, ((maxstep)::MaxStep), Rules),
   ( QueryType == (nmquery)
     -> assertz(nmquery_problem(Label,MaxStep2,Rules))
   ; assertz(query_problem(Label,MaxStep2,Rules)) ).


check_bounds(T1,T2,Label) :-
    ( (integer(T1), T1>=0)
      -> true
    ; fatal_error("In query ~w, the lower bound for maxstep must be a nonnegative integer.~n",[Label]), fail),
    
    ( (integer(T2)
       -> ( T1>T2
           -> fatal_error("In query ~w, the upper bound for maxstep must be at least large as the lower bound ",[Label])
         ; true)
      ; ( T2 \== 'infinity' 
          -> fatal_error("In query ~w, the upper bound for maxstep must be a nonnegative integer or infinity.~n",[Label]) 
        ; true))).


process_unsolvable_query(Label,FieldList) :-
   member_check((invariant: Inv), FieldList),
   member_check(((maxstep): Goal), FieldList),

   delete(FieldList, (label::Label), FieldList1),
   delete(FieldList1, ((maxstep):: any), FieldList2),
   delete(FieldList2, ((maxstep): Goal), FieldList3),
   delete(FieldList3, (invariant: Inv), Rules),

   append_name(Label,'_init',InitLabel),
   append(Rules,[(0:Inv)],InitRules),
   assertz(query_problem(InitLabel,0,InitRules)),

   append_name(Label,'_goal',GoalLabel),
   append([(0:Inv)],[(0:Goal)],GoalRules),
   assertz(query_problem(GoalLabel,0,GoalRules)),
   
   append_name(Label,'_trans',TransLabel),
   append([(0:Inv)],[-(((1:Inv)))],TransRules),
   assertz(query_problem(TransLabel,1,TransRules)).


append_name(A,B,C) :-
   name(A,L), 
   name(B,L1),
   append(L,L1,L2),
   name(C,L2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% insert rules 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------%
% insert_rule_schema
%-----------------------------------------------------------------------------%

insert_rule_schema(Term,SchemaNo) :-
   do_term_expansion(Term,TermTree),
   leaf_element(TermTree,TermTree1),  
   ( (SchemaNo == 0)
     -> asserta(rule_schema(TermTree1))
   ; asserta(rule_schema1(TermTree1)) ).


%-----------------------------------------------------------------------------%
% insert_inertia_rules
%-----------------------------------------------------------------------------%

insert_inertia_rules :-
   findall(L, (is_sort(inertialFluent(L)), 
               domain(inertialFluent(L),D), D \== []), Ls),
   insert_inertia_rules(Ls). 

insert_inertia_rules([]).

insert_inertia_rules([L|Ls]) :-
   insert_inertia_rules_aux(L), 
   insert_inertia_rules(Ls).

insert_inertia_rules_aux(L) :-
   Term = (inertial var(inertialFluent(L),-1)),
   do_term_expansion(Term,TermTree),
   leaf_element(TermTree,(H <= B)),
   ( value(mode,transition)
     -> assertz(rule_schema1((H <= B)))
   ; assertz(rule_schema((H <= B))) ).


%-----------------------------------------------------------------------------%
% insert_attribute_rules
%-----------------------------------------------------------------------------%

insert_attribute_rules((A,B),Action) :-
   !,
   insert_attribute_rules(A,Action), 
   insert_attribute_rules(B,Action).

insert_attribute_rules(Cs,Action) :-
   Cs =.. [F|Sorts],  
   sorts_to_vars(Sorts,Vars),
   Cs1 =.. [F|Vars], 

   Action =.. [F1|Sorts1], 
   sorts_to_vars(Sorts1,Vars1),
   Action1 =.. [F1|Vars1], 

   ( value(mode,history)  % macro(maxstep,_,_)  % history mode
     -> assertz(rule_schema((false <= ((var(astep,-1): Cs1 eq none)<->(var(astep,-1): Action1)))))
   ; assertz(rule_schema1((false <= ((0: Cs1 eq none)<->(0: Action1))))) ),
   assertz(attribute0(Action,Cs)).

%-----------------------------------------------------------------------------%
% insert_additiveFluent_rules
%-----------------------------------------------------------------------------%

insert_additiveFluent_rules(AF,M,N) :-  % AF = additiveFluent(S)
   V_AF = var(AF,-1),   % additive Fluent
   arg(1,AF,S),         % integer sort
   V_VS = var(S,-1),
   V_A = var(action(boolean),-1),
   V_A1 = var(action(boolean),-2),
   V_I = var(additiveInteger,-100),
   V_I1 = var(additiveInteger,-101),

   Term = (default contribution(V_A, V_AF) = 0 where \+ abby(V_A)),
   insert_rule_schema(Term,1),

   Term1 = (caused accumulatedContribution(V_A,V_AF)=V_I
            if first(V_A) & contribution(V_A,V_AF) = V_I where \+ abby(V_A)),
   insert_rule_schema(Term1,1),

   Term2 = (caused accumulatedContribution(V_A,V_AF)=V_I+V_I1
            if next(V_A1,V_A) & accumulatedContribution(V_A1,V_AF) = V_I1
                              & contribution(V_A,V_AF) = V_I
            where \+ abby(V_A), \+abby(V_A1), V_I+V_I1 >= minAdditive, V_I + V_I1 =< maxAdditive ),
   insert_rule_schema(Term2,1),

   Term3 = (caused V_AF=V_I+V_VS
            after last(V_A) & accumulatedContribution(V_A,V_AF)=V_I
                  & V_AF=V_VS        
            where \+ abby(V_A), V_I+V_VS >= M, V_I+V_VS =< N),
   insert_rule_schema(Term3,1).


%-----------------------------------------------------------------------------%
% insert_additiveAction_rules
%-----------------------------------------------------------------------------%

insert_additiveAction_rules(AF,M,N) :-  % AF = additiveFluent(S)
   V_AF = var(AF,-1),   % additive Fluent
   V_A = var(action(boolean),-1),
   V_A1 = var(action(boolean),-2),
   V_I = var(additiveInteger,-100),
   V_I1 = var(additiveInteger,-101),

   Term = (default contribution(V_A, V_AF) = 0 where \+ abby(V_A)),
   insert_rule_schema(Term,1),

   Term1 = (caused accumulatedContribution(V_A,V_AF)=V_I
            if first(V_A) & contribution(V_A,V_AF) = V_I where \+ abby(V_A)),
   insert_rule_schema(Term1,1),

   Term2 = (caused accumulatedContribution(V_A,V_AF)=V_I+V_I1
            if next(V_A1,V_A) & accumulatedContribution(V_A1,V_AF) = V_I1
                              & contribution(V_A,V_AF) = V_I
            where \+ abby(V_A), \+ abby(V_A1), V_I+V_I1 >= minAdditive, 
                  V_I + V_I1 =< maxAdditive ),
   insert_rule_schema(Term2,1),

   Term3 = (caused V_AF=V_I
            if last(V_A) & accumulatedContribution(V_A,V_AF) = V_I
            where \+ abby(V_A), V_I >= M, V_I =< N),
   insert_rule_schema(Term3,1).

%-----------------------------------------------------------------------------%
% insert_query_rules.
%-----------------------------------------------------------------------------%

insert_query_rules([R0|_Rs]) :-
   leaf_element(R0,R),
   ( (R = (((B=>H) where C)); R = (((H<-B) where C)); 
      R = (((H<=B) where C)); 
      R = (((<- B) where C)), H = false; 
      R = (((<= B) where C)), H = false)
      ->  assertz(query_rule_schema(((H<=B) where C)))
   
   ; (R = (B=>H); R = (H<-B); R = (H<=B);
      R = (<- B), H = false; R = (<= B), H = false)
     ->   assertz(query_rule_schema((H<=B)))

   ; assertz(query_rule_schema(((R <= true)))) ),
   fail.

insert_query_rules([_R|Rs]) :-
   insert_query_rules(Rs).

insert_query_rules([]) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% instantiate_sorts : ground domain_schema/2 to produce domain/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instantiate_sorts :-
   get_ccsorts(Nodes),
   instantiate_sorts(Nodes,[]),
   !.

instantiate_sorts([],_Ancestors).
instantiate_sorts([node(S,Subs)|Nodes],Ancestors) :-
   ( domain(S,_)  % skip since already constructed
      -> instantiate_sorts(Nodes,Ancestors)
   ; member(S,Ancestors)
     -> fatal_error("Cyclic term.  term/arg of sort ~w.",[S])
   ; instantiate_sorts(Subs,[S|Ancestors]),  % subs first
     ( domain_schema(S,Cs) 
       -> true 
     ; Cs = [] ),
     instantiate_sorts_in_terms(Cs,Ancestors), % in args
     instantiate_terms(Cs,Es1),
     subsorts_extension(Subs,Es2),   % after instantiate_sort(Subs,_) above
                                     % subsorts_extension(Subs,_) returns
                                     % all subsorts of Sort S
     append(Es1,Es2,Es3),
     remove_duplicates(Es3,Es),

     ( domain(S,Es) 
     -> true 
     ; assertz(domain(S,Es))),
     instantiate_sorts(Nodes,Ancestors) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% instantiate_sorts_in_terms(
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instantiate_sorts_in_terms([],_Ancestors).

instantiate_sorts_in_terms([C|Cs],Ancestors) :-
   C =.. [_|Sorts],  % arguments
   find_nodes(Sorts,Nodes),  
   instantiate_sorts(Nodes,Ancestors),
   instantiate_sorts_in_terms(Cs,Ancestors).

instantiate_sorts_in_terms([C|Cs],[]) :-
   C =.. [_|Sorts],
   ( Sorts = [] 
     -> instantiate_sorts_in_terms(Cs,[])
   ; fail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_nodes(+Sorts,-Nodes) 
%%   Sorts : a list of sorts
%%   Nodes : corresponding nodes in ccsorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
find_nodes(Sorts,Nodes) :-
   get_ccsorts(Graph),
   find_nodes(Sorts,Graph,Nodes).

find_nodes([],_Graph,[]).

find_nodes([Sort|Sorts],Graph,[Node|Nodes]) :-
   ( find_node(Graph,Sort,Node) 
     -> find_nodes(Sorts,Graph,Nodes)
   ; fatal_error("~w is not a known sort.",[Sort]) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_node(+Graph,+Sort,-Nodes) : find node for Sort in ccsorts Graph
%%   Graph : ccsorts in process
%%   Sort  : sort name
%%   Nodes : corresponding nodes in ccsorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
find_node([node(S,Subs)|Nodes],Sort,Node) :-
   ( S = Sort
     -> Node = node(S,Subs)
   ; find_node(Subs,Sort,Node) 
     -> true
   ; find_node(Nodes,Sort,Node) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% subsorts_extension(+Subs,-Es) : returns all ground instances in domain/2
%%                                 of nodes in Subs
%%  Subs : list of nodes in ccsorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subsorts_extension([],[]).

subsorts_extension([node(S,_)|Nodes],Es) :-
   domain(S,Es1),
   subsorts_extension(Nodes,Es2),
   append(Es1,Es2,Es).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% instantiate_terms(+Cs,-Ds) : Cs : [alive(turkey)] Ds : [alive(turkey1) alive(turkey2)]
%%   Cs : list of objects or constants whose arguements are sort names
%%   Ds : list of objects or constants whose arguements are replaced 
%%          with ground instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%findall(on(_21109,_21110),user:bind_vars_to_terms([_21109=[a,b],_21110=[a,b,table]]),[on(a,a),on(a,b),on(a,table),on(b,a),on(b,b),on(b,table)|_23722],_23722) 
instantiate_terms([C|Cs],Ds) :-
   subst_vars_for_type_names(C,D,Vs),
   findall(D,bind_vars_to_terms(Vs),Ds,Es),
   instantiate_terms(Cs,Es).
instantiate_terms([],[]).        


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% subst_vars_for_type_names(+C,-D,-Vs) :
%%   C : object or constant whose argument are sort(type) names
%%   D : object or constant whose argument are the corresponding variable
%%   Vs: list of var=[ground instance of sort]
%%   ex) subst_vars_for_type_names(arirang(aa),arirang(_61671),[_61671=[a,a1]])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subst_vars_for_type_names(C,D,Vs) :-
   ( atom(C) ; composite_sort(C) ; C = _* ),
   !,
   ( domain(C,Cs) -> Vs = [=(D,Cs)] ; D = C, Vs = [] ).
subst_vars_for_type_names(C,D,Vs) :-
   functor(C,F,N),
   functor(D,F,N),
   subst_vars_for_type_names_arg(C,0,N,D,Vs).

subst_vars_for_type_names_arg(_C,N,N,_D,[]) :-
   !.
subst_vars_for_type_names_arg(C,M,N,D,Vs) :-
   M1 is M+1,
   arg(M1,C,A),
   subst_vars_for_type_names(A,B,Xs),
   arg(M1,D,B),
   subst_vars_for_type_names_arg(C,M1,N,D,Ys),
   append(Xs,Ys,Vs).

bind_vars_to_terms([=(V,Cs)|Vs]) :-
   member(V,Cs), bind_vars_to_terms(Vs).
bind_vars_to_terms([]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% enumerate_atoms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Determines all ground atoms in the theory, based on the object and
% constant declarations.  Each atom will be assigned an integer (in
% an unbroken sequence).  After all atoms have been enumerated, the variable
% atom_count will be set to the number of atoms.  The variable
% original_atom_count is also set to the number of atoms; while atom_count
% will be incremented as new atoms are added during rule grounding
% and completion, original_atom_count will not, and it will therefore always
% show the number of "real" atoms (not auxiliary atoms) in the theory.
%
% In transition mode, some atoms will need to be shifted to later time units
% during the query process; it is therefore necessary to enumerate the atoms
% in a manner which makes it clear which atoms should be shifted and which
% should not.  This is accomplished by first enumerating all atoms
% corresponding to rigid constants, then fluents at time 0, then actions and
% attributes at time 0, and finally fluents at time 1.  At the end of the
% procedure, the variables atom_count and original_atom_count are set as above.
% In addition, rigid_count, fluent_count, and action_count will store the
% number of rigid constants, fluents, and actions, respectively.  (The latter
% two represent the number of fluents or actions at *each* time step, not
% the total number.)  Also, static0_atom_count will store the number of rigid
% constants and fluents together; this is the number of atoms at the start of
% the sequence which will not need to be shifted during a query.
%
% History and basic mode don't need shifting, but the atoms are still processed
% in the same order, continuing the pattern beyond time step 1.  The
% show_models procedure relies on this ordering for efficient display of
% solutions.  The various atom counts stored for transition mode aren't stored
% for the other modes -- only atom_count and original_atom_count.

enumerate_atoms :-
   iset(atom_count,0),
   ( value(mode,transition)
     -> Steps = [0,1]
   ; domain(step,Steps) ),
   enumerate_atoms([none|Steps]).

enumerate_atoms([]) :-
   value(atom_count,AC),
   iset(original_atom_count,AC).

enumerate_atoms([none|Ts]) :-
   domain(rigidAtomicFormula,Fs),
   enumerate_atoms(none,Fs),
   ( value(mode,transition) -> set_atom_count(rigid_count,_) ; true ),
   enumerate_atoms(Ts).

enumerate_atoms([T|_Ts]) :-
   ( domain(simpleFluentAtomicFormula,Fs)
   ; domain(sdFluentAtomicFormula,Fs) ),
   enumerate_atoms(T,Fs),
   fail.

enumerate_atoms([0|_Ts]) :- 
   value(mode,transition),
   set_atom_count(atom_count_0,_),
   set_atom_count(fluent_count,rigid_count),
   fail.

enumerate_atoms([T,_T2|_Ts]) :-
   ( domain(actionAtomicFormula,Fs)
   ; domain(abActionAtomicFormula,Fs)
   ; domain(attributeAtomicFormula,Fs) ),
   enumerate_atoms(T,Fs),
   fail.

enumerate_atoms([0|_Ts]) :-
   value(mode,transition),
   set_atom_count(action_count,atom_count_0),
   fail.

enumerate_atoms([_T|Ts]) :-
   enumerate_atoms(Ts).

enumerate_atoms(T,Fs) :-
   member(F,Fs),
   \+ (F = (A2 eq false), boolean_constant(A2)),
   ( T == none
     -> A = F
   ; A = (T: F) ),
   \+ atom_integer(A,_),
   incr(atom_count,N),
   assertz(atom_integer(A,N)),
   fail.

enumerate_atoms(_,_).

set_atom_count(Count,Prev) :-
   ( nonvar(Prev), value(Prev,PrevVal) -> true ; PrevVal = 0 ),
   ( value(atom_count,AC) -> true ; AC = 0 ),
   AC2 is AC - PrevVal,
   iset(Count,AC2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% renumber atoms prior to shifting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

renumber_atoms :-

   % During rule grounding and completion, new atoms may be introduced to
   % simplify some of the clauses.  These atoms will be added to the end of
   % the sequence.  However, some of these new atoms appear in clauses
   % corresponding to time step 0 and don't need to be shifted during
   % transition mode.  We need to
   % move these atoms down to the portion of the atom sequence which isn't
   % shifted.  Also, since new atoms for time 0 will appear before
   % fluents for time 0, we need to move new atoms for time 1 to appear
   % before fluents for time 1, so that each time unit is shifted the same
   % way.  The sequence of atoms looks like the following:
   %
   %     Before shift:			After shift:
   %     -------------			------------
   %     Rigid constants		Rigid constants
   %     Fluents at time 0		New atoms for time 0 (from rules
   %     Actions/attributes at time 0	   & completion)
   %     Fluents at time 1		Fluents at time 0
   %     New atoms from rules		Actions/attributes at time 0
   %        (time 0 & 1)		New atoms for time 1 (from rules
   %     New atoms from completion	   & completion)
   %        (time 0 & 1)		Fluents at time 1
   %
   % Several counters were set during enumeration of the atoms which tell how
   % large each section is: rigid_count, fluent_count, action_count,
   % and new_atom_count hold the number of rigid fluents, fluents,
   % actions/attributes, and new atoms, respectively.
   % static0_new_atom_count holds the number of auxiliary atoms corresponding
   % to time unit 0 only.
   %
   % After this procedure has completed, static0_atom_count will be changed
   % to reflect the total number of atoms which will not be shifted, including
   % new atoms of both kinds.

   value(atom_count,C),
   value(rigid_count,RC),
   value(fluent_count,FC),
   value(action_count,AC),
   value(aux_atom_count_from_rules_0,NAR0),
   value(aux_atom_count_from_rules,NAR),
   value(aux_atom_count_from_completion_0,NAC0),
   value(aux_atom_count_from_completion,NAC),

   % The Level variables store the highest index for each type of atom, and
   % the Shift variables store the amount by which that type will be shifted.
   % See the table above for a depiction of the relative ordering of atom
   % types before and after shifting.
   %
   % atoms with an index <= Level1 won't be moved -- these are rigid constants
   % atoms with Level1 < index <= Level2 will be shifted upward -- these are
   %    fluents at time 0 and actions/attributes at time 0
   % atoms with Level2 < index <= Level3 will be shifted upward -- these are
   %    fluents at time 1
   % atoms with Level3 < index <= Level4 will be shifted downward to the gap
   %    above Level1 -- these are new atoms generated by rules at time 0
   % atoms with Level4 < index <= Level5 will be shifted downward to the gap
   %    above Level2 -- these are new atoms generated by rules at time 1
   % atoms with Level5 < index <= Level6 will be shifted downward to the gap
   %    between Level4 and Level1 -- these are new atoms generated by 
   %    completion at time 0
   % atoms with index > Level6 will be shifted downward to the gap between
   %    Level4 and Level2 -- these are new atoms generated by completion at
   %    time 1

   Level1 is RC,
   Level2 is Level1 + FC + AC,
   Level3 is Level2 + FC,
   Level4 is Level3 + NAR0,
   Level5 is Level3 + NAR,   % NAR includes NAR0 + NAR1
   Level6 is Level5 + NAC0,
   Shift2 is NAR0 + NAC0,
   Shift3 is NAR + NAC,
   Shift4 is -(2*FC+AC),
   Shift5 is -FC + NAC0,
   Shift6 is -(2*FC+AC)-(NAR-NAR0),
   Shift7 is -FC,
   Shifts = [[Level1,0],[Level2,Shift2],[Level3,Shift3],[Level4,Shift4],
      [Level5,Shift5],[Level6,Shift6],[C,Shift7]],

   save_atom_integer_mapping,
   findall(atom_integer(A,I),atom_integer(A,I),AIs),
   retractall(atom_integer(_,_)),
   renumber_atoms(AIs,Shifts),

   findall(C0,clause0(C0),Cs0),
   retractall(clause0(_)),
   renumber_clauses(0,Cs0,Shifts),
   findall(C1,clause1(C1),Cs1),
   retractall(clause1(_)),
   renumber_clauses(1,Cs1,Shifts),

   value(atom_count_0,AC0),
   NewAC0 is AC0 + NAR0 + NAC0,
   iset(atom_count_0,NewAC0).

save_atom_integer_mapping :-
   atom_integer(A,I),
   assertz(saved_atom_integer(A,I)),
   fail.
save_atom_integer_mapping.


renumber_atoms([],_).

renumber_atoms([atom_integer(A,I)|AIs],[[Bound,Shift]|Shifts]) :-
   ( I =< Bound
     -> NewI is I + Shift,
        assertz(atom_integer(A,NewI)),
	renumber_atoms(AIs,[[Bound,Shift]|Shifts])
   ; renumber_atoms([atom_integer(A,I)|AIs],Shifts) ).

renumber_clauses(_,[],_).

renumber_clauses(Phase,[C|Cs],Shifts) :-
   % For each clause, shift all of the literals in the clause to match the
   % shifted atoms
   renumber_clause(C,Shifts,NewC),
   ( Phase == 0
     -> assertz(clause0(NewC))
   ; assertz(clause1(NewC)) ),
   renumber_clauses(Phase,Cs,Shifts).


renumber_clause([],_,[]).

renumber_clause([L|Ls],Shifts,[N|Ns]) :-
   renumber_atom_index(L,Shifts,N),
   renumber_clause(Ls,Shifts,Ns).

renumber_atom_index(I,[[Bound,Shift]|Shifts],N) :-
   ( I > 0
     -> ( I =< Bound
          -> N is I + Shift
        ; renumber_atom_index(I,Shifts,N) )
   ; AbsI is 0 - I,
     renumber_atom_index(AbsI,[[Bound,Shift]|Shifts],AbsN),
     N is 0 - AbsN ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Procedures used in process_rule_schemas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_macro 
%% ex) (p=var(sort(p))<=p=var(sort(p))&&p=var(sort(p)))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% generate a variable V_Sort_Identifier and assert
get_var(Sort,Identifier,Var) :-  
   name('V',[ListV]),
   name('_',[ListUS]),
   ( Sort =.. [F,A]
     -> name(F,ListF),
        ( A =.. [A1,R]  % disk*
          -> name(A1,ListA1),
             name(R,ListR),
             append(ListA1,ListR,ListA)
        ; name(A,ListA) ),
        append(ListF,ListA,ListL)
   ; name(Sort,ListL) ),
   
   ( Identifier =.. [FI,AI]
     -> name(FI,ListFI),
        name(AI,ListAI),
        append(ListFI, ListAI, ListNV0)
   ; name(Identifier,ListNV0)),

   append([ListV,ListUS|ListL],ListNV0,ListNewVarLabel),
   name(Var,ListNewVarLabel),
   (var_sort(Var,Sort) -> true; assertz(var_sort(Var,Sort))). 
    
get_internal_var(Sort,Var) :- 
  value(var_counter,VC),
  VC1 is VC-1,
  iset(var_counter,VC1),
  get_var(Sort,VC,Var).

% need to be changed
get_sort(Var,ValSort) :-
   atom(Var), 
   var_sort(Var,CompositeSort), 
   !, % variable
   arg(1,CompositeSort,ValSort).

get_sort(Constant,ValSort) :-
   functor(Constant,F,N),
   functor(B,F,N),
   domain_schema(Sort,Ds), 
   member(B,Ds),
   arg(1,Sort,ValSort).

get_sort2(Constant,Sort) :-
   functor(Constant,F,N),
   functor(B,F,N),
   domain_schema(Sort,Ds), 
   member(B,Ds).


decode_macro(Schema,Schema) :- 
   (var(Schema); atomic(Schema)), 
   !.
decode_macro(Schema,DecodedSchema) :-
   functor(Schema,F,N),
   functor(Schema1,F,N),
   decode_macro_arg(Schema,1,N,Schema1), 
   ( Schema1=sort(C) 
     -> get_sort(C,Sort),
        DecodedSchema=Sort
   ; Schema1=sort2(C) 
     -> get_sort2(C,Sort),
        DecodedSchema=Sort
   ; Schema1=var(Sort,Id)
     -> get_var(Sort,Id,Var),
        DecodedSchema=Var
   ; DecodedSchema=Schema1 ).

decode_macro_arg(_Schema,M,N,_Schema1) :-
   M > N, 
   !.
decode_macro_arg(Schema,M,N,Schema1) :-
   arg(M,Schema,A),
   decode_macro(A,B),
   arg(M,Schema1,B),
   M1 is M+1,
   decode_macro_arg(Schema,M1,N,Schema1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% expand_boolean % this should be done when the constant isn't an argument
%% ex) edge(0,2) -> edge(0,2)eq true , before binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% macro
boolean_constant(var(Sort,_)) :-
   member_check(Sort,
          [simpleFluent(boolean), inertialFluent(boolean),
           sdFluent(boolean), rigid(boolean), 
           action(boolean), abAction(boolean), exogenousAction(boolean), attribute(boolean)]). 

% variable
boolean_constant(C) :-  
   var_sort(C,Sort),
   member_check(Sort,
          [simpleFluent(boolean), inertialFluent(boolean),
           sdFluent(boolean), rigid(boolean), 
           action(boolean), abAction(boolean), exogenousAction(boolean), attribute(boolean)]). 

% name
boolean_constant(C) :-
   !,
   functor(C,F,N),
   boolean_constants(F,N).


% should be top down construction 
expand_boolean(Schema,Schema) :- var(Schema), !.
expand_boolean(Schema,Schema) :-
    functor(Schema,F,_), member_check(F, [eq, /\, \/, @<, ==]), !.
expand_boolean(Schema,ExpandedSchema) :-
    ( (Schema = -((T: C)), boolean_constant(C))
        -> ExpandedSchema = (T: C eq false)
      ; (Schema = -(C), boolean_constant(C))
        -> ExpandedSchema = (C eq false)
      ; boolean_constant(Schema)
        -> ExpandedSchema = (Schema eq true)
      ; is_constant(Schema)
        -> ExpandedSchema = Schema 
      ; functor(Schema,F,N),    
        functor(ExpandedSchema,F,N),
        expand_boolean_arg(Schema,1,N,ExpandedSchema) ).

expand_boolean_arg(_Schema,M,N,_Schema1) :- M > N, !.
expand_boolean_arg(Schema,M,N,Schema1) :-
   arg(M,Schema,A),
   expand_boolean(A,B),
   arg(M,Schema1,B),
   M1 is M+1,
   expand_boolean_arg(Schema,M1,N,Schema1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% remove_time_stamps(Schema,RemovedSchema) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove time stamps for rigid constant and test expression

remove_time_stamps(Schema,RemovedSchema) :-
   ( ( (Schema = ((_ : C eq V)) ), rigid(C) )
     -> RemovedSchema = (C eq V)
   ; (Schema = ((_ : T)), test(T))
     -> RemovedSchema = T
   ; (Schema = ((_ : A)), rigid(A))
     -> RemovedSchema = A
   ; functor(Schema,F,N),
     functor(RemovedSchema,F,N),
     remove_time_stamps_arg(Schema,1,N,RemovedSchema) ).

remove_time_stamps_arg(_Schema,M,N,_Schema1) :- M > N,   !.
remove_time_stamps_arg(Schema,M,N,Schema1) :-
   arg(M,Schema,A),
   remove_time_stamps(A,B),
   arg(M,Schema1,B),
   M1 is M+1,
   remove_time_stamps_arg(Schema,M1,N,Schema1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% postprocessing_before_binding = expand_boolean + remove_time_stamps_for_rigid
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
postprocessing_before_binding(Schema,Schema1) :-
   expand_boolean(Schema,Schema2), 
   remove_time_stamps(Schema2,Schema1).
*/

postprocessing_before_binding(Schema,ExpandedSchema) :-
     ( (functor(Schema,F,_), member_check(F, [eq, /\, \/, @<, ==]))
     -> ExpandedSchema = Schema
     ; (Schema = ((_ : C eq V)), rigid(C))
       -> ExpandedSchema = (C eq V)
     ; (Schema = ((_ : T)), test(T))
       -> ExpandedSchema = T      
     ; (Schema = ((_ : A)), rigid(A))
       -> ( boolean_constant(A)
            -> ExpandedSchema = (A eq true)
          ; ExpandedSchema = A )
     ; (Schema = -((T: C)), boolean_constant(C))
       -> ( rigid(C)
            -> ExpandedSchema = (C eq false)
          ; ExpandedSchema = (T: C eq false) )
     ; (Schema = -(C), boolean_constant(C))
       -> ExpandedSchema = (C eq false)
     ; boolean_constant(Schema)
       -> ExpandedSchema = (Schema eq true)
     ; ( is_constant(Schema)     % don't expand boolean constants inside some
                              %  other constant
         -> ExpandedSchema = Schema
       ; functor(Schema,F,N),    
         functor(ExpandedSchema,F,N),
         postprocessing_before_binding_arg(Schema,1,N,ExpandedSchema) ) ).

postprocessing_before_binding_arg(_Schema,M,N,_Schema1) :-
   M > N, 
   !.
postprocessing_before_binding_arg(Schema,M,N,Schema1) :-
   arg(M,Schema,A),
   postprocessing_before_binding(A,B),
   arg(M,Schema1,B),
   M1 is M+1,
   postprocessing_before_binding_arg(Schema,M1,N,Schema1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% postprocessing_after_binding
%% ex) edge(1,1+2) -> edge(1,3)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% as soon as arguments are evaluated, C = V -> C eq V is done.
postprocessing_after_binding(Schema,Schema) :- 
   (var(Schema); atomic(Schema)), 
   !.
postprocessing_after_binding(Schema0,EvaluatedSchema) :-
   ( ( Schema0 = (T: C eq false), boolean_constant(C))
     -> Schema = -((T: C eq true))
   ; ( Schema0 = (C eq false), boolean_constant(C))
     -> Schema = -(C eq true)
   ; Schema = Schema0 ),

   ( evaluable_expr(Schema)
     -> ( test(Schema)
          -> ( Schema =.. [= | Args]
               -> Schema1 =.. [=:= | Args]  % to call prolog to evaluate =
             ; Schema1 = Schema ),          %  in evaluable expression
             ( call(Schema1)
               -> EvaluatedSchema=true
             ; EvaluatedSchema=false )
        ; EvaluatedSchema is Schema)  % otherwise error
   ; functor(Schema,F,N),
     functor(EvaluatedSchema,F,N),
     postprocessing_after_binding_arg(Schema,1,N,EvaluatedSchema) ).

postprocessing_after_binding_arg(_Schema,M,N,_Schema1) :-
   M > N, 
   !.
postprocessing_after_binding_arg(Schema,M,N,Schema1) :-
   arg(M,Schema,A),
   postprocessing_after_binding(A,B),
   arg(M,Schema1,B),
   M1 is M+1,
   postprocessing_after_binding_arg(Schema,M1,N,Schema1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace_false_with_negative_literal
%% ex) edge(0,2) eq false -> -(edge(0,2) eq true) after binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%^ macro would be better? - no it's slower
replace_false_with_negative_literal(Schema,Schema) :-
   (var(Schema); atomic(Schema)),
   !.
replace_false_with_negative_literal(Schema,ReplacedSchema) :-
   ( ( (Schema = (T: C eq false)), boolean_constant(C))
     -> ReplacedSchema = -((T: C eq true))
   ; ( (Schema = (C eq false)), boolean_constant(C))
     -> ReplacedSchema = -(C eq true)
   ; functor(Schema,F,N),
     functor(ReplacedSchema,F,N),
     replace_false_with_negative_literal_arg(Schema,1,N,ReplacedSchema) ).

replace_false_with_negative_literal_arg(_Schema,M,N,_Schema1) :-
   M > N,
   !.
replace_false_with_negative_literal_arg(Schema,M,N,Schema1) :-
   arg(M,Schema,A),
   replace_false_with_negative_literal(A,B),
   arg(M,Schema1,B),
   M1 is M+1,
   replace_false_with_negative_literal_arg(Schema,M1,N,Schema1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%^ note
%^ decode macro and expand boolean put together; conflict - 
%^ decode-macro should be bottom up for var processing
%^ while expand-boolean should be top-down to avoid(/\A eq true)

%%%%%%%%%%%%%%%%%%%%%%
% evaluable_expr(+Expr) : check whether Expr is an evaulable expression
%                        used in eval_expr
%%%%%%%%%%%%%%%%%%%%%%
evaluable_expr(Expr) :- number(Expr), !.
%evaluable_expr(Expr) :- var_sort(Expr,S), \+ composite_sort(S), !.
                           % caused N=1. : in decode_nested_constants_in_rule
evaluable_expr(-(Expr)) :- evaluable_expr(Expr), !.
evaluable_expr(Expr) :-
   functor(Expr,F,2), 
   !,                    % binary operator
   ( member_check(F,['+','-','*','/','//','rem','mod','gcd','min',
               'max']) %,'**','>','<','=<','>=','='])
     -> arg(1,Expr,FirstArg), evaluable_expr(FirstArg), 
        arg(2,Expr,SecondArg), evaluable_expr(SecondArg) ).

evaluable_expr(Expr) :-          % unary operator
   functor(Expr,F,1), 
   !,
   ( member_check(F,['abs'])
     -> arg(1,Expr,FirstArg), evaluable_expr(FirstArg) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% eval_expr
%% ex) edge(1,1+2) -> edge(1,3)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% as soon as arguments are evaluated, C = V -> C eq V is done.
eval_expr(Schema,Schema) :- 
   (var(Schema); atomic(Schema)), 
   !.

eval_expr(Schema,EvaluatedSchema) :-
   ( evaluable_expr(Schema)
     -> ( test(Schema)
          -> ( Schema =.. [= | Args]
               -> Schema1 =.. [=:= | Args]  % to call prolog to evaluate =
             ; Schema1 = Schema ),          %  in evaluable expression
             ( call(Schema1)
               -> EvaluatedSchema=true
             ; EvaluatedSchema=false )
        ; EvaluatedSchema is Schema)  % otherwise error
   ; functor(Schema,F,N),
     ( ( (F == '='),arg(1,Schema,C),is_constant(C) ) 
          % since decode_nested_constants was executed before, 
          %  the remaining C = X should all turn into C eq X 
          %  if C is a constant
       -> F1 = 'eq'
     ; F1 = F ),
     functor(EvaluatedSchema,F1,N),
     eval_expr_arg(Schema,1,N,EvaluatedSchema) ).

eval_expr_arg(_Schema,M,N,_Schema1) :-
   M > N, 
   !.
eval_expr_arg(Schema,M,N,Schema1) :-
   arg(M,Schema,A),
   eval_expr(A,B),
   arg(M,Schema1,B),
   M1 is M+1,
   eval_expr_arg(Schema,M1,N,Schema1).

          

%---------------------------------------------------------------------


adjust_where(C,_) :-
   var(C), !.
adjust_where((-(A)),(\+ (A1))) :- 
   \+ evaluable_expr(A), 
   !, 
   adjust_where(A,A1).
adjust_where(C,C1) :-
   functor(C,F,N), 
   ( (F == '&&'; F == '&')
      -> functor(C1,',',N)
   ; (F == '++')
      -> functor(C1,';',N)
   ; (F == '=')  % instead of =:=. car1=:=car2 will not do; instead car1==car2
                 %  it's okay because the condition will be called after
                 %  evaluating expression arguments.
      -> functor(C1,'==',N) ), 
   !,
   adjust_where_arg(C,1,N,C1). 
adjust_where(C,C1) :-
   functor(C,F,N),
   functor(C1,F,N), 
   adjust_where_arg(C,1,N,C1). 

adjust_where_arg(_C,M,N,_C1) :-
   M>N,
   !.
adjust_where_arg(C,M,N,C1) :-
   arg(M,C,A),
   adjust_where(A,B), 
   arg(M,C1,B), 
   M1 is M+1, 
   adjust_where_arg(C,M1,N,C1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_rule_schemas 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Ground all of the rule schemas, by replacing every variable in each schema
% with each possible object of the appropriate type.  During grounding,
% new atoms may be introduced to simplify complicated formulae
% (specifically those which contain a large conjunction of disjunctions or
% disjunction of conjunctions -- new atoms are introduced to avoid
% distributing one connective across the other).  At the end of the procedure,
% the variable rule_count will store the total number of rules.
%
% In transition mode, some of the clauses generated will need to be shifted
% to later time units when a query is made.  The rules are therefore grounded
% in such a way as to distinguish between the clauses which must be shifted
% and those which must not.  First, all rule schemas stored with the predicate
% rule_schema/2 are grounded; these correspond to static laws at time 0.
% Next, the schemas in rule_schema1/2 are grounded; these correspond to static
% laws at time 1 and dynamic laws, which must be shifted.  At the end of the
% procedure, the variable rule_count will still store the total number of
% rules as above.  Additionally, rule_count_0 will contain the number
% of rules which will not be shifted; aux_atom_count_from_rules_0
% will store the number of new atoms generated during the grounding of
% static laws at time 0; and aux_atom_count_from_rules will store the total
% number of new atoms introduced during rule grounding.

abby(A eq true) :- ab(A). 
abby(A eq false) :- ab(A). 

process_rule_schemas :-
   iset(rule_count_0,0),
   iset(rule_count,0),
   iset(aux_atom_count,0),

   % Start a loop -- select each rule schema and process it.  After all rules
   % in rule_schema/2 have been processed, set the value of static0_rule_count
   % equal to the current rule_count, and then start processing rules in
   % rule_schema1/2.

   ( value(mode,transition)
     -> ( % First, process static laws at time 0
          Phase=0,
	  assertz(domain(step,[0])),
	  assertz(domain(astep,[])),
	  rule_schema(Schema)
	; % Set variables related to static laws at time 0, then process
	  % static laws at time 1 & dynamic laws
          Phase=1,
	  value(atom_count,AC),
	  value(original_atom_count,OAC),
	  AACR0 is AC - OAC,
	  iset(aux_atom_count_from_rules_0,AACR0),
	  value(rule_count,RC),
	  iset(rule_count_0,RC),
	  retractall(domain(step,_)),  
	  retractall(domain(astep,_)),
	  assertz(domain(step,[0,1])),
	  assertz(domain(astep,[0])),
	  rule_schema1(Schema) )
   ; rule_schema(Schema) ),

   ( Schema = (Schema1 where Condition)
     -> adjust_where(Condition,Condition1) 
   ; Schema = Schema1,
     Condition1 = true ),

% postprocessing before binding
   decode_macro(Schema1,Schema2),        
   decode_nested_constants_in_rule(Schema2,Schema3,CVs),
   postprocessing_before_binding(Schema3,Schema5),
   find_free_variables(Schema5,Vs1),

/* to save time
%   ( (Phase==1, \+ rule_has_time_stamps(Schema7))
%      -> fail
%   ; true ),
*/
   ( Condition1 \== true 
     -> decode_macro(Condition1,Condition2),
        decode_nested_constants_for_condition(Condition2,Condition3,CVs),
        expand_boolean(Condition3,Condition4),
        remove_time_stamps(Condition4,Condition5),
        find_free_variables(Condition5,Vs2)
     ; Condition5 = true, 
       Vs2 = [] ),

   append(Vs1,Vs2,Vs3),
   remove_duplicates(Vs3,Vs),
   renaming_subst(Vs,Sub),
   subst_free(Schema5,Schema6,Sub),
   subst_free(Condition5,Condition6,Sub),

   % at this point H0<=B0 is a rule schema whose variable is 
   % in the form of prolog, and Sub is a pair like, ['B'=_297545,'L'=_297906]
   bind_vars(Sub),  % loop starts here - until every free variable is 
                    %   replaced with ground instances of corresponding sort
   ( (eval_expr(Condition6,Condition7), call(Condition7))
                % eval_expr?  car1=:=car2 will not do; instead car1==car2
     -> true 
   ; fail ),

   postprocessing_after_binding(Schema6,(H<=B)),

   negation_normal_form(B,B1),
   ( B1 = false -> fail ; true ),
   negation_normal_form(H,H1),

   % remember the current value of atom_count
   value(atom_count,A),

   % convert head to CNF and body to DNF, possibly introducing new auxiliary
   % atoms
   nnf_to_cnf(H1,Hss,Aux1),
   nnf_to_dnf(B1,Bss,Aux2),
   store_rule_schemas(Hss,Bss),

   % if either head or body is empty, the rule wasn't stored, so the
   % clauses defining each new atom shouldn't be either.  (Otherwise
   % these atoms are defined in the input file but are useless since they
   % don't appear in any "real" clause.)  Reset atom_count to its previous
   % level to "delete" those new atoms.
   ( ( Hss == []; Bss == [] ) 
     -> iset(atom_count,A)

       % otherwise, store the clauses defining new atoms used in the
       % main clause.
   ; store_aux_schemas(Aux1),
     store_aux_schemas(Aux2) ),
   fail.

process_rule_schemas :-
   value(original_atom_count,OAC),
   value(atom_count,AC),
   AACFR is AC - OAC,
   iset(aux_atom_count_from_rules,AACFR).

store_rule_schemas(Hss,Bss) :-
   member(Ms,Hss),
   member(Ns,Bss),
   incr(rule_count,I),
   db_open_external(I),
   db_store_rule(I,rule_body(I,Ns)),
   ( Ms = [] -> Ms1 = [0]; Ms1 = Ms ),
   ( Ms1 = [D]
     -> db_store_rule(I,(D<=I)) 
   ; fatal_error("One of the rules is not definite.",[])),
   fail.  % loop ends here

store_rule_schemas(_,_).


store_aux_schemas(Aux) :-
   member(C,Aux),
   negate_lits(C,NC),
   store_rule_schemas([[]],[NC]),
   fail.

store_aux_schemas(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_nested_constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% decode_nested_constants_in_rule(+Rule,-Rule1,-CVs) : 
%    Rule: causal rule in which constants may contain other constants
%          as arguments. For a theory to be definite, such nesting
%          should not occur in the heads
%    Rule1: the resulting rule of unfolding
%    CVs:   used in condition 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_nested_constants_in_rule((H<=B),(H<=B2),CVs) :-
%^  should check nested constants are not used in the head of a rule
    ( (H = ((_: C = D)) ; (H = (C = D)))
      -> ( (evaluable_expr(C), evaluable_expr(D))
           -> true
         ; (evaluable_expr(C); evaluable_expr(D))
           -> subst_functor(eq,=,H,H2),
              fatal_error("Unrecognized identifiers in ~w",H2)
         ; subst_functor(eq,=,(H<=B),(H1<=B1)), 
           fatal_error("The rule is not definite: ~w",(H1<=B1)) )
    ; true ),

   decode_nested_constants(B,B1,CVs),
   list_to_conjuncts(CVs,Conjuncts),
   B2 = (B1 & Conjuncts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% decode_nested_constants(+Formula,-Formula1,-CVs)  
%   Formula:  may contain nested constants
%   Formula1: nested constants are replaced with variables
%   CVs:      a list of (T: C eq V)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_nested_constants(Formula,Formula1,CVs) :-
   decode_nested_constants(Formula,Formula1,[],CVs,-1).

decode_nested_constants(Formula,Formula1,Acc,CVs,Step) :-
   ( (is_constant(Formula); expr(Formula); test(Formula))  
           % besides constants, arithmetic expression between constants
           % (e.g., C1+C2 where C1 and C2 are constants) or test 
           % predicates(e.g., nextTo(C1,C2) where nextTo is a user defined
           % constants) should also be considered in unfolding
     -> unfold_nested_constants(Formula,Formula1,Acc,CVs,Step,_,-1,_)
   ; quantified_formula(Formula)
     -> unfold_quantified_formula(Formula,Formula1,Acc,CVs,[],Step)
   ; ( Formula = (T: _)
       -> Step1 = T
     ; Step1 = Step ),
     functor(Formula,F,N),
     functor(Formula1,F,N),
     decode_nested_constants_arg(Formula,1,N,Formula1,Acc,CVs,Step1) ).

decode_nested_constants_arg(_Schema,M,N,_Schema1,Acc,Acc,_Step) :-
   M > N, 
   !.
decode_nested_constants_arg(Schema,M,N,Schema1,Acc,CVs,Step) :-
   arg(M,Schema,A),
   decode_nested_constants(A,B,Acc,Acc1,Step),
   arg(M,Schema1,B),
   M1 is M+1,
   decode_nested_constants_arg(Schema,M1,N,Schema1,Acc1,CVs,Step).

quantified_formula([\/ _Q | _F]).
quantified_formula([/\ _Q | _F]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unfold_nested_constants(+C,-C1,+Acc,-CVs,+Step,?F0,+N0,?Arity0)
%   C: constants or expressions to unfold
%   C1: constant whose nested constants are replaced with variables
%   Acc: accumulation of CVs
%   CVs: a list which contains (T: C eq V) where C is a nested constant
%   Step: time stamp for elements in CVs
%   F0,N0,Arity0 : functor,current argument index and arity of a 
%                  constant that contains this constant C as an argument
%                  used to check whether C should be replaced with a variable.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% exception to unfold nested constants: @< and == are used to 
%   to compare constants themselves, so arguments shouldn't be unfolded
unfold_nested_constants((A@<B),(A@<B),Acc,Acc,_Step,_,_,_) :- !.
unfold_nested_constants((A==B),(A==B),Acc,Acc,_Step,_,_,_) :- !.

% if K = -1, outermost functor
unfold_nested_constants((T:C),D,Acc,CVs,Step,F0,N0,Arity0) :-
% D may be C1 or V1 depending on whether C itself should be replaced 
%  with a variable
   !,
   functor(C,F,N),
   functor(C1,F,N), 
   unfold_nested_constants_arg(C,C1,Acc,Acc1,Step,F,1,N), 
   ( (\+ is_constant(C) ; skip_unfolding(F0,N0,Arity0))
     -> D=(T:C1),
        CVs=Acc1
   ; ( member_check((T: eq(C1,V1)),Acc1)  
                          % if the binding is already in the list
       -> D=V1,
          CVs = Acc1
     ; D=V1,
       get_sort(C1,Sort),
       get_internal_var(Sort,V1),
       append(Acc1,[(T: eq(C1,V1))],CVs) ) ).

unfold_nested_constants(C,D,Acc,CVs,Step,F0,N0,Arity0) :-
% D may be C1 or V1
   !,
   functor(C,F,N),
   functor(C1,F,N), 
   unfold_nested_constants_arg(C,C1,Acc,Acc1,Step,F,1,N), 
   ( (\+ is_constant(C); skip_unfolding(F0,N0,Arity0))
     -> D=C1,
        CVs=Acc1
   ;   ( member_check((Step: eq(C1,V1)),Acc1)  % problem?
                          % if the binding is already in the list
        -> D=V1,
           CVs = Acc1
      ; D=V1,
        get_sort(C1,Sort),
        get_internal_var(Sort,V1),
        append(Acc1,[(Step: eq(C1,V1))],CVs) ) ).


unfold_nested_constants_arg(_C,_C2,Acc,Acc,_Step,_F,M,N) :-
   M > N,
   !.
unfold_nested_constants_arg(C,C2,Acc,CVs,Step,F,M,N) :-
   arg(M,C,A),
   unfold_nested_constants(A,A2,Acc,Acc1,Step,F,M,N),
   arg(M,C2,A2),
   M1 is M+1,
   unfold_nested_constants_arg(C,C2,Acc1,CVs,Step,F,M1,N),
   !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% skip_unfolding(+Functor,+N,+Arity): check the condition
%   whether a constant should be replaced with a variable.
%   1. if a constant is the outermost one
%   2. if a constant of composite sort is expected as an argument
%      (e.g., contribution(a,ac))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
skip_unfolding(_,-1,_) :- !.   % condition 1
skip_unfolding(F,N,Arity) :-   % condition 2
   !, 
   functor(P,F,Arity), 
   domain_schema(_,L),
   member(P,L),
   arg(N,P,K), 
   composite_sort(K). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unfold_quantified_formula(+QSubF,-F1,+Acc,-CVs,+AccQ,+Step)
%   QSubF: quantified formula
%   F    : new formula whose nested constants are unfolded
%   Acc  : accumulation of CVs
%   CVs  : a list of (T: C eq V) where C is a nested constant
%          V is universally quantified
%   AccQ : accumulation of quantifiers
%   Step : time stamp for elements in CVs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unfold_quantified_formula([\/Q|SubF],Formula1,Acc,CVs,AccQ,Step) :-
   append(AccQ,[Q],AccQ1),
   ( quantified_formula(SubF)
     -> unfold_quantified_formula(SubF,SubF1,Acc,CVs,AccQ1,Step),
        Formula1 = [\/Q | SubF1] 
   ; decode_nested_constants(SubF,SubF1,[],CVs0,0),
     separate_cvpairs(CVs0,AccQ1,QCVs,UCVs),
     make_quantified_formula(SubF1,QCVs,SubF2),
     append(Acc,UCVs,CVs),
     Formula1 = [\/Q|SubF2] ).

unfold_quantified_formula([/\Q|SubF],Formula1,Acc,CVs,AccQ,Step) :-
   append(AccQ,[Q],AccQ1),
   ( quantified_formula(SubF)
     -> unfold_quantified_formula(SubF,SubF1,Acc,CVs,AccQ1,Step),
        Formula1 = [/\Q | SubF1] 
   ; decode_nested_constants(SubF,SubF1,[],CVs0,0),
     separate_cvpairs(CVs0,AccQ1,QCVs,UCVs),
     make_quantified_formula(SubF1,QCVs,SubF2),
     append(Acc,UCVs,CVs),
     Formula1 = [/\Q|SubF2] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% separate_cvpairs(+CVs,+AccQ,-QCVs,-UCVs)
%%   CVs : a list of (T: C eq V)
%%   AccQ : quantified variables
%%   QCVs : subset of CVS where C contains a variable in AccQ 
%%          (or quantified)
%%   UCVs : subset of CVS where C does not contain a variable 
%%          in AccQ (or unquantified)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
separate_cvpairs(CVs,AccQ,QCVs,UCVs) :-
   find_unquantified_cvpairs(CVs,AccQ,UCVs),
   subtract(CVs,UCVs,QCVs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_unquantified_cvpairs(+CVs,+AccQ,-UCVs)
%%   CVs : a list of (T: C eq V)
%%   AccQ : quantified variables
%%   UCVs : subset of CVS where C does not contain a variable
%%          in AccQ (or unquantified)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_unquantified_cvpairs(CVs,[Q|Qs],UCVs) :-
   find_cvpairs_containing_quantifier(Q,CVs,QCVs), 
   subtract(CVs,QCVs,CVs1), 
   find_unquantified_cvpairs(CVs1,Qs,UCVs). 
find_unquantified_cvpairs(UCVs,[],UCVs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_cvpairs_containing_quantifier(+Q,+CVs,-QCVs)
%%   Q: quantified variable
%%   CVs: a list of (T: C eq V)
%%   QCVs: subset of CVs where C contains a variable Q
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_cvpairs_containing_quantifier(Q,CVs,QCVs) :-
   find_cvpairs_containing_quantifier(Q,CVs,[],QCVs).

find_cvpairs_containing_quantifier(Q,[(T: C eq V)|CVs],Acc,QCVs) :-
   ( constant_has_quantified_variable(C,Q)
     -> append(Acc,[(T: C eq V)],Acc1)
   ; Acc1 = Acc ),
   find_cvpairs_containing_quantifier(Q,CVs,Acc1,QCVs).

find_cvpairs_containing_quantifier(_Q,[],QCVs,QCVs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% constant_has_quantified_variable(C,Va) : check whether
%%     a variable appears as arguments of C
%%   C : constant (from (T: C eq V) in CVs )
%%   Va: variable (from quantified variable list)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
constant_has_quantified_variable(C,Va) :-
   C =.. [_F | Args], 
   member_check(Va,Args). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% make_quantified_formula(+Formula,+QCVs,-Formula2)
%%    Formula : input formula
%%    QCVs : a list of (T: C eq V) where C contains a 
%%       quantified variable
%%    Formula2 : variables V in (T: C eq V) is quantified 
%%    ex) With [\/N | c(d(N))]
%%        Call: make_quantified_formula((0:c('V_boolean-20')),
%%                 [(0:d('N')eq 'V_boolean-20')],_18102) ? s
%%        Exit: make_quantified_formula((0:c('V_boolean-20')),
%%                 [(0:d('N')eq 'V_boolean-20')],
%%                 [\/'V_boolean-20'|(0:c('V_boolean-20'))
%%                                    &&(0:d('N')eq 'V_boolean-20')]) ? 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_quantified_formula(Formula,[(T: C eq V)|CVs],Formula2) :-
   make_quantified_formula(Formula,CVs,Formula1),
   Formula2 = [\/ V | Formula1 && (T: C eq V)].
make_quantified_formula(Formula,[],Formula).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% used in macro expansion 
%^ by additive fluent in 'by' clause
%^ doesn't work for now
decode_by(A,Val,CVs,T) :- trace, decode_by_1(A,Val,CVs,T).
decode_by_1(A,Val,CVs,T) :-
   ( is_constant(A) 
     -> unfold_nested_constants_inside(A,Val,[],CVList,T)
   ; (expr(A); is_constant(A) ; test(A))
     -> unfold_nested_constants(A,Val,[],CVList,T) ),
   replace_comma_with_and_1(CVList,CVs). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_nested_constants_in_fact(Formula,Formula1)
%%   : decode nested constants in fact in query.
%%     Differently from a rule, CVs should be explicitly
%%     quantified by universal quantifiers
%%    Formula :  
%%    Formula1: 
%%    ex) 
%%         :- query
%%         0:  c(d(N,N1))
%%       -> Call: decode_nested_constants_in_fact((0:c(d('N','N1'))),_2425) ? s
%%          Exit: decode_nested_constants_in_fact((0:c(d('N','N1'))),[/\'V_boolean-11'|(0:d('N','N1')eq 'V_boolean-11')->>(0:c('V_boolean-11'))]) ? 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_nested_constants_in_fact(Formula,Formula1) :-
   decode_nested_constants(Formula,Formula2,CVs), 
%   make_univ_quantified_formula_from_cvpairs(Formula2,CVs,Formula1).
  make_exist_quantified_formula_from_cvpairs(Formula2,CVs,Formula1).

make_univ_quantified_formula_from_cvpairs(Formula,CVs,Formula1) :-
   findall(V,member((_T: _C eq V),CVs),Vs), 
               %  collect variables to be universally quantified
   ( Vs == []
     -> Formula1 = Formula
   ; list_to_tuple(CVs,CVsTuple),
     Formula2 = (CVsTuple ->> Formula),
     wrap_univ_quantifiers(Vs,Formula2,Formula1) ).

wrap_univ_quantifiers([Q|Qs],Formula,Formula1) :-
   Formula2 = [/\Q| Formula],
   wrap_univ_quantifiers(Qs,Formula2,Formula1).
wrap_univ_quantifiers([],Formula,Formula).

make_exist_quantified_formula_from_cvpairs(Formula,CVs,Formula1) :-
   findall(V,member((_T: _C eq V),CVs),Vs), 
               %  collect variables to be universally quantified
   ( Vs == []
     -> Formula1 = Formula
   ; %list_to_tuple(CVs,CVsTuple),
     conjoin_list(CVs,CVsTuple),
     Formula2 = (CVsTuple & Formula),
     wrap_exist_quantifiers(Vs,Formula2,Formula1) ).

wrap_exist_quantifiers([Q|Qs],Formula,Formula1) :-
   Formula2 = [\/Q| Formula],
   wrap_exist_quantifiers(Qs,Formula2,Formula1).
wrap_exist_quantifiers([],Formula,Formula).

list_to_conjuncts([L|Ls],Cs) :-
   list_to_conjuncts(Ls,Cs1),
   Cs = (L && Cs1).
list_to_conjuncts([],true).   

decode_nested_constants_for_condition(Formula,Formula1,CVs) :-
   ( has_no_constants(Formula)
     -> true
   ; fatal_error("constants cannot be used in where clause: ~w", Formula) ),
   decode_nested_constants(Formula,Formula1,CVs,_,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_query_rule_schemas :-
   iset(query_rule_count,0),
   query_rule_schema(Schema),

   ( Schema = (Schema1 where Condition)
     -> adjust_where(Condition,Condition1) 
   ; Schema = Schema1,
     Condition1 = true ),

 % postprocessing before binding
   decode_macro(Schema1,Schema2),       
   decode_nested_constants_in_rule(Schema2,Schema3,CVs),
   expand_boolean(Schema3,Schema4),     
   remove_time_stamps(Schema4,Schema5),
   find_free_variables(Schema5,Vs1),

   ( Condition1 \== true 
     -> decode_macro(Condition1,Condition2),
        decode_nested_constants_for_condition(Condition2,Condition3,CVs),
        expand_boolean(Condition3,Condition4),
        remove_time_stamps(Condition4,Condition5),
        find_free_variables(Condition5,Vs2)
     ; Condition5 = true, 
       Vs2 = [] ),

   append(Vs1,Vs2,Vs3),
   remove_duplicates(Vs3,Vs),
   renaming_subst(Vs,Sub),
   subst_free(Schema5,Schema6,Sub),
   subst_free(Condition5,Condition6,Sub),

   % at this point H0<=B0 is a rule schema whose variable is 
   % in the form of prolog, and Sub is a pair like, ['B'=_297545,'L'=_297906]
   bind_vars(Sub),  % loop starts here - until every free variable is 
                    %   replaced with ground instances of corresponding sort
   ( (eval_expr(Condition6,Condition7), call(Condition7))
     -> true 
   ; fail ),
   
   do_term_expansion(Schema6,Schema61),   %^ for maxstep expansion 
                                          %^  maxstep: true -> 3: true.
   postprocessing_after_binding(Schema61,(H<=B)),

   negation_normal_form(B,B1),
   ( B1 = false -> fail ; true ),
   negation_normal_form(H,H1),

   nnf_to_cnf(H1,Hss,Aux1),
   nnf_to_dnf(B1,Bss,Aux2),
   store_query_rule_schemas(Hss,Bss),

   ( ( Hss == []; Bss == [] ) 
       -> true  
     ; store_aux_query_schemas(Aux1),
       store_aux_query_schemas(Aux2) ),
   fail.
process_query_rule_schemas.


store_query_rule_schemas(Hss,Bss) :-
   member(Ms,Hss),
   member(Ns,Bss),
   incr(query_rule_count,I),
   db_open_query_external(I),
   db_store_query_rule(I,query_rule_body(I,Ns)),
   ( Ms = [] -> Ms1 = [0]; Ms1 = Ms ),
   ( Ms1 = [D]
     -> db_store_query_rule(I,(D<-I))
   ; fatal_error("One of the rules is not definite.",[])),
   fail.  
store_query_rule_schemas(_,_).

store_aux_query_schemas(Aux) :-
   member(NC,Aux),
   store_query_rule_schemas([[]],NC),
   fail.
store_aux_query_schemas(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bind_vars/2 is used to bind variables with appropriate constants. 
% Here is a trace showing how bind_vars/2 works:
%+      1      1 Call: bind_vars(['V_F'=_36123]) ? 
%        2      2 Call: var('V_F',_36581) ? 
%        2      2 Exit: var('V_F',fluent) ? 
%        3      2 Call: domain(fluent,_37266) ? 
%        3      2 Exit: domain(fluent,[alive,loaded(g1),loaded(g2)]) ? 
% +      4      2 Call: bind_vars([]) ? 
% +      4      2 Exit: bind_vars([]) ? 
%?+      1      1 Exit: bind_vars(['V_F'=alive]) ?

%^jo- should this routine be able to handle var(sort(#1))?
bind_vars([=(Name,Var)|Bs]) :- 
   ( var_sort(Name,Sort)
     -> ( Sort == computed
          -> true
        ; domain(Sort,Es)
           -> member(Var,Es) 
        ; fatal_error("Unknown sort (~w).",[Sort]) )
   ; fatal_error("Unknown variable (~w).",[Name]) ),
   bind_vars(Bs).
bind_vars([]).


% Stage is either 'domain' or 'query', depending on whether it's the domain
% description or the query that's being completed

generate_completion_clauses(Stage) :-
   value(mode,Mode),
   generate_completion_clauses(Stage,Mode).

generate_completion_clauses(Stage,Mode) :-
   % Generates all of the completion clauses for the theory.  First, it
   % finds each rule of the form (false <= Body), and adds the negation of
   % Body as a clause.  Then, for each atom A in the theory, it collects all
   % of the rules of the form (A <= Body), and adds clauses to set A
   % equivalent to the disjunction of every such Body.  The same is then
   % done for -A.

   % In transition mode, some of these clauses will need to be shifted during
   % query processing.  Thus, completion must be performed in such a way as
   % to make clear which clauses should be shifted and which should not.
   % Completion for transition mode will therefore be performed in two steps.
   % First, in Phase 0, all of the rules corresponding to static laws at time 0
   % will be completed; then all of the rules corresponding to static laws at
   % time 1 and dynamic laws will be completed.  (Clauses resulting from the
   % former will not be shifted to later time steps; clauses from the latter
   % will.)  At the end of the procedure, the variable clause_count_0
   % will store the number of clauses which will not be shifted; clause_count
   % will store the total number of clauses;
   % aux_atom_count_from_completion_0 will store the number of new
   % atoms introduced for static laws at time 0 during completion; and
   % aux_atom_count_from_completion will store the total number of new
   % atoms introduced during completion.

   ( Stage == domain
     -> iset(clause_count_0,0),
	iset(clause_count,0)
   ; iset(query_clause_count,0) ),

   ( Stage == domain, Mode == transition
     ->	value(atom_count_0,AC0),
	value(atom_count,AC1),
        value(rule_count_0,RC0),
	( Phase = 0
	; write_clauses_for_eq([0],Stage),
	  value(clause_count,CC0),
	  iset(clause_count_0,CC0),
	  value(atom_count,AC2),
	  AACC0 is AC2 - AC1,
	  iset(aux_atom_count_from_completion_0,AACC0),
	  Phase = 1 )
   ; % if maxstep is 0 when processing a transition mode query, generate
     % completion only for phase 0 atoms
     Mode == transition, macro(maxstep,true,0)
     -> value(atom_count_0,AC0),
	value(rule_count_0,RC0),
	Phase = 0
   ; true ),

   % For each rule with false in the head, add its negated body as a clause
   ( ( Stage == domain
       -> db_fetch_rule((0<=L)),
	  ( Phase == 0 -> L =< RC0
	  ; Phase == 1 -> L > RC0
	  ; true ),
          db_fetch_rule(rule_body(L,Ns))
     ; db_fetch_query_rule((0<-L)),
       ( Phase == 0 -> L =< RC0
       ; Phase == 1 -> L > RC0
       ; true ),
       db_fetch_query_rule(query_rule_body(L,Ns)) ),
     negate_lits(Ns,Cs),
     store_clause(Cs,Stage,Mode,Phase),
     fail

   % Generate the completion of each atom.  In Phase 0, do so only for rigid
   % constants and fluents at time 0 (those atoms with an index <=
   % static0_atom_count) and new atoms generated for static laws at
   % time 0 (those atoms with index > original_atom_count, but <= this plus
   % static0_new_atom_count_from_rules).  In Phase 1, do so for the remaining
   % atoms.  If not in transition mode (i.e. neither phase 0 nor 1), don't
   % worry about it and just do every atom.

   ; (atom_integer((_Step: Constant eq V),N); atom_integer((Constant eq V),N)),

     % *jc* need to do this more efficiently
     ( Phase == 0 -> N =< AC0
     ; Phase == 1 -> N > AC0
     ; true ),
     ( singleton_domain_constant(Constant)
       -> fail
     ; true),
     C is 0-N,

       % store clauses corresponding to right-to-left implications

     ( ( ( Stage == domain 
           -> db_fetch_rule((N<=L)),
              db_fetch_rule(rule_body(L,Ms))
          ; db_fetch_query_rule((N<-L)),   
            db_fetch_query_rule(query_rule_body(L,Ms)) ),
          negate_lits(Ms,Cs),
          store_clause([N|Cs],Stage,Mode,Phase),
          fail

       % store clauses corresponding to left-to-right implications

       ; % collect all the rule bodies with N as the head into a DNF formula
	 ( Stage == domain 
           -> rule_bodies_for_literal(N,Nss)
	 ; query_rule_bodies_for_literal(N,Nss) ),
	 % convert this to CNF, possibly introducing auxiliary atoms
	 distribute_or_over_and(Nss,NewNss,Aux),
         ( member(Ns,NewNss),
           store_clause([C|Ns],Stage,Mode,Phase)
	 ; member(Ns,Aux),
	   store_clause(Ns,Stage,Mode,Phase) ),
         fail )

       % if the current atom is a boolean fluent, generate completion clauses
       % for its negation too.  (If it's nonboolean, this isn't necessary,
       % since there is a separate atom corresponding to each value of the
       % fluent.)

       ; ( ( atom_integer((_Constant eq true),N) ; 
             atom_integer((_Step: _Constant eq true),N) )
             -> ( ( Stage == domain 
                    -> db_fetch_rule((C<=L)),
                       db_fetch_rule(rule_body(L,Ms))
                  ; db_fetch_query_rule((C<-L)),
                    db_fetch_query_rule(query_rule_body(L,Ms)) ),
                  negate_lits(Ms,Cs),
                  store_clause([C|Cs],Stage,Mode,Phase),
                  fail
                ; ( Stage == domain 
                    -> rule_bodies_for_literal(C,Nss)
                  ; query_rule_bodies_for_literal(C,Nss)),
		  distribute_or_over_and(Nss,NewNss,Aux),
                  ( member(Ns,NewNss),
                    store_clause([N|Ns],Stage,Mode,Phase)
		  ; member(Ns,Aux),
		    store_clause(Ns,Stage,Mode,Phase) ),
                  fail )
           ; fail ))).

generate_completion_clauses(domain,Mode) :-
   ( Mode == transition
     -> write_clauses_for_eq([1],domain)
   ; write_clauses_for_eq(domain) ),
   value(atom_count,AC),
   iset(domain_atom_count,AC),
   value(original_atom_count,OAC),
   AAC is AC - OAC,
   iset(aux_atom_count,AAC),
   value(aux_atom_count_from_rules,AACFR),
   AACFC is AAC - AACFR,
   iset(aux_atom_count_from_completion,AACFC).

generate_completion_clauses(query,_Mode).

singleton_domain_constant(Constant) :-
   domain(D,Ds), 
   member(Constant,Ds),
   arg(1,D,Sort),
   domain(Sort,Ss),
   length(Ss,1).


%%%%

number_of_clauses([Ns|Nss],Num) :-
   length(Ns,Num1),
   number_of_clauses(Nss,Num2),
   Num is Num1 * Num2.

number_of_clauses([],1).


number_of_clauses(Nss,I,K) :-
        value(max_no_of_clauses_to_optimize,MaxNoOfClauses),
	I > MaxNoOfClauses, 
	!,
	( member([],Nss) -> K = 0 ; K = I ).

number_of_clauses([],K,K).

number_of_clauses([Ns|Nss],I,K) :-
	length(Ns,J),
	I1 is I * J,
	number_of_clauses(Nss,I1,K).


size_of_clauses([Ns|Nss],S) :-
   !,
   size_of_clauses(Ns,S1),
   size_of_clauses(Nss,S2),
   S is S1 + S2.

size_of_clauses([],0) :- !.

size_of_clauses(_A,1).


% store_clauses asserts clauses using the appropriate predicate.
% Stage is 'domain' or 'query'; Mode is 'transition', 'basic', or 'history';
% Phase is 0 or 1 (since in transition mode clauses for time 0 and clauses
% for time 1 are asserted using separate predicates, since only the latter
% are shifted to later time units).

store_clauses([Ns|Nss],Stage,Mode,Phase) :-
   store_clause(Ns,Stage,Mode,Phase),
   store_clauses(Nss,Stage,Mode,Phase).


store_clause(Ns,Stage,Mode,Phase) :-
   sort(Ns,Cs),
   ( tautology(Cs), value(eliminate_tautologies,true)
     -> true
   ; ( Stage == domain
       -> incr(clause_count,_),
	  ( Mode == transition, Phase == 1
	    -> assertz(clause1(Cs))
	  ; assertz(clause0(Cs)) )
     ; incr(query_clause_count,_),
       assertz(query_clause(Cs)) )).


tautology([N|_Ns]) :-
        N>0, !, fail.
tautology([N|Ns]) :-
        M is (0-N), 
        ( ord_member(M,Ns)
            -> true 
        ; tautology(Ns)  ).

member_of_each([N|Rs],[Ns|Nss]) :-
        member(N,Ns), member_of_each(Rs,Nss).
member_of_each([],[]).


member_of_each_append(R,[Ns|Nss]) :-
   member(N,Ns),
   member_of_each_append(Rs,Nss),
   append(N,Rs,R).
member_of_each_append([],[]).


negate_lits([[N|Ns]|Nss],[[C|Cs]|Css]) :-
   C is -N,
   negate_lits(Ns,Cs),
   negate_lits(Nss,Css).

negate_lits([N|Ns],[C|Cs]) :-
   C is -N,
   negate_lits(Ns,Cs).

negate_lits([],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% write_clauses_for_eq 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%^jo- domain check > 1 here
% boolean only - apply to other domains with cardinality 2.

% write_clauses_for_eq can be applied to specific time steps by passing a
% list of steps.  (This is necessary for transition mode, in which clauses
% for time steps 0 and 1 must be generated at different times to avoid
% problems with shifting.)  If write_clauses_for_eq/0 is called, it simply
% passes the value of the entire step domain to write_clauses_for_eq/1.
% Stage is either 'domain' or 'query'.  This parameter determines what should
% be done with the clauses generated if CCalc is in transition mode -- assert
% the clauses in the database if 'domain', or write the clauses directly if
% 'query'.

write_clauses_for_eq(Stage) :-
   write_clauses_for_eq([],Stage).

write_clauses_for_eq(Steps,Stage) :-
   findall(L, (is_sort(simpleFluent(L)), L \== boolean, 
               domain(simpleFluent(L),D), D \== []), Ls),
          % find fluent(L) whose value sort L has more than 
          % two objects in its domain
   ( Ls \== [] 
     -> write_clauses_for_eq(simpleFluent,Ls,Steps,Stage)
   ; true),

   findall(L1, (is_sort(sdFluent(L1)), L1 \== boolean, 
                domain(sdFluent(L1),D1), D1 \== []), Ls1),
   ( Ls1 \== [] 
     -> write_clauses_for_eq(sdFluent,Ls1,Steps,Stage)
   ; true),

   findall(L2, (is_sort(action(L2)), L2 \== boolean, 
                domain(action(L2),D2), D2 \== []), Ls2),
   ( Ls2 \== [] 
     -> write_clauses_for_eq(action,Ls2,Steps,Stage)
   ; true),

   findall(L3, (is_sort(attribute(L3)), 
                domain(attribute(L3),D3), D3 \== []), Ls3), 
   ( Ls3 \== [] 
     -> write_clauses_for_eq(attribute,Ls3,Steps,Stage)
   ; true),
   findall(L4, (is_sort(rigid(L4)), L4 \== boolean, 
                domain(rigid(L4),D4), D4 \== []), Ls4),
   % write clauses for rigid constants only when processing time 0
   ( (Ls4 \== [], (Steps=[] ; member(0,Steps)))
     -> write_clauses_for_eq(rigid,Ls4,[0],Stage)
   ; true).

write_clauses_for_eq(_Composite,[],_Steps,_Stage).

write_clauses_for_eq(Composite,[L|Ls],Steps,Stage) :-
   write_exist_clauses_for_eq(Composite,L,Steps,Stage),
   write_uniq_clauses_for_eq(Composite,L,Steps,Stage),
   write_clauses_for_eq(Composite,Ls,Steps,Stage).


% TermTree3 = col(0)eq 1++col(0)eq 2++col(0)eq 3

write_exist_clauses_for_eq(Composite,L,Steps,Stage) :-
   CompositeSort =.. [Composite,L],

   % if Steps has any values (it will be [0] or [1] for transition mode,
   % or the list of all steps in history mode), choose each step from the list
   % in succession and add clauses for that step.  If Steps is [], just use
   % variables instead of specific steps.  For fluents, all time steps will
   % be processed.  For actions, all times but the last will be processed,
   % and the clauses for actions at time N are added at the same time as the
   % clauses for fluents at time N+1 (for shifting purposes).  Rigid constants
   % are processed only at time 0 since they are never shifted.

   ( (Composite = simpleFluent; Composite = sdFluent)
     -> ( Steps == []
	  -> Step = var(step,-1)
	; member(Phase,Steps),
	  Step = Phase )
   ; (Composite = action; Composite = attribute)
     -> ( Steps == []
          -> Step = var(astep,-1)
        ; member(Phase,Steps),
          Phase > 0,
          Step is Phase - 1 )
   ; Composite = rigid
     -> Step = 0 ),

   ( Composite = attribute 
     -> L2 = (L*)
   ; L2 = L),

   Term = ([\/var(L2,-1) | 
                 (Step: var(CompositeSort,-1) eq var(L2,-1))]),

   decode_macro(Term,TermTree),
   do_term_expansion(TermTree,TermTree11),
   remove_time_stamps(TermTree11,TermTree1), %temp %remove_time_for_rigid

   find_free_variables(TermTree1,Vs),
   renaming_subst(Vs,Sub),
   subst_free(TermTree1,TermTree2,Sub),
   bind_vars(Sub),
   
   negation_normal_form(TermTree2,TermTree3),
   write_clause2(TermTree3,Phase,Stage),
   fail.

write_exist_clauses_for_eq(_Composite,_L,_Steps,_Stage).

write_uniq_clauses_for_eq(Composite,L,Steps,Stage) :-
   value(mode,Mode),
   CompositeSort =.. [Composite,L],

   % if Steps has any values (it will be [0] or [1] for transition mode,
   % or the list of all steps in history mode), choose each step from the list
   % in succession and add clauses for that step.  If Steps is [], just use
   % variables instead of specific steps.  For fluents, all time steps will
   % be processed.  For actions, all times but the last will be processed,
   % and the clauses for actions at time N are added at the same time as the
   % clauses for fluents at time N+1 (for shifting purposes).  Rigid constants
   % are processed only at time 0 since they are never shifted.

   ( (Composite = simpleFluent; Composite = sdFluent)
     -> ( Steps == []
          -> domain(step,StepDom)
        ; StepDom = Steps ),
        member(Step,StepDom),
        Phase = Step
   ; (Composite = action; Composite = attribute)
     -> ( Steps == []
          -> domain(astep,StepDom),
             member(Step,StepDom),
             Phase = Step
        ; member(Phase,Steps),
          Phase > 0,
          Step is Phase - 1 )
   ; Composite = rigid
     -> Step = 0 ),

   ( Composite = attribute
     -> L2 = (L*)
   ; L2 = L ),

   domain(CompositeSort,CDom),
   domain(L2,LDom),
   length(LDom,Len),

   ( Len < 5
     -> % if the domain of the fluent has fewer than 5 elements, it's more
	% economical to add -P ++ -Q for each pair of elements P and Q

	member(Const,CDom),
	suffix([Val1|Vals],LDom),
	( Composite == rigid
	  -> find_atom_integer((Const eq Val1),Int1)
	; find_atom_integer((Step: Const eq Val1),Int1) ),
	Neg1 is -Int1, 
	member(Val2,Vals),
	( Composite == rigid
	  -> find_atom_integer((Const eq Val2),Int2)
	; find_atom_integer((Step: Const eq Val2),Int2) ),
	Neg2 is -Int2,
	store_clause([Neg1,Neg2],Stage,Mode,Phase)

   ; % if the domain has 5 or more elements, it's more economical to include
     % uniqueness clauses equivalent to
     %
     %    V_0 <-> A_0
     %	  A_i -> -V_(i-1)         for i > 0
     %    V_i <-> V_(i-1) ++ A_i  for i > 0
     %
     % where the V_i are the elements of the domain, and the A_i are new
     % auxiliary atoms

     member(Const,CDom),
     value(atom_count,FirstA),

     LDom = [FirstVal|RestDom],
     ( Composite == rigid
       -> find_atom_integer((Const eq FirstVal),FirstConstA)
     ; find_atom_integer((Step: Const eq FirstVal),FirstConstA) ),
     FirstConstN is -FirstConstA,

     member(Val,RestDom),
     ( Composite == rigid
       -> find_atom_integer((Const eq Val),ConstA)
     ; find_atom_integer((Step: Const eq Val),ConstA) ),
     ConstN is -ConstA,

     value(atom_count,LastA),
     LastN is -LastA,
     incr(atom_count,NewA),
     NewN is -NewA,

     ( LastA == FirstA
       -> ( Clause = [FirstConstN,ConstN]
	  ; Clause = [NewN,FirstConstA,ConstA]
	  ; Clause = [NewA,FirstConstN]
	  ; Clause = [NewA,ConstN] ),
	  store_clause(Clause,Stage,Mode,Phase)
     ; ( Clause = [NewN,LastA,ConstA]
       ; Clause = [NewA,LastN]
       ; Clause = [NewA,ConstN]
       ; Clause = [LastN,ConstN] ),
       store_clause(Clause,Stage,Mode,Phase) )),

/*   decode_macro(Term,TermTree),
   do_term_expansion(TermTree,TermTree11),
   remove_time_stamps(TermTree11,TermTree1), %temp

   find_free_variables(TermTree1,Vs),
   renaming_subst(Vs,Sub),
   subst_free(TermTree1,TermTree2,Sub),
   bind_vars(Sub),

   negation_normal_form(TermTree2,TermTree3),

   ( TermTree3 = false 
     -> true
   ; write_clause2(TermTree3,Phase,Stage) ),
*/

   fail.
 
write_uniq_clauses_for_eq(_Composite,_L,_Steps,_Stage).

%   Term = r TermTree3 = col(0)eq 1++col(0)eq 2++col(0)eq 3

write_clause2(Term,Phase,Stage) :- 
   write_clause2_aux(Term,Clause), 
   value(mode,Mode),
   store_clause(Clause,Stage,Mode,Phase).

write_clause2_aux(A++B,[I|Is]) :- 
   ( A = -C
     -> atom_integer(C,I1),
        I is -I1
   ; atom_integer(A,I)),
   write_clause2_aux(B,Is).

write_clause2_aux(A,[I]) :- 
   ( A = -C
     -> atom_integer(C,I1),
        I is -I1
   ; atom_integer(A,I)).

%---------------------------------------------------------------------

%^jo? is_hashed? 
find_atom_integer(A,N) :- 
   atom_integer(A,N), 
   !.

lowercase([],[]).
lowercase([C|Cs],[L|Ls]) :-
	( C >= 65, C =< 90 ->
	   L is C + 32
	; L = C ),
	lowercase(Cs,Ls).
lowercase(C1-C2,L1-L2) :-
	% This is a workaround to handle the dash in satz-rand, which
	% confuses name/2 since atoms can't have dashes
	lowercase(C1,L1),
	lowercase(C2,L2).
lowercase(C,L) :-
	name(C,CName),
	lowercase(CName,LName),
	name(L,LName),
	!.
lowercase(C,C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set(Option,Value) : sets various user options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set(solver,Value) :-
   !,
   lowercase(Value,Value2),
   % check whether choice of solver is a valid choice, and whether current
   % number of interpretations is appropriate for that solver
   ( value(num,NOI)
     -> true
   ; NOI = 1 ),
   ( member(Value2,[relsat,grasp,mchaff])
     -> NumValue = NOI
   ; member(Value2,[sato,sato_old,walksat])
     -> ( NOI == all
          -> format("~nNote: The current value of num is 'all', but for ",[]),
	     format("~w, num must be a~npositive integer.  num ",[Value2]),
             format("will be set to 1.~n",[]),
	     NumValue = 1
	; NumValue = NOI )
   ; member(Value2,[relsat_old,satz,satz-rand,zchaff])
     -> ( NOI == 1
          -> true
        ; format("~nNote: The current value of num is \'~w\', but ",[NOI]),
	  format("~w cannot return multiple~nsolutions.  num will ",[Value2]),
	  format("be set to 1.~n",[]) ),
	NumValue = 1
   ; format("~nError: ~w is not a valid solver choice.~n",[Value]),
     format("Please see 'help(solver)' for a list of valid choices.~n",[]),
     fail ),
   iset(solver,Value2),
   ( value(num,NOI)
     -> % set 'num' to its current value so that any warnings for this
        % solver/num combination are displayed
        set(num,NumValue)
   ; true ).

set(num,Value) :-
   % restricts the value of num to a valid choice for the current SAT solver.
   % Some solvers can return only a single interpretation, others can return N
   % interpretations for any integer N, and some can return 'all'
   % interpretations.
   value(solver,Solver),
   !,
   ( member(Solver,[relsat,grasp,mchaff])
     -> ( Value == all
	  -> SetValue = Value
	; integer(Value), Value > 0
	  -> SetValue = Value
	; format("~nError: The value of num must be 'all' or a positive ",[]),
	  format("integer for solver ~w.~nDefaulting to 1.~n",[Solver]),
	  SetValue = 1 )
   ; member(Solver,[sato,sato_old,walksat])
     -> ( ( \+ integer(Value) ; Value =< 0 )
          -> format("~nError: The value of num must be a positive ",[]),
	     format("integer for solver ~w.~nDefaulting to 1.~n",[Solver]),
	     SetValue = 1
	; SetValue = Value,
	  ( value(solver,walksat)
	    -> format("~nWarning: walksat is stochastic and may report ",[]),
	       format("that a satisfiable problem is~nunsatisfiable, or ",[]),
	       format("may return fewer solutions than the number ",[]),
	       format("requested, even~nwhen that many exist.~n",[])
	  ; Value > 1
	    -> format("~nWarning: ~w may return fewer than the ",[Solver]),
	       format("requested number of solutions, even~nwhen that ",[]),
	       format("many exist, but will always find at least one ",[]),
	       format("solution if the problem~nis satisfiable.~n",[])
          ; true ))
   ; Value == 1
     -> SetValue = Value
   ; integer(Value)
     -> format("~nError: Solver ~w cannot return multiple ",[Solver]),
        format("interpretations.  num will be~nset to 1.~n",[]),
        SetValue = 1
   ; format("~nError: num must be a positive integer or 'all'.",[]),
     format("Defaulting to 1.~n",[]),
     SetValue = 1 ),
   iset(num,SetValue),
   ( value(eliminate_tautologies,false), SetValue \= 1
     -> format("~nWarning: when 'num' is set for multiple interpretations ",[]),
	format("and~neliminate_tautologies is set to false, it is possible",[]),
	format(" CCalc may generate~nmultiple copies of some ",[]),
	format("interpretations.~n",[])
   ; true ).

% If no solver is currently defined, display a warning if the new value of num
% is other than 1, but set it anyway.
set(num,Value) :-
   !,
   ( Value == 1
     -> true
   ; format("~nWarning: Not all solvers are capable of returning multiple",[]),
     format(" solutions.~n",[]) ),
   iset(num,Value).

set(dir,Value) :-
   % make sure there's a slash at the end of the path -- add one if not
   format_to_chars("~w",[Value],V),
   common_last(V,C),
   ( C == 47	% check if path ends in a slash
     -> iset(dir,Value)
   ; format_to_atom(V2,"~w/",[Value]),
     iset(dir,V2) ).

set(eliminate_tautologies,Value) :-
   member(Value,[off,no,n,false]),
   value(num,NOI),
   ( NOI \= 1
     -> format("~nWarning: when 'num' is set for multiple interpretations ",[]),
	format("and~neliminate_tautologies is set to false, it is possible",[]),
	format(" CCalc may generate~nmultiple copies of some ",[]),
	format("interpretations.~n",[])
   ; true ),
   iset(eliminate_tautologies,false).

set(sorted,_) :-
   format("~nNote: the 'sorted' option has been disabled.~n",[]),
   !.

set(Term,Value) :-
   ( Term = solver_opts(_),
     Value1 = Value
   ; member(Term,[timed,verbose,compact,optimize,eliminate_tautologies,file_io,
		  debug])
     -> ( member(Value,[on,yes,y,true,t])
	  -> Value1 = true
	; member(Value,[off,no,n,false,f,[],nil])
	  -> Value1 = false
	; format("Error: value must be 'true' or 'false' for option \'~w\'.~n",
		 [Term]),
	  fail )
   ; format("~s~w~s~s~n",["Error: ", Term, " is not a valid option.  Please ",
			   "see \'help(parameters)\'."]),
     fail ),
   iset(Term,Value1).

% internal 'set' procedure, for use within ccalc -- doesn't perform any
% error checking
iset(Term,Value) :-
   retractall(value(Term,_)),
   assertz(value(Term,Value)).


incr(Term,Value) :-
  ( retract(value(Term,N))
    -> Value is N+1, 
       assertz(value(Term,Value))
  ; Value = 1, 
    assertz(value(Term,Value)) ).

decr(Term,Value) :-
  ( retract(value(Term,N))
    -> Value is N-1, 
       assertz(value(Term,Value))
  ; Value = 1, 
    assertz(value(Term,Value)) ).

show_values :-
	value(Var,Val),
	format("~n    ~w = ~w",[Var,Val]),
	fail.
show_values.
 
% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%
% Calling the sat solvers.
% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%

% call_sat sends the input clauses to the SAT solver.  It also calls any
% optional programs being used (e.g. compact and sort).
%
% If file I/O is being used (via the command 'set(file_io,true)'), the input
% is first written to a file; this is passed first to compact (if that option
% is enabled), then to sort (also optional), and finally to the SAT solver.
% If instead pipe I/O is used ('set(file_io,false)', the default), all the
% processes are started running in the background with pipes between them, and
% then the input is written to the first pipe in the chain.
%
% The first argument to call_sat is Notify.  If Notify is not 'notify', all
% output is suppressed.  (This was formerly used to prevent unnecessary output
% when a solution was being verified.  At this time, CCalc no longer supports
% verification, but the Notify argument is still available in case it's needed
% later.)
%
% The other three arguments to call_sat are Out1, Out2, and VarTable.  These
% are output arguments which will contain the streams corresponding to the
% output files/pipes from the SAT solver.  Only mchaff uses both Out1 and Out2;
% the other solvers leave Out2 unbound.  If compact finds a solution on its
% own, Out1 will contain either "SAT" or "UNSAT" instead of a stream.
% VarTable holds the stream containing the variable table generated by compact
% if it's being used, and is unbound otherwise.

% Out1 is either "SAT" or "UNSAT" if compact solved the problem, or a stream
% containing the first output file from the SAT solver otherwise.  Out2 is a
% stream containing the second output file if the solver is mchaff, and is
% unbound otherwise.  VarTable is a stream containing the variable table from
% compact if it's used, and unbound otherwise.

call_sat(Out1,Out2,VarTable,CompactTime) :-
   call_sat(no_notify,Out1,Out2,VarTable,CompactTime).

call_sat(_,_,_,_,_) :-
   ( clause([]) ; query_clause([]) ),
   !,
   write('Theory is unsatisfiable -- it contains the empty clause.'), nl,
   fail.

call_sat(Notify,Out1,Out2,VarTable,[CompactTime,Lockfile]) :-
   common_statistics(_),
   % If file I/O is being used, write the input to a file.  Otherwise, create
   % a pipe to which CCalc will write the input later.
   ( value(file_io,true)
     -> In = 'ccsat.in',
	open(In,write,InStrm),
	write_input(Notify,InStrm),
	close(InStrm)
   ; make_pipe(In) ),

   value(solver,Solver),

   % Run compact -- if file_io is true, it will run to completion, or if
   % file_io is false, it will run in the background
   ( value(compact,true)
     -> call_compact(Notify,In,CompactAns,SolvIn,VarTable0,CompactTime,
                     Lockfile)
   ; SolvIn = In ),

   % If compact determined the satisfiability of the theory, we don't need to
   % call the SAT solver, but can simply return the answer.  If compact was
   % unable to determine whether the theory is satisfiable or not, or if we
   % aren't using compact, or if we're using pipe I/O and haven't called
   % compact yet, then we must then call the SAT solver.
   ( nonvar(CompactAns)
     -> Out1 = CompactAns,
	open(VarTable0,read,VarTable)
   ; % sort

     % If file I/O is used, run the SAT solver now.  If pipe I/O is used,
     % start the SAT solver running in the background and create pipes to/from
     % it.
     call_solver(Notify,Solver,SolvIn,SolvOut1,SolvOut2), 

     % If pipe I/O is used, start writing the input (now that all of the
     % processes are running in the background, waiting for it)
     ( value(file_io,false)
       -> open(In,write,InStrm),
	  ( value(compact,true)
	    -> open(VarTable0,read,VarTable)
	  ; true ),

	  % If mchaff is the solver, and pipe I/O is being used, open the
	  % second output pipe now.  (If it is opened earlier, CCalc blocks;
	  % if it's opened later, mChaff blocks.)
	  ( value(solver,mchaff)
	    -> open(SolvOut2,read,Out2)
	  ; true ),

	  % write the SAT solver input to the pipe
	  write_input(Notify,InStrm),
	  close(InStrm),
          rm_pipe(In),

	  % if compact is on, read its result and pass the compacted theory
	  % on to the SAT solver.  If compact found the answer on its own,
	  % return that; otherwise wait for the solver to terminate.
	  ( value(compact,true),
	    read_compact_ans(Notify,VarTable,CompactAns),
	    nonvar(CompactAns),
	    Out1 = CompactAns,
	    rm_pipe(SolvOut1),
	    ( nonvar(SolvOut2) -> rm_pipe(SolvOut2) ; true ),
	    ( CompactAns == "UNSAT" -> rm_pipe(VarTable) ; true )
	  ; Out1 = SolvOut1 )

     ; % If we're using file I/O, open the second output file for mchaff now.
       % For satz, pass the filename for the second output along instead of
       % opening it; this file is only created if the theory is satisfiable,
       % so CCalc will wait until it's sure the answer is SAT before it tried
       % to open the solution file.
       Out1 = SolvOut1,
       ( value(compact,true)
	 -> open(VarTable0,read,VarTable)
       ; true ),
       ( value(solver,mchaff)
	 -> open(SolvOut2,read,Out2)
       ; value(solver,satz)
	 -> Out2 = SolvOut2
       ; true ) ) ).

call_compact(Notify,In,Ans,Result,VarTable,Time,Lockfile) :-
   determine_os(OS),
   value(ccalc_dir,CCDir),
   ( value(mode,transition)
     -> value(extended_atom_count,AC)
   ; value(atom_count,AC) ),
   Vars is AC + 1,

   % If pipe I/O is being used, create all of the pipes to/from compact and
   % start it running in the background.  If file I/O is being used, run
   % compact to completion on the input.

   ( value(file_io,false)
     -> make_pipe(Result),
	make_pipe(VarTable),
	format_to_atom(CompactCall,
	   "~wsolvers/~s/compact -l -v~d -f~w < ~w > ~w 2>> ~w",
	   [CCDir,OS,Vars,VarTable,In,Result,VarTable]),
	( Notify == notify, value(verbose,true)
	  -> format("% Starting compact in background.~n",[])
	; true ),
	flush_output,
	time_system(CompactCall,background,Time,Lockfile)
   ; Result = 'ccsat.compacted',
     VarTable = 'ccsat.compact.vartable',
     Stderr = 'ccsat.compact.info',
     format_to_atom(CompactCall,
	"bash -c \"~wsolvers/~s/compact -l -v~d -f~w < ~w > ~w 2> ~w\"",
	[CCDir,OS,Vars,VarTable,In,Result,Stderr]),
     ( Notify == notify
       -> format("% Compacting...",[]),
	  flush_output
     ; true ),
     time_system(CompactCall,TC),
     ( Notify == notify
       -> format(" done.",[]),
	  ( value(timed,true)
	    -> format("  (~2f seconds)~n",[TC])
	  ; nl )
     ; true ),
     open(Stderr,read,StderrStrm),
     read_compact_ans(Notify,StderrStrm,Ans),
     close(StderrStrm) ).


read_compact_ans(Notify,VarTableStrm,Ans) :-
   ( Notify = notify, value(verbose,true)
     -> format("% Reading output file from compact...",[]),
	flush_output
   ; true ),

   % Remember the current input stream (so we can return to it later)
   current_input(CurIn),

   % Read the output compact sent to stderr.  This will contain information
   % about compact's search process; in particular, it will be noted here
   % if compact found a contradiction in the input (in which case the theory
   % is unsatisfiable and we don't even need to call the SAT solver).  Also,
   % compact has been modified to print the DIMACS header ("p cnf ...") for
   % the compacted theory to stderr.  If this header indicates that the
   % compacted theory has 0 atoms, it means that compact was able to resolve
   % all variables, and hence the theory is satisfiable (and again we don't
   % need to call the SAT solver).  If the compacted theory has at least one
   % atom but 0 clauses, the header is modified to indicate 1 clause, and the
   % tautological clause "1 -1 0" is passed along with it to the SAT solver.
   % (Many solvers don't like empty theories.)  Otherwise, the header is simply
   % passed on to the SAT solver.

   set_input(VarTableStrm),
   repeat,
      read_line(Line),
      ( Line == end_of_file
	-> fatal_error("Invalid output from compact.~n",[])
      ; ( Line == "Contradiction after unit resolution."
        ; prefix(Line,_,"Contradiction after failed lit test.") )
	-> Ans = "UNSAT"
      ; Line == "Satisfying interpretation found"
	-> Ans = "SAT"
      ; Line == "No interpretation found"
	-> true
      ; fail ),
   !,
   ( Notify = notify, value(verbose,true)
     -> format(" done.~n",[])
   ; true ),
   set_input(CurIn).


call_solver(Notify,Solver,In,Out1Stream,Out2) :-
   value(ccalc_dir,CCDir),
   value(num,NOI),
   determine_os(OS),
   ( value(solver_opts(Solver),SOpts)
     -> true
   ; SOpts = '' ),

   % Check to make sure the SAT solver executable is available.
   format_to_atom(FullName,"~wsolvers/~s/~w",[CCDir,OS,Solver]),
   ( common_file_exists(FullName)
     -> true
   ; fatal_error("Executable ~w is missing.",[FullName]) ),

   % Create I/O pipes (or files if file_io option is set)
   ( value(solver,satz)
     -> common_file_delete('satx.sol'),
	( value(file_io,false)
	  -> Out1 = 'satx.sol',
	     system('mkfifo satx.sol')
	; Out1 = 'ccsat.out' )
   ; value(file_io,false)
     -> make_pipe(Out1)
   ; Out1 = 'ccsat.out' ),

   % Determine name and command line for selected solver
   ( Solver == relsat
     -> Name = "relsat 2.0",
	( NOI == all
	  -> NOpt = a
	; NOpt = NOI ),
	format_to_atom(Call,"~w -#~w ~w ~w >~w",
	   [FullName,NOpt,SOpts,In,Out1])
   ; Solver == relsat_old
     -> Name = "relsat 1.1.2",
	format_to_atom(Call,"~w ~w ~w >~w",[FullName,SOpts,In,Out1])
   ; Solver == sato_old
     -> Name = "sato 3.1.2",
	format_to_atom(Call,"~w -f -m~w ~w ~w >~w",
	   [FullName,NOI,SOpts,In,Out1])
   ; Solver == sato
     -> Name = "sato 3.2",
	( value(sato_v,true)
	  -> VOpt = "-v"
	; VOpt = "" ),
	format_to_atom(Call,"~w -f -m~w ~s ~w ~w >~w",
	   [FullName,NOI,VOpt,SOpts,In,Out1])
   ; Solver == satz
     -> Name = "satz 215",
	format_to_atom(Call,"~w ~w ~w >~w",[FullName,SOpts,In,Out1]),
	( value(file_io,true)
	  -> Out2 = 'ccsat.out2'
	; true )
   ; Solver == satz-rand
     -> Name = "satz-rand 4.9",
	format_to_atom(Call,"~w -out - ~w ~w >~w",[FullName,SOpts,In,Out1])
   ; Solver == grasp
     -> Name = "GRASP",
	( NOI == all
	  -> NOpt = a
	; NOpt = NOI ),
	format_to_atom(Call,"~w +O +s~w ~w ~w >~w",
	   [FullName,NOpt,SOpts,In,Out1])
   ; Solver == walksat
     -> Name = "WalkSAT 37",
	format_to_atom(Call,"cat ~w | ~w -sol -numsol ~w ~w >~w 2> /dev/null",
	   [In,FullName,NOI,SOpts,Out1])
   ; Solver == zchaff
     -> Name = "ZChaff",
	format_to_atom(Call,"~w ~w ~w >~w",[FullName,SOpts,In,Out1])
   ; Solver == mchaff
     -> Name = "mChaff spelt3",
	( value(file_io,true)
	  -> Out2 = 'ccsat.out2',
	     Opts = 'mchaff-opts.smj'
	; make_pipe(Out2),
	  common_tmpname(Opts) ),
	write_mchaff_opts(Opts,Out2),
	format_to_atom(Call,"~w ~w /dev/null ~w >~w 2>/dev/null",
	   [FullName,In,Opts,Out1])
   ; fatal_error("~w is not a valid SAT solver.",[Solver]) ),

   % If file I/O is being used, call the SAT solver now, and open the first
   % output file for reading once it's done.  If pipe I/O is being used, start
   % solver running in the background, and open the first output pipe for
   % reading so it doesn't block.  (mchaff doesn't open its second output file
   % until after its input pipe is open, so CCalc will block if it tries to
   % open Out2 now.)
   ( value(file_io,true)
     -> ( Notify == notify
	  -> format("% Calling ~s... ",[Name]),
	     flush_output,
	     system_value(Call),
	     format("done.~n",[])
	; system_value(Call) ),
	( value(solver,satz)
	  -> system('mv satx.sol ccsat.out2')
	; true )
   ; ( var(Opts)
       -> OptsCall = ''
     ; format_to_atom(OptsCall,"; rm ~w",[Opts]) ),
     format_to_atom(BkgdCall,"(~w ~w) &",[Call,OptsCall]),
     ( Notify == notify, value(verbose,true)
       -> format("% Starting ~s in background.~n",[Name]),
	  flush_output
     ; true ),
     system(BkgdCall) ),
   open(Out1,read,Out1Stream).


% Create the file used to pass options to mChaff.  Most of the options are
% always the same; these are contained in the files 'mchaff-opts.smj.1' and
% 'mchaff-opts.smj.2' in the solvers directory.  The options which are changed
% for each call by CCalc are added between the contents of those two files
% and passed to mChaff.

write_mchaff_opts(OptsFile,Out2Pipe) :-
   common_tmpname(OptsFile0),
   tell(OptsFile0),
   format("  solutionsFile = ~w~n",[Out2Pipe]),
   value(num,NOI),
   ( NOI == all
     -> NArg = 0
   ; NArg = NOI ),
   format("  limitSolutionsToN = ~d~n",[NArg]),
   told,
   value(ccalc_dir,CCDir),
   format_to_atom(Call,
      "cat ~wsolvers/mchaff-opts.smj.1 ~w ~wsolvers/mchaff-opts.smj.2 > ~w",
      [CCDir,OptsFile0,CCDir,OptsFile]),
   system(Call),
   common_file_delete(OptsFile0).

write_input(Notify,Dest) :-
   ( value(mode,transition)
     -> value(extended_atom_count,AC),
	value(extended_clause_count,CC)
   ; value(atom_count,AC),
     value(clause_count,CC) ),
   value(query_clause_count,QC),
   TC is QC + CC,
   ( Notify == notify, value(verbose,true)
     -> format("% Writing input clauses...",[])
   ; true ),
   common_statistics(_),
   current_output(CurOut),

   % If there are no clauses, print a warning and write a dummy tautological
   % clause for the SAT solver (since some of them don't like 0 clauses).
   ( TC == 0
     -> % compact and sort don't need the DIMACS header ('p cnf # #'); the
	% SAT solvers do.
	set_output(Dest),
	( \+ value(compact,true), \+ value(sorted,true)
	  -> format("p cnf ~d 1~n",[AC])
	; true ) ,
	write_clause([1,-1])

   % otherwise, write all the clauses to the input pipe/file
   ; set_output(Dest),
     ( \+ value(compact,true), \+ value(sorted,true)
       -> format("p cnf ~d ~d~n",[AC,TC])
     ; true ),
     ( ( clause(C) ; query_clause(C) ),
       write_clause(C),
       fail
     ; true )),

   common_statistics(T),
   set_output(CurOut),

   ( Notify == notify, value(verbose,true)
     -> format(" done.",[]),
	( value(timed,true)
	  -> TS is T/1000,
	     format("  (~2f seconds)~n",[TS])
	; nl ),
	( TC == 0
	  ->  ( (value(solver,Solver), Solver \== relsat)
                 -> format("~nWarning: Since the theory has no clauses for SAT solver input,~n",[]),
       	           format("         ~w may not return correct solutions.~n",[Solver]), 
                   format("         Trying relsat instead...~n~n",[]),
                   set(solver,relsat)
                 ; true)
	; true )
   ; true ).


check_compact_ans(Ans) :-
        read_line(Line),
        ( Line == end_of_file ->
             seen,
             safe_see('ccsat.out4'),
             read_line(Line2),
             ( Line2 == end_of_file ->
                  Ans = "SAT"
             ; true ),
             seen
        ; ( Line == "Contradiction after unit resolution." ;
            prefix(Line,_,"Contradiction after failed lit test.") ) ->
             Ans = "UNSAT"
        ; check_compact_ans(Ans) ).


% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%
% The user level query and planning procedures.
% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%

output :- extract_info("SAT",M,_,_,_DMIKLESTODO1,_DMIKLESTODO2,_DMIKLESTODO3), show_models(M,sat).

% sat/0 tests the causal theory for satisfiability.  No facts are used.
sat :-
        common_statistics(_),
	( value(mode,transition)
	    -> format("% Displaying possible states: ~n",[]),
	       flush_output,
               shift_atoms_and_clauses(0)
	; true ),
	retractall(query_clause(_)),
	iset(query_clause_count,0),
	call_sat(notify,Out1,Out2,VarTable,CompactTime),
	extract_info(Out1,Out2,VarTable,Ans,Models,TimeMsg,Other),
	!,
        ( Ans == "UNSAT"
	    -> report_time(CompactTime,TimeMsg), seen, fail
	; Ans == "SAT"
	    -> report_time(CompactTime,TimeMsg),
               ( value(mode,transition)
                 -> assertz(domain(step,[0])),
                    show_models(Models,sat), 
                    retract(domain(step,_))
  	       ; show_models(Models,sat) )
        ; format("~n~s",[Other]), seen, !, fail ).

%%% query_/0 prompts the user for facts.
(query) :-
   common_statistics(_),
   read_rules(FieldList), 
%   semituple_to_list(Fields,FieldList),
   ( member_check((label::Label),FieldList)
      ->  ( atomic(Label) 
            -> true 
          ; fatal_error("Invalid label field (~w).",[Label]) )
   ; Label =  0 ),
   
   ( member_check(((maxstep)::MaxStep),FieldList)
     -> do_term_expansion(MaxStep,MaxStep1),
        eval_expr(MaxStep1,MaxStep2),
        ( value(mode,history)
          -> nonfatal_error("maxstep is already declared by macro. ",[])
        ; ( ( integer(MaxStep2) ; (MaxStep2 = (T1..T2), T2>=T1) )
            -> true  %iset(dynamic_detected,true)
          ; fatal_error("Invalid time field (~w).",[MaxStep]) ) )
   ; ( value(mode,history)
       -> macro((maxstep),_,MaxStep2)
     ; MaxStep2 = 0 ) ),

   delete(FieldList, (label::Label), FieldList2),
   delete(FieldList2, ((maxstep)::MaxStep), Rules),

   ( Label == 0
     -> value(query_counter,M), 
        Label1 = M,
        M1 is M-1,
        iset(query_counter,M1)
   ; Label1 = Label ),

   assertz(query_problem(Label1,MaxStep2,Rules)),
   query(Label1).


(nmquery) :-
   common_statistics(_),
   read_rules(FieldList), 
%   semituple_to_list(Fields,FieldList),   
   ( member_check((label::Label),FieldList)
      ->  ( atomic(Label) 
            -> true 
          ; fatal_error("Invalid label field (~w).",[Label]) )
   ; Label =  0 ),
   
   ( member_check(((maxstep)::MaxStep),FieldList)
     -> do_term_expansion(MaxStep,MaxStep1),
        eval_expr(MaxStep1,MaxStep2),
        ( value(mode,history)
          -> nonfatal_error("maxstep is already declared by macro. ",[])
        ; ( ( integer(MaxStep2) ; (MaxStep2 = (T1..T2), T2>=T1) )
            -> true %iset(dynamic_detected,true)
          ; fatal_error("Invalid time field (~w).",[MaxStep]) ) )
   ; ( value(mode,history)
       -> macro((maxstep),_,MaxStep2)
     ; MaxStep2 = 0 ) ),

   delete(FieldList, (label::Label), FieldList2),
   delete(FieldList2, ((maxstep)::MaxStep), Rules),

   ( Label == 0
     -> value(query_counter,M), 
        Label1 = M,
        M1 is M-1,
        iset(query_counter,M1)
   ; Label1 = Label ),

   assertz(nmquery_problem(Label1,MaxStep2,Rules)),
   nmquery(Label1).

%%% When its argument is atomic, query/1 assumes that the argument is a 
%%% problem label.  Owise, it assumes that it is a problem description.


% Query in transition mode should be treated likewise in history mode 
% because rules may refer to time explicitly. To do that, we need to 
% construct domain/2 again based on step [0..maxStep] every time we 
% invoke a query in transition mode. This could be inefficient, but
% unavoidable for now. We could check if the maxstep is same as before,
% skip constructing domain/2 for slight improvement. (this is not done 
% yet, but not difficult)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query(Label) : process the monotonic query labelled by Label
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assert_extended_steps_in_domain(MaxStep) :-
   retractall(domain(step,_)),
   retractall(domain(astep,_)),
   expand_sequence(0,MaxStep,Steps),
   AMaxStep is MaxStep-1,
   expand_sequence(0,AMaxStep,Asteps),
   assertz(domain(step,Steps)),
   assertz(domain(astep,Asteps)).

verify_invariant(TransLabel,TransAns) :-
   query(TransLabel,TransAns).

verify_init(InitLabel,InitAns) :-
   query(InitLabel,InitAns).

verify_goal(GoalLabel,GoalAns) :-
   query(GoalLabel,GoalAns).


query(A) :- 
   A =.. [maxstep | [Label,MaxStep]],
   !, 
   query(Label,MaxStep,_). 


% verifying unsolvable query
query(Label) :-
   atomic(Label),
   ( query_problem(Label,MaxStep,_Rules)
     -> true
   ; fatal_error("Invalid query label (~q).",[Label]) ),
   MaxStep=='any',
   !,
   
   format("~n% Verifying that the problem is not solvable...~n~n",[]), 

   append_name(Label,'_trans',TransLabel),
   append_name(Label,'_init',InitLabel),
   append_name(Label,'_goal',GoalLabel),

   format("~n% Verifying the given invariant...~n~n", []),
   verify_invariant(TransLabel,TransAns), 
   ( TransAns == "UNSAT"
     -> format("~n% Verified the invariant. ~n~n",[])
   ; fatal_error("Wrong invariant. The above is a counterexample.~n~n",[]) ), 

   format("~n% Verifying that initial state satisfies the invariant...~n~n", []),
   verify_init(InitLabel,InitAns),
   ( InitAns == "SAT"
     -> format("~n% Initial state satisfies the invariant. ~n~n",[])
   ; fatal_error("Initial state does not satisfy the invariant.~n~n",[])),

   format("~n% Verifying that every goal state does not satisfy the invariant...~n~n", []),
   verify_goal(GoalLabel,GoalAns),
   ( GoalAns == "UNSAT"
     -> format("% Every goal state does not satisfy the invariant. ~n~n",[])
   ; fatal_error("% There exists a goal state that satisfies the invariant. ~The above is a counterexample.n~n",[]) ), 

   format("~n% Verified that the problem is not solvable for any number of steps.~n~n",[]).


query(Label) :- 
   value(solver,Solver),
   query(Label,_),
   iset(solver,Solver).


query(Fields) :- process_query(Fields,query), !.   

query(Label,Ans) :-
   atomic(Label),
%   (value(mode,transition) ; value(mode,history)), 
   !,
   ( query_problem(Label,MaxStep,Rules)
     -> true
   ; fatal_error("Invalid query label (~q).",[Label]) ),

   ( MaxStep == -1
     -> fatal_error("maxstep is not specified. ",[])
   ; true),

   ( value(mode,history)
     -> macro(maxstep,true,MaxStep),
        assert_extended_steps_in_domain(MaxStep),
        query_aux(MaxStep,Rules,Ans)
   ; ( (value(mode,transition); value(mode,basic))
       -> ( MaxStep = (T1..T2)         % transition mode
             -> iterative_query(T1,T2,Rules)
          ; retractall(macro(maxstep,true,_)),
            assertz(macro(maxstep,true,MaxStep)),      
            assert_extended_steps_in_domain(MaxStep),
            query_aux(MaxStep,Rules,Ans) ) ) ).

iterative_query(T1,T2,Rules) :-
   ( (number(T2),  T1<T2) ; T2 == 'infinity' ), 
   !,
   retractall(macro(maxstep,true,_)),
   assertz(macro(maxstep,true,T1)),      
   assert_extended_steps_in_domain(T1),
   query_aux(T1,Rules,Ans),
   ( Ans == "UNSAT"  
     -> NewT1 is T1+1,
        iterative_query(NewT1,T2,Rules)
   ; true).

iterative_query(T1,T1,Rules) :-
   number(T1), !,
   retractall(macro(maxstep,true,_)),
   assertz(macro(maxstep,true,T1)),      
   assert_extended_steps_in_domain(T1),
   query_aux(T1,Rules,_).


%^ temporary disable
query(Label,MaxStep1,Ans) :-
   atomic(Label),
   (value(mode,transition) ; value(mode,history)), 
   !,
   ( query_problem(Label,_MaxStep0,Rules)
     -> true
   ; fatal_error("Invalid query label (~q).",[Label]) ),

   eval_expr(MaxStep1,MaxStep),
   
   ( value(mode,history)
     -> fatal_error("Can't pass maxstep along with query in history mode. ~n",[])
   ; ( value(mode,transition)
       -> ( MaxStep = (T1..T2)         % transition mode
            -> iterative_query(T1,T2,Rules)
          ; retractall(macro(maxstep,true,_)),
            assertz(macro(maxstep,true,MaxStep)),      
            assert_extended_steps_in_domain(MaxStep),
            query_aux(MaxStep,Rules,Ans) ) ) ).

query_aux(MaxStep,Rules,Ans) :-
   ( value(mode,transition)
     -> ( value(verbose,true)
	  -> format("% Shifting atoms and clauses... ",[])
	; true ),
	flush_output,
	common_statistics(_),
	shift_atoms_and_clauses(MaxStep),
	( value(verbose,true)
	  -> format("done.",[]),
	     ( value(timed,true)
	       -> common_statistics(ST),
	          STS is ST/1000,
	          format(" (~2f seconds)~n",[STS])
	     ; nl )
	; true ),
	value(extended_atom_count,EAC),
        value(extended_clause_count,ECC),
        format("% After shifting: ~d atoms",[EAC]),
	( value(aux_atom_count,AAC), AAC > 0
	  -> format(" (including new atoms)",[])
	; true ),
	format(", ~d clauses~n",[ECC]),
	flush_output
   ; true ),

   % The last query executed might have introduced auxiliary atoms, so
   % reset atom_count to its original value to eliminate these
   value(domain_atom_count,DAC),
   iset(atom_count,DAC),

   do_term_expansion(Rules,Rules1),  % should expand because in transition 
                                       % mode it may have not properly decoded
                                       % because step was limited to [0,1]
                                       % while any time stamps could have been
                                       % mentioned %? can we skip this?
   replace_comma_with_and(Rules1,Rules11),

   % This code was modified to improve efficiency.  Previously, all of the
   % query clauses were conjoined, and then this large formula was processed
   % and converted to CNF.  However, if several of the query clauses had
   % quantified variables, the quantifiers would be moved to the outside, so
   % that subformulae which didn't need to be under the scope of those
   % quantifiers would nonetheless be repeated many times when the quantified
   % variable was grounded.  Instead, CCalc now processes each query clause
   % and converts it to CNF separately, and *then* conjoins them all, to avoid
   % clause explosion.

   findall( R2,
	    ( member(R,Rules11),
	      ( decode_macro(R,Rules2),
	        decode_nested_constants_in_fact(Rules2,Rules3),
	        postprocessing_before_binding(Rules3,Rules4),
	        postprocessing_after_binding(Rules4,Rules6),
	        universal_closure(Rules6,Rules7),
	        clausify_wff(Rules7,R2)
		-> true )),
	    RCs0 ),
   findall( R, ( member(RC,RCs0), member(R,RC) ), RCs),

   length(RCs,QCC),
   iset(query_clause_count,QCC),
   retractall(query_clause(_)),
   ( member(RC,RCs),
     assertz(query_clause(RC)),
     fail
   ; true ),

   call_sat(notify,Out1,Out2,VarTable,CompactTime),
   extract_info(Out1,Out2,VarTable,Ans,Models,TimeMsg,Other),
   !,
   ( Ans == "UNSAT"
     -> report_time(CompactTime,TimeMsg),
        seen,
        ( value(mode,transition)
	  -> macro(maxstep,true,MaxStep),
	     format("No solution with maxstep ~w.~n~n",[MaxStep])
        ; ! ) % ,     
   ; Ans == "SAT"
     -> report_time(CompactTime,TimeMsg),
        show_models(Models,query)
   ; format("~n~s",[Other]), seen, !, fail   ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% nmquery(Label) : process the nonmonotonic query labelled by Label
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nmquery(A) :- 
   A =.. [maxstep | [Label,MaxStep]],
   !, 
   value(solver,Solver),
   nmquery(Label,MaxStep),
   iset(solver,Solver).

%^ here - iset(mode,basic) and don't shift clausex
nmquery(Label) :-
   atomic(Label), 
   value(solver,Solver),
   value(mode,M),
   (M == transition ; M == basic),
   !,
   iset(mode,history), % nmquery in transition mode is temporarily set to
                      %  history mode

   ( nmquery_problem(Label,MaxStep0,Rules)
     -> true
   ; iset(mode,M),
     fatal_error("Invalid nmquery label (~q).",[Label]) ),

   do_term_expansion(Rules,Rules3), 
   replace_comma_with_and(Rules3,Rules2),

   retract(nmquery_problem(Label,MaxStep0,Rules)),
   assertz(nmquery_problem(Label,MaxStep0,Rules2)),

   ( (MaxStep0) = (T1..T2)
     -> expand_sequence(T1,T2,MaxSteps),
	member(MaxStep,MaxSteps)
   ; MaxStep = MaxStep0 ),

   % temp?
   retractall(macro(maxstep,true,_)),
   assertz(macro(maxstep,true,MaxStep)),

   retractall(domain(_,_)),
   retractall(domain_schema(step,_)),
   retractall(domain_schema(astep,_)),
   expand_sequence(0,MaxStep,Ms),
   assertz(domain_schema(step,Ms)),
   M1 is MaxStep-1,
   expand_sequence(0,M1,Ms1),
   assertz(domain_schema(astep,Ms1)),
   instantiate_sorts,  % cut

   iset(mode,M), % get back to the original mode
   (M == transition 
   -> 
   ( value(verbose,true)
     -> format("% Shifting atoms and clauses... ",[])
   ; true ),
   flush_output,
   common_statistics(_),
   shift_atoms_and_clauses(MaxStep),
   ( value(verbose,true)
     -> format("done.",[]),
	( value(timed,true)
	  -> common_statistics(ST),
	     STS is ST/1000,
	     format(" (~2f seconds)~n",[STS])
	; nl )
   ; true ),
   value(extended_atom_count,EAC),
   value(extended_clause_count,ECC),
   format("% After shifting: ~d atoms",[EAC]),
   ( value(aux_atom_count,AAC), AAC > 0
     -> format(" (including new atoms)",[])
   ; true ),
   format(", ~d clauses~n",[ECC]),
   flush_output  
 ; true ),
  
   nmquery_aux(Rules2),
   % temp?
   retractall(macro(maxstep,true,_)), 
   iset(solver,Solver).

nmquery(Label) :-
   atomic(Label),
   value(mode,history),
   !,
   value(solver,Solver),
   ( nmquery_problem(Label,_MaxStep,Rules)
     -> true
   ; fatal_error("Invalid nmquery label (~q).",[Label]) ),
   nmquery_aux(Rules),
   iset(solver,Solver).

%nmquery(Label) :- 
%   atomic(Label),
%   value(mode,basic), 
%   fatal_error("Use sat instead of nmquery under basic mode. ",[]).

nmquery(Fields) :- process_query(Fields,nmquery).

nmquery(Label,MaxStep1) :-
   atomic(Label),
   value(mode,transition),
   !,
   value(solver,Solver),
   retractall(macro(maxstep,true,_)),

   iset(mode,history), % nmquery in transition mode is temporarily set to
                       %  history mode

   ( nmquery_problem(Label,MaxStep0,Rules)
     -> true
   ; iset(mode,transition),
     fatal_error("Invalid nmquery label (~q).",[Label]) ),

   eval_expr(MaxStep1,MaxStep),

   do_term_expansion(Rules,Rules2), 
   retract(nmquery_problem(Label,MaxStep0,Rules)),
   assertz(nmquery_problem(Label,MaxStep,Rules2)), % overwrite MaxStep

   ( MaxStep = (T1..T2)
     -> expand_sequence(T1,T2,Steps),
	member(CurMaxStep,Steps)
   ; CurMaxStep = MaxStep ),

   % temp?
   retractall(macro(maxstep,true,_)),
   assertz(macro(maxstep,true,CurMaxStep)),

   retractall(domain(_,_)),
   retractall(domain_schema(step,_)),
   retractall(domain_schema(astep,_)),
   expand_sequence(0,CurMaxStep,Ms),
   assertz(domain_schema(step,Ms)),
   M1 is CurMaxStep-1,
   expand_sequence(0,M1,Ms1),
   assertz(domain_schema(astep,Ms1)),
   instantiate_sorts,  % cut

   iset(mode,transition), % get back to transition mode

   common_statistics(_),
   ( value(verbose,true)
     -> format("% Shifting atoms and clauses... ",[])
   ; true ),
   flush_output,
   common_statistics(_),
   shift_atoms_and_clauses(CurMaxStep),
   ( value(verbose,true)
     -> format("done.",[]),
	( value(timed,true)
	  -> common_statistics(ST),
	     STS is ST/1000,
	     format(" (~2f seconds)~n",[STS])
	; nl )
   ; true ),
   value(extended_atom_count,EAC),
   value(extended_clause_count,ECC),
   format("% After shifting: ~d atoms",[EAC]),
   ( value(aux_atom_count,AAC), AAC > 0
     -> format(" (including new atoms)",[])
   ; true ),
   format(", ~d clauses~n",[ECC]),
   flush_output,

   nmquery_aux(Rules2),
   % temp?
   retractall(macro(maxstep,true,_)),
   set(solver,Solver).

nmquery(Label,_MaxStep) :-
   atomic(Label),
   value(mode,history),
   !,
   fatal_error("Can't pass maxstep along with query in history mode. ~n",[]).

%nmquery(Label,_) :- 
%   atomic(Label),
%   value(mode,basic), 
%   fatal_error("Use sat instead of nmquery under basic mode. ",[]).

nmquery_aux(Rules) :-
   retractall(query_rule(_)),    
   retractall(query_rule_schema(_)),
   retractall(query_rule_body(_,_)),
   retractall((_H<-_B)),

   % The last query executed might have introduced auxiliary atoms, so
   % reset atom_count to its original value to eliminate these
   value(domain_atom_count,DAC),
   iset(atom_count,DAC),

   insert_query_rules(Rules),

   process_query_rule_schemas,

   retractall(query_clause(_)),  % why here?
   generate_completion_clauses(query), !, % red cut

   call_sat(notify,Out1,Out2,VarTable,CompactTime),
   extract_info(Out1,Out2,VarTable,Ans,Models,TimeMsg,Other),
   !,
   ( Ans == "UNSAT"
     -> report_time(CompactTime,TimeMsg),
	seen,
	( value(mode,transition)
	  -> macro(maxstep,true,MaxStep),
	     format("No solution with maxstep ~w.~n~n",[MaxStep])
	; ! ),
	fail
   ; Ans == "SAT"
     -> report_time(CompactTime,TimeMsg),
        show_models(Models,query)
   ; format("~n~s~n",[Other]), seen, !, fail ).



% extract_info(Out1,Out2,VarTable,Ans,Model,TimeMsg,Other)
% The first two arguments are input, the rest are output.
% Out1 and Out2 are the names of the pipes containing output data from the
% SAT solver.  (Out2 is used only for some solvers, like mChaff, which returns
% search information in one file and the models themselves in another.)
% ANS is either "SAT" or "UNSAT"; Model is a list of atoms assigned true in the
% satisfying assignment (if ANS is SAT); TimeMsg is the message from the sat
% solver regarding solution time.
%
% The argument 'Other' is currently not used.  Formerly, Other was bound only
% when CCalc encountered unexpected output from the solver and could not
% determine whether the answer was SAT or UNSAT, in which case Other contained
% a message about the unexpected output.  Now, however, extract_info prints
% an error message and aborts when this happens.

extract_info("UNSAT",_,_,"UNSAT",_,"Contradiction found during compaction.",_).

extract_info("SAT",_,VT,"SAT",Models,"Solution found during compaction.",_) :-
   current_input(CurIn),
   set_input(VT),
   get_renamed_vars(Vars),
   ( value(file_io,false) -> rm_pipe(VT) ; close(VT) ),
   set_input(CurIn),
   rebuild_models([[]],Vars,Models).

extract_info(Out1,Out2,VarTable,Ans,Models,TimeMsg,Other) :-
   ( value(verbose,true)
     -> format("% Reading output file(s) from SAT solver...",[]),
	flush_output
   ; true ),
   current_input(CurIn),
   ( value(compact,true)
     -> set_input(VarTable),
	get_renamed_vars(Vars),
	( value(file_io,false) -> rm_pipe(VarTable) ; close(VarTable) )
   ; true ),
   set_input(Out1),
   value(solver,Solver),
   extract_info_aux(Solver,Out2,Ans,Models0,TimeMsg,Other),
   ( ( Ans \= "SAT", Ans \= "UNSAT" )
     -> cleanup_after_extract(Out1,Out2),
	fatal_error("Cannot read SAT solver output files.",[])
   ; true ),
   ( value(verbose,true)
     -> format(" done.~n",[]),
	flush_output
   ; true ),
   set_input(CurIn),
   ( Ans == "SAT", value(compact,true)
     -> rebuild_models(Models0,Vars,Models)
   ; Models = Models0 ),
   cleanup_after_extract(Out1,Out2).


cleanup_after_extract(Out1,Out2) :-
   % Close the output pipes/files.  If they're pipes, delete them, too.
   ( value(file_io,false)
     -> rm_pipe(Out1),
	( nonvar(Out2) -> rm_pipe(Out2) ; true )
   ; close(Out1) ),

   % Delete unnecessary files create by some SAT solvers
   common_file_delete(timetable),
   common_file_delete(record).


extract_info_aux(sato,_,Ans,Models,TimeMsg,Other) :-
   !,
   read_sato_answer(Ans),
   ( Ans = "UNSAT"
     -> read_sato_time(TimeMsg)
   ; Ans = "SAT"
     -> read_sato_models(Models),
	read_sato_time(TimeMsg)
   ; Other = Ans ).

extract_info_aux(sato(_),_,Ans,Models,TimeMsg,Other) :-
   !,
   extract_info_aux(sato,_,Ans,Models,TimeMsg,Other).

extract_info_aux(sato_old,_,Ans,Models,TimeMsg,Other) :-
   !,
   read_sato_old_answer(Ans),
   ( Ans == "UNSAT"
     -> read_sato_time(TimeMsg)
   ; Ans == "SAT"   
     -> read_sato_old_models(Models),
        read_sato_time(TimeMsg)
   ; Other = Ans ).

extract_info_aux(sato_old(_),_,Ans,Models,TimeMsg,Other) :-
   !,
   extract_info_aux(sato_old,_,Ans,Models,TimeMsg,Other).

extract_info_aux(relsat,_,Ans,Models,TimeMsg,Other) :-
   !,
   read_relsat_lines(Ans,Models,PrepTime,SolveTime,Other,_),
   get_number(PrepTime,PrepNum,_),
   get_number(SolveTime,SolveNum,_),
   TotalNum is PrepNum + SolveNum,
   format_to_charlist(TotalTime,"~d",[TotalNum]),
   ccalc_format_time(TotalTime,PrepTime,SolveTime,TimeMsg).

extract_info_aux(relsat_old,_,Ans,[Model],TimeMsg,Other) :-
   !,
   read_relsat_old_prep_time(PrepTime),
   read_relsat_old_answer(Ans),
   ( Ans == "SAT"
     -> read_relsat_old_model(Model)
   ; Ans == "UNSAT"
     -> true
   ; Other = Ans ),
   read_relsat_old_solve_time(SolveTime),
   get_number(PrepTime,PrepNum,_),
   get_number(SolveTime,SolveNum,_),
   TotalNum is PrepNum + SolveNum,
   format_to_charlist(TotalTime,"~d",[TotalNum]),
   ccalc_format_time(TotalTime,PrepTime,SolveTime,TimeMsg).

extract_info_aux(relsat_old(_),_,Ans,[Model],TimeMsg,Other) :-
   !,
   extract_info_aux(relsat_old,_,Ans,[Model],TimeMsg,Other).

extract_info_aux(satz,Out2,Ans,[Model],TimeMsg,Other) :-
   !,
   read_line(Line),
   ( Line == end_of_file
     -> ( var(Ans) -> Ans = "No solution was found." ; true ),
	( var(TimeMsg) -> TimeMsg = "Solution time not reported." ; true ),
	( var(Model) -> Model = [] ; true )
   ; prefix(Line,"****the instance is satisfiable *****",_)
     -> Ans = "SAT",
	( nonvar(Out2)
	  -> current_input(CurIn),
	     open(Out2,read,Out2S),
	     set_input(Out2S),
	     extract_info_aux(satz,_,_,[Model],_,_),
	     close(Out2S),
	     set_input(CurIn)
	; true ),
	( ( var(Model) ; var(TimeMsg) )
	  -> extract_info_aux(satz,Out2,Ans,[Model],TimeMsg,Other)
	; true )
   ; prefix(Line,"****the instance is unsatisfiable *****",_)
     -> Ans = "UNSAT",
	( var(TimeMsg)
	  -> extract_info_aux(satz,Out2,Ans,[Model],TimeMsg,Other)
	; true )
   ; prefix(Line,"Program terminated in ",RestLine)
     -> get_until(RestLine," ",TimeStr,_),
	append("Solution time: ",TimeStr,TimeStr2),
	append(TimeStr2," seconds.",TimeMsg),
	( ( var(Ans) ; Ans = "SAT", var(Model) )
	  -> extract_info_aux(satz,Out2,Ans,[Model],TimeMsg,Other)
	; true )
   ; get_positive_numbers(Line,Model)
     -> ( ( var(Ans) ; var(TimeMsg) )
	  -> extract_info_aux(satz,Out2,Ans,[Model],TimeMsg,Other)
	; true )
   ; extract_info_aux(satz,Out2,Ans,[Model],TimeMsg,Other) ).

extract_info_aux(satz-rand,_,Ans,[Model],TimeMsg,Other) :-
   !,
   read_satz_rand_answer(Ans,Model),
   ( Ans == "UNSAT" ; Ans == "SAT" ; Other = Ans ), !,
   read_satz_rand_time(TimeMsg).

extract_info_aux(grasp,_,Ans,Models,TimeMsg,Other) :-
   !,
   read_grasp_info(Ans,Models,PrepTime,SolveTime,TotalTime,Msg),
   ccalc_format_time(TotalTime,PrepTime,SolveTime,TimeMsg0),
   ( var(Msg)
     -> TimeMsg = TimeMsg0
   ; format_to_charlist(TimeMsg1,"~nResources exceeded... search aborted.",[]),
     append(TimeMsg0,TimeMsg1,TimeMsg) ),
   ( Ans \== "SAT", Ans \== "UNSAT"
     -> Other = Ans
   ; true ).

extract_info_aux(walksat,_,Ans,Models,TimeMsg,Other) :-
   !,
   read_walksat_preamble,
   read_walksat_preamble,
   read_walksat_models(Models),
   read_walksat_time(TimeMsg),
   read_walksat_answer(Ans),
   ( Ans \== "SAT", Ans \== "UNSAT"
     -> Other = Ans
   ; true ).

extract_info_aux(zchaff,_,Ans,[Model],TimeMsg,Other) :-
   !,
   read_zchaff_answer(Ans),
   ( Ans == "SAT"
     -> read_zchaff_model(Model)
   ; Ans == "UNSAT"
     -> true
   ; Other = Ans ),
   read_zchaff_time(TimeMsg).

extract_info_aux(mchaff,Out2,Ans,Models,TimeMsg,Other) :-
   !,
   current_input(Out1),
   set_input(Out2),
   read_mchaff_models(Models),
   set_input(Out1),
   read_mchaff_answer(Ans0),
   read_mchaff_time(TimeMsg),
   ( Ans0 == "UNSAT", Models == [] ->
     % if mchaff says the instance is unsatisfiable and doesn't
     % return any solutions, it really is unsatisfiable
     Ans = "UNSAT"
   ; ( Ans0 == "SAT" ; Ans0 == "UNSAT" ) ->
     % if mchaff says the instance is unsatisfiable, but returns
     % some solutions to it, then there are *some* solutions, but
     % not as many as requested with the num option
     Ans = "SAT"
   ; Ans = Ans0,
     Other = Ans0 ).

extract_info_aux(_,_,_,_,_,_) :-
   fatal_error("Unknown sat solver!",[]).


read_sato_answer(Ans) :-
	read_line(Line),
	( Line = end_of_file
	    -> Ans = "No solution was found."
	; Line = "The clause set is unsatisfiable."
	    -> Ans = "UNSAT"
	; Line = "Model #1: (indices of true atoms)" 
	    -> Ans = "SAT"
	%% Unfortunately, it is beneath sato to bother with just one clause!
	; Line = "Warning: There is only one input clause. sato ignores this trivial case. "
	    -> Ans = Line
	; read_sato_answer(Ans) ).

            
read_sato_models([Model|Rest]) :-
         read_line(_),
         read_line(NextLine),
	 get_positive_numbers(NextLine,Model),
         read_line(_),
         read_line(NextLine1),
         ( prefix(NextLine1,"Model",_)
              -> read_sato_models(Rest)
         ; Rest = []).

         
read_sato_old_answer(Ans) :-
        read_line(Line),
        ( Line = end_of_file
            -> Ans = "No solution was found."
        ; Line = "UNSAT"
            -> Ans = Line
        ; Line = "SAT"
            -> Ans = Line  
        %% Unfortunately, it is beneath sato to bother with just one clause!
        ; Line = "Warning: There is only one input clause. sato ignores this trivial case. "
            -> Ans = Line
        ; read_sato_old_answer(Ans) ).

            
read_sato_old_models([Model|Rest]) :-
         read_line(NextLine),
         get_positive_numbers(NextLine,Model),
         read_line(_),
         read_line(NextLine1),
         (NextLine1 = "SAT"
              -> read_sato_old_models(Rest)
         ; Rest = []).


read_sato_time(TimeMsg) :-
             read_line(Line),
             ( Line == end_of_file ->
		  TimeMsg = "Solution time: unknown"
             ; prefix(Line,"run time (seconds)",TotalStr) ->
		  read_line(Line2),
		  prefix(Line2,"  build time",PrepStr),
		  read_line(Line3),
		  prefix(Line3,"  search time",SolveStr),
		  ccalc_format_time(TotalStr,PrepStr,SolveStr,TimeMsg)
             ; read_sato_time(TimeMsg) ).

read_satz_rand_time(TimeMsg) :-
        read_line(Line),
        ( Line == end_of_file ->
             TimeMsg = "Solution time: unknown."
        ; prefix(Line,"Program terminated in ",RestLine) ->
             get_until(RestLine," ",TimeStr,_),
             append("Solution time: ",TimeStr,TimeStr2),
             append(TimeStr2," seconds.",TimeMsg)
        ; read_satz_rand_time(TimeMsg) ).


read_satz_rand_answer(Ans,Model) :-
        read_line(Line),
        ( Line == end_of_file
          -> Ans = "No solution was found."
        ; prefix(Line," trial",_)
          -> read_line(_),
             read_line(ModelLine),
             read_satz_rand_answer(Ans,_),
             ( Ans == "SAT"
               -> get_positive_numbers(ModelLine,Model)
             ; true )
        ; prefix(Line,"****the instance is satisfiable *****",_)
          -> Ans = "SAT"
        ; prefix(Line,"****the instance is unsatisfiable *****",_)
          -> Ans = "UNSAT"
        ; read_satz_rand_answer(Ans,Model) ).

% reading the output file for relsat 2.0 is more complicated than other
% solvers, since it presents information in an inconvenient order and sometimes
% doesn't present runtimes

read_relsat_lines(Ans,Models,PrepTime,SolveTime,Other,Time) :-
	read_line(Line),
	( Line == "c Processing phase stats: " ->
	     % whatever "Time" is passed back refers to prep time
	     read_relsat_lines(Ans,Models,_,SolveTime,Other,PrepTime)
	; Line == "c Solution phase stats: " ->
	     % whatever "Time" is passed back refers to solve time
	     read_relsat_lines(Ans,Models,PrepTime,_,Other,SolveTime),
	     % reset returned Time to "0", in case relsat reported runtime
	     % for the solution phase but not the processing phase
	     Time = "0"
	; ( prefix(Line,"c   Seconds elapsed (real time): ",TimeStr)
	  ; prefix(Line,"c   Seconds Elapsed (real time) : ",TimeStr) ) ->
	     % return the time read on this line; this will be either prep
	     % or solve time, depending on what heading was read at an
	     % earlier level
             get_until(TimeStr," ",Time,_),
	     read_relsat_lines(Ans,Models,PrepTime,SolveTime,Other,_)
	; prefix(Line,"Solution",_) ->
	     % read the model on this line
	     between(Line,_SolNum,ModelStr,": "),
	     get_positive_numbers(ModelStr,Model),
	     % read information from successive lines, including possibly
	     % more models
	     read_relsat_lines(Ans,RestModels,PrepTime,SolveTime,Other,Time),
	     % concatenate model from this line with remaining models.
	     % (Note that RestModels will be uninstantiated only if relsat
	     % returns UNSAT -- which shouldn't happen if it's returning a
	     % solution on this line -- since Models is set to [] when end
	     % end of file is read.)
	     Models = [Model|RestModels]
	; Line == "SAT" ->
	     Ans = Line,
	     % ignore any ANS returned by following lines -- which shouldn't
	     % happen anyway -- and keep Other unbound
	     read_relsat_lines(_,Models,PrepTime,SolveTime,_,Time)
	; Line == "UNSAT" ->
	     Ans = Line,
	     % ignore any ANS returned by following lines, and keep both
	     % Models and Other unbound
	     read_relsat_lines(_,_,PrepTime,SolveTime,_,Time)
	; Line == end_of_file ->
	     % bind default values for all the values, in case lines were
	     % missing from relsat's output (e.g. no solution time was listed,
	     % which happens if relsat determines during preprocessing that
	     % the theory is unsatisfiable), and terminate recursion
	     Models = [],
	     Other = "No solution was found.",
	     PrepTime = "0",
	     SolveTime = "0",
	     Time = "0"
	; read_relsat_lines(Ans,Models,PrepTime,SolveTime,Other,Time) ).


read_relsat_old_prep_time(PrepTime) :-
        read_line(Line),
        ( Line = end_of_file ->
	     PrepTime = "0"
        ; prefix(Line,"c Preprocessing instance:",Rem) ->
             between(Rem,"in "," seconds",PrepTime)
        ; read_relsat_old_prep_time(PrepTime) ).   


read_relsat_old_answer(Ans) :-
        read_line(Line),
        ( Line == "UNSAT" ->
             Ans = Line
        ; Line == "SAT" ->
             Ans = Line
        ; Line == end_of_line ->
             Ans = "No solution was found."
        ; read_relsat_old_answer(Ans) ).


read_relsat_old_model(Model) :-
	read_line(Line),
	get_positive_numbers(Line,Model).


read_relsat_old_solve_time(SolveTime) :-
	read_line(Line),
	( Line == end_of_file ->
	     SolveTime = "0"
	; prefix(Line,"Seconds Elapsed (real time) : ",TimeString) ->
	     get_until(TimeString," ",SolveTime,_)
	; read_relsat_old_solve_time(SolveTime) ).

read_grasp_info(Ans,Models,PrepTime,SolveTime,TotalTime,Msg) :-
	read_line(Line0),
	( Line0 == end_of_file ->
	     Ans = "No solution was found.",
	     Models = [],
	     PrepTime = "unknown",
	     SolveTime = "unknown",
	     TotalTime = "unknown"
	; drop_spaces(Line0,Line),
	  ( prefix(Line,"Done creating structures. Elapsed time: ",PrepTime) ->
	       read_grasp_info(Ans,Models,_,SolveTime,TotalTime,Msg)
	  ; Line == "GRASP Information: Computed solution" ->
	       read_line(_),
	       read_line(_),
	       read_line(ModelLine),
	       read_grasp_info(_,RestModels,_,SolveTime,TotalTime,Msg),
	       get_numbers(ModelLine,Lits),
	       read_grasp_models(RestModels,Lits,Models),
	       Ans = "SAT"
	  ; prefix(Line,"Done searching.... RESOURCES EXCEEDED. Elapsed time: ",
	       SolveTime) ->
		  Msg = "Resources exceeded... search aborted.",
		  read_grasp_info(_,Models,PrepTime,_,TotalTime,_)
	  ; prefix(Line,
	       "Done searching.... SATISFIABLE INSTANCE. Elapsed time: ",
	       SolveTime) ->
		  Ans = "SAT",
		  read_grasp_info(_,Models,PrepTime,_,TotalTime,Msg)
	  ; prefix(Line,
	       "Done searching.... UNSATISFIABLE INSTANCE. Elapsed time: ",
	       SolveTime) ->
		  Ans = "UNSAT",
		  read_grasp_info(_,Models,PrepTime,_,TotalTime,Msg)
	  ; prefix(Line,"Terminating GRASP. Total time: ",TotalTime) ->
	       read_grasp_info(Ans,Models,PrepTime,SolveTime,_,Msg)
	  ; read_grasp_info(Ans,Models,PrepTime,SolveTime,TotalTime,Msg) ) ).


read_grasp_models(InModels,Lits,OutModels) :-
   value(num,N),
   % num_models is the maximum number of models which remain to be read
   iset(num_models,N),
   findall( M,
	    ( read_grasp_models_aux(Lits,1,M),
	      \+ member(M,InModels),
	      ( value(num_models,all)
	        -> true
	      ; decr(num_models,_) )),
	    LineModels ),
   append(InModels,LineModels,OutModels).

read_grasp_models_aux([],Num,Model) :-
   % If the next integer in sequence is greater than the number of atoms,
   % and the list of literals in the solution is empty, all literals have
   % been read.  Otherwise, an empty list of literals means the remaining
   % atoms all have "don't care" values, so the procedure branches to assign
   % either true of false to that atom.  Backtracking will produce all
   % solutions.
   \+ value(num_models,0),
   ( value(extended_atom_count,AC) -> true ; value(atom_count,AC) ),
   ( Num > AC
     -> Model = []
   ; NextNum is Num + 1,
     read_grasp_models_aux([],NextNum,RestModel),
     ( Model = RestModel
     ; \+ value(num_models,0),
       Model = [Num|RestModel] )).

read_grasp_models_aux([L|Lits],Num,Model) :-
   % If the next positive integer in the sequence or its negation is the
   % next integer listed in the model, then the value of that atom is known.
   % If neither is present, that atom has a "don't care" value, so the
   % procedure branches as above.
   \+ value(num_models,0),
   NextNum is Num + 1,
   ( L = Num
     -> read_grasp_models_aux(Lits,NextNum,RestModel),
	Model = [Num|RestModel]
   ; L is -Num
     -> read_grasp_models_aux(Lits,NextNum,RestModel),
	Model = RestModel
   ; read_grasp_models_aux([L|Lits],NextNum,RestModel),
     ( Model = RestModel
     ; \+ value(num_models,0),
       Model = [Num|RestModel] )).


read_walksat_preamble :-
	read_line(Line),
	( Line == "" ->
	     true
	; read_walksat_preamble ).


read_walksat_models(Models) :-
	read_line(Line),
	( ( Line == end_of_file ; Line == "" ) ->
	     Models = []
	; prefix(Line,"Begin assign with lowest # bad = 0",_) ->
	     read_walksat_model(Model),
	     read_walksat_models(RestModels),
	     Models = [Model|RestModels]
	; read_walksat_models(Models) ).


read_walksat_model(Model) :-
	read_line(Line),
	( Line == "End assign" ->
	     Model = []
	; get_positive_numbers(Line,FirstVars),
	  read_walksat_model(RestVars),
	  append(FirstVars,RestVars,Model) ).


read_walksat_time(TimeMsg) :-
        read_line(Line),
        ( Line == end_of_file ->
             TimeMsg = "Solution time: unknown."
        ; prefix(Line,"total elapsed seconds = ",Time) ->
	     format_to_charlist(TimeMsg,"Solution time: ~s seconds.",[Time])
        ; read_walksat_time(TimeMsg) ).


read_walksat_answer(Ans) :-
	read_line(Line),
        ( Line = end_of_file
            -> Ans = "No solution was found."
        ; Line = "ASSIGNMENT FOUND"
            -> Ans = "SAT"
        ; Line = "ASSIGNMENT NOT FOUND"
            -> Ans = "UNSAT"
        ; read_walksat_answer(Ans) ).


read_zchaff_answer(Ans) :-
	read_line(Line),
	( Line == end_of_file
            -> Ans = "No solution was found."
        ; Line == "Verify Solution successful. Instance satisfiable"
            -> Ans = "SAT"
        ; Line == "Instance unsatisfiable"
            -> Ans = "UNSAT"
        ; read_zchaff_answer(Ans) ).


read_zchaff_model(Model) :-
	read_line(Line),
	get_positive_numbers(Line,Model).


read_zchaff_time(TimeMsg) :-
	read_line(Line),
	( Line == end_of_file ->
             TimeMsg = "Solution time: unknown."
        ; prefix(Line,"Total Run Time",RestLine) ->
	     drop_spaces(RestLine,TimeStr),
	     format_to_charlist(TimeMsg,"Solution time: ~s seconds.",[TimeStr])
        ; read_zchaff_time(TimeMsg) ).


read_mchaff_answer(Ans) :-
	read_line(Line),
	( Line == end_of_file ->
	     Ans = "No solution was found."
	; prefix(Line,"Satisfyable instance",_) ->
	     Ans = "SAT"
	; prefix(Line,"Unsatisfiable instance",_) ->
	     Ans = "UNSAT"
	; read_mchaff_answer(Ans) ).


read_mchaff_time(TimeMsg) :-
	read_line(Line),
	( Line == end_of_file ->
	     TimeMsg = "Solution time: unknown."
	; prefix(Line,"UserTimeForSearch: ",SearchTime0) ->
	    read_line(Line2),
	    prefix(Line2,"TotalUserTime: ",TotalTime),
	    get_number(SearchTime0,SearchNum,_),
	    get_number(TotalTime,TotalNum,_),
	    PrepNum is max(TotalNum - SearchNum,0),
	    format_to_charlist(SearchTime,"~2f",[SearchNum]),
	    format_to_charlist(PrepTime,"~2f",[PrepNum]),
	    ccalc_format_time(TotalTime,PrepTime,SearchTime,TimeMsg)
	; read_mchaff_time(TimeMsg) ).


% read_mchaff_models reads the models returned by mChaff in the output file,
% up to the number specified in the 'num' option by the user.  It stops
% reading solutions and stops branching on "don't care" values in the solutions
% once it reaches this number.

read_mchaff_models(Models) :-
   value(num,N),
   % num_models is the maximum number of models which remain to be read
   iset(num_models,N),
   read_mchaff_models_aux([],Models).

read_mchaff_models_aux(InModels,OutModels) :-
   % read one model at a time from the output file
   read_line(Line),
   ( prefix(Line,"\"[",ModelBits)
     -> % find all complete models which satisfy the (possibly incomplete,
	% containing "don't care" values) model returned by mChaff, removing
	% models which were previously found
	findall( M,
		 ( read_mchaff_models_aux2(ModelBits,1,M),
		   \+ member(M,InModels),
		   % when a model is found, decrement the number of models
		   % remaining
		   ( value(num_models,all)
		     -> true
		   ; decr(num_models,_) )),
		 LineModels ),
	append(InModels,LineModels,NewModels),
	read_mchaff_models_aux(NewModels,OutModels)
   ; OutModels = InModels ).


% In read_mchaff_models_aux2, the "Bits" are actually a list of characters,
% where '1' represents a true atom, '0' represents a false atom, and 'X'
% represents a "don't care" value.  Index is the atom number corresponding to
% the first element of the list.  This procedure returns Model, which is a
% complete model that satisfies the (incomplete if it contains "don't care"
% values) model represented by the "bits".  Backtracking allows all such
% models, i.e. all combinations of true or false for "don't care" atoms, to be
% generated.  If the variable 'num_models' is 0, this procedure fails
% immediately; this stops the branching on "don't care" values when the maximum
% number of models has already been read.
%
% mChaff appears to eliminate a sequence of X's at the end of a model.  That
% is, a solution may include fewer characters than the number of atoms in the
% theory, in which case the N atoms shown correspond to the first N atoms,
% and all atoms with an index greater than N are assumed to have value "X".
% This condition must be checked when the end-of-model character "]" is
% encountered.

read_mchaff_models_aux2([Bit|RestBits],Index,Model) :-
   % if requested number of models have been generated, stop generating more
   ( value(num_models,0)
     -> false

   % character "]" marks end of model (unless not all atoms have been given
   % values -- see above)
   ; Bit == 93,
     ( value(mode,transition)
       -> value(extended_atom_count,AC)
     ; value(atom_count,AC) ),
     Index > AC
     -> Model = []

   ; NextIndex is Index + 1,
     ( Bit == 93
       -> % if end of model was already reached, recursively generate models
	  %   assuming all atoms not displayed have "don't care" values
	  read_mchaff_models_aux2([Bit|RestBits],NextIndex,RestModel)
      ; % otherwise just recur on rest of model shown
	read_mchaff_models_aux2(RestBits,NextIndex,RestModel) ),
     ( Bit == 49  % character "1" means atom is true -- add to model
       -> Model = [Index|RestModel]
     ; Bit == 48  % character "0" means atom is false -- don't add to model
       -> Model = RestModel
     ; % otherwise, atom is "don't care" -- can be true or false
       ( Model = RestModel
       ; ( value(num_models,0)
	   -> false
	 ; Model = [Index|RestModel] )))).


bits_to_numbers(String,Nums) :- bits_to_numbers(String,Nums,1).
bits_to_numbers([Bit|RestBits],Nums,Index) :-
	Index2 is Index + 1,
	( Bit == 49 ->   % character "1"
	     bits_to_numbers(RestBits,RestNums,Index2),
	     Nums = [Index|RestNums]
	; ( Bit == 48 ; Bit == 88 ) ->   % character "0" or "X"
	     bits_to_numbers(RestBits,Nums,Index2)
	; Nums = [] ).

% SWI Prolog used to have format_time/3 to print out the date and time
% but no format_time/4, so CCalc defined a predicate with this name, to 
% print the time solvers take.  Now SWI added a built-in format_time/4, 
% but different from what our format_time/4 did.  (Their version is like 
% format_time/3 with locale info added.)
% We renamed all instances of format_time/4 to be ccalc_format_time/4.
% -- Selim T. Erdogan, 26 Jan 2008
%
ccalc_format_time(TotalLine,PrepLine,SolveLine,Msg) :-
	drop_spaces(TotalLine,TotalStr),
	get_until(TotalStr," ",Total,_),
	append("Solution time: ",Total,Msg1),
	append(Msg1," seconds (prep ",Msg2),
	drop_spaces(PrepLine,PrepStr),
	get_until(PrepStr," ",Prep,_),
	append(Msg2,Prep,Msg3),
	append(Msg3," seconds, search ",Msg4),
	drop_spaces(SolveLine,SolveStr),
	get_until(SolveStr," ",Solve,_),
	append(Msg4,Solve,Msg5),
	append(Msg5," seconds)",Msg).


% rebuild_models(CModels,Vars,RModels) converts the models returned by the
% SAT solver, CModels, into complete models, RModels, using the variable list
% Vars returned by compact.  (Compact may determine that some atoms must be
% true or false and eliminate them from the theory it passes to the SAT solver,
% so these atoms must be added back in before the solution is displayed.)

rebuild_models([],_,[]).

rebuild_models([CModel|RestCModels],Vars,[RModel|RestRModels]) :-
   rebuild_model(CModel,Vars,RModel),
   rebuild_models(RestCModels,Vars,RestRModels).

rebuild_model(_,[],[]).

rebuild_model(CModel,[[R,-1]|RestVars],[R|RestR]) :-
	rebuild_model(CModel,RestVars,RestR).

rebuild_model(CModel,[[_,-2]|RestVars],RestR) :-
	rebuild_model(CModel,RestVars,RestR).

rebuild_model(CModel,[[R,C]|RestVars],[R|RestR]) :-
	member(C,CModel),
	rebuild_model(CModel,RestVars,RestR).

rebuild_model(CModel,[_|RestVars],RestR) :-
	rebuild_model(CModel,RestVars,RestR).


get_renamed_vars([Var|RestVars]) :-
	read_line(Line),
	get_numbers(Line,Var),
	get_renamed_vars(RestVars).
get_renamed_vars([]).


% reports the execution time for compact (if it's turned on) and for the SAT
% solver.

report_time([CompactTime,Lockfile],Msg) :-
   ( value(timed,true)
     -> ( nonvar(CompactTime)
	  -> see(CompactTime),
	     wait(Lockfile),
	     parse_time(T), !,
	     seen,
	     format("% Compact time: ~2f seconds.~n",[T]),
	     common_file_delete(CompactTime)
	; true ),
	format("% ~s~n",[Msg])
   ; true ),
   nl.

clausify_wff(Wff,Nss) :-
   findall( Ns,
	    ( gcnf(Wff,Css),
	      member(Ms,Css),
              sort(Ms,Ns),
	      \+ tautology(Ns) ),	
	    Nss ).

universal_closure(Wff,NewWff) :-
   find_free_variables(Wff,Vs),
   wrap_univ_quantifiers(Vs,Wff,NewWff).

% given wff W, returns CNF Cs
gcnf(W,Cs) :-
        negation_normal_form(W,W11),
%^temp
        do_term_expansion(W11,W1),
	nnf_to_cnf(W1,Cs0,Aux),
	append(Cs0,Aux,Cs).

nnf_to_cnf(W1,Cs,AuxCs) :-
   ( W1 = false
     -> Cs = [[]],
	AuxCs = []
   ; W1 = true
     -> Cs = [],
	AuxCs = []
   ; distribute_or_over_and(W1,Cs,AuxCs) ).


gdnf(W,Cs) :-
   negation_normal_form(W,W1),
   value(optimize,Opt),
   iset(optimize,false),
   nnf_to_dnf(W1,Cs,_),
   iset(optimize,Opt).


nnf_to_dnf(W,Cs,AuxCs) :-
   ( W = false
     -> Cs = [],
	AuxCs = []
   ; W = true
     -> Cs = [[]],
	AuxCs = []
   ; distribute_and_over_or(W,Cs,AuxCs) ).

negation_normal_form(A,E) :-
   equiv(A,A1),
   !,
   negation_normal_form(A1,E).

negation_normal_form((A && B),E) :- 
   !,
   negation_normal_form(A,A1),
   ( A1 = false 
     -> E = false
   ; A1 = true
     -> negation_normal_form(B,E)
   ; negation_normal_form(B,B1),
   ( B1 = false
     -> E = false
   ; B1 = true
     -> E = A1
   ; E = (A1 && B1) ) ).

negation_normal_form((A ++ B),E) :- 
   !,
   negation_normal_form(A,A1),
   ( A1 = false 
     -> negation_normal_form(B,E)
   ; A1 = true
     -> E = true
   ; negation_normal_form(B,B1),
   ( B1 = false
     -> E = A1
   ; B1 = true
     -> E = true
   ; E = (A1 ++ B1) ) ).

negation_normal_form(-A,E) :-
   !,
   ( ( value(debug,true)
       -> extended_test(A)
     ; test(A) )
     -> ( call(A)
          -> E = false
        ; E = true )
   ; integer(A) 
     -> B is 0 - A,
        E = B
   ; E = -A ).

negation_normal_form(A,E) :-
   ( ( value(debug,true)
       -> extended_test(A)
     ; test(A) )
     -> ( call(A)
          -> E = true
        ; E = false )
   ; E = A ).

extended_test(A) :-
   ( A =.. [=,A1,A2]
     -> ( ( (evaluable_expr(A1) ; object(A1)), 
            (evaluable_expr(A2) ; object(A2)) )
          -> test(A)
        ; fatal_error("Invalid expression (~w).", [A]) )
   ; test(A) ).

%/*
replace_false_with_negative_literal_list([L|Ls],[L1|L1s]) :-
   replace_false_with_negative_literal(L,L1),
   replace_false_with_negative_literal_list(Ls,L1s).
replace_false_with_negative_literal_list([],[]).
%*/

/*
replace_false_with_negative_literal_list(Fs,Ns) :-
   replace_false_with_negative_literal_list(Fs,[],Ns).
replace_false_with_negative_literal_list([L|Ls],Acc,newL) :-
   replace_false_with_negative_literal(L,L1),
   append(Acc,[L1],Acc1),
   replace_false_with_negative_literal_list(Ls,Acc1,newL).
replace_false_with_negative_literal_list([],newL,newL).
*/

%equiv(((T: A eq true)=(T: B eq true)), ((T: A eq true)<->(T: B eq true))).
%equiv(((A eq true)=(B eq true)), ((A eq true)<->(B eq true))).
equiv((A <-> B),((-A ++ B) && (A ++ -B))).
equiv((A ->> B),(-A ++ B)).
%equiv((A , B),( A &&  B)).
equiv((A & B),( A &&  B)).
equiv(-Wff,NewWff) :- equivnot(Wff,NewWff).

equiv([/\X|A],E) :-
%   tuple_to_list(X,Vs),
   Vs=[X],
   find_computed_variables(Vs,Us),
   renaming_subst(Vs,Sub),
   subst_free(A,A1,Sub),
   findall((A1,Us),bind_vars(Sub),As),
           replace_false_with_negative_literal_list(As,Cs),
           unify_computed_variables(Cs,Bs,Us),
   conjoin_list(Bs,E0),
   eval_expr(E0,E).  % why this was added? to evaluate arithmetic inside constant


equiv([\/X|A],E) :-
   Vs=[X],
%   tuple_to_list(X,Vs),
   find_computed_variables(Vs,Us),
   renaming_subst(Vs,Sub),
   subst_free(A,A1,Sub),
   findall((A1,Us),bind_vars(Sub),As), 
           replace_false_with_negative_literal_list(As,Cs),
           unify_computed_variables(Cs,Bs,Us),
   disjoin_list(Bs,E0),
%^format("~nE0 is : ",[]),
%^print_list([E0]),
   eval_expr(E0,E).


find_computed_variables([],[]).
find_computed_variables([V|Vs],Us) :-
   ( var_sort(V,computed)
     -> Us = [V|Us1],
        find_computed_variables(Vs,Us1)
   ; find_computed_variables(Vs,Us) ).

unify_computed_variables([],[],_).
unify_computed_variables([(A,Us)|As],[A|Bs],Us) :-
   unify_computed_variables(As,Bs,Us).

% We separate the negative Wffs to take advantage of indexing.
equivnot((A <-> B),((A && -B) ++ (-A && B))).
equivnot((A ->> B),(A && -B)).
equivnot((A , B),(-A ++ -B)).
equivnot((A & B),(-A ++ -B)).
equivnot((A && B),(-A ++ -B)).
equivnot((A ++ B),(-A && -B)).
equivnot((-A),A).
equivnot([/\X|A],[\/X|(-A)]).
equivnot([\/X|A],[/\X|(-A)]).

or(true,_,true) :-
	!.
or(_,true,true) :-
	!.
or(A,false,A) :-
	!.
or(false,A,A) :-
	!.
or(A,B,(A ++ B)).


and(false,_,false) :-
	!.
and(_,false,false) :-
	!.
and(A,true,A) :-
	!.
and(true,A,A) :-
	!.
and(A,B,(A && B)).



renaming_subst([],[]).
renaming_subst([N|Ns],[=(N,_)|Sub]) :-
	renaming_subst(Ns,Sub).



%^jo- tuple (1,2,3) 
%^jo- list  [1,2,3]

tuple_to_list((A,Bs),[A|As]) :-
   !,
   tuple_to_list(Bs,As).
tuple_to_list(A,[A]).

list_to_tuple([A,A1|As],(A,Bs)) :-
   !,
   list_to_tuple([A1|As],Bs).
list_to_tuple([A],A).
list_to_tuple([],true).

semituple_to_list((A;Bs),[A|As]) :-
	!,
	semituple_to_list(Bs,As).
semituple_to_list(A,[A]).

list_to_semituple([A,A1|As],(A;Bs)) :-
	!,
	list_to_semituple([A1|As],Bs).
list_to_semituple([A],A).
list_to_semituple([],false).

% takes a list of atoms and forms the conjunction of all of them
conjoin_list([],true).
conjoin_list([A|As],Wff) :-
   conjoin_list(As,Wff1),
   ( Wff1 = true 
     -> Wff = A
   ; Wff = (A && Wff1) ).

% takes a list of atoms and forms the disjunction of all of them
disjoin_list([],false).
disjoin_list([A|As],Wff) :-
   disjoin_list(As,Wff1),
   ( Wff1 = false 
     -> Wff = A
   ; Wff = (A ++ Wff1) ).

tuple_to_conjunct(A,B) :-
   tuple_to_list(A,C),
   conjoin_list(C,B).

%/*
% list to list
replace_comma_with_and(L,L1) :-
   tuples_to_conjs(L,L1). 
%*/

/*
% tuple to tuple : logic programming
replace_comma_with_and((A,As),(A & Bs)) :-
   format("~Here", []),
   !,
   replace_comma_with_and(As,Bs). 
replace_comma_with_and(A,A).

% list to tuple: also conjoin one more level deep
*/
replace_comma_with_and_1([],true).
replace_comma_with_and_1([A|As],(B & Bs)) :-
   conjoin_tuple(A,B),
   replace_comma_with_and_1(As,Bs). 
   
conjoin_tuple((A,As),Wff) :-
   conjoin_tuple(As,Wff1),
   ( Wff1 = true 
     -> Wff = A
   ; Wff = (A & Wff1) ).
conjoin_tuple(A,A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% distribute_or_over_and(F,Aux,R,AuxCs) converts formula F to CNF, representing
% it in list notation using the integers assigned to the atoms (by the
% atom_integer predicate).  In this list notation, every formula is a list of
% lists of integers, where each sublist represents a disjuction of literals and
% the whole list represents a conjunction of these disjunctions.  'True' is
% represented by [] (since the empty conjuction is vacuously true), and 'false'
% is represented by [[]] (since the empty disjunction is false).
%
% If the option 'optimize' is set to true, auxiliary atoms will be
% introduced to shorten the result; Aux will then contain the additional
% clauses which define the auxiliary atoms in terms of other atoms.  If 
% 'optimize' is false, no auxiliary atoms will be introduced and Aux will be
% the empty list.
%
% The three-argument procedure is a wrapper for the version with 8 parameters.
% In this version, Pos is the conversion of the formula to CNF; PosAux is the
% set of clauses defining auxiliary atoms used in Pos; PosN is the number of
% clauses in Pos; Neg is the conversion of the formula's negation to CNF;
% NegVar is a list of uninstantiated variables that appear in NegAux; and
% NegAux and NegN are analagous to PosAux and PosN.  The negation of each
% subformula is computed simultanously with the subformula itself, and is
% used when (and only when) an auxiliary atom is defined using that subformula
% (since if aux. atom K is equivalent to formula F, clauses for -K ++ F and
% K ++ -F are added).  It's easier to convert F and -F simultaneously than
% do it with separate calls to distribute_or_over_and since we *always* want
% both versions so that we can compare the number of clauses generated when
% we replace F with an auxiliary atom and when we don't, to decide which is
% smaller.
%
% The uninstantiated variables in NegAux are auxiliary atoms which appear
% in Neg, the optimization of -F.  Neg will only be used if F is replaced with
% an auxiliary variable, and we don't want to add "real" auxiliary atoms for
% that formula unles it's used, since otherwise we will have "orphaned"
% auxiliary atoms which aren't contained in any clauses and can therefore
% have arbitrary values.  Thus, we leave these atoms uninstantiated until
% Neg is actually used, at which point the procedure bind_all_vars/1 is used
% to add the new atoms.  NegVar is a list of pairs [K,NegK], where K is a
% new atom to be introduced and NegK is its negation.  (We can't just use
% -K in the formula when K is uninstantiated; it won't be evaluated, so if
% for example K is assigned the new atom integer 1000, -K will be -(1000)
% instead of -1000.  This causes problems, so instead we treat K and NegK as
% separate variables and let bind_all_vars/1 establish the relationship
% between them.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

distribute_or_over_and(F,Result,Aux) :-
   distribute_or_over_and(F,Result,Aux,_,_,_,_,_),
   !.

% Handle a formula in DNF list notation
distribute_or_over_and([[]],[],[],1,[[]],[],[],1) :- !.

distribute_or_over_and([],[[]],[],1,[],[],[],1) :- !.

distribute_or_over_and([[A]],Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN) :-
   !,
   distribute_or_over_and(A,Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN).

distribute_or_over_and([[A|B]],Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN) :-
   !,
   distribute_or_over_and(([[A]]&&[B]),Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN).

distribute_or_over_and([A|B],Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN) :-
   !,
   distribute_or_over_and(([A]++B),Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN).

% Convert a disjunction to CNF
distribute_or_over_and((A ++ B),Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN) :-
   !,
   distribute_or_over_and(A,PosA,PosAuxA,PosNA,NegA,NegVarA,NegAuxA,NegNA),
   distribute_or_over_and(B,PosB,PosAuxB,PosNB,NegB,NegVarB,NegAuxB,NegNB),

   % calculate the number of clauses generated if neither A nor B, only A,
   % or only B (respectively) is replaced with an auxiliary atom.  (It's
   % never valuable to replace both, because the number of clauses saved by
   % avoiding distribution is always outweighed by the number of clauses
   % generated by auxiliary clauses.)

   N00 is PosNA * PosNB,
   N10 is PosNB + PosNA + NegNA,
   N01 is PosNA + PosNB + NegNB,

   % optimize positive formula A ++ B
   ( ( value(optimize,false)
     ; N00 =< N10, N00 =< N01 )
     -> % replace neither A nor B with auxiliary atoms: i.e. simply perform
	% distribution, by disjoining each disjunct from A with each disjunct
	% of B in turn and then conjoining all of these resulting disjunctions
	findall( R,
		 ( member(DA,PosA),
		   member(DB,PosB),
		   append(DA,DB,R) ),
		 Pos ),
	PosN is N00,

	% No new auxiliary clauses were added, so simply append the clauses
	% generated for A and for B
	append(PosAuxA,PosAuxB,PosAux),

	% Since no aux. clauses were introduced, the negations of A and B
	% were not used, and therefore none of the auxiliary atoms in those
	% formulae were instantiated.  Thus, we append the atoms from A and B
	% and return them.
	append(NegVarA,NegVarB,NegVar)

   ; N10 =< N01
     -> % replace A with an auxiliary atom.  Thus the formula itself will
	% become K ++ B, which will then be distributed, and we'll also add
	% clauses to define K <-> A.  The latter is of course -K ++ A and
	% K ++ -A, so this is where we use the optimized negation of A we
	% calculated in the recursive call.  So first we must bind all of the
	% uninstantiated variables representing auxiliary atoms in -A.
	bind_all_vars(NegVarA),

	% assign a new auxiliary atom to replace A
	( value(extended_atom_count,_)
	  -> incr(extended_atom_count,K)
	; incr(atom_count,K) ),
	NegK is -K,

	% distribute K ++ B, by simply adding K to each conjunct of B
	append_to_each(K,PosB,Pos),

	% similarly generate clauses for -K ++ A
	append_to_each(NegK,PosA,PosAux1),

	% similarly generate clauses for K ++ -A
	append_to_each(K,NegA,PosAux2),

	% collect the newly defined auxiliary clauses with those defined
	% for all the subformulae used, and return
	append(PosAuxA,PosAuxB,PosAuxAB),
	append(PosAux1,PosAux2,PosAux12),
	append(PosAuxAB,PosAux12,PosAuxAB12),
	append(PosAuxAB12,NegAuxA,PosAux),
	PosN is PosNB,
	NegVar = NegVarB

   ; % replace B with an auxiliary atom -- analogous to the previous case
	bind_all_vars(NegVarB),
	( value(extended_atom_count,_)
	  -> incr(extended_atom_count,K)
	; incr(atom_count,K) ),
	NegK is -K,
	append_to_each(K,PosA,Pos),
	append_to_each(NegK,PosB,PosAux1),
	append_to_each(K,NegB,PosAux2),
	append(PosAuxA,PosAuxB,PosAuxAB),
	append(PosAux1,PosAux2,PosAux12),
	append(PosAuxAB,PosAux12,PosAuxAB12),
	append(PosAuxAB12,NegAuxB,PosAux),
	PosN is PosNA,
	NegVar = NegVarA ),

   % optimize formula's negation (-A && -B).  This is easy since -A and -B
   % are both in CNF after recursion-- no distribution is necessary.
   append(NegA,NegB,Neg),
   append(NegAuxA,NegAuxB,NegAux),
   NegN is NegNA + NegNB.   

% Convert a conjunction to CNF
distribute_or_over_and((A && B),Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN) :-
   !,
   distribute_or_over_and(A,PosA,PosAuxA,PosNA,NegA,NegVarA,NegAuxA,NegNA),
   distribute_or_over_and(B,PosB,PosAuxB,PosNB,NegB,NegVarB,NegAuxB,NegNB),

   % optimize formula (A && B) -- this is easy since A and B are already
   % in CNF after recursion
   append(PosA,PosB,Pos),
   append(PosAuxA,PosAuxB,PosAux),
   PosN is PosNA + PosNB,
   
   % optimize formula's negation (-A ++ -B).  This requires distributing
   % disjunction over conjunction and possibly introducing auxiliary atoms;
   % it's analagous to the converting the positive form of a disjunction,
   % above.

   N00 is NegNA * NegNB,
   N10 is NegNB + PosNA + NegNA,
   N01 is NegNA + PosNB + NegNB,
   ( ( value(optimize,false)
     ; N00 =< N10, N00 =< N01 )
     -> % replace neither A nor B
	findall( R,
		 ( member(DA,NegA),
		   member(DB,NegB),
		   append(DA,DB,R) ),
		 Neg ),
	NegVar = [],
	append(NegAuxA,NegAuxB,NegAux),
	NegN = N00
   ; N10 =< N01
     -> % replace A.  K and NegK are deliberately unbound here; they'll be
	% bound at a higher level of the expression tree if this subformula
	% is ever used
	bind_all_vars(NegVarA),
	NegVar = [[K,NegK]|NegVarB],
	append_to_each(K,NegB,Neg),
	append_to_each(NegK,NegA,NegAux1),
	append_to_each(K,PosA,NegAux2),
	append(NegAuxA,NegAuxB,NegAuxAB),
	append(NegAux1,NegAux2,NegAux12),
	append(NegAuxAB,NegAux12,NegAuxAB12),
	append(NegAuxAB12,PosAuxA,NegAux),
	NegN = NegNB
   ; % replace B.  K and NegK are unbound; see comment above.
     bind_all_vars(NegVarB),
     NegVar = [[K,NegK]|NegVarA],
     append_to_each(K,NegA,Neg),
     append_to_each(NegK,NegB,NegAux1),
     append_to_each(K,PosB,NegAux2),
     append(NegAuxA,NegAuxB,NegAuxAB),
     append(NegAux1,NegAux2,NegAux12),
     append(NegAuxAB,NegAux12,NegAuxAB12),
     append(NegAuxAB12,PosAuxB,NegAux),
     NegN = NegNA ).

% Replace a negative literal with its corresponding integer
distribute_or_over_and(-A,E,[],1,NegE,[],[],1) :-
   !,
   distribute_or_over_and(A,NegE,_,_,E,_,_,_).

% Replace a positive literal with its corresponding integer
distribute_or_over_and(A,[[E]],[],1,[[NegE]],[],[],1) :-
   ( find_atom_integer(A,E)
     -> NegE is -E
   ; integer(A),
     A >= 0,
     ( value(extended_atom_count,AC) -> true ; value(atom_count,AC) ),
     A =< AC
     -> E = A,
	NegE is -E
   ; integer(A),
     A =< 0,
     NegA is -A
     -> distribute_or_over_and(NegA,[[NegE]],_,_,[[E]],_,_,_)
   ; subst_functor(eq,=,A,A1), 
     fatal_error("~q is not an atomicFormula.",[A1]) ).


% distribute_and_over_or converts a formula to DNF.  It works in the same
% manner as distribute_or_over_and, above. The result is returned in DNF list
% notation, which is a list of lists of atoms in which each sublist is a
% conjunction of atoms and the whole list is a disjunction of these
% conjunctions.  'False' is [] (the empty disjunction) and 'true' is
% [[]] (the empty conjunction).  Aux is still returned in CNF (since the
% auxiliary clauses are always added directly to the propositional theory,
% which is in CNF, rather than going through completion).
%
% This procedure is mostly uncommented since it parallels distribute_or_over_
% and, except where noted below.  See that procedure for details.  

distribute_and_over_or(F,Result,Aux) :-
   distribute_and_over_or(F,Result,Aux,_,_,_,_,_),
   !.

distribute_and_over_or((A && B),Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN) :-
   !,
   distribute_and_over_or(A,PosA,PosAuxA,PosNA,NegA,NegVarA,NegAuxA,NegNA),
   distribute_and_over_or(B,PosB,PosAuxB,PosNB,NegB,NegVarB,NegAuxB,NegNB),

   N00 is PosNA * PosNB,
   N10 is PosNB + PosNA + NegNA,
   N01 is PosNA + PosNB + NegNB,

   % optimize formula A && B
   ( ( value(optimize,false)
     ; N00 =< N10, N00 =< N01 )
     -> findall( R,
		 ( member(DA,PosA),
		   member(DB,PosB),
		   append(DA,DB,R) ),
		 Pos ),
	PosN is N00,
	append(PosAuxA,PosAuxB,PosAux),
	append(NegVarA,NegVarB,NegVar)

   ; N10 =< N01
     -> % replace A
	bind_all_vars(NegVarA),
	( value(extended_atom_count,_)
	  -> incr(extended_atom_count,K)
	; incr(atom_count,K) ),
	NegK is -K,
	append_to_each(K,PosB,Pos),
	negate_lits(NegA,NegatedNegA),
	append_to_each(NegK,NegatedNegA,PosAux1),
	negate_lits(PosA,NegatedPosA),
	append_to_each(K,NegatedPosA,PosAux2),
	append(PosAuxA,PosAuxB,PosAuxAB),
	append(PosAux1,PosAux2,PosAux12),
	append(PosAuxAB,PosAux12,PosAuxAB12),
	append(PosAuxAB12,NegAuxA,PosAux),
	PosN is PosNB,
	NegVar = NegVarB
   ; % replace B
	bind_all_vars(NegVarB),
	( value(extended_atom_count,_)
	  -> incr(extended_atom_count,K)
	; incr(atom_count,K) ),
	NegK is -K,
	append_to_each(K,PosA,Pos),
	negate_lits(NegB,NegatedNegB),
	append_to_each(NegK,NegatedNegB,PosAux1),
	negate_lits(PosB,NegatedPosB),
	append_to_each(K,NegatedPosB,PosAux2),
	append(PosAuxA,PosAuxB,PosAuxAB),
	append(PosAux1,PosAux2,PosAux12),
	append(PosAuxAB,PosAux12,PosAuxAB12),
	append(PosAuxAB12,NegAuxB,PosAux),
	PosN is PosNA,
	NegVar = NegVarA ),

   % optimize formula's negation (-A ++ -B)
   append(NegA,NegB,Neg),
   append(NegAuxA,NegAuxB,NegAux),
   NegN is NegNA + NegNB.   

distribute_and_over_or((A ++ B),Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN) :-
   !,
   distribute_and_over_or(A,PosA,PosAuxA,PosNA,NegA,NegVarA,NegAuxA,NegNA),
   distribute_and_over_or(B,PosB,PosAuxB,PosNB,NegB,NegVarB,NegAuxB,NegNB),

   % optimize formula (A ++ B)
   append(PosA,PosB,Pos),
   append(PosAuxA,PosAuxB,PosAux),
   PosN is PosNA + PosNB,
   
   % optimize formula's negation (-A && -B)
   N00 is NegNA * NegNB,
   N10 is NegNB + PosNA + NegNA,
   N01 is NegNA + PosNB + NegNB,

   ( ( value(optimize,false)
     ; N00 =< N10, N00 =< N01 )
     -> % replace neither A nor B with auxiliary atoms
	findall( R,
		 ( member(DA,NegA),
		   member(DB,NegB),
		   append(DA,DB,R) ),
		 Neg ),
	NegVar = [],
	append(NegAuxA,NegAuxB,NegAux),
	NegN is N00
   ;  N10 =< N01
     -> % replace A.  K and NegK are deliberately unbound here; they'll be
	% bound at a higher level of the expression tree if this subformula
	% is ever used
	bind_all_vars(NegVarA),
	NegVar = [[K,NegK]|NegVarB],
	append_to_each(K,NegB,Neg),
	negate_lits(PosA,NegatedPosA),
	append_to_each(NegK,NegatedPosA,NegAux1),
	negate_lits(NegA,NegatedNegA),
	append_to_each(K,NegatedNegA,NegAux2),
	append(NegAuxA,NegAuxB,NegAuxAB),
	append(NegAux1,NegAux2,NegAux12),
	append(NegAuxAB,NegAux12,NegAuxAB12),
	append(NegAuxAB12,PosAuxA,NegAux),
	NegN is NegNB
   ; % replace B.  K and NegK are unbound; see comment above.
     bind_all_vars(NegVarB),
     NegVar = [[K,NegK]|NegVarA],
     append_to_each(K,NegA,Neg),
     negate_lits(PosB,NegatedPosB),
     append_to_each(NegK,NegatedPosB,NegAux1),
     negate_lits(NegB,NegatedNegB),
     append_to_each(K,NegatedNegB,NegAux2),
     append(NegAuxA,NegAuxB,NegAuxAB),
     append(NegAux1,NegAux2,NegAux12),
     append(NegAuxAB,NegAux12,NegAuxAB12),
     append(NegAuxAB12,PosAuxB,NegAux),
     NegN is NegNB ).

% Literals are represented the same way in both CNF and DNF.

distribute_and_over_or(A,Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN) :-
   distribute_or_over_and(A,Pos,PosAux,PosN,Neg,NegVar,NegAux,NegN).


% bind_all_vars/1 assigns integers to each uninstantiated new atom (and its
% negation) in a list.  See distribute_or_over_and, above, for details.

bind_all_vars([]).

bind_all_vars([[V,NegV]|Vs]) :-
   ( value(extended_atom_count,_)
     -> incr(extended_atom_count,K)
   ; incr(atom_count,K) ),
   V = K,
   NegV is -K,
   bind_all_vars(Vs).

% Note: Unlike value(original_atom_count,M), value(atom_count,M) includes the
% number of new atoms introduced to avoid clause explosion.
write_header(Xs,Ys) :-
   ( value(extended_atom_count,M) -> true ; value(atom_count,M) ),
   ( value(extended_clause_count,N) -> true ; value(clause_count,N) ),
   value(query_clause_count,QCC),
   length(Xs,N1), 
   length(Ys,N2), 
   Num is (N + N1 + N2 + QCC),
   format("p cnf ~w ~w~n",[M,Num]).

write_header2(Xs,Ys) :-
   ( value(extended_atom_count,M) -> true ; value(atom_count,M) ),
   ( value(extended_clause_count,N) -> true ; value(clause_count,N) ),
   value(query_clause_count,QCC),
   length(Xs,N1), 
   length(Ys,N2), 
   Num is (N + N1 + N2 + QCC),
   M1 is M+1,
   format("p cnf ~w ~w~n",[M1,Num]).

write_header(Xs,Ys,Zs) :-
   ( value(extended_atom_count,M) -> true ; value(atom_count,M) ),
   ( value(extended_clause_count,N) -> true ; value(clause_count,N) ),
   length(Xs,N1), 
   length(Ys,N2), 
   length(Zs,N3), 
   Num is (N + N1 + N2 + N3),
   format("p cnf ~w ~w~n",[M,Num]).

% each clause should be a list of integers, 
% the arguments are a list of clauses

write_clauses([]).

write_clauses([C|Cs]) :-
   write_clause(C), write_clauses(Cs).


write_clause([]) :-
   write(0), nl.

write_clause([L|Ls]) :-
   write(L), write(' '), write_clause(Ls).


get_numbers(String,Nums) :-
        drop_spaces(String,NewS),
        ( NewS = [] 
            -> Nums = [] 
        ; get_number(NewS,Num,Rest), 
          Nums = [Num|Nums1],
          get_numbers(Rest,Nums1) ).

get_positive_numbers(String,Nums) :-
        drop_spaces(String,NewS),
        ( NewS = [] 
            -> Nums = [] 
        ; get_number(NewS,Num,Rest), 
	  ( Num > 0
	      -> Nums = [Num|Nums1]
	  ; Nums = Nums1 ),
          get_positive_numbers(Rest,Nums1) ).


drop_spaces([C|Cs],Cs1) :-
        ( (C = 32; C = 9) -> drop_spaces(Cs,Cs1)
        ; Cs1 = [C|Cs] ).
drop_spaces([],[]).

get_number(String,Num,Rest) :-
        get_until(String,[32,10],Chars,Rest), 
        common_number_chars(Num,Chars).

get_until([Char|Cs],Delimiters,[],Cs) :-
        member(Char,Delimiters), !.
get_until([C|Cs],Delimiters,[C|Chars],Rest) :-
        get_until(Cs,Delimiters,Chars,Rest).
get_until([],_,[],[]).


/*
% FL is fluent buy_howmany(magazine) eq 2
is_attribute(FL) :-
	FL =.. [eq,Arg1,_Arg2], 
	functor(Arg1, FArg1,NArg1),
	functor(Attr,FArg1,NArg1),
        attribute0(_Act,Attr).

% or cross_fast(boat)
is_attribute(Arg1) :-
	functor(Arg1, FArg1,NArg1), 
	functor(Attr,FArg1,NArg1),
        attribute0(_Act,Attr).
*/


%%%%%%%%%%%%%%
%% is_attribute(A,Attr) : check Attr is an attribute of action A
%% Given A and Attr 
%% A: action with const args  ex) move(a)
%% Attr: attribute with const args  ex) to(a)
is_attribute(A,Attr) :-
   A =.. [AName|AConstArgs], 
   Attr =.. [AttrName| AttrConstArgs],
   prefix(AttrConstArgs,AConstArgs,_),
%   consts_to_sorts(AConstArgs,ASortArgs),
%   consts_to_sorts(AttrConstArgs,AttrSortArgs),
   consts_to_sorts(AConstArgs,ASortArgs),
   consts_to_sorts(AttrConstArgs,AttrSortArgs), 
   A1 =.. [AName|ASortArgs],
   Attr1 =.. [AttrName|AttrSortArgs],
   attribute0(A1,Attr1),
   !.

get_attribute(FL,A,(Attr=Arg2)) :-   % to(a) eq table, move(a),_
   A =.. [_FA|ArgsA],            % move(a) =.. [move,a]
   FL =.. [FFL|ArgsFL],         % to(a) eq table =.. [eq,to(a),table]
   (=(FFL,eq)		     % buy_howmany(magazine) eq 2
    -> ArgsFL = [Arg1,Arg3]     % [to(a),table]=[to(a),table]
       ; Arg1 = FL),	     % cross_fast(boat) 
   Arg1 =.. [FArg1|ArgsArg1],   % to(a)=..[to,a]
   is_attribute(A,Arg1),
   append(ArgsA,ArgsAttr,ArgsArg1),   % append([a],[],[a]
   (\==(FFL,eq) 		% cross_fast(boat)
    -> Arg2 = true
       ; Arg2 = Arg3),
   Attr =.. [FArg1|ArgsAttr].  % to =.. [to]


display_attributes(A,Attrs) :-
   ( A = (A1 eq V)
   ; A1 = A ), !,
   A1 =.. [FA|ArgsA],
   list_to_tuple(ArgsA,TupleA),
   ( TupleA == true
     -> format("~w(",[FA])
   ; format("~w(~w,",[FA,TupleA]) ) ,
   !,
   display_attrs(Attrs),
   !,
   display_others(A,Attrs),
   !,
   write(')'),
   ( A = (A1 eq V), V \= true
     -> format("=~w",[V])
   ; true ),
   write('  ').

display_attrs([]) :- !.
display_attrs([A]) :-
	write(A).
display_attrs([A|As]) :-
	write(A),
	write(','),
	display_attrs(As).
		

display_others(A,Attrs) :-
	get_others(A,Attrs,Others0),
	remove_duplicates(Others0,Others),
	!,
	(Others = []
		-> true
	; write(','),
	  display_attrs(Others) ).

% for some action A, there may be some attributes Attrs that
% occur in the model, and some attributes Others that do not occur in the
% model. Note that Others can be the attributes with boolean values.
get_others(A,Attrs,Others) :-
	functor(A,FA,NA),
	domain_schema(_,Cs),
	member(C,Cs),
	functor(C,FA,NA),
 	findall((Attr:false),
		(attribute0(C,Attr0),is_different_attr(A,Attr0,Attrs,Attr)),
		Others).

is_different_attr0(_Attr0,[]).
is_different_attr0(Attr0,[(Attr1:_)|Attrs]) :-
        functor(Attr0,FAttr,_NAttr),
	Attr1 =.. [FAttr1|_ArgsAttr1],
	FAttr \== FAttr1, 
	is_different_attr0(Attr0,Attrs).

is_different_attr(A,Attr0,Attrs,Attr) :-
	is_different_attr0(Attr0,Attrs),

        A =.. [_FA|ArgsA],

        functor(Attr0,FAttr,NAttr),

	domain(simpleFluent,Fs),
        functor(Attr2,FAttr,NAttr),
	member(Attr2,Fs),
	Attr2 =.. [FAttr|ArgsAttr0],
        	
        append(ArgsA,ArgsAttr,ArgsAttr0),
	Attr =.. [FAttr|ArgsAttr].

% show_models displays a list of models.  QueryMode tells how the output should
% be formatted, based on what command was issued by the user; 'sat' means
% that satisfying interpretations should be displayed simply as a set of
% atoms, without any reference to steps, while 'plan' means that fluents and
% actions should each be shown for each step of the plan.
% (Mode 'query' currently corresponds to the former command 'query', while
% mode 'plan' corresponds to the old command 'plan' and the new command
% 'query'.  This should be removed eventually since queries in the old sense
% are no longer used.)

show_models(Models,QueryMode) :-
   ( Models = [_Model1,_Model2|_Rest]
     -> show_models(Models,QueryMode,1,true)
   ; show_models(Models,QueryMode,1,false) ).

show_models([Model|Rest],QueryMode,Num,Label) :-
   common_statistics(_),

   % print a heading appropriate for the current mode
   ( Label == true
     -> ( QueryMode == (query)
	  -> format("Solution ~w:~n~n",[Num])
	; (QueryMode == sat),
	  ( ( value(mode,history); value(mode,transition) )
	    -> format("State ~w:~n~n",[Num])
	  ; format("Satisfying Interpretation ~w:~n~n",[Num]) ))
   ; true ),

   % recursively show all models in list
   ( (QueryMode == query)
     -> domain(step,Ts),
	show_model(Model,[none|Ts])
   ; value(mode,basic)
     -> show_model(Model,[none])
   ; show_model(Model,[none,0]) ),

   NextNum is Num+1,
   show_models(Rest,QueryMode,NextNum,Label).

show_models([],_,_,_).


show_model(_,[]) :- nl.

show_model(Model,[T|Ts]) :-
   % determine the atom numbers for the first fluent and first action in
   % time step T
   ( value(extended_atom_count,AC) -> true ; value(atom_count,AC) ),
   ( T \= none
     -> format("~w:  ",[T]), flush_output,
	( domain(simpleFluentAtomicFormula,[Fl0|_]),
	  find_atom_integer((T : Fl0),FirstFlNum),
	  ( NextT is T+1,
            find_atom_integer((NextT : Fl0),NextStepNum)
	  ; NextStepNum is AC + 1 )
	; FirstFlNum is AC + 1,
	  NextStepNum = FirstFlNum ),
	( domain(actionAtomicFormula,[Act0|_]),
	  find_atom_integer((T : Act0),FirstActNum)
        ; FirstActNum is AC + 1 ),
	iset(displayed_fluent,true)
   ; FirstFlNum = 0,
     ( domain(simpleFluentAtomicFormula,Fls),
       member(Fl0,Fls),
       find_atom_integer((0 : Fl0),FirstActNum)
     ; FirstActNum is AC + 1 ),
     NextStepNum = FirstActNum,
     iset(displayed_fluent,false) ),
   !,

   %%%  show positive fluents for each step T %%%

   % iterate through elements of model
   ( suffix([PosN|RestModel0],Model),

     % if next atom in model is >= the index of the first action for this time
     % step, it must be the case that all fluents for this time step have been
     % displayed already, so skip to next
     ( PosN >= FirstActNum
       -> RestModel1 = [PosN|RestModel0]
     ; find_atom_integer(PosA,PosN),

       % if time signature of current fluent matches current time step, display
       % the atom and continue
       ( ( T == none, PosA \= (_T : _Fl), PosFl = PosA
         ; T \= none, PosA = (T : PosFl) )
         -> in_show_list(PosA),
	    ( value(displayed_fluent,false)
	      -> iset(displayed_fluent,true)
	    ; true ),
	    show_fluent(PosFl),            
            fail
       % otherwise, stop displaying fluents for this time step
       ; RestModel1 = [PosN|RestModel0] ) )
   ; RestModel1 = [] ),
   !,

   %%% show negative fluents for each step T %%%
   ( show_spec(Specs),
     ( member(negative,Specs)
     ; member(all,Specs)
     ; member(-_,Specs) )
     -> ( iterate_negative(NegN,Model,FirstFlNum,FirstActNum),
          find_atom_integer(NegA,NegN),
          in_show_list((-NegA)),
          ( T == none -> NegFl = NegA ; NegA = (T: NegFl) ),
	  ( value(displayed_fluent,false)
	    -> iset(displayed_fluent,true)
	  ; true ),
          show_fluent((-NegFl)),
          fail
        ; true )
   ; true ),

   ( value(displayed_fluent,true) -> format("~n~n",[]) ; true ),
   retractall(value(displayed_fluent,_)),
   dm,
   % show actions for the current time instant
   ( T \= none,
     Ts \= []
     -> ( domain(actionAtomicFormula,AAFs) -> true ; AAFs = [] ),
	iset(displayed_action,false),
        ( suffix([ActN|RestModel2],RestModel1),
          ( ActN >= NextStepNum
            -> RestModel3 = [ActN|RestModel2]
	  ; find_atom_integer(ActA,ActN),
	    ActA = (T : Act),
	    member(Act,AAFs),
	    in_show_list(ActA)
	    -> get_all_attributes(T,Act,RestModel2,Attrs),
	       ( value(displayed_action,false)
	         -> iset(displayed_action,true),
	            format("ACTIONS:  ",[])
	       ; true ),
               ( Attrs == []
                 -> show_fluent(Act)
               ; display_attributes(Act,Attrs) ),
	       fail )
        ; RestModel3 = RestModel1 ),

        % show negative abnormality fluents for the current time instant
	( show_spec(Specs),
	  ( member(negative,Specs)
	  ; member(all,Specs)
	  ; member(-_,Specs) )
	  -> ( iterate_negative(NegN,RestModel1,FirstActNum,NextStepNum),
	       find_atom_integer(NegA,NegN),
	       in_show_list((-NegA)),
	       ( T == none -> NegFl = NegA ; NegA = (T: NegFl) ),
	       ( value(displayed_action,false)
	         -> iset(displayed_action,true),
		    format("ACTIONS:  ",[])
	       ; true ),
	       show_fluent((-NegFl)),
	       fail
	     ; true )
        ; true ),

	( value(displayed_action,true) -> format("~n~n",[]) ; true ),
	retractall(value(displayed_action,_)),
	show_model(RestModel3,Ts)

   ; show_model(RestModel1,Ts) ).

dm.
iterate_negative(Start,Model,Start,End) :-
   Start =< End,
   ( Model == []
   ; Model = [M|_],
     Start < M ).

iterate_negative(N,[Start|Ms],Start,End) :-
   Next is Start + 1,
   Next < End,
   iterate_negative(N,Ms,Next,End).

iterate_negative(N,Model,Start,End) :-
   Next is Start + 1,
   Next < End,
   iterate_negative(N,Model,Next,End).

get_all_attributes(T,Act,Model,Attrs) :-
   ( Act = (Act1 eq _V)
   ; Act1 = Act ), !,
   Act1 =.. [ActName|Args],
   consts_to_sorts(Args,ArgSorts),
   ActTemplate =.. [ActName|ArgSorts],
   findall( AttrStr,
	    ( attribute0(ActTemplate,AttrTemplate),
	      AttrTemplate =.. [AttrName|AttrArgSorts],
	      append(ArgSorts,RestAttrArgSorts,AttrArgSorts),
	      consts_to_sorts(RestAttrArgs,RestAttrArgSorts),
	      append(Args,RestAttrArgs,AttrArgs),
	      Attr =.. [AttrName|AttrArgs],
	      atom_integer((T:Attr eq V),N),
	      ordered_memberchk(N,Model),
	      get_attribute((Attr eq V),Act1,AttrStr) ),
	    Attrs ),
   !.

show_fluent(-F) :- 
   ( F =.. [eq,Arg1,Arg2]
     -> ( ab(Arg1)
          -> true
        ; Arg2 == true
	  -> format("-~w  ",[Arg1])
	; Arg2 == false
	  -> format("~w  ",[Arg1])
	% don't display the values a nonboolean fluent *doesn't* have
	; true )
   ; format("-~w  ",[F]) ),
   flush_output,
   !.

show_fluent(F) :-
   ( F =.. [eq,Arg1,Arg2]
     -> ( ab(Arg1)
          -> true
        ; Arg2 == true
	  -> format("~w  ",[Arg1])
	; Arg2 == false
	  -> format("-~w  ",[Arg1])
	; format("~w=~w  ",[Arg1,Arg2]) )
   ; format("~w  ",[F]) ),
   flush_output.


count_atoms(N) :- 
	findall(x,atom_integer(_,_),S), length(S,N).
count_rule_schemas(N) :- 
	findall(x,rule_schema(_),S), length(S,N).
count_rules(N) :-
	findall(x,db_fetch_rule((_<=_)),S), length(S,N).

complement_list([F|Fs],[G|Gs]) :-
        comp(F,G),
        complement_list(Fs,Gs).
complement_list([],[]).

comp(-F,F) :-
        !.
comp(F,-F).

% global variables
value(var_counter,-3).
value(dynamic_detected,false).
value(additive_detected,false).
value(mode,basic).
value(models_labeled,false).
value(max_no_of_clauses_in_internal_db,400000).
%value(max_no_of_clauses_in_internal_db,10).
value(max_no_of_clauses_to_optimize,8). % 8 default
value(query_counter,-1).
value(debug,true).


show_options :-
   nl,
   ( value(solver,Solver)
     -> format("    solver = ~w~n",[Solver]),
	( value(solver_opts(Solver),Opts)
	  -> format("    solver_opts(~w) = '~w'~n",[Solver,Opts])
	; true )
   ; true ),
   member(Option,[num,dir,file_io,timed,verbose,compact,optimize,
		  eliminate_tautologies]),
   ( value(Option,Value)
     -> format("    ~w = ~w~n",[Option,Value])
   ; true ),
   fail.

show_options.

show_parameters :- show_options.


reset_options :-
   % load default options from options.std file in ccalc directory, then
   % from user's options.std (if any), overriding defaults
   value(ccalc_dir,D),
   format_to_atom(DefaultOptsFile,'~woptions.std',[D]),
   compile(DefaultOptsFile),
   environ('PWD',P),
   format_to_atom(UserOptsFile,'~w/options.std',[P]),
   ( common_file_exists(UserOptsFile)
     -> compile(UserOptsFile)
   ; true ).

reset_parameters :- reset_options.


show_atoms :-
	atom_integer(A,I),
  	  format("~n~d. ~w",[I,A]),
	fail.
show_atoms.


show_problems :- show_problem(_).

show_problem(Label) :-
 % dmiles - TODO (define)
	show_plan(Label),
	fail.
show_problem(Label) :-
	show_query(Label),
	fail.
show_problem(_).

show_queries :- show_query(_).

show_query(Label) :-
   query_problem(Label,MaxStep,Rules),
   format("~n<<query ~q>>",[Label]),
   format("~nmaxstep :: ~q;",[MaxStep]),
   show_formulas(Rules),
   write('.'),
   nl,
   fail.
show_query(_).

show_formulas((A;B)) :-
  !,
  show_formulas(A),
  write(';'),
  show_formulas(B).
show_formulas(A) :-
  format("~n  ~w",[A]).

show_clauses :-
   clause(Cs),
   nl,
   show_clause(Cs),
   fail.
show_clauses.

/*
show_clauses_external :-
        safe_see('ccsat.in1'),
	repeat,
          read_line(Cs),
	  ( Cs = end_of_file
	      -> !, seen
	  ; get_numbers(Cs,Ns),
	    common_select(0,Ns,Ms),
	    nl,
	    show_clause(Ms),
	fail ).
*/

show_clause([]).
show_clause([N|Ns]) :-
	show_literal(N),
	( Ns = []
	    -> write('.')
	; write(' ++ ') ),
	show_clause(Ns).

show_literal(N) :-
	( N = 0
	    -> write(false)
	; N > 0
	    -> ( find_atom_integer(A,N)
	           -> show_atom(A)
	       ; write(N) )
	; M is 0-N,
	  ( find_atom_integer(A,M)
	      -> show_atom(-A)
	  ; write(N) ) ).


% show_atom(X) :- write(X), !.

show_atom(A) :-
   ( ( A = (T:(A0 eq true)) ; A = -((T:(A0 eq false))) )
     -> format('~w:~w',[T,A0])
   ; ( A = (T:(A0 eq false)); A = -((T:(A0 eq true))) )
     -> format('-(~w:~w)',[T,A0])
   ; A = -((T:(A0 eq V)))
     -> format('~w:~w\\=~w',[T,A0,V])
   ; A = (T : (A0 eq V))
     -> format('~w:~w=~w',[T,A0,V])

   ; ( A = (A0 eq true) ; A = -((A0 eq false)) )
     -> format('~w',[A0])
   ; ( A = (A0 eq false); A = -((A0 eq true)) )
     -> format('-~w',[A0])
   ; A = -((A0 eq V))
     -> format('~w\\=~w',[A0,V])
   ; A = (A0 eq V)
     -> format('~w=~w',[A0,V])
   ; write(A) ),
   !.

show_schemas :-
	rule_schema(Rule),
	  write(Rule),
	  nl,
	fail.
show_schemas.


/* Since we may change the mapping from atoms to integers in transition
mode (we do so if we introduce new variables), it is useful to 
keep around the old mapping tables and use them when displaying rules
in transition mode.  The alternative would be to remap the rules along
with the clauses, but that seems like a waste.  Except for display, the 
rules aren't needed after the clauses are generated. */

%find_saved_integer_atom(N,A) :- 
%	( saved_integer_atom(N,A)
%	    -> true
%	; integer_atom(N,A) ), 
%	!.

find_saved_atom_integer(A,N) :- 
   ( saved_atom_integer(A,N)
     -> true
   ; atom_integer(A,N) ),
   !.

show_rules(H) :-
	( H = -A
	    -> db_fetch_rule((N<=L)),
	       ( N > 0 
                   -> fail
       	       ; M is 0-N,
	         find_saved_atom_integer(A1,M),
	         A = A1,
	         nl, show_rule((N,L)), fail )
	; db_fetch_rule((N<=L)),
	  ( N < 0
	      -> fail
	  ; N = 0 
	      -> H = false,
	         nl, show_rule((N,L)), fail 
	  ; find_saved_atom_integer(A1,N),
	    H = A1,
	    nl, show_rule((N,L)), fail ) ).
show_rules(_).

show_rules :-
	db_fetch_rule((N<=L)), 
	  nl, 
	  show_rule((N,L)), 
	fail.
show_rules.

show_rule((N,L)) :-
   show_head([N]), 
   db_fetch_rule(rule_body(L,Ns1)), 
   ( Ns1 == []
     -> write('.')
   ; write(' <= '),
     show_body(Ns1) ).

show_query_rules :-
	db_fetch_query_rule((N<-L)), 
	  nl, 
	  show_query_rule((N,L)), 
	fail.
show_query_rules.

show_query_rule((N,L)) :-
   show_head([N]), 
   write(' <= '), 
   db_fetch_query_rule(query_rule_body(L,Ns1)), 
   show_body(Ns1).


show_head([N|Ns]) :-
	show_rule_literal(N),
	( Ns = [] 
	    -> true
	; write(' ++ ') ),
	show_head(Ns).
show_head([]).

show_body([N|Ns]) :-
	show_rule_literal(N),
	( Ns = []
	    -> true
	; write(',') ),
	show_body(Ns).
show_body([]) :- write('.').

show_rule_literal(N) :-
	( N = 0
	    -> write(false)
	; N > 0
	    -> ( find_saved_atom_integer(A,N)
	           -> show_atom(A)
	       ; write(N) )
	; M is 0-N,
	  ( find_saved_atom_integer(A,M)
	      -> show_atom(-A)
	  ; write(N) ) ).


next(T,T1) :- T1 is T+1.

% time(T) :- domain(time,Ts), member(T,Ts).

rule_bodies_for_literal(N,Nss) :-
   findall(Ns,( db_fetch_rule((N<=L)), db_fetch_rule(rule_body(L,Ns)) ),Nss).

query_rule_bodies_for_literal(N,Nss) :-
   findall(Ns,( db_fetch_query_rule((N<-L)), db_fetch_query_rule(query_rule_body(L,Ns)) ),Nss).

find_free_variables(Wff,Vs) :-
   find_free_variables(Wff,[],Vs), close_list(Vs).

find_free_variables(X,_Bound,_Vs) :- 
   var(X),	
   !.
find_free_variables(Term,Bound,Vs) :-
   atom(Term), 
   var_sort(Term,_S),
   ( member(Term,Bound)
     -> true
   ; member(Term,Vs) ),
   !.
find_free_variables([/\X|W],Bound,Vs) :-
   !,
   find_free_variables(W,[X|Bound],Vs).

% find_free_variables([\/'L'\/'B'|loc('B1')eq 'B'],[],_60854) ? 

find_free_variables([\/X|W],Bound,Vs) :-
   !,
   find_free_variables(W,[X|Bound],Vs).

find_free_variables(Term,Bound,Vs) :-
   functor(Term,_,N),
   free_variables_arg(Term,0,N,Bound,Vs).


free_variables_arg(_Term,N,N,_Bound,_Vs) :-
   !.
free_variables_arg(Term,M,N,Bound,Vs) :-
   M1 is M+1,
   arg(M1,Term,A),
   find_free_variables(A,Bound,Vs),
   free_variables_arg(Term,M1,N,Bound,Vs).

close_list([]) :-
	!.
close_list([_|Xs]) :- close_list(Xs).



% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%
% Procedures related to show/1.
% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%

in_show_list(Atom) :-
   % if no show spec was specified, default to 'positive'
   ( show_spec(Specs) ->
     true
   ; Specs = [positive] ),

   % extract the constant from the atom
   ( Atom = (_T : Const)
   ; Atom = -((_T:Const))
   ; Atom = -Const
   ; Const = Atom ),
   !,

   % if the atom is an action, check to see whether it's an abnormality
   % predicate.  If so, display it only if the show list includes 'ab' or
   % that specific abnormality predicate.  If it's a normal action, always
   % display if it's positive, and never if it's negative.
   % 
   % Update: All positive normal (i.e. non-ab) actions are not shown anymore.
   % We additionally check if the action atom is a ground instance of one of 
   % the Specs naming specific constants. (i.e. if the action has been 
   % specified using the "show" command/predicate.
   % -- Selim T. Erdogan, 2 Mar 2008
   %
   ( ( domain(actionAtomicFormula,Acts),
       member(Const,Acts) )
     -> ( ab(AbTerm),
	  is_ground_instance(Const,AbTerm)
	  -> member(Spec,Specs),
	     ( Spec == all
	       -> true
	     ; Spec == ab
	       -> Atom \= -(_)
	     ; is_ground_instance(Atom,Spec) )
	; Atom \= -(_),
	  Const \= (contribution(_,_) eq _),
	  Const \= (accumulatedContribution(_,_) eq _),
          % The updated part is the addition below
          % -- Selim T. Erdogan, 2 Mar 2008
          member(Spec,Specs),
          ( Spec == all
          ; Spec == positive
          ; is_ground_instance(Atom,Spec) ) )


   % If Atom isn't an action, then it's displayed if:
   %    'all' is one of the show specs; or
   %    'positive' is one of the show specs, and Atom is neither the negation
   %       of a positive atom nor a constant with a value of 'none'; or
   %    'negative' is one of the show specs, and Atom is the negation of a
   %       positive atom or a constant with a value of 'none'; or
   %    Atom is a ground instance of one of the Specs naming specific
   %       constants.
   ;  member(Spec,Specs),
      ( Spec == all
      ; Spec == positive,
	\+ Atom = -_Atom2,
	\+ Const = (_FL eq none)
      ; Spec == negative,
	( Atom = -_Atom2 ; Const = (_FL eq none) )
      ; is_ground_instance(Atom,Spec) )), !.


% is_ground_instance(Atom,Spec) decides whether Atom is a ground instance of
% Spec.  This means that for each part of the Atom (time step, constant name,
% arguments, and value), the corresponding part of the Spec must be the same
% or a variable of the same type.  The Spec may omit the step, argument list,
% or value, in which case that part of the Spec matches any corresponding
% part in the atom.  (However, the Spec cannot omit some of the arguments and
% not others.)

is_ground_instance([],[]) :- !.

is_ground_instance([A|As],[S|Ss]) :-
   !,
   is_ground_instance(A,S),
   is_ground_instance(As,Ss).

% A ground term in a Spec matches itself in the Atom.
is_ground_instance(Atom,Atom).

% A variable in the Spec matches a ground term of the same sort in the Atom.
is_ground_instance(Atom,Spec) :-
   var_sort(Spec,Sort),
   const_to_sort(Atom,Sort).

% If Spec omits a time step, it matches any time step in Atom.
is_ground_instance(Atom,Spec) :-
   \+ functor(Spec,:,_),
   Atom = (_T2: FL),
   !,
   is_ground_instance(FL,Spec).

% If Spec omits a value, it matches any value in Atom.
is_ground_instance(Atom,Spec) :-
   \+ functor(Spec,eq,_),
   Atom = (FL eq _Val),
   !,
   is_ground_instance(FL,Spec).

% If Spec omits an argument list, it matches any argument list in Atom.
is_ground_instance(Atom,Spec) :-
   Spec =.. [SF],
   Atom =.. [AF|AArgs], AArgs \= [],
   !,
   is_ground_instance(AF,SF).
   
% If both Atom and Spec have an argument list, they must match.
is_ground_instance(Atom,Spec) :-
   Atom =.. [AF|AArgs], AArgs \= [],
   Spec =.. [SF|SArgs], SArgs \= [],
   !,
   is_ground_instance(AF,SF),
   is_ground_instance(AArgs,SArgs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% valid_spec confirms whether a list of show specifications is valid.  Each
% element of the list should be 'positive', 'negative', 'all', 'none', or
% a term for which the ground terms are fluents.
%
% Update: A term for which the ground term is an action or an attribute
% used to generate a warning, saying that these are "not filtered", meaning
% that the appearance of such terms in a show specification would simply
% be ignored.  Now such specifications are taken into accound (see in_show_list/1)
% so these warnings are not shown anymore.
% -- Selim T. Erdogan, 2 Mar 2008
%
% If there is a constant named 'positive', 'negative', 'all', 'none', or 'ab',
% and a show statement contains that name, CCalc will assume that the
% show statement refers to the show keyword, but print a warning message
% informing the user that the show statement is ambiguous.  If the show
% statement contains that constant together with a timestamp, argument list,
% and/or value (like 'show positive=true'), there is no ambiguity and no
% warning is displayed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

valid_spec([Spec]) :-
   member(Spec,[positive,negative,ab,all,none]),
   !,
   ( consts(Spec,_)
     -> nonfatal_error("~s~w~s~w~s~n~s~w~s~n~s~w~s~s~n~s~n",
           ["Show spec '",Spec,"' is ambiguous.  '",Spec,
            "' is a reserved keyword",
            "   in show statements; to refer to the constant '",Spec,
            "', please include a",
            "   timestamp, argument list, or value (for example, 'show ",
            Spec,"=V'"," for some",
            "   variable V of the appropriate type)."]),
        fail
   ; true ).

valid_spec([-Term]) :- 
   valid_spec([Term]).

valid_spec([Term]) :- 
   % A spec may include a time step; if so, it must be a valid step
   ( Term = (T: Term0)
     -> domain(step,Ts),
	member(T,Ts)
   ; Term0 = Term ),

   % A spec may include a value
   ( Term0 = (Fl eq Val)
     -> Term1 = Term0
   ; Fl = Term0,
     Term1 = (Term0 eq Val) ),

   functor(Fl,F,N),
   ( functor_spec(F/N),
     find_free_variables(Term1,[V|Vs])
     -> renaming_subst([V|Vs],Sub),
	subst_free(Term1,Term2,Sub)
   ; N = 0
     -> consts(F,N1),
	% if constant F has N1 arguments, make a list of that many variables
	nth(N1,Vs,X), last(Vs,X), !,
	Term11 =.. [F|Vs],
	Term2 = (Term11 eq Val)
   ; Term2 = Term0 ),
   !,

   ( ( domain(simpleFluentAtomicFormula,Fs)
     ; domain(sdFluentAtomicFormula,Fs) ),
     member(Term2,Fs)
     -> true 
   ; domain(rigidAtomicFormula,Rs), member(Term2,Rs)
     -> var(T)
   ; domain(actionAtomicFormula,As), member(Term2,As),
     ( ( var(N1) -> AbN = N ; AbN = N1 ),
       ab(AbTerm),
       functor(AbTerm,F,AbN)
       -> true
     % Update: Now we allow for filtering of actions
     % -- Selim T. Erdogan, 2 Mar 2008
     %
     % ; nonfatal_error("Actions and attributes are not filtered!",[]) )
       ; true )
   ; domain(attributeAtomicFormula,Ats), member(Term2,Ats)
     % Update: Now we allow for filtering of actions
     % -- Selim T. Erdogan, 2 Mar 2008
     %
     % -> nonfatal_error("Actions and attributes are not filtered!",[]) ).
     -> true ).

valid_spec([Term]) :-
   nonfatal_error("Show spec (~w) does not have an instance.~n",[Term]).

valid_spec([]).

valid_spec([H|T]) :-
         valid_spec([H]),
         !,
         valid_spec(T).

valid_spec(Spec) :-
	nonfatal_error("Show spec (~w) does not have an instance.",[Spec]).

% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%
% READ procedures.
% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%

bind_vars_to_names([=(Var,Var)|Names]) :-
   bind_vars_to_names(Names).
bind_vars_to_names([]).

read_and_expand(Term,ExpandedTerm) :-
   read_term(Term,[variable_names(Names)]),
   bind_vars_to_names(Names), 
   do_term_expansion0(Term,TermTree),
   leaf_element(TermTree,ExpandedTerm).

do_term_expansion0(Term,NewTerm) :-
   do_term_expansion1(Term,Term1), 
   do_term_expansion(Term1,NewTerm).

do_term_expansion1((:- query Q), (:- query Q)) :- 
   \+ value(mode,history), !.

do_term_expansion1((:- nmquery Q), (:- nmquery Q)) :- 
   \+ value(mode,history), !.

do_term_expansion1((:- macros A), (:- macros A)) :- !.

do_term_expansion1(Term,NewTerm) :-
   do_one_expansion1(Term,Term1,Flag),
   ( Flag == true 
     -> do_term_expansion1(Term1,NewTerm)
   ; NewTerm = Term1 ).

do_one_expansion1(Term,NewTerm,Flag) :-
   ( var(Term)            %% Normally, Term will not be a variable;
     -> NewTerm = Term         %% but see calls in plan/0,1.
   ; macro1(Term,NewTerm) 
       -> Flag = true
     ; functor(Term,F,N),      % given Term
       functor(NewTerm,F,N),   % given F,N
       do_one_expansion_arg1(Term,1,N,NewTerm,Flag) ).

% M is the arguemnt index, N is the length of the argument,
% D will the new term with argument expanded
do_one_expansion_arg1(_C,M,N,_D,_Flag) :-
   M > N, 
   !.
do_one_expansion_arg1(C,M,N,D,Flag) :-
   arg(M,C,A),
   do_one_expansion1(A,B,Flag),
   arg(M,D,B),
   M1 is M+1,
   do_one_expansion_arg1(C,M1,N,D,Flag).

%%%%%%%%%%%%%

do_term_expansion((:- query Q), (:- query Q)) :- 
   \+ value(mode,history), !.

do_term_expansion((:- nmquery Q), (:- nmquery Q)) :- 
   \+ value(mode,history), !.

do_term_expansion((:- macros A), (:- macros A)) :- !.

do_term_expansion(Term,NewTerm) :-
   do_one_expansion(Term,Term1,Flag),
   ( Flag == true 
     -> do_term_expansion(Term1,NewTerm)
   ; NewTerm = Term1 ).

do_one_expansion(((maxstep):: M), ((maxstep):: M),_) :- !.  
      % why not do_term_expansion? - should inspect into M 

do_one_expansion(Term,NewTerm,Flag) :-
   ( var(Term)            %% Normally, Term will not be a variable;
     -> NewTerm = Term         %% but see calls in plan/0,1.
   ; ( macro(Term,Call,NewTerm), call(Call))
       -> Flag = true
     ; functor(Term,F,N),      % given Term
       functor(NewTerm,F,N),   % given F,N
       do_one_expansion_arg(Term,1,N,NewTerm,Flag) ).

% M is the arguemnt index, N is the length of the argument,
% D will the new term with argument expanded
do_one_expansion_arg(_C,M,N,_D,_Flag) :-
   M > N, 
   !.
do_one_expansion_arg(C,M,N,D,Flag) :-
   arg(M,C,A),
   do_one_expansion(A,B,Flag),
   arg(M,D,B),
   M1 is M+1,
   do_one_expansion_arg(C,M1,N,D,Flag).


leaf_element((A;B),X) :-
	!,
	(leaf_element(A,X); leaf_element(B,X)).
leaf_element(A,A).


read_rules(Rules) :-
%	nl,
        write('enter query (then ctrl-d)'), 
        nl, 
	read_formulas([],Rules).

read_formulas(SoFar,Formula) :-
   read_and_expand(Term0,_Term),
   ( Term0 == end_of_file
     -> format("~n^D~n",[]),
        reverse(SoFar,Formula)
   ; read_formulas([Term0|SoFar],Formula)).


read_time(Time) :-
	nl,
	write('enter time (>= 1)'),
	nl,
	read_and_expand(_T0,T),
	( T == end_of_file
	    -> Time = 1
	; integer(T), T >= 1 
	    -> Time = T
	; read_time(Time) ).


fatal_error(FormatString,Args) :-
	nl,
	format("Error: ",[]),
	format(FormatString,Args),
	nl,
	close_abort.

nonfatal_error(FormatString,Args) :-
	nl,
	format("Warning: ",[]),
	format(FormatString,Args),
	nl.



% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%
% Shifting atoms and clauses for transition mode.
% - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -%

% formerly formulate_theory/1

shift_atoms_and_clauses(Time) :-
   % if we previously shifted any atoms or clauses, retract them
   ( retract((atom_integer(_,_) :- atom_integer_extension(_,_)))
     -> true
   ; true ),
   retractall(atom_integer_extension(_,_)),
   retractall(clause(_)),

   ( Time == 0
     -> % if maxtime is 0, not only do we not need to shift, but we also need
	% to ignore clauses corresponding to time 1
	value(atom_count_0,AC),
	value(clause_count_0,CC)
   ; Time == 1
     -> % if maxtime is 1, we don't need to shift, just write all of the
	% clauses we've already generated
	value(atom_count,AC),
	value(clause_count,CC)
   ; fail ),
   !,
   iset(extended_atom_count,AC),
   iset(extended_clause_count,CC),

   write_shifted_clauses(Time,0,0).

shift_atoms_and_clauses(Time) :-
   value(atom_count_0,AC0),
   value(atom_count,AC),
   Shift is AC - AC0,
   EAC is (Shift * Time) + AC0,
   iset(extended_atom_count,EAC),

   value(clause_count_0,CC0),
   value(clause_count,CC),
   ECC is ((CC - CC0) * Time) + CC0,
   iset(extended_clause_count,ECC),

   enumerate_atom_extension(1,Time,Shift),
   assertz( (atom_integer(A,I) :- atom_integer_extension(A,I)) ),
   value(rigid_count,RC),
   write_shifted_clauses(Time,Shift,RC).

% Shift the atoms to each new time step.  The first argument is the number
% of steps for the current shift; the second is the maximum time step to which
% it should shift; and the third is the number of atoms which are being
% shifted.  For each atom that is to be shifted (i.e. all atoms with an index
% greater than static0_atom_count), it simply adds the shift size to the atom
% to get its new index for each successive time step.

enumerate_atom_extension(MaxTime,MaxTime,_) :- !.

enumerate_atom_extension(Time,_,Shift) :-
   IShift is Time * Shift,
   value(atom_count_0,AC0),
   atom_integer((T:A),I),
   I > AC0,
   ShiftedT is T + Time,
   ShiftedI is I + IShift,
   assertz(atom_integer_extension((ShiftedT:A),ShiftedI)),
   fail.

enumerate_atom_extension(Time,MaxTime,Shift) :-
   Time2 is Time + 1,
   enumerate_atom_extension(Time2,MaxTime,Shift).

write_shifted_clauses(_,_,_) :-
   assertz((clause(C) :- clause0(C))),
   fail.

write_shifted_clauses(0,_,_) :- !.

write_shifted_clauses(_,_,_) :-
   assertz((clause(C) :- clause1(C))),
   fail.

write_shifted_clauses(1,_,_) :- !.

write_shifted_clauses(MaxTime,Shift,Base) :-
   write_shifted_clauses(2,MaxTime,Shift,Base).

write_shifted_clauses(Time,_MaxTime,Shift,Base) :-
   TShift is (Time - 1) * Shift,
   assertz(( clause(C) :-
		clause1(C1),
		map_clause_shift(C1,TShift,Base,C) )),
   fail.

write_shifted_clauses(MaxTime,MaxTime,_,_) :- !.

write_shifted_clauses(Time,MaxTime,Shift,Base) :-
   Time1 is Time + 1,
   write_shifted_clauses(Time1,MaxTime,Shift,Base).

% shift integers in clausal form
map_clause_shift([],_,_,[]).
map_clause_shift([M|Ms],Shift,BC,[N|Ns]) :-
   ( M > 0
     -> ( M =< BC 
          -> N = M 
        ; N is M + Shift )
   ; P is 0 - M, 
   ( P =< BC 
     -> N = M 
   ; N is M - Shift ) ),
   map_clause_shift(Ms,Shift,BC,Ns).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  System and File Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% system_value calls system/2 and discards the value.  This is necessary
% because system/1 fails when the Call fails, whereas system/2 always succeeds
% (and returns failure in the Value argument if the call failed).

system_value(Call) :- system(Call,_Value).


% time_system(Call,Time) executes a system command and times it.  There is an
% optional second parameter which can be 'background' if the command is to
% be executed in the background, or 'nobackground' if not; if unspecified,
% the default is 'nobackground.

time_system(Call,Time) :- time_system(Call,nobackground,Time).

% time_system(Call,nobackground,Time) executes Call in the foreground, and
% when it terminates, parses the time output and returns the execution time
% of Call

time_system(Call,nobackground,Time) :-
   common_tmpname(Temp),
   format_to_atom(Call2,"(time sh -c '~w') 2> ~w",[Call,Temp]),
   system_value(Call2),
   safe_see(Temp),
   parse_time(Time), !,
   seen,
   common_file_delete(Temp).

% time_system(Call,background,Time) executes Call in the background, and
% creates a temporary file to which the time output will be redirected,
% the filename of which it returns as Time.  (The time output can't be parsed
% here since this procedure doesn't wait for the background call to terminate.)

time_system(Call,background,Time) :-
   common_tmpname(Time),
   format_to_atom(Call2,"(time sh -c '~w') 2> ~w &",[Call,Time]),
   system_value(Call2).

% with a fourth argument, time_system also creates a lockfile (with an
% automatically generated filename) which will be deleted when the Call is
% complete, and returns the name of the Lockfile.  Thus other procedures
% can wait for Call to complete before proceeding.

time_system(Call,background,Time,Lockfile) :-
   common_tmpname(Lockfile),
   format_to_atom(LockCall,"lockfile ~w",[Lockfile]),
   system_value(LockCall),
   common_tmpname(Time),
   format_to_atom(BkgdCall,"((time sh -c '~w') 2> ~w ; rm -f ~w) &",
      [Call,Time,Lockfile]),
   system_value(BkgdCall).

% wait for a lockfile to be released

wait(Lockfile) :-
   format_to_atom(Call,"lockfile ~w ; rm -f ~w",[Lockfile,Lockfile]),
   system_value(Call).


% parse the time output in file Time.

parse_time(Time) :-
   read_line(Line),
   ( Line == end_of_file
     -> !, fail
   ; ( prefix(Line,"user",RestLine)
   ; prefix(Line,"sys",RestLine) ),
     drop_spaces(RestLine,TimeStr),
     ( ( between(TimeStr,HrStr,RestTimeStr,":"),
         between(RestTimeStr,MinStr,SecStr,":")
       ; between(TimeStr,HrStr,RestTimeStr,"h"),
         between(RestTimeStr,MinStr,RestTimeStr2,"m"),
         between(RestTimeStr2,SecStr,_,"s") )
       -> get_number(HrStr,Hr,_),
          get_number(MinStr,Min,_),
          get_number(SecStr,Sec,_),
          Time1 is 3600*Hr+60*Min+Sec
     ; ( between(TimeStr,MinStr,SecStr,":")
       ; between(TimeStr,MinStr,RestStr,"m"),
         between(RestStr,SecStr,_,"s") )
       -> get_number(MinStr,Min,_),
          get_number(SecStr,Sec,_),
          Time1 is 60*Min+Sec
     ; get_number(TimeStr,Time1,_) ),
     ( parse_time(Time2)
       -> Time is Time1 + Time2
     ; Time is Time1 )
   ; parse_time(Time) ).


% make_pipe creates a pipe (with an automatically generated filename) using the
% Unix 'mkfifo' command.

make_pipe(P) :-
   common_tmpname(P),
   format_to_atom(Call,"mkfifo ~w",[P]),
   system(Call).


% rm_pipe closes and removes a file or pipe.  The argument can be either
% the filename of the file/pipe or a stream connected to the file/pipe.

rm_pipe(P) :-
   ( atom(P),
     ( common_file_exists(P)
       -> common_file_delete(P)
     ; true ) ).
%   ; current_stream(F,_,P)
%     -> close(P),
%	common_file_delete(F) ).



% print_stream prints the contents of a stream.  Its argument should be a
% stream which is already open (though it doesn't have to be the current input
% stream).  This procedure is primarily for debugging purposes.

print_stream(P) :-
   current_input(C),
   set_input(P),
   repeat,
      read_line(L),
      ( L \= end_of_file
	-> format("~s~n",[L]),
	   fail
      ; true ),
   set_input(C).


% close_all_streams closes all open files.  It checks the name of every open
% stream to ensure it is not an integer, which SWI-Prolog uses for standard I/O
% streams; nor '', which SICStus uses for those; nor ccalc.pl, since we don't
% want to close ccalc.pl while we're consulting it.  If a filename passes
% all of these tests, the stream is closed.

close_all_streams :-
   value(ccalc_dir,CCDir),
   format_to_atom(CCalcName,"~wccalc.pl",[CCDir]),
   ( current_stream(F,_,S),
     \+ integer(F),
     F \= '',
     F \= CCalcName,
     close(S),
     fail
   ; true ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Other Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-----------------------------------------------------------------------------%
%  check_grules
%-----------------------------------------------------------------------------%

check_grules :-
	(A<=B), 
	( common_ground((A<=B))
            -> true
	; nl, write('Not Ground: '), write((A<=B)) ),
	fail.
check_grules.


%-----------------------------------------------------------------------------%
%  show_grules
%-----------------------------------------------------------------------------%

show_grules :-
	(A<=B), 
        nl, 
	write((A<=B)),
	fail.
show_grules.

%-----------------------------------------------------------------------------%
%  max, min
%-----------------------------------------------------------------------------%

max(A, B, C) :- ( A < B -> C = B; C = A ).
max(A, B, C, D) :- max(A,B,E), max(E,C,D).

min(A, B, C) :- ( A < B -> C = A; C = B ).


%-----------------------------------------------------------------------------%
%  functor_spec
%-----------------------------------------------------------------------------%

functor_spec(Spec) :-
	Spec = F/N, atom(F), integer(N), N >= 0, 
	domain_schema(_,L), functor(T,F,N), member(T,L).


%-----------------------------------------------------------------------------%
%  close_abort
%-----------------------------------------------------------------------------%

close_abort :- seen, told, ( value(noabort,true) -> fail ; abort ).


%-----------------------------------------------------------------------------%
%  next_time
%-----------------------------------------------------------------------------%

next_time(T,N,T) :- T =< N.
next_time(M,N,T) :- M < N, M1 is M+1, next_time(M1,N,T).

next_time(T,T).
next_time(M,T) :- M1 is M+1, next_time(M1,T).


%-----------------------------------------------------------------------------%
%  prefix
%-----------------------------------------------------------------------------%

prefix([C|Cs],[C|Ps],After) :-
	prefix(Cs,Ps,After).
prefix(Cs,[],Cs).


%-----------------------------------------------------------------------------%
%  common_sublist
%-----------------------------------------------------------------------------%

common_sublist([],[]).
common_sublist([E|Sublist],[E|List]) :-
	common_sublist(Sublist,List).
common_sublist(E,[_|List]) :-
	common_sublist(E,List).


%-----------------------------------------------------------------------------%
%  exact_sublist
%-----------------------------------------------------------------------------%

exact_sublist(List,Pat,After) :-
	prefix(List,Pat,After).
exact_sublist([_|Cs],Pat,After) :-
	exact_sublist(Cs,Pat,After).


%-----------------------------------------------------------------------------%
%  between
%-----------------------------------------------------------------------------%

between(List,Before,After,Between) :-
	exact_sublist(List,Before,Rem1),
	prefix(Rem1,Between,Rem2),
	prefix(Rem2,After,_).


%-----------------------------------------------------------------------------%
%  print_list
%-----------------------------------------------------------------------------%

%^jo- test routine, for debugging only.
print_list([L|Ls]) :-
   ( is_list(L) 
     -> print_list(L)
   ; format("~n~w  ", [L])),
   print_list(Ls). 
print_list([]) :-
   format("~n",[]).

% memberCheck(L,E)
%       List L has as an element some term that is identical to term E.
%       Succeeds once at most.


%-----------------------------------------------------------------------------%
%  member_check
%-----------------------------------------------------------------------------%

member_check(X,[X|_Xs]) :- !.
member_check(X,[Y|Ys]) :- X \= Y, member_check(X,Ys).


%-----------------------------------------------------------------------------%
%  display_atoms
%-----------------------------------------------------------------------------%

display_atoms([L|Ls]) :-
   atom_integer(A, L),
   format("~w  ", [A]),
   display_atoms(Ls).
display_atoms([]) :- format("~n", []).
  

%-----------------------------------------------------------------------------%
%  subtract
%-----------------------------------------------------------------------------%

subtract([], _, []).
subtract([Element|Elements], Set, Difference) :-
        member_check(Element,Set),
        !,
        subtract(Elements, Set, Difference).
subtract([Element|Elements], Set, [Element|Difference]) :-
        subtract(Elements, Set, Difference).

ordered_memberchk(X,[X|_]) :- !.
ordered_memberchk(X,[Y|Ys]) :-
   X > Y,
   ordered_memberchk(X,Ys).


append_to_each(_,[],[]).

append_to_each(K,[C|Cs],[[K|C]|Cs2]) :-
   append_to_each(K,Cs,Cs2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_dirs.
:- initialize.
:- reset_options.
:- format("~n~nCausal Calculator: Version 2.0.~n",[]).
:- format("Type 'help.' for online help.~n~n~n",[]).
