% Authors: Guenter Kniesel, Paulo Moura
% Date: 25.06.2006

/*
 * This file contains predicates for working with Logtalk entities.
 */
:- object(utils4entities).

:- public([
	source_file_entity/3,           % File, Line, Entity
	entity_source_file/3,           % Entity, File, Line
	entity_multifile_predicate_source_file/6,
	entity/1,
	entity_property/3,

	visible_in_entity/3,	        % Entity, Name, Arity
	declared_in_entity/4,           % Entity, Name, Arity, DeclaringEntity
	referenced_but_undeclared/3,    % Entity, Name, Arity
	declared_but_undefined/3,       % Entity, Name, Arity
	defined_in_entity/2,            % Entity, Head
	defined_in_entity/3,            % Entity, Name, Arity
	defined_in_entity/4,            % Entity, Name, Arity, DefiningEntity
	defined_in_files/4,             % Entity, Name, Arity, Locations
	defined_in_file/6,              % Entity, Name, Arity, Nth,File,StartingLine
	declared_in_file/4,             % Entity, Name, Arity, Location=[File-[Line]]

	assert_in_entity/2,             % Entity, Head
	assert_in_entity/3,             % Entity, Head, Body
	clause_in_entity/2,             % Entity, Head
	clause_in_entity/3,             % Entity, Head, Body
	retract_in_entity/2,            % Entity, Head
	retract_in_entity/3,            % Entity, Head, Body
	retractall_in_entity/2,         % Entity, Head
	call_in_entity/2,               % Entity, Goal
	call_and_report_contex_entity/1,% Goal
	report_contex_entity/1,         % Entity
	listing_in_entity/2,            % Entity, FunctorOrHeadOrFkt/Arity

	copy_entity_predicate/3,        % SrcEntity, TargetEntity, Head
	move_entity_predicate/3         % SrcEntity, TargetEntity, Head
	]
).

:- uses(list, [member/2, memberchk/2]).
% :- use_module( library(listing), [listing/1]).

:- uses(user, [all/1, assert1T/1, ctc_error/2, log_on_stdout/2]).

:- meta_predicate( call_and_report_contex_entity(0) ).

% :- module_transparent(call_and_report_contex_entity/1).
call_and_report_contex_entity(Goal) :-
% SWI-Prolog:
	% context_entity(M),
	% log_on_stdout('Calling ~w.~n',[M:Goal]),
	% call(M:Goal).
% Logtalk (Goal = Self::Call at the call site!):
	log_on_stdout('Calling ~w.~n',[Goal]),
	call(Goal).

%:- module_transparent(report_contex_entity/1).
%report_contex_entity(M) :- context_entity(M),
%    log_on_stdout('Context module = ~w.~n',[M]).



% file + directory + line number -> entity + entity kind + entity properties
% (see the Logtalk Reference Manual -> Grammar for entity property names)
%
% get_entity_data(+atom, +atom, +integer, ?entity_identifier, ?atom, -list)

get_entity_data(File, Directory, Line, Entity, Kind, Properties) :-
	entity_property(Entity, Kind, file(File, Directory)),
	entity_property(Entity, Kind, lines(Begin, End)),
	Begin =< Line, End >= Line,
	findall(
		Property,
		entity_property(Entity, Kind, Property),
		Properties
	).


entity(Entity) :-
	current_object(Entity).
entity(Entity) :-
	current_protocol(Entity).
entity(Entity) :-
	current_category(Entity).


entity_property(Object, object, Property) :-
	catch(object_property(Object, Property), _, fail).
entity_property(Category, category, Property) :-
	catch(category_property(Category, Property), _, fail).
entity_property(Protocol, protocol, Property) :-
	catch(protocol_property(Protocol, Property), _, fail).


%% source_file_entity(+File,+Line,?Entity)
%% source_file_entity(+File,-Line,?Entity)
%
% Entity is an entity (object, protocol, category) defined in File.
% In mode ++? Line can be any line number that is between the start
% and end line of Entity.
% In mode +-? Line is the start line of Entity.
source_file_entity(FullPath, Line, Entity) :-
	nonvar(Line),
	!,
	once((	split_file_path:split_file_path(FullPath, Directory, File, _, lgt)
		;	split_file_path:split_file_path(FullPath, Directory, File, _, logtalk)
	)),
	logtalk::loaded_file(FullPath),
	entity_property(Entity, Kind, file(File, Directory)),
	entity_property(Entity, Kind, lines(Begin, End)),
	Begin =< Line, End >= Line.

source_file_entity(FullPath, Line, Entity) :-
	once((	split_file_path:split_file_path(FullPath, Directory, File, _, lgt)
		;	split_file_path:split_file_path(FullPath, Directory, File, _, logtalk)
	)),
	logtalk::loaded_file(FullPath),
	entity_property(Entity, Kind, file(File, Directory)),
	entity_property(Entity, Kind, lines(Line, _)).


entity_source_file(Entity, FullPath, Line) :-
	entity_property(Entity, _, file(File, Directory)),
	atom_concat(Directory, File, FullPath),
	entity_property(Entity, _, lines(Line, _)),
	!.


entity_multifile_predicate_source_file(Entity, Functor, Arity, To, FullPath, Line) :-
	entity_property(Entity, _, file(File, Directory)),
	atom_concat(Directory, File, FullPath),
	entity_property(Entity, _, provides(Functor/Arity, To, Properties)),
	list::memberchk(line_count(Line), Properties),
	!.


%% visible_in_entity(?Entity,?Name,?Arity) is nondet.
%
% Succeeds if the predicate Name/Arity is visible in Entity
% either via a local declaration or import.
%
visible_in_entity(Object,Name,Arity) :-   scoped(Object,Name,Arity).
visible_in_entity(Object,Name,Arity) :-   local(Object,Name,Arity).

scoped(Object,Name,Arity) :-                         % scoped = (public, protected, private)
    Object<<current_predicate(Name/Arity).

local(Object,Name,Arity) :-                          % local = defined but not declared
    object_property(Object, defines(Name/Arity, _)),
    \+ object_property(Object, declares(Name/Arity, _)).


%% declared_in_entity(?Entity,?Name,?Arity,?DeclaringEntity) is nondet.
%
% Succeed if the predicate Name/Arity visible in Entity is declared in
% DeclaringEntity. Entity = DeclaringEntity holds if Entity contains
% a local (non-imported) declaration. Otherwise, DeclaringEntity is the
% module from which the declaration is imported.
% Note that predicate suceeds even if there is no defining clause for
% Name/Arity in DeclaringEntity (definition implies declaration but
% not vice versa).

declared_in_entity(Object,Name,Arity,DeclaringEntity) :-
	visible_in_entity(Object,Name,Arity),
	functor(Head,Name,Arity),
	Object<<predicate_property(Head, declared_in(DeclaringEntity)).

% Defined = There is at least one clause in the declaring module.
% Then the declaring module is also a defining module.
% Note that the clause(es) in the module can come from different files.
defined_in_entity(Entity,Head) :-
	functor(Head,Name,Arity),
	defined_in_entity(Entity,Name,Arity).

defined_in_entity(Entity,Name,Arity) :- % <<< deleted 1 argument
	defined_in_entity(Entity,Name,Arity,Entity).

defined_in_entity(Object,Name,Arity,DefiningEntity) :-
	scoped(Object,Name,Arity),
	functor(Head,Name,Arity),
	Object<<predicate_property(Head, defined_in(DefiningEntity)).
defined_in_entity(Object,Name,Arity,Object) :-
	local(Object,Name,Arity).



%% declared_but_undefined(-Entity,-Name,-Arity,?DeclaringEntity) is semidet
%
% Succeed if the predicate Name/Arity visible in Entity is declared in
% DeclaringEntity but not defined by any clause.
declared_but_undefined(Object,Name,Arity) :- % <<< deleted 1 argument
	declared_in_entity(Object,Name,Arity,_),
	\+ defined_in_entity(Object,Name,Arity).


% In Logtalk it is possible that a "local" predicate is defined
% (in the sense that there is at least one clause) but it is
% not explicitly declared (in a scope directive). Therefore we
% need to negate both conditions to get the semantics that we
% are using in the PDT (where undeclared implies undefined).
referenced_but_undeclared(Object,Name,Arity) :-
	\+ defined_in_entity(Object,Name,Arity,_),
	\+ declared_in_entity(Object,Name,Arity,_).



%% defined_in_file(-Entity,-Name,-Arity,-N,?File,?Line) is nondet
%  defined_in_file(+Entity,+Name,+Arity,+N,?File,?Line) is det
%
%  Get the source locations (File and Line) of all clauses
%  that define Entity::Name/Arity.
%  NOTE: For Logtalk we can only get the first one!

defined_in_file(Entity, Functor, Arity, 1, Path, Line) :-
	nonvar(Entity),
	nonvar(Functor),
	nonvar(Arity),
	!,
	entity_property(Entity, _Kind, defines(Functor/Arity, Properties)),
	entity_property(Entity, _Kind, file(File, Directory)), !,
	atom_concat(Directory, File, Path),
	list::memberchk(line_count(Line), Properties).
defined_in_file(Entity, Functor, Arity, 1, Path, Line) :-
	entity_property(Entity, _Kind, file(File, Directory)),
	atom_concat(Directory, File, Path),
	entity_property(Entity, _Kind, defines(Functor/Arity, Properties)),
	list::memberchk(line_count(Line), Properties).

%    declared_in_entity(Entity,Functor,Arity,Entity),
%    functor(Head,Functor,Arity),
%    nth_clause(Entity:Head,N,Ref),
%    clause_property(Ref,file(File)),
%    clause_property(Ref,line_count(Line)).



%% declared_in_file(+Entity, +Name, +Arity, -Location) is det
%
% Location is a list of Path-[Line] elements, where Path is the full
% path of the file containing the declaration for the predicate Name/Arity,
% which is declared in Entity.
% Line = 1 (approximating the line number information missing for declarations).

declared_in_file(Entity, Name, Arity, [Path-[Line]]) :-
    entity_property(Entity, _Kind, declares(Name/Arity, Properties)),
    (  entity_property(Entity, _Kind, file(File, Directory))
    ->  atom_concat(Directory, File, Path),
        ( list::memberchk(line_count(Line), Properties)      % Line of the scope directive
        -> true
        ;  Line = 0                                          % Implicit dynamic declaration
        )                                                    % via an assert. No line number.
    ;   (  entity_property(Entity, _Kind, (dynamic))
        -> File = 'No Logtalk source file (dynamic declaration)'
        ;  File = 'No Logtalk source file (built-in entity)'
        ),
        Line = 0
    ).


%%
% declared_or_defined_in_files(+Entity,+Name,+Arity, Locations) is semidet
%
% Locations is a list of File-Lines terms whose Lines
% is a list of numbers indicating the starting lines of
% the clauses for Entity:Name/Arity contained in File.
defined_in_files(Entity,Name,Arity,Locations) :-
	( var(Entity)
	; var(Name)
	; var(Arity)
	),
	throw( input_argument_free(defined_in_files(Entity,Name,Arity,Locations)) ).

defined_in_files(Entity,Name,Arity,Locations) :-
    findall( File-Lines,
             setof( Line, Entity^Name^Arity^N^
                    defined_in_file(Entity,Name,Arity, N,File,Line),
                    Lines
             ),
             SrcLocations
    ),
    (  SrcLocations == []
    -> ( declared_in_entity(Entity,Name,Arity,DeclaringEntity),
         declared_in_file(DeclaringEntity,Name,Arity,DeclLocation),
         Locations = DeclLocation
       )
    ;  Locations = SrcLocations
    ).

/*
 * call_local_pred_of_entity(+Entity, +Head) is nondet
 *
 * Call Head in Entity only if Entity contains a local definition
 * (that is, does not import the predicate from another module).
 */
call_local_pred_of_entity(Entity, Head) :-
	functor(Head,F,N),
	defined_in_entity(Entity, F,N,Entity),
	Entity<<Head.

/*
 * call_in_entity(+Object, +Head) is nondet
 *
 * Call Head in Entity regardless whether Head is locally defined or
 * imported from another module.
 */
:- meta_predicate(call_in_entity(*,0)).
call_in_entity(Entity,Goal) :-
   ( nonvar(Entity)
   -> Entity::Goal
    ; ctc_error('Goal called in variable entity: ~w:~w.',
                 [Entity,Goal])
   ).


/*
 * Assert clauses in an explicitly specified module.
 *
 * CAUTION: Due to the semantics of modules in SWI-Prolog, the
 * clause ends up in the module from which the explicitly specified
 * module imports the declaration of the predicate to be asserted.
 *
 * assert_in_entity/2,3 differs from a normal assert called in a
 * module, which would assert the fact into the module containing
 * the invocation of assert (unless the predicate containing the
 * invocation and all its parents on the stack were "module_transparent"
 * and the invoking module was loaded via use_entity ...).
 */
assert_in_entity(Entity,Head      ) :- assertz( :(Entity,Head)            ).
assert_in_entity(Entity,Head,[]   ) :- assertz( :(Entity,Head)            ).
assert_in_entity(Entity,Head,Body ) :- \+ is_list(Body), !, assertz( :(Entity,':-'(Head,Body)) ).

assert_in_entity(Entity,Head, []) :-
	assert_in_entity(Entity,Head ).

assert_in_entity(Entity,Head,      [Opt]) :-
	(  Opt == unique
	-> assert1T( :(Entity,Head))
	;  assertz(        :(Entity,Head))
	).
assert_in_entity(Entity,Head,Body, [Opt]) :-
	(  Opt == unique
	-> assert1T( :(Entity,':-'(Head,Body)))
	;  assertz(        :(Entity,':-'(Head,Body)))
	).


/*
 * Get or retract clauses from an explicitly specified module.
 * The Entity argument must not be a variable!
 * These predicates never access clauses imported from other modules.
 * They only get or delete clauses that are actually asserted in the
 * specified module. This appears to be standard SWI Prolog behaviour
 * meanwhile but it sometimes changed, so for safety we prefer to
 * enforce it ourselves.
 */
clause_in_entity(Entity,Head	) :- defined_in_entity(Entity, Head), clause( :(Entity,Head),_ ) .
clause_in_entity(Entity,Head,Body) :- defined_in_entity(Entity, Head), clause( :(Entity,Head),Body ) .

retract_in_entity(Entity,Head   ) :- defined_in_entity(Entity, Head), retract( :(Entity,Head) ) .
retract_in_entity(Entity,Head,Body) :- defined_in_entity(Entity, Head), retract( :(Entity,':-'(Head,Body)) ) .

retractall_in_entity(Entity,Head) :- defined_in_entity(Entity, Head) -> retractall( :(Entity,Head) ) ; true.

%listing_in_entity(Entity,Goal) :- listing( Entity:Goal ).

/*
 * Copy all clauses whose head unifies Arg3 from module Arg1 to
 * module Arg2 without deleting the original clauses.
 */
copy_entity_predicate(InpEntity, OutEntity, Head) :-
	all( copy_entity_clause(InpEntity, OutEntity, Head) ).


copy_entity_clause(InpEntity, OutEntity, Head) :-
	clause_in_entity(InpEntity,Head,Body),
	assert_in_entity(OutEntity,Head,Body).

/*
 * Move all clauses whose head unifies Arg3 from module Arg1 to
 * module Arg2, deleting the original clauses.
 */
move_entity_predicate(FromEntity, ToEntity,Head) :-
	copy_entity_predicate(FromEntity, ToEntity, Head),
	retractall_in_entity(FromEntity,Head).

/*
 * Replace all clauses whose old head unifies Arg2 from module Arg1
 * and whose head unifies Arg2 .
 */
replace_entity_predicate(Entity, Old, New) :-
	retract_in_entity(Entity, Old),
	assert_in_entity(Entity, New).


/*
 * Tests:

assert_in_entity(Entity,Head     , Goal) :- assertz( :(Entity,Head)      ), call(Goal).
assert_in_entity(Entity,Head,Body, Goal) :- assertz( :(Entity,Head,Body) ), call(Goal).

% :- Dynamically created contents of user module globally visible (without module prefix):
%       Entity = user, Head=uuu(1), Goal=uuu(X), assert_in_entity(Entity,Head, Goal).

% :- Contents of other modules not visible without module prefix:
%       Entity = mmmm, Head=uuu(2), Goal=uuu(X), assert_in_entity(Entity,Head, Goal).

% :- Contents of other modules visible with explicit module prefix:
%       Entity = mmmm, Head=uuu(3), Goal=mmmm:uuu(X), assert_in_entity(Entity,Head, Goal).

% :- Dynamic creation of explicit module prefix:
%       Entity = mmmm, Head=uuu(4), Goal=':'(mmmm,uuu(X)), assert_in_entity(Entity,Head, Goal).

*/

:- end_object.
