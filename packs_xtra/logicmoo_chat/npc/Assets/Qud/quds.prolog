%%%
%%% QUD SYSTEM (Question Under Discussion)
%%%

%%
%% Enumeration
%%

%% qud(?Qud, ?Type)
%  Qud is an existing qud of type Type.
qud(Qud, Type) :-
    qud(Qud),
    Qud/type:Type.

%% qud(?Qud)
%  Qud is an existing qud.
qud(A) :-
       descendant_qud_of($root, A).

%% descendant_qud_of(?Ancestor, ?Descendant)
%  Both Ancestor and Descendant are existing quds and Descendent
%  is a descendent of Ancestor in the tree.
descendant_qud_of(Ancestor, Descendant) :-
    Ancestor/quds/_>>Child,
    ( Descendant=Child 
      ; descendant_qud_of(Child,Descendant) ).



%=autodoc
%% parent_qud_of( ?Child, ?Parent) is semidet.
%
% Parent Topic Of.
%
parent_qud_of(Child, Parent) :-
   Parent is Child.'Parent'.'Parent'.



%=autodoc
%% qud_uid( ?Qud, ?UID) is semidet.
%
% Topic Uid.
%
qud_uid(Qud, UID) :-
   property(Qud, "Key", UID).

:- public qud_status/2, set_qud_status/2.

%% qud_status(+Qud, -Status)
%  Qud's status field is the specified status.
qud_status(Qud, Status) :-
   Qud/status:Status.

%% set_qud_status(+Qud, +Status)
%  IMPERATIVE
%  Update's qud's status field.
set_qud_status(Qud, Status) :-
   assert(Qud/status:Status).



%=autodoc
%% normalize_task( ?Status, ?Task) is semidet.
%
% Normalize Task.
%
normalize_task(set_status(Status),
	       call(set_qud_status($task, Status))).

%% current_priority(-Priority)
% Priority is the priority of the currently runnign qud,
% or 1, if this code is not running in a qud.
current_priority(P) :-
   $task \= null,
   $task/priority:P,
   !.
current_priority(1).

%%
%% Creation
%%

:- public begin_qud/2, begin_qud/3, begin_child_qud/4.

%% begin_qud(+Type, +Priority)
%  IMPERATIVE
%  Creates a new qud of type Type at me level.
begin_qud(Type, Priority) :-
   begin_qud(Type, Priority, _).

%% begin_qud(+Type, +Priority, -Child)
%  IMPERATIVE
%  Creates a new qud of type Type at me level and returns its ELNode
%  in Child.
begin_qud(Type, Priority, Child) :-
    begin_child_qud($root, Type, Priority, Child).

%% begin_child_qud(+Parent, +Type, Priority, -Child, +Assertions)
%  IMPERATIVE
%  Creates a new qud of type Type as a child of Parent, and returns its
%  ELNode in Child.
%  Adds Assertions to its ELNode.
begin_child_qud(Parent, Type, Priority, Child, Assertions) :-
    begin(allocate_UID(ChildUID),
	  assert(Parent/quds/ChildUID/type:Type),
	  Parent/quds/ChildUID>>Child,
	  assert(Child/priority:Priority),
	  forall(member(A, Assertions),
		 assert(A)),
	  goto_state(Child, start)).

%% begin_child_qud(+Parent, +Type, +Priority, -Child)
%  IMPERATIVE
%  Creates a new qud of type Type as a child of Parent, and returns its
%  ELNode in Child.
begin_child_qud(Parent, Type, Priority, Child) :-
    begin_child_qud(Parent, Type, Priority, Child, [ ]).

%%
%% Destruction
%%

:- public stop_qud/1.

%% stop_qud(+Qud)
%  IMPERATIVE
%  Kstops qud and all its children.
%  Calls on_stop/2 on it before deletion.
stop_qud(Qud) :-
    begin(Qud/type:Type,
	  ignore(on_stop(Type, Qud)),
	  stop_children(Qud),
	  retract(Qud)).

%% stop_children(+Qud)
%  IMPERATIVE
%  Calls stop_qud/1 on all children of Qud.

:- public stop_children/1.

stop_children(Qud) :-
    forall(Qud/quds/_>>Subqud,
	   stop_qud(Subqud)).

%% on_stop(+Type, +Qud)
%  IMPERATIVE
%  Called when Qud is to be destroyed.
%  Called before either it or its children are destroyed.

:- external on_stop/2.

%%
%% State switching
%%

%% goto_state(+Qud, +State)
%  IMPERATIVE
%  Switches Qud to State, running entry/exit handlers as appropriate.

:- public goto_state/2.

goto_state(Qud, State) :-
   begin(Qud/type:Type,
	 ignore(( Qud/state:OldState,
		  on_exit_state(OldState, Type, Qud) )),
	 assert(Qud/state:State/enter_time: $now),
	 ignore(on_enter_state(State, Type, Qud))).

%% on_enter_state(+NewState, +Type, +Qud)
%  IMPERATIVE
%  Called just after Qud is switched to state NewState.

:- external on_enter_state/3.

%% on_exit_state(+OldState, +Type, +Qud)
%  IMPERATIVE
%  Called just before Qud is switched from state OldState.

:- external on_exit_state/3.

%%
%% Initialization
%%

character_initialization :-
   %\+ $global_root/configuration/inhibit_qud_initialization,
   forall(character_qud(Type, Priority),
	  begin_qud(Type, Priority)).

%% character_qud(?Type, ?Priority)
%  Type is a qud of the character.
%  It will be spawn automatically upon character initialization.

character_qud(Type, Priority) :-
   standard_qud(Type, Priority).
character_qud(Type, Priority) :-
   special_qud(Type, Priority).

%% standard_qud(?Type, ?Priority)
%  Type is a standard qud of all characters.
%  It will be spawn automatically upon character initialization.

:- external standard_qud/2.

%% special_qud(?Type, ?Priority)
%  Type is a qud of this particular character.
%  It will be spawn automatically upon character initialization.

:- external special_qud/2.
