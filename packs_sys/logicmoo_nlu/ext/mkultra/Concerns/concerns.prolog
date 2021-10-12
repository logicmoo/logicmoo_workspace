/next_uid:0.

spawn_activity(Type, Child) :-
    Parent is $root,
    spawn_child_activity(Parent, Type, Child).

spawn_child_activity(Parent, Type, Child) :-
    allocate_UID(ChildUID),
    assert(Parent/activities/ChildUID/type:Type),
    Parent/activities/ChildUID>>Child,
    ignore(on_initiate(Type, Child)).

kill_activity(Activity) :-
    Activity/type:Type,
    ignore(on_kill(Type, Activity)),
    kill_children(Activity),
    retract(Activity).

kill_children(Activity) :-
    forall(Activity/activities/_>>Subactivity,
	   kill_activity(Subactivity)).

allocate_UID(ChildUID) :-
    /next_uid:ChildUID,
    NextUID is ChildUID+1,
    assert(/next_uid:NextUID).

kill_all_activities :-
    Root is $root,
    kill_children(Root).

activity(Activity, Type) :-
    activity(Activity),
    Activity/type:Type.

activity(A) :-
       R is $root,
       descendant_activity_of(R, A).
descendant_activity_of(Ancestor, Descendant) :-
    Ancestor/activities/_>>Child,
    ( Descendant=Child 
      ; descendant_activity_of(Child,Descendant) ).

activities :-
    forall(activity(A, T),
	   ( write(A), write("\t"), write(T), write("\t"),
	     findall(Action,
		     propose_action(T, A, Action),
		     Actions),
	     writeln(Actions) )).

