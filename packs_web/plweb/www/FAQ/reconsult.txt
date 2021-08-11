---+ Where is the reconsult/1 predicate?

Some old Prologs used to have consult/1 that simply added the clauses to
the database and reconsult/1 to reload during development. It lead to a
lot of confusion. SWI-Prolog always wipes clauses loaded from a file
that was already loaded. If you wish to load clauses from different
files you need the ISO standard :- multifile Name/Arity ... directive;
see multifile/1.

@see For reloading files after editing, use make/0.
@see For more info on sourcecode management, see [[Initialising and
Managing a Prolog project][</pldoc/man?section=IDE>]] from the SWI-Prolog
reference manual.
