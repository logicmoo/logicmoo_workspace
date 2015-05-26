:- module(implemented_in, [implemented_in/1,
			   implemented_in/3]).

:- use_module(library(lists)).
:- use_module(library(extra_location)).
:- use_module(library(prolog_codewalk), []). % for message_location//1
:- use_module(library(normalize_head)).
:- use_module(library(audit/audit_codewalk)).

:- multifile
    prolog:message//1,
    prolog:message_location//1.

prolog:message(acheck(implemented_in(From, Args))) -->
    prolog:message_location(From),
    ['Implements ~w'-Args].

implemented_in(MGoal0, From, Args) :-
    normalize_head(MGoal0, MGoal),
    M:Goal = MGoal,
    functor(Goal, F, A),
    findall(MI, ( current_module(M),
		  \+ predicate_property(MGoal, imported_from(_)),
		  MI = M
		; predicate_property(MGoal, imported_from(MI))
		), UML),
    sort(UML, ML),
    member(M, ML),
    ( ( loc_declaration(Goal, M, Declaration, From),
	Declaration \= goal
      ; loc_dynamic(Goal, M, Declaration, From),
	Declaration \= dynamic(query, _, _)
      ),
      Args = [M:F/A-Declaration]
    ; From = clause(ClauseRef),
      catch(( clause(M:Goal, _, ClauseRef),
	      nth_clause(M:Goal, N, ClauseRef)
	    ), _, fail),
      Args = [M:F/A-N]
    ).

:- dynamic prepared/0.

:- public prepare/0.

prepare :-
    audit_walk_code([source(false),
		     infer_meta_predicates(false),
		     autoload(false),
		     evaluate(false),
		     trace_reference(_),
		     module_class([user, system, library])],
		    check_trivial_fails:collect_dynamic_locations(M, FromChk),
		    M,
		    FromChk),
    retractall(prepared),
    assertz(prepared).

implemented_in(MGoal) :-
    ( prepared -> true ; prepare ),
    forall(implemented_in(MGoal, From, Args),
	   print_message(information, acheck(implemented_in(From, Args)))).
