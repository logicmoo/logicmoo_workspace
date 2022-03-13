%%%
%%% Simple forward-chaining system
%%%

:- op(1200, xfx, '==>>').
:- external (==>>)/2.




%% assert_if_unew( ?P) is semidet.
%
% Assert If Unew.
%
assert_if_unew(P):- must_or_rtrace(tellg(P)),!.
%assert_if_unew(P):- assert_if_new(P).



%% tell( ?P) is semidet.
%
% Canonicalize And Store.
%
tell(P) :- tellg(P), !.

%expand_assert(tell,P,PP), 
%tellg(P) :- current_predicate(_,P), \+ \+ call(P), !.
tellg(P) :- expand_assert(tellg,P,Q),!,tellg0(P,Q).

%% tellg( ?P) is semidet.
%
% Tellg.
%

tellg0(P,Q) :- (clause_asserted(Q);clause_asserted(P)), !.
tellg0(P,Q) :- current_predicate(_,P), \+ \+ unity_call(P), !.
tellg0(P,Q) :-
   tell_assertion(Q),
   forall(when_added(P, Action),
	  begin(maybe_log_when_added_action(P, Action),
		Action)).

:- external log_when_added_action/2.


%% maybe_log_when_added_action( ?P, ?Action) is semidet.
%
% Maybe Log When Added Action.
%
maybe_log_when_added_action(P, Action) :-
   log_when_added_action(P, Action) -> log((P ==>> Action)) ; true.

:- multifile(when_added/2).


%% when_added( ?Assertion, ?TODO) is semidet.
%
% When Added.
%
when_added(P, tell(Q)) :-
   (P ==>> Q).

:- external tell_globally/1.


%% tell_assertion( ?P) is semidet.
%
% Canonicalize And Store Assertion.
%
tell_assertion(P) :-  
  \+ \+tell_globally(P)-> $global::assert(P); assert(P).


