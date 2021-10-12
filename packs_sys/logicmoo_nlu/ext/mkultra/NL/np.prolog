%%%
%%%                  Noun Phrases
%%%

test_file(generate(np, _), "NL/np_tests").
test_file(complete(np, _), "NL/np_tests").
test_file(parse(np, _), "NL/np_tests").

:- indexical selectional_constraints=null.

impose_selectional_constraint(Var, Type) :-
   bind(selectional_constraints, [Var:Type | $selectional_constraints]).

selectional_constraint(Var, Type) :-
   memberchk(Var:Type, $selectional_constraints),
   !.
selectional_constraint(_, entity).

%:- randomizable np/7.

%% np(?Meaning, ?Case, Agreement, +GapIn, -GapOut)
%  Noun phrases

% Gaps
np((X^S)^S, _C, _Agreement, np(X), nogap) -->
   [ ].

% Pronouns
np(NP, Case, Agreement, Gap, Gap) -->
   pronoun(Case, Agreement,NP).

% Proper names
np((E^S)^S, Case, third:Number, Gap, Gap) -->
   { \+ bound_discourse_variable(E) },
   proper_name(E, Number),
   opt_genitive(Case).

opt_genitive(subject) --> [].
opt_genitive(object) --> [].
opt_genitive(genitive) --> ['\'', s].


% PARSE/COMPLETE ONLY
% "a KIND" from unbound variables with declared types
np(LF, _, third:singular, Gap, Gap) -->
   { var(LF) }, 
   [a],
   kind_noun(Kind, singular),
   { LF = ((X^S)^(S, is_a(X, Kind))) }.

% GENERATE ONLY
% "a KIND" from unbound variables with declared types
np((X^S)^S, _, third:singular, Gap, Gap) -->
   { var(X) },
   [a ],
   { discourse_variable_type(X, Kind) },
   kind_noun(Kind, singular).

% PARSE ONLY
% "the NOUN"
np((X^S)^S, _C, third:singular, Gap, Gap) -->
   [ the ],
   { var(X),
     input_from_player},	% filter for parsing
   kind_noun(Kind, singular),
   { resolve_definite_description(X, is_a(X, Kind)) }.

% GENERATE ONLY
% "the NOUN"
np((X^S)^S, _C, third:singular, Gap, Gap) -->
   { nonvar(X),
     % If it has a proper name or a bound variable, then don't use this rule.
     \+ proper_name(X, _),
     base_kind(X, Kind) },
   [the],
   kind_noun(Kind, singular).

% COMPLETE ONLY
% "the NOUN"
np((X^S)^S, _C, third:singular, Gap, Gap) -->
   % If we're generating (nonvar(X)) rather than completing (var(X)),
   % don't generate something that has a proper name.
   [the],
   { var(X),
     input_from_player,
     \+ bound_discourse_variable(X) },
   kind_noun(Kind, singular),
   { leaf_kind(Kind),
     object_matching_selectional_constraint(X, Kind) }.

object_matching_selectional_constraint(X, Kind) :-
   selectional_constraint(X, ConstraintKind),
   kind_of(Kind, ConstraintKind),
   is_a(X, Kind).

% GENERATE ONLY
% Fixed strings.
np((String^S)^S, _, _, Gap, Gap) -->
   {string(String)},
   [String].

% GENERATE ONLY
% Numbers.
np((Number^S)^S, _, _, Gap, Gap) -->
   {number(Number)},
   [Number].

resolve_definite_description(X, Constraint) :-
   nonvar(X),
   !,
   Constraint.
resolve_definite_description(Object, is_a(Object, Kind)) :-
   kind_of(Kind, room),
   !,
   is_a(Object, Kind).
resolve_definite_description(Object, Constraint) :-
   % This rule will fail in the test suite b/c the global environment has no gameobject.
   \+ running_tests,
   % Pick the nearest one, if it's something that nearest works on.
   nearest(Object, Constraint),
   !.
resolve_definite_description(_Object, Constraint) :-
   % Punt, and choose whatever Prolog gives us first.
   Constraint.

%%%
%%% Realizing property values
%%%

property_value_np(_Property, Value) --> [Value].

