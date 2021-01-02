
axiom(initiates(wake_up(X), awake(X), T), []).
axiom(terminates(fall_asleep(X), awake(Y), T), []).
axiom(initially(neg(awake(nathan))), []).
abducible(dummy).

executable(wake_up(_X)).
executable(fall_asleep(_X)).



