
:- expects_dialect(lps).

maxTime(4).
actions roll(_).
unserializable roll(_).
fluents rolled(_).

initially rolled(none).

if true then roll(1).
if true then roll(2).

roll(X) updates Old to New in rolled(Old) if New = X.
