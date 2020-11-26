concatener([], [w|z], l) :- l = [w|z].
concatener([x|y], [w|z], l) :- l = [x|v], concatener(y, [w|z], v).

test :- x = 4, !(x < 3).

