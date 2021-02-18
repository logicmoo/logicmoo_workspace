
:- expects_dialect(lps).

% trash.lps
maxTime(10).
fluents locked(_), trash(_), bin(_).
actions dispose(_,_).
events unlock(_).

initially  locked(container1), trash(bottle1), bin(container1), bin(container2).
observe 	unlock(container1) from 4 to 5.

if 	trash(Object) at T1, bin(Container) at T1 then
	dispose(Object, Container) from T2 to _T3, T1 =<T2.


unlock(Container) terminates locked(Container).
dispose(Object, _Container) terminates 	trash(Object).

false  dispose(_Object, Container), locked(Container).
