

:- expects_dialect(swi).

'==>'(a,b).

:- ensure_loaded(library(pfc_test)).
:- expects_dialect(pfc).

(a ==> b).


