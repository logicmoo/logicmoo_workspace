% Using an "event meta-call" in a precondition to avoid enumerating all forbidden events
events foo, bar, blah.
observe foo from 1 to 2.
observe bar from 2 to 3.
observe blah from 3 to 4.
false happens(E,_,_), E\==bar.
