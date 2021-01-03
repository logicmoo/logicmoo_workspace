:- module(foo,[]).

%!  html_fragments(+Fragments, +In, +Out, +State, +Options) is det.
%
%   Copy In to Out, inserting HTML elements using Fragments.

/** <Module> 
mcomment */

axiom(initiates(wake_up(X), 
  %was awake
   awake(X), T), []).

    % line comment

/* block comment */

axiom(terminates(fall_asleep(X), awake(Y), T), [/*in*/]). % end comment

a_fact(arg).

:- if(true).

'a_atom'.

:- endif.

:- writeq(hi).

axiom(initially(neg(awake(/*foo*/nathan))), [/**/]):-
 %comment a
 a,b,
 c.

end_of_file.

syntax error.
ok

done

