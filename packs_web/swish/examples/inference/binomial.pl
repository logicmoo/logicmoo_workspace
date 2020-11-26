/* example of a binomial distribution

*/
/** <examples>
?- hist(10000,G).
% show the distribution of 10000 samples. 
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

b(B):binomial(B,10,0.5).

:- end_lpad.

hist(Samples,Chart):-
  mc_sample_arg(b(V),Samples,V,L0),
  maplist(to_pair,L0,L1),
  keysort(L1,L),
  maplist(key,L,X),
  maplist(value,L,Y),
  Chart = c3{data:_{x:x,
    columns:[[x|X],[freq|Y]], type:bar},
    axis:_{ x:_{ tick:_{fit:false}}},
     bar:_{
     width:_{ ratio: 1.0 }},
     legend:_{show: false}}.

