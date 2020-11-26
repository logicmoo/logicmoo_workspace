:- module(lf_consts,
	[modal_tense_and_aspect/1,
	 conj/2,
	 quant/2
	 ]
    ).

/*

Domain and/or language-specific LF constants used by the code in $REGULUS/Prolog/riacs_sem_postproc.pl

*/

%----------------------------------------------------------------------

modal_tense_and_aspect(can).
modal_tense_and_aspect(would).
modal_tense_and_aspect(imperative).

%----------------------------------------------------------------------

conj(and, exists_and).

%----------------------------------------------------------------------

quant(a, exist).
quant(any, exist).
quant(anything, exist).
quant(something, exist).
quant(bare, exist).
quant(null, exist).
quant(the_sing, def_sing).
quant(the_plur, def_plur).
quant(that, def_sing).
quant(all, all).
quant(all_the, all).
quant(which, what).
quant(both, 2).
quant(N, N) :- number(N).
