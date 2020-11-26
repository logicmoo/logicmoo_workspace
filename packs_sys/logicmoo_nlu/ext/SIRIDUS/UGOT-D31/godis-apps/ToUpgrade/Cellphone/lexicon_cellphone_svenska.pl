
 
/*************************************************************************

         name: lexicon_cellphone_svenska.pl 
      version: Apr 7, 1999
  description: Lexicon for Nokia 3210, in swedish
       author: Staffan Larsson
 
*************************************************************************/

:- module( lexicon_cellphone_svenska, [output_form/2, input_form/2, yn_answer/1] ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

output_form( dummy, "" ).

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

input_form( [dummy], blah ).

/*----------------------------------------------------------------------
     yn_answer( A )
     --- is A yes or no?
----------------------------------------------------------------------*/

yn_answer(A):-
	A = yes;
	A = no.



