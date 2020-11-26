/*************************************************************************

         name: discourseGrammar.pl
      version: April 11, 2000
  description: Grammar rules for discourse
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- use_module(englishLexicon,[lexicon/4]).


/*========================================================================
   Discourse Rules
========================================================================*/

d2(D2)--> d1(D1), {combine(d2:D2,[d1:D1])}.

d1(D1)--> s2(S2), {combine(d1:D1,[s2:S2])}.
d1(D1)--> s2(S2), d1(D2), {combine(d1:D1,[s2:S2,conj,d1:D2])}.
d1(D1)--> s2(S2), {lexicon(coord,_,Word,Type)}, Word, d1(D2),
	{combine(d1:D1,[s2:S2,Type,d1:D2])}.




