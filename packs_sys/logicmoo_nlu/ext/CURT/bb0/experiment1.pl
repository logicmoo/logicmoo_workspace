/*************************************************************************

         name: experiment1.pl (Volume 1, Chapter 2)
      version: July 1, 1997
  description: This is the code of the first experiment
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- ensure_loaded(comsemOperators).

/*========================================================================
   Syntax-Semantics Rules 
========================================================================*/

s(Sem)--> np(Sem), vp(SemVP), 
   {
    arg(1,SemVP,X),
    arg(1,Sem,X),
    arg(2,Sem,Matrix),
    arg(2,Matrix,SemVP)
   }.
            
s(Sem)--> np(SemNP), vp(Sem), 
   {
    arg(1,Sem,SemNP)
   }.

np(Sem)--> pn(Sem).

np(Sem)--> det(Sem), noun(SemNoun), 
   {
    arg(1,SemNoun,X),
    arg(1,Sem,X),
    arg(2,Sem,Matrix),
    arg(1,Matrix,SemNoun)
   }.

vp(Sem)--> iv(Sem).

vp(Sem)--> tv(SemTV), np(Sem), 
   {
    arg(2,SemTV,X),
    arg(1,Sem,X),
    arg(2,Sem,Matrix),
    arg(2,Matrix,SemTV)
   }.

vp(Sem)--> tv(Sem), np(SemNP), 
   { 
    arg(2,Sem,SemNP)
   }.

/*========================================================================
   Proper Names
========================================================================*/

pn(vincent)--> [vincent].

pn(mia)--> [mia].

/*========================================================================
   Transitive Verbs
========================================================================*/

tv(love(_,_))--> [loves].

/*========================================================================
   Intransitive Verbs
========================================================================*/

iv(snort(_))--> [snorts].

/*========================================================================
   Determiners
========================================================================*/

det(exists(_,_ & _))--> [a].

det(forall(_,_ > _))--> [every].

/*========================================================================
   Nouns
========================================================================*/

noun(woman(_))--> [woman].

noun(footmassage(_))--> [foot,massage].
