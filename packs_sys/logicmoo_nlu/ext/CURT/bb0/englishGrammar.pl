/*************************************************************************

         name: englishGrammar.pl
      version: November 12, 1997
  description: Grammar rules for a small coverage of English
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- use_module(englishLexicon,[lexicon/4]).

/*========================================================================
   Sentences
========================================================================*/

s2(S2)--> s1(S1), {combine(s2:S2,[s1:S1])}.

s2(S2)--> [if], s1(S1A), [then], s1(S1B),
          {combine(s2:S2,[s1:S1A,cond,s1:S1B])}.

s1(S1)--> np2(NP2), vp2(VP2), {combine(s1:S1,[np2:NP2,vp2:VP2])}.


/*========================================================================
   Noun Phrases
========================================================================*/

np2(NP2)--> np1(NP1), {combine(np2:NP2,[np1:NP1])}.
np2(NP2)--> np1(NP1A), coord(C), np1(NP1B),
            {combine(np2:NP2,[np1:NP1A,coord:C,np1:NP1B])}.

np1(NP1)--> det(Det), noun2(N2), {combine(np1:NP1,[det:Det,n2:N2])}.
np1(NP1)--> pn(PN), {combine(np1:NP1,[pn:PN])}.
np1(NP1)--> pro(Pro), {combine(np1:NP1,[pro:Pro])}.
np1(NP1)--> np(NP), {combine(np1:NP1,[np:NP])}.


/*========================================================================
   Nouns
========================================================================*/

noun2(N2)--> noun1(N1), {combine(n2:N2,[n1:N1])}.
noun2(N2)--> noun1(N1A), coord(C), noun1(N1B),
          {combine(n2:N2,[n1:N1A,coord:C,n1:N1B])}.

noun1(N1)--> adj(A), noun1(N), {combine(n1:N1,[adj:A,n1:N])}.
noun1(N1)--> noun(N), {combine(n1:N1,[noun:N])}.
noun1(N1)--> noun(N), pp(PP), {combine(n1:N1,[noun:N,pp:PP])}.
noun1(N1)--> noun(N), rc(RC), {combine(n1:N1,[noun:N,rc:RC])}.


/*========================================================================
   Verb Phrases
========================================================================*/

vp2(VP2)--> vp1(VP1), {combine(vp2:VP2,[vp1:VP1])}.
vp2(VP2)--> vp1(VP1A), coord(C), vp1(VP1B),
            {combine(vp2:VP2,[vp1:VP1A,coord:C,vp1:VP1B])}.

vp1(VP1)--> mod(Mod), v2(inf,V2), {combine(vp1:VP1,[mod:Mod,v2:V2])}.
vp1(VP1)--> v2(fin,V2), {combine(vp1:VP1,[v2:V2])}.

v2(fin,V2)--> cop(Cop), np2(NP2), {combine(v2:V2,[cop:Cop,np2:NP2])}.
v2(fin,V2)--> cop(Cop), neg(Neg), np2(NP2),
              {combine(v2:V2,[cop:Cop,neg:Neg,np2:NP2])}.

v2(I,V2)--> v1(I,V), {combine(v2:V2,[v1:V])}.
v2(I,V2)--> v1(I,V1A), coord(C), v1(I,V1B),
            {combine(v2:V2,[v1:V1A,coord:C,v1:V1B])}.

v1(I,V1)--> iv(I,IV), {combine(v1:V1,[iv:IV])}.
v1(I,V1)--> tv(I,TV), np2(NP2), {combine(v1:V1,[tv:TV,np2:NP2])}.


/*========================================================================
   Prepositional Phrases
========================================================================*/

pp(PP)--> prep(Prep), np2(NP2), {combine(pp:PP,[prep:Prep,np2:NP2])}.


/*========================================================================
   Relative Clauses
========================================================================*/

rc(RC)--> relpro(RP), vp2(VP2), {combine(rc:RC,[relpro:RP,vp2:VP2])}.


/*========================================================================
   Lexical Rules
========================================================================*/

iv(I,IV)--> {lexicon(iv,Sym,Word,I),ivSem(Sym,IV)}, Word.

tv(I,TV)--> {lexicon(tv,Sym,Word,I),tvSem(Sym,TV)}, Word.

cop(Cop)--> {lexicon(cop,Sym,Word,_),tvSem(Sym,Cop)}, Word.

det(Det)--> {lexicon(det,_,Word,Type),detSem(Type,Det)}, Word.

pn(PN)--> {lexicon(pn,Sym,Word,G),pnSem(Sym,G,PN)}, Word.

pro(Pro)--> {lexicon(pro,Gender,Word,Type),proSem(Gender,Type,Pro)}, Word.

np(NP)--> {lexicon(np,Symbol,Word,Type),npSem(Type,Symbol,NP)}, Word.

noun(N)--> {lexicon(noun,Sym,Word,_),nounSem(Sym,N)}, Word.

relpro(RP)--> {lexicon(relpro,_,Word,_),relproSem(RP)}, Word.

prep(Prep)--> {lexicon(prep,Sym,Word,_),prepSem(Sym,Prep)}, Word.

adj(Adj)--> {lexicon(adj,Sym,Word,_),adjSem(Sym,Adj)}, Word.

mod(Mod)--> {lexicon(mod,_,Word,Type),modSem(Type,Mod)}, Word.

neg(Neg)--> [not], {modSem(neg,Neg)}.

coord(C)--> {lexicon(coord,_,Word,Type), coordSem(Type,C)}, Word.

