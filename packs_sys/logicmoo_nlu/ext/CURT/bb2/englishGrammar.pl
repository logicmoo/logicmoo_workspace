/*************************************************************************

    File: englishGrammar.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB2, version 1.0 (June 2004).

    BB2 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

/*========================================================================
   Texts
========================================================================*/

t([sem:T])--> 
   s([coord:no,sem:S]),
   {combine(t:T,[s:S])}.

t([sem:T])--> 
   s([coord:no,sem:S1]), 
   t([sem:S2]),
   {combine(t:T,[s:S1,t:S2])}.

t([sem:T])--> 
   s([coord:yes,sem:S]),
   {combine(t:T,[s:S])}.

t([sem:T])--> 
   s([coord:yes,sem:S1]), 
   t([sem:S2]),
   {combine(t:T,[s:S1,t:S2])}.

t([sem:T])--> 
   q([sem:Q]),
   {combine(t:T,[q:Q])}.


/*========================================================================
   Sentences
========================================================================*/

s([coord:no,sem:Sem])--> 
   np([coord:_,num:Num,gap:[],ref:no,sem:NP]), 
   vp([coord:_,inf:fin,num:Num,gap:[],sem:VP]), 
   {combine(s:Sem,[np:NP,vp:VP])}.

s([coord:yes,sem:Sem])--> 
   s([coord:ant,sem:S1]), 
   s([coord:con,sem:S2]), 
   {combine(s:Sem,[s:S1,s:S2])}.

s([coord:yes,sem:Sem])--> 
   s([coord:either,sem:S1]), 
   s([coord:or,sem:S2]), 
   {combine(s:Sem,[s:S1,s:S2])}.

s([coord:ant,sem:Sem])--> 
   [if], 
   s([coord:no,sem:S]),
   {combine(s:Sem,[if:S])}.

s([coord:either,sem:Sem])--> 
   [either], 
   s([coord:no,sem:S]),
   {combine(s:Sem,[either:S])}.

s([coord:con,sem:Sem])--> 
   [then], 
   s([coord:no,sem:S]),
   {combine(s:Sem,[then:S])}.

s([coord:con,sem:Sem])-->
   s([coord:no,sem:S]),
   {combine(s:Sem,[then:S])}.

s([coord:or,sem:Sem])-->
   [or], 
   s([coord:no,sem:S]),
   {combine(s:Sem,[or:S])}.

sinv([gap:G,sem:S])-->
   av([inf:fin,num:Num,sem:Sem]),
   np([coord:_,num:Num,gap:[],ref:no,sem:NP]),
   vp([coord:_,inf:inf,num:Num,gap:G,sem:VP]), 
   {combine(sinv:S,[av:Sem,np:NP,vp:VP])}.


/*========================================================================
   Questions
========================================================================*/

q([sem:Sem])--> 
   whnp([num:Num,sem:NP]), 
   vp([coord:_,inf:fin,num:Num,gap:[],sem:VP]), 
   {combine(q:Sem,[whnp:NP,vp:VP])}.

q([sem:Sem])--> 
   whnp([num:_,sem:NP]), 
   sinv([gap:[np:NP],sem:S]),
   {combine(q:Sem,[sinv:S])}.


/*========================================================================
   Noun Phrases
========================================================================*/

np([coord:no,num:sg,gap:[np:NP],ref:no,sem:NP])--> [].

np([coord:yes,num:pl,gap:[],ref:Ref,sem:NP])--> 
   np([coord:no,num:sg,gap:[],ref:Ref,sem:NP1]), 
   coord([type:conj,sem:C]), 
   np([coord:_,num:_,gap:[],ref:Ref,sem:NP2]), 
   {combine(np:NP,[np:NP1,coord:C,np:NP2])}.

np([coord:yes,num:sg,gap:[],ref:Ref,sem:NP])--> 
   np([coord:no,num:sg,gap:[],ref:Ref,sem:NP1]), 
   coord([type:disj,sem:C]), 
   np([coord:_,num:sg,gap:[],ref:Ref,sem:NP2]), 
   {combine(np:NP,[np:NP1,coord:C,np:NP2])}.

np([coord:no,num:sg,gap:[],ref:no,sem:NP])--> 
   det([mood:decl,type:_,num:Num,sem:Det]), 
   n([coord:_,num:Num,sem:N]), 
   {combine(np:NP,[det:Det,n:N])}.

np([coord:no,num:sg,gap:[],ref:no,sem:NP])--> 
   pn([sem:PN]), 
   {combine(np:NP,[pn:PN])}.

np([coord:no,num:sg,gap:[],ref:Ref,sem:NP])--> 
   pro([ref:Ref,sem:PN]), 
   {combine(np:NP,[pn:PN])}.

np([coord:no,num:sg,gap:[],ref:no,sem:NP])--> 
   qnp([mood:decl,sem:QNP]), 
   {combine(np:NP,[qnp:QNP])}.


/*========================================================================
   WH Noun Phrases
========================================================================*/

whnp([num:sg,sem:NP])--> 
   qnp([mood:int,sem:QNP]), 
   {combine(whnp:NP,[qnp:QNP])}.

whnp([num:sg,sem:NP])--> 
   det([mood:int,type:_,num:_,sem:Det]), 
   n([coord:_,num:_,sem:N]), 
   {combine(whnp:NP,[det:Det,n:N])}.


/*========================================================================
   Nouns
========================================================================*/

n([coord:yes,num:Num,sem:N])--> 
   n([coord:no,num:Num,sem:N1]), 
   coord([type:_,sem:C]),  
   n([coord:_,num:Num,sem:N2]),
   {combine(n:N,[n:N1,coord:C,n:N2])}.

n([coord:C,num:Num,sem:Sem])--> 
   adj([sem:A]), 
   n([coord:C,num:Num,sem:N]), 
   {combine(n:Sem,[adj:A,n:N])}.

n([coord:no,num:Num,sem:N])--> 
   noun([num:Num,sem:Noun]),
   {combine(n:N,[noun:Noun])}.

n([coord:no,num:Num,sem:Sem])--> 
   noun([num:Num,sem:N]), 
   nmod([sem:PP]),
   {combine(n:Sem,[noun:N,nmod:PP])}. 

nmod([sem:N])--> 
   pp([sem:PP]),
   {combine(nmod:N,[pp:PP])}.

nmod([sem:N])--> 
   rc([sem:RC]),
   {combine(nmod:N,[rc:RC])}.

nmod([sem:Sem])--> 
   pp([sem:PP]), 
   nmod([sem:NMod]),
   {combine(nmod:Sem,[pp:PP,nmod:NMod])}.


/*========================================================================
   Verb Phrases
========================================================================*/

vp([coord:yes,inf:Inf,num:Num,gap:[],sem:VP])--> 
   vp([coord:no,inf:Inf,num:Num,gap:[],sem:VP1]), 
   coord([type:_,sem:C]), 
   vp([coord:_,inf:Inf,num:Num,gap:[],sem:VP2]),
   {combine(vp:VP,[vp:VP1,coord:C,vp:VP2])}.

vp([coord:no,inf:Inf,num:Num,gap:[],sem:VP])--> 
   av([inf:Inf,num:Num,sem:Mod]), 
   vp([coord:_,inf:inf,num:Num,gap:[],sem:V2]), 
   {combine(vp:VP,[av:Mod,vp:V2])}.

vp([coord:no,inf:Inf,num:Num,gap:[],sem:VP])--> 
   cop([inf:Inf,num:Num,sem:Cop]), 
   np([coord:_,num:_,gap:[],ref:_,sem:NP]), 
   {combine(vp:VP,[cop:Cop,np:NP])}.

vp([coord:no,inf:Inf,num:Num,gap:[],sem:VP])--> 
   iv([inf:Inf,num:Num,sem:IV]), 
   {combine(vp:VP,[iv:IV])}.

vp([coord:no,inf:I,num:Num,gap:G,sem:VP])-->   
   tv([inf:I,num:Num,ref:Ref,sem:TV]), 
   np([coord:_,num:_,gap:G,ref:Ref,sem:NP]), 
   {combine(vp:VP,[tv:TV,np:NP])}.


/*========================================================================
   Prepositional Phrases
========================================================================*/

pp([sem:PP])--> 
   prep([sem:Prep]), 
   np([coord:_,num:_,gap:[],ref:no,sem:NP]), 
   {combine(pp:PP,[prep:Prep,np:NP])}.


/*========================================================================
   Relative Clauses
========================================================================*/

rc([sem:RC])--> 
   relpro([sem:RP]), 
   vp([coord:_,inf:fin,num:sg,gap:[],sem:VP]), 
   {combine(rc:RC,[relpro:RP,vp:VP])}.


/*========================================================================
   Lexical Rules
========================================================================*/

iv([inf:Inf,num:Num,sem:Sem])--> 
   {lexEntry(iv,[symbol:Sym,syntax:Word,inf:Inf,num:Num])},
   Word,
   {semLex(iv,[symbol:Sym,sem:Sem])}.

tv([inf:Inf,num:Num,ref:Ref,sem:Sem])--> 
   {lexEntry(tv,[symbol:Sym,syntax:Word,inf:Inf,num:Num])},
   Word,
   {semLex(tv,[symbol:Sym,ref:Ref,sem:Sem])}.

cop([inf:Inf,num:Num,sem:Sem])--> 
   {lexEntry(cop,[pol:Pol,syntax:Word,inf:Inf,num:Num])},
   Word,
   {semLex(cop,[pol:Pol,sem:Sem])}.

det([mood:M,type:Type,num:Num,sem:Det])--> 
   {lexEntry(det,[syntax:Word,mood:M,num:Num,type:Type])},
   Word,
   {semLex(det,[type:Type,number:Num,sem:Det])}. 

pn([sem:Sem])--> 
   {lexEntry(pn,[symbol:Sym,syntax:Word])},
   Word,  
   {semLex(pn,[symbol:Sym,sem:Sem])}.

pro([ref:Ref,sem:Sem])--> 
   {lexEntry(pro,[symbol:Sym,ref:Ref,syntax:Word])},
   Word,  
   {semLex(pro,[symbol:Sym,sem:Sem])}.

relpro([sem:Sem])--> 
   {lexEntry(relpro,[syntax:Word])},
   Word,
   {semLex(relpro,[sem:Sem])}.

prep([sem:Sem])--> 
   {lexEntry(prep,[symbol:Sym,syntax:Word])},
   Word,
   {semLex(prep,[symbol:Sym,sem:Sem])}.

adj([sem:Sem])--> 
   {lexEntry(adj,[symbol:Sym,syntax:Word])},
   Word,
   {semLex(adj,[symbol:Sym,sem:Sem])}.

av([inf:Inf,num:Num,sem:Sem])--> 
   {lexEntry(av,[syntax:Word,inf:Inf,num:Num,pol:Pol])},
   Word,
   {semLex(av,[pol:Pol,sem:Sem])}.

coord([type:Type,sem:Sem])--> 
   {lexEntry(coord,[syntax:Word,type:Type])},
   Word, 
   {semLex(coord,[type:Type,sem:Sem])}.

qnp([mood:M,sem:NP])--> 
   {lexEntry(qnp,[symbol:Symbol,syntax:Word,mood:M,type:Type])},
   Word,
   {semLex(qnp,[type:Type,symbol:Symbol,sem:NP])}.

noun([num:_,sem:Sem])--> 
   {lexEntry(noun,[symbol:Sym,syntax:Word])},
   Word,
   {semLex(noun,[symbol:Sym,sem:Sem])}.

