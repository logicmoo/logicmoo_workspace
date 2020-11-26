/*************************************************************************

         name: englishLexicon.pl
      version: November 12, 1997; March 9, 1999.
  description: Lexical entries for a small coverage of English
      authors: Patrick Blackburn & Johan Bos
 
This file contains the lexical entries for a small fragment of
English.  Entries have the form lexicon(Cat,Sym,Phrase,Misc), where
Cat is the syntactic category, Sym the predicate symbol introduced
by the phrase, Phrase a list of the words that form the phrase, and
Misc miscellaneous information depending on the the type of entry.

*************************************************************************/

:- module(englishLexicon,[lexicon/4]).

/*========================================================================
   Determiners: lexicon(det,_,Words,Type)
========================================================================*/

lexicon(det,_,[every],uni).
lexicon(det,_,[a],indef).
lexicon(det,_,[the],def).
lexicon(det,_,[which],wh).
lexicon(det,_,[one],card(1)).
lexicon(det,_,[another],alt).
lexicon(det,_,[his],poss(male)).
lexicon(det,_,[her],poss(female)).
lexicon(det,_,[its],poss(nonhuman)).

/*========================================================================
   Nouns: lexicon(noun,Symbol,Words,{[],[Hypernym],Hypernym})
========================================================================*/

lexicon(noun,animal,[animal],[animate,nonhuman,organism]).
lexicon(noun,artifact,[artifact],[object]).
lexicon(noun,beverage,[beverage],[drinkable,food]).
lexicon(noun,building,[building],[immobile,artifact]).
lexicon(noun,container,[container],[instrument]).
lexicon(noun,cup,[cup],[container]).
lexicon(noun,device,[device],[instrument]).
lexicon(noun,burger,[burger],[edible,food]).
lexicon(noun,boxer,[boxer],[person]).
lexicon(noun,boss,[boss],[person]).
lexicon(noun,car,[car],[vehicle]).
lexicon(noun,chainsaw,[chainsaw],[device]).
lexicon(noun,criminal,[criminal],[person]).
lexicon(noun,customer,[customer],[person]).
lexicon(noun,drug,[drug],[food]).
lexicon(noun,entity,[entity],[concrete,thing]).
lexicon(noun,episode,[episode],[event]).
lexicon(noun,event,[event],[abstract,unisex,thing]).
lexicon(noun,fdshake,[five,dollar,shake],[beverage]).
lexicon(noun,food,[food],[object]).
lexicon(noun,footmassage,[foot,massage],[event]).
lexicon(noun,gimp,[gimp],[person]).
lexicon(noun,glass,[glass],[container]).
lexicon(noun,gun,[gun],[weapon]).
lexicon(noun,hammer,[hammer],[device]).
lexicon(noun,hashbar,[hash,bar],[building]).
lexicon(noun,person,[person],[human,animate,organism]).
lexicon(noun,husband,[husband],[married,man]).
lexicon(noun,instrument,[instrument],[mobile,artifact]).
lexicon(noun,joke,[joke],[event]).
lexicon(noun,man,[man],[male,person]).
lexicon(noun,needle,[needle],[device]).
lexicon(noun,object,[object],[nonliving,unisex,entity]).
lexicon(noun,organism,[organism],[living,entity]).
lexicon(noun,owner,[owner],[person]).
lexicon(noun,piercing,[piercing],[event]).
lexicon(noun,plant,[plant],[nonhuman,inanimate,organism]).
lexicon(noun,qpwc,[quarter,pounder,with,cheese],[edible,food]).
lexicon(noun,radio,[radio],[instrument]).
lexicon(noun,restaurant,[restaurant],[building]).
lexicon(noun,robber,[robber],[person]).
lexicon(noun,suitcase,[suitcase],[container]).
lexicon(noun,shotgun,[shotgun],[weapon]).
lexicon(noun,sword,[sword],[weapon]).
lexicon(noun,vehicle,[vehicle],[instrument]).
lexicon(noun,weapon,[weapon],[instrument]).
lexicon(noun,wife,[wife],[married,woman]).
lexicon(noun,woman,[woman],[female,person]).

/*========================================================================
   Proper Names: lexicon(pn,Symbol,Words,{male,female})
========================================================================*/

lexicon(pn,butch,[butch],male).
lexicon(pn,esmarelda,[esmarelda],female).
lexicon(pn,honey_bunny,[honey,bunny],female).
lexicon(pn,jimmy,[jimmy],male).
lexicon(pn,jody,[jody],female).
lexicon(pn,jules,[jules],male).
lexicon(pn,lance,[lance],male).
lexicon(pn,marsellus,[marsellus],male).
lexicon(pn,marsellus,[marsellus,wallace],male).
lexicon(pn,marvin,[marvin],male).
lexicon(pn,mia,[mia],female).
lexicon(pn,mia,[mia,wallace],female).
lexicon(pn,pumpkin,[pumpkin],male).
lexicon(pn,thewolf,[the,wolf],male).
lexicon(pn,vincent,[vincent],male).
lexicon(pn,vincent,[vincent,vega],male).
lexicon(pn,yolanda,[yolanda],female).


/*========================================================================
   Noun Phrases: lexicon(np,Symbol,Words,Type)
========================================================================*/

lexicon(np,person,[who],wh).

/*========================================================================
   Intransitive Verbs: lexicon(iv,Symbol,Words,{fin,inf})
========================================================================*/

lexicon(iv,collapse,[collapses],fin).
lexicon(iv,collapse,[collapse],inf).
lexicon(iv,dance,[dances],fin).
lexicon(iv,dance,[dance],inf).
lexicon(iv,die,[dies],fin).
lexicon(iv,die,[die],inf).
lexicon(iv,growl,[growls],fin).
lexicon(iv,growl,[growl],inf).
lexicon(iv,okay,[is,okay],fin).
lexicon(iv,outoftown,[is,out,of,town],fin).
lexicon(iv,married,[is,married],fin).
lexicon(iv,playairguitar,[plays,air,guitar],fin).
lexicon(iv,playairguitar,[play,air,guitar],inf).
lexicon(iv,smoke,[smokes],fin).
lexicon(iv,smoke,[smoke],inf).
lexicon(iv,snort,[snorts],fin).
lexicon(iv,snort,[snort],inf).
lexicon(iv,shriek,[shrieks],fin).
lexicon(iv,shriek,[shriek],inf).
lexicon(iv,walk,[walks],fin).
lexicon(iv,walk,[walk],inf).

/*========================================================================
   Transitive Verbs: lexicon(tv,Symbol,Words,{fin,inf})
========================================================================*/

lexicon(tv,clean,[cleans],fin).
lexicon(tv,clean,[clean],inf).
lexicon(tv,drink,[drinks],fin).
lexicon(tv,drink,[drink],inf).
lexicon(tv,date,[dates],fin).
lexicon(tv,date,[date],inf).
lexicon(tv,discard,[discards],fin).
lexicon(tv,discard,[discard],inf).
lexicon(tv,eat,[eats],fin).
lexicon(tv,eat,[eat],inf).
lexicon(tv,enjoy,[enjoys],fin).
lexicon(tv,enjoy,[enjoy],inf).
lexicon(tv,hate,[hates],fin).
lexicon(tv,hate,[hate],inf).
lexicon(tv,have,[has],fin).
lexicon(tv,have,[have],inf).
lexicon(tv,donewith,[is,done,with],fin).
lexicon(tv,kill,[kills],fin).
lexicon(tv,kill,[kill],inf).
lexicon(tv,know,[knows],fin).
lexicon(tv,know,[know],inf).
lexicon(tv,like,[likes],fin).
lexicon(tv,like,[like],inf).
lexicon(tv,love,[loves],fin).
lexicon(tv,love,[love],inf).
lexicon(tv,pickup,[picks,up],fin).
lexicon(tv,pickup,[pick,up],inf).
lexicon(tv,shoot,[shot],fin).
lexicon(tv,shoot,[shoots],fin).
lexicon(tv,shoot,[shoot],inf).
lexicon(tv,tell,[told],fin).
lexicon(tv,tell,[tell],inf).
lexicon(tv,worksfor,[works,for],fin).
lexicon(tv,worksfor,[work,for],inf).

/*========================================================================
   Copula
========================================================================*/

lexicon(cop,'=',[is],fin).

/*========================================================================
   Prepositions: lexicon(prep,Symbol,Words,_)
========================================================================*/

lexicon(prep,about,[about],_).
lexicon(prep,in,[in],_).
lexicon(prep,of,[of],_).
lexicon(prep,with,[with],_).


/*========================================================================
   Prepositions: lexicon(adj,Symbol,Words,Antonyms)
========================================================================*/

lexicon(adj,big,[big],[small]).
lexicon(adj,kahuna,[kahuna],[]).
lexicon(adj,male,[male],[female]).
lexicon(adj,female,[female],[male]).
lexicon(adj,concrete,[concrete],[abstract]).
lexicon(adj,animate,[animate],[inanimate]).
lexicon(adj,human,[human],[nonhuman]).
lexicon(adj,mobile,[mobile],[immobile]).
lexicon(adj,edible,[edible],[drinkable]).
lexicon(adj,married,[married],[unmarried]).
lexicon(adj,living,[living],[nonliving]).


/*========================================================================
   Pronouns: lexicon(pro,Sym,Words,{refl,nonrefl})
========================================================================*/

lexicon(pro,male,[he],nonrefl).
lexicon(pro,female,[she],nonrefl).
lexicon(pro,nonhuman,[it],nonrefl).
lexicon(pro,male,[him],nonrefl).
lexicon(pro,female,[her],nonrefl).
lexicon(pro,male,[himself],refl).
lexicon(pro,female,[herself],refl).
lexicon(pro,nonhuman,[itself],refl).

/*========================================================================
   Relative Pronouns: lexicon(relpro,_,Words,_)
========================================================================*/

lexicon(relpro,_,[who],_).
lexicon(relpro,_,[that],_).

/*========================================================================
   Coordinations: lexicon(coord,_,Words,{conj,disj})
========================================================================*/

lexicon(coord,_,[and],conj).
lexicon(coord,_,[or],disj).

/*========================================================================
   Modifiers: lexicon(mod,_,Words,Type)
========================================================================*/

lexicon(mod,_,[does,not],neg).
lexicon(mod,_,[did,not],neg).

