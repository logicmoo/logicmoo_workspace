/*************************************************************************

         name: threadingDRT1.pl (Volume 2, Chapter 2)
      version: July 29, 2001
  description: DRS-threading (Johnson & Klein 1986)
      authors: Patrick Blackburn & Johan Bos
       typist: Christof Rumpf, 15.01.03 (code taken from book)
 
*************************************************************************/
/*
s(DrsIn-DrsOut) -->
	np(X,DrsIn-Drs),
	vp(X,Drs-DrsOut).
*/	
s(Drs) -->
	np(X,Drs,Scope),
	vp(X,Scope).

np(X,DrsIn-DrsOut,Drs-DrsOut) --> pn(X,DrsIn-Drs).

np(X,Drs,Scope) -->
	det(X,Drs,Restr,Scope),
	noun(X,Restr).

vp(X,DrsIn-DrsOut) --> iv(X,DrsIn-DrsOut).

iv(X,drs(Dom,Conds)-drs(Dom,[dance(X)|Conds])) --> [dances].

pn(X,drs(Dom,Conds)-drs([X|Dom],[X=mia|Conds])) --> [mia].

noun(X,drs(Dom,Conds)-drs([Dom],[gimp(X)|Conds])) --> [gimp].

det(X,DrsIn-DrsOut,RestrIn-RestrOut,ScopeIn-ScopeOut) -->
	[every],
	{
	 DrsIn = drs(Dom,Conds),
	 DrsOut = drs(Dom,[RestrOut > ScopeOut|Conds]),
	 RestrIn = drs([X],[]),
	 ScopeIn = drs([],[])
	}.
