/*************************************************************************

    File: semRulesCooper.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

/*========================================================================
   Semantic Rules
========================================================================*/

combine(t:Converted,[s:Sem]):- 
   betaConvert(Sem,Converted).

combine(t:Converted,[q:Sem]):- 
   betaConvert(Sem,Converted).

combine(s:app(A,B),[s:A,s:B]).
combine(s:lam(B,imp(S,B)),[if:S]).
combine(s:lam(B,or(S,B)),[either:S]).
combine(s:S,[then:S]).
combine(s:S,[or:S]).
combine(s:S,[np:[A|S1],vp:[B|S2]]):-
   appendLists(S1,S2,S3),
   sRetrieval([app(A,B)|S3],Retrieved),
   betaConvert(Retrieved,S).

combine(sinv:S,[av:[A],np:[B|S1],vp:[C|S2]]):-
   appendLists(S1,S2,S3),
   sRetrieval([app(B,app(A,C))|S3],Retrieved),
   betaConvert(Retrieved,S).
  
combine(q:Q,[whnp:[A|S1],vp:[B|S2]]):-
   sRetrieval([app(A,B)|S2],VP),
   sRetrieval([VP|S1],Retrieved),
   betaConvert(Retrieved,Q),
   Q=que(_,_,_).

combine(q:Q,[sinv:Q]):-
   Q=que(_,_,_).

combine(np:A,[pn:A]).
combine(np:A,[qnp:A]).
combine(np:[lam(P,app(P,X)),bo(app(A,B),X)|S],[det:[A],n:[B|S]]).
combine(np:[app(A,B)|S],[det:[A],n:[B|S]]).
combine(np:[app(app(B,A),C)|S3],[np:[A|S1],coord:[B],np:[C|S2]]):-
   appendLists(S1,S2,S3).

combine(whnp:[lam(P,app(P,X)),bo(app(A,B),X)|S],[det:[A],n:[B|S]]).
combine(whnp:[lam(P,app(P,X)),bo(A,X)],[qnp:[A]]).

combine(n:[app(A,B)|S],[adj:[A],n:[B|S]]).
combine(n:A,[noun:A]).
combine(n:[app(B,A)|S],[noun:[A],nmod:[B|S]]).
combine(n:[app(app(B,A),C)|S3],[n:[A|S1],coord:[B],n:[C|S2]]):-
   appendLists(S1,S2,S3).

combine(nmod:A,[pp:A]).
combine(nmod:A,[rc:A]).
combine(nmod:[lam(P,app(A,app(B,P)))|S3],[pp:[A|S1],nmod:[B|S2]]):-
   appendLists(S1,S2,S3).

combine(vp:[app(A,B)|S],[av:[A],vp:[B|S]]).
combine(vp:[app(A,B)|S],[cop:[A],np:[B|S]]).
combine(vp:A,[iv:A]).
combine(vp:[app(A,B)|S],[tv:[A],np:[B|S]]).
combine(vp:[app(app(B,A),C)|S3],[vp:[A|S1],coord:[B],vp:[C|S2]]):-
   appendLists(S1,S2,S3).

combine(pp:[app(A,B)|S],[prep:[A],np:[B|S]]).

combine(rc:[app(A,B)|S],[relpro:[A],vp:[B|S]]).
