/*  Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2011, K.U. Leuven
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(listmap,
	[
		listmap_empty/1,
		listmap_lookup/3,
		listmap_insert/4,
		listmap_remove/3,
		listmap_merge/5
	]).

listmap_empty([]).

listmap_lookup([K-V|R],Key,Q) :-
	( Key == K ->
		Q = V
	;
		Key @> K,
		listmap_lookup(R,Key,Q)
	).

listmap_insert([],Key,Value,[Key-Value]).
listmap_insert([P|R],Key,Value,ML) :-
	P = K-_,
	compare(C,Key,K),
	( C == (=) ->
		ML = [K-Value|R]
	; C == (<) ->
		ML = [Key-Value,P|R]
	;
		ML = [P|Tail],
		listmap_insert(R,Key,Value,Tail)
	).

listmap_merge(ML1,ML2,F,G,ML) :-
	( ML1 == [] ->
		ML = ML2
	; ML2 == [] ->
		ML = ML1
	;
		ML1 = [P1|R1], P1 = K1-V1,
		ML2 = [P2|R2], P2 = K2-V2,
		compare(C,K1,K2),
		( C == (=) ->
			Call =.. [F,V1,V2,NV],
			call(Call),
			ML = [K1-NV|Tail],
			listmap_merge(R1,R2,F,G,Tail)
		; C == (<) ->
			Call =.. [G,V1,NV],
			call(Call),
			ML = [K1-NV|Tail],
			listmap_merge(R1,ML2,F,G,Tail)
		;
			Call =.. [G,V2,NV],
			call(Call),
			ML = [K2-NV|Tail],
			listmap_merge(ML1,R2,F,G,Tail)
		)
	).
		
	
listmap_remove([],_,[]).
listmap_remove([P|R],Key,NLM) :-
	P = K-_,
	compare(C,Key,K),
	( C == (=) ->
		NLM = R
	; C == (<) ->
		NLM = [P|R]
	;
		NLM = [P|Tail],
		listmap_remove(R,Key,Tail)
	).
		
		
