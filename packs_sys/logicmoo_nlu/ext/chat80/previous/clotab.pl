/* @(#)clotab.pl	24.1 2/23/88 */


/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

% Normal form masks

is_pp(#(1,_,_,_)).

is_pred(#(_,1,_,_)).

is_trace(#(_,_,1,_)).

is_adv(#(_,_,_,1)).

is_trace82(#(_,_,1,_),#(0,0,0,0)).

is_trace_bits(#(0,0,1,0)).

is_adv_bits(#(0,0,0,1)).

is_empty_bits(#(0,0,0,0)).

is_np_all(#(1,1,1,0)).

is_s_all(#(1,0,1,1)).

is_np_no_trace(#(1,1,0,0)).

% Mask operations

plus_mask(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   or_xmask(B1,C1,D1),
   or_xmask(B2,C2,D2),
   or_xmask(B3,C3,D3),
   or_xmask(B4,C4,D4).

minus_mask(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   anot_xmask(B1,C1,D1),
   anot_xmask(B2,C2,D2),
   anot_xmask(B3,C3,D3),
   anot_xmask(B4,C4,D4).

or_xmask(1,_,1).
or_xmask(0,1,1).
or_xmask(0,0,0).

anot_xmask(X,0,X).
anot_xmask(_X,1,0).

% Noun phrase position features

is_to_role_case(subj,_,#(1,0,0)).
is_to_role_case(compl,_,#(0,_,_)).
is_to_role_case(undef,main,#(_,0,_)).
is_to_role_case(undef,aux,#(0,_,_)).
is_to_role_case(undef,decl,_).
is_to_role_case(nil,_,_).

is_subj_case(#(1,0,0)).
is_verb_case(#(0,1,0)).
is_prep_case(#(0,0,1)).
is_compl_case(#(0,_,_)).


portray_bit(Bit,Value,[?(Bit)|Bits],Bits) :- var(Value), !.
portray_bit(Bit,1,[+Bit|Bits],Bits).
portray_bit(Bit,0,[-Bit|Bits],Bits).
portray_bit(Bit,What,[Bit=What|Bits],Bits).
portray_bit(_,_,Bits,Bits).


%:- op(100,fx,?).

user:portray('#'(PP,Pred,Trace,Adv)) :-
   portray_bit(pp,PP,S0,S1),
   portray_bit(pred,Pred,S1,S2),
   portray_bit(trace,Trace,S2,S3),
   portray_bit(adv,Adv,S3,[]),
   write(S0).
