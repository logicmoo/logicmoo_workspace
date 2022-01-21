%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%================================================================================
% THREAD(GoalsIn,SIn,SOut,ThreadedGoals)
%================================================================================

thread((Cond->Then;Else),SIn,SOut,Answer) :- !,
	thread(Cond,SIn,S1,CondOut),
	thread(Then,S1,S2,ThenOut),
	thread(Else,SIn,S3,ElseOut),
	Answer = ( CondOut 
		 -> (ThenOut, SOut = S2)
		 ;  (ElseOut, SOut = S3)
		 ).

thread((Cond->Then),SIn,SOut,Answer) :- !,
	thread(Cond,SIn,S1,CondOut),
	thread(Then,S1,S2,ThenOut),
	Answer = ( CondOut 
		 -> (ThenOut, SOut = S2)
		 ).

thread(if(Cond,Then,Else),SIn,SOut,Answer)  :- !,
	thread(Cond,SIn,S1,CondOut),
	thread(Then,S1,S2,ThenOut),
	thread(Else,SIn,S3,ElseOut),
	Answer = if(CondOut,
	            (ThenOut, SOut = S2),
		    (ElseOut, SOut = S3)
		   ).

thread((A,B),SIn,SOut,Answer) :- !,
	thread(A,SIn,S1,AOut),
	thread(B,S1,SOut,BOut),
	Answer = (AOut,BOut).

thread((A;B),SIn,SOut,Answer) :- !, 
	thread(A,SIn,S1,AOut),
	thread(B,SIn,S2,BOut),
	Answer = ((AOut, SOut = S1)
		 ;(BOut, SOut = S2)).

% If call is variable, won't thread right if requires state.
thread(call(Call),SIn,SOut,Answer) :- !,
	( var(Call) 
	-> ( Answer =  call(Call),
	     SIn = SOut )
	;  ( thread(Call,SIn,SOut,CallOut),
	     Answer =  call(CallOut)
	   )
	).

thread((\+ Call),SIn,SOut,Answer) :- !,
	thread(Call,SIn,_S1,CallOut),
	Answer = ((\+ CallOut), SIn = SOut).

thread(setof(X,Test,Xs),SIn,SIn,Answer) :- !,
	thread(Test,SIn,S1,TestOut),
	Answer = setof(X,S1^TestOut,Xs).

thread(bagof(X,Test,Xs),SIn,SIn,Answer) :- !,
	thread(Test,SIn,S1,TestOut),
	Answer = bagof(X,S1^TestOut,Xs).

thread(X^Test,SIn,SOut,Answer) :- !,
	thread(Test,SIn,SOut,TestOut),
	Answer = X^TestOut.


thread((:-B),SIn,SOut,Answer) :- !,
	thread(B,SIn,SOut,BOut),
	Answer = (:-BOut).

thread((H:-B),SIn,SOut,Answer) :- !,
	thread(H,SIn,SOut,HOut),
	thread(B,SIn,SOut,BOut),
	Answer = (HOut:-BOut).
	
thread(true(GIn),SIn,SOut,Answer) :- !,
	SIn = SOut,
	Answer = true_in(GIn,SIn).

thread(add(GIn),SIn,SOut,Answer) :- !,
	Answer = add_in(GIn,SIn,SOut).

thread(del(GIn),SIn,SOut,Answer) :- !,
	Answer = del_in(GIn,SIn,SOut).

thread(GIn,SIn,SOut,GOut) :-
	add_state(GIn,SIn,SOut,GOut).

