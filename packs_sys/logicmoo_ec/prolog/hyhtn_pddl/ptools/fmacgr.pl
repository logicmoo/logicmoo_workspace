

/* ************* fmacgr fmacgr fmacgr fmacgr fmacgr **************** */

 

/* This file contains a procedure to make solutions 
   into MACRO OPERATORS      F M A C G R          
   --- Uses fred4 as a scratch file             
   --- This version uses 'goal regression'  */

mac(X) :- frame(name: X,
		type: problem,
		ancest:_,
		init_world: I,
		goal: GG,
		_,
		solution: S),
		mac1(S,I,GG,X).

macros(R,I,G,X) :- 
	producemacros(on),
	macros1(R,I,G,X). 
macros(_,_,_,_).


macros1([p(S,O,_)|R],I,G,X) :- 
	del(S,G,G),writeL(['*mac*-no goals solved by op ',O,nl,S,nl,G]),
	macros1(R,I,G,X). 
macros1([p(_,O,_)|R],I,G,X) :- 
	length(O,1),
	macros1(R,I,G,X). 
macros1([p(S,O,_)|R],I,G,X) :- 
	intersection(S,G,GG),
	gensym(X,XX),
	mac1(p(S,O,_),I,GG,XX),
	macros1(R,I,G,X). 
macros1(_,_,_,_).

mac1(p(S,OLm,_),I,GG,X) :-

	producemacros(on),
	nonvar(S),
	length(OLm,Len),
	replacemacs(OLm,OL),		/* primitives->macs,update mac'score*/
	!,Len > 1,	/* original opseq must be > 1       */
	del(I,GG,G),
	applyopseq1(OL,I,SS1),        /* no precon check */
	removelast(SS1,SS),

	reverse([I|SS],SSR),
	reverse(OL,OLR),
	/*      v  v   v     ^   ^   ^          Ch = WPe1       */
	getprec(G,OLR,SSR, WPs1,Ch,ConstLD), /* old method below */
	list_to_set(ConstLD,ConstL),

	/*getprecon(OL,Pre,Ch,[I|SS]),Pre= O1.pre+(Oi.pre-O(i-1).add) */

	applyopseq1(OL,WPs1,FL),        /*Ch are acculmutated perman.cons.*/
	last(FL,Finalstate),
	del(WPs1,Finalstate,Add),	/* Add = Finalstate - Pre */
	del(G,Add,Sadd),

	generalise(WPs1,ConstL,P1),
	generalise(Ch,ConstL,Ch11),
	generalise(G,ConstL,A2),	
	generalise(Sadd,ConstL,A1),	/* consts which are not in ConstL */
					/* are turned into variables  */
	generaliseL(ConstL,OL,OL1,V4),  /* generalize op seq. instances
  					   & collect vars in V4*/
	list_to_set(V4,V5),	   /* assume NO CONSTS IN UNINST. OPS NAMES */
	V =.. [X|V5],

	andtolist(Ch,ChL),		/* this part ensures minimal gen'n */
	generaliseL(ConstL,ChL,_,Cv),	/* by adding not equal literals to */
	list_to_set(Cv,Cv1),		/* the 'check' literals            */
	add_ne(Cv1,Ne1),
	slim(Ne1,Ne2),	/* dirty way (by 1st letter) of del'ing some ne's */
	remove(nil,Ne2,Ne),
	ad(Ch11,Ne,Ch1),
	possible_macro(X,V,OL1,Ch1,P1,A2,A1).

possible_macro(_,_,OL1,_,_,_,_) :-
	frame(name: _,
			type: operator,
			macrop:[_|OL1],
			check: _,
			precon: _,
			padd: _,
			add: _,
			delete: _
			).

possible_macro(X,V,OL1,Ch1,P1,A2,A1) :-
	clock(Time),		/* attach a value to a macro */ 
	Ti is Time + 2,		/* currently 2 more than task no. */ 
	tell(X),
	write(	frame(name: V,
			type: operator,
			macrop:[Ti|OL1],
			check: Ch1,
			precon: P1,
			padd: A2,
			add: A1,
			delete: nil)),
		write('.'),nl,told,     /* turns upper case into vars!!! */
	see(X),read(TERM),seen,assert(TERM),
	tell(X),     /* ******* report new macro ******* */
	write(V),nl,nl,
	write('check: '),write(Ch1),nl,
	write('precon: '),write(P1),nl,
	write('padd: '),write(A2),nl,
	write('add: '),write(A1),nl,
	write('macrop: '),write(OL1),nl,
	told.


add_ne([_],nil).
add_ne([],nil).
add_ne([X|Y],Z) :- add_ne1(X,Y,Z1),add_ne(Y,Z2),ad(Z1,Z2,Z).

add_ne1(_,[],nil).
add_ne1(X,[Y],ne(X,Y)).
add_ne1(X,[Z|Y],ne(X,Z)&Z1) :- 
		add_ne1(X,Y,Z1).

slim(ne(U,V)&Y,ne(U,V)&Z) :- 
		name(U,[U1|_]),name(V,[U1|_]),slim(Y,Z).
slim(ne(_,_)&Y,Z) :- slim(Y,Z).
slim(ne(U,V),ne(U,V)) :-
		name(U,[U1|_]),name(V,[U1|_]).
slim(_,nil).
			/*    ^   ^    ^    */
getprec(Abs,[O1|T],[S|T1],Abs4,Ch,ConstL) :-

		functor(O1,Fun,NN), /* ops must be unique by name & slots no.*/
		functor(Ou,Fun,NN), /* must look at uninst'ed operator */
		frame(name: Ou,_,_,_:EV,precon:PV,_,_,_),
		get_consts(PV,CS1),get_consts(EV,CS2),
		append(CS1,CS2,CS),

		frame(name:O1,_,_,check:Ch1,precon:Pr,padd:PA,add:AD,_),
		hold(Pr,S),                 /* These 3 lines instantiate */
		frame(name:_,_,always:A,_), /* vars not already done by  */
		hold(Ch1,A),		    /* op parameter instants     */
		
		del(PA,Abs,Abs1),
		del(AD,Abs1,Abs2),
		ad(Pr,Abs2,Abs3),
		getprec(Abs3,T,T1,Abs4,Ch2,CSC),
		append(CSC,CS,ConstL),
		ad(Ch1,Ch2,Ch).

getprec(Abs,[],_,Abs,nil,[]) :- !.


/*    		getprecon([O1|T],P,Ch,[S|T1]) :-
		frame(name:O1,_,_,check:Ch1,precon:Pr,_,_,_),
		hold(Pr,S),                 
		frame(name:_,_,always:A,_),
		hold(Ch1,A),		  
		getpre([O1|T],P1,Ch2,T1,nil),
		ad(Ch1,Ch2,Ch),
		ad(Pr,P1,P).
		getpre([O1,O2],P1,Ch,[S],Astore) :- 
		frame(name:O1,_,_,_,_,padd:PA,add:A,_),
		frame(name:O2,_,_,check:Ch,precon:Pr,_,_,_),
		hold(Pr,S),
		frame(name:C,_,always:Always,_),
		hold(Ch,Always),
		ad(PA,A,AA),
		ad(AA,Astore,A1),
		del(A1,Pr,P1).
		getpre([O1|[O2|T]],P2,Ch,[S|T1],Astore) :-
		frame(name:O1,_,_,_,_,padd:PA,add:A,_),
		frame(name:O2,_,_,check:Ch2,precon:Pr,_,_,_),
		hold(Pr,S),
		frame(name:_,_,always:Always,_),
		hold(Ch2,Always),
		ad(PA,A,AA),
		ad(AA,Astore,A1),
		del(A1,Pr,P1),
		getpre([O2|T],P,Ch3,T1,A1),
		ad(Ch2,Ch3,Ch),
		ad(P1,P,P2).                  */

/* This part changes constants to variables    NB: only 1-depth         */
/* by turning the 1st letter to a capital.                              */
/* -the first parameter must be a &-exp            constants considered */ 

generalise(nil,_,nil).
generalise(X&Y,ConstL,X1&Y1) :-	
		X =.. [H|T],
		genlist(ConstL,T,T1,_),
		X1 =.. [H|T1],!,
		generalise(Y,ConstL,Y1).
generalise(X,ConstL,X1) :-
		X =.. [H|T],
		genlist(ConstL,T,T1,_),
		X1 =.. [H|T1].

generaliseL(ConstL,[X|Y],[X1|Y1],L2) :-
		X =.. [H|T],
		genlist(ConstL,T,T1,L1),
		X1 =.. [H|T1],!,
		generaliseL(ConstL,Y,Y1,L3),
		append(L1,L3,L2).
generaliseL(_,[],[],[]).

genlist(_,[],[],[]).
genlist(ConstL,[H|T],[H1|T1],[H1|T2]) :-      
		not(var(H)),
		not(member(H,ConstL)), /* CHECK H IS NOT A SPECIAL CONST */
	  	name(H,[I|J]),		
		I1 is I - 32,
		name(H1,[I1|J]),
		genlist(ConstL,T,T1,T2).
genlist(ConstL,[H|T],[H|T1],L) :-
		genlist(ConstL,T,T1,L).

/* take a list of instantiated ops and replace any macros with their
   primitives */

replacemacs([],[]).

replacemacs([O|OL],T1) :-
		frame(name:O,_,macrop:[_|T],check:Ch,_,_,_,_),
		increment(O),
		frame(name:_,_,always:Always,_),	/* instantiate params*/
		hold(Ch,Always),
		replacemacs(OL,T2),
		append(T,T2,T1).
replacemacs([O|T],[O|T1]) :-
			replacemacs(T,T1).
		
increment(O) :-
		functor(O,Fun,NN),
		functor(Ou,Fun,NN),
		retract(frame(name: Ou,
			type: operator,
			macrop:[N|T],
			check: Ch,
			precon: P,
			padd: PA,
			add: A,
			delete: D)),
		N1 is N+1,
		asserta( frame(name: Ou,
			type: operator,
			macrop:[N1|T],
			check: Ch,
			precon: P,
			padd: PA,
			add: A,
			delete: D)).
