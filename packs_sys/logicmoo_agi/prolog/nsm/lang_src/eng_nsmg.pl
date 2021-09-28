/*
    This file is part of NSM-DALIA, an extensible parser and generator
    for NSM grammars.
       
    Copyright (C) 2009 Francesco Zamblera.

    This program is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation, either version 3 of
    the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/


:- include('../bin/operators.pl').

synt_grammar_type(dependency).
morph_grammar_type(dependency). 
                      
morph_threshold(40).   %  WORD LEVEL
                       
dep_threshold(1,200). 
                       % PHRASE LEVEL
dep_threshold(2,300).  
                       % CLAUSE LEVEL
dep_threshold(3,400).
                       % SENTENCE LEVEL
dep_threshold(4,500).

max_dep_threshold(4).


transcr_table([
     "no-one":"noone", "cannot":"can not",
      "sometimes" : "some times", "sh":"S", "ch":"C"]).

e_triggers << ["C","S","s","z","x","o","i"].
vow << ["a","e","i","o","u"].

ph : [ "s" => "es", [C]-[], cond : [C << e_triggers]].
ph : [ X+"i" => X+"y", []-"#", cond : []].
ph : [ "'s" => "'", "s"-"#", cond : []].

ph : [ X+"e" => X, []-"ing#", cond : []].
ph : [ "have" => "ha", []-"d#", l:[]-"ed", cond : []].
ph : [ "have" => "ha", []-"s#", l:[]-"s", cond : []].
ph : [ "do" => "di", []-"d#", l:[]-"ed", cond : []].
ph : [ "will" => "woul", []-"d#", l:[]-"ed", cond : []].
ph : [ "can" => "coul", []-"d#", l:[]-"ed", cond : []].
ph : [ "feel" => "fel", []-"t#", l:[]-"ed", cond : []].

ph : [ "see"-"ed#" => "saw#", []-[], cond : []].

ph : [ "ed" => "d", [V]-"#", cond : [V << vow]].
ph : [ "ed" => "d", "oul"-"#", cond : []].
ph : [ "ed" => "d", "hear"-"#", cond : []].
ph : [ "ed" => "t", "fel"-"#", l:"feel"-[], cond : []].

ph : [ "en" => "ne", "do"-"#", cond : []].

% SENTENCE LEVEL

425 dr ct(clause(if),S1) - ct(s,S2) ==>  ct(s,if(S1,S2)).
425 dr ct(clause(because),S1) - ct(s,S2) ==>  ct(s,because(S1,S2)).
425 dr ct(clause(when),S1) - ct(s,S2) ==>  ct(s,when(S1,S2)).
425 dr ct(s,S2) + ct(clause(as),S1) ==>  ct(s,as(S1,S2)).

420 dr ct(s,s(NEG,Tense,CAN,T,D,p(EVAL,[o:sp(this, e, sing(e), [], something(fact))|A]),e,e)) +
       ct(clause(if),S1) ==>
       ct(s,s(NEG,Tense,CAN,T,D,p(EVAL,[o:S1|A]))) // [eval(EVAL)].
420 dr ct(s,s(NEG,Tense,CAN,T,D,p(EVAL,[o:sp(this, e, sing(e), [], something(fact))|A]),e,e)) +
       ct(v(_),p(V,[e,e,inf,e,e],ARG))
        ==>
       ct(s,s(NEG,Tense,CAN,T,D,p(EVAL,[o:p(V,ARG)|A]))) // [eval(EVAL)].


415 dr ct(adv(sent),maybe) + ct(s,S) ==> ct(s,maybe(S)).
415 dr ct(pp(cause),N) - ct(s,S) ==> ct(s,because(S,N)).

414 dr ct(neg,not) + ct(pp(cause),K) ==> ct(pp(cause),not(K)).

413  dr ct(conj,CONJ) + ct(s,S) ==>
        ct(clause(CONJ),S).


410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,[e:S,o:e|A]),LOC,MAN)) +
       ct(d(_),that) + 
       ct(s,s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,
	       [e:S,
	        o:s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)|A]),
		LOC,MAN)).
410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,[e:S,o:e|A]),LOC,MAN)) +
       ct(d(_),that) + 
       ct(s,s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,
	       [e:S,
	        o:s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)|A]),
		LOC,MAN)).

410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S,o:e]),LOC,MAN)) +
       ct(v(T),p(V,[PROG,PF,inf,e,NEG1],[R:e|A])) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S,o:[p(V2,[R:e|A])]]),LOC,MAN))
       // [v_form(V,PF,PROG,V1),
	  pred_neg(NEG1,V1,V2)].

410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S,o:sp(AA,BB,CC,DD,EE)]),LOC,MAN)) +
       ct(v(T),p(V,[PROG,PF,inf,e,NEG1],[R:e|A])) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S,o:[p(V2,[R:sp(AA,BB,CC,DD,EE)|A])]]),LOC,MAN))
      // [v_form(V,PF,PROG,V1),
	  pred_neg(NEG1,V1,V2)].


% CLAUSE LEVEL
 

320 dr ct(pp(time),sp(D,Alt,sing(e),[],time(time))) 
     - ct(s,s(NEG,Tense,CAN,e,DUR,p(V1,A),LOC,MAN))
        ==>
       ct(s,s(NEG,Time,CAN,e,DUR,p(V1,A),LOC,MAN)) //
       [time_loc(D,Alt,D1),
	time_agr(e,at,D1,Time,Tense)].

320 dr ct(adv(time(_)),time(Ext,P,D)) 
     - ct(s,s(NEG,Tense,CAN,e,DUR,p(V1,A),LOC,MAN))
    ==>
       ct(s,s(NEG,Time,CAN,e,DUR,p(V1,A),LOC,MAN)) //
       [time_agr(Ext,P,D,Time,Tense)].

320 dr  ct(n(_),sp(e,e,sing(one),[],time(time))) 
      - ct(s,s(NEG,Tense,CAN,e,e,p(V1,A),LOC,MAN)) ==>
        ct(s,s(NEG,Tense,CAN,freq(one),e,p(V1,A),LOC,MAN)).
320 dr  ct(n(_),sp(e,e,plur(NUM),[],time(time))) 
      - ct(s,s(NEG,Tense,CAN,e,e,p(V1,A),LOC,MAN)) ==>
        ct(s,s(NEG,Tense,CAN,freq(NUM),e,p(V1,A),LOC,MAN)).

320 dr  ct(s,s(NEG,Tense,CAN,e,e,p(V1,A),LOC,MAN)) + ct(pp(b),PP)
        ==>
       ct(s,s(NEG,Tense,CAN,e,dur(DUR),p(V1,A),LOC,MAN)) //
       [duration_mod(PP,DUR)].

315 dr  ct(s,s(not,Tense,CAN,e,DUR,p(V1,A),LOC,MAN)) +
        ct(adv(a),anymore) ==>
       ct(s,s(neg,Tense,CAN,e,DUR,p(more(V1),A),LOC,MAN)).

310 dr ct(s,s(NEG,Tense,CAN,e,DUR,p(V1,A),LOC,MAN)) +
       ct(n(_),sp(AA,BB,CC,DD,times(X))) ==> 
       ct(s,s(NEG,Tense,CAN,sp(AA,BB,CC,DD,times(X)),DUR,p(V1,A),LOC,MAN)).

310 dr ct(s,s(NEG,Tense,CAN,TIMES,e,p(V1,A),LOC,MAN)) +
       ct(pp(b),sp(ref(unkn),e,sing(e),[Attr],time(1))) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,dur(Attr),p(V1,A),LOC,MAN)).


310 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),e,MAN)) +
       ct(adv(l(_)),ADV) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),l(ADV),MAN)).

310 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),e,MAN)) +
       ct(pp(l),sp(AA,BB,CC,DD,somewhere(X))) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),l(sp(AA,BB,CC,DD,somewhere(X))),MAN)).

310 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),LOC,e)) +
       ct(pp(l),sp(AA,BB,CC,DD,manner(X))) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),LOC,sp(AA,BB,CC,DD,manner(X)))).

310 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),LOC,e)) +
       ct(adv,like(this)) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),LOC,manner(this))).


% EXISTENCE (THERE IS, EXIST rientra nel normale sogg-v dep).
305 dr      ct(adv(l(i)),there )
	  + ct(v(v),p(be,[PROG,PF,TPAST,TFUT,NEG],[j:Subj,o:sp(D,Alt,Num,Ev,N1)])) 
	==> ct(s,s(NEG1,Tense,CAN,e,e,p(V1,[j:sp(D,Alt,Num,Ev,N)]),e,e)) 
	 // 	[subj_case(N,N1),
		 indet_sp(D,Num,N1),
		 neg_existence(NEG,D,NEG1),
		 v_par(be,TPAST,TFUT,PF,PROG,CAN,Tense,Subj,Num,N),
	         v_form(exist,PF,PROG,V1)].

305 dr ct(n(_),sp(Det,Alt,Num,Ev,N1)) + ct(v(v),p(V,[PROG,PF,TPAST,TFUT,NEG],[R:Subj|A])) ==>
        ct(s,s(NEG,Tense,CAN,e,e,p(V1,[R:sp(Det,Alt,Num,Ev,N)|A]),e,e)) //
	[subj_case(N,N1),
	 v_par(V,TPAST,TFUT,PF,PROG,CAN,Tense,Subj,Num,N),
	 v_form(V,PF,PROG,V1)].

305 dr ct(n(_),sp(Det,Alt,Num,Ev,N1)) + ct(v(stat),p(V,[e,PF,TPAST,TFUT,NEG],[R:Subj|A])) ==>
        ct(s,s(NEG,Tense,CAN,e,e,p(V1,[R:sp(Det,Alt,Num,Ev,N)|A]),e,e)) //
	[subj_case(N,N1),
	 v_par(be,TPAST,TFUT,PF,e,CAN,Tense,Subj,Num,N),
	 v_form(V,PF,e,V1)].
/*
305 dr ct(n(d),sp(this,e,sing(e),[],fact)) + ct(v(stat),p(V,[e,PF,TPAST,TFUT,NEG],[o:Subj|A])) ==>
        ct(s,s(NEG,Tense,CAN,e,e,p(V1,[o:prop|A]))) //
	[v_par(be,TPAST,TFUT,PF,e,CAN,Tense,Subj,sing(e),something(_)),
	 v_form(V,PF,e,V1)].
*/



250 dr ct(v(aux),p(be,F,[j:S])) +  ct(pp(R),sp(A,B,C,D,E)) ==>
	  ct(v(v),p(be,F,[j:S,R:sp(A,B,C,D,E)])).
250 dr ct(v(aux),p(be,F,[j:S])) +  ct(n(_),sp(A,B,C,D,E)) ==>
	  ct(v(v),p(be,F,[j:S,o:sp(A,B,C,D,E)])).
250 dr ct(v(aux),p(be,F,[j:S])) +  ct(a,A) ==>
	  ct(v(stat),p(A,F,[o:S|GOOD_FOR])) //
	  [good_for(A,GOOD_FOR)].
250 dr ct(v(aux),p(be,F,[j:S])) +  ct(adv(l(_)),ADV)==>
	  ct(v(v),p(be,F,[j:S,l:ADV])).
250 dr ct(v(aux),p(be,F,[j:S])) +  ct(adv,like(X))==>
	  ct(v(stat),p(like,F,[j:S,o:X])).
250 dr ct(v(aux),p(have,F,[j:S])) +  ct(n(_),sp(A,B,C,D,E)) ==>
	  ct(v(v),p(have,F,[j:S,o:sp(A,B,C,D,E)])).


230 dr ct(v(aux),p(V,[F1,F2,F3,F4,e],R)) + ct(neg,not) ==>
       ct(v(aux),p(V,[F1,F2,F3,F4,not],R)). 

225 dr ct(v(T),p(V,F,[S,O,Da,Co,R:e|A])) + ct(pp(R),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[S,O,Da,Co,R:sp(AA,B,C,D,E)|A])).

220 dr ct(v(T),p(V,F,[S,O,Da,R:e|A])) + ct(pp(R),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[S,O,Da,R:sp(AA,B,C,D,E)|A])).

215 dr ct(v(T),p(V,F,[S,O,R:e|A])) + ct(pp(R),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[S,O,R:sp(AA,B,C,D,E)|A])).

210 dr ct(v(T),p(V,F,[R:S,R1:e|A])) + ct(pp(R1),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[R:S,R1:sp(AA,B,C,D,E)|A])).

207 dr ct(v(T),p(V,F,[R:S,o:e|A])) + ct(d(_),this) ==>
	  ct(v(T),p(V,F,[R:S,o:this|A])).

205 dr ct(v(T),p(V,[F1,F2,F3,F4,NEG],[R:S,o:e|A])) 
     + ct(n(_),sp(AA,B,C,D,E))
     ==> ct(v(T),p(V,[F1,F2,F3,F4,NEG1],[R:S,o:sp(AA1,B,C,D,E)|A])) 
     // [obj_det(NEG,NEG1,AA,AA1)].

205 dr ct(v(T),p(V,F,[R:S,o:e|A])) + ct(alt,more) ==>
	  ct(v(T),p(V,F,[R:S,o:more|A])).



57 dr ct(v(aux),p(fut,[e,e,TPAST,e,NEG],_)) + ct(v(T),p(V,[PROG,PF,e,e,e],A)) 
         ==> ct(v(T),p(V,[PROG,PF,TPAST,fut,NEG],A)).
57 dr ct(v(aux),p(can,[e,e,TPAST,e,NEG],_)) + ct(v(T),p(V,[PROG,PF,e,e,e],A)) 
         ==> ct(v(T),p(V,[PROG,PF,TPAST,can,NEG],A)).


56 dr  ct(neg,not) + ct(v(T),p(V,[F1,F2,inf,e,e],A))  ==>
          ct(v(T),p(V,[F1,F2,inf,e,not],A)).

55 dr ct(p,d) + ct(v(T),p(V,[F1,F2,e,e,e],A)) 
         ==> ct(v(T),p(V,[F1,F2,inf,e,e],A)).

54 dr ct(v(aux),p(have,[e,e,F3,F4,F5],[_:S|_])) + ct(v(T),p(V,[F1,en,e,e,e],[R:e|A])) 
         ==> ct(v(T),p(V,[F1,pf,F3,F4,F5],[R:S|A])).
54 dr ct(v(aux),p(have,[e,e,F3,F4,F5],[_:S|_])) + ct(v(T),p(V,[F1,ed,e,e,e],[R:e|A])) 
         ==> ct(v(T),p(V,[F1,pf,F3,F4,F5],[R:S|A])).

53 dr ct(v(aux),p(do,[e,e,F2,e,not],[_:S])) + ct(v(v),p(V,[e,e,e,e,e],[R:e|A]))
         ==> ct(v(v),p(V,[e,e,F2,e,not],[R:S|A])).
53 dr ct(v(aux),p(be,[e,F1,F2,F3,F5],[j:S])) + ct(v(T),p(V,[ing,e,e,e,e],[R:e|A]))
         ==> ct(v(T),p(V,[i,F1,F2,F3,F5],[R:S|A])).


51 dr ct(v(aux),p(V,[F1,F2,F3,F4,e],A)) + ct(neg,not) ==> 
      ct(v(aux),p(V,[F1,F2,F3,F4,not],A)).
51 dr ct(v(v),p(do,[e,e,F3,e,e],[_:S|_])) 
               + ct(neg,not)  ==> 
         ct(v(aux),p(do,[e,e,F3,e,not],[_:S])).

50 dr ct(v(T),p(V,[e,e,e,e,e],A)) + ct(i,ing_form) ==> ct(v(T),p(V,[ing,e,e,e,e],A)).
50 dr ct(v(T),p(V,[e,e,e,e,e],A)) + ct(i,n_form) ==> ct(v(T),p(V,[e,en,e,e,e],A)).
50 dr ct(v(T),p(V,[e,e,e,e,e],A)) + ct(i,d_form) ==> ct(v(T),p(V,[e,e,ed,e,e],A)).
50 dr ct(v(T),p(V,[e,e,e,e,e],A)) + ct(i,s_form) ==> ct(v(T),p(V,[e,e,s,e,e],A)).



/* ADJ, FRONT */
57 dr ct(adv(l(i)),far) + ct(p,src) ==> ct(adv(l(tr)),far).
57 dr ct(adv(l(i)),very(far)) + ct(p,src) ==> ct(adv(l(tr)),very(far)).

55 dr ct(adv,very) + ct(a,A) ==> ct(a,very(A)).
55 dr ct(adv,very) + ct(adv(l(i)),far) ==> ct(adv(l(i)),very(far)).

54 dr ct(n(d-very),sp(A,B,C,[very(e)|D],E)) + ct(a,ADJ) ==> 
ct(n(d),sp(A,B,C,[very(ADJ)|D],E)).

53 dr ct(p,l) + ct(n(n),sp(e,e,sing(e),[],front)) + ct(p,gen) ==>
      ct(adv(l),front).

53 dr ct(n(d),sp(A,B,C,D,E)) + ct(adv,very) ==> ct(n(d-very),sp(A,B,C,[very(e)|D],E)).


50 dr ct(conj,because) + ct(p,gen) ==> ct(p,cause).

/* NP */

% 120 dr ct(adv(l(tr)),far) + ct(adv(l(i)),L) ==> ct(adv(l(i)),far(L)).

125 dr ct(p,P) + ct(d(_),this) ==> 
       ct(pp(R),sp(this,e,sing(e),[],something(fact)))  // [pp_role(P,this,R)].
124 dr ct(p,P) + ct(n(_),N) ==> ct(pp(R),N)  // [pp_role(P,N,R)].

122 dr ct(d(_),any) + ct(alt,more) ==> ct(adv(a),anymore).

121 dr ct(num,all) + ct(n(n),sp(D,Alt,plur(e),Eval,N)) ==>
       ct(n(n),sp(D,Alt,plur(all),Eval,N)).

120 dr ct(adv(l(tr)),P) + ct(n(_),N) ==> ct(adv(l(tr)),loc(P,N)). 
120 dr ct(adv(l(tr)),P) + ct(adv(l(i)),L)  ==> ct(adv(l(tr)),loc(P,L)). 

120 dr ct(adv(time(tr)),time(Ext,P,e))
     +   ct(n(n),sp(D,e,sing(e),[],time(time))) 
     ==> ct(adv(time(tr)),time(Ext,P,d(D))). 

119 dr ct(n(n),sp(D,e,sing(e),Attr,time(time))) + ct(adv(time(tr)),time(e,P,e)) ==>
       ct(adv(time(tr)),time(e(Ext),P,e)) // [time_ext(D,Attr,Ext)].


118 dr ct(rel,sp(A,B,C,D,H)) 
           + ct(n(_),sp(A1,B1,C1,D1,H1) )
	 ==> ct(n(n),sp(A1,B1,C1,[g:sp(A,B,C,D,H)|D1],H1)).
117 dr ct(as,sp(A,same,C,D,H)) 
           + ct(n(_),sp(A1,B1,C1,D1,H1) )
	 ==> ct(n(n),sp(A,same(sp(A1,B1,C1,D1,H1),C,D,H))).

116 dr ct(part,NUM) + ct(n(_),sp(A,B,plur(X),D,H) ) ==>
       ct(n(n),sp(e,e,NUM2,[],of(sp(A,B,plur(X),D,H)))) //
       [np_number(NUM,_,NUM2)].

115 dr ct(n(n),sp(D,ALT,NUM,ATTR,N)) + ct(p,gen) 
       ==> ct(rel,sp(D,ALT,NUM,ATTR,N)) // [is_relational(N)].

114 dr ct(n(T),sp(A,B,C,[EVAL|D],N1)) + ct(pp(b),N2) ==>
        ct(n(T),sp(A,B,C,[EVAL_FOR|D],N1)) // [eval_for(EVAL,N2,EVAL_FOR)].

113 dr ct(n(T),sp(A,B,C,[],N)) + ct(adv,like(N1)) ==>
     ct(n(T),sp(A,B,C,like(N1),N)) // [indet_sp(A,C,N)].

105 dr ct(comp,like) + ct(n(_),N) ==> ct(adv,like(N)).

100 dr ct(comp,like) + ct(d(_),this) ==> ct(adv,like(this)).

97 dr ct(n(n),sp(ref(known),same,NUM,ATTR,N)) + ct(conj,as) ==>
      ct(as,sp(ref(known),same,NUM,ATTR,N)).

95 dr ct(gen(_),N) + ct(n(T),sp(e,e,Num,A,N1)) ==> ct(n(T),sp(e,e,Num,A,of(N1,N))).

90 dr ct(n(T),N) + ct(gen,gen) ==> ct(gen(T),N).

85 dr ct(d(NUM),D) + ct(n(n),sp(e,ALT,NUM,ATTR,N)) ==> ct(n(n),sp(D,ALT,NUM,ATTR,N)).
85 dr ct(num,some) + ct(n(n),sp(e,ALT,sing(X),ATTR,N))
       ==> ct(n(n),sp(some,ALT,sing(X),ATTR,N)).


83 dr ct(alt,same) + ct(n(n),sp(e,e,NUM,Attr,N)) ==> ct(n(n),sp(e,same,NUM,Attr,N)).

80 dr ct(num,NUM) + ct(n(n),sp(e,ALT,NUM1,A,N)) ==> ct(n(n),sp(e,ALT,NUM2,A,N)) //
   [np_number(NUM,NUM1,NUM2)].

78 dr ct(num,NUM) + ct(p,gen) ==> ct(part,NUM).

76 dr ct(alt,other(2)) + ct(n(n),sp(e,e,NUM,Attr,N)) ==> ct(n(n),sp(ref(unkn),other,NUM,Attr,N)) .
75 dr ct(alt,other) + ct(n(n),sp(e,e,NUM,Attr,N)) ==> ct(n(n),sp(e,other,NUM,Attr,N)) .
73 dr ct(alt,more) + ct(n(n),sp(e,e,NUM,Attr,N)) ==> ct(n(n),sp(e,more,NUM,Attr,N)) .

72 dr ct(n(d),sp(e,e,sing(e),Attr,N)) + ct(a,A) ==> ct(n(d),sp(e,e,sing(e),[A|Attr],N)).

71 dr ct(a,A) + ct(n(n),sp(e,e,sing(e),Attr,N)) ==> ct(n(n),sp(e,e,sing(e),[A|Attr],N)).
71 dr ct(a,A) + ct(n(n),sp(e,e,plur(e),Attr,N)) ==> ct(n(n),sp(e,e,plur(e),[A|Attr],N)).

70 dr ct(n(d),sp(D,e,sing(e),Attr,N)) + ct(alt,other(1)) ==> ct(n(d),sp(D,other,sing(e),Attr,N)).

% this someone: marginal
42 dr ct(d(_),this) + ct(n(d),sp(e,e,sing(e),[],someone(X))) ==> ct(n(n),sp(this,e,sing(e),[],someone(X))).



% MORPHOLOGY

/* PLURALE NOMI */
20 dr ct(n(n),sp(e,e,sing(e),[],N))  + ct(i,s_form)
           ==> ct(n(n),sp(e,e,plur(e),[],N)).

p : neg_existence(e,no,not).
p : neg_existence(Neg,_,Neg).
p : neg_existence(e,_,e).

p : indet_sp(ref(unkn),_Num,_H).
p : indet_sp(any,_Num,_H).
p : indet_sp(no,_Num,_H).
p : indet_sp(e,plur(_),_H).
p : indet_sp(e,sing(e),someone(person)).
p : indet_sp(e,sing(e),something(thing)).
p : indet_sp(e,sing(all),someone(person)).
p : indet_sp(e,sing(all),something(thing)).


p: subj_case(me,me(subj)).
p: subj_case(X,X).


% Vanno messi in quest'ordine per via di GEN, che costruisce i primi 
% dall'ultimo (se i(_), pf(_), pf(i(_))  matcherebbero con V).
p : v_form(V,pf,i,pf(i(V))).
p : v_form(V,pf,e,pf(V)).
p : v_form(V,e,i,i(V)).
p : v_form(V,e,e,V).

%  v_par(V,TPAST,TFUT,PF,PROG,CAN,Tense,Subj,Num,N).
%  BE - pres
p : v_par(be,s,e,e,e,e,e,am,sing(e),me).
p : v_par(be,s,e,e,e,e,e,are,sing(e),you).
p : v_par(be,s,e,e,e,e,e,is,sing(_),_).
p : v_par(be,s,e,e,e,e,e,are,plur(_),_).

% BE - past
p : v_par(be,ed,e,e,e,e,before(now),was,sing(e),me).
p : v_par(be,ed,e,e,e,e,before(now),were,sing(e),you).
p : v_par(be,ed,e,e,e,e,before(now),was,sing(_),_).
p : v_par(be,ed,e,e,e,e,before(now),were,plur(_),_).


%  BE LOC - pres
p : v_par(l(_),s,e,e,e,e,e,am,sing(e),me).
p : v_par(l(_),s,e,e,e,e,e,are,sing(e),you).
p : v_par(l(_),s,e,e,e,e,e,is,sing(_),_).
p : v_par(l(_),s,e,e,e,e,e,are,plur(_),_).

% BE LOC - past
p : v_par(l(_),ed,e,e,e,e,before(now),was,sing(e),me).
p : v_par(l(_),ed,e,e,e,e,before(now),were,sing(e),you).
p : v_par(l(_),ed,e,e,e,e,before(now),was,sing(_),_).
p : v_par(l(_),ed,e,e,e,e,before(now),were,plur(_),_).

% - BE V-ing, pres
p : v_par(_,s,e,e,i,e,e,am,sing(e),me).
p : v_par(_,s,e,e,i,e,e,are,sing(e),you).
p : v_par(_,s,e,e,i,e,e,is,sing(_),_).
p : v_par(_,s,e,e,i,e,e,are,plur(_),_).

% BE V-ing, past
p : v_par(_,ed,e,e,i,e,before(now),was,sing(e),me).
p : v_par(_,ed,e,e,i,e,before(now),were,sing(e),you).
p : v_par(_,ed,e,e,i,e,before(now),was,sing(_),_).
p : v_par(_,ed,e,e,i,e,before(now),were,plur(_),_).

% FUT, CAN
p: v_par(_,e,fut,_,_,e,after(now),e,_,_).
p: v_par(_,ed,fut,_,_,e,after(before),e,_,_).
p: v_par(_,e,can,_,_,can,e,e,_,_).
p: v_par(_,ed,can,_,_,can,before(now),e,_,_).

% HAVE V-EN. Vince su PROG (you have been, inv. di you are)
p : v_par(_,s,e,pf,_,e,e,e,sing(_),something(_)).
p : v_par(_,s,e,pf,_,e,e,e,sing(_),someone(_)).
p : v_par(_,e,e,pf,_,e,e,e,_,_).
p : v_par(_,ed,e,pf,_,e,before(before),e,_,_).

% V-pres. Non ci dev'essere PROG, se no vale BE
p : v_par(_,s,e,e,e,e,e,e,sing(_),something(_)).
p : v_par(_,s,e,e,e,e,e,e,sing(_),someone(_)).
p : v_par(_,e,e,e,e,e,e,e,_,me).
p : v_par(_,e,e,e,e,e,e,e,_,you).
p : v_par(_,e,e,e,e,e,e,e,plur(_),_).

% V-past
p : v_par(_,ed,e,e,e,e,before(now),e,_,_).

p : duration_mod(sp(ref(unkn),e,sing(e),[LONG],time(time)),LONG).
p : duration_mod(sp(some,e,sing(e),[],time(time)),some).

p : time_ext(some,_,some).
p : time_ext(ref(unkn),[Ext],Ext).

p : time_loc(this,_,this).
p : time_loc(that,_,that).
p : time_loc(some,_,some).
p : time_loc(_,same,same).
p : time_loc(e,e,e).

p : time_agr(Ext,before,that,time(Ext,before,that),before(before)).
p : time_agr(Ext,after,that,time(Ext,after,that),after(before)).
p : time_agr(Ext,before,D,time(Ext,before,D),before(now)).
p : time_agr(Ext,after,D,time(Ext,after,D),after(now)).
p : time_agr(e,at,D,time(e,at(after),D),after(now)).
p : time_agr(e,at,D,time(e,at(before),D),before(now)).
p : time_agr(e,now,e,time(e,now,e),e).


p : eval_for(good,N,good(N)).
p : eval_for(bad,N,bad(N)).
p : eval_for(very(good),N,very(good(N))).
p : eval_for(very(bad),N,very(bad(N))).

p : np_number(e,sing(e),sing(e)).
p : np_number(e,sing(all),sing(all)).
p : np_number(e,plur(e),plur(e)).
p : np_number(one,sing(e),sing(one)).
p : np_number(two,plur(e),plur(two)).
p : np_number(some,plur(e),plur(some)).
p : np_number(many,plur(e),plur(many)).
p : np_number(all,plur(e),plur(all)).

p : inst_arg(R,N,[R:e|Rest],[R:N|Rest]).
p : inst_arg(R,N,[A1,R:e|Rest],[A1,R:N|Rest]).
p : inst_arg(R,N,[A1,A2,R:e|Rest],[A1,A2,R:N|Rest]).
p : inst_arg(R,N,[A1,A2,A3,R:e],[A1,A2,A3,R:N]).

p : inst_adj(yes,_,_,e,e).
p : inst_adj(no,l,N,N,e).
p : inst_adj(no,m,N,e,N).

p : is_relational(kind).
p : is_relational(part).
p : is_relational(side).

p : pp_role(c,sp(_,_,_,_,something(_)),i).
p : pp_role(R,_,R).

p : pred_neg(not,V,not(V)).
p : pred_neg(e,V,V).

p : prop_obj(know).
p : prop_obj(think).

p : good_for(good,[b:e]).
p : good_for(bad,[b:e]).
p : good_for(_,[]).


p : eval(good).
p : eval(bad).
p : eval(very(good)).
p : eval(very(bad)).

p : obj_det(not,not,any,e).
p : obj_det(e,not,no,e).
p : obj_det(e,e,D,D).
p : obj_det(not,not,D,D).


arc(start,_).
arc(_,stop).

arc(v(_),i).
arc(n(n),i).
arc(i,gen).
arc(n(_),gen).

ct(n(d),it) = sp(this,e,sing(e),[],something(fact)).

ct(n(n),people) = sp(e,e,plur(e),[],people).
ct(n(n),one_time) = sp(e,e,sing(one),[],time(time)).
ct(n(n),two_times) = sp(e,e,plur(two),[],time(time)).
ct(n(n),many_times) = sp(e,e,plur(many),[],time(time)).
ct(n(n),all_times) = sp(e,e,plur(all),[],time(time)).

ct(n(d),everything) = sp(e, e, sing(all), [], something(thing)).
ct(n(d),everyone) = sp(e, e, sing(all), [], someone(person)).
ct(n(d),everywhere) = sp(e, e, sing(all), [], somewhere(place)).

ct(n(d),any(N)) = sp(any,e,sing(e),[],N).
ct(n(d),no(N)) = sp(no,e,sing(e),[],N).

ct(n(_),N) = sp(e,e,sing(e),[],N).

/*
ct(n(_),someone(N)) = sp(e,e,sing(e),[],someone(N)).
ct(n(_),something(N)) = sp(e,e,sing(e),[],something(N)).
ct(n(_),somewhere(N)) = sp(e,e,sing(e),[],somewhere(N)).
ct(n(_),time(N)) = sp(e,e,sing(e),[],sometime(N)).
*/

ct(v(v),happen) = p(happen,[e,e,e,e,e],[o:e,d:e]).
ct(v(v),do) = p(do,[e,e,e,e,e],[a:e,o:e,d:e,c:e,i:e]).
ct(v(v),move) = p(move,[e,e,e,e,e],[a:e]).

ct(v(v),die) = p(die,[e,e,e,e,e],[o:e]).
ct(v(v),live) = p(live,[e,e,e,e,e],[a:e,c:e]).

ct(v(v),feel) = p(feel,[e,e,e,e,e],[e:e,o:e,g:e]).
ct(v(v),know) = p(know,[e,e,e,e,e],[e:e,o:e,t:e]).
ct(v(v),think) = p(think,[e,e,e,e,e],[e:e,o:e,t:e]).
ct(v(v),say) = p(say,[e,e,e,e,e],[a:e,o:e,d:e,t:e]).
ct(v(v),hear) = p(hear,[e,e,e,e,e],[a:e,o:e,t:e]).
ct(v(v),see) = p(see,[e,e,e,e,e],[a:e,o:e]).
ct(v(v),want) = p(want,[e,e,e,e,e],[e:e,o:e]).
ct(v(v),touch) = p(touch,[e,e,e,e,e],[a:e,o:e]).

ct(v(v),exist) = p(exist,[e,e,e,e,e],[j:e]).

ct(v(aux),am) = p(be,[e,e,s,e,e],[j:am]).
ct(v(aux),is) = p(be,[e,e,s,e,e],[j:is]).
ct(v(aux),are) = p(be,[e,e,s,e,e],[j:are]).
ct(v(aux),was) = p(be,[e,e,ed,e,e],[j:was]).
ct(v(aux),were) = p(be,[e,e,ed,e,e],[j:were]).
ct(v(aux),V) = p(V,[e,e,e,e,e],[j:e]).

ct(adv(time(_)),A) = time(e,A,e).


m(i,"s",s_form).
m(i,"ing",ing_form).
m(i,"ed",d_form).
m(i,"en",n_form).

m(gen,"'s",gen).

m(n(n),"people",people).

m(n(d),"someone",someone(person)).
m(n(d),"something",something(thing)).
m(n(d),"sometime",time(time)).
m(n(d),"somewhere",somewhere(place)).
m(n(d),"somehow",manner(manner)).

m(n(d),"anyone",any(someone(person))).
m(n(d),"anything",any(something(thing))).
m(n(d),"anytime",any(time(time))).
m(n(d),"anywhere",any(somewhere(place))).
m(n(d),"anyhow",any(manner(manner))).

m(n(d),"everyone",everyone).
m(n(d),"everything",everything).
m(n(d),"everywhere",everything).


m(n(d),"noone",no(someone(person))).
m(n(d),"nothing",no(something(thing))).
m(n(d),"notime",no(time(time))).
m(n(d),"nowhere",no(somewhere(place))).
m(n(d),"nohow",no(manner(manner))).

m(n(n),"person",someone(person)).
m(n(n),"thing",something(thing)).
m(n(n),"time",time(time)).
m(n(n),"place",somewhere(place)).
m(n(n),"way",manner(manner)).


m(n(p),"I",me(subj)).
m(n(p),"me",me).
m(n(p),"you",you).

m(n(n),"bodi",body).
m(n(n),"kind",kind).
m(n(n),"part",part).

m(num,"one",one).
m(num,"two",two).
m(num,"some",some).
m(num,"many",many).
% m(num,"muC",many(1)).
m(num,"all",all).

m(alt,"more",more).

m(a,"good",good).
m(a,"bad",bad).
m(a,"big",big).
m(a,"small",small).

m(d(sing(_)),"this",this).
m(d(sing(_)),"that",that).
m(d(sing(_)),"a",ref(unkn)).
m(d(sing(_)),"an",ref(unkn)).
m(d(_),"the",ref(known)).
m(d(_),"any",any).
m(d(_),"no",no).
m(d(plur(_)),"these",this).
m(n(d),"it",it).

m(alt,"same",same).
m(alt,"other",other).
m(alt,"else",other(1)).
m(alt,"another",other(2)).

m(adv,"very",very).

m(comp,"like",like).

m(v(aux),"be",be).
m(v(aux),"am",am).
m(v(aux),"is",is).
m(v(aux),"was",was).
m(v(aux),"are",are).
m(v(aux),"were",were).
m(v(aux),"have",have).
m(v(aux),"will",fut).
m(v(aux),"can",can).

m(v(v),"do",do).
m(v(v),"happen",happen).
m(v(v),"move",move).
m(v(v),"touC",touch).

m(v(v),"live",live).
m(v(v),"die",die).
m(v(v),"exist",exist).

m(n(n),"word",word).
m(a,"true",true).
m(v(v),"sai",say).
m(v(v),"hear",hear).
m(v(v),"see",see).
m(v(v),"think",think).
m(v(v),"feel",feel).
m(v(v),"know",know).
m(v(v),"want",want).


m(adv(l(tr)),"inside",inside).
m(adv(l(tr)),"above",above).
m(adv(l(tr)),"below",below).
m(adv(l(tr)),"near",near).
m(adv(l(i)),"far",far).
m(adv(l(i)),"here",here).
m(adv(l(i)),"there",there).
m(n(n),"front",front).
m(n(n),"side",side).

m(adv(time(i)),"now",now).
m(adv(time(tr)),"before",before).
m(adv(time(tr)),"after",after).
m(a,"long",long).
m(a,"Sort",short).
m(n(n),"once",one_time).
m(n(n),"twice",two_times).
m(n(n),"often",many_times).
m(n(n),"always",all_times).

m(neg,"not",not).
m(conj,"because",because).
m(conj,"if",if).
m(conj,"when",if).
m(conj,"as",as).

m(p,"to",d).
m(p,"in",l).
m(p,"on",l).
m(p,"with",c).
m(p,"about",t).
m(p,"for",b).
m(p,"at",time).
m(p,"of",gen).
m(p,"from",src).
m(p,"towards",g).

m(adv(sent),"maybe",maybe).

m(n(n),"God",someone(god)).
m(n(n),"bird",something(bird)).

m(group,"",begin).
m(group,"",end).


