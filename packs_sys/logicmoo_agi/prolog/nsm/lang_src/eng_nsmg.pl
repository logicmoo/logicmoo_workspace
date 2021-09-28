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

(eng:e) gtitle "English NSM Grammar".
gauthor "Francesco Zamblera".
gdate "2009".
gversion "1.0".
gackn "Cliff Goddard".

synt_grammar_type(dependency).
morph_grammar_type(dependency). 
                      

(eng:e) glevels "These are the needed levls in English".

morph_threshold(40).   %  WORD LEVEL
                       
dep_threshold(1,120). 

dep_threshold(2,300). 
                       % PHRASE LEVEL
dep_threshold(3,400).  
                       % CLAUSE LEVEL
dep_threshold(4,500).
                       % SENTENCE LEVEL
dep_threshold(5,590).

max_dep_threshold(5).

(eng:e)
gtranscr "This is a transcription table.".
transcr_table([
     "no-one":"noone", "cannot":"can not", "another" : "a other",
      "sometimes" : "some times", "sh":"S", "ch":"C"]).


/* MORPHOPHONEMIC  CLASSES */

e_triggers << ["C","S","s","z","x","o","i"].
vow << ["a","e","i","o","u"].




/* MORPHOLOGY */


% Allowed morpheme sequences

ms ::: [v(_),?i].
ms ::: [n(n),?i,?gen].
ms ::: [_].


/*
arc(start,_).
arc(_,stop).

arc(v(_),i).
arc(n(n),i).
arc(i,gen).
arc(n(_),gen).
*/

% Noun s-form (plural)
20 dr ct(n(n),sp(e,e,e,[],N))  + ct(i,s_form)
           ==> ct(n(n),sp(e,e,num(plur),[],N)).


% Verbal morphology

% -ing
30 dr ct(v(T),p(V,[e,e,e,e,e],A)) + ct(i,ing_form) ==> ct(v(T),p(V,[ing,e,e,e,e],A)).

% -en
31 dr ct(v(T),p(V,[e,e,e,e,e],A)) + ct(i,n_form) ==> ct(v(T),p(V,[e,en,e,e,e],A)).

% -ed
32 dr ct(v(T),p(V,[e,e,e,e,e],A)) + ct(i,d_form) ==> ct(v(T),p(V,[e,e,ed,e,e],A)).

% -s
33 dr ct(v(T),p(V,[e,e,e,e,e],A)) + ct(i,s_form) ==> ct(v(T),p(V,[e,e,s,e,e],A)).



/* LEXICAL DEPENDENCIES */


%VARIABLE NAMES

41 dr  ct(n(T), sp(DET, ALT, NUM, ATT, something(thing))) +
       ct(lit_name,String)
   ==> ct(n(T), sp(DET, ALT, NUM, ATT, something(name(String)))) 
   // [prolog_pred (nsm_dict:assert_dict_variable(eng:e,String,something(name(String)),
					 something(thing)))].

41 dr  ct(n(T), sp(DET, ALT, NUM, ATT, someone(person))) +
       ct(lit_name,String)
   ==> ct(n(T), sp(DET, ALT, NUM, ATT, someone(name(String))))
  // [prolog_pred (nsm_dict:assert_dict_variable(eng:e,String,someone(name(String)),
					someone(person)))].
41 dr  ct(n(T), sp(DET, ALT, NUM, ATT, somewhere(thing))) +
       ct(lit_name,String)
   ==> ct(n(T), sp(DET, ALT, NUM, ATT, somewhere(name(String)),
		  somewhere(place)))
   // [prolog_pred (nsm_dict:assert_dict_variable(eng:e,String,somewhere(name(String))))].
41 dr  ct(n(T), sp(DET, ALT, NUM, ATT, time(thing))) +
       ct(lit_name,String)
   ==> ct(n(T), sp(DET, ALT, NUM, ATT, time(name(String))))
   // [prolog_pred (nsm_dict:assert_dict_variable(eng:e,String,time(name(String)),
					 tome(time)))].

% because of
42 dr ct(conj,because) + ct(p,gen) ==> ct(p,cause).

% one of, some of
43 dr ct(num,NUM) + ct(p,gen) ==> ct(part,NUM).

% in front of
44 dr ct(p,l) + ct(n(n),sp(e,e,e,[],front)) + ct(p,gen) ==>
      ct(adv(l),front).

% far from, very far from, away from,...
45 dr ct(adv(l(i)),FAR) + ct(p,src) ==> ct(adv(l(tr)),FAR) //
   [adv_from(FAR)].




/* VERY */

47 dr ct(adv,very) + ct(a,A) ==> ct(a,very(A)).
48 dr ct(adv,very) + ct(adv(l(i)),X) ==> ct(adv(l(i)),very(X)).



/* VERB GROUP */

59 dr ct(v(aux),p(fut,[e,e,TPAST,e,NEG],_)) + ct(v(T),p(V,[PROG,PF,e,e,e],A)) 
         ==> ct(v(T),p(V,[PROG,PF,TPAST,fut,NEG],A)).
58 dr ct(v(aux),p(can,[e,e,TPAST,e,NEG],_)) + ct(v(T),p(V,[PROG,PF,e,e,e],A)) 
         ==> ct(v(T),p(V,[PROG,PF,TPAST,can,NEG],A)).


57 dr ct(p,d) + ct(v(T),p(V,[F1,F2,e,e,e],A)) 
         ==> ct(v(T),p(V,[F1,F2,inf,e,e],A)).

55 dr ct(v(aux),p(have,[e,e,F3,F4,F5],[_:S|_])) + ct(v(T),p(V,[F1,en,e,e,e],[R:e|A])) 
         ==> ct(v(T),p(V,[F1,pf,F3,F4,F5],[R:S|A])).
54 dr ct(v(aux),p(have,[e,e,F3,F4,F5],[_:S|_])) + ct(v(T),p(V,[F1,e,ed,e,e],[R:e|A])) 
         ==> ct(v(T),p(V,[F1,pf,F3,F4,F5],[R:S|A])).

53 dr ct(v(aux),p(do,[e,e,F2,e,not],[_:S])) + ct(v(v),p(V,[e,e,e,e,e],[R:e|A]))
         ==> ct(v(v),p(V,[e,e,F2,e,not],[R:S|A])).
52 dr ct(v(aux),p(be,[e,F1,F2,F3,F5],[j:S])) + ct(v(T),p(V,[ing,e,e,e,e],[R:e|A]))
         ==> ct(v(T),p(V,[i,F1,F2,F3,F5],[R:S|A])).


51 dr ct(v(aux),p(V,[F1,F2,F3,F4,e],A)) + ct(neg,not) ==> 
      ct(v(aux),p(V,[F1,F2,F3,F4,not],A)).
50 dr ct(v(v),p(do,[e,e,F3,e,e],[_:S|_])) 
               + ct(neg,not)  ==> 
         ct(v(aux),p(do,[e,e,F3,e,not],[_:S])).



% PHRASE LEVEL I: NP

/* NP */

% 120 dr ct(adv(l(tr)),far) + ct(adv(l(i)),L) ==> ct(adv(l(i)),far(L)).



119 dr ct(adv(l(tr)),P) + ct(n(_),N) ==> ct(adv(l(tr)),loc(P,N)). 
118 dr ct(adv(l(tr)),P) + ct(adv(l(i)),L)  ==> ct(adv(l(tr)),loc(P,L)). 

117 dr ct(adv(time(tr)),time(Ext,P,e))
     +   ct(n(n),sp(D,e,e,[],time(time))) 
     ==> ct(adv(time(tr)),time(Ext,P,d(D))). 

116 dr ct(n(n),sp(D,e,e,Attr,time(time))) + ct(adv(time(tr)),time(e,P,e)) ==>
       ct(adv(time(tr)),time(e(Ext),P,e)) // [time_ext(D,Attr,Ext)].

/*
115 dr ct(rel,sp(A,B,C,D,part)) 
           + ct(n(_),sp(A1,B1,C1,D1,H1) )
	 ==> ct(n(n),sp(A,B,C,[rel(gen,sp(A1,B1,C1,D1,H1))|D],part)).


114 dr ct(rel,sp(A,B,C,D,H)) 
           + ct(n(_),sp(A1,B1,C1,D1,H1) )
	 ==> ct(n(n),sp(A1,B,C1,[rel(H,sp(A,B1,C,D,H))|D1],H1)).
*/

113 dr ct(as,sp(A,same,C,D,H)) 
           + ct(n(_),sp(A1,B1,C1,D1,H1) )
	 ==> ct(n(n),sp(A,same(sp(A1,B1,C1,D1,H1),C,D,H))).
/*
112 dr ct(part,NUM1) + ct(n(_),sp(A,B,num(X),D,H) ) ==>
       ct(n(n),sp(e,e,num(NUM1),[],of(sp(A,B,num(X),D,H)))) 
       // [is_plural(num(X))].
*/
/*
111 dr ct(n(n),sp(D,ALT,NUM,ATTR,N)) + ct(p,gen) 
       ==> ct(rel,sp(D,ALT,NUM,ATTR,N)) // [is_relational(N)].
*/

110 dr ct(n(T),sp(A,B,C,[],N)) + ct(adv,like(N1)) ==>
     ct(n(T),sp(A,B,C,[rel(like,N1)],N)) // [indet_sp(A,C,N)].

105 dr ct(comp,like) + ct(n(_),N) ==> ct(adv,like(N)).

100 dr ct(comp,like) + ct(d(_),this) ==> ct(adv,like(this)).

99 dr ct(n(n),sp(this(1),same,NUM,ATTR,N)) + ct(conj,as) ==>
      ct(as,sp(this,same,NUM,ATTR,N)).

98 dr ct(gen(_),N) + ct(n(T),sp(e,e,Num,A,N1)) ==> ct(n(T),sp(e,e,Num,A,of(N1,N))).



97 dr ct(n(T),N) + ct(gen,gen) ==> ct(gen(T),N).


95 dr ct(alt,other(2)) + ct(n(n),sp(e,e,NUM,Attr,N)) ==> ct(n(n),sp(e,other,NUM,Attr,N)) .
94 dr ct(alt,other) + ct(n(n),sp(e,e,NUM,Attr,N)) ==> ct(n(n),sp(e,other,NUM,Attr,N)) .
93 dr ct(alt,more) + ct(n(n),sp(e,e,NUM,Attr,N)) ==> ct(n(n),sp(e,more,NUM,Attr,N)) .

92 dr ct(n(d),sp(e,e,e,Attr,N)) + ct(a,A) ==> ct(n(d),sp(e,e,e,[A|Attr],N)).

91 dr ct(d(e),ref(unkn)) + ct(n(n),sp(e,e,e,ATTR,N)) ==> ct(n(n),sp(e,e,num(one),ATTR,N)).

90 dr ct(d(_),this(1)) + ct(n(n),sp(e,same,num(some),ATTR,N)) ==> ct(n(n),sp(this,same,num(some),ATTR,N)).

89 dr ct(d(_),this(1)) + ct(n(n),sp(e,same,NUM,ATTR,N)) ==> ct(n(n),sp(this,same,NUM,ATTR,N)).

88 dr ct(d(plur),this) + ct(n(n),sp(e,ALT,num(plur),ATTR,N)) ==> ct(n(n),sp(this,ALT,num(some),ATTR,N)).

87 dr ct(d(plur),this) + ct(n(n),sp(e,ALT,NUM,ATTR,N)) ==> ct(n(n),sp(this,ALT,NUM,ATTR,N)) //
   [is_plural(NUM)].

86 dr ct(d(e),this) + ct(n(n),sp(e,ALT,e,ATTR,N)) ==> ct(n(n),sp(this,ALT,e,ATTR,N)).



85 dr ct(num,some) + ct(n(n),sp(e,ALT,e,ATTR,N))
       ==> ct(n(n),sp(some,ALT,e,ATTR,N)).

83 dr ct(alt,same) + ct(n(n),sp(e,e,NUM,Attr,N)) ==> ct(n(n),sp(e,same,NUM,Attr,N)).

81 dr ct(num,one) + ct(n(n),sp(e,ALT,e,A,N)) ==> ct(n(n),sp(e,ALT,num(one),A,N)).
80 dr ct(num,NUM) + ct(n(n),sp(e,ALT,num(plur),A,N)) ==> ct(n(n),sp(e,ALT,num(NUM),A,N)).




71 dr ct(a,A) + ct(n(n),sp(e,e,NUM,Attr,N)) ==> ct(n(n),sp(e,e,NUM,[A|Attr],N)).

70 dr ct(n(d),sp(D,e,e,Attr,N)) + ct(alt,other(1)) ==> ct(n(d),sp(D,other,e,Attr,N)).

% this someone: marginal
41 dr ct(d(_),this) + ct(n(d),sp(e,e,e,[],someone(X))) ==> ct(n(n),sp(this,e,e,[],someone(X))).





% PHRASE LEVEL II : PP

128 dr ct(n(T),sp(A,B,C,D,kind)) + ct(pp(gen),sp(e,e,e,[],E)) 
	  ==> ct(n(T),sp(A,B,C,[rel(kind,e)|D],E)).


127 dr ct(n(T),sp(A,B,C,D,E)) + ct(pp(gen),NP1) 
	  ==> ct(n(T),sp(A,B,C,[rel(gen,NP1)|D],E)).

125 dr ct(adv(time(tr)), time(e, after, e)) + ct(d(_),this) ==>
       ct(adv(time(tr)), time(e, after, d(that))).

124 dr ct(p,P) + ct(d(e),this) ==> 
       ct(pp(R),sp(this,e,e,[],something(fact)))  // [pp_role(P,this,R)].

123 dr ct(p,P) + ct(n(_),N) ==> ct(pp(R),N)  // [pp_role(P,N,R)].

122 dr ct(d(_),any) + ct(alt,more) ==> ct(adv(a),anymore).

121 dr ct(num,all) + ct(n(n),sp(D,Alt,num(plur),Eval,N)) ==>
       ct(n(n),sp(D,Alt,num(all),Eval,N)).





% PHRASE LEVEL III : VP
250 dr ct(v(aux),p(be,F,[j:S])) +  ct(pp(R),sp(A,B,C,D,E)) ==>
	  ct(v(v),p(be,F,[j:S,R:sp(A,B,C,D,E)])).
250 dr ct(v(aux),p(be,F,[j:S])) +  ct(n(_),sp(A,B,C,D,E)) ==>
	  ct(v(v),p(be,F,[j:S,j:sp(A,B,C,D,E)])).

250 dr ct(v(aux),p(be,F,[j:S])) +  ct(a,A) ==>
	  ct(v(stat),p(A,F,[o:S|GOOD_FOR])) //
	  [good_for(A,GOOD_FOR)].

250 dr ct(v(aux),p(be,F,[j:S])) +  ct(adv(l(_)),ADV)==>
	  ct(v(v),p(be,F,[j:S,l:ADV])).
250 dr ct(v(aux),p(be,F,[j:S])) +  ct(adv,like(X))==>
	  ct(v(stat),p(like,F,[j:S,o:X])).
250 dr ct(v(aux),p(have,F,[j:S])) +  ct(n(_),sp(A,B,C,D,E)) ==>
	  ct(v(v),p(have,F,[j:S,o:sp(A,B,C,D,E)])).


235 dr  ct(neg,not) + ct(v(T),p(V,[F1,F2,inf,e,e],A))  ==>
          ct(v(T),p(V,[F1,F2,inf,e,not],A)).


230 dr ct(v(aux),p(V,[F1,F2,F3,F4,e],R)) + ct(neg,not) ==>
       ct(v(aux),p(V,[F1,F2,F3,F4,not],R)). 

228 dr ct(v(T),p(V,F,[S,O,Da,Co,In,X,R:e|A])) + ct(pp(R),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[S,O,Da,Co,In,X,R:sp(AA,B,C,D,E)|A])).

227 dr ct(v(T),p(V,F,[S,O,Da,Co,In,R:e|A])) + ct(pp(R),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[S,O,Da,Co,In,R:sp(AA,B,C,D,E)|A])).

225 dr ct(v(T),p(V,F,[S,O,Da,Co,R:e|A])) + ct(pp(R),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[S,O,Da,Co,R:sp(AA,B,C,D,E)|A])).

220 dr ct(v(T),p(V,F,[S,O,Da,R:e|A])) + ct(pp(R),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[S,O,Da,R:sp(AA,B,C,D,E)|A])).

215 dr ct(v(T),p(V,F,[S,O,R:e|A])) + ct(pp(R),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[S,O,R:sp(AA,B,C,D,E)|A])).

210 dr ct(v(T),p(V,F,[R:S,R1:e|A])) + ct(pp(R1),sp(AA,B,C,D,E)) ==>
	  ct(v(T),p(V,F,[R:S,R1:sp(AA,B,C,D,E)|A])).

207 dr ct(v(T),p(V,F,[R:S,o:e|A])) + ct(d(_),this) ==>
	  ct(v(T),p(V,F,[R:S,o:sp(this,e,e,[],something(fact))|A])).

205 dr ct(v(T),p(V,[F1,F2,F3,F4,NEG],[R:S,o:e|A])) 
     + ct(n(_),sp(AA,B,C,D,E))
     ==> ct(v(T),p(V,[F1,F2,F3,F4,NEG1],[R:S,o:sp(AA1,B,C,D,E)|A])) 
     // [obj_det(NEG,NEG1,AA,AA1)].

205 dr ct(v(T),p(V,F,[R:S,o:e|A])) + ct(alt,more) ==>
	  ct(v(T),p(V,F,[R:S,o:more|A])).



% CLAUSE LEVEL 
 


% SUBJECT
307 dr ct(n(_),sp(Det,Alt,Num,Ev,N1)) + ct(v(v),p(V,[PROG,PF,TPAST,TFUT,NEG],[R:Subj|A])) ==>
        ct(s,s(NEG,Tense,CAN,e,e,p(V1,[R:sp(Det,Alt,Num,Ev,N)|A]),e,e)) //
	[subj_case(N,N1),
	 v_par(V,TPAST,TFUT,PF,PROG,CAN,Tense,Subj,Num,N),
	 v_form(V,PF,PROG,V1)].

% SUBJECT
306 dr ct(n(_),sp(Det,Alt,Num,Ev,N1)) + ct(v(stat),p(V,[e,PF,TPAST,TFUT,NEG],[R:Subj|A])) ==>
        ct(s,s(NEG,Tense,CAN,e,e,p(V1,[R:sp(Det,Alt,Num,Ev,N)|A]),e,e)) //
	[subj_case(N,N1),
	 v_par(be,TPAST,TFUT,PF,e,CAN,Tense,Subj,Num,N),
	 v_form(V,PF,e,V1)].
306 dr ct(d(_),this) + ct(v(stat),p(V,[e,PF,TPAST,TFUT,NEG],[R:Subj|A])) ==>
        ct(s,s(NEG,Tense,CAN,e,e,p(V1,[R:sp(this,e,e,e,something(fact))|A]),e,e)) //
	[v_par(be,TPAST,TFUT,PF,e,CAN,Tense,Subj,e,something(fact)),
	 v_form(V,PF,e,V1)].

% EXISTENCE (THERE IS, EXIST rientra nel normale sogg-v dep).
308 dr      ct(adv(l(i)),there )
	  + ct(v(v),p(be,[PROG,PF,TPAST,TFUT,NEG],[j:Subj,j:sp(D,Alt,Num,Ev,N)])) 
	==> ct(s,s(NEG1,Tense,CAN,e,e,p(V1,[j:sp(D1,Alt,Num,Ev,N)]),e,e)) 
	 // 	[
		 indet_sp(D,Num,N),
		 obj_det(NEG,NEG1,D,D1),		 
		 v_par(be,TPAST,TFUT,PF,PROG,CAN,Tense,Subj,Num,N),
	         v_form(exist,PF,PROG,V1)
		].

% ADVERBIALS

/* cf. 321
% TIMES
310 dr ct(s,s(NEG,Tense,CAN,e,DUR,p(V1,A),LOC,MAN)) +
       ct(n(_),sp(AA,BB,CC,DD,times(X))) ==> 
       ct(s,s(NEG,Tense,CAN,sp(AA,BB,CC,DD,times(X)),DUR,p(V1,A),LOC,MAN)).
*/

% DURATION
310 dr ct(s,s(NEG,Tense,CAN,TIMES,e,p(V1,A),LOC,MAN)) +
       ct(pp(b),sp(e,e,num(one),[Attr],time(time))) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,dur(Attr),p(V1,A),LOC,MAN)).
310 dr ct(s,s(NEG,Tense,CAN,TIMES,e,p(V1,A),LOC,MAN)) +
       ct(pp(b),sp(some,e,e,e,time(time))) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,dur(some),p(V1,A),LOC,MAN)).

% rifarla - va in ciclo infinito
% LOC
310 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),e,MAN)) +
       ct(adv(l(_)),ADV) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),loc(ADV),MAN)).

310 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),e,MAN)) +
       ct(pp(LOC),sp(AA,BB,CC,DD,somewhere(X))) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),loc(LOC,sp(AA,BB,CC,DD,somewhere(X))),MAN)).



% MANNER
310 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),LOC,e)) +
       ct(pp(l),sp(AA,BB,CC,DD,manner(X))) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),LOC,sp(AA,BB,CC,DD,manner(X)))).

310 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),LOC,e)) +
       ct(adv,like(this)) ==> 
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V1,A),LOC,manner(this))).

% anymore
315 dr  ct(s,s(not,Tense,CAN,e,DUR,p(V1,A),LOC,MAN)) +
        ct(adv(a),anymore) ==>
       ct(s,s(neg,Tense,CAN,e,DUR,p(more(V1),A),LOC,MAN)).
/* cfr. 310 duration
320 dr  ct(s,s(NEG,Tense,CAN,e,e,p(V1,A),LOC,MAN)) + ct(pp(b),PP)
        ==>
       ct(s,s(NEG,Tense,CAN,e,dur(DUR),p(V1,A),LOC,MAN)) //
       [duration_mod(PP,DUR)].
*/

% TIMES
321 dr  ct(s,s(NEG,Tense,CAN,e,e,p(V1,A),LOC,MAN))
      - ct(n(_),sp(e,e,num(NUM),[],time(time))) 
        ==>
        ct(s,s(NEG,Tense,CAN,freq(NUM),e,p(V1,A),LOC,MAN)).

322 dr    ct(s,s(NEG,Tense,CAN,e,e,p(V1,A),LOC,MAN)) 
       -  ct(n(_),sp(e,e,num(one),[],time(time))) 
       ==>
        ct(s,s(NEG,Tense,CAN,freq(one),e,p(V1,A),LOC,MAN)).

% TIME
323 dr ct(adv(time(_)),time(Ext,P,D)) 
     - ct(s,s(NEG,Tense,CAN,e,DUR,p(V1,A),LOC,MAN))
    ==>
       ct(s,s(NEG,time(emph,Ext,P1,D1),CAN,e,DUR,p(V1,A),LOC,MAN)) //
       [time_agr(Ext,P,D,time(Ext,P1,D1),Tense)].
324 dr ct(pp(time),sp(D,Alt,e,[],time(time))) 
     - ct(s,s(NEG,Tense,CAN,e,DUR,p(V1,A),LOC,MAN))
        ==>
       ct(s,s(NEG,time(emph,Ext,P1,D1),CAN,e,DUR,p(V1,A),LOC,MAN)) //
       [time_loc(D,Alt,D1),
	time_agr(e,at,D1,time(Ext,P1,D1),Tense)].








% SENTENCE LEVEL I : SIMPLE SENTENCE

410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,[e:S]),LOC,MAN)) +
       ct(d(_),that) + 
       ct(s,s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,
	       [e:S,
	        prop:s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)]),
		LOC,MAN)).
410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,[e:S]),LOC,MAN)) +
       ct(s,s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,
	       [e:S,
	        prop:s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)]),
		LOC,MAN)).
410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,[e:S]),LOC,MAN)) +
       ct(n(_),SP) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,
	       [e:S,
	        o:SP]),
		LOC,MAN)).
410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,[e:S]),LOC,MAN)) +
       ct(d(e),this) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,
	       [e:S,
	        o:sp(this,e,e,[],something(fact))]),
		LOC,MAN)).
410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,[e:S,o:O]),LOC,MAN)) +
       ct(pp(t),TOP) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(know,
	       [e:S,
	        o:O,
	        t:TOP]),
		LOC,MAN)).



410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,[e:S]),LOC,MAN)) +
       ct(d(_),that) + 
       ct(s,s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,
	       [e:S,
	        prop:s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)]),
		LOC,MAN)).
410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,[e:S]),LOC,MAN)) +
       ct(s,s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,
	       [e:S,
	        prop:s(NEG1,Tense1,CAN1,TIMES1,DUR1,P1,LOC1,MAN1)]),
		LOC,MAN)).
410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,[e:S,o:O]),LOC,MAN)) +
       ct(pp(t),SP) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,[e:S,o:O,t:SP]),
		LOC,MAN)).
410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,[e:S]),LOC,MAN)) +
       ct(n(_),SP) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,[e:S,o:SP]),
		LOC,MAN)).
410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,[e:S]),LOC,MAN)) +
       ct(pp(t),SP) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(think,[e:S,o:sp(e,e,e,[],something(thing)),t:SP]),
		LOC,MAN)).

410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S,o:e]),LOC,MAN)) +
       ct(v(_T),p(V,[PROG,PF,inf,e,NEG1],[R:e|A])) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S,o:[p(V2,[R:e|A])]]),LOC,MAN))
       // [ v_form(V,PF,PROG,V1),
	    pred_neg(NEG1,V1,V2) 
	 ].

410 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S,o:sp(AA,BB,CC,DD,EE)]),LOC,MAN)) +
       ct(v(_T),p(V,[PROG,PF,inf,e,NEG1],[R:e|A])) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(want,[e:S,o:[p(V2,[R:sp(AA,BB,CC,DD,EE)|A])]]),LOC,MAN))
      // [v_form(V,PF,PROG,V1),
	  pred_neg(NEG1,V1,V2)].


414 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V,[e:S]),LOC,MAN)) +
       ct(n(_),NP) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V,[e:S,o:NP]),LOC,MAN)) //
       [prop_compl(V)].
415 dr ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V,[e:S,o:O]),LOC,MAN)) +
       ct(pp(t),NP) ==>
       ct(s,s(NEG,Tense,CAN,TIMES,DUR,p(V,[e:S,o:O,t:NP]),LOC,MAN)) //
       [prop_compl(V)].

       
415 dr ct(neg,not) + ct(pp(cause),K) ==> ct(pp(cause),not(K)).

416 dr ct(adv(sent),maybe) + ct(s,S) ==> ct(s,maybe(S)).

416 dr ct(adv,only) + ct(pp(cause),sp(Det,AA,BB,CC,DD)) ==>
       ct(pp(cause),sp(only(Det),AA,BB,CC,DD)). 
417 dr  ct(s,S) - ct(pp(cause),N) ==> ct(s,because(S,N)).

419  dr ct(conj,CONJ) + ct(s,S) ==>
        ct(clause(CONJ),S).

420 dr ct(s,S2) + ct(clause(because),S1)  ==>  ct(s,because(S1,S2)).


420 dr ct(s,s(NEG,Tense,CAN,T,D,p(EVAL,[o:sp(this, e, e, [], something(fact))|A]),e,e)) +
       ct(v(_),p(V,[e,e,inf,e,e],ARG))
        ==>
       ct(s,s(NEG,Tense,CAN,T,D,p(EVAL,[o:p(V,ARG)|A]))) // [eval(EVAL)].


480 dr ct(s,s(NEG,Tense,CAN,T,D,p(EVAL,[o:sp(this, e, e, [], something(fact))|A]),e,e)) +
       ct(clause(if),S1) ==>
       ct(s,s(NEG,Tense,CAN,T,D,p(EVAL,[o:S1|A]))) // [eval(EVAL)].



% SENTENCE LEVEL II: COMPOUND SENTENCES

570 dr ct(clause(if),S1) + ct(s,S2) ==>  ct(s,if(S1,S2)).
571 dr ct(clause(when),S1) + ct(s,S2) ==>  ct(s,when(S1,S2)).
572 dr ct(s,S2) + ct(clause(as),S1) ==>  ct(s,like(S1,S2)).



% MORPHOLOGY
 
p ::: adv_from(far). 
p ::: adv_from(very(far)). 
p ::: adv_from(away). 

p ::: is_plural(num(plur)).
p ::: is_plural(num(two)).
p ::: is_plural(num(some)).
p ::: is_plural(num(many)).
p ::: is_plural(num(all)).
% p : is_plural(plur(_)).

p ::: det_num(e,e).
p ::: det_num(e,num(one)).
% p : det_num(plur,plur).
p ::: det_num(num(plur),num(plur)).
p ::: det_num(num(plur),num(_)).

p ::: neg_existence(e,no,not).
p ::: neg_existence(Neg,_,Neg).
p ::: neg_existence(e,_,e).


% indet_sp(D,Num,N),		 
p ::: indet_sp(ref(unkn),_Num,_H).
p ::: indet_sp(any,_Num,_H).
p ::: indet_sp(no,_Num,_H).
p ::: indet_sp(e,num(_),_H).
p ::: indet_sp(e,e,someone(person)).
p ::: indet_sp(e,e,something(thing)).
% p : indet_sp(e,num(all),someone(person)).
% p : indet_sp(e,num(all),something(thing)).


p ::: subj_case(d(me),d(me(subj))).
p ::: subj_case(X,X).


% Vanno messi in quest'ordine per via di GEN, che costruisce i primi 
% dall'ultimo (se i(_), pf(_), pf(i(_))  matcherebbero con V).
p ::: v_form(V,pf,i,pf(i(V))).
p ::: v_form(V,pf,e,pf(V)).
p ::: v_form(V,e,i,i(V)).
p ::: v_form(V,e,e,V).

%  v_par(V,TPAST,TFUT,PF,PROG,CAN,Tense,Subj,Num,N).
%  BE - pres
p ::: v_par(be,s,e,e,e,e,e,am,e,d(me)).
p ::: v_par(be,s,e,e,e,e,e,are,e,d(you)).
p ::: v_par(be,s,e,e,e,e,e,is,e,_).
p ::: v_par(be,s,e,e,e,e,e,is,num(one),_).
p ::: v_par(be,s,e,e,e,e,e,are,num(_),_).

% BE - past
p ::: v_par(be,ed,e,e,e,e,time(e,before,e),was,e,d(me)).
p ::: v_par(be,ed,e,e,e,e,time(e,before,e),were,e,d(you)).
p ::: v_par(be,ed,e,e,e,e,time(e,before,e),was,e,_).
p ::: v_par(be,ed,e,e,e,e,time(e,before,e),was,num(one),_).
p ::: v_par(be,ed,e,e,e,e,time(e,before,e),were,num(_),_).


%  BE LOC - pres
p ::: v_par(l(_),s,e,e,e,e,e,am,e,d(me)).
p ::: v_par(l(_),s,e,e,e,e,e,are,e,d(you)).
p ::: v_par(l(_),s,e,e,e,e,e,is,e,_).
p ::: v_par(l(_),s,e,e,e,e,e,is,num(one),_).
p ::: v_par(l(_),s,e,e,e,e,e,are,num(_),_).

% BE LOC - past
p ::: v_par(l(_),ed,e,e,e,e,time(e,before,e),was,e,d(me)).
p ::: v_par(l(_),ed,e,e,e,e,time(e,before,e),were,e,d(you)).
p ::: v_par(l(_),ed,e,e,e,e,time(e,before,e),was,e,_).
p ::: v_par(l(_),ed,e,e,e,e,time(e,before,e),was,num(one),_).
p ::: v_par(l(_),ed,e,e,e,e,time(e,before,e),were,num(_),_).

% - BE V-ing, pres
p ::: v_par(_,s,e,e,i,e,e,am,e,d(me)).
p ::: v_par(_,s,e,e,i,e,e,are,e,d(you)).
p ::: v_par(_,s,e,e,i,e,e,is,e,_).
p ::: v_par(_,s,e,e,i,e,e,is,num(one),_).
p ::: v_par(_,s,e,e,i,e,e,are,num(_),_).

% BE V-ing, past
p ::: v_par(_,ed,e,e,i,e,time(e,before,e),was,e,d(me)).
p ::: v_par(_,ed,e,e,i,e,time(e,before,e),were,e,d(you)).
p ::: v_par(_,ed,e,e,i,e,time(e,before,e),was,e,_).
p ::: v_par(_,ed,e,e,i,e,time(e,before,e),was,num(one),_).
p ::: v_par(_,ed,e,e,i,e,time(e,before,e),were,num(_),_).

% FUT, CAN
p ::: v_par(_,e,fut,_,_,e,time(e,after,e),e,_,_).
p ::: v_par(_,ed,fut,_,_,e,time(e,before,anaf),e,_,_).
p ::: v_par(_,e,can,_,_,can,e,e,_,_).
p ::: v_par(_,ed,can,_,_,can,time(e,before,e),e,_,_).

% HAVE V-EN. Vince su PROG (you have been, inv. di you are)
p ::: v_par(_,s,e,pf,_,e,e,e,e,something(_)).
p ::: v_par(_,s,e,pf,_,e,e,e,e,someone(_)).
p ::: v_par(_,s,e,pf,_,e,e,e,num(one),something(_)).
p ::: v_par(_,s,e,pf,_,e,e,e,num(one),someone(_)).
p ::: v_par(_,e,e,pf,_,e,e,e,_,_).
p ::: v_par(_,ed,e,pf,_,e,time(e,before,anaf),e,_,_).

% V-pres. Non ci dev'essere PROG, se no vale BE
p ::: v_par(_,s,e,e,e,e,e,e,e,something(_)).
p ::: v_par(_,s,e,e,e,e,e,e,e,someone(_)).
p ::: v_par(_,s,e,e,e,e,e,e,num(one),something(_)).
p ::: v_par(_,s,e,e,e,e,e,e,num(one),someone(_)).
p ::: v_par(_,e,e,e,e,e,e,e,_,d(me)).
p ::: v_par(_,e,e,e,e,e,e,e,_,d(you)).
p ::: v_par(_,e,e,e,e,e,e,e,num(_),_).

% V-past
p ::: v_par(_,ed,e,e,e,e,time(e,before,e),e,_,_).

p ::: duration_mod(sp(ref(unkn),e,sing(e),[LONG],time(time)),LONG).
p ::: duration_mod(sp(some,e,sing(e),[],time(time)),some).

p ::: time_ext(some,_,some).
p ::: time_ext(ref(unkn),[Ext],Ext).

p ::: time_loc(this,_,this).
p ::: time_loc(that,_,that).
p ::: time_loc(some,_,some).
p ::: time_loc(_,same,same).
p ::: time_loc(e,e,e).

p ::: time_agr(Ext,before,D,time(Ext,before,D),time(e,before,e)).
p ::: time_agr(Ext,after,D,time(Ext,after,D),time(e,after,e)).
p ::: time_agr(e,at,D,time(e,at(after),D),time(e,after,e)).
p ::: time_agr(e,at,D,time(e,at(before),D),time(e,before,e)).
p ::: time_agr(Ext,before,that,time(Ext,before,d(that)),time(e,before,anaf)).
p ::: time_agr(Ext,after,that,time(Ext,after,d(that)),time(e,after,anaf)).
p ::: time_agr(Ext,before,that,time(Ext,after,d(that)),time(e,after,e)).
p ::: time_agr(Ext,after,d(that),time(Ext,after,d(that)),time(e,before,e)).
p ::: time_agr(e,now,e,time(e,now,e),e).


p ::: eval_for(good,N,good(N)).
p ::: eval_for(bad,N,bad(N)).
p ::: eval_for(very(good),N,very(good(N))).
p ::: eval_for(very(bad),N,very(bad(N))).

p ::: inst_arg(R,N,[R:e|Rest],[R:N|Rest]).
p ::: inst_arg(R,N,[A1,R:e|Rest],[A1,R:N|Rest]).
p ::: inst_arg(R,N,[A1,A2,R:e|Rest],[A1,A2,R:N|Rest]).
p ::: inst_arg(R,N,[A1,A2,A3,R:e],[A1,A2,A3,R:N]).

p ::: inst_adj(yes,_,_,e,e).
p ::: inst_adj(no,l,N,N,e).
p ::: inst_adj(no,m,N,e,N).

p ::: is_relational(kind).
p ::: is_relational(part).
p ::: is_relational(side).

p ::: pp_role(c,sp(_,_,_,_,something(_)),i).
p ::: pp_role(R,_,R).

p ::: pred_neg(not,V,not(V)).
p ::: pred_neg(e,V,V).

p ::: prop_obj(know).
p ::: prop_obj(think).

p ::: good_for(good,[b:e]).
p ::: good_for(bad,[b:e]).
p ::: good_for(_,[]).


p ::: eval(good).
p ::: eval(bad).
p ::: eval(very(good)).
p ::: eval(very(bad)).

p ::: obj_det(not,not,any,e).
p ::: obj_det(e,not,no,e).
p ::: obj_det(e,e,D,D).
p ::: obj_det(not,not,D,D).



/* LEXICON : MACROS */

ct(adv,forever) ::: ct(pp(b),sp(ref(unkn),e,e,[all],time(1))).

ct(n(d),it) ::: sp(this,e,e,[],something(fact)).
ct(n(d),he) ::: sp(this,e,e,[],someone(person)).

ct(n(n),people) ::: sp(e,e,num(plur),[],people).
ct(n(n),one_time) ::: sp(e,e,num(one),[],time(time)).
ct(n(n),two_times) ::: sp(e,e,num(two),[],time(time)).
ct(n(n),many_times) ::: sp(e,e,num(many),[],time(time)).
ct(n(n),all_times) ::: sp(e,e,num(all),[],time(time)).

ct(n(d),everything) ::: sp(e, e, num(all), [], something(thing)).
ct(n(d),everyone) ::: sp(e, e, num(all), [], someone(person)).
ct(n(d),everywhere) ::: sp(e, e, num(all), [], somewhere(place)).

ct(n(d),any(N)) ::: sp(any,e,e,[],N).
ct(n(d),no(N)) ::: sp(no,e,e,[],N).

ct(n(_),N) ::: sp(e,e,e,[],N).
% ct(n(_),N) ::: sp(e,e,e,[],N).




ct(v(v),happen) ::: p(happen,[e,e,e,e,e],[o:e,d:e]).
ct(v(v),do) ::: p(do,[e,e,e,e,e],[a:e,o:e,d:e,c:e,i:e,b:e]).
ct(v(v),move) ::: p(move,[e,e,e,e,e],[a:e]).

ct(v(v),die) ::: p(die,[e,e,e,e,e],[o:e]).
ct(v(v),live) ::: p(live,[e,e,e,e,e],[a:e,c:e]).

ct(v(v),feel) ::: p(feel,[e,e,e,e,e],[e:e,o:e,g:e]).
ct(v(v),know) ::: p(know,[e,e,e,e,e],[e:e]).
ct(v(v),think):::  p(think,[e,e,e,e,e],[e:e]).
ct(v(v),say) ::: p(say,[e,e,e,e,e],[a:e,o:e,d:e,t:e]).
ct(v(v),hear):::  p(hear,[e,e,e,e,e],[a:e,o:e,t:e]).
ct(v(v),see) ::: p(see,[e,e,e,e,e],[a:e,o:e]).
ct(v(v),want):::  p(want,[e,e,e,e,e],[e:e,o:e]).
ct(v(v),touch):::  p(touch,[e,e,e,e,e],[a:e,o:e]).

ct(v(v),exist) ::: p(exist,[e,e,e,e,e],[j:e]).

ct(v(aux),am) ::: p(be,[e,e,s,e,e],[j:am]).
ct(v(aux),is) ::: p(be,[e,e,s,e,e],[j:is]).
ct(v(aux),are):::  p(be,[e,e,s,e,e],[j:are]).
ct(v(aux),was):::  p(be,[e,e,ed,e,e],[j:was]).
ct(v(aux),were):::  p(be,[e,e,ed,e,e],[j:were]).
ct(v(aux),V) ::: p(V,[e,e,e,e,e],[j:e]).

ct(adv(time(_)),A) ::: time(e,A,e).


/* ALLOMORPH CONSTRUCTION RULES */


ph ::: [ "sai"-"s#" => "says#", []-[], cond : []].
ph ::: [ "sai"-"es#" => "says#", []-[], cond : []].
ph("es") ::: [ "s" => "es", [C]-[], cond : [C << e_triggers]].
ph("-y") ::: [ X+"i" => X+"y", []-"#", cond : []].
ph ::: [ "'s" => "'", "s"-"#", cond : []].

ph ::: [ X+"e" => X, []-"ing#", cond : []].
ph ::: [ "have" => "ha", []-"d#", l:[]-"ed", cond : []].
ph ::: [ "have" => "ha", []-"s#", l:[]-"s", cond : []].
ph ::: [ "do" => "di", []-"d#", l:[]-"ed", cond : []].
ph ::: [ "will" => "woul", []-"d#", l:[]-"ed", cond : []].
ph ::: [ "can" => "coul", []-"d#", l:[]-"ed", cond : []].
ph("felt") ::: [ "feel" => "fel", []-"t#", l:[]-"ed", cond : []].
ph ::: [ "think" => "though", []-"t#", l:[]-"ed", cond : []].

ph ::: [ "see"-"ed#" => "saw#", []-[], cond : []].
ph("knew") ::: [ "know"-"ed#" => "knew#", []-[], cond : []].

ph ::: [ "ed" => "d", [V]-"#", cond : [V << vow]].
ph ::: [ "ed" => "d", "oul"-"#", cond : []].
ph ::: [ "ed" => "d", "hear"-"#", cond : []].
ph ::: [ "ed" => "t", "fel"-"#", l:"feel"-[], cond : []].
ph ::: [ "ed" => "t", "though"-"#", l:"think"-[], cond : []].

ph ::: [ "en" => "ne", "do"-"#", cond : []].


/* LEXICON */

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


m(n(p),"I",d(me(subj))).
m(n(p),"me",d(me)).
m(n(p),"you",d(you)).

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

m(d(e),"this",this).
m(d(plur),"these",this).
m(d(_),"the",this(1)).
m(d(e),"that",that).
m(d(e),"a",ref(unkn)).
m(d(e),"an",ref(unkn)).
m(d(_),"any",any).
m(d(_),"no",no).
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
m(adv,"forever",forever).

m(neg,"not",not).
m(conj,"because",because).
m(conj,"if",if).
m(conj,"when",when).
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

m(n(d),"he",he).
m(adv,"only",only).


/*
m(group,"",begin).
m(group,"",end).
*/
