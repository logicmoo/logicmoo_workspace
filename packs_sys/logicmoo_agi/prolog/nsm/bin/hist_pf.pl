/*
    This file is part of NSM-DALIA, an extensible parser and generator
    for NSM grammars.
       
    Copyright (C) 2009 Francesco Zamblera.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/


:- include('dynamic.pl').

/*
do_pf(Lang,S1,S2) :-
	findall(pf(Num,A,B,C), historic_pf(Lang,Num,A,B,C),Rules),
	do_pf_rules(Lang,Rules,S1,S2).
*/

/*
do_pf_rules(_Lang,[],_From,PF,PF,NewMorphemes,NewMorphemes).
do_pf_rules(Lang,Rules,From,PF1,PF,NewMorphemes1,NewMorphemes) :-
	find_first_rule(Rules,From,NewRules),
	do_pf_rules(Lang,NewRules,PF1,PF,NewMorphemes1,NewMorphemes).

do_pf_rules(Lang,[Rule|Rules],PF1,PF,NewMorphemes1,NewMorphemes) :-
	do_pf_rule(Lang,Rule,PF1,PF2,NewMorphemes1,NewMorphemes2),
	do_pf_rules(Lang,Rules,PF2,PF,NewMorphemes2,NewMorphemes).
do_pf_rules(_Lang,[],PF,PF,NM,NM).


do_pf_rule(Lang,pf(nowf,Num,m(Cat1,A,LF1),m(Cat2,B,LF2),Cond),m(Cat1,PF,LF1),m(Cat2,NewPF,LF2),NewMorphemes1,NewMorphemes) :-
	sublist(A,PF,Before,After), % IN: UTILS.PL
	check_cond_list(Lang,Cond), %IN: CHECKERS.PL 
	!,
	append(Before,B,L1),
	append(L1,After,NewPF1),
	do_pf_rule(Lang,pf(nowf,Num,m(Cat1,A,LF1),m(Cat2,B,LF2),Cond),m(Cat1,NewPF1,LF1),m(Cat2,NewPF,LF2),NewMorphemes1,NewMorphemes).

do_pf_rule(Lang,pf(wf,Num,m(Cat1,A,LF1),m(Cat2,B,LF2),Cond),m(Cat1,PF,LF1),m(Cat1,PF,LF1),NewMorphemes1,NewMorphemes) :-
	sublist(A,PF,Before,After), % IN: UTILS.PL
	check_cond_list(Lang,Cond), %IN: CHECKERS.PL 
	!,
	append(Before,B,L1),
	append(L1,After,NewPF),
	Num1 is Num + 1,
	append(NewMorphemes1,[Num1:m(Cat2,NewPF,LF2)],NewMorphemes).

do_pf_rule(_,_,PF,PF,NM,NM).

*/


do_pf(_Lang,[],_From,shape(PF,PF),[]).
do_pf(Lang,Rules,From,shape(PF1,PF),WF) :-
	find_first_rule(Rules,From,NewRules),
	do_pf_rules(Lang,NewRules,PF1,PF,WF).

do_pf_rules(Lang,[pf(nowf,Num,M1,M2,C)|Rules],PF1,PF,WF) :-
	do_pf_rule(Lang,pf(nowf,Num,M1,M2,C),PF1,PF2),
	do_pf_rules(Lang,Rules,PF2,PF,WF).
do_pf_rules(Lang,[pf(wf,Num,M1,M2,C)|Rules],PF1,PF,[Num1:PF2|WF]) :-
	do_wf_rule(Lang,pf(wf,Num,M1,M2,C),PF1,PF2),
	Num1 is Num + 1,
	do_pf_rules(Lang,Rules,PF2,PF,WF).	
do_pf_rules(_Lang,[],PF,PF,[]).


do_pf_rule(Lang,pf(nowf,Num,m(Cat1,A,LF1),m(Cat2,B,LF2),Cond),m(Cat1,PF,LF1),m(Cat2,NewPF,LF2)) :-
	sublist(A,PF,Before,After), % IN: UTILS.PL
	check_cond_list(Lang,Cond), %IN: CHECKERS.PL 
	!,
	append(Before,B,L1),
	append(L1,After,NewPF1),
	do_pf_rule(Lang,pf(nowf,Num,m(Cat1,A,LF1),m(Cat2,B,LF2),Cond),m(Cat1,NewPF1,LF1),m(Cat2,NewPF,LF2)).
do_pf_rule(_,_,PF,PF).


do_wf_rule(Lang,pf(wf,_Num,m(Cat1,A,LF1),m(Cat2,B,LF2),Cond),m(Cat1,PF,LF1),m(Cat2,PF2,LF2)) :-
	sublist(A,PF,Before,After), % IN: UTILS.PL
	check_cond_list(Lang,Cond), %IN: CHECKERS.PL 
	!,
	append(Before,B,L1),
	append(L1,After,PF2).
do_wf_rule(_,_,PF,PF).	
	


/*
do_pf_rules(_Lang,[],_From,M,M).
do_pf_rules(Lang,Rules,From,M1,M) :-
	find_first_rule(Rules,From,NewRules),
	do_pf_rules(Lang,NewRules,M1,M).

do_pf_rules(Lang,[Rule|Rules],M1,M) :-
	do_pf_rule(Lang,Rule,M1,M2),
	do_pf_rules(Lang,Rules,M2,M).
do_pf_rules(_Lang,[],M,M).

do_pf_rule(Lang,pf(Num,m(_Cat1,PF1,_LF1),m(_Cat2,PF2,_LF2),Cond),m(Cat1,PF,LF1),m(Cat1,NewPF,LF1)) :-
	sublist(PF1,PF,Before,After), % IN: UTILS.PL
	check_cond_list(Lang,Cond), %IN: CHECKERS.PL 
	!,
	append(Before,PF2,L1),
	append(L1,After,NewPF1),
	do_pf_rule(Lang,pf(Num,m(_Cat1,PF1,_LF1),m(_Cat2,PF2,_LF2),Cond),m(_Cat2,NewPF1,_LF2),m(Cat1,NewPF,LF1)).
do_pf_rule(_,_,M,M).
*/


find_first_rule([],_,[]).
find_first_rule([pf(WF,Num,A,B,C)|Rules],From,[pf(WF,Num,A,B,C)|Rules]) :-
	Num >= From,
	!.
find_first_rule([pf(_,Num,_,_,_)|Rules],From,R1) :-
	Num < From,
	find_first_rule(Rules,From,R1).
	


/*
cleanSpaces([' '|S],S1) :-
	!,
	cleanSpaces2(S,S1).
cleanSpaces(S,S1) :- 
	cleanSpaces2(S,S1).


cleanSpaces2([' ',' '|S],[' '|S1]) :-
	!,
	cleanSpaces2(S,S1).
cleanSpaces2([' '],[]) :- !.
cleanSpaces2([],[]).
cleanSpaces2([X|S],[X|S1]) :-
	cleanSpaces2(S,S1).
*/



