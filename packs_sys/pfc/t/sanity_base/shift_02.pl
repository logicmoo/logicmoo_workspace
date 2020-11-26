
:- include(test_header).

w(W):-writeln(W).

p :- reset(q,Cont,Term),
   writeln(Term),
   call_continuation(Cont).

q :- catch(r,Ball,writeln(Ball)).

r :- shift(rterm), throw(rball).

% ?- p.

scce2(Setup,Goal,Undo) :- 
   reset(d(Setup,Goal,Undo),Cont,UndoTerm),
   call(UndoTerm),
   call_continuation(Cont).

d(Setup,Goal,Undo) :- Setup,Goal,shift(Undo).


head(h, a, b).
head(g, b, b).
head(h, x, y).



:- dynamic(scce0/0).

% hide from optimizations (well as anything)
call_w_detr(Goal,Det):- call((Goal,deterministic(Det),true)).


scce1(Setup,Goal,Undo):-
      once(Setup),
      (call_w_detr(Goal,Det)
        *-> (Det == true -> once(Undo) ; (once(Undo);(once(Setup),fail)))
        ; (once(Undo),fail)).

scce22(Setup,Goal,Undo):-
   reset(scce2r(Undo,Setup),Cont,Term),
   call(Term),
   Goal,
   call_continuation(Cont).

% scce2r(Setup,Goal,Undo):- once(Setup), (Goal *-> shift(once(Undo)) ; (!,once(Undo),fail)).
scce2r(Goal,Undo):- Goal ,shift(once(Undo)).


do_call(Goal,Done,true):- Goal, deterministic(Done).
do_call(_,   true,fail).

skip_call(_Goal,_Done,_Next):- fail.

scce4(Setup,Goal,Undo):- 
   prolog_current_choice(ExitCP),
   DoUndo = once(true),
   DoSetup = once((Setup,nb_setarg(1,DoUndo,Undo))),
   repeat, 
   prolog_current_choice(_RepCP),
   ((DoSetup;(true,fail)),
      (do_call(Goal,Done,Next)*->true;(prolog_cut_to(ExitCP),fail)),
      (Done==true->prolog_cut_to(ExitCP);true),
      DoUndo,
      Next),
   fail.
   

scce3(S,Goal,C):- scce2(
   (asserta(scce0,_REF)),
   (nop(between(1,3,_X)),S,Goal,C),
   true),fail.

a :- reset(newpred(Term),Cont,Term), w(after_reset), call_continuation(Cont).
b :-
   shift(shifted),
   w(after_shift).

newpred(Term) :- b, w(inside_reset(Term)).


y11:- scce1(writeln(start),(between(1,3,X),between(1,X,Y),writeln(X-Y)), writeln(end)),fail.  % Works
y12:- scce1(asserta(scce0,REF),(between(1,3,X),between(1,X,Y),writeln(X-Y)),writeln(REF)),fail.  % Broken
y21:- scce2(writeln(start),(between(1,3,X),between(1,X,Y),writeln(X-Y)), writeln(end)),fail.  % Works
y22:- scce4((asserta(scce0,REF),writeln(start(REF))),(between(1,3,X),between(1,X,Y),writeln(X-Y)),writeln(end(REF))),fail.  % Broken

y23:- scce2((asserta(scce0,REF),writeln(start(REF,X))),(between(1,3,X),writeln(mid(REF,X))),writeln(end(REF,X))),fail.  % Broken

y24:- scce2((writeln(bmid(REF,X)),asserta(scce0,REF),writeln(mid(REF,X))),(between(1,3,X),writeln(start(REF,X))),writeln(end(REF,X))),fail.  % Broken

y32:- scce3((repeat,asserta(scce0,REF),between(1,3,X),writeln(start(REF,X))),writeln(mid(REF,X)),writeln(end(REF,X))),fail.  % Broken

/*

?- y11.
start
1-1
end
start
2-1
end
start
2-2
end
start
3-1
end
start
3-2
end
start
3-3
end
false.

?- y12.
1
<clause>(0x21a0970)
ERROR: Uninstantiated argument expected, found <clause>(0x21a0970) (2-nd argument)
ERROR: In:
ERROR:   [14] asserta(scce0,<clause>(0x21a0970))

what i wanted was the output of:

?- repeat, asserta(scce0,REF),call((between(1,3,X),between(1,X,Y),writeln(X-Y))),  writeln(REF),  X-Y == 3-3.

*/

