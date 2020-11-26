:- dynamic(fdet/1).
:- dynamic(society_goal/0).

:- dynamic(begin/4).

:- dynamic(end/4).

:- dynamic(maxmove/1).

:- dynamic(ban/3).

:- dynamic(force/3).

:- dynamic(move_rules/3).

:- dynamic(left/4).

:- dynamic(prereq/2).

:- dynamic(cell/4).

society_goal.

begin(1,1,_,_).

end(_,_,_,home).

maxmove(5.0).

ban([_,_,_,paper],[_,_,_,treasure],2.0).

force([_,_,COL,money],[_,_,COL,treasure],3.0).

move_rules([_,_,_,paper],left,1.0).

left(X1,Y1,X2,Y2) :- 
    X2#<X1.

prereq([_,_,_,troll],[_,_,_,arm]).

cell(0,0,yellow,troll).

cell(0,1,default,empty).

cell(0,2,default,empty).

cell(0,3,default,empty).

cell(0,4,default,empty).

cell(0,5,default,empty).

cell(0,6,blue,troll).

cell(0,7,default,empty).

cell(1,0,default,empty).

cell(1,1,default,empty).

cell(1,2,default,empty).

cell(1,3,red,empty).

cell(1,4,default,empty).

cell(1,5,default,empty).

cell(1,6,default,empty).

cell(1,7,default,empty).

cell(2,0,default,empty).

cell(2,1,default,empty).

cell(2,2,red,treasure).

cell(2,3,default,empty).

cell(2,4,default,empty).

cell(2,5,default,empty).

cell(2,6,default,empty).

cell(2,7,default,empty).

cell(3,0,red,paper).

cell(3,1,default,empty).

cell(3,2,default,empty).

cell(3,3,default,empty).

cell(3,4,default,empty).

cell(3,5,default,empty).

cell(3,6,yellow,money).

cell(3,7,default,empty).

cell(4,0,default,empty).

cell(4,1,yellow,arm).

cell(4,2,default,empty).

cell(4,3,default,empty).

cell(4,4,default,empty).

cell(4,5,yellow,troll).

cell(4,6,default,empty).

cell(4,7,default,empty).

cell(5,0,default,empty).

cell(5,1,default,empty).

cell(5,2,default,empty).

cell(5,3,default,empty).

cell(5,4,default,empty).

cell(5,5,default,empty).

cell(5,6,default,empty).

cell(5,7,blue,arm).

cell(6,0,red,arrow).

cell(6,1,default,empty).

cell(6,2,yellow,money).

cell(6,3,default,empty).

cell(6,4,default,empty).

cell(6,5,default,empty).

cell(6,6,default,empty).

cell(6,7,default,empty).

cell(7,0,default,empty).

cell(7,1,default,empty).

cell(7,2,default,empty).

cell(7,3,default,empty).

cell(7,4,default,empty).

cell(7,5,default,empty).

cell(7,6,yellow,treasure).

cell(7,7,yellow,home).

