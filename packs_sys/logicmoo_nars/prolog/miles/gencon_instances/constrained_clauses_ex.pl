/*
type_restriction((A < B),[number(A),number(B)]).


ex(merge([1],[2],[1,2]),'+').
ex(merge([6],[4],[4,6]),'+').
ex(merge([2,3,4,5],[4,7],[2,3,4,4,5,7]),'+'). 
ex(merge([44,55,66],[22,33,44,55],[22,33,44,44,55,55,66]),'+'). 
ex(merge([],[54,66,77,88,97],[54,66,77,88,97]),'+'). 
ex(merge([],[4],[4]),'+'). 
ex(merge([],[],[]),'+').
ex(merge([22,23,24,25],[],[22,23,24,25]),'+'). 
ex(merge([24],[],[24]),'+'). 
ex(merge([29,39,49,59],[37,79,99],[29,37,39,49,59,79,99]),'+').
ex(merge([2],[4,7],[2,4,7]),'+').
ex(merge([1],[2],[2,1]),'-').
ex(merge([6],[4],[6,4]),'-').
ex(merge([8],[7],[8]),'-').
ex(merge([2,3,4,5],[4,7],[4,3,2,4,5,7]),'-'). 
ex(merge([44,55,66],[22,33,44,55],[22,44,55,33,55,44,66]),'-'). 
ex(merge([29,39,49,59],[37,79,99],[37,39,29,59,79,49,99]),'-').
ex(merge([2],[4,7],[4,2]),'-').
ex(merge([1,2],[3,4],[1,3,2,4]),'-').


*/

type_restriction(male(A),[atom(A)]).
type_restriction(female(A),[atom(A)]).
type_restriction(parent(A,B),[atom(A),atom(B)]).


ex(father(ma,b),+).
ex(father(mc,d),+).
ex(father(me,f),+).
ex(father(mg,h),+).
ex(father(mi,j),+).
ex(father(mk,l),+).

ex(father(fa,b),-).
ex(father(fc,d),-).
ex(father(fe,f),-).
ex(father(fg,h),-).
ex(father(fi,j),-).
ex(father(fk,l),-).

ex(father(b,ma),-).
ex(father(d,mc),-).
ex(father(f,me),-).
ex(father(h,mg),-).
ex(father(j,mi),-).
ex(father(l,mk),-).
ex(father(b,fa),-).
ex(father(d,fc),-).
ex(father(f,fe),-).
ex(father(h,fg),-).
ex(father(j,fi),-).
ex(father(l,fk),-).


ex(human(ma),+).
ex(human(mc),+).
ex(human(me),+).
ex(human(mg),+).
ex(human(mi),+).
ex(human(mk),+).
ex(human(fa),+).
ex(human(fc),+).
ex(human(fe),+).
ex(human(fg),+).
ex(human(fi),+).
ex(human(fk),+).
ex(human(b),+).
ex(human(d),+).
ex(human(f),+).
ex(human(h),+).
ex(human(j),+).
ex(human(l),+).
ex(human(a),-).
ex(human(c),-).


male(ma).
male(mc).
male(me).
male(mg).
male(mi).
male(mk).
male(b).
male(d).
male(f).
male(h).
male(j).
male(l).

female(fa).
female(fc).
female(fe).
female(fg).
female(fi).
female(fk).


parent(ma,b).
parent(mc,d).
parent(me,f).
parent(mg,h).
parent(mi,j).
parent(mk,l).

parent(fa,b).
parent(fc,d).
parent(fe,f).
parent(fg,h).
parent(fi,j).
parent(fk,l).


/*

ex(t(nil),+).
ex(t(tree(nil,0,nil)),+).
ex(t(tree(nil,0,tree(nil,0,nil))),+).
ex(t(tree(nil,0,tree(nil,s(0),nil))),+).
ex(t(tree(nil,0,tree(nil,s(s(0)),nil))),+).
ex(t(tree(nil,0,tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(nil,s(0),nil)),+).
ex(t(tree(nil,s(0),tree(nil,0,nil))),+).
ex(t(tree(nil,s(0),tree(nil,s(0),nil))),+).
ex(t(tree(nil,s(0),tree(nil,s(s(0)),nil))),+).
ex(t(tree(nil,s(0),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(nil,s(s(0)),nil)),+).
ex(t(tree(nil,s(s(0)),tree(nil,0,nil))),+).
ex(t(tree(nil,s(s(0)),tree(nil,s(0),nil))),+).
ex(t(tree(nil,s(s(0)),tree(nil,s(s(0)),nil))),+).
ex(t(tree(nil,s(s(0)),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(nil,s(s(s(0))),nil)),+).
ex(t(tree(nil,s(s(s(0))),tree(nil,0,nil))),+).
ex(t(tree(nil,s(s(s(0))),tree(nil,s(0),nil))),+).
ex(t(tree(nil,s(s(s(0))),tree(nil,s(s(0)),nil))),+).
ex(t(tree(nil,s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,0,nil),0,nil)),+).
ex(t(tree(tree(nil,0,nil),0,tree(nil,0,nil))),+).
ex(t(tree(tree(nil,0,nil),0,tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,0,nil),0,tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,0,nil),0,tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,0,nil),s(0),nil)),+).
ex(t(tree(tree(nil,0,nil),s(0),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,0,nil),s(0),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,0,nil),s(0),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,0,nil),s(0),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,0,nil),s(s(0)),nil)),+).
ex(t(tree(tree(nil,0,nil),s(s(0)),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,0,nil),s(s(0)),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,0,nil),s(s(0)),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,0,nil),s(s(0)),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,0,nil),s(s(s(0))),nil)),+).
ex(t(tree(tree(nil,0,nil),s(s(s(0))),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,0,nil),s(s(s(0))),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,0,nil),s(s(s(0))),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,0,nil),s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(0),nil),0,nil)),+).
ex(t(tree(tree(nil,s(0),nil),0,tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(0),nil),0,tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(0),nil),0,tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(0),nil),0,tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(0),nil)),+).
ex(t(tree(tree(nil,s(0),nil),s(0),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(0),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(0),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(0),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(s(0)),nil)),+).
ex(t(tree(tree(nil,s(0),nil),s(s(0)),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(s(0)),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(s(0)),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(s(0)),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),nil)),+).
ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(0),nil),s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),0,nil)),+).
ex(t(tree(tree(nil,s(s(0)),nil),0,tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),0,tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),0,tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),0,tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(0),nil)),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(0),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(0),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(0),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(0),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),nil)),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(0)),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),nil)),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(s(0)),nil),s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),0,nil)),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),0,tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),0,tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),0,tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),0,tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),nil)),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(0),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),nil)),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(0)),tree(nil,s(s(s(0))),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),nil)),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),tree(nil,0,nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),tree(nil,s(0),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),tree(nil,s(s(0)),nil))),+).
ex(t(tree(tree(nil,s(s(s(0))),nil),s(s(s(0))),tree(nil,s(s(s(0))),nil))),+).

ex(t(0),-).
ex(t(s(0)),-).
ex(t(s(s(0))),-).
ex(t(s(s(s(0)))),-).
ex(t(tree(nil,nil,nil)),-).
ex(t(tree(nil,tree(nil,0,nil),nil)),-).
ex(t(tree(0,0,s(0))),-).
ex(t(tree(s(s(0)),s(0),s(s(s(0))))),-).
ex(t(tree(nil,0,s(0))),-).
ex(t(tree(nil,s(0),s(s(s(0))))),-).
ex(t(tree(0,0,nil)),-).
ex(t(tree(s(s(s(0))),s(0),nil)),-).
ex(t(tree(nil,s(nil),nil)),-).
ex(t(tree(nil,s(s(nil)),nil)),-).
ex(t(tree(nil,s(tree(nil,0,nil)),nil)),-).
ex(t(tree(nil,s(s(tree(nil,s(0),nil))),nil)),-).
ex(t(tree(nil,s(s(s(nil))),nil)),-).


ex(p(f([],[])),+).
ex(p(f([a],[b])),+).
ex(p(f([a,a],[b,b])),+).
ex(p(f([a,a,a],[b,b,b])),+).
ex(p(f([a,a,a,a],[b,b,b,b])),+).


ex(p([]),-).
ex(p([a]),-).
ex(p([a,a]),-).
ex(p([a,a,a]),-).
ex(p([a,a,a,a]),-).
ex(p([b]),-).
ex(p([b,b]),-).
ex(p([b,b,b]),-).
ex(p([b,b,b,b]),-).
ex(p(f([a],[])),-).
ex(p(f([],[b,b])),-).
ex(p(f([a,a,a],[b,b])),-).
ex(p(f([a],[b,b,b])),-).
ex(p(f([a,a,a,a],[b,b,b])),-).
*/
