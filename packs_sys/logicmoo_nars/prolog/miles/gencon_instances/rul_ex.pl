%%% use init_kb('gilppi_instances/rul_ex.pl',type) for loading
%%%                                          ~~~~~

matomic(X):- atomic(X).
matom(X):- atom(X).
mnumber(X):- number(X).
/*
ex(app([1,2]),'+').
ex(app([a]),'+').
ex(app([]),'+').
ex(app([p]),'+').
ex(app([r,s]),'+').
ex(app([g]),'+').
ex(app([9,8,7]),'+').
ex(app([4,3,5]),'+').
ex(app([r,w]),'+').
ex(app([j,k,l,m]),'+').


ex(split([3]),'+').
ex(split([4]),'+').
ex(split([1,2]),'+').
ex(split([55,66]),'+').
ex(split([7,8,9]),'+').
ex(split([11,12,13]),'+').
ex(split([14,15,16,17]),'+').
ex(split([18,19,20,21]),'+').
ex(split([22,23,24,25,26]),'+').
ex(split([27,28,29,30,31]),'+').


ex(obti([[],0,[]]),'+').
ex(obti([]),'+').
ex(obti([[],2,[]]),'+').
ex(obti([[[],1,[]],2,[[[],3,[]],4,[]]]),'+').
ex(obti([[[],6,[[],7,[]]],8,[[],9,[]]]),'+').
ex(obti([[[],89,[]],90,[[],91,[]]]),'+').
ex(obti([[[],91,[]],92,[[],93,[]]]),'+').
ex(obti([[],50,[[[],51,[]],52,[[],54,[]]]]),'+').
ex(obti([[[[],36,[]],37,[[],39,[]]],46,[]]),'+').


ex(plus(0),'+').
ex(plus(s(0)),'+').
ex(plus(s(s(0))),'+').
ex(plus(s(s(s(s(s(s(0))))))),'+').
ex(plus(s(s(s(0)))),'+').
ex(plus(s(s(s(s(0))))),'+').



ex(equiv(not(and([]))),'+').
ex(equiv(not(and([a]))),'+').
ex(equiv(not(and([b]))),'+').
ex(equiv(not(and([x,y]))),'+').
ex(equiv(not(and([r,s]))),'+').
ex(equiv(not(and([k,l,m]))),'+').
ex(equiv(not(and([kk,ll,mm]))),'+').
ex(equiv(not(and([h,i,j,k]))),'+').
ex(equiv(not(and([hh,ii,jj,kk]))),'+').
ex(equiv(not(and([c,d,e,f,g]))),'+').
ex(equiv(not(and([cc,dd,ee,ff,gg]))),'+').
ex(equiv(not(or([]))),'+').
ex(equiv(not(or([a1]))),'+').
ex(equiv(not(or([b1]))),'+').
ex(equiv(not(or([x1,y1]))),'+').
ex(equiv(not(or([r1,s1]))),'+').
ex(equiv(not(or([k1,l1,m1]))),'+').
ex(equiv(not(or([kk1,ll1,mm1]))),'+').
ex(equiv(not(or([h1,i1,j1,k1]))),'+').
ex(equiv(not(or([hh1,ii1,jj1,kk1]))),'+').
ex(equiv(not(or([c1,d1,e1,f1,g1]))),'+').
ex(equiv(not(or([cc1,dd1,ee1,ff1,gg1]))),'+').
ex(equiv(or([])),'+').
ex(equiv(or([not(a)])),'+').
ex(equiv(or([not(b)])),'+').
ex(equiv(or([not(x),not(y)])),'+').
ex(equiv(or([not(r),not(s)])),'+').
ex(equiv(or([not(k),not(l),not(m)])),'+').
ex(equiv(or([not(kk),not(ll),not(mm)])),'+').
ex(equiv(or([not(h),not(i),not(j),not(k)])),'+').
ex(equiv(or([not(hh),not(ii),not(jj),not(kk)])),'+').
ex(equiv(or([not(c),not(d),not(e),not(f),not(g)])),'+').
ex(equiv(or([not(cc),not(dd),not(ee),not(ff),not(gg)])),'+').
ex(equiv(and([])),'+').
ex(equiv(and([not(a1)])),'+').
ex(equiv(and([not(b1)])),'+').
ex(equiv(and([not(x1),not(y1)])),'+').
ex(equiv(and([not(r1),not(s1)])),'+').
ex(equiv(and([not(k1),not(l1),not(m1)])),'+').
ex(equiv(and([not(kk1),not(ll1),not(mm1)])),'+').
ex(equiv(and([not(h1),not(i1),not(j1),not(k1)])),'+').
ex(equiv(and([not(hh1),not(ii1),not(jj1),not(kk1)])),'+').
ex(equiv(and([not(c1),not(d1),not(e1),not(f1),not(g1)])),'+').
ex(equiv(and([not(cc1),not(dd1),not(ee1),not(ff1),not(gg1)])),'+').


ex(equiv(not(and([]))),'+').
ex(equiv(not(or([]))),'+').
ex(equiv(not(and([a]))),'+').
ex(equiv(not(or([b]))),'+').
ex(equiv(not(and([x,y]))),'+').
ex(equiv(not(or([r,s]))),'+').
ex(equiv(not(and([k,l,m]))),'+').
ex(equiv(not(or([kk,ll,mm]))),'+').
ex(equiv(and([])),'+').
ex(equiv(or([])),'+').
ex(equiv(and([not(a1)])),'+').
ex(equiv(or([not(b1)])),'+').
ex(equiv(and([not(x1),not(y1)])),'+').
ex(equiv(or([not(r1),not(s1)])),'+').
ex(equiv(and([not(k1),not(l1),not(m1)])),'+').
ex(equiv(or([not(kk1),not(ll1),not(mm1)])),'+').


newp4([A|B]):-matom(A),newp4(B).
equiv(and(A)):-newp2(A).
equiv(not A):-newp3(A).
equiv(or(A)):-newp1(A).
newp1([]).
newp1([not A|B]):-matom(A),newp1(B).
newp2([]).
newp2([not A|B]):-matom(A),newp1(B).
newp3(and(A)):-newp5(A).
newp3(or(A)):-newp4(A).
newp4([]).
newp5([]).
newp5([A|B]).



ex(t(f(a,f(b,f(c,x)))),'+').
ex(t(f(d,f(e,f(f,f(g,x))))),'+').
ex(t(f(i,f(j,x))),'+').
ex(t(f(h,x)),'+').
ex(t(x),'+').
ex(t(f(k,g(y))),'+').
ex(t(f(m,g(g(y)))),'+').
ex(t(f(n,g(g(g(y))))),'+').
ex(t(f(o,g(g(g(g(y)))))),'+').
ex(t(f(p,y)),'+').




ex(r(x),'+').
ex(r(f(h,x)),'+').
ex(r(f(i,g(j,x))),'+').
ex(r(f(a,g(b,f(c,x)))),'+').
ex(r(f(d,g(e,f(f,g(g,x))))),'+').
ex(r(f(q,g(r,f(s,g(t,f(u,x)))))),'+').
ex(r(f(k,g(l,f(m,g(n,f(o,g(p,x))))))),'+').
ex(r(f(k1,g(l1,f(m1,g(n1,f(o1,g(p1,f(q1,x)))))))),'+').




ex(s(x),'+').
ex(s(f(h,x)),'+').
ex(s(f(g(j,x),g(j,x))),'+').
ex(s(f(g(b,h(c,x)),g(b,h(c,x)))),'+').
ex(s(f(g(e,h(f,f(g,x))),g(e,h(f,f(g,x))))),'+').
ex(s(f(g(r,h(s,f(t,g(u,x)))),g(r,h(s,f(t,g(u,x)))))),'+').
ex(s(f(g(l,h(m,f(n,g(o,h(p,x))))),g(l,h(m,f(n,g(o,h(p,x))))))),'+').
ex(s(f(g(l1,h(m1,f(n1,g(o1,h(p1,f(q1,x)))))),g(l1,h(m1,f(n1,g(o1,h(p1,f(q1,x)))))))),'+').
ex(s(f(g(l1,h(m1,f(n1,g(o1,h(p1,f(q1,g(r1,x))))))),
   g(l1,h(m1,f(n1,g(o1,h(p1,f(q1,g(r1,x))))))))),'+').
ex(s(f(g(l1,h(m1,f(n1,g(o1,h(p1,f(q1,g(t1,h(u1,x)))))))),
  g(l1,h(m1,f(n1,g(o1,h(p1,f(q1,g(t1,h(u1,x)))))))))),'+').
ex(s(f(g(l1,h(m1,f(n1,g(o1,h(p1,f(q1,g(v1,h(w1,f(x1,x))))))))),
  g(l1,h(m1,f(n1,g(o1,h(p1,f(q1,g(v1,h(w1,f(x1,x))))))))))),'+').


ex(i(not(not(and(a,a)))),'+').
ex(i(not(and(not(a),not(a)))),'+').
ex(i(not(and(not(a),and(a,a)))),'+').
ex(i(not(and(and(a,a),not(a)))),'+').
ex(i(not(and(and(a,a),and(a,a)))),'+').
ex(i(not(not(a))),'+').
ex(i(not(and(a,a))),'+').
ex(i(not(a)),'+').
ex(i(not(not(not(a)))),'+').
ex(i(and(not(not(a)),not(a))),'+').
ex(i(and(not(not(a)),and(not(a),not(a)))),'+').
ex(i(and(not(not(a)),and(not(a),and(a,a)))),'+').
ex(i(and(not(not(a)),and(not(a),a))),'+').
ex(i(and(not(and(a,a)),not(not(a)))),'+').
ex(i(and(not(and(a,a)),not(and(a,a)))),'+').
ex(i(and(not(and(a,a)),not(a))),'+').
ex(i(and(not(and(a,a)),and(not(a),not(a)))),'+').
ex(i(and(not(and(a,a)),and(not(a),and(a,a)))),'+').
ex(i(and(not(and(a,a)),and(not(a),a))),'+').
ex(i(and(not(not(a)),and(and(a,a),not(a)))),'+').
ex(i(and(not(not(a)),and(and(a,a),and(a,a)))),'+').
ex(i(and(and(not(a),not(a)),and(a,and(a,a)))),'+').
ex(i(and(and(not(a),not(a)),and(a,a))),'+').
ex(i(and(and(not(a),not(a)),a)),'+').
ex(i(and(and(not(a),and(a,a)),not(not(a)))),'+').
ex(i(and(not(a),not(a))),'+').
ex(i(and(not(a),and(a,a))),'+').
ex(i(and(not(a),a)),'+').
ex(i(and(and(a,a),not(a))),'+').
ex(i(and(and(a,a),and(a,a))),'+').
ex(i(and(and(a,a),a)),'+').
ex(i(and(a,not(a))),'+').
ex(i(and(a,and(a,a))),'+').
ex(i(and(a,a)),'+').
ex(i(a),'+').
*/

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
