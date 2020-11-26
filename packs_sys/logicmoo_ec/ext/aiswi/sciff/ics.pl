:-module(ics,[ics/2]).

ics([h(sono(X,Y,C,F),_)],
        [[cell(X,Y,C,F)]]).

ics([h(start,0.0),h(sono(X,Y,C,F),T)],
        [[T2=T+1.0,en(sono(X,Y,C,F),T2)]]).

ics([h(start,0.0)],
        [[e(sono(0,0,yellow,money),0.0)]]).

ics([h(start,0.0)],
        [[T>0,T2>T,e(sono(7,7,default,home),T),en(sono(_,_,_,_),T2)]]).

