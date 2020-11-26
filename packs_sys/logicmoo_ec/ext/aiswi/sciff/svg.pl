% blah

svg_debug:-
    svg('debug.svg',no,[history,exp,grid]).
svg_history:-
    svg('history.svg',anim(4),[history,grid]).
svg(OutFile,Anim,Options):-
    open(OutFile,write,Stream),
    write(Stream,'<?xml version="1.0" encoding="utf-8"?>'), nl(Stream),
    write(Stream,'<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG Tiny 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-tiny.dtd">'), nl(Stream),
    write(Stream,'<svg viewBox="0 0 1000 '),
    write(Stream,1000),
    write(Stream,'" xmlns="http://www.w3.org/2000/svg">'),nl(Stream),
    (member(history,Options)
     ->    findall_constraints(h(_,_),History)
     ;      History=[]),
    (History = [] -> Max = 10 ; maxtime(History,Max)),
    (Anim=no, member(grid,Options)
     -> Step is 600/Max, vert_grid(Stream,Max,Step,400)
     ; true),
    (Anim = anim(_) -> animated_bar(Stream,Anim) ; true),
    svg_list(Stream,History,Max,Anim,20,Options),
    (member(exp,Options)
     ->    findall_constraints(e(_,_),Exp)
     ;      Exp=[]),
    svg_list(Stream,Exp,Max,Anim,500,Options),
    write(Stream,'</svg>'),
    close(Stream).

maxtime([#(h(_,T),_)],T).
maxtime([#(h(_,T),_)|L],TM):-
    maxtime(L,TM1),
    (TM1 > T -> TM=TM1 ; TM=T).

svg_list(_,[],_,_,_,_).
svg_list(S,[#(h(A,Texp),_)|L],Max,anim(Sec),X1,Options):-
    %X2 is X1 + 10,
    X2 is X1,
    write(S,'<text x="'),
    write(S,X1),
    write(S,'" y="'),
    fd_min(Texp,Tmin), fd_max(Texp,Tmax),
    T is (Tmin+Tmax)/2, Tmin1 is round(Tmin/Max*900), Tmax1 is round(Tmax/Max*900),
    T1 is round(T*900/Max),
    T2 is T*Sec/Max,
    write(S,T1),
    write(S,'" style="fill:white;stroke:none">H('),
    write(S,A),
    write(S,','),
    write(S,Texp),
    write(S,')'),
    write(S,'<set attributeType="CSS" attributeName="fill" to="black" begin="'),
    write(S,T2),
    write(S,'s" />'),
    write(S,'</text>'), nl(S),
    rectangle(S,X1,X2,Tmin1,Tmax1,green),
    nl(S),
    svg_list(S,L,Max,anim(Sec),X2,Options).
svg_list(S,[#(e(A,Texp),_)|L],Max,anim(Sec),X1,Options):-
    X0 is X1 - 10,
    X2 is X1 + 10,
    %X2 is X1,
    write(S,'<text x="'),
    write(S,X2),
    write(S,'" y="'),
    fd_min(Texp,Tmin), fd_max(Texp,Tmax),
    T is (Tmin+Tmax)/2, Tmin1 is round(Tmin/Max*900), Tmax1 is round(Tmax/Max*900),
    T1 is round(T*900/Max),
    
    write(S,T1),
    write(S,'" style="fill:green">E('),
    write(S,A),
    write(S,','),
    write(S,Texp),
    write(S,')'),
    %T2 is T*Sec/Max,
    %write(S,'<set attributeType="CSS" attributeName="fill" to="black" begin="'),
    %write(S,T2),   Animation
    %write(S,'s" />'),
    write(S,'</text>'), nl(S),
    
    triangle(S,X0,X2,Tmin1,Tmax1,T1,green),
    nl(S),
    svg_list(S,L,Max,anim(Sec),X1,Options).
svg_list(S,[#(Atom,_)|L],Max,no,Y1,Options):-
    Atom =.. [F,A,Texp],
    atom_color(F,Col),
    Y2 is Y1 + 20,
    Y0 is Y1-20,
    (member(grid,Options) -> line(S,0,1000,Y1,Y1,lightgray); true),
    write(S,'<text x="'),
    write(S,10),
    write(S,'" y="'),
    fd_min(Texp,Tmin), fd_max(Texp,Tmax),
    %T is (Tmin+Tmax)/2, 
    Tmin1 is Tmin/Max*500+400, Tmax1 is Tmax/Max*500+400,
    %T1 is T*500/Max,
    %T2 is T*Sec/Max,
    write(S,Y1),
    write(S,'" style="fill:'), write(S,Col), write(S,';stroke:none">'),
    write(S,F),
    write(S,'('),
    write(S,A),
    write(S,','),
    write(S,Texp),
    write(S,')'),
    %write(S,'<set attributeType="CSS" attributeName="fill" to="black" begin="'),
    %write(S,T2),
    %write(S,'s" />'),
    write(S,'</text>'), nl(S),
    (Tmin1=Tmax1 -> Tmax2 is Tmin1+1; Tmax2 = Tmax1),
    fd_dom(Texp,Dom),
    dom_rectangle(S,Dom,400,1000,Max,Y0,Y1,Col),
    % rectangle(S,Tmin1,Tmax2,Y0,Y1,Col),
    nl(S),
    svg_list(S,L,Max,no,Y2,Options).

animated_bar(S,anim(Sec)):-
    write(S,'<rect x="0" y="00" width="20" height="900" style="fill:blue;stroke:black" >'),nl(S),
    write(S,'<animate attributeName="height" from="0" to="900" begin="0s" dur="'),
    write(S,Sec),
    write(S,'s" fill="freeze" />'),nl(S),
    write(S,'</rect>'), nl(S).

rectangle(S,X1,X2,Y1,Y2,Col):-
    write(S,'<rect x="'), write(S,X1),
    write(S,'" y="'), write(S,Y1),
    write(S,'" width="'),
    Width is X2-X1, write(S,Width),
    write(S,'" height="'),
    Height is Y2-Y1, write(S,Height),
    write(S,'" style="fill:'),
    write(S,Col),
    write(S,';stroke:black" >'),nl(S),
%    write(S,'<animate attributeName="height" from="0" to="500" begin="0s" dur="'),
%    write(S,Sec),
%    write(S,'s" fill="freeze" />'),nl(S),
    write(S,'</rect>'), nl(S).    

line(S,X1,X2,Y1,Y2,Col):-
    write(S,'<line x1="'), write(S,X1),
    write(S,'" y1="'), write(S,Y1),
    write(S,'" x2="'),
    write(S,X2),
    write(S,'" y2="'),
    write(S,Y2),
    write(S,'" stroke="'),
    write(S,Col),
    write(S,'" />'),nl(S).

atom_color(h,blue).
atom_color(e,green).

dom_rectangle(S,{Dom},Xmin,Xmax,Max,Y0,Y1,Col):-
    X is Dom*(Xmax-Xmin)/Max+Xmin,
    X1 is X+1,
    rectangle(S,X,X1,Y0,Y1,Col).
dom_rectangle(S,A..B,Xmin,Xmax,Max,Y0,Y1,Col):-
    X1 is A*(Xmax-Xmin)/Max+Xmin,
    X2 is B*(Xmax-Xmin)/Max+Xmin,
    rectangle(S,X1,X2,Y0,Y1,Col).
dom_rectangle(S,A\/B,Xmin,Xmax,Max,Y0,Y1,Col):-
    dom_rectangle(S,A,Xmin,Xmax,Max,Y0,Y1,Col),
    dom_rectangle(S,B,Xmin,Xmax,Max,Y0,Y1,Col).

triangle(S,X1,X2,Tmin1,Tmax1,Texp,Col):-
    write(S,'<path d="M'), write(S,X1), write(S,' '), write(S,Tmin1),
    write(S,'V'), write(S,Tmax1),
    write(S,'L'), write(S,X2), write(S,' '), write(S,Texp), write(S,'Z" fill="'),
    write(S,Col), write(S,'" stroke="black" opacity=".7" />'), nl(S).

:- multifile user:debugger_command_hook/2.
user:debugger_command_hook(unknown([115,118,103],_), Actions) :- % Command: svg
    Actions = [],
    svg('debug.svg',no,[history,exp,grid]).

vert_grid(_,0,_,_):- !.
vert_grid(S,N,Step,Xmin):-
    X is N*Step+Xmin,
    line(S,X,X,0,1000,lightgray),
    write(S,'<text x="'), write(S,X), write(S,'" y="980" stroke="lightgray">'),
    write(S,N), write(S,'</text>'), nl(S),
    N1 is N-1,
    vert_grid(S,N1,Step,Xmin).
