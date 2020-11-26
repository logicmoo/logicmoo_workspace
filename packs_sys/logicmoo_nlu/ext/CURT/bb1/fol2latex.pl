/*************************************************************************

    File: fol2latex.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/


fol2file(F):-
    open('test.tex',write,Stream),
    write(Stream,'\\documentstyle{article}'),
    nl(Stream),
    write(Stream,'\\begin{document}'),
    nl(Stream),
    numbervars(F,0,_),
    fol2latex(F,Stream),
    nl(Stream),
    write(Stream,'\\end{document}'),
    nl(Stream),
    close(Stream).

fol2latex(F):-
    fol2latex(F,user).


fol2latex(some(X,F),Stream):- !,
    write(Stream,'$\\exists$'),
    write_term(Stream,X,[numbervars(true)]),
    fol2latex(F,Stream).

fol2latex(all(X,F),Stream):- !,
    write(Stream,'$\\forall$'),
    write_term(Stream,X,[numbervars(true)]),
    fol2latex(F,Stream).

fol2latex(lam(X,F),Stream):- !,
    write(Stream,'$\\lambda$'),
    write_term(Stream,X,[numbervars(true)]),
    fol2latex(F,Stream).

fol2latex(que(X,F1,F2),Stream):- !,
    write(Stream,'?'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,'('),
    fol2latex(F1,Stream),
    write(Stream,','),
    fol2latex(F2,Stream),
    write(Stream,')').

fol2latex(and(F1,F2),Stream):- !,
    write(Stream,'('),
    fol2latex(F1,Stream),
    write(Stream,' $\\land$ '),
    fol2latex(F2,Stream),
    write(Stream,')').

fol2latex(imp(F1,F2),Stream):- !,
    write(Stream,'('),
    fol2latex(F1,Stream),
    write(Stream,' $\\to$ '),
    fol2latex(F2,Stream),
    write(Stream,')').

fol2latex(or(F1,F2),Stream):- !,
    write(Stream,'('),
    fol2latex(F1,Stream),
    write(Stream,' $\\lor$ '),
    fol2latex(F2,Stream),
    write(Stream,')').

fol2latex(app(F1,F2),Stream):- !,
    write(Stream,'('),
    fol2latex(F1,Stream),
    write(Stream,'@'),
    fol2latex(F2,Stream),
    write(Stream,')').

fol2latex(leq(X,Y),Stream):- !,
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,'$\\leq$'),
    write_term(Stream,Y,[numbervars(true)]).

fol2latex(not(F),Stream):- !,
    write(Stream,'$\\neg$'),
    fol2latex(F,Stream).

fol2latex(pred1(L,S,X),Stream):- !, 
    write_term(Stream,L,[numbervars(true)]),
    write(Stream,':\\textsc{'),
    write_term(Stream,S,[numbervars(true)]),
    write(Stream,'}('),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,')').

fol2latex(pred2(L,S,X,Y),Stream):- !,
    write_term(Stream,L,[numbervars(true)]),
    write(Stream,':\\textsc{'),
    write_term(Stream,S,[numbervars(true)]),
    write(Stream,'}('),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,','),
    write_term(Stream,Y,[numbervars(true)]),
    write(Stream,')').

fol2latex(eq(L,X,Y),Stream):- !,
    write_term(Stream,L,[numbervars(true)]),
    write(Stream,':'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,'='),
    write_term(Stream,Y,[numbervars(true)]).

fol2latex(F,Stream):-
    F =.. [Symbol,Arg],
    write(Stream,'\\textsc{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}('),
    write_term(Stream,Arg,[numbervars(true)]),
    write(Stream,')').

fol2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2],
    write(Stream,'\\textsc{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}('),
    write_term(Stream,Arg1,[numbervars(true)]),
    write(Stream,','),
    write_term(Stream,Arg2,[numbervars(true)]),
    write(Stream,')').

fol2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2,Arg3],
    write(Stream,'\\textsc{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}('),
    write_term(Stream,Arg1,[numbervars(true)]),
    write(Stream,','),
    write_term(Stream,Arg2,[numbervars(true)]),
    write(Stream,','),
    write_term(Stream,Arg3,[numbervars(true)]),
    write(Stream,')').
