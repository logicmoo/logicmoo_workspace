:- use_module(library(pillow)).
:- use_module(parser_utils).
:- use_module(library(lists)).

:- dynamic html_env/1.

browse_html(File,Atom):-
    read_file_to_string(File,String),
    html2terms(String,Term),
    atom_codes(Atom,SearchString),
    find_pos(env(a,[name=SearchString],[]),Term,Following),
    print_html(Following).

list_topics(File):-
    read_file_to_string(File,String),
    html2terms(String,Term),
    write('Help available on the following topics'), nl,
    write('======================================'), nl,
    list_topics_term(Term).

list_topics_term([env(a,[name=Topics],[])|T]):- !,
    write(' *'), format('~s',[Topics]), write('* '), nl,
    list_topics_term(T).
list_topics_term([]).
list_topics_term([env(_,_,L)|T]):-!,
    list_topics_term(L),
    list_topics_term(T).
list_topics_term([_|T]):-
    list_topics_term(T).

find_pos(H,[H|T],T):- !.
find_pos(H,[env(_,_,L)|_],R):-
    find_pos(H,L,R),!.
find_pos(X,[_|T],R):-
    find_pos(X,T,R).

print_html([]):-!.
print_html([env(a,[name=_],[])|_]):- !. % Next help topic: end
print_html([env(h1,_,[L])|T]):- !, nl, format('~s',[L]), nl, 
    underline(L,'='), nl, print_html(T).
print_html([env(h2,_,[L])|T]):- !, nl, format('~s',[L]), nl, 
    underline(L,'='), nl, print_html(T).
print_html([env(h3,_,[L])|T]):- !, nl, format('~s',[L]), nl,
    underline(L,'-'), nl, print_html(T).
print_html([env(h4,_,[L])|T]):- !, nl, format('~s',[L]), nl,
    nl, print_html(T).
print_html([env(h5,_,[L])|T]):- !, nl, format('~s',[L]), print_html(T).
print_html([env(h6,_,[L])|T]):- !, nl, format('~s',[L]), print_html(T).
print_html([env(p,_,L)|T]):- !, print_html(L), nl, print_html(T).
print_html([comment(_)|T]):- !, print_html(T).
print_html([env(a,_,L)|T]):- !, write(' *'), print_html(L), write('* '), print_html(T).
print_html([env(ul,_,L)|T]):- !, print_html(L), nl, print_html(T).
print_html([li$[]|T]):- !, nl, write(' - '), print_html(T).
print_html([env(li,_,L)|T]):- !, nl, write(' - '), print_html(L), print_html(T).
print_html([env(center,_,L)|T]):- !, nl, write('   '), print_html(L), nl, print_html(T).
% Ugly but fast ...
print_html([env(pre,_,L)|T]):- assert(html_env(pre)), print_html(L), nl, 
    retract(html_env(pre)), print_html(T).
print_html([p$[]|T]):-!, nl, print_html(T).
print_html([br$[]|T]):- !, nl, print_html(T).
print_html([hr$[]|T]):-!, nl, 
    write('---------------------------------------------------------------------'),
    nl,
    print_html(T).
print_html([env(_ENV,_,L)|T]):- %all other environments are ignored
    !, print_html(L), print_html(T).
print_html([[]|T]):-!,
    print_html(T).
%print_html([L|T]):-
%    L=[10|L1],!,
%    format('~s',[L1]),
%    print_html(T).
print_html([L|T]):-
    L=[_|_],!,
    %format('~s',[L]),
    print_string(L),
    print_html(T).

underline([],_).
underline([_|T],C):-
    format('~s',[C]),
    underline(T,C).

print_string([]).
print_string([10|S]):- html_env(pre), !, nl, print_string(S).
print_string([10|S]):- write(' '), print_string(S).
print_string(S):-
    special_char(Char,Atom),
    atom_codes(Char,S1), append(S1,T,S),!,
    write(Atom), print_string(T).
print_string([C|S]):- format('~s',[[C]]), print_string(S).

% Special html characters: special_char(htmlcode,atom to visualise).
special_char('&rarr;','--->').
special_char('&and;','/\\').
special_char('&or;','\\/').
special_char('&exist;',' EXISTS ').
special_char('&forall;',' FORALL ').
special_char('&gt;','>').
special_char('&lt;','<').
special_char('&nbsp;',' ').
