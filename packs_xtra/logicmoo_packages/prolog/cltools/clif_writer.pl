/* -*- Mode: Prolog -*- */

:- module(clif_writer,
          [
           export_clif/2
          ]).

:- use_module(cl_io).
:- use_module(cl_transform).

:- multifile cl_io:serialize_cltext_hook/4.
cl_io:serialize_cltext_hook(_File,clif,Text,Opts) :-
        export_clif(Text,Opts).

export_clif(Text,Opts) :-
        write_axioms(Text,_,Opts).


write_comment(A):-
        format('// ~w~n',[A]).

write_axioms(A,Toks,_) :-
        cltext(1,A,Toks,[]),
        concat_atom(Toks,S),
        write(S),
        nl.

jop(and).
jop(or).
jop(not).

nl --> ['\n'].

opb --> ['('].
opb(X) --> ['('],[X],[' '].
cl --> [')'].
cl(_+1) --> !,cl.
cl(_) --> !,cl,nl,nl.

next(T) --> nl,tabs(T).

tabs(T+1) --> !,[' '],tabs(T).
tabs(_) --> [].

cltext(_,[]) --> !.
cltext(T,[H|L]) --> cltext(T,H),!,cltext(T,L).
cltext(_,[H|_]) --> {throw(cannot_translate(H))}.


cltext(T,module(X,Y,Text)) --> !,opb('cl-module'),name(X),names(Y),nl,cltext(T+1,Text),cl(T).

%cltext(T,cltext(Text)) --> !,opb('cl-text'),nl,cltext(T+1,Text),cl(T).
cltext(T,cltext(Text)) --> !,cltext(T,Text).
cltext(T,namedtext(X,Text)) --> !,opb('cl-text'),name(X),nl,cltext(T+1,Text),cl(T).
cltext(T,'$comment'(X,Text)) --> !,opb('cl-comment'),quoted(X),[' '],cltext(T+1,Text),cl(T).
cltext(T,X) --> clsentence(T,X).

clsentences(_,[]) --> [].
clsentences(T,[S|SL]) --> clsentence(T,S),!,clsentences(T,SL).

% quantified sentences
clsentence(T,forall(X,Y)) --> !,next(T),opb(forall),boundlist(X),clsentence(T+1,Y),cl(T).
clsentence(T,exists(X,Y)) --> !,next(T),opb(exists),boundlist(X),clsentence(T+1,Y),cl(T).
clsentence(T,if(X,Y)) --> !,next(T),opb(if),clsentence(T,X),next(T+1),clsentence(T+1,Y),cl(T).
clsentence(T,iff(X,Y)) --> !,next(T),opb(iff),clsentence(T,X),nl,clsentence(T+1,Y),cl(T).

clsentence(T,X) --> boolsent(T,X).
clsentence(T,X) --> atomsent(T,X).

boolsent(T,X) --> {X=..[Op|L],jop(Op)},!,opb(Op),clsentences(T,L),cl(T).

atomsent(T,X) --> {is_list(X),X=[P|L]},!,opb(P),clterms(L),cl(T). % reif
atomsent(T,X) --> {compound(X),X=..[P|L]},!,opb(P),clterms(L),cl(T).
atomsent(_T,X) --> {\+compound(X),safe(X,XS)},!,[XS].

boundlist(L) --> opb,vars(L),cl.

vars([V]) --> [V].
vars([V|VL]) --> [V],!,[' '],vars(VL).

clterms([V]) --> clterm(V).
clterms([V|VL]) --> clterm(V),!,[' '],clterms(VL).

clterm(X) --> clsentence(1,X).

name(X) --> [X].
names(L) --> vars(L).

quoted(X) --> {sformat(Q,'~q',[X])},[Q].

safe(X,X) :-
        atom_chars(X,['?'|Chars]),
        atom_chars(X2,Chars),
        safe(X2,X2).
safe(X,X) :-
        atom_chars(X,Chars),
        forall(member(C,Chars),
               (   C='_'
               ;   (   C @>= 'a', C @=< 'z')
               ;   (   C @>= 'A', C @=< 'Z')
               ;   (   C @>= '0', C @=< '9' % '0
                   )
               )),
        !.

safe(X,S) :-
        %sformat(S,'~q',[X]).
        sformat(S,'"~w"',[X]). % TODO

               
               
