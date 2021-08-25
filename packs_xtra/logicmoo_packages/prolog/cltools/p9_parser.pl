/* -*- Mode: Prolog -*- */

:- module(p9_parser,
          [
           ]).

:- use_module(cl_io).
:- use_module(cl_transform).

:- multifile cl_io:parse_cltext_hook/4.
cl_io:parse_cltext_hook(File,prover9,Text,Opts) :-
        file_to_cltext(File,Text,Opts).

file_to_cltext(File,Text,Opts) :-
        read_file_to_codes(File,Codes,[]),
        codes_cltext(Codes,Text).

atom_cltext(A,Text):-
	atom_codes(A,Codes),
	codes_cltext(Codes,Text).
codes_cltext(Codes,Text):-
	(   lexicaltokens(Tokens,Codes,[])
	->  true
	;   atom_codes(A,Codes),
	    throw(cannot_tokenize(A))),
	remove_white(Tokens,Tokens2), % TODO - should not be necessary
	debug(commonlogic,'Tokens=~w',[Tokens2]),
	cltext(Text,Tokens2,[]).


remove_white([],[]).
remove_white([white|L],L2):- !,remove_white(L,L2).
remove_white([H|L],[H|L2]):- !,remove_white(L,L2).



jop(and).
jop(or).

nl --> ['\n'].
dot --> ['.\n'].

whitespace --> "%",!,ignoreline.
whitespace --> " ",!.
whitespace --> "\n",!.
        
ignoreline --> "\n", !.
ignoreline --> [_],ignoreline.


clcomment(Fmt,X) --> ['% '],{sformat(A,Fmt,X)},[A],nl.  % TODO - newlines
clcomment(X) --> ['% '],{sformat(A,'~q',[X])},[A],nl.  % TODO - newlines

cltext([]) --> !.
cltext([H|L]) --> !,cltext(H),cltext(L).


% quantified sentences
axiom(BoundList,forall(X,Y)) --> !,varx(all,X),[' '],{append(X,BoundList,NewBoundList)},brac(NewBoundList,Y),dot.
axiom(BoundList,exists(X,Y)) --> !,varx(exists,X),[' '],{append(X,BoundList,NewBoundList)},brac(NewBoundList,Y),dot.
axiom(BoundList,X) --> exprs(BoundList,X),dot.

brac(BoundList,X) --> !,['('],exprs(BoundList,X),[')'].

exprs(_BoundList,[]) --> !.
exprs(BoundList,[H|L]) --> !,exprs(BoundList,H),exprs(BoundList,L).

exprs(BoundList,iff(X,Y)) --> !,['('],exprs(BoundList,X),[' <-> '],exprs(BoundList,Y),[')'].
exprs(BoundList,if(X,Y)) --> !,['('],exprs(BoundList,X),[' -> '],exprs(BoundList,Y),[')'].
exprs(BoundList,'='(X,Y)) --> !,['('],exprs(BoundList,X),[' = '],exprs(BoundList,Y),[')'].
exprs(BoundList,not(X)) --> !,['-'],brac(BoundList,X).
exprs(BoundList,X) --> {X=..[Op|L],jop(Op)},!,exprj(BoundList,Op,L).
exprs(BoundList,X) --> {compound(X),X=..[P|L]},!,predsym(P,L),['('],exprj(BoundList,',',L),[')'].
exprs(BoundList,X) --> {member(X,BoundList),var_p9(X,V)},!,[V].
exprs(_BoundList,X) --> {safe_atom(X,A)},!,[A].

exprs(BoundList,[H]) --> !,exprs(BoundList,H).
exprs(BoundList,[H|T]) --> exprs(BoundList,H),[','],!,exprs(BoundList,T).

varx(Q,L) --> {is_list(L)},!,vars(Q,L).
varx(Q,X) --> {X=..L},!,vars(Q,L). % vars get parsed as terms..
vars(_,[]) --> [].
vars(Q,[H|T]) --> [' ',Q,' '],{var_p9(H,V)},[V],vars(Q,T).

% always use variable arity
%predsym(P,L) --> {length(L,N)},!,[P],[N].
%predsym(P,L) --> {variable_arity(P),length(L,N)},!,[P],[N].
predsym(P,_) --> [P].

exprj(BoundList,_Op,[H]) --> !,exprs(BoundList,H).
exprj(BoundList,Op,[H|T]) --> !,exprs(BoundList,H),opx(Op),exprj(BoundList,Op,T).

opx(and) --> [' & '].
opx(or) --> [' | '].
opx(',') --> [', '].

var_p9(A,V):-
        atom_concat('?',A1,A),
        !,
        var_p9(A1,V).

var_p9(A,V):-
        sub_atom(A,0,1,_,Ch),
        (   Ch @>= 'a',
            Ch @=< 'z'
        ->  V=A
        ;   downcase_atom(A,V)).

safe_atom(A,A):-
        atom_chars(A,[C1|L]),
        C1 @>= 'A',
        C1 @=< 'Z',
        forall(member(C,L),
               safe_char(C)),
        !.

safe_atom(A,Safe):-
        concat_atom(Toks,'"',A),
        concat_atom(Toks,'\\"',A2),
        sformat(Safe,'"~w"',[A2]).
        
safe_char(C):- is_alpha(C).
safe_char('_').
safe_char(C):- C @>= '0', C @=< '9'.


