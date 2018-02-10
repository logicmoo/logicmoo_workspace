
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparqlprog/emulate_builtins)).

:- rdf_register_prefix(foaf,'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix('','http://example.org/').

:- begin_tests(bultins_test).


% string test functions should accept as input: str, atom, lang-literal, str-literal
test(str_starts1) :- assertion(str_starts("abc","ab")).
test(str_starts2) :- assertion(str_starts("abc"@en,"ab")).
test(str_starts3) :- assertion(str_starts("abc"^^xsd:string,"ab")).
test(str_starts4) :- assertion(str_starts(abc,"ab")).
test(str_starts5) :- assertion(str_starts(abc,ab)).
test(str_starts6) :- assertion(str_starts("abc",ab)).

test(str_ends) :- assertion(str_ends("abc","bc")).

test(str_replace) :- assertion(str_replace("abc","ab","x","xc")).
test(str_replace_eval) :-
        seval(str_replace("ABC DEF GHI"," ","X"),S),
        assertion(S == "ABCXDEFXGHI").

test(lcase1) :- assertion(lcase("ABC","abc")).
test(lcase2) :-
        assertion(lcase("ABC",X)),
        assertion(X="abc").
test(lcase_eval) :-
        seval(lcase("AbC"),S),
        assertion(S == "abc").

test(seval1) :-
        assertion( seval(append([a,b],[c,d]),[a,b,c,d]) ).
test(seval_nested) :-
        seval(lcase(str_replace("ABC DEF GHI"," ","X")),S),
        assertion(S == "abcxdefxghi").

test(arith1) :-
        assertion( seval(1+2,3) ).
test(arith_nested) :-
        assertion( seval(1+(2*3),7) ).

test(arith_types) :-
        One="1"^^xsd:integer,
        Two="2",
        assertion( seval(One+Two,3) ).


        

    
:- end_tests(bultins_test).
