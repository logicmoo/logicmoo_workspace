/* -*- Mode: Prolog -*- */

:- use_module(clif_parser).

:- begin_tests(parse,[]).

:- style_check(-atom).

testtext('(cl-comment \'hi\' (a x))').
testtext('(forall (x) (a x))').
testtext('(forall (x) (= x x))').
testtext('(forall (?x ?y ?z)
   (if (and (part_of ?x ?z)
            (part_of ?z ?y))
       (part_of ?x ?y)))').
testtext('
(forall (?r)
   (if (transitive ?r)
        (forall (?x ?y ?z)
        (if (and (?r ?x ?z)
                 (?r ?z ?y))
            (?r ?x ?y)))))').
testtext('
(cl-comment \'axiom\'
(forall (?x ?y ?z)
  (if (and (part_of ?x ?z)
           (part_of ?z ?y))
      (part_of ?x ?y))))').
testtext('
(cl-text axiom:1234
(forall (?x ?y ?z)
  (if (and (part_of ?x ?z)
           (part_of ?z ?y))
      (part_of ?x ?y))))').
%(cl-imports axiom:1234)').
testtext('(cl-imports axiom:1234)').
testtext('(cl-text axiom:1234 (a b))').
%testtext('(cl-text axiom:1234 (a b)) (cl-imports axiom:1234)').
testtext('
(cl-module World
	
(forall (x)(wldr x x))
	
(forall (x y)(if (wldr x y)(wldr y x)))
	
(forall (x y z)(if (and (wldr x y)(wldr y z))(wldr x z)))
	
) ').

testtext('(forall ex:romanceNovel ((x man)) (exists ((y woman)) 
   (and (loves x y) (not (loves y x))) ))
').
testtext('(cl-text text1 (cl-text text2 (a b)) (cl-text text2 (a b)))').
testtext('(cl-module m1 (cl-text text2 (a)))').
testtext('(cl-module m1 (cl-text text2 (a b)) (cl-text text2 (a b)))'). % CHECK: should this be permitted?
testtext('(cl-comment "abc" (cl-module m1 (cl-text text2 (a b)) (cl-text text2 (a b))))').
testtext('(exists (s Situation) (and (PTim s "20 January 2008") (Dscr s (that p))))').
testtext('(istAtTime 1998 (that (Married x y)))').
testtext('(forall (x y i)(and
   (= x (begin (from x y)))
   (= y (end (from x y)))
   (= i (from (begin i)(end i)))
))').
testtext('(forall (r p n)
        (if
         (and (owl:onProperty r p) 
              (owl:minCardinality r n)) 
         (forall (x)(iff 
                     (r x) 
                     (exists ((L NOREPEATSLIST))
                             (and 
                              (forall (y)(if (member y L)(p x y) ))
                              (lesseq n (length L))
                              ))))))').

testtext('(= abc (intersectionOf a (intersectionOf b c)))').

test(parse) :-
        findall(A,
                (   testtext(A),
                    format('~nParsing: ~w~n',[A]),
                    (   atom_cltext(A,T)
                    ->  format('PASS: ~w~n',[T])
                    ;   format(user_error,'FAIL: ~w~n',[A]),
                        fail)),
                FailedAtoms),
        (   FailedAtoms=[]
        ->  format('all parsed~n',[])
        ;   format('Failed: ~w~n',[FailedAtoms]),
            fail).

:- end_tests(parse).
