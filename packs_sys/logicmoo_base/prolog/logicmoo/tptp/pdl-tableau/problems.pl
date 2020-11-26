%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2003-2005, Renate Schmidt,  University of Manchester
%           2009-2010, Ullrich Hustadt, University of Liverpool
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% problems(+ProblemClassId, +ProblemList)
%
%     Classification of problems


problems(routine, [
    1, 2, 3, 4, 5, 6, 7, 8, 9, 
    14, 15, 16, 17, 18, 19, 
    20, 22, 23, 24, 25, 26, 28, 29, 
    30, 31, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 44, 45, 46, 47, 48, 49,
    51, 52, 53, 54, 55, 56, 57, 58, 59,
    60, 61, 63, 64, 65, 66, 67, 68, 69,
    70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 88, 89, 
    90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
   100, 101, 102, 103, 104, 105, 106, 107
]).

% remove problems difficult for complement splitting rules:
problems(routine_cs, Set) :-
    problems(routine, Routine),
    list_difference(Routine, [
        41, 
        60, 61, 62, 63, 64
], Set).

% remove problems difficult for 3 way complement splitting rules:
problems(routine_cs3, Set) :-
    problems(routine_cs, Routine),
    list_difference(Routine, [
        21, 22, 51,
        67
], Set).

problems(routine_w_test, [
    43,
    50,
    71
]).

problems(difficult, [
    12, 
    27,
    32,
    62,
    86, 87
]).

problems(very_difficult, [
    10, 11, 13,
    21
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% problem(+ProblemId, +ExpectedResult, -Result)
%
%     Collection of problems together with ExpectedResult (satisfiable,
%     unsatisfiable or unknown)

% unsatisfiable
problem(1, unsatisfiable, Result) :- 
    satisfiable(
        not(
            equiv(box(comp(a,b),p), box(a,box(b,p)))
    ), Result).

% unsatisfiable
problem(2, unsatisfiable, Result) :- 
    satisfiable(
        not(
            implies(box(or(a,b),p), and(box(a,p), box(b,p)))
    ), Result).

% unsatisfiable
problem(3, unsatisfiable, Result) :- 
    satisfiable(not(
        implies(box(star(a),p), and(p, box(star(a),p)))
    ), Result).

% De Giacomo, Massacci (2000), p. 127, Fig. 6
% unsatisfiable
problem(4, unsatisfiable, Result) :- 
    satisfiable(and(dia(comp(b,star(a)),p),
            box(comp(star(b),star(a)), not(p))
    ), Result).

% De Giacomo, Massacci (2000), p. 127, Fig. 7
% satisfiable
problem(5, satisfiable, Result) :- 
    satisfiable(and(not(p), and(
            box(star(b),dia(b,not(p))),
    box(star(b),dia(star(a),p))
    )), Result).

problem(6, satisfiable, Result) :- 
    satisfiable(and(not(p), and(
            box(star(b),dia(b,not(p))),
    box(star(b),dia(star(a),not(p)))
    )), Result).

% Dima's example
problem(7, satisfiable, Result) :- 
    satisfiable(
        and(dia(star(comp(star(a),b)),p),
            box(star(comp(a,comp(star(a), b))), not(p)))
    , Result).

% Dima's example
problem(8, satisfiable, Result) :- 
    satisfiable(
        and(dia(star(comp(star(a),b)),p),
            box(star(comp(b,comp(star(b), star(a)))), not(p)))
    , Result).

problem(9, satisfiable, Result) :- 
    satisfiable(and(
        dia(b,dia(b,not(p))),dia(c,p)
    ), Result).

% Ullrich's example
problem(10, satisfiable, Result) :- 
    satisfiable(
        and(p2,
        and( or(or(p5, p3), p4),
        and( or(or(p5, not(p2)), not(p3)),  
        and( or(or(p1, p3), p4),
        and( or(or(not(p4), not(p5)), p3),  
        and( or(or(not(p3), not(p4)), not(p5)),  
        and( or(or(p3, p2), not(p1)),
        and( or(or(p2, not(p5)), not(p3)),  
        and( or(or(not(p1), p3), p4),
        and( or(or(not(p4), p2), not(p5)),  
        and( or(or(not(p2), not(p1)), not(p5)),
        and( or(or(not(p5), not(p1)), not(p2)),  
        and( or(or(not(p4), not(p2)), not(p3)),  
        and( or(or(not(p2), not(p3)), p4),
        and( or(or(not(p3), not(p5)), p1),
             or(or(not(p2), not(p1)), p4)
        ))))))))))))))), 
    Result).
        
% Ullrich's example
% note the close relationship to problem 10: problem 11 is a modalized
% version of problem 10 (except for the first conjunct of problem 10).
% unsatisfiable?
% Dima: unknown result; algorithm interrupted after 21 million proof steps
problem(11, unknown, Result) :- 
    satisfiable(
        and( box(star(r1), dia(r1, or(p0, not(p0)))),
        and( box(star(r1), or(or(box(r1,p5), box(r1,p3)), box(r1,p4))),
        and( box(star(r1), or(or(box(r1,p5), box(r1,not(p2))), box(r1,not(p3)))),  
        and( box(star(r1), or(or(box(r1,p1), box(r1,p3)), box(r1,p4))),
        and( box(star(r1), or(or(box(r1,not(p4)), box(r1,not(p5))), box(r1,p3))),  
        and( box(star(r1), or(or(box(r1,not(p3)), box(r1,not(p4))), box(r1,not(p5)))),  
        and( box(star(r1), or(or(box(r1,p3), box(r1,p2)), box(r1,(not(p1))))), 
        and( box(star(r1), or(or(box(r1,p2), box(r1,not(p5))), box(r1,not(p3)))),  
        and( box(star(r1), or(or(box(r1,not(p1)), box(r1,p3)), box(r1,p4))),
        and( box(star(r1), or(or(box(r1,not(p4)), box(r1,p2)), box(r1,not(p5)))),  
        and( box(star(r1), or(or(box(r1,not(p2)), box(r1,not(p1))), box(r1,not(p5)))),
        and( box(star(r1), or(or(box(r1,not(p5)), box(r1,not(p1))), box(r1,not(p2)))),  
        and( box(star(r1), or(or(box(r1,not(p4)), box(r1,not(p2))), box(r1,not(p3)))),  
        and( box(star(r1), or(or(box(r1,not(p2)), box(r1,not(p3))), box(r1,p4))),
        and( box(star(r1), or(or(box(r1,not(p3)), box(r1,not(p5))), box(r1,p1))),
        and( box(star(r1), or(or(box(r1,not(p2)), box(r1,not(p1))), box(r1,p4))),
        and( box(star(r1), or(not(p1), dia(star(r1),p2))),
        and( box(star(r1), or(not(p2), dia(star(r1),p3))),
        and( box(star(r1), or(not(p3), dia(star(r1),p4))),
        and( box(star(r1), or(not(p4), dia(star(r1),p5))),
             box(star(r1), or(not(p5), dia(star(r1),p1)))
        )))))))))))))))))))),
     Result).

% Dima's example
problem(12, unsatisfiable, Result) :- 
    satisfiable(
        and(box(star(star(comp(a,star(b)))),p),
                dia(star(comp(comp(star(a),a),star(b))),not(p))), 
    Result).

problem(13, unsatisfiable, Result) :- 
    satisfiable(
        and(box(star(star(comp(star(a),star(star(b))))),p),
            dia(star(comp(comp(star(star(a)),star(a)),star(star(b)))),not(p))
    ), Result).

% From Baader (1990), Example 3.6, p. 16.
% An example of a `good cycle'
problem(14, satisfiable, Result) :- 
    satisfiable(
        and(p, and(dia(a,p),
               box(plus(a), dia(a,p))))
    , Result).

% Another example of a `good cycle'
problem(15, satisfiable, Result) :- 
    satisfiable(
        and(p, and(dia(a,p),
               box(star(a), dia(a,p))))
    , Result).

% From Baader (1990), Example 3.8, p. 17.
% An example of a `bad cycle'
problem(16, unsatisfiable, Result) :- 
    satisfiable(
        and(not(p), and(dia(plus(a),p),
               box(plus(a), not(p))))
    , Result).

% A smaller example of a `bad cycle'
problem(17, unsatisfiable, Result) :- 
    satisfiable(
        and(dia(star(a),p),
            box(star(a),not(p)))
    , Result).

problem(18, satisfiable, Result) :- 
    satisfiable(
        and(dia(star(or(a,b)),p),
            box(star(a),not(p))
        )
    , Result).

problem(19, unsatisfiable, Result) :- 
    satisfiable(
        and(dia(star(b),p),
            box(star(or(a,b)),not(p))
    )
    , Result).

problem(20, unsatisfiable, Result) :- 
    satisfiable(
        and(dia(star(or(a,b)),p),
            box(star(b),and(dia(star(a),p),
                            box(star(a),not(p))))
    )
    , Result).

% Dima's example
% unsatisfiable
% test_free_pdl.pl: 7,378,999 inferences in 31.60 seconds (233513 Lips) (NNF version)
% pdl.pl:          13,387,034 inferences in 35.47 seconds (377418 Lips) (NNF version)
% dGM still working
problem(21, unsatisfiable, Result) :- 
    satisfiable(
        and(box(star(star(comp(star(or(star(or(a,c)),c)),star(b)))),p),
            dia(star(comp(comp(star(or(star(or(a,c)),c)),
                                       star(or(star(or(a,c)),c))),star(b))),
                not(p))), Result).

problem(22, unsatisfiable, Result) :- 
    satisfiable(
        and(box(star(or(star(or(a,c)),c)),p),
            dia(star(or(a,c)),not(p))
        ), Result).

problem(23, satisfiable, Result) :- 
    satisfiable(not(
            implies( 
                implies(dia(comp(x,y),p), dia(y,p)),
                implies(dia(comp(star(x),y),p), dia(y,p))
        ))
    , Result).

problem(24, satisfiable, Result) :- 
    satisfiable(not(
        implies( 
            equiv(dia(or(comp(x,y),y),p), dia(y,p)),
            equiv(dia(or(comp(star(x),y),y),q), dia(y,q))
        ))
    , Result).

% satisfiable
problem(25, satisfiable, Result) :- 
    satisfiable(not(
        implies( 
            equiv(box(or(comp(x,y),y),p), box(y,p)),
            equiv(box(or(comp(star(x),y),y),q), box(y,q))
        ))
    , Result).

problem(26, satisfiable, Result) :- 
    satisfiable([],not(
        equiv( 
            box(or(comp(x,y),y),p), 
            box(or(comp(star(x),y),y),p)
        ))
    , Result).

problem(27, satisfiable, Result) :- 
    satisfiable(
        [implies(dia(u,true), dia(x,true)),
         implies(dia(v,true), dia(y,true))],
        not(
            implies(dia(comp(u,v),true), dia(comp(x,y),true))
        )
, Result).

problem(28, satisfiable, Result) :- 
    satisfiable(
        [implies(dia(u,p1), dia(x,p1)),
         implies(dia(v,p2), dia(y,p2))],
        not(
            implies(dia(comp(u,v),q), dia(comp(x,y),q))
        )
, Result).

problem(29, satisfiable, Result) :- 
    satisfiable([],
        not(
        implies(and(
         implies(dia(u,p1), dia(x,p1)),
         implies(dia(v,p2), dia(y,p2))),
            implies(dia(comp(u,v),q), dia(comp(x,y),q))
        ))
, Result).

problem(30, satisfiable, Result) :- 
    satisfiable([],
        not(
        implies(and(
         equiv(dia(or(u,x),p1), dia(x,p1)),
         equiv(dia(or(v,y),p2), dia(y,p2))),
            equiv(dia(or(comp(u,v),comp(x,y)),q), dia(comp(x,y),q))
        ))
, Result).

problem(31, satisfiable, Result) :- 
    satisfiable(
        [equiv(dia(or(u,x),p1), dia(x,p1)),
         equiv(dia(or(v,y),p2), dia(y,p2))],
        not(
            equiv(dia(or(comp(u,v),comp(x,y)),q), dia(comp(x,y),q))
        )
    , Result).

problem(32, satisfiable, Result) :- 
    satisfiable(
        [implies(dia(u,true), dia(x,true)),
         implies(dia(v,true), dia(y,true))],
        not(
            implies(dia(comp(u,v),true), dia(comp(x,y),true))
        )
, Result).

problem(33, satisfiable, Result) :- 
    satisfiable(not(
        implies( and(
            implies(dia(u,p), dia(x,p)),
            implies(dia(v,p), dia(x,p))),
            implies(dia(comp(u,v),p), dia(comp(x,x),p))
        ))
, Result).

problem(34, unsatisfiable, Result) :- 
    satisfiable(
        [ implies(dia(u,p), p) ],
        not(
            implies(dia(u,p), dia(star(x),p))
        )
    , Result).

problem(35, unsatisfiable, Result) :- 
    satisfiable(
        [ implies(dia(u,p), dia(x,p)) ],
        not(
           implies(dia(u,p), dia(star(x),p))
        )
    , Result).

problem(36, unsatisfiable, Result) :- 
    satisfiable(
       [ implies(dia(u,true), dia(star(x),true)),
         implies(dia(v,true), dia(star(x),true))],
       not(
           implies(dia(comp(u,v),true), dia(star(x),true))
       )
, Result).

problem(37, unsatisfiable, Result) :- 
    satisfiable(
        [ implies(dia(u,p), dia(y,p)),
          implies(dia(comp(x,y),p), dia(y,p))],
        not(
            implies(dia(comp(star(x),u),p), dia(y,p))
        )
, Result).

problem(38, satisfiable, Result) :- 
    satisfiable([],
        not(
        implies(and(
          equiv(dia(or(u,y),p1), dia(y,p1)),
          equiv(dia(or(comp(x,y),y),p2), dia(y,p2))),
            equiv(dia(or(comp(star(x),y),u),q), dia(y,q))
        ))
, Result).

problem(39, unsatisfiable, Result) :- 
    satisfiable(
        [ implies(dia(u,true), dia(y,true)),
           implies(dia(comp(y,x),true), dia(y,true))],
        not(
           implies(dia(comp(u,star(x)),true), dia(y,true))
        )
, Result).

problem(40, unsatisfiable, Result) :- 
    satisfiable(
        [ implies(dia(x,true), dia(u,true)),
          implies(dia(u,true), dia(y,true))],
        not(
           implies(dia(x,true), dia(y,true))
        )
, Result).

% (a + b)* = (a*;b*)*, Kracht
% Jan 2006: equivalent reduction
problem(41, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(star(or(a,b)),p),
            dia(star(comp(star(a),star(b))),p)
        ))
, Result).

% (a;b)* = skip + a;(b;a)*;b
% Jan 2006: equivalent reduction
problem(42, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(star(comp(a,b)),p),
            or(p,
               dia(comp(a,comp(star(comp(b,a)),b)),p)
            )
        ))
, Result).

% (p?)* = skip
% Jan 2006: equivalent reduction
problem(43, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            q,
            dia(star(test(p)),q)
        ))
, Result).

% [(a^2)+]p & [(a^3)+]-p . -> -<a^6>T
problem(44, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        implies(
            and(
                box(plus(comp(a,a)),p),
                box(plus(comp(a,comp(a,a))),not(p))
            ),
            not( dia(comp(a,comp(a,comp(a,comp(a,comp(a,a))))),true) )
        ))
, Result).

% <(a + b)*>p & [a*]-p
problem(45, satisfiable, Result) :- 
    satisfiable([],
        and(
            dia(star(or(a,b)),p),
            box(star(a),not(p))
        )
, Result).

problem(45, satisfiable, Result) :- 
    satisfiable([],
        and(
            dia(star(or(b,a)),p),
            box(star(a),not(p))
        )
, Result).

% <(a*;b*)*>p & [a*]-p
problem(46, satisfiable, Result) :- 
    satisfiable([],
        and(
            dia(star(comp(star(a),star(b))),p),
            box(star(a),not(p))
        )
, Result).

% <(a*;b*)*>p & [a*]-p
problem('46a', satisfiable, Result) :- 
    satisfiable([],
        and(
            dia(star(comp(star(b),star(a))),p),
            box(star(a),not(p))
        )
, Result).

problem(47, satisfiable, Result) :- 
    satisfiable([],
        and(
            dia(star(comp(star(b),star(a))),p),
            box(star(a),not(p))
        )
, Result).

problem(48, satisfiable, Result) :- 
    satisfiable([],
        and(
            or(dia(star(a),p),
               dia(star(b),p)),
            box(star(a),not(p))
        )
, Result).

% Dima/Ullrich's example
problem(49, satisfiable, Result) :- 
    satisfiable([],
        and(
            box(star(or(a,b)), dia(plus(comp(a,b)),p)),
            box(star(or(a,b)), dia(plus(comp(b,a)),not(p)))
        )
, Result).

problem(50, unsatisfiable, Result) :- 
    satisfiable([],
        and(not(p),
            dia(star(or(test(p),test(not(p)))),p)
        )
    , Result).

problem(51, satisfiable, Result) :- 
    satisfiable(
        and(and(
            dia(star(or(star(a),star(b))),p),
            box(star(a),not(p))),
            box(star(b),not(p)))
    , Result).

problem(52, satisfiable, Result) :- 
    satisfiable(
        and(and(
            dia(star(or(a,b)),p),
            box(star(a),not(p))),
            box(star(b),not(p)))
    , Result).

problem(53, satisfiable, Result) :- 
    satisfiable(
        and(and(
            dia(star(comp(star(a),star(b))),p),
            box(star(a),not(p))),
            box(star(b),not(p)))
    , Result).

problem(54, satisfiable, Result) :- 
    satisfiable(
        and(dia(star(or(a,b)),p),
            box(star(a),
               or(and(dia(star(a),p), 
                      box(star(a),not(p))), 
                  not(p)))
        )
    , Result).

problem(55, satisfiable, Result) :- 
    satisfiable(
        and(dia(star(or(a,b)),p),
            box(star(a),
               or(not(p),
                  and(dia(star(a),p), 
                      box(star(a),not(p)))
               ))
        )
    , Result).

problem(56, satisfiable, Result) :- 
    satisfiable(
        and(dia(star(or(a,b)),p),
            box(star(a),
               or(and(dia(star(c),p), 
                      box(star(c),not(p))), 
                  not(p)))
        )
    , Result).

problem(57, satisfiable, Result) :- 
    satisfiable(
        and(dia(star(or(a,b)),p),
            box(star(a),
               or(and(p, 
                      not(p)), 
                  not(p)))
        )
    , Result).

% a+ = a;a*
% Jan 2006: equivalent reduction
problem(58, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(plus(a),p),
            dia(comp(a,star(a)),p)
        ))
, Result).

% a* = id + a+
% Jan 2006: equivalent reduction
problem(59, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(star(a),p),
            dia(or(id,star(a)),p)
        ))
, Result).

% (a + b)* = a*;(b;a*)*
% Jan 2006: equivalent reduction
problem(60, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(star(or(a,b)),p),
            dia(comp(star(a),star(comp(b,star(a)))),p)
        ))
, Result).

% (a + b)* = (a*;b)*;a*
% Jan 2006: equivalent reduction
problem(61, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(star(or(a,b)),p),
            dia(comp(star(comp(star(a),b)),star(a)),p)
        ))
, Result).

% a + b;(a + b)*;a = (b+;a*)*;a
% From Dima: R+(Q;(R+Q)*;R) is equivalent to (Q+;R*)*;R
% The proof has length 4831 (NNF version), 3632/3885 (new dGM_nd/dGM version)
% Jan 2006: equivalent reduction
problem(62, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(or(a,comp(comp(b,star(or(a,b))),a)),p),
            dia(comp(star(comp(plus(b),star(a))),a),p)
        ))
, Result).

% S;(S+;R)+ = S;S+;R + S;S+;R;(S+;R)+
% Jan 2006: equivalent reduction
problem(63, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(comp(s,plus(comp(plus(s),r))),p),
            dia(or(comp(comp(s,plus(s)),r),
                   comp(comp(comp(s,plus(s)),r),plus(comp(plus(s),r)))),p)
        ))
, Result).

% (S;R+)+;R = S;R+;R + (S;R+)+;S;R+;R
% Jan 2006: equivalent reduction
problem(64, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(comp(plus(comp(s,plus(r))),r),p),
            dia(or(comp(comp(s,plus(r)),r),
                   comp(comp(comp(plus(comp(s,plus(r))),s),plus(r)),r)),p)
        ))
, Result).

% R+;R+ = R;R*;R
% Jan 2006: equivalent reduction
problem(65, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        implies(
            dia(comp(plus(r),plus(r)), p),
            dia(comp(comp(r,star(r)),r), p)
        ))
, Result).

% Dima's example
problem(66, satisfiable, Result) :- 
    satisfiable([],
        and(dia(star(comp(star(a),b)), p),
            not(dia(star(comp(comp(b,star(b)),star(a))), p))
        )
, Result).

% Transscribed example from Alberucci & Jaeger
problem(67, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        implies(
            and(box(a,and(p,box(plus(or(a,b)),q))), 
                box(b,and(q,box(plus(or(a,b)),p)))), 
            box(plus(or(a,b)),or(p,q))
        ))
, Result).

% Negation of 67
problem(68, satisfiable, Result) :- 
    satisfiable([],
        implies(
            and(box(a,and(p,box(plus(or(a,b)),q))), 
                box(b,and(q,box(plus(or(a,b)),p)))), 
            box(plus(or(a,b)),or(p,q))
        )
, Result).

% From Alberucci, PhD, p. 96
problem(69, satisfiable, Result) :- 
    satisfiable([],
        not(
        implies(
            box(a,and(p,box(plus(or(a,b)),q))), 
            box(or(a,b), box(a,and(p,box(plus(or(a,b)),q))))
        ))
, Result).

problem(70, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(q,
            dia(star(id),q)
        ))
    , Result).

problem(71, unsatisfiable, Result) :- 
    satisfiable([],
        not(
        implies(q,
            dia(test(true),q)
        ))
    , Result).

% S;(S+;R)+ = S;S+;R + S;S+;R;T
problem(72, satisfiable, Result) :- 
    satisfiable([],
        not(
        equiv(
            dia(comp(s,plus(comp(plus(s),r))),p),
            dia(or(comp(comp(s,plus(s)),r),
                   comp(comp(comp(s,plus(s)),r),t)),p)
        ))
, Result).

% (S;R+)+;R = S;R+;R + T;S;R+;R
problem(73, satisfiable, Result) :- 	% check !!
    satisfiable([],
        not(
        equiv(
            dia(comp(plus(comp(s,plus(r))),r),p),
            dia(or(comp(comp(s,plus(r)),r),
                   comp(comp(comp(t,s),plus(r)),r)),p)
        ))
, Result).

% 1+a;a* <= a*
% Jan 2006: equivalent reduction
problem(74, unsatisfiable, Result) :-
    satisfiable([],
        not(
            implies(
                dia(or(test(true),comp(a,star(a))), p),
                dia(star(a),p))
        )
, Result).

% 1+a*;a <= a*
% Jan 2006: equivalent reduction
problem(75, unsatisfiable, Result) :-
    satisfiable([],
        not(
            implies(
                dia(or(test(true),comp(star(a),a)), p),
                dia(star(a),p))
        )
, Result).

% a;c+b <= c implies a*;b <= c
% equivalent to problem 37
problem(76, unsatisfiable, Result) :- 
    satisfiable([
            implies(
                dia(or(comp(a,c),b), true),
                dia(c,true))
        ],
        not(
            implies(
                dia(comp(star(a),b), true),
                dia(c,true))
        )
, Result).

% c;a+b <= c implies b;a* <= c
% equivalent to problem 39
% Jan 2006: not equivalent
problem(77, unsatisfiable, Result) :-
    satisfiable([
            implies(
                dia(or(comp(c,a),b), true),
                dia(c,true))
        ],
        not(
            implies(
                dia(comp(b,star(a)), true),
                dia(c,true))
        )
, Result).

problem(78, satisfiable, Result) :-
    satisfiable([
            implies(
                dia(or(comp(c,a),b), p),
                dia(c,p))
        ],
        not(
            implies(
                dia(comp(b,star(a)), p),
                dia(c,p))
        )
, Result).

% p;q = 0 implies (p + q)*;1 <= q*;p*
% Jan 2006: equivalent reduction
problem(79, unsatisfiable, Result) :-
    satisfiable([
            not(
                dia(comp(p,q),true))
        ],
        not(
            implies(
                dia(comp(star(or(p,q)),test(true)), y),
                dia(comp(star(q),star(p)),y))
        )
, Result).

% p;(q;p)* = (p;q)*;p
% Jan 2006: equivalent reduction
problem(80, unsatisfiable, Result) :-
    satisfiable([],
        not(
            equiv(
                dia(comp(p,star(comp(q,p))),x),
                dia(comp(star(comp(p,q)),p),x))
        )
, Result).

% s + uru = t + uru  implies  s* + uru = t* + uru  
% Jan 2006: not equivalent
problem(81, unsatisfiable, Result) :-
    satisfiable([
            equiv(
                dia(or(s,comp(u,comp(r,u))),true),
                dia(or(t,comp(u,comp(r,u))),true)
            )
        ],
        not(
            equiv(
                dia(or(star(s),comp(u,comp(r,u))),true),
                dia(or(star(t),comp(u,comp(r,u))),true))
        )
, Result).

% b(d*ca*b)*d*ca* <= (a*bd*c)*a*
problem(82, unsatisfiable, Result) :-
    satisfiable([],
        not(
            implies(
                dia(comp(b,comp(star(comp(star(d),comp(c,comp(star(a),b)))),comp(star(d),comp(c,star(a))))),true),
                dia(comp(star(comp(star(a),comp(b,comp(star(d),c)))),star(a)),true))
        )
, Result).

% b(d*ca*b)*d*ca* <= (a*bd*c)*a*
% equivalent variation of problem 82
problem(83, unsatisfiable, Result) :-
    satisfiable([],
        not(
            implies(
                dia(comp(b,comp(star(comp(star(d),comp(c,comp(star(a),b)))),comp(star(d),comp(c,star(a))))),p),
                dia(comp(star(comp(star(a),comp(b,comp(star(d),c)))),star(a)),p))
        )
, Result).

% b(d*ca*b)*d* <= (a*bd*c)*a*bd*
problem(84, unsatisfiable, Result) :-
    satisfiable([],
        not(
            implies(
                dia(comp(b,comp(star(comp(star(d),comp(c,comp(star(a),b)))),star(d))),true),
                dia(comp(star(comp(star(a),comp(b,comp(star(d),c)))),comp(star(a),comp(b,star(d)))),true))
        )
, Result).

% b(d*ca*b)*d* <= (a*bd*c)*a*bd*
% equivalent variation of problem 84
problem(85, unsatisfiable, Result) :-
    satisfiable([],
        not(
            implies(
                dia(comp(b,comp(star(comp(star(d),comp(c,comp(star(a),b)))),star(d))),p),
                dia(comp(star(comp(star(a),comp(b,comp(star(d),c)))),comp(star(a),comp(b,star(d)))),p))
        )
, Result).

% ax + by <= x and cx + dy <= y  implies  (a + bd*c)*x + (a + bd*c)*bd*y <= x
problem(86, unsatisfiable, Result) :-
    satisfiable([
            implies(
                dia(or(comp(a,x),comp(b,y)),true),
                dia(x,true)),
            implies(
                dia(or(comp(c,x),comp(d,y)),true),
                dia(y,true))
        ],
        not(
            implies(
                dia(or(
                    comp(star(or(a,comp(b,comp(star(d),c)))),x),
                    comp(star(or(a,comp(b,comp(star(d),c)))),comp(b,comp(star(d),y)))), true),
                dia(x,true))
        )
, Result).

% ax + by <= x and cx + dy <= y  implies  (d + ca*b)*y + (d + ca*b)*ca*x <= y
% check equivalence
problem(87, unsatisfiable, Result) :-
    satisfiable([
            implies(
                dia(or(comp(a,x),comp(b,y)),true),
                dia(x,true)),
            implies(
                dia(or(comp(c,x),comp(d,y)),true),
                dia(y,true))
        ],
        not(
            implies(
                dia(or(
                    comp(star(or(d,comp(c,comp(star(a),b)))),y),
                    comp(star(or(d,comp(c,comp(star(a),b)))),comp(c,comp(star(a),x)))), true),
                dia(y,true))
        )
, Result).

problem(88, unsatisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(a),and(not(p),and(not(q),or(p,q)))),
        and(dia(a,dia(star(a),and(not(p),and(not(q),or(p,q))))), 
            box(star(a),dia(a,dia(star(a),and(not(p),and(not(q),or(p,q))))))))
, Result).

problem(89, satisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(star(a)),p),
            not(p)
        )
, Result).


problem(90, unsatisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(star(a)),dia(star(a),p)),
            not(dia(star(a),p))
        )
, Result).

problem(91, satisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(star(a)),and(p,q)),
            not(and(p,q))
        )
, Result).

problem(92, satisfiable, Result) :-
    satisfiable([],
        dia(a,and(
            dia(star(star(a)),p),
            not(p)
        ))
, Result).

problem(93, unsatisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(star(a)),dia(star(a),dia(a,p))),
            not(dia(star(a),p))
        )
, Result).


problem(94, unsatisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(a),dia(a,p)),
            not(dia(a,dia(star(a),p)))
        )
, Result).

problem(95, unsatisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(a),dia(a,p)),
            not(dia(a,dia(star(star(a)),p)))
        )
, Result).


problem(96, unsatisfiable, Result) :-
    satisfiable([],
        and(and(
	    q,
            not(dia(star(a),q))),
            dia(star(star(a)),or(dia(star(a),q),not(q)))
        )
, Result).

% Variations of 41
problem(97, unsatisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(comp(star(a),star(b))), p),
            not(dia(star(or(a,b)),dia(star(a),p)))
        )
, Result).


problem(98, satisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(comp(star(a),star(b))), p),
            not(dia(star(or(a,b)),dia(a,p)))
        )
, Result).


problem(99, satisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(comp(star(a),star(b))), p),
            not(dia(star(or(a,comp(b,a))),p))
        )
, Result).


problem(100, satisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(comp(star(a),star(b))), p),
            not(dia(star(or(a,comp(comp(star(b),b),comp(star(a),a)))),p))
        )
, Result).

% same as 100, but a and b switched in 1st conjunct -> many more steps
problem(101, satisfiable, Result) :-
    satisfiable([],
        and(
            dia(star(comp(star(b),star(a))), p),
            not(dia(star(or(a,comp(comp(star(b),b),comp(star(a),a)))),p))
        )
, Result).

problem(102, unsatisfiable, Result) :-
    satisfiable([],
        and(
            not(dia(star(or(a,b)),dia(star(a),p))),
            dia(star(a),dia(star(a), p))
        )
, Result).


problem(103, unsatisfiable, Result) :-
    satisfiable([],
        and(and(
            dia(star(a),p),
            not(p)),
            box(a,not(dia(star(a),p)))
        )
, Result).

problem(104, satisfiable, Result) :-
    satisfiable([],
        dia(a,or(
                 dia(b,and(and(and(or(p,q),or(not(p),q)),
                               or(p,not(q))),or(not(p),not(q)))),
	         dia(c,and(and(and(or(p,q),or(not(p),q)),
                               or(p,not(q))),or(not(p), or(not(q), r)))))),
          Result).

% Problem 105 is due to Florian Widmann
% ignorability and the formation of inconsistent sets
problem(105, satisfiable, Result):-
    satisfiable([],
      and(box(star(a),dia(a,box(c,not(p)))),
          and(box(c,not(p)),box(star(a),dia(star(b),dia(c,p))))), Result).

% Problem 106 is due to Florian Widmann
% problems with the definition of 'fulfilled'
problem(106, unsatisfiable, Result) :-
    satisfiable(and(dia(star(comp(a,a)),p),
                and(dia(a,dia(star(comp(a,a)),p)),
                    box(a,box(star(comp(a,a)),and(not(p),q))))), Result).

% Problem 107 is due to Florian Widmann
% caching of consistent sets is unsound
problem(107, unsatisfiable, Result) :-
    satisfiable(or(dia(a,box(star(comp(b,b)),and(p,and(dia(c,and(p,and(q,not(p)))),dia(b,dia(b,p)))))),
                   and(dia(a,dia(b,p)),
                       box(a,box(b,box(star(comp(b,b)),and(p,and(dia(c,and(p,and(q,not(p)))),dia(b,dia(b,p))))))))), Result).


