```text `
                      SIMULATION OF MULTI-AGENT TUTORING
            Written by Thomas Hoppe, Technical University of Berlin
                                      and
                     Yiu Cheung Ho, King''s College London.


MULTAGNT is a simulation of error detection and identification in a
small tutoring setting, consisting of a Teacher and a Learner. The
objective of MULTAGNT is to show, how through meta-logical knowledge the
Teacher can detect and identify deficiencies in the Learner''s knowledge.
The meta-logical knowledge used consists of predicates:                   

    - can_do, for determining what a theory of an agent can prove
    - cannot_do, for determining what a theory of an agent cannot
      prove
    - demo_trace_2, for determining the erroneous clause in an
      agent''s theory

which are used by an agent (usually the Teacher) to judge the
knowledge of another agent (refered as Learner). Agent''s are
represented by sets of theories, where a theory is a set of clauses.

In this setting the Teacher''s knowledge is assumed to be correct.
Thus, it can be used as reference knowledge, for generating question
posed to the Learner and to judge the knowledge of the Learner.


SIZE:  41 kilobytes.                  
                   

INTERNAL DOCUMENTATION : Predicates are commented with a statement of
their purpose. However, you will probably also need to read the report
by Brazdil quoted in MULTAGNT.PL.


This entry consists of the following files:

    README.MD - this one.

    CALLS_1.PL   - two driver files.
    CALLS_2.PL

    LEARNER1.PL  - the (possibly faulty) knowledge of two learners.
    LEARNER2.PL    learner1 is correct; learner2 is not.

    MULTAGNT.PL  - the main simulation.

    TEACHER.PL   - the teacher''s knowledge of various topics.


I think the main predicate is in MULTAGNT.PL, "diagnose". However, I
haven''t had time to examine the software.          


CHECKED ON EDINBURGH-COMPATIBLE (POPLOG) PROLOG : no.

PORTABILITY : Possible problems include
- the declaration of dynamic clauses in some Prolog dialects;
- different operator declarations; and
- you may need to write some trivial predicates (e.g. member and
  append).                   

```
