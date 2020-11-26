# yadlr

yadlr is multi-valued reasoning system written in Prolog.

At its current state of development, yadlr only provides ABox services
over the SHOIQ Description Logic, although there is no DL-like front
end and atleast, atmost constructs have to be translated using the
dlalldifferent built-in concept, where the "atom" is a list of variables.

Emphasis is given to coupling yadlr to a ILP systems in order to
evaluate multi-valued DL hypotheses, so there is no plan for providing
TBox services in the forseeable future.


## Prolog APIs

yadlr provides two Prolog front-ends, yadlr and prodlr. Although the
intension is to eventually allow all front-end and inference engine
combinations, currently the yadlr can be used with the resolution and
Prolog engine, and prodlr with the Prolog engine.

Before loading either of the front-end modules, use_inference_engine/1
and use_algebra/1 must be asserted in the user namespace.

yadlr.pl provides the following predicates:

```prolog
    yadlr_init( +KB )

    yadlr_concept( +KB, +ConceptName )
    yadlr_relation( +KB, +RelationName )
    yadlr_instance( +KB, +InstanceName )

    yadlr_assert( +KB, +Formula, +Degree )

    check_membership( +KB, +InstanceName, +ConceptName, +Degree, -Restrictions )
    check_types( +KB, +InstanceName, +Degree, ?ConceptNames, -Restrictions )
    check_members( +KB, +ConceptName, +Degree, ?InstanceNames, -Restrictions )

    set_proof_tree_log( +Filename|no )
    unset_proof_tree_log

    set_depth_limit( +YESNO )
```

prodlr.pl provides the following predicates:

```prolog
    declare_concept( +ConceptName, +SuperConceptName )
    assert_instance( +ConceptName, +InstanceName, +Degree )
    add_to_concept( +ConceptName, [ (+InstanceName, +Degree) ] )

    declare_relation( +RelationName, +DomainConceptName, +RangeConceptName )
    add_to_relation( +RelationName, +InstanceName, [ (+FillerInstanceName, +Degree) ] )

    concept_select( +InList, ?ConceptName, -OutList )
    forall_select( +InList, +RelationName, +ConceptName, -OutList )
    atleast_select( +InList, ?RelationName, ?ConceptName, +Min, -OutList )
    atmost_select( +InList, ?RelationName, ?ConceptName, +Max, -OutList )
    self_select( +InList, ?RelationName, -OutList )
```

## Formula Syntax

S is the start symbol. F is a formula non-terminal symbol. A is an
atomic non-terminal symbol. <whatever> are terminal symbols.

    S :== all(X,F)
    F :== all(X,F)
    F :== exists(X,F)
    F :== dlnot(F)
    F :== dland(F, F)
    F :== dlor(F, F)
    F :== dlimplies(F, F)
    F :== dlequiv(F, F)
    F :== C(A)
    F :== R(A, A)
    F :== dlalldifferent( [X1..Xn] )
    A :== <Instance> | X
    C :== <Concept Name>
    R :== <Relation Name>
    X :== <Variable>


## Proof Tree Log

By declaring
    :- use_proof_tree_log( yes ).
each proof will print on standard output all the derivations made during
the proof. The tree representation can be retrieved from this log by
keeping track of the depth of the derivation.

STEP-TYPE is either i (initial), mp (modus-ponens), or l (leaf).

Line syntax:

    LINE :== DEPTH ( STEP-TYPE ) ( NUM-RESTRS ) CLAUSE-LIST
    DEPTH :== <Integer>
    STEP-TYPE :== i || mp || l 
    NUM-RESTRS :== NUM-RESTR NUM-RESTRS
    NUM-RESTR :== <Algebraic Expression involving d,x>
    CLAUSE-LIST :== [ CLAUSE | CLAUSE-LIST ]
    CLAUSE :== yclause( <Positive Literal List>, <Negative Lit List>, <Fuzzy Degree> )

## Known Problems

Complementization doesn't take into account complement relationships between
"symmetric" number restrictions and quantifiers. This is going to be tricky
to fix, I see trouble ahead.

