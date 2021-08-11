# Using monotonic tabling for RDFS entailment reasoning

SWI-Prolog got a new form of tabling some time ago: _monotonic tabling_.
That remained in a rather experimental state for quite a while. It is
now picked up again to deal with reasoning about C++ binary code
together with CMU in the [Pharos](https://github.com/cmu-sei/pharos)
project.

To show the potential of this type of reasoning I wrote an RDFS
entailment reasoner. We first translate the [RDFS entailment
rules](https://www.researchgate.net/publication/268419911_Towards_Efficient_Schema-Enhanced_Pattern_Matching_over_RDF_Data_Streams/figures?lo=1)
to Prolog in the most straight forward way:

```
rdfs(P, rdf:type, rdf:'Property') :-                            % rdf1
    rdfs(_, P, _).
rdfs(X, rdf:type, C) :-                                         % rdfs2
    rdfs(P, rdfs:domain, C),
    rdfs(X, P, _).
rdfs(Y, rdf:type, C) :-                                         % rdfs3
    rdfs(P, rdfs:range, C),
    rdfs(_, P, Y).
rdfs(X, rdf:type, rdfs:'Resource') :-                           % rdfs4a
    rdfs(X, _, _).
rdfs(Y, rdf:type, rdfs:'Resource') :-                           % rdfs4b
    rdfs(_, _, Y).
rdfs(P, rdfs:subPropertyOf, R) :-                               % rdfs5
    rdfs(P, rdfs:subPropertyOf, Q),
    rdfs(Q, rdfs:subPropertyOf, R).
rdfs(P, rdfs:subPropertyOf, P) :-                               % rdfs6
    rdfs(P, rdf:type, rdf:'Property').
rdfs(X, Q, Y) :-                                                % rdfs7
    rdfs(P, rdfs:subPropertyOf, Q),
    rdfs(X, P, Y).
rdfs(C, rdfs:subClassOf, rdfs:'Resource') :-                    % rdfs8
    rdfs(C, rdf:type, rdfs:'Class').
rdfs(X, rdf:type, D) :-                                         % rdfs9
    rdfs(C, rdfs:subClassOf, D),
    rdfs(X, rdf:type, C).
rdfs(C, rdfs:subClassOf, C) :-                                  % rdfs10
    rdfs(C, rdf:type, rdfs:'Class').
rdfs(C, rdfs:subClassOf, E) :-                                  % rdfs11
    rdfs(C, rdfs:subClassOf, D),
    rdfs(D, rdfs:subClassOf, E).
rdfs(P, rdfs:subPropertyOf, rdfs:member) :-                     % rdfs12
    rdfs(P, rdf:type, rdfs:'ContainerMembershipProperty').
rdfs(X, rdfs:subClassOf, rdfs:'Literal') :-                     % rdfs13
    rdfs(X, rdf:type, rdfs:'Datatype').

% Link to the rdf triples:

rdfs(S, P, O) :-
    rdf(S, P, O).
```

Now, this won't do much good in classical Prolog. The best you can do is
create a fixed point algorithm that tries these rules and add all
entailed __new__ triples to the database. Repeat this process until no
rule produces any new triple. If you add a new triple you must repeat
this process to keep the entailed graph consistent. You can improve on
that by ordering the rules to reduce the number of required iterations.
That requires additional brain cycles from you though :) .

Instead, we add these __two declarations__ to the database (at the top
of the file):

    :- table rdfs/3 as monotonic.
    :- dynamic rdf/3 as monotonic.

If we now add some rdf/3 triples to the database and ask for the
entailed graph by querying ``rdfs/3`` everything works by magic. This is
because _tabled evaluation_ watches for goals that are being evaluated
that are a _variant_ (equivalent goal after variable renaming) of the
current goal. In that scenario it effectively reverts to _bottom up_
evaluation while it only propagates _new_ answers. This provides
termination warrants similar to Datalog: any program with a finite
Herbrand universe terminates.

SWI-Prolog's innovation is in the option "_as_ __monotonic__". Given
this option the initial tabled execution constructs a _dependency graph_
linking the dynamic data to the derived tables using
[continuations](https://www.swi-prolog.org/pldoc/man?section=delcont).
As new facts are added to the dynamic predicates (rdf/3 here), these
continuations are triggered and propagate the consequences (and possibly
expand the dependency network if new rules can fire due to the new
data). __Effectively, our clean declarative rules have been transformed
into an efficient forward chaining engine with no effort from us!__

## Prefixes

We borrow some code from SWI-Prolog's Semantic Web (`semweb`) package to
deal with RDF prefixes, making e.g., `rdf:'Property'` a plain atom at
compile time (again, add above the rules):

```
:- use_module(library(semweb/rdf_prefixes)).

:- rdf_meta
    rdfs(r,r,o).
```

## Experiment

I've carried out a small experiment using the
[AAT](https://www.getty.edu/research/tools/vocabularies/aat/) as RDF
data (286,227 triples). First, we run ``rdfs/3``, initializing the
dependencies. Note that this has no solutions as there are no triples in
our database.

    ?- rdfs(S,P,O).
     false.

Now we add our triples after reading them from RDF/XML:

    ?- load_rdf('aat.rdf', Triples), time(maplist(assertz, Triples)).
    % 37,971,687 inferences, 6.301 CPU in 6.333 seconds (99% CPU, 6026215 Lips)

This results in 502,117 ``rdfs/3`` answers. Now we can load the AAT
schema:

    ?- load_rdf('aat.rdfs', Triples), time(maplist(assertz, Triples)).
    % 12,198,444 inferences, 1.490 CPU in 1.491 seconds (100% CPU, 8185847 Lips)

Raising the number of ``rdfs/3`` edges to 664,950. The memory used by
Prolog after these steps is 766Mbytes.

If we reverse the order, first loading all rdf/3 data and then calling
``rdfs/3`` to compute the entailment, the loading time is 0.7 seconds and
the inference time is 4.9 seconds, resulting in the same 664,950 edges.
__Batch computing is only 40% faster than incremental maintenance__!.


## Further steps

Transactions as implemented by transaction/1 provide isolation and
atomic changes to the database. Consistent cooperation with tabling is
experimental, but already works for the above scenario. This allows for
efficient "what if" reasoning: add some rdf/3 facts inside the
transaction, examine the entailed graph for certain properties and
decide to _commit_ or _roll back_.

Monotonic tables may be __watched for new answers__. The low-level
mechanism using prolog_listen/3 is present. A more robust and simpler to
use interface is under consideration.

## Conclusions

Tabling greatly enhances the expressive power of Prolog for rule based
reasoning. _Monotonic_ tabling provides efficient maintenance of
consistent results in a changing world given a monotonic rule set.
SWI-Prolog also allows for _"what if"_ reasoning and acting on newly
inferred results.

## Future work

Being limited to _monotonic_ rules is a strong restriction and too
strong for the Pharos project mentioned at the top. We are currently
working on program transformation to transform a rule set with negation
into two monotonic rule sets. One of these computes an upper bound while
the other contains the negated answers.

## Note

To work as advertised you need the next development release (8.3.14) or
compile SWI-Prolog from source.
