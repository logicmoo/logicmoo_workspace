
This is a reasoning engine for multi-agent epistemic queries in the situation
calculus.  It was developed as part of the PhD thesis (and subsequent journal
paper submission) for:

  Asynchronous Multi-Agent Reasoning in the Situation Calculus
  Ryan F Kelly and Adrian R Pearce

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

---------------------------------

To get up and running, you will need a working installation of SWI-Prolog,
available from:

   http://www.swi-prolog.org/


---------------------------------

This is a simplistic prolog implementation of epistemic reasoning in the
situation calculus, using techniques from the paper.  Two example domains are
available:

  * The "party invitation" domain, in which two agents may learn the
    location of a party by reading an invitation, possibly in secret.

  * The "hunt-the-wumpus" domain, in which two agents are hunting a wumpus
    through a dungeon and have partial observability of each other's
    actions.

The domains are assumed to be finite, and quantification over actions or
observations is done by expanding cases.  Concurrent actions are not supported,
although this is purely an implementation limit.

Fluents are reified for easy manipulation, with the predicate "holds"
evaluating a fluent at a given situation.

Reasoning in the fluent domain is performed using a pure-prolog prover for
modal first-order logic, modeled after the classical leanTAP prover.  It
current treats existential quantifiers by enumerating the (finitely many)
possibilities in a pre-processing step, so it performs poorly on larger
examples due to exponential blow-up.

The included files are:

  * prover.pl:   Pure-prolog theorem prover for the reified fluent domain,
                 including knowledge modalities for multiple agents.

  * fluent.pl:   Create and manipulate reified fluent terms.  This file also
                 provides simplication routines to help keep the size of
                 fluent terms manageable.

  * sitcalc.pl:   Domain-independent sitcalc primitives, such as regression()
                  and holds().

  * domain_*.pl:  Domain-specific predicates, such as ADP fluent definitions,
                  causes_true/causes_false, etc.


The files come with unit tests included, which may help demonstrate the syntax
and use of the various predicates.  To run the test suite, do the following
in the prolog shell:

    ?- [main].
    ?- run_tests.

