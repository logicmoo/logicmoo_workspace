
This is a reasoning engine for Complex Epistemic Modalities in the Situation 
Calculus It was developed as part of Ryan Kelly's PhD thesis "Asynchronous 
Multi-Agent Reasoning in the Situation Calculus".  Further details are
available at:

   http://www.rfk.id.au/research/thesis/

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

As well as the "twbcompile" program from the Tableaux Workbench Suite:

   http://twb.rsise.anu.edu.au/


---------------------------------

This is a simplistic prolog implementation of epistemic reasoning in the
situation calculus, using techniques from the thesis.  The domain
implemented is the "party invitation" example.
Concurrent actions are not currently supported, although this is purely
an implementation limit.

Fluents are reified for easy manipulation, with the predicate "holds"
evaluating a fluent at a given situation.  Used like so:

    ?- [main].
    true.

    ?- holds(loc(c),s0).
    true.

    ?- holds(knows(ann,loc(C)),s0).
    fail.

    ?- holds(knows(ann,loc(C)),do(read(ann),s0)).
    true.


We assume all variables have a finite domain, so that fluents can be
propositionalized to perform reasoning.  We have modified the PDL
prover from the Tableuax Workbench project do handle variable assignments,
which we shell out to for the actual reasoning.  You will need to compile
the file "vpdl/vpdl.ml" using the TWB compiler; `make` should do this for you.

The included files are:

  * fluent.pl:  Create and manipulate reified fluent terms.  This file also
                provides simplication routines to help keep the size of
                fluent terms manageable.

  * epath.pl:   Create and manipulate epistemic path terms.  This is pretty
                similar in scope to fluent.pl.

  * twb_pdl.pl:  Interface with the PDL prover for the Tableaux Workbench
                 project to static epistemic reasoning.

  * sitcalc.pl:  Domain-independent sitcalc primitives, such as regression()
                 and holds().

  * domain.pl:  Domain-specific predicates, such as ADP fluent definitions,
                causes_true/causes_false, etc.

  * vpdl/*:     Modified PDL prover that can handle variable assignments.


The files come with unit tests included, which may help demonstrate the syntax
and use of the various predicates.  To run the test suite, do the following
in the prolog shell:

    ?- [main].
    ?- run_tests.

