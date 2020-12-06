
This is MIndiGolog: IndiGolog for cooperative execution by multi-agent teams

The base files:   * sitcalc.pl: foundational axioms of the situation calculus
                  * mindigolog.pl: semantics and solver for MIndiGolog programs
                  * main.pl: top-level control file, pulling it all together

The domain-specific files:
             * domain.pl: axiomatisation of your domain in the calculus
             * program.pl: definitions of your procedures

The environment:   Development is being done under SWI prolog, as it
                   is open source and provides a real-valued constraint
                   solver.

