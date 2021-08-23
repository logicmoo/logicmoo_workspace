==============================================================================
		     The PIE System for First-Order-Based
		    Proving, Interpolating and Eliminating

		     http://cs.christophwernhard.com/pie/

				  README.TXT
==============================================================================

- This is a development snapshot of the PIE system.  Enhancements of its
  functionality is planned and the modularization of the system might change.

- The system currently includes parts of ToyElim, which is based on
  propositional logic.  See http://cs.christophwernhard.com/toyelim/ for a
  user manual describing the functionality of ToyElim and installation
  instructions mainly concerning the interfacing to external SAT solvers.

- REQUIREMENTS
  - A Unix-like environment (Linux or Mac OS/X) is assumed
  - SWI Prolog (has been tested with version 8.2.4)
  - A LaTeX installation
  - Optionally: The first-order prover/model searcher Prover9/Mace4
  - Optionally: Further first-order provers that accept TPTP syntax,
    e.g., the E prover and Vampire
  - Optionally: The MiniSat SAT Solver
  - Optionally: The TPTP library of proving problems
  - Optionally (for the ToyElim functionality): The MiniSat SAT Solver and the
    Coprocessor SAT preprocessor
  - Optionally: Graphviz for visualizing proof graphs
  - Optionally: GNU Emacs for editing PIE source files and interaction with
    Prolog. The Prolog mode from https://bruda.ca/emacs/prolog_mode_for_emacs
    is recommended (see also http://www.swi-prolog.org/FAQ/GnuEmacs.html)

- INSTALLATION

  - Checkout or update the system or, respectively unpack it:
    Clone the pie Git repository:
    $ git clone http://cs.christophwernhard.com/git/pie.git
    This allows to update to new versions later with
    $ cd pie ; git pull
    Alternatively: Download and unpack the pie.tgz archive
  - Set the PIE environment variable to the pie/src/ directory
    (including the trailing slash)
  - Configure the Unix system such that related external commands
    are on the PATH (and thus found from calls to the shell/2
    predicate of SWI Prolog):
    pdflatex, bibtex, prover9, mace4, prooftrans, tptp2X, eprover,
    vampire, minisat, dot
  - NOTE: The use EProver and Vampire via PIE requires the tptp2X utility that
    comes with the TPTP. See NOTES ON INSTALLING tptp2X for PIE below.
  - Set the TPTPDirectory variable to your TPTP installation
  - Ensure that /tmp exists and is writeable (the PIE system
    writes some temporary files into that directory)
  - Start the system by invoking SWI Prolog with pie/folelim/load.pl,
    for example:
    $ swipl --stack_limit=12g -f ${PIE}/folelim/load.pl

- APPLICATION: PROCESSING A DOCUMENT WITH MACRO DECLARATIONS
  AND REASONER INVOCATIONS

  - The main way to use PIE is to write formalizations as suggested by the
    example documents in pie/scratch/

  - To process such a document:

    - Consult it (i.e. [re-]load it into the Prolog system)
    - Invoke the "ppl" predicate, e.g., from the user input:
      ?- ppl.
    - The statements in the documents are processed, which
      might effect proving, interpolation and elimination
    - A result document /tmp/tmp_ppl.tex is written and 
      processed with pdflatex, such that /tmp/tmp_ppl.pdf
      is the final result

  - Working on such documents:
  
    - scratch_demo_04.pl is a small document which involves proving,
      interpolation and elimination, and is thus suitable to take as a start.
    - During developing formalizations, direct calls to the predicates
      ppl_valid, ppl_ipol, ppl_elim from the Prolog shell are often 
      helpful (in the final document versions, some of these calls
      are typically embedded in ":- ppl_printtime")
    - Examples that work without loading a document:
      ?- ppl_valid(all(x, (all(x, p(x)) -> ex(x, p(x)))), [prover=cm]).
      ?- ppl_ipol(all(x, p(a,x)) -> ex(x, p(x,b))).
      ?- ppl_elim(ex2(q, all(x, ((p(x) -> q(x)), (q(x) -> r(x)))))).
    - Occasionally it is helpful to display the expansion of a formula:
      ?- mac_expand(+Formula)
    - Note on interplay with first-order provers: ppl_valid by default invokes
      Prover9/Mace4 expecting that it is installed. With option "prover=cm"
      the CM prover included with the system is invoked, and with options like
      "prover=tptp(eprover)" further external provers that support the TPTP
      format

- APPLICATION: PROVING TPTP PROBLEMS DIRECTLY WITH CM PROVER

  - From Prolog, the CM prover can be invoked directly with the cmpr/2
    predicate, for example:

    ?- cmpr('PUZ001+1', []).

    The second argument is a list of options. This predicate is described in
    the draft documentation that accompanies the development snapshot.

  - The CM Prover can also be called with a shell script:

    scripts/cm-prove-tptp-problem.sh 

    Example calls are in the comments in that script.  The options
    are passed to the cmpr/2 predicate.

==============================================================================
NOTES ON INSTALLING tptp2X FOR USE WITH PIE 
==============================================================================

It appears that the setting-up of tptp2X for SWI-Prolog included in the TPTP
became sowewhat outdated in recent years. A set-up that is sufficient for the
requirements of PIE with SWI 8.2.4 can be achieved with TPTP-v7.4.0 as
follows:

   1. Run tptp2X_install
      - Choose "swi" as Prolog system.
      - Select "all formats" with "a". Unexpectedly, however, this does lead
        not to the inclusion of any formats.
   2. In the file tptp2X
      - replace "set PrologArguments='-G16G -L8G -T8G'"
        with set PrologArguments='--stack_limit=16g'
   3. In the file tptp2X.main
      - uncomment the line %:-consult('format.tptp').
   4. In the file tptp2X.syntax:
      Comment out two clauses which lead to syntax errors. Their heads are:
      - count_formula_connectives('??' Formula,CountVector):-
      - count_formula_connectives('?*' Variables : Formula,CountVector):-
