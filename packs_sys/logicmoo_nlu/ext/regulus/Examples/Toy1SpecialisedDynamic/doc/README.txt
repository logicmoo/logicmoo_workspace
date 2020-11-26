

INSTALLING, COMPILING AND RUNNING THE TOY DYNAMIC LEXICON APPLICATION

Manny Rayner

Last revised: Sep 18, 2008


1. OVERVIEW

The toy dynamic lexicon app demonstrates dynamic grammar capabilities
using a version of the Toy1Specialised grammar, enhanced with a few
sample dynamic lexicon declarations. It does not perform any dialogue
processing, but only recognition and dynamic addition of lexicon
content.

2. INSTALLING

Perform a 'make' in the directory $REGULUS/Toy1SpecialisedDynamic/scripts
This should do two things:

- Build a dummy top-level recognition package (very quick)

- Build a dynamic version of the Toy1Specialised grammar

Note that it is NOT necessary to build a Nuance recognition package. The Nuance
grammar is compiled at runtime by the compilation server.

3. STARTING UP THE APP 

- Start a license manager

- Invoke $REGULUS/Examples/Toy1SpecialisedDynamic/scripts/run_resource_manager.bat

- Invoke $REGULUS/Examples/Toy1SpecialisedDynamic/scripts/run_compilation_server.bat

- Invoke $REGULUS/Examples/Toy1SpecialisedDynamic/scripts/run_recserver.bat

- Invoke $REGULUS/Examples/Toy1SpecialisedDynamic/scripts/run_toy_dynamic_app.bat

4. RUNNING THE APP

The app starts up and goes into a loop. At each iteration, the user can type one
of the following:

- [Return] Perform recognition and print result.

- NEW NAME <Name>  e.g.
  NEW NAME beverley
  NEW NAME howard the duck
  
  Add the new name to the vocabulary, using the lexicon macro system_name_phrase.
  The effect should be as if the lexicon entry

  @person_name(<NameAsCommaList>, <NameAsSemConstant>)

  had been included in the original grammar. For example, if you type "NEW NAME howard the duck",
  the effect is as if the lexicon entry

  @person_name((howard, the, duck), howard_the_duck)

  had been included in the grammar.

- EXIT Exit loop and quit.

5. COVERAGE

Coverage of the recogniser is defined by the training corpus in 
$REGULUS/Examples/Toy1SpecialisedDynamic/corpora/toy1_corpus.pl, together
with any dynamic names that may be added. Note that imperative sentences
in the corpus are prefaced by a name. Originally, the only name in
the lexicon is "magnificent one".

6. BUILDING YOUR OWN DYNAMIC APPLICATIONS

If you want to build your own dynamic application, start by looking
in the files $REGULUS/Examples/Toy1SpecialisedDynamic/Regulus/toy1_lex.pl
and $REGULUS/Examples/Toy1SpecialisedDynamic/Prolog/toy1_app.pl:

6.1 Regulus/toy1_lex.pl

This file contains the lexicon. Look at the use of lexicon macros and
"dynamic_lexicon" declarations. In order to allow dynamic assertion of 
a lexicon entry at runtime, a suitable macro must be defined and 
declared dynamic.

Note that Regulus compilation, invoked by doing 'make' in the scripts
directory, creates the file Generated/toy1_dynamic_lex_associations.pl.
This file contains declarations that need to be accessed by the 
application at runtime.

6.2 Prolog/toy1_app.pl

The code in $REGULUS/Examples/Toy1SpecialisedDynamic/Prolog/toy1_app.pl
is intentionally very simple. It should be easy to adapt to other applications
which require dynamic lexicon capabilities. Some specific things to note:

- In the predicate initialise/4, note the call 

   init_dynamic_lexicon_runtime(DynamicLexAssociationsFile)

This needs to be made before any dynamic lexicon calls are made.

- The actual process of adding a dynamic lexicon entry is performed
by the call 

   assert_dynamic_lex_entry(MacroCall)

in add_name/1. MacroCall should be a lexicon entry, defined using
a lexicon macro which has been declared dynamic. For example,
typing 

   NEW NAME howard the duck

at the top level of the application results in the call
 
   assert_dynamic_lex_entry( @person_name((howard, the, duck), howard_the_duck))



