COVERAGE

PSA is a Regulus grammar, based on the general grammar, which
recognises a subset of the coverage of the Personal Satellite
Assistant application. Typical sentences are "go to flight deck",
"measure temperature and pressure" and "is radiation increasing". The
recogniser can be run against a toy Prolog application which uses the Regulus
Speech Server to recognise user utterances and echo them back in
post-processed LF form. Communication between the Prolog applications and
the Regulus Speech Server process is by a simple file-based protocol.

FILES

- Regulus/psa_lex.regulus contains the domain-specific Regulus lexicon.

- corpora/psa_sents.pl contains the training corpus for the EBL grammar specialisation process.

- Prolog/toy_app.pl contains a minimal application, based on the Regulus Speech Server. It supports
  a push-to-talk interface which echoes back representations of user utterances in post-processed LF form.

- scripts/psa.cfg is the config file.

- scripts/config.pl defines library directories and file search paths.

- scripts/build_nuance_files.pl is a Prolog file that calls Regulus to build the Nuance grammar.

- scripts/Makefile is a makefile that builds the Nuance grammar and then compiles it into a recognition package.
  The resulting package will be created as GeneratedFiles/recogniser.		  

- scripts/run_recserver.bat starts a Nuance recserver process for the generated package.

- scripts/run_regulus_server.bat starts a Regulus Speech Server process with appropriate parameters.

- scripts/regulus_server.cfg is a config file used by the Regulus Speech Server process.

- scripts/run_toy_app.pl is a Prolog file which, when loaded, starts the application defined in Prolog/toy_app.pl.

COMPILING THE GRAMMAR

1. Start a Cygwin window.

2. cd $REGULUS/Examples/Toy1/scripts

3. make

TESTING THE RECOGNISER

1. Create an empty directory called C:/Temp/RegulusFiletalk. This will
hold the files used for communication between the Prolog process and
the Regulus Speech Server process.

2. If there is no license manager already running, invoke $REGULUS/scripts/run_license.bat

3. Go to $REGULUS/Examples/PSA/scripts

4. Invoke run_recserver.bat

5. Invoke run_regulus_server.bat

6. Invoke run_toy_app_prolog.bat

