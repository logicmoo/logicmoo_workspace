COVERAGE

Toy0 is a minimal Regulus grammar which recognises a few simple
phrases like "a dog" or "two cats". The only interesting point is that
the grammar enforces agreement between determiner and noun, so that
for example "two dog" or "a cats" is blocked.

FILES

- Regulus/toy0.regulus contains the whole grammar.

- scripts/toy0.cfg is the config file.

- scripts/config.pl defines library directories and file search paths.

- scripts/build_nuance_files.pl is a Prolog file that calls Regulus to build the Nuance grammar.

- scripts/Makefile is a makefile that builds the Nuance grammar and then compiles it into a recognition package.
  The resulting package will be created as GeneratedFiles/recogniser.		  

- scripts/run_recserver.bat starts a Nuance recserver process for the generated package.

- scripts/run_xapp.bat starts a Nuance Xapp process for the generated package.

COMPILING THE GRAMMAR

1. Start a Cygwin window.

2. cd $REGULUS/Examples/Toy0/scripts

3. make

TESTING THE RECOGNISER

0. If there is no license manager already running, invoke $REGULUS/scripts/run_license.bat

1. Go to $REGULUS/Examples/Toy0/scripts

2. Invoke run_recserver.bat

3. Invoke run_xapp.bat
