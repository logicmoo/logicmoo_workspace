1. COVERAGE

Toy1 is a Regulus grammar which recognises simple commands and queries in a home automation
devices, e.g. "turn on the fan" or "is the light switched on".

It is possible to run the grammar inside a toy spoken language dialogue application
or a toy English -> French speech translation application. The spoken dialogue application
responds to the commands and questions, and the speech translation application 
translates them into French.

2. FILES

2.1 Files for toy English grammar

- Regulus/toy1.regulus contains the English grammar rules and lexicon entries.

- Regulus/toy1_declarations.regulus contains the declarations for the English grammar.

- scripts/toy1.cfg is the config file for the English grammar.

- scripts/library_declarations.pl defines library directories and file search paths.

- scripts/build_nuance_files.pl is a Prolog file that calls Regulus to build the Nuance grammar.

- scripts/Makefile is a makefile with two targets:
  - "default" builds the Nuance grammar and then compiles it into a recognition package.
    The resulting package will be created as Generated/recogniser.		
  - "french_generation" build the French generation grammar, used for the 
    speech translation app  

- scripts/run_recserver.bat starts a Nuance recserver process for the generated package.

- scripts/run_xapp.bat starts a Nuance Xapp process for the generated package.

2.2 Files for toy spoken dialogue application

- scripts/toy1_dialogue.cfg is the config file for the dialogue application grammar.

- Prolog/toy1_app.pl contains the top-level code for the spoken language application.

- Prolog/{input_manager, dialogue_manager, output_manager}.pl contain the dialogue processing code.

- scripts/run_vocalizer3.bat starts an English Vocalizer 3 TT3 server.

- scripts/run_regulus_server.bat starts a Regulus SpeechServer process.

- scripts/regulus_server.cfg is a config file for the Regulus SpeechServer.

- scripts/load_and_run_app.pl contains the calls to load the Prolog application.

- scripts/load_and_run_app.bat loads the Prolog code.

2.3 Files for the toy speech translation application

- Regulus/toy1_french_generation.regulus is the French generation grammar.

- scripts/toy1_slt.cfg is the English config file for the speech translation app.

- scripts/toy1_french_generation.cfg is the French config file for the speech translation app.

- Prolog/toy_slt_app.pl contains the code for the application.

- Prolog/interlingua_declarations.pl defines the permitted interlingua constants.

- Prolog/eng_to_interlingua.pl contains rules that map English representations into interlingua representations.

- Prolog/interlingua_to_fre.pl contains rules that map interlingua representations into French representations.

- scripts/run_vocalizer3_fre.bat starts an French Vocalizer 3 TT3 server.

- scripts/run_regulus_server.bat starts a Regulus SpeechServer process.

- scripts/regulus_server.cfg is a config file for the Regulus SpeechServer.

- scripts/load_and_run_slt_app.pl contains the calls to load the Prolog speech translation application.

- scripts/load_and_run_slt_app.bat loads the Prolog code for the speech translation application.

3. COMPILING THE ENGLISH AND FRENCH GRAMMARS AND THE RECOGNISER

- Start a Cygwin window.

- cd $REGULUS/Examples/Toy1/scripts

- make

4. TESTING THE GRAMMAR AND PROLOG CODE

- Start a Cygwin window.

- cd $REGULUS/Examples/Toy1/scripts

- make test

This should run the dialogue application and the speech translation application in batch text mode.

5. TESTING THE RECOGNISER

- If there is no license manager already running, invoke $REGULUS/scripts/run_license.bat

- Go to $REGULUS/Examples/Toy1/scripts

- Invoke run_recserver.bat

- Invoke run_xapp.bat

6. RUNNING THE TOY SPOKEN DIALOGUE APPLICATION

- Create an empty directory called C:/Temp/RegulusRuntime/filetalk. 
  This is to hold the files used for communication between the application 
  and the Regulus SpeechServer process.

- If you have not already done so, install the Nuance Vocalizer 3 TTS Engine
  with the default English voice (Laurie).

- If there is no license manager already running, invoke $REGULUS/scripts/run_license.bat

- Invoke run_recserver.bat

- Invoke scripts/run_vocalizer3.bat

- Invoke run_regulus_server.bat

- Invoke load_and_run_app.bat

7. RUNNING THE TOY SPEECH TRANSLATION APPLICATION

- Create an empty directory called C:/Temp/RegulusRuntime/filetalk. 
  This is to hold the files used for communication between the application 
  and the Regulus SpeechServer process.

- If you have not already done so, install the Nuance Vocalizer 3 TTS Engine
  with the default French voice (Julie).

- If there is no license manager already running, invoke $REGULUS/scripts/run_license.bat

- Invoke run_recserver.bat

- Invoke scripts/run_vocalizer3.bat

- Invoke run_regulus_server.bat

- Invoke load_and_run_slt_app.bat
