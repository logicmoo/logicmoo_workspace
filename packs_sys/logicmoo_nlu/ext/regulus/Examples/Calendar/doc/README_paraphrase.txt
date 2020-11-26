
Some initial notes on the English paraphrase grammar

- There are three .cfg files in the scripts directory:
  - paraphrase.cfg. Use this to compile paraphrase grammar for generation (original purpose)
  - paraphrase_recogniser.cfg. Use this to compile paraphrase grammar for recognition
  - paraphrase_recogniser_parser.cfg. Use this to do speech recognition + parsing with paraphrase grammar

- The grammar files
  - The generation and recognition versions of the grammar are slightly different. 
  - It's done using include files, with most of the rules shared.
  - Look in the .cfg files to see the details

- Make targets, in scripts/Makefile
  - paraphrase_grammar. Compile paraphrase grammar for generation 
  - paraphrase_recogniser. Compile paraphrase grammar into recognition package

- Running the paraphrase recogniser
  - Start Regulus with paraphrase_recogniser_parser.cfg
  - LOAD to load normal paraphrase grammar for parsing
  - LOAD_RECOGNITION to start recogniser. This assumes a script in $REGULUS/scripts/run_license.bat which starts a license manager.
  - RECOGNISE to recognise and parse. This also logs wavfiles in $REGULUS/recorded_wavfiles.

  