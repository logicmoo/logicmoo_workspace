HOW TO RUN THE TOY1 CALENDAR APP 

- The Toy1 calendar app includes FIVE processes: license manager, recserver, vocalizer, dialogue server, and app.

- Required: Nuance 8.5, Nuance Vocalizer 4, SICStus Prolog 3.11/3.12.5/4.0.4, Regulus, Cygwin/UNIX

- Compile recogniser: 
  cd $REGULUS/Examples/Calendar/scripts
  make

  This should create a recognition package in $REGULUS/Examples/Calendar/Generated/recogniser and also some other resources

- To run the system 
  - Start Nuance license manager
  - Invoke $REGULUS/Examples/Calendar/scripts/run_recserver.bat
  - Invoke $REGULUS/Examples/Calendar/scripts/run_vocalizer4.bat
  - Invoke $REGULUS/Examples/Calendar/scripts/run_server.bat	
  - Do one of the following
    - Invoke $REGULUS/Examples/Calendar/scripts/run_app_using_server.bat [version printing debugging output]
    - Invoke $REGULUS/Examples/Calendar/scripts/run_app_using_server_nodebug.bat [version NOT printing trace output]
  - Hit <return> and speak
    - The system should give help examples after each turn based on statistical recognition of the user utterance.
    - There are more examples in $REGULUS/Examples/Calendar/corpora/calendar_dev_corpus.pl
  - Type EXIT to exit top loop, or RESTART to reinitialise the dialogue context without restarting.

INTERFACING TO THE DIALOGUE SERVER

The dialogue server is a simple socket-based server which handles all interpretation and dialogue management.
In the initial version, the minimal functionality is recognition string in, TTS string out. 

- To start dialogue server
  - Edit the file $REGULUS/Examples/Calendar/scripts/load_and_run_server.pl to specify the port used (currently 1985).
  - Invoke $REGULUS/Examples/Calendar/scripts/run_server.bat
  - Connect to specified socket

- Message protocol: see $REGULUS/Examples/Calendar/Prolog/dialogue_server.pl

COLLECTING AND USING RECORDED DATA

Each session with the app should create a new timestamped wavfile directory in $REGULUS/Examples/Calendar/corpora/speech.
These wavfiles can be fed back into the debug-edit-test cycle as follows.

- Do 'make update_transcriptions' in $REGULUS/Examples/Calendar/scripts. This should update the file
  $REGULUS/Examples/Calendar/corpora/calendar_transcriptions.txt to include lines for all wavfiles in
  $REGULUS/Examples/Calendar/corpora/speech. Wavfiles which have not been transcribed will have blank lines.
- Add transcriptions to the wavfiles which have not yet been transcribed, using the Nuance xwavedit tool. 
  - The easiest way to do this is to invoke xwavedit from Cygwin on one or more complete directories, using wildcards.
    For example, if you want to transcribe all the wavfiles for 2007-10-03 and 2007-10-04, do
    
    cd $REGULUS/Examples/Calendar/corpora/speech
    xwavedit 2007-10-03*/*.wav 2007-10-04*/*.wav
  - You will get an xwavedit window listing the wavfiles you have specified. Transcribe each wavfile as follows:
    - Specify the wavfile you wish to edit, by selecting it in the uppermost pane.
    - Tick the Enable Transcript box in the lower left.
    - Click Play to hear the wavfile.
    - Write the transcription to the Transcription pane in the middle.
    - *** IMPORTANT: CLICK SAVE BEFORE MOVING ON TO THE NEXT WAVFILE, OR YOU WILL LOSE YOUR TRANSCRIPTION ***
- At any point, you can rerun 'make update_transcriptions' in $REGULUS/Examples/Calendar/scripts. This will update
  $REGULUS/Examples/Calendar/corpora/calendar_transcriptions.txt and show you which wavfiles remain to be transcribed.
- Continue until you have transcribed all the wavfiles, then check calendar_transcriptions.txt into CVS.
- When you have finished transcribing, do 'make transcriptions_data_to_sent_data' in $REGULUS/Examples/Calendar/scripts.
  This will update the file $REGULUS/Examples/Calendar/corpora/dev_corpus_from_transcriptions.pl. Note that records 
  for new wavfiles are automatically divided into timestamped mini-dialogues, so that the notional date used in 
  batch testing will correspond to the actual date when the wavfiles were recorded. 
- Check calendar_dev_corpus.pl to make sure that the new data looks plausible. When you are satisfied that 
  calendar_dev_corpus.pl is correct, check it into CVS. 
- Finally, do a plain 'make' in $REGULUS/Examples/Calendar/scripts. This will use the new version of calendar_dev_corpus.pl
  to perform the following operations:
  - Remake the specialised grammar
  - Remake the recogniser
  - Run a batch dialogue test (in text mode) on calendar_dev_corpus.pl.
  - Use the results of the batch dialogue test to remake the help resources.
- If you are at Geneva, zip or tar the new wavfile directories, and upload them to the internal project area so that
  other project personnel can access them. 
  - *** IMPORTANT: DO NOT CHECK THE WAVFILES INTO CVS, SINCE THERE IS NOT ENOUGH SPACE FOR THEM ***
