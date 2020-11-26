
HOW TO RUN THE EXAMPLES (WINDOWS + CYGWIN VERSION)

--------------------------------------------------------

Toy0 in GSL

- Make recogniser 
  cd $REGULUS/Examples/Toy0/scripts
  make hand_coded

- Test using nl-tool
  cd $REGULUS/Examples/Toy0/Nuance
  nl-tool -package hand_coded_toy0 -grammar .MAIN

- Test using Xapp
  - Start license manager
  - Start $REGULUS/Examples/Toy0/scripts/run_recserver_hand_coded.bat
  - Start $REGULUS/Examples/Toy0/scripts/run_xapp_hand_coded.bat

--------------------------------------------------------

Toy0

- Make recogniser
  cd $REGULUS/Examples/Toy0/scripts
  make

- Compile Java app
  Edit $REGULUS/Examples/Toy0/scripts/java_settings.bat as described in book
  Edit $REGULUS/Examples/Toy0/java/JDemo.java as described in book
  Start $REGULUS/Examples/Toy0/scripts/compile_jdemo.bat

- Test using Xapp
  - Start license manager
  - Start $REGULUS/Examples/Toy0/scripts/run_recserver.bat
  - Start $REGULUS/Examples/Toy0/scripts/run_xapp.bat

- Run Prolog Toy0 dialogue app 
  - Start license manager
  - Start $REGULUS/Examples/Toy0/scripts/run_recserver.bat
  - Start $REGULUS/Examples/Toy0/scripts/run_vocalizer3.bat
  - Start $REGULUS/Examples/Toy0/scripts/run_app.bat

- Run Prolog Toy0 speech translation app 
  - Start license manager
  - Start $REGULUS/Examples/Toy0/scripts/run_recserver.bat
  - Start $REGULUS/Examples/Toy0/scripts/run_vocalizer3_fre.bat
  - Start $REGULUS/Examples/Toy0/scripts/run_slt_app.bat

- Run Java Toy0 dialogue app 
  - Start license manager
  - Start $REGULUS/Examples/Toy0/scripts/run_recserver.bat
  - Start $REGULUS/Examples/Toy0/scripts/run_jdemo.bat

--------------------------------------------------------

Toy1

- Make main recogniser and generation grammar
  cd $REGULUS/Examples/Toy1/scripts
  make

- Make recogniser for extended version of Toy1 dialogue app
  cd $REGULUS/Examples/Toy1/scripts
  make recogniser_corrections

- Run Prolog Toy1 dialogue app 
  - Start license manager
  - Start $REGULUS/Examples/Toy1/scripts/run_recserver.bat
  - Start $REGULUS/Examples/Toy1/scripts/run_vocalizer3.bat
  - Start $REGULUS/Examples/Toy1/scripts/run_app.bat

- Run Prolog Toy1 dialogue app (extended version with corrections)
  - Start license manager
  - Start $REGULUS/Examples/Toy1/scripts/run_recserver_corrections.bat
  - Start $REGULUS/Examples/Toy1/scripts/run_vocalizer3.bat
  - Start $REGULUS/Examples/Toy1/scripts/run_app_corrections.bat

- Run Prolog Toy0 speech translation app 
  - Start license manager
  - Start $REGULUS/Examples/Toy1/scripts/run_recserver.bat
  - Start $REGULUS/Examples/Toy1/scripts/run_vocalizer3_fre.bat
  - Start $REGULUS/Examples/Toy1/scripts/run_slt_app.bat

--------------------------------------------------------

Toy1Specialised

- Make recogniser and generation grammar
  cd $REGULUS/Examples/Toy1Specialised/scripts
  make

- Run Prolog Toy1Specialised dialogue app 
  - Start license manager
  - Start $REGULUS/Examples/Toy1Specialised/scripts/run_recserver.bat
  - Start $REGULUS/Examples/Toy1Specialised/scripts/run_vocalizer3.bat
  - Start $REGULUS/Examples/Toy1Specialised/scripts/run_app.bat

- Run Prolog Toy0 speech translation app 
  - Start license manager
  - Start $REGULUS/Examples/Toy1Specialised/scripts/run_recserver.bat
  - Start $REGULUS/Examples/Toy1Specialised/scripts/run_vocalizer3_fre.bat
  - Start $REGULUS/Examples/Toy1Specialised/scripts/run_slt_app.bat
