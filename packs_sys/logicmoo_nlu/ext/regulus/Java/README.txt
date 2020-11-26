
This directory contains an experimental GUI-based interface to
Regulus, developed by Elisabeth Kron and Manny Rayner. 

To be able to use the GUI, you will first need to do the following:

- Set the environment variable SPINSTALLDIR to point to the directory where you
  have SICStus Prolog installed. On most Windows machine, this will be something
  like "C:/Program Files/SICStus Prolog 3.12.5".

- Copy the file $REGULUS/Prolog/default_config_files.pl to $REGULUS/Prolog/config_files.pl
  and edit it to include pointers to the config files you want to be able to use.

- In case the JAR file is not up to date, remake it by doing

  cd $REGULUS/Java
  make clean
  make

  This should take about a minute.  

To start the GUI, do the following:

- Invoke the script $REGULUS/Java/run_prolog.bat.

- Invoke the script $REGULUS/Java/run_java.bat.

An initial version of a tutorial introduction can be found in
$REGULUS/doc/RegulusGUITutorial.pdf.
