
The JudgeGUI is a Java program designed for annotating and
manipulating trace files produced by MedSLT. For historical reasons,
it's here - though it probably shouldn't be here, since it's
MedSLT-specific.

- To make the JAR file, do the following:

  cd $REGULUS/Java
  make clean_judge
  make judge

  This should take about a minute.  

- To invoke the JudgeGUI, you will need to do the following:

  cd $REGULUS/Java
  java -classpath "RegulusGUI.jar;." JudgeGUI.JudgeGUI <DirectoryToJudge>

  This can certainly be done more elegantly by writing a little script and putting 
  $REGULUS/Java/RegulusGUI.jar in the classpath.

