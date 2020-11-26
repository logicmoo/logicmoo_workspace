This directory contains the godis-basic system, a very simple dialogue system built with Trindikit.


Run single.sh or single.bat (or consult the file single_agent.pl in Agents) to start the godis-basic system with a serial control algorithm. This configuration does not need OAA.

There are two configurations in which the system components are distributed over several OAA agents. These can be started using OAA´s Startit agent. OAA 2.3.0 or later is needed. Also the environment variable OAA_HOME must be set to the OAA installation directory (e.g. /home/david/0aa2.3.0). Before they can be run, the TextIO java agent must be compiled. Enter directory Agents and run compileTextIO.bat or compileTextIO.sh

Run godis-basic.sh or godis-basic.bat to start the Startit agent with a proper configuration file. 

In these configurations a active input module is coupled with the output module in a java agent (see Agents/TextIO.java). They use a triggered control algorithm which is run when something is written to the input variable in the TIS. 

 In the "distributed" project all the other Trindikit components runs as separate agents. In the "semi-distributed" project the TIS and the interpret, generate, select and update modules are bundled together in one agent.

When testing, start the conversation by typing "price" or "visa".

