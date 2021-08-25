
Mini getting-started guide to get the Elevator examples running:

1. Make sure the installation of IndiGolog is correct 
	(read install.txt at the root directory of IndiGolog)

2. Make sure TCL/TK program 'wish' is in the path (in Linux it is often in /usr/bin)
	(the simulator environment uses a small TCL/TK interface to enter exog. actions async)

3. Go to directory <mypath>/indigolog/Examples/ElevatorSim-BAT and start SWI Prolog with
	sufficient global & heap memory: /usr/bin/pl -L128m -G128m -T64m

4. Consult main_swi.pl and execute goal "main". Several controllers will be offered.
	Enter the controller you want to run.
   That is, in the Prolog prompt that appears do the following:
       ?- [main].
       ?- main.
	Available Controllers: [1, 2, 3, 4]
	Which controller do you want to execute?
       4.		

5. Watch the IndiGolog Agent control the elevator.

6. To run it once again, execute goal "main" again.


