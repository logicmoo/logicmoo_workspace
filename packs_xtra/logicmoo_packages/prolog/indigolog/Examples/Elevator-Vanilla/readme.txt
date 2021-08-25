This directory contains 5 versions of a simple elevator controller for
IndiGolog that appeared in Reiter 2001 Knowledge in Action book, the ConGolog
paper in IJCAI-97, and other examples designed by Hector Levesque.  

In all cases, an elevator is operating in a building with 6 floors.  The
elevator can go up and down and responds to call buttons on floors being on.
Once all call buttons are off, the elevator goes to the first floor and stops.

   Example 1: the most basic elevator
   Example 2: using Indigolog's search to minimize up and down motion
   Example 3: has exogenous actions that can change the world
   Example 4: has sensing of individual elevator buttons
   Example 5: has sensing of all elevator buttons

To run these examples, load one of "mainN_swi" or "mainN_ecl" (where N is 1,
2, 3, 4, or 5) depending on which Prolog you have, call main/0, and you should
see some output.  For examples 3, 4, and 5, you will need to provide a stream
of inputs for either simulated exogenous actions or simulated sensing.  (See
the exampleN.pl file for more details.)

Here is a trace of SWI Prolog on Example 1. The elevator starts on floor 3,
and call buttons are lit on floors 2 and 5.

   Welcome to SWI-Prolog (Multi-threaded, Version 5.2.13)
   Copyright (c) 1990-2003 University of Amsterdam.
   SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
   and you are welcome to redistribute it under certain conditions.
   Please visit http://www.swi-prolog.org for details.
   
   For help, use ?- help(Topic). or ?- apropos(Word).
   
   ?- [main1_swi].
   %   indigolog-vanilla compiled 0.02 sec, 13,676 bytes
   %  ../../Interpreters/indigolog-vanilla_swi compiled 0.02 sec, 15,364 bytes
   %  example1 compiled 0.01 sec, 3,960 bytes
   % main1_swi compiled 0.03 sec, 20,304 bytes
   
   Yes

   ?- main.
   down
   open
   close
   off(2)
   up
   up
   up
   open
   close
   off(5)
   down
   down
   down
   down
   open
   
   15 actions.
   
   Yes
