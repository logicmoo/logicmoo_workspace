# Installing LPS #

## LPS on SWISH (local or cloud web server) ##
See <https://bitbucket.org/lpsmasters/lps_corner/src/master/swish/INSTALL_server.md>

## Core engine ##
To simply run LPS as a Prolog program:

* Pull or download this whole repository into a new folder "LPS"
* Install Prolog:
    * Install **SWI Prolog 8.1.1** or later from http://www.swi-prolog.org/download/stable
	
### Getting Started with a core engine ###

* Launch Prolog and consult LPS by entering ['your_path_to_LPS/utils/psyntax.P']. 
* Then enter, for example, depending on your operating system, one of

	golps( '<your_path_to_LPS>/examples/CLOUT_workshop/diningPhilosophers.lps') . % Mac, Linux
	
	golps( '<your_path_to_LPS>\\examples\\CLOUT_workshop\\diningPhilosophers.lps') . % Windows

* Or for more informative output, enter, for example, one of

    golps( '<your_path_to_LPS>/examples/CLOUT_workshop/diningPhilosophers.lps', [verbose]).
	
    golps( '<your_path_to_LPS>\examples\CLOUT_workshop\diningPhilosophers.lps', [verbose]).

* Use golps(FullFilePathName). to run your programs

A typical session would look like this: 

    $ swipl
    Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.2.0)
    Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
    SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
    and you are welcome to redistribute it under certain conditions.
    Please visit http://www.swi-prolog.org for details.

    For help, use ?- help(Topic). or ?- apropos(Word).

    ?- ['./utils/psyntax.P'].

    ?- golps('./examples/CLOUT_workshop/prisoners.lps', [verbose]).




     ----- time is now 1 -----

      Events and actions from 0 to 1 are [refuses(you),bears_witness(me)]

      State at time 1 is [total_years_in_jail(me,0),total_years_in_jail(you,0)]

       Process reactive rules at time 1
       Old:  [reactive_rule([happens(bears_witness(_G1840),_G1837,_G1838),happens(refuses(_G1849),_G1837,_G1838)],  [happens(gets(_G1840,0),_G1838,_G1856),happens(gets(_G1849,3),_G1838,_G1856)]),reactive_rule([happens(bears_witness(_G1800),_G1797,_G1798),happens(bears_witness(_G1809),_G1797,_G1798),other(_G1800,_G1809)],[happens(gets(_G1800,2),_G1798,_G1822)]),reactive_rule([happens(refuses(_G1760),_G1757,_G1758),happens(refuses(_G1769),_G1757,_G1758),other(_G1760,_G1769)],[happens(gets(_G1760,1),_G1758,_G1782)]),reactive_rule([happens(refuses(_G1730),_G1727,_G1728),other(_G1735,_G1730)],[happens(refuses(_G1735),_G1728,_G1743)]),reactive_rule([happens(bears_witness(_G1700),_G1697,_G1698),other(_G1705,_G1700)],[happens(bears_witness(_G1705),_G1698,_G1713)])]
       New:  [reactive_rule([happens(bears_witness(_G1700),_G1697,_G1698),other(_G1705,_G1700)],[happens(bears_witness(_G1705),_G1698,_G1713)]),reactive_rule([happens(refuses(_G1730),_G1727,_G1728),other(_G1735,_G1730)],[happens(refuses(_G1735),_G1728,_G1743)]),reactive_rule([happens(refuses(_G1760),_G1757,_G1758),happens(refuses(_G1769),_G1757,_G1758),other(_G1760,_G1769)],[happens(gets(_G1760,1),_G1758,_G1782)]),reactive_rule([happens(bears_witness(_G1800),_G1797,_G1798),happens(bears_witness(_G1809),_G1797,_G1798),other(_G1800,_G1809)],[happens(gets(_G1800,2),_G1798,_G1822)]),reactive_rule([happens(bears_witness(_G1840),_G1837,_G1838),happens(refuses(_G1849),_G1837,_G1838)],[happens(gets(_G1840,0),_G1838,_G1856),happens(gets(_G1849,3),_G1838,_G1856)])]
      Process goal tree at time 1
      New: [goal(3,[[happens(bears_witness(you),1,_G3231)]]),goal(2,[[happens(refuses(me),1,_G3027)]]),goal(1, [[happens(gets(me,0),1,_G2380),happens(gets(you,3),1,_G2380)]])]
      Goal is happens(bears_witness(you),1,_G3231)
      . Result/resolvent is []
      Goal is happens(refuses(me),1,_G3027)
      . Result/resolvent is []
      Goal is happens(gets(me,0),1,_G2380)
      . Result/resolvent is [happens(gets(you,3),1,2)]
      Goal is happens(gets(you,3),1,2)
      . Result/resolvent is []

     ----- time is now 2 -----

...


     ----- time is now 5 -----

     Events and actions from 4 to 5 are [gets(you,0),gets(me,3),refuses(you),bears_witness(me)]

     State at time 5 is [total_years_in_jail(you,6),total_years_in_jail(me,6)]

     Process reactive rules at time 5
     Old:  [reactive_rule([happens(bears_witness(_G117),_G114,_G115),happens(refuses(_G126),_G114,_G115)],[happens(gets(_G117,0),_G115,_G133),happens(gets(_G126,3),_G115,_G133)]),reactive_rule([happens(bears_witness(_G86),_G83,_G84),happens(bears_witness(_G95),_G83,_G84),other(_G86,_G95)],[happens(gets(_G86,2),_G84,_G108)]),reactive_rule([happens(refuses(_G55),_G52,_G53),happens(refuses(_G64),_G52,_G53),other(_G55,_G64)],[happens(gets(_G55,1),_G53,_G77)]),reactive_rule([happens(refuses(_G34),_G31,_G32),other(_G39,_G34)],[happens(refuses(_G39),_G32,_G47)]),reactive_rule([happens(bears_witness(_G13),_G10,_G11),other(_G18,_G13)],[happens(bears_witness(_G18),_G11,_G26)])]
     New:  [reactive_rule([happens(bears_witness(_G13),_G10,_G11),other(_G18,_G13)],[happens(bears_witness(_G18),_G11,_G26)]),reactive_rule([happens(refuses(_G34),_G31,_G32),other(_G39,_G34)],[happens(refuses(_G39),_G32,_G47)]),reactive_rule([happens(refuses(_G55),_G52,_G53),happens(refuses(_G64),_G52,_G53),other(_G55,_G64)],[happens(gets(_G55,1),_G53,_G77)]),reactive_rule([happens(bears_witness(_G86),_G83,_G84),happens(bears_witness(_G95),_G83,_G84),other(_G86,_G95)],[happens(gets(_G86,2),_G84,_G108)]),reactive_rule([happens(bears_witness(_G117),_G114,_G115),happens(refuses(_G126),_G114,_G115)],[happens(gets(_G117,0),_G115,_G133),happens(gets(_G126,3),_G115,_G133)])]
     Process goal tree at time 5
     New: [goal(15,[[happens(bears_witness(you),5,_G4702)]]),goal(14,[[happens(refuses(me),5,_G4498)]]),goal(13,[[happens(gets(me,0),5,_G3851),happens(gets(you,3),5,_G3851)]])]
     Goal is happens(bears_witness(you),5,_G4702)
     . Result/resolvent is []
     Goal is happens(refuses(me),5,_G4498)
     . Result/resolvent is []
     Goal is happens(gets(me,0),5,_G3851)
     . Result/resolvent is [happens(gets(you,3),5,6)]
     Goal is happens(gets(you,3),5,6)
     . Result/resolvent is []

      Time is up. Unsolved goals: 
     []
     ** 5 cycles took 0.021470 seconds **
     true.



If or when this output is too verbose, either configure your shell to save it or use in SWI-Prolog's `protocol(File)` command. 
