# Installing LPS #

## LPS on SWISH (local web environment) ##
This is likely the most sensible option for most people.
### Local machine ###
For individual usage the following simplified instructions apply (valid for Windows, Mac and probably Linux too):

* Install the Development version of SWI-Prolog from <http://www.swi-prolog.org/download/devel>; make sure swipl is in your PATH
* clone <https://bitbucket.org/lpsmasters/lps_corner> (this repository)
	* either execute swish/INSTALL.sh or download and expand <https://www.dropbox.com/s/g8f3y1ag8v4afq5/bower_components.zip?dl=0> into swish/web
* either clone and build the latest swish from <https://github.com/SWI-Prolog/swish>, or simply download and extract <https://www.dropbox.com/s/fkn5omu302cc25t/swish.zip?dl=0> (based on swish 24217ef) 
* make sure that you end up with two directories lps_demo (with subdirectories doc,engine,etc.) and swish (with subdirectories client, etc.), side by side in the same directory D
* open a command shell window
* cd D/lps_demo/swish
* swipl -l user_module_file.pl -l ../../swish/server.pl -g server:server

You should get a message such as:

```
% Started server at http://localhost:3050/
```
Open that address with your browser, and you should now have the same functionality as <http://lpsdemo.interprolog.com> :
![Opening LPS on SWISH](https://bitbucket.org/lpsmasters/lps_corner/src/HEAD/doc/OpeningSWISH.png)

### Cloud server ###
Please see separate document for [installing on SWISH server](https://bitbucket.org/lpsmasters/lps_corner/src/HEAD/swish/INSTALL_server.md), e.g. for a Linux server on the cloud. 

## Core engine ##
To simply run LPS as a Prolog program:

* Pull or download this whole repository into a new folder "LPS"
* Install Prolog:
    * Install a recent (>= April 4 2016) **XSB Prolog**, such as the one in the [Prolog Studio site](http://interprolog.com/wiki/index.php?title=Studio_Download_and_installation#Installing_XSB_Prolog) for Windows; or pull the latest XSB from the [Sourceforge SVN site](https://sourceforge.net/p/xsb/src/HEAD/tree/trunk/XSB/) and build it yourself - NOT the official release as of July 2016.
    * ...and/or:
    * Install **SWI Prolog** from http://www.swi-prolog.org/download/stable
	
### Getting Started with a core engine ###

* Launch Prolog and consult 'your_path_to_LPS/engine/interpreter.P' by entering ['your_path_to_LPS/utils/psyntax.P']. after the prompt
?-
	* This launches the "new" rak interpreter; to use the old (soon to be deprecated) Wei interpreter consult instead '<your_path_to_LPS>/Wei-Engine/interpreter.P'
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

Previous examples in the old syntax can still be executed as follows: 


    $ swipl
    Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.2.0)
    Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
    SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
    and you are welcome to redistribute it under certain conditions.
    Please visit http://www.swi-prolog.org for details.

    For help, use ?- help(Topic). or ?- apropos(Word).

    ?- ['./Wei-engine/interpreter.P'].
    true.

    ?- go('./examples/dining_philosophers.lpsw').
	
    * Cycle 1 *
    ********************
    
    ********************
    
    Resolved goals:
    [happens(think(philosopher(4)),_G125,_G142),adjacent(_G148,philosopher(4),_G150),happens(pickup_forks(_G148,philosopher(4),_G150),_G155,_G156),tc(_G142=<_G155),happens(eat(philosopher(4)),_G156,_G171),happens(putdown_forks(_G148,philosopher(4),_G150),_G171,_G126),tc(1=<_G125)]
    [happens(think(philosopher(3)),_G97,_G236),adjacent(_G242,philosopher(3),_G244),happens(pickup_forks(_G242,philosopher(3),_G244),_G249,_G250),tc(_G236=<_G249),happens(eat(philosopher(3)),_G250,_G265),happens(putdown_forks(_G242,philosopher(3),_G244),_G265,_G98),tc(1=<_G97)]
    [happens(think(philosopher(2)),_G69,_G336),adjacent(_G342,philosopher(2),_G344),happens(pickup_forks(_G342,philosopher(2),_G344),_G349,_G350),tc(_G336=<_G349),happens(eat(philosopher(2)),_G350,_G365),happens(putdown_forks(_G342,philosopher(2),_G344),_G365,_G70),tc(1=<_G69)]
    [happens(think(philosopher(1)),_G41,_G666),adjacent(_G675,philosopher(1),_G677),happens(pickup_forks(_G675,philosopher(1),_G677),_G685,_G686),tc(_G666=<_G685),happens(eat(philosopher(1)),_G686,_G707),happens(putdown_forks(_G675,philosopher(1),_G677),_G707,_G42),tc(1=<_G41)]
    [happens(think(philosopher(0)),_G13,_G1058),adjacent(_G1067,philosopher(0),_G1069),happens(pickup_forks(_G1067,philosopher(0),_G1069),_G1077,_G1078),tc(_G1058=<_G1077),happens(eat(philosopher(0)),_G1078,_G1099),happens(putdown_forks(_G1067,philosopher(0),_G1069),_G1099,_G14),tc(1=<_G13)]

...

    * Cycle 11 *
    ********************
    
    ********************
    
    Resolved goals:
    []

    * Cycle 12 *
    * No more observations, terminating.
    
    ** 11 cycles took 0.029258 seconds (0.000112 expanding rules) **
    true.

If or when this output is too verbose, one can use some resources to catch it and store to check it later. See, for instance, in SWI-Prolog the `protocol(File)` command. 
