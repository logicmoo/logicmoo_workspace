# Logic Production Systems (LPS)#
Welcome to "LPS Corner", a repository to support the development of LPS engines, examples and related tools.

## Please notice new Syntax at

[this wiki page](https://bitbucket.org/lpsmasters/lps_corner/wiki/Syntax).

## What is LPS? ##

“Logic-based Production System" is a new computer language that combines the characteristics of an imperative programming language with those of a declarative database and knowledge representation language. It is the result of over a decade of research led by [Bob Kowalski](https://www.doc.ic.ac.uk/~rak/) and [Fariba Sadri](https://www.doc.ic.ac.uk/~fs/) at Imperial College London. 

The kernel of LPS consists of a database, together with reactive rules of the form ***if*** *antecedent* ***then*** *consequent*. The database changes destructively in response to actions and external events, according to a domain-specific causal theory. Computation consists in making the reactive rules true, by performing actions to make the *consequent* true whenever the *antecedent* becomes true. In addition, LPS includes Prolog-like logic programs both to recognise when *antecedents* become true and to generate plans of actions that make *consequents* true.

Slide presentation at [1st CLOUT workshop 2017](https://bitbucket.org/lpsmasters/lps_corner/raw/930d3e0b15e8477ff941ddc0ca7843083fba207e/doc/CLOUT_workshop_21Jan2017.pptx) gives a flavour of the language. 
Additional material about the kernel of LPS is available on the [RuleML wiki](http://wiki.ruleml.org/index.php/KELPS). More technical papers can be found on Bob Kowalski's home page (https://www.doc.ic.ac.uk/~rak/), which also includes a copy of his 2011 book, which presents some of the philosophy underlying LPS.

The implementation in this repository is an evolved version of the interpreter developed during David Wei's Master Thesis project at Imperial College, London, supervised by Fariba Sadri and Krysia Broda.

Current LPS syntax can be seen at  

### License and Copyright ###
All files in this repository are copyright Imperial College London and open source licensed with 3-clause BSD, except for files with other specific copyright and licensing notices.

Main authors include [Bob Kowalski](https://www.doc.ic.ac.uk/~rak/), [Fariba Sadri](https://www.doc.ic.ac.uk/~fs/), and also contributions by [Miguel Calejo](http://calejo.com) and [Jacinto Dávila](http://webdelprofesor.ula.ve/ingenieria/jacinto).

## Installing ##
### Core engine ###
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


### Core Engine + Prolog Studio ###
InterProlog Consulting's Prolog Studio adds somes facilities for LPS running over XSB Prolog: a semantic highlighting editor and a timeline visualizer. 
To install:

* Pull or download this whole repository into a new folder "LPS"
* Follow instructions at top of [InterProlog Studio](http://interprolog.com/wiki/index.php?title=Studio_Download_and_installation), including a recent XSB Prolog install

## Known Bugs

* TBD

## Other projects using LPS ##
See the robot game at [https://bobthesimplebot.github.io]()

## How to contribute ##
* See [lps_corner/RoadMap.md](https://bitbucket.org/lpsmasters/lps_corner/src/6a587ecd410fd81b8d799554dd86796af9c7e380/RoadMap.md?at=master) 
* Before you commit any changes to the LPS engine or tools, make sure you execute all tests with 'test_examples'.
* Please comment your code, and add appropriate copyright/license header
* To edit this and other future wiki documents: [learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### Writing tests ###
If you add a file to the examples directory and execute it with make_test, e.g. go(MyFile,[make_test]), a test results file MyFile.lpst will be generated. If the program behaves correctly with the current version of LPS, commit this file too. 

When you later execute go(MyFile,[run_test]), the program behavior will be compared with the previous test results; ditto when you run all tests, with 'test_examples'.

### Is there support? Who do I talk to? ###
There is no support for LPS at this point. The following contacts imply no obligation nor guarantee.

* Bug reporting and support for this implementation: 
	* lps@interprolog.com
* To find out more about LPS/KELPS in general: 
	* r.kowalski@imperial.ac.uk
	* f.sadri@imperial.ac.uk