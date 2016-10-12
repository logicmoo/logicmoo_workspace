# Logic Production Systems (LPS)#
Welcome to "LPS Corner", a repository to support the development of LPS engines, examples and related tools.
## What is LPS? ##
“Logic-based Production System" is a new computer language that combines the characteristics of an imperative programming language with those of a declarative database and knowledge representation language. It is the result of over a decade of research led by [Bob Kowalski](https://www.doc.ic.ac.uk/~rak/) and [Fariba Sadri](https://www.doc.ic.ac.uk/~fs/) at Imperial College London. 

The kernel of LPS consists of a database, together with reactive rules of the form ***if*** *antecedent* ***then*** *consequent*. The database changes destructively in response to actions and external events, according to a domain-specific causal theory. Computation consists in making the reactive rules true, by performing actions to make the *consequent* true whenever the *antecedent* becomes true. In addition, LPS includes Prolog-like logic programs both to recognise when *antecedents* become true and to generate plans of actions that make *consequents* true.

Slide presentations at [RuleML 2012](http://www.slideshare.net/ruleml2012/kelps-lps-30-aug-2012) and [RuleML 2015](https://www.dropbox.com/s/yqy678k6y4k543k/KELPS%2022%20Jan%202015.ppt?dl=0) give a flavour of the language. Additional material about the kernel of LPS is available on the [RuleML wiki](http://wiki.ruleml.org/index.php/KELPS). More technical papers can be found on Bob Kowalski's home page (https://www.doc.ic.ac.uk/~rak/), which also includes a copy of his 2011 book, which presents some of the philosophy underlying LPS.

The implementation in this repository is an evolved version of the interpreter developed during David Wei's Master Thesis project at Imperial College, London, supervised by Fariba Sadri and Krysia Broda.

### License and Copyright ###
All files in this repository are copyright Imperial College London and open source licensed with 3-clause BSD, except for files with other specific copyright and licensing notices.

Main authors include [Bob Kowalski](https://www.doc.ic.ac.uk/~rak/), [Fariba Sadri](https://www.doc.ic.ac.uk/~fs/) and David Wei, and also contributions by [Miguel Calejo](http://calejo.com) and [Jacinto Dávila](http://webdelprofesor.ula.ve/ingenieria/jacinto).

## Installing ##
### Core engine ###
* Pull or download this whole repository into a new folder "LPS"
* Install Prolog:
    * Install a recent (>= April 4 2016) **XSB Prolog**, such as the one in the [Prolog Studio site](http://interprolog.com/wiki/index.php?title=Studio_Download_and_installation#Installing_XSB_Prolog) for Windows; or pull the latest XSB from the [Sourceforge SVN site](https://sourceforge.net/p/xsb/src/HEAD/tree/trunk/XSB/) and build it yourself - NOT the official release as of July 2016.
    * ...and/or:
    * Install **SWI Prolog** from http://www.swi-prolog.org/download/stable
	
### Getting Started with a core engine ###

* Launch Prolog and consult '\<your_path_to_LPS\>/Wei-Engine/interpreter.P' by entering  ['\<your_path_to_LPS\>/Wei-engine/interpreter.P']. after the prompt ?-
* Then enter, for example, go('\<your_path_to_LPS\>/examples/dining_philosophers.lpsw'). 
* Or for more informative output, enter, for example, go('\<your_path_to_LPS\>/examples/dining_philosophers.lpsw',[verbose]).
* Put any new program into the folder "\<your_path_to_LPS\>/examples", in order to run it.

A typical session would look like this: 

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
### Missing candidate actions
No matter the strategy (goal_strat(breadth) or not), the following generates a1,a1,... when it should generate only a1+a2 (simultaneous):

	reactive_rule( [happens(a3,_,_)],
	 [happens(a1,T1,T2),happens(a2,T1,T2)] ).
### missing solutions to fluent in composite event
Given the following

	 goFromTo(Here,There,T1,T2) :- 
	not killed(robot,T1), something_failing.
	 goFromTo(Here,There,T1,T3) :- 
	not killed(robot,T1), it_never_gets_here.


### timeless predicate looping
l_timeless(foo,[foo]).
This will cause a nonterminating computation. In a future syntax we should probably use 'prolog' for the body.

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