# Logic Production Systems (LPS)#
Welcome to "LPS Corner", a repository to support the development of LPS engines, examples and related tools.
## What is LPS? ##
"Logic Production Systems" is a new agent-oriented programming language that combines reactive behaviour with goal-oriented planning. It is the result of over a decade of research led by Bob Kowalski and Fariba Sadri at Imperial College London.  BLA BLA 

For an introduction please see [paper1](URL1) or [paper2](URL2). More references available from Bob Kowalksi's [home page] (https://www.doc.ic.ac.uk/~rak/)

The implementation in this repository is an evolved version of the interpreter developed during [David Wei's Master Thesis](URL?) project at Imperial College, London, supervised by [Fariba Sadri](https://www.doc.ic.ac.uk/~fs/).

### License and Copyright ###
All files in this repository are copyright Imperial College London and open source licensed with [3-clause BSD](https://opensource.org/licenses/BSD-3-Clause), except for files with other specific copyright and licensing notices.

Main authors include Robert Kowalski, Fariba Sadri and David Wei, and also contributions by Miguel Calejo and [Jacinto Dávila](http://webdelprofesor.ula.ve/ingenieria/jacinto).

## Installing ##
### Core engine ###

* Pull or download this whole repository into a new folder "LPS"
* Install Prolog:
    * Install a recent (>= April 4 2016) **XSB Prolog**, such as the one in the [Prolog Studio site](http://interprolog.com/wiki/index.php?title=Studio_Download_and_installation#Installing_XSB_Prolog) for Windows; or pull the latest XSB from the [Sourceforge SVN site](https://sourceforge.net/p/xsb/src/HEAD/tree/trunk/XSB/) and build it yourself - NOT the official release as of July 2016.
    * ...and/or:
    * Install **SWI Prolog** from http://www.swi-prolog.org/download/stable
* Launch Prolog and consult 'your_path/LPS/Wei-Engine/interpreter.P'

### Core Engine + Prolog Studio ###
InterProlog Consulting's Prolog Studio adds somes facilities for LPS running over XSB Prolog: a semantic highlighting editor and a timeline visualizer. 
To install:

* Pull or download this whole repository into a new folder "LPS"
* Follow instructions at top of [InterProlog Studio](http://interprolog.com/wiki/index.php?title=Studio_Download_and_installation), including a recent XSB Prolog install

## Known Bugs
### Missing candidate actions
No matter the strategy (goal_strat(breadth) or not), the following generates a1,a1,... when it should generate only a1+a2 (simultaneous):

	reactive_rule( [happens(cond3,_,_)],
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
* Before you commit any changes to the LPS engine or tools, make sure you execute all tests with 'test_examples'.
* Please comment your code, and add appropriate copyright/license header
* To edit this and other future wiki documents: [learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### Writing tests ###
If you add a file to the examples directory and execute it with make_test, e.g. go(MyFile,[make_test]), a test results file MyFile.lpst will be generated. If the program behaves correctly with the current version of LPS, commit this file too. 

When you later execute go(MyFile,[run_test]), the program behavior will be compared with the previous test results; ditto when you run all tests, with 'test_examples'.

### Is there support? Who do I talk to? ###
There is no support for LPS at this point. The following contacts imply no obligation nor guarantee.

* Repo owner or admin: lps@interprolog.com
* Bugs and engine support: lps@interprolog.com