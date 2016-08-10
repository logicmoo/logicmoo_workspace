# Logic Production Systems (LPS)#
Welcome to "LPS Corner", a repository to support the development of LPS engines, examples and related tools.
## What is LPS? ##
“Logic-based Production System" is a new computer language that combines the characteristics of an imperative programming language with those of a declarative database and knowledge representation language. It is the result of over a decade of research led by [Bob Kowalksi] (https://www.doc.ic.ac.uk/~rak/) and [Fariba Sadri](https://www.doc.ic.ac.uk/~fs/) at Imperial College London. The kernel of LPS consists of a database, together with reactive rules of the form ***if*** *antecedent* ***then*** *consequent*. The database changes destructively in response to actions and external events, according to a domain-specific causal theory. Computation consists in making the reactive rules true, by performing actions to make the *consequent* true whenever the *antecedent* becomes true. In addition, LPS includes Prolog-like logic programs both to recognise when *antecedents* become true and to generate plans of actions that make *consequents* true.Slide presentations at [RuleML 2012](http://www.slideshare.net/ruleml2012/kelps-lps-30-aug-2012) and [RuleML 2015](https://www.dropbox.com/s/yqy678k6y4k543k/KELPS%2022%20Jan%202015.ppt?dl=0) give a flavour of the language. Additional material about the kernel of LPS is available on the [RuleML wiki](http://wiki.ruleml.org/index.php/KELPS). More technical papers can be found on Bob Kowalksi's [home page] (https://www.doc.ic.ac.uk/~rak/), which also includes a copy of his 2011 book, which presents some of the philosophy underlying LPS.The implementation in this repository is an evolved version of the interpreter developed during David Wei's Master Thesis project at Imperial College, London, supervised by Fariba Sadri and Krysia Broda.
### License and Copyright ###
All files in this repository are copyright Imperial College London and open source licensed with 3-clause BSD, except for files with other specific copyright and licensing notices.

Main authors include [Bob Kowalksi] (https://www.doc.ic.ac.uk/~rak/), [Fariba Sadri](https://www.doc.ic.ac.uk/~fs/) and David Wei, and also contributions by [Miguel Calejo](http://calejo.com) and [Jacinto Dávila](http://webdelprofesor.ula.ve/ingenieria/jacinto).
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
	* r.kowalski@imperial.ac.uk	* f.sadri@imperial.ac.uk


