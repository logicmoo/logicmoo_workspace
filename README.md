# Logic Production Systems (LPS)#
Welcome to "LPS Corner", a repository to support the open source development of LPS engines, examples and related tools. Most of the current version is contributed by LogicalContracts from the open source component of its [Logical Contracts Server](http://logicalcontracts.com/server/) product.

## Major release, March 2, 2018 ##
This is the first update since Summer 2017, and incorporates most improvements developed by LogicalContracts:

* LPS background servers
* LPS program hibernation / restore
* External (Prolog) fluents and events
* Simplified "fluent editing" actions
* Simulated real (calendar) time
* Bug fixes

.. and more, please see preliminary documentation at the [LogicalContracts site](http://logicalcontracts.com/server/).

## What is LPS? ##

“Logic-based Production System" is a new computer language that combines the characteristics of an imperative programming language with those of a declarative database and knowledge representation language. It is the result of over a decade of research led by [Bob Kowalski](https://www.doc.ic.ac.uk/~rak/) and [Fariba Sadri](https://www.doc.ic.ac.uk/~fs/) at [Imperial College London](http://lps.doc.ic.ac.uk). 

The kernel of LPS consists of a database, together with reactive rules of the form ***if*** *antecedent* ***then*** *consequent*. The database changes destructively in response to actions and external events, according to a domain-specific causal theory. Computation consists in making the reactive rules true, by performing actions to make the *consequent* true whenever the *antecedent* becomes true. In addition, LPS includes Prolog-like logic programs both to recognise when *antecedents* become true and to generate plans of actions that make *consequents* true.

See the [Rule-ML 2017 Tutorial](https://bitbucket.org/lpsmasters/lps_corner/src/master/doc/RuleML_2017/) for an introduction to the language. The paper [Reactive Computing as Model Generation](http://www.doc.ic.ac.uk/%7Erak/papers/LPS%20revision.pdf) provides its declarative and operational semantics. 

Additional material about the kernel of LPS is available on the [RuleML wiki](http://wiki.ruleml.org/index.php/KELPS). See also the slide presentation at [1st CLOUT workshop 2017](https://bitbucket.org/lpsmasters/lps_corner/raw/930d3e0b15e8477ff941ddc0ca7843083fba207e/doc/CLOUT_workshop_21Jan2017.pptx). Other technical papers and bibliographic references can be found on Bob Kowalski's home page at <https://www.doc.ic.ac.uk/~rak/>, which also includes a copy of his 2011 book, which presents some of the philosophy underlying LPS.

## Trying LPS online ##
See and try the language at <http://demo.logicalcontracts.com>: 

![Opening LPS on SWISH](https://bitbucket.org/repo/z4LaLk/images/1779163991-Opening_lpsdemo.png)
Start with menu Examples / First Steps with LPS and links therein, as well as "LPS Examples". Refer to the LPS [syntax](https://bitbucket.org/lpsmasters/lps_corner/wiki/Syntax).

For regular or intensive usage you can install LPS on your machine, using a number of alternatives:

## Installing your own ##

Besides trying it rightway on your browser at <http://demo.logicalcontracts.com>, you can install LPS:

* as your own local instance of the lpsdemo web application, on Windows, Mac, Linux
* as a server in your own private or university cloud
* as a SWI Prolog program

Please refer to the [INSTALL](https://bitbucket.org/lpsmasters/lps_corner/src/HEAD/INSTALL.md) instructions.

XSB Prolog support is deprecated, but still available in the [Aug 15, 2017 version](https://bitbucket.org/lpsmasters/lps_corner/commits/be54e22ffd3fdb5fc80e57dd3c4fe4f3672e415a)

### License and Copyright ###
All files in this repository are copyright Imperial College London and open source licensed with 3-clause BSD, except for files with other specific copyright and licensing notices, all being some sort of open source. 

Main authors include [Bob Kowalski](https://www.doc.ic.ac.uk/~rak/), [Fariba Sadri](https://www.doc.ic.ac.uk/~fs/), [Miguel Calejo](http://calejo.com) and also contributions by [Jacinto Dávila](http://webdelprofesor.ula.ve/ingenieria/jacinto). The engine implementation in this repository is an evolved version of the core interpreter developed during David Wei's Master Thesis project at Imperial College, London, supervised by Fariba Sadri and Krysia Broda. 


### Core Engine + Prolog Studio (deprecated) ###
InterProlog Consulting's Prolog Studio adds somes facilities for LPS running over XSB Prolog: a semantic highlighting editor and a timeline visualizer. 
To install:

* Pull or download this whole repository into a new folder "LPS"
* Follow instructions at top of [InterProlog Studio](http://interprolog.com/wiki/index.php?title=Studio_Download_and_installation), including a recent XSB Prolog install

## Known Bugs

* TBD

## Other projects using LPS ##
See the robot game at [https://bobthesimplebot.github.io]()

## How to contribute ##
* See [lps_corner/RoadMap.md](https://bitbucket.org/lpsmasters/lps_corner/src/HEAD/RoadMap.md) 
* Before you commit any changes to the LPS engine or tools, make sure you execute all tests with 'interpreter:test_examples'.
* Please comment your code, and add appropriate copyright/license header
* To edit this and other future wiki documents: [learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### Writing tests ###
If you add a file to the examples directory and execute it with the make_test option, e.g. ```go(MyFile,[make_test])```, a test results file MyFile.lpst will be generated. If the program behaves correctly with the current version of LPS, commit this file too. 

When you later execute go(MyFile,[run_test]), the program behavior will be compared with the previous test results; ditto when you run all tests, with 'interpreter:test_examples'.

### Is there support? Who do I talk to? ###
The following contacts imply no obligation nor guarantee:

* Bug reporting and occasional support: 
	* lps@interprolog.com
* To find out more about LPS/KELPS in general: 
	* r.kowalski@imperial.ac.uk
	* f.sadri@imperial.ac.uk