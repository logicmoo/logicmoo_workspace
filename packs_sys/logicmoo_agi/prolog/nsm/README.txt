---+                                  NSM-DALIA 

@version 1.0
@author Francesco Zamblera
@license gnu gpl

---++ Overview
NSM-DALIA is a package for parsing and generating the subset of	
a natural language known as its *|natural semantic metalanguage|*.

Version 1.0 comes with a command-line interface: users write their
commands at a prompt; commands are executed and results printed. 
Then NSM-DALIA waits for another command.

With NSM-DALIA you can:

* parse a sentence into an NSM-PROLOG formula;
* generate a sentence from an NSM-PROLOG formula;
* translate a sentence from a language-particular NSM into another;
* analyse, generate and translate whole texts, written according to the
standard conventions of the NSM community and stored in files;
* write an NSM dictionary (a dictionary of a language in which entries
are defined in NSM);
* write the grammar of (the NSM of) a language.

---++ Starting NSM-DALIA

SWI-PROLOG must be installed in your system for NSM-DALIA to  work.

You can start NSM-DALIA on Windows by double-clicking on the file "dalia_cline.pl".

On Linux, open a terminal and switch to the  NSM-DALIA directory; then start SWI-PROLOG
(the command should be simply "prolog"), and, when SWI-PROLOG is loaded, type
['dalia_cline']. at the PROLOG prompt.

---++ Starting Documentation Server

By starting the file dalia_cline_doc.pl, the system runs a documentation
server, to which you can connect with your browser at http://localhost:4000

---++ NSM-source files

The file nsm_files.txt is a short tutorial on writing NSM-source files.


---++ Demo

The demo directory contains some parsing and generation sample files.

1. tpi_sentences.txt is an example of parsing Tok Pisin sentences.
Here is a run:

==
DALIA> l(tpi).
 ** tpi:e language module loaded.
 ** Current L1 set to tpi:e

DALIA> pf("demo/tpi_sentences.txt").

ct(s, s(e, e, e, e, e, p(think, [e:sp(e, e, e, [], d(me)), o:e, t:e]), e, e)).
ct(s, s(e, e, e, e, e, p(know, [e:sp(e, e, e, [], d(me))]), e, e)).
ct(s, s(e, e, e, e, e, p(want, [e:sp(e, e, e, [], d(me))]), e, e)).

...
==

2. eng2tpi.txt is an example of translation from English to Tok Pisin.
You find instruction on how to process such files in the file itself.


