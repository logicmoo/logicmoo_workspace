---+ AceRules
 
Tobias Kuhn, 2008-11-24.


---++ Requirements

* SWI Prolog is required that is available from http://www.swi-prolog.org/ . Version 5.6.61 is known
  to work with AceRules.
* Compiled version of APE in ../ape. If APE is at a different location then the parameter in
  parameters.pl has to be changed. APE is available under LGPL license from the Attempto website:
  http://attempto.ifi.uzh.ch/site/downloads/ . Version 6.0-081023 is known to work with AceRules.
* If using the stable model semantics, the programs Smodels and Lparse need to be installed and the
  path of their executables need to be in the path environment variable when running AceRules. These
  programs are available under the GPL license and can be downloaded from
  http://www.tcs.hut.fi/Software/smodels/ . Smodels 2.33 and Lparse 1.1.1 are known to work with
  AceRules.


---++ Interface Modules

* The interface module acerules_processor.pl uses Prolog terms for input and output.
* run_acerules.pl is an interface module that uses files or pipes as input and output.


---++ Architecture

Basically, an AceRules program is processed in three phases:

1. Parser
2. Interpreter
3. Verbalizer

The interpreter module is exchangeable. AceRules allows to choose between different semantics.
Currently, two interpreter modules are available for AceRules:

1. Courteous Logic Programs
2. Stable Model Semantics (with and without strong negation)


---++ Shell Command

Important Notice:
The SWI Prolog command is considered "swipl". If this is different on your machine (e.g. "pl") then
the script won't work, unless you change it.

The script "acerules" is used for invocation from a shell environment:

==
acerules [OPTIONS] INPUT [OUTPUT]
==

INPUT is the file containing the AceRules program.

OUTPUT is he file in which the output is written (standard output device is taken if this parameter
is omited).

OPTIONS: The following options are possible. They have to occur in this order and they cannot be
  condensed (i.e. write "-s -d" not "-sd"):

* "-s" or "--stable" for stable model semantics (without strong negation).
* "-g" or "--stable-strong" for stable model semantics with strong negation.
* "-c" or "--court" for courteous semantics. (Default)
* "-n" or "--normal" for normal output. (Default)
* "-d" or "--debug" for debug output.
* "-t" or "--trace" for trace output.
* "-a" or "--ace-trace" for trace output in ACE.
