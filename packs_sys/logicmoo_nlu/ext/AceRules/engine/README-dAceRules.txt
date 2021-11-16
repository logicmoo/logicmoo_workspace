---+ dAceRules

---++ Requirements

* AceRules and everything that is required for AceRules
* defeasible-rules and everything that is required for defeasible-rules
* dAceRule executable must be in AceRules-master/engine folder together with the scripts acerules-answers.pl, acerules-rules.pl, run_acerules_answers.pl, run_acerules_rules.pl, plus modified version of acerules_processor.pl
* defeasible-rules (and ape) must be in AceRules-master folder
* clingo must be in PATH variable
* Permission to generate temp files
* java

---++ Shell Command

Run using:

java -jar dAceRules-assembly-0.0.0.jar inputFile

Run 

java -jar dAceRules-assembly-0.0.0.jar -h

for further options.

--++ Examples

See examples in AceRules-master/engine/examples-defeasible folder.




