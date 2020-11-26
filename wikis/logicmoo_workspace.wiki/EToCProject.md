(E2C)

* The critical paths to [[EToCProject]]:

	* [[SxxMachine]] + [[LarKc]]: Fastest method to develop, but will run too slow.
	* [[PopLog]]
	* [[SteelBankCommonLisp]] + something

* Dependencies:

	* depends([[EToCProject]],or(+([[SxxMachine]],[[LarKc]]),[[PopLog]],+([[SteelBankCommonLisp]],something))).
	* depends([[LarKc]], [[SxxMachine]]).
	* depends([[SxxMachine]], 'test to see if its ready').

* Use Cases:

	* Translating IRC logs into EmacsWikis.
	* Translating IRC logs into academic articles as part of Coauthor using GPT-2 over citeseer (generate-phrase (cyclify INPUT) #$RKFParaphraseMt).   We replace #$RKFParaphraseMt with AcademicParaphraseMt).
	* Parsing Math Knowledge from English into CycLanguage.
	* Translating .do and .notes files into Logic.
	* Read D&D Dungeon Master campaign notes and translate into logic for use with role-playing.
	* Reading text to index knowledge (for instance for the Free Life Planner).
	* Parsing text received by Alexa into commands.

* Notes:

[[ShopThree]] is going to be how i test if the [[EToCProject]] parse makes sense.

So the right way to [[EToCProject]] is through [[ScriptApplierModule]] / [[PlanApplierModule]]

[[PlanApplierModule]] needs [[ShopThree]].

[[ScriptApplierModule]] (and/or [[PlanApplierModule]]) creates a set of valid
stories given any context.. and tells the older [[EToCProject]] method
what it expects to parse

* Q/A

Q: Do you think one could use [[EToCProject]] to do [[MathematicalKnowledgeManagement]], extracting out math terminology?

A: It depends .. [[ScriptApplierModule]] / [[EToCProject]] is more likely to learn the math out of reading

p
Q: 6-14 20:47:11 <aindilis> do you have a recent semi-working version of [[EToCProject]] I could bootstrap my version from, or should I just be patient?

A: 2019-06-14 20:48:28 <dmiles> not really i definitely had saved off verisons but all them are on rusted out CDs or dead harddrives


* See Also:

[[FEToCProject]].

2019-11-15 14:20:56 <dmiles> e2c was mostly fixing cycorps code between
      versions that no longer runs,, i9 fixed almost 1/2 but it take aniother
      month to fix the rest
2019-11-15 14:21:10 <dmiles> then it take a month to expose it 
2019-11-15 14:21:38 <dmiles> expose it = write up docs on how to use it
2019-11-15 14:21:59 <dmiles> well build an applkication that lets peopel use
      the code etc
2019-11-15 14:22:25 <dmiles> hoping maybe once funding comes in i can hire
      somone from cycorp to work on it
2019-11-15 14:22:42 <dmiles> (instead of me)

