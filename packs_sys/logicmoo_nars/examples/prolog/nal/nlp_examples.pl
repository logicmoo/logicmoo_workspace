:- [library(nars/nars)].

end_of_file.

Give the system three judgments, with the default truth-value ?1, 0.9?:

(1)     {“cat” * cat} ? stand-for	
	[“cat” stands for cat.]

(2)     {“fish” * fish} ? stand-for	
	[“fish” stands for fish.]

(3)     {{“cat” * “eat” * “fish”} * ((cat * fish) ? food)} ? stand-for 	
	[“cat eat fish” means that cats have fish for food.]

From them, the system can uses the induction rule to derive generalized knowledge while replacing the constant terms in the premises by variable terms:

(4)     {{(stand-for / _ #X) * “eat” * “fish”} * ((#X * fish) ? food)} ? stand-for  ?1, 0.45?
	[Derived from (1) and (3) by induction, where #X is a variable term, and (stand-for / _ #X) the term that stands for #X]

(5)    {{“cat” * “eat” * (stand-for / _ #Y)} * ((cat  * #Y) ? food)} ? stand-for  ?1, 0.45?
	[Derived from (2) and (3) by induction, where #Y is a variable term, and (stand-for / _ #Y) the term that stands for #Y]

(6)    {{(stand-for / _ #X) * “eat” * (stand-for / _ #Y)} * ((#X * #Y) ? food)} ? stand-for  ?1, 0.29?
	[Derived from (1) and (5), or (2) and (4), using the induction rule again]


These conclusions can be considered as “hypotheses”, which are beliefs with relatively low confidence. They provide the system with linguistic knowledge about the meaning of templates “#X eat fish”, “cat eat #Y”, and “#X eat #Y”, respectively, where a term with the prefix ‘#’ is a variable term that can be instantiated by various words or phrases. If the same template is produced from distinct evidence repeatedly, the belief will be strengthened, i.e., get a higher confidence value. 

Assume two more judgments are given to the system, with the default truth-value:

(7)     {“dog” * dog} ? stand-for	
	[“dog” stands for dog.]

(8)     {“meat” * meat} ? stand-for	
	[“meat” stands for meat.]

From them and the above hypotheses, the system can draw conclusions about novel sentences and compound terms:

(9)     {{“dog” * “eat” * “fish”} * ((dog * fish) ? food)} ? stand-for  ?1, 0.41?
	[Derived from (4) and (7) by deduction.]

(10) {{“cat” * “eat” * “meat”} * ((cat * meat) ? food)} ? stand-for  ?1, 0.41?
	[Derived from (5) and (8) by deduction.]

(11) {{“dog” * “eat” * “meat”} * ((dog * meat) ? food)} ? stand-for  ?1, 0.23?
	[Derived from (6), (7), and (8) by deduction twice.]

The above results (9), (10), and (11) can be used for both the understanding and the generation of novel sentences which are not in the training materials.

