================================
SemLink VerbNet/PropBank Mapping
================================

  Version: 1.2
      URL: http://verbs.colorado.edu/~edloper/semlink/
 Citation: Edward Loper, Szu ting Yi, and Martha Palmer. 2007.
           Combining lexical resources: Mapping between propbank and
           verbnet. In Proceedings of the 7th International Workshop
           on Computational Linguistics, Tilburg, the Netherlands.

Updated by Kevin, 4/4/2013

Overview
--------
The SemLink mapping between VerbNet and PropBank consists of two
parts: a lexical mapping and a token mapping.  The lexical mapping
specifieds the potential mappings between PropBank and VerbNet for a
given word; but it does not specify which of those mappings should be
used for any given occurrence of the word.  The token mapping provides
the correct mapping between arguments for every predicate in the
PropBank corpus.  

In some cases, a predicate from PropBank will not exist in VerbNet;
will not exist in the correct sense; or will have arguments without
corresponding roles in VerbNet.  In these cases, the VerbNet role is
listed as 'None' and the argument is left in its unmapped (ARGn) form.

Type Mapping
------------
The type mapping is provided as a single xml file, containing entries
of the form:

	<predicate lemma="muzzle">
		<argmap pb-roleset="muzzle.01" vn-class="9.9">
			<role pb-arg="0" vn-theta="Agent" />
			<role pb-arg="1" vn-theta="Destination" />
			<role pb-arg="2" vn-theta="Theme" />
		</argmap>
		<argmap pb-roleset="muzzle.01" vn-class="22.4">
			<role pb-arg="0" vn-theta="Agent" />
			<role pb-arg="1" vn-theta="Patient" />
		</argmap>
	</predicate>

Each <predicate> entry describes a single verb lemma, which are verbs 
found in VerbNet and PropBank.  <predicate> entries also contain one
or more <argmap> entries.  Each <argmap> entry describes the mapping
between arguments for a specific (PropBank roleset, VerbNet class)
pair, using one or more <role> entries.  Each <role> entry describes
the mapping between PropBank ARGn labels and VerbNet thematic roles
for a single argument role.  In the above example, when the "muzzle"
verb is used in the sense described by VerbNet class 9.9, the PropBank
and VerbNet roles map as follows:

   ================================
   PropBank       VerbNet
   --------------------------------
    ARG0    <->    Agent
    ARG1    <->    Destination
    ARG2    <->    Theme
   ================================

History
-------
Relase 1.2 removed a handful of errors and improved the dataset. 
Release 1.0 contained a bug that caused some of the verbs that are not
contained in VerbNet at all to get improper annotations in the
vnpbprop.txt file.  This bug has now been fixed.
