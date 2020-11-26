=================================================================================
                                 VerbAtlas 1.0
                             http://verbatlas.org

               Andrea Di Fabio, Simone Conia and Roberto Navigli

               Sapienza NLP Group, Sapienza University of Rome
                             http://nlp.uniroma1.it

               Web site and resource maintenance by Babelscape
                             http://babelscape.com
=================================================================================


VerbAtlas is a novel large-scale manually-crafted semantic resource for
wide-coverage, intelligible and scalable Semantic Role Labeling.
The goal of VerbAtlas is to manually cluster WordNet synsets that share similar
semantics into a set of semantically-coherent frames.

VerbAtlas is licensed under the CC BY-NC-SA 4.0 License.


=================================================================================
PACKAGE CONTENTS
=================================================================================


* README.txt (this file);
* LICENSES.txt (terms and conditions for the files provided in this package)
* VERBATLAS_LICENSE.txt (terms and conditions of the CC BY-NC-SA 4.0 License);
* WORDNET_LICENSE.txt (original license from WordNet 3.0);
* BABELNET_LICENSE.txt (original license from BabelNet 4.0);
* VerbAtlas-1.0/ (contains the files for VerbAtlas 1.0) 
    * VA_frame_ids.tsv       (name and id of each VerbAtlas frame)
    * VA_va2pas.tsv          (the argument structure of each VerbAtlas frame)
    * VA_bn2va.tsv           (the synsets in each VerbAtlas frame)
    * VA_preference_ids.tsv  (name, id, reference synset for each preference)
    * VA_bn2sp.tsv           (preferences for each synset)
    * VA_bn2shadow.tsv       (shadow arguments for each synset)
    * VA_bn2implicit.tsv     (implicit arguments for each synset)
    * pb2va.tsv              (mapping from PropBank to VerbAtlas)
    * bn2wn.tsv              (one-to-one mapping from BabelNet to WordNet)
    * wn2lemma.tsv           (mapping from WordNet synset to lemmas)
    * wn2sense.tsv           (mapping from WordNet synset to WordNet sense key)

Every file in this package is licensed under the CC BY-NC-SA 4.0 License but 
wn2lemma.tsv and wn2sense.tsv, which are licensed under the WordNet 3.0 License,
and bn2wn.tsv, which is licensed under the BabelNet 4.0 license.


=================================================================================
RESOURCES
=================================================================================


VerbAtlas relies on:

* WordNet 3.0 (https://wordnet.princeton.edu/download/current-version)
* BabelNet 4.0.1 (http://babelnet.org)
* Proposition Bank I (PropBank - http://propbank.github.io/)   


=================================================================================
FORMAT
=================================================================================


1. VA_frame_ids.tsv
Each line contains the ID and the name of a VerbAtlas frame, separated by a tab.
For example:

    va:0001f    TOLERATE
    va:0002f    OBEY
    va:0003f    CARRY_TRANSPORT
    ...

2. VA_va2pas.tsv
Each line contains the ID of a VerbAtlas frame and its argument structure, where
each element is separated by a tab.
For example:

    va:0001f    Agent    Theme    Beneficiary    Attribute
    va:0002f    Agent    Theme
    va:0003f    Agent    Theme    Destination    Source    Instrument ...
    ...

3. VA_bn2va.tsv
Each line contains a BabelNet synset ID and its corresponding VerbAtlas frame ID,
separated by a tab.
For example:

    bn:00082138v    va:0001f
    bn:00082200v    va:0001f
    bn:00082203v    va:0001f
    ...

4. VA_preference_ids.tsv
Each line contains the ID of a VerbAtlas selectional preference, its reference
BabelNet synset ID, and its name.
For example:

    va:0001p    bn:00000467n    absorbent
    va:0002p    bn:00000492n    abstraction
    va:0003p    bn:00000902n    acid
    ...

5. VA_bn2sp.tsv
Each line contains the selectional preferences for each BabelNet synset,
separated by a tab. If a role supports multiple selectional preferences, they
are separated by a pipe (|).
For example:

    bn:00015081n    Patient    va:0084p|va:0083p    Instrument    va:0084p|va:0083p
    bn:00082116v    Agent    va:0081p|va:0103p    Topic    va:0106p|va:0026p    ...
    bn:00082117v    Agent    va:0084p|va:0083p    ...
    ...

6. VA_bn2shadow.tsv
Each line contains the shadow arguments, if any, for each BabelNet synset ID.
A synset may support multiple shadow arguments in different roles.
For example:

    bn:00082230v    Theme    bn:00070772n
    bn:00082124v    Stimulus    bn:00030466n
    bn:00082266v    Result    bn:00098546a    Instrument    bn:00000902n
    ...

7. VA_bn2implicit.tsv
Each line contains the implicit arguments, if any, for each BabelNet synset ID.
A synset may support multiple implicit arguments in different roles.
A role may have multiple implicit arguments, separated by a pipe (|).
For example:

    bn:00082144v    Patient    bn:00011769n
    bn:00082205v    Theme    bn:00067181n    Source    bn:00021015n
    bn:00082162v    Patient    bn:00035378n|bn:00035596n|bn:00036686n
    ...

8. pb2va.tsv
A mapping from PropBank to VerbAtlas. Each line maps a PropBank predicate sense
and its corresponding argument structure to a VerbAtlas frame and its argument
structure as follows:
<PB predicate sense>    <VA frame>    <PB role>    <VA role>    ...     
For example:

    abandon.01    va:0255f    A0    Agent    A1    Theme    A2    Attribute
    abandon.02    va:0016f    A0    Agent    A1    Patient    A2    Result
    abandon.03    va:0253f    A0    Agent    A1    Patient    A2    Recipient
    ...

9. bn2wn.tsv
A mapping from BabelNet synset to WordNet synset for verbs. Each line contains a
BabelNet synset ID and its corresponding WordNet synset ID, separated by a tab.
For example:

    bn:00082116v    wn:00865776v
    bn:00082117v    wn:02168378v
    bn:00082118v    wn:02228031v
    ...

10. wn2lemma.tsv
A mapping from a WordNet synset to a lemma. Each line contains a WordNet synset
ID and a lemma, separated by a tab. NOTE: the same WordNet synset ID may appear
in multiple lines. NOTE: the same lemma may appear in multiple lines.
For example:

    wn:00001740v    breathe
    wn:00001740v    take-a-breath
    wn:00002325v    respire
    wn:00002573v    respire
    ...

11. wn2sense.tsv
A mapping from a WordNet synset to a WordNet sense key. Each line contains a 
WordNet synset ID and a WordNet sense key, separated by a tab. NOTE: the same
WordNet synset ID may appear in multiple lines.
For example:

    wn:00001740v    breathe%2:29:00::
    wn:00001740v    take_a_breath%2:29:00::
    wn:00002325v    respire%2:29:01::
    wn:00002573v    respire%2:29:02::
    ...


=================================================================================
REFERENCE PAPER
=================================================================================


When using this resource, please refer to the following paper:

    Andrea Di Fabio, Simone Conia and Roberto Navigli

    "VerbAtlas: a Novel Large-Scale Verbal Semantic Resource
    and Its Application to Semantic Role Labeling"

    In Proceedings of the 2019 Conference on Empirical Methods in Natural 
    Language Processing and the 9th International Joint Conference on 
    Natural Language Processing (EMNLP-IJCNLP),
    Hong Kong, China, November 3-7, 2019, pages 627-637.


=================================================================================
CONTACTS
=================================================================================


If you have any enquiries, please contact:

Andrea Di Fabio - Sapienza Università di Roma
(difabio [at] di [dot] uniroma1 [dot] it)

Simone Conia - Sapienza Università di Roma
(conia [at] di [dot] uniroma1 [dot] it)

Roberto Navigli - Sapienza Università di Roma
(navigli [at] di [dot] uniroma1 [dot] it)


