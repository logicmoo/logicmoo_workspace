
My ARC WIKI [ARC - XWiki](https://logicmoo.org/xwiki/bin/view/Main/ARC/)

[Chollet Youtube Videos discussing ARC](https://www.youtube.com/watch?v=mEVnu-KZjq4&t=523s)

Create an AI capable of solving reasoning tasks it has never seen before…

* Initial thought: 
 
This is a very unique sort of test that specifically looks for the type of reasoning humans do naturally.   Evidently machine learning tests very poorly at ARC . Specifically because it requires the understanding of visual individuation and relating them to mental abstractions.  The right way to approach the test according to the Author is a  “DSL” that produces the visual images.. Currently the SOTA with the best score out of 1000s of teams competing being 20%-30%..   I was already halfway world's top scores so should I dedicate a full 90 days towards getting to a score of 100%? 
 
* Not too hard for humans
    * Most humans can get 89% without trying too hard 
    * Trying hard they can get 100%
    * Supposedly a real test of precursor/proto-AGI
    * Humans can learn how to take the tests in under 3 minutes
    * … 
 
* Too hard for Machine Learning
    * SOTA 7.5%
    * No examples of what the machine needs to know already
    * Made very easy for ML to try (tests organized as Input/Output pairs)
    * … 
 
* Too hard for GOFAI
    * SOTA 20/28% (Public/Private)
    * Naive brute search is a million squared search paths 
    * Informed search (Search space cut down by humans making up paradigms) 
    * Designed ideally as DSL
    * … 
 
* Douglas MUARC program?
* (H)MUARC that Douglas is writing for 3 months  (H is for “Hardcoded”)
    * Am I overconfident in my skills?
    * [https://gitlab.logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/tree/master/packs_sys/logicmoo_agi/prolog/kaggle_arc](https://gitlab.logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/tree/master/packs_sys/logicmoo_agi/prolog/kaggle_arc)
    * Reached ML’s SOTA (7.5%) after 8 hours of devel
    * Reached the leaderboard SOTA (20%) after 60 devel hours
    * Will reach privates SOTA after 100 hours (28%)
    * BUT I think I can get 100% in 3 months of work 
 
* (G)MUARC allows to tweak the parameters that will “G”-generate the “H”
    * Can I get this to handle non vision problems? Word problems? Etc?
    * 160-320 hours  
 
* ALEPH-MUARC-I will examine “G” and “H” to try to write it’s very own (G)MUARC
    * Too large a task for ALEPH?
    * 160-320 hours  
 
* ALEPH-MUARC-II
*  

(H)MUARC Source code is organized into:

* [Kaggle_arc.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc.pl) - toplevel routines for invoking testing
* [Kaggle_arc_test_iface.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_test_iface.pl) - Interface for loading training and testing the results
* [Kaggle_arc_domaintypes.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_domaintypes.pl) - Intrinsic Types used by the system
* [Kaggle_arc_howdiff.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_howdiff.pl) - Shows how different Terms are different
* [Kaggle_arc_uniqueness.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_uniqueness.pl) - Shows how different Objects are unique
* [Kaggle_arc_physics.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_physics.pl) - Basics physics like gravity/rotation and distance
* [Kaggle_arc_imagens.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_imagens.pl)   - For producing non-novel images
* [Kaggle_arc_imageproc.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_imageproc.pl) -  Basic image manipulation
* [Kaggle_arc_individuation.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_individuation.pl) - Object Differentiation/Individualization
* [Kaggle_arc_interpreter.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_interpreter.pl) - Various DSL Interpreters used by the system
* [Kaggle_arc_learning.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_learning.pl) - Utils for different Terms are different
* [Kaggle_arc_object.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_object.pl) - Utils for Objects  tracking/creation)
* [Kaggle_arc_recognise.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_recognise.pl) - OGS-Util for searching Images
* [Kaggle_arc_symmetry.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_symmetry.pl) - Image processing that looks at symmetry 
* [Kaggle_arc_ui_ansi.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_ui_ansi.pl) - Code for the ANSI Terminal such as Image printing
* [Kaggle_arc_ui_html.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_ui_html.pl) - Code for HTTPD UI
* [Kaggle_arc_utils.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_utils.pl) - Code Utils not specific to ARC
* [Kaggle_arc_explaination.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_explaination.pl) - Explanation faculties (Debugging info for now)
* [Kaggle_arc.unused](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc.unused)  - Code no longer in active use by the system
* [Kaggle_arc_aleph.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_aleph.pl)  - For future experimentation with ALEPH
* [Kaggle_arc_alephlib.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_alephlib.pl) - Beginning of experimentation with ALEPH
* [Kaggle_arc_metagol.pl](https://github.com/logicmoo/logicmoo_workspace/blob/master/packs_sys/logicmoo_agi/prolog/kaggle_arc/kaggle_arc_metagol.pl) - For future experimentation with METAGOL

Active Daily Random Thoughts/Notes:  [https://docs.google.com/document/d/1ewzuNdL_GyYZiXLh5XJ6RTQFFcJtziaLJtIgUVJmYVk/edit](https://docs.google.com/document/d/1ewzuNdL_GyYZiXLh5XJ6RTQFFcJtziaLJtIgUVJmYVk/edit)


---

5/31/2022 

* Need to inform people waiting on LOGICMOO-AGI employment/contracts that our funding will probably come from the success of Douglas’ ARC code?
* Focused on a set of tests that only show me that the individuation strategy was sane. (TThis is ready for 6/9/2022 demo)

I don't think AGI can be created by upscaling AI.    But I can see where people with a different definition of AGI think this .. but they have a different definition of AGI... 

 Their definition of AGI is:
```
that a computer can do quite a few "important" tasks that humans (they) value 
 ... with a HINT of cross domain transfer
```
 

Although that is a reasonable goal .. it is not the definition of AGI.. 

 I think a better definition:
```
AGI is a system that is capable of learning to do ALL the things humans do
 ... with little or training so that mostly ALL things are cross domain transferred
```
 

Upscaling cannot convert the first thing into the second.  The most important part of AGI is it needs to "learn" on its own without "more training"   example: with only our minds .. we can sit there with our eyes closed and learn and discover new things .. This is without any more training.. our ability to be general comes from this ability.   


If this is what we want to simulate.. we actually have to simulate it !  rather than  proving "programs that cant simulate are still able to pass the same tests"  (are we supposed to play dumb and believe that that it must of simulated?)   


This "simulate" is a process that allows transfer learning .. .though transfer learning can happen in other ways .. such as in ontologies and KGs .. or even as most people are banking on that NNs back propagation will allow some types of transfer.

The main problem is training, so far no ones been able to come up with a type of training that would enable AGI.
  

Training  that "enables AGI" and  training  that allows "passing tests for AGI" are completely different things.

A system that "is an AGI" and a system that "passes tests only an AGI can" are completely different things as well. 

The whole idea of the Turing test was based on the idea that people are not clever enough to tell the difference and not clever enough to create a real test.  But, I think we are clever enough to create real tests..   


ARC is an example:
Chollet's ARC is a test that checks to see if what you learned in the training images will transfer (generalize) to the test image  

 ( AI2's ARC test does not test for AGI, it tests for AI..  

   Tests that can still be passed by memorization code (ML) and deduction code (GOFAI) .. not testing for any transferences )..   deduction and memorizations are really cool !   I love AI !  But AGI I love more  )


---

6/1/2022 

* Updated Wiki page (link header/footer of this) with higher level what the files do
* Should I tell Discord server about this work?
* Maybe I’ll stream my daily work on the Internet? (16 hours a day)
* I think the code will be still be ready for demo/explanation for SNET’s  AI-DSL group  6/9/2022
* How involved should SNET be ?    I told Nils (5/25/2002) I could port to MeTTa…   HOWEVER That would take me 70-160 hours to extend the MeTTa kernel (giving it the superpowers of backtracking and the GC to handle MUARC) ).. Would they want that if MeTTA already does everything they need?  I need to ask for a description of what the MeTTa kernel does practically today.  So far it seems to be a MiniKanren impl … Is the type of inference I do in MUARC even something they would want?  Also what sort of time/performance hit would my code take? 
* Time taking options:
    * Guile-log (80 hours) 
        * Will make my code run ? the speed but compatible with OpenCog Prime.
        * Will be fixing bugs with in Guile-log 
        * Had Ben’s approval for a temporal inference engine i wrote (ARC would need to be confirmed again and wage renegotiated .. would need to sell him on the importance of ARC) 
    * Yap (50 hours) or Sicstus (40 hours) 
        * Yap make my code run 2 to 4 times faster
        * Sicstus would be 4 to 20 times faster
        * Very little little extra work
    * MeTTA (160 hours)
        * I’d need to add the type of basic inference that MUARC does to MeTTa in Rust
        * Rust native version would be ? speed of the current C version?
        * I suppose it is less hours (40-80 hours?) if I make MeTTa call the C code
        * The “pro” for SNET is 3 months from after I stop messing with the port. They can say that the SOTA in ARC runs in MeTTA.  Is there a “pro” for me?
    * No port
        * Costs me no extra 
        * Can continue to develop and make work better

---

6/3/2022

* Explained to someone that (H)MUARC would pass ARC 87% - 100% this is STILL not a proto AGI..   It is the next program called (G)MUARC that comes after (H)MUARC that is more closely related to the proto AGI.   Cute thing is  (G)MUARC takes less time than (H)MUARC (160 hours .. not 480 hours)
    *  (G)MUARC is the editing tool that is used to regenerate better versions of MUARCs.  Even though I am an expert at writing programs that write new programs, I still have to create a (H)MUARC version in order to know what (G)MUARC needs from me.


---

6/4/2022

* Begun web interface to running ARC Tests from LOGICMOO [https://logicmoo.org/swish/arc/arc_testing_interface.html](https://logicmoo.org/swish/arc/arc_testing_interface.html) 
* Non logicmoo runs at [http://gitlab:1777/swish/arc/arc_testing_interface.html](http://gitlab:1777/swish/arc/arc_testing_interface.html)
* 


---


6/10/2022

 The process of (H)MUARC is as follows….

Individuate the input images (using the “complete” algo)

* Individuate-by-color-masses (no diagonals except black)
* Individuate-by-a-priori-shape-lib (human created)
* Individuate-by-folding
* Individuate-by-navigation (ray shooters)
* Individuate-by-single-thickness brush
* (TODO) Individuate-by-N-thickness brush
* With the rest
    * Rest is noise (saved to noise channel regardless)
    * Individuate rest by-color
    * Individuate rest as-pixels 
    * Individuate-by-distance to previous individuals
* With Individuals compute:
    * Possible “fill” areas
    * Distances
        * Negative for individuals inside the fill areas
        * Zero is for touching
        * Positive is distance
    * Classify:  
        * Dot,hv_line(H/V),dg_line(U/D), Rectangular, Polygon
        * Solid(T/F), Outline, UnknownPoly,
* Rerun individuation on the individuals using only themselves
* Rerun individuation on the individuals using individuals in their fill areas 


Individuate the output image  (using the “complete” algo again)

Identify remaining, new and removed individuals:  Images get created from groups

* Only New
* Only Removed
* Only Remaining
* Only New and Removed

With the 4 Groups plus the In and the Out

* Diff each of the 6 against the 5 others
* Regroup individuals by classification labels, sizes, and colors
* Generate algorithms that transform all 30 pairs 

Find correspondence between the 30 algos which answers and does: 

* Are there commonalities between remaining, added and removed individuals ?
* Find what remaining individuals have in common
* Find what removed individuals have in common
* Find what added individuals have in common
* Are there commonalities between removed and added individuals ?
* Are there commonalities between remaining and added individuals ?
* Are there commonalities between remaining and removed individuals ?

(If the commonalities are not detectable, it backtracks (this means that REDO port in Prolog of proof is called this sometimes enables and disables parts of the code)

The Algos are saved for each of the training sample pairs (3-5 training pairs per test) 

The overlapping algos in the training samples are considered its “Theory” they concatenate together and run in the DSL I created for the vm

 .. depending on the result it may backtrack.  Usually it is the inviduators that need disabling.. They can obfuscate the results usually with too much individual of  information 

Backtracking will disable and rerun the various combinations of the above code 

for Example:

* Remove the nose channel
* Individuate-by-color-masses (no diagonals except black)
* Individuate-by-a-priori-shape-lib (just those saved by previous pass)
* Individuate-by-folding (not tried the second pass)
* Individuate-by-navigation (ray shooters)
* Individuate-by-single-thickness brush
* (TODO) Individuate-by-N-thickness brush
* With the rest
    * Rest is noise (saved to noise channel regardless)
    * Individuate rest by-color
    * Individuate rest as-pixels 
    * Individuate-by-distance to previous individuals
* With Individuals compute:
    * RE-Classify:  
        * Dot,hv_line(H/V),dg_line(U/D), Rectangular, Polygon
        * Solid(T/F), Outline, KnownPoly,
* Rerun individuation on the individuals using only themselves
* Rerun individuation on the individuals using individuals in their fill areas


