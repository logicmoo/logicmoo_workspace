<pre>

2019-09-05 18:29:38 <aindilis> dmiles: can you refer to other argument values
      from within a CycL formula?
2019-09-05 18:29:47 <aindilis> e.g. something like:
      #$likes(#$AndrewDougherty,#friendFn(#$argFn(#$thisContainingPredicate,1)))
2019-09-05 18:30:13 <aindilis> also, I'm very curious how you would go about
      rewriting https://github.com/aindilis/chap2/blob/master/sample.pl
2019-09-05 18:30:45 <aindilis> I wrote it instantly without any thought, but I
      realized getting all the intersentential and interparagraph relations
      correct is really daunting
2019-09-05 18:31:13 <aindilis> and I was wondering, I know you had said E2C
      can do intersentential relationships iirc, but this goes beyond
      coreference I think
2019-09-05 18:31:35 <aindilis> I'm not sure DRSes would handle it, though it
      might, but I am loath to work in only FOL
2019-09-05 18:32:25 <dmiles> [Mon](isa TheActiveSubject
      SentenceSubjectIndexical)
2019-09-05 18:32:26 <dmiles> [Mon](isa TheSentenceSubject
      SentenceSubjectIndexical)
2019-09-05 18:32:26 <dmiles> [Mon](resultIsa HowManyQueryingFn
      SentenceSubjectIndexical)
2019-09-05 18:32:26 <dmiles> [Mon](resultIsa QuantityFn
      SentenceSubjectIndexical)
2019-09-05 18:32:41 <dmiles> for English.. but we also have them for CycL
2019-09-05 18:34:10 <dmiles> Each instance of #$IndeterminateTerm is a CycL
      symbol for which, in order to establish its denotation, it is not
      sufficient to establish every aspect of the context of its use.
2019-09-05 18:35:48 <aindilis> "He may either move the King to f1 or h1 or h2,
      or he may interpose his Rook on e3."
2019-09-05 18:35:57 <dmiles> for
      #$likes(#$AndrewDougherty,#friendFn(#$argFn(#$thisContainingPredicate,1)))
      i use
      https://github.com/logicmoo/sigma_ace/blob/master/engine/sigma_functions.pl
2019-09-05 18:36:01 <aindilis> 'The latter would be very unwise as Black would
      simply take the Rook with his Bishop, again checking White''s King.'
2019-09-05 18:36:08 <dmiles> as my rewrit etechnique
2019-09-05 18:36:14 <dmiles> but in Cyc that isnt required
2019-09-05 18:36:21 <aindilis> how does one get the denotation of "The latter"
      in sentence 2
2019-09-05 18:36:55 <dmiles> but the probelm you are asking about has to do
      with isntrpsention on the particualr sentnce  TheAssertion
2019-09-05 18:37:29 <dmiles> "#$TheAssertionSentence denotes the EL sentence
      of the `current assertion' being referred to in the context of a
      particular tool that is manipulating CycLAssertions." 
2019-09-05 18:37:29 <aindilis> wow cool
2019-09-05 18:38:24 <dmiles> mannay are under #$TheTerm
2019-09-05 18:38:45 <dmiles>
      http://m.logicmoo.org:3602/cgi-bin/cg?cb-lq&c15974&2&c93430
2019-09-05 18:39:39 <aindilis> not sure what that is
2019-09-05 18:39:46 <aindilis> ah nm
2019-09-05 18:40:11 <dmiles> They are global variables
2019-09-05 18:40:29 <dmiles> that change as inference runs
2019-09-05 18:40:47 <aindilis> ah
2019-09-05 18:40:55 <aindilis> nice
2019-09-05 18:41:21 <dmiles> like (equals (PredicateFn TheAssertionSentence)
      equals)
2019-09-05 18:41:32 <aindilis> ah
2019-09-05 18:41:57 <aindilis> well awesome, so it can be done, another
      opportunity for Cyc to show its great worth
2019-09-05 18:42:01 <dmiles> yeah can create them to as it runs
2019-09-05 18:42:20 <dmiles> yeah/you
2019-09-05 18:43:52 <dmiles> by (and (isa ThePreviousPhrase TheTerm)
      (minimizextent  (equals ThePreviousPhrase ..)  ... )
2019-09-05 18:44:13 <aindilis> hey I have to run for 10 mins
2019-09-05 18:44:38 <aindilis> thank you for your answer, if you can, I'd like
      to discuss more the NLU aspects of CHAP2 when we get back, since I
      wouldn't really know where else to find the info
2019-09-05 18:45:37 <dmiles> oh yeah for
      #$likes(#$AndrewDougherty,#friendFn(#$argFn(#$thisContainingPredicate,
      i did even create  several  Glovals such as  TheSpeaker
2019-09-05 18:45:50 <dmiles> TheArg1Fn
2019-09-05 18:50:48 <aindilis> wow sweet
2019-09-05 18:51:39 <aindilis> so you can do like (TheArg2Fn (TheArg1Fn
      TheAssertionSentence)) ?
2019-09-05 18:52:15 <aindilis> and does it unify?
#logicmoo> 

</pre>
