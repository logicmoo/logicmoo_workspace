/* <module>
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(feature_replace_abox,[]).

:- lmb.

:- 
  Before = foo(abox),
   fully_expand(Before,After),
  mpred_must(After = defaultAssertMt(ABox),foo(ABox)).


:- Before = (create_predicate_istAbove(abox,F,A)),
   fully_expand(Before,After),
  mpred_must(After = defaultAssertMt(ABox),create_predicate_istAbove(ABox,F,A)).

:- Before = ((mpred_prop(F, A, pfcCreates)/(is_ftNameArity(F,A))==>{create_predicate_istAbove(abox,F,A)})),
   fully_expand(Before,After),
  mpred_must(After =(mpred_prop(F, A, pfcCreates)/(is_ftNameArity(F,A))==>{defaultAssertMt(ABox),create_predicate_istAbove(ABox,F,A)})).


:- Before = bar(fooFn),
   defunctionalize((,),Before,After),
   After= (mudEquals(A,fooFn),bar(A)).



