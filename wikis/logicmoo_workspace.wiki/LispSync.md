Adding a new class to [[LarKc]] called [[LispSync]].

[[LispSync]] is like [[PrologSync]] except it is supposed to expose the CYC-API into the browsable CYC KB, so that much of the stuff we don't see how to use is asserted to the KB.  Calling CYC-API from CycL.

EvaluateSubLFn is not sufficient, since what calls instances would be used to invoke the planner.  [[LispSync]] adds those calls.

It also allows us to see the planner intermediate states in the browser.

This is similar to the API auto synthesis, but [[LispSync]] exposes a hand picked one right now... however, setting up a regexp system that notices them.

Its written in Java but the regexp notices when Lisp creates classes matching the regexp like SHOP-*

Though one thing to figure out still, is if there is way to capture the field set/gets of arbitrary classes.

[[DouglasRMiles]] created a system to capture that in 2006, but with all the removed classes in J9 it won't carry over.


LispSync reverse engineers Common Lisp programs like [[ShopThree]], [[ShrdluSystem]], [[DAYDREAMER]], etc while they are being loaded.
In doing so replaces some of their vital internal data-structures: DOMAINS, PLANS, LEXICAL-INFO, DAYDREAMS, GOALS, PLANS with proxy objects that are stored in the KB using assert/retract/query into the KB...
The objects can continue to run as they did before, but now any of the parts can be hacked /controlled/ vetted/populated from CYC rules.  
LispSync is essentially the Java version [[BABYLON]] the Lycurgus talks about.  
Why its called "Sync" is there are times when things are not always going to be straight forward.
It is important to [[EToCProject]] because it allows Lisp coded [[ShrlduSystem]]'s LEXICAL information to mix with [[DAYDREAMER]]'s.
And these programs end up with not only their personal data structures but each others.
The "Sync" is between [[ShrdluSystem]], [[ShopThree]], happens from the CYC KB using forward-chaining,
Initially this was one of the reasons i writing WAM-CL .. (the Common Lisp interpreter written in Prolog) is because the SHOPS.pl implementation allowed data-structure sharing without the CYC KB.
Initially this was one of the reasons i started writing WAM-CL .. (the Common Lisp interpreter written in Prolog) is because the SHOPS.pl implementation allowed datastruture sharing within the _Prolog_ database.
Sometimes it converts the right hand side ois a CYC implication into virtual object s.. for instance when we assert we have a problem it can construct a SHOP-PROBLEM object.
Which right there contains all API to run the domain.
Which right there contains all API to run the problem for a solution by assertion as to what domain it starts the visible hierarchy from,
similar to how Drools works with Java,
([[LispSync]] does with Common Lisp).
This system also allows CYC to have programmatic control over itself
[[LispSync]] however is very [[LarKC]]_CL specific.. it won't translate over the SBCL :(
for [[SteelBankCommonLisp]] .. [[BABYLON]] will be an option to replace [[LispSync]].
