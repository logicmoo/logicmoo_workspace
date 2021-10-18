
ProgramK:
 here in prolog/programk

This comes with two Chatbot impls:

# AIML2.0 
The first ever Prolog Impl of the complete AIML Spec  - 
@TODO Need soem offical 2.0 compliance tests!

Using ChomskyAIML Set

`swipl aiml_2_0 @load chomskyAIML`

Running ProgramD tests

`swipl aiml_2_0 @load test_suite/ProgramD`

Running ProgramQ tests

`swipl aiml_2_0 @load test_suite/ProgramQ`

# AIML-LF
A Deep Structure Logical Form (Caveman Language!)

This used a non-English/Logic based Chatbot.
It reads AIML file but reengineers them to no longer use natural language (it to use  Deep Structure Logical Form instead!)

Using ChomskyAIML Set
````swipl aiml_lf load chomskyAIML```

Running ProgramD tests
````swipl aiml_lf load test_suite/ProgramD```

# Running AIML-LF tests
````swipl aiml_lf load test_suite/srlAIML```


... bunch of gibberish...

when you get the
````
|:  
prompt 
|:  hello bot, how are you today?
````

 typing `annie` will make the aiml run the sanity chacks

if what you type is interpretable as prolog, it will treat it as prolog.
````
<dmiles_afk> |: member(X,[1,2,3]).
<dmiles_afk> member(1,[1,2,3]):['X'=1]
<dmiles_afk> member(2,[1,2,3]):['X'=2]
<dmiles_afk> member(3,[1,2,3]):['X'=3]
````
typing prolog
gets you back to prolog

make
remakes it
