# Syntax of LPS

Let us have fun documenting the syntax of LPS. Actually, we are working with two syntaxes for the time being. A higher level one, which follows and the lower level or Wei's syntax. Please, beware: 

* * *

## LPS Higher level syntax ##

Files have the .lps extension and (more or less) the following structure: 

```
#!Prolog

  spec ::= statement | statement spec
  statement ::= settings | rules 
  settings ::= max_time | actions | fluents | initial_state | observations 
  rules ::= if_rules | if_then_rules | initiate_rules | terminate_rules | constraints 
  if_rules ::= if_rule | if_rule if_rules
  if_rule ::= literal "." | literal "if" conjunction "." 
  if_then_rules ::= if_then_rule | if_then_rule if_then_rules
  if_then_rule ::= "if" conjunction "then" conjunction "." 
  constraints ::= constraint | constraint constraints
  constraint ::= "false" conjunction "." 
  conjunction ::= "true" | literal | literal "," conjunction
  
```
where

### Observations

observed events can be written in any of the forms:

```
#!Prolog

   event from T1 to T2
   event from T1
   event to T2

```

* * *

## Wei's syntax ##
  
Files have the .lpsw extension and (more or less) the following structure: 

```
#!Prolog

  spec ::= statement | statement spec
  statement ::= settings | rules 
  settings ::= max_time | actions | fluents | initial_state | observations | events
  rules ::= if_rules | reactive_rules | initiate_rules | terminate_rules | constraints 
  if_rules ::= if_rule | if_rule if_rules
  if_rule ::= timeless_rule | event_rule 
  timeless_rule ::= "l_timeless(" literal "," conjunction ")."
  event_rule ::= "l_events(" happens_literal "," hold_conjunction_list ")."
  reactive_rules ::= if_then_rule | if_then_rule if_then_rules
  if_then_rule ::=  "reactive_rule(" conjunction "," conjunction ")."
  constraints ::= constraint | constraint constraints
  constraint ::= "d_pre(" conjunction ")."
  conjunction ::= "true" | literal | literal "," conjunction
```