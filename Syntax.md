# Syntax of LPS

Let us have fun documenting the syntax of LPS. Actually, we are working with two syntaxes for the time being. A higher level one, which follows and the lower level or Wei's syntax. Please, beware: 

* * *

## LPS Higher level syntax ##

Files have the .lps extension and (more or less) the following structure: 

```
#!Prolog

  spec ::= statement | spec
  statement ::= setting | rules 
  setting ::= max_time | actions | fluents | initial_state | observations | events 
  rules ::= if_rules | if_then_rules | initiate_rules | terminate_rules | constraints 
  if_rules ::= literal "." | literal "if" conjunction "." | if_rules 
  if_then_rules ::= "if" conjunction "then" literal "." | if_then_rules
  constraints ::= "false" conjunction | constraints 
  conjunction ::= literal | literal "," conjunction
  
```
where

### Events

events can be written in any of the forms:

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

  spec ::= statement | spec
```