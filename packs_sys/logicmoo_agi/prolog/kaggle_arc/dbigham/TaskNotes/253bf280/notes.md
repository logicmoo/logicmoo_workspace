# 253bf280

## Examples

![ARC examples for 253bf280](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 253bf280](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* Initially, the input objects that will be connected by a line are parsed as separate objects.

#### Mapping Input Objects to Output Objects

* When we are building a mapping from input objects to output objects, we notice cases where there are two input objects that map onto a single output object. When we see this, we combine those two input objects into a single “object” / group within the input, and we represent the mapping to the output object as an AddComponents transform, whereby the green line is being added as a component.
e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* We initially group the object mappings using the Angle property, and find rules for transforming inputs to outputs.
* However, if we try to apply those rules to a new input, they won’t apply to any inputs, because the input objects aren’t yet grouped. We need something in the rules to tell the system to try forming groups of input objects prior to applying the rules.
* To solve this, after producing our initial rule set, we check the input objects from the training set that apply to each rule, and if given a rule those input objects are “groups” of disconnected objects, we form group specifications with all of the common property values of objects in those groups.
For example, here are the group specifications we use for this task:



![image 2](image2.png?raw=true)

* Then, when applying a ruleset to a new input, if the “Group” specifies a list of group patterns, we consider whether any groups of input objects, if they were to form a group, would match that pattern. If so, we form a grouped input object instead of having those input objects be on their own.
* Once this has been done, the rules can be applied as normal.


![image 3](image3.png?raw=true)
