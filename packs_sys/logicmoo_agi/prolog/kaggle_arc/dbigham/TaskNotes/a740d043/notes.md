# a740d043

## Examples

![ARC examples for a740d043](examples.png?raw=true)

## Rules (DSL)

![DSL rules for a740d043](rules.png?raw=true)

## Notes
The approach we’re using with this input is to treat the scene as a single object, which helps in the case of the second training example where the two objects are non-contiguous and so would normally be treated as two different objects.

The downside to treating the scene as a single object is that there is a lot of empty space “surrounding” (but within) the object, which we don’t want, so when we’re building the mapping from input objects to output objects, upon not being able to find a corresponding object in an output scene, we try removing empty space, which in this case allows a mapping to be found.

A significant additional challenge is that the scene-as-single-object mode wasn’t even getting the chance to run, because a rule set is found when processing the scenes in the standard way:


![image 1](image1.png?raw=true)

This rule set is nonsensical and doesn’t generalize to the test example.

To deal with this, we need to try finding a rule set using the scene-as-single-object approach even if we find what appears to be a working rule set parsing the scene using a preferred parsing mode. If we find another working rule set, we can then compare the complexity of the rule sets, choosing the scene-as-single-object rule set if it is sufficiently better.
