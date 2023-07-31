# 56dc2b01

## Examples

![ARC examples for 56dc2b01](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 56dc2b01](rules.png?raw=true)

## Notes

### Whole Scene Rotational Normalization

To make finding a rule set easier for inputs like this, we introduce whole-scene rotational normalization.

This involves inspecting the training inputs to see:

* Whether there are candidate referenceable objects that can be used to detect the orientation. (e.g. the green object and the red object being in a certain order within the scene)
* Whether the input scenes can be rotated in such a way that the horizontal or vertical ordering of the above referenceable objects becomes consistent.
In this case, we notice that we can rotate the second and third examples so that, just like the first example, the green object is on the left, and the red object is on the right. Given that observation, if we haven’t been able to find a working rule set, we apply the rotations to normalize the examples, and then try finding rules again.

If we do find rules, we add an annotation to the rule data structure to specify that rotational normalization should be performed, and what configuration we should see if we have normalized a scene correctly:



![image 1](image1.png?raw=true)

i.e. If we have normalized the scene correctly, the green object should be on the left, and the red object should be on the right.


### Added Objects Referencing Other Output Objects

A second challenge is be able specify the horizontal location of the light blue line.

To make this easier, we introduce the ability of referring to *output* objects when specifying where added objects should be created. In our case, we use the location of the now-moved green object to make it easy to specify the location of the blue line.



![image 2](image2.png?raw=true)
