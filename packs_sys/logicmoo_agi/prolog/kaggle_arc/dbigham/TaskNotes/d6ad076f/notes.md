# d6ad076f

## Examples

![ARC examples for d6ad076f](examples.png?raw=true)

## Rules (DSL)

![DSL rules for d6ad076f](rules.png?raw=true)

## Notes
Here we enhance whole-scene rotational normalization with a more general approach, which we consider if the initial approach of using the orientation of referenceable objects doesn’t see a way to normalization rotation.  For this more general approach, we simply look at the orientation of all of the objects in the scene, as to whether they are left-to-right, top-to-bottom, etc. If so, we try to normalize the rotation of scenes to match the orientation of objects in the first scene. We check both the set of input scenes and the set of output scenes.

For this task, we get the following normalized scenes: 


![image 1](image1.png?raw=true)

Of note is that this approach doesn’t always put the smaller rectangle on the same side of the input, but simply getting the inputs into a vertical orientation is enough for the rule finder to succeed.

To get this task working, I also needed to make a number of scoring tweaks, and be less greedy about choosing referenceable objects when producing the dynamic expressions for property values. Previously, I was arbitrarily choosing the first property of a referenceable object that was usable, whereas I now select all possible properties, and then downstream choose the property that produces the best looking expression.
