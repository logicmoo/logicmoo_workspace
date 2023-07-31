# 5168d44c

## Examples

![ARC examples for 5168d44c](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 5168d44c](rules.png?raw=true)

## Notes
Building on previous examples that use rotational normalization, what makes this task unique is that it matters that we rotate vertical examples counter-clockwise rather than clockwise when making them horizontal.

We are unable to see this using any pair of referenceable objects, so we instead observe that the motion of the red object can be used to understand the orientation of an input scene. This does require that, for each example, we use both the input scene and output scene to determine the direction of motion. One would think this isn’t a usable approach, since at test time, we aren’t given the output scene. But all we need to do here, at training time, is to see that the motion of objects tells us that counter-clockwise rather than clockwise rotations should be favored. We add a new key to the rules data structure to specify this:


![image 1](image1.png?raw=true)

At test time, if we would see a vertically oriented scene, we would be sure to rotate it counter-clockwise rather than clockwise to make it horizontally oriented. As it turns out, the test input is already horizontally oriented, so we end up not needing to do any rotational normalization at all, but this design ensures that had it been vertical, the test would pass.
