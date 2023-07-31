# b7249182

## Examples

![ARC examples for b7249182](examples.png?raw=true)

## Rules (DSL)

![DSL rules for b7249182](rules.png?raw=true)

## Notes
Leverages rotational normalization.  The first input is vertically oriented but the other training examples are horizontally oriented, so they are normalized by rotating them by 90 degrees.

I have defined a new “Y” shape that has an Angle property and a StemHeight property. (the “stem” is the bottom stroke of the “Y”)  While this works (and hints at other useful shapes to define like “C”), this suggests that a more general approach would need the ability to take an object that is composed of lines and produce a sort of shape-like representation in a just-in-time fashion.

To get this task to work, I needed to fix some issues in the rule finder so that a conclusion could specify Y2 and Height to specify the vertical location of an object.
