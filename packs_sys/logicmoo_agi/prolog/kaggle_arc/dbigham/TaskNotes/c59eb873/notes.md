# c59eb873

## Examples

![ARC examples for c59eb873](examples.png?raw=true)

## Rules (DSL)

![DSL rules for c59eb873](rules.png?raw=true)

## Notes
Similar to how we detect image rotations, we add scaled versions of each object to its “Shapes” list.  When building the mapping from input objects to output objects, we take the input object’s “Shapes” list, and check if there are any output objects that match one of the images in that list. In this case, we find that there’s an output object that is a scaled version of the input object.

Once we have a mapping from input objects to output objects for all example pairs, we see that the transform is the same in each case, so we can form a general rule.

Additionally, we can now specify a “Width” and “Height” top-level attribute of the rule set association to specify the width and height of the output scene.  In our case, it is a multiple of the width and height of the input scene.
