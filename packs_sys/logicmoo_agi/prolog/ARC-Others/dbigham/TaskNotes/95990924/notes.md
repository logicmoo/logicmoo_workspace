# 95990924

## Examples

![ARC examples for 95990924](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 95990924](rules.png?raw=true)

## Notes
This example wasn’t working because some of the output objects were globulating into larger objects.

The existing strategy for trying to avoid this was to find repeated sub-images and use that as a clue for when to break up objects into multiple independent objects. Previously we were only looking in the current image for discrete objects, and using those as our sub-images, but that didn’t work for the second example’s output scene.

I enhanced this mechanism to collect sub-images from all input and output images. That solved the problem, but it also broke a number of other examples where I was now getting false positives. i.e. There were now cases where it was breaking up objects that shouldn’t have been broken up, so I needed to add a couple more heuristics to avoid those cases.
