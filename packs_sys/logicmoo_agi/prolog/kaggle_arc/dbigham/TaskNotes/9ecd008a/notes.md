# 9ecd008a

## Examples

![ARC examples for 9ecd008a](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 9ecd008a](rules.png?raw=true)

## Notes
Imputation of a rectangle in the input using symmetry.


### Algorithm

* Check for ImputedRectangleForSymmetry. For all examples:
   * Check whether either the output width is less than the input width or the output height is less than the input height.
   * Check whether the input width and height are the same.
   * Check whether the width and height are divisible by two.
   * Take the top-left quarter of the image, and process each pixel one-by-one. For each pixel:
       * Find the four colors for that location by using the rules of symmetry.
       * If all colors are the same, then move on to the next pixel.
       * If 3 of the colors are the same, and one is different, we’ll consider the differing location to be a corruption and add it to our list: corruptedColor -> expectedColor
       * If 2 colors are the same, then defer our decision until having processed all pixels whereby we will have determined what the color of the rectangle is, and we can use that information to choose the correct color at this location.
       * If no colors are the same, then abort and consider the image to not be of type ImputedRectangleForSymmetry.
   * If there were one or more corrupted pixels found, then check:
       * Whether their number is equal to the number of pixels in the output. If not, abort.
       * Whether they form a rectangle. If not, abort.
   * Conclude that this is a ImputedRectangleForSymmetry transform.