# d13f3404

## Examples

![ARC examples for d13f3404](examples.png?raw=true)

## Rules (DSL)

![DSL rules for d13f3404](rules.png?raw=true)

## Notes
* If:
   * The input image is not large. (width <= 8 and height <= 8)
   * And/or:
       * The output width is at least 1.5x as wide as the input.
       * The output height is at least 1.5x as high as the input.
   * The colors used in the input and output are the same.
   * The proportions of colors used in the input and output scenes have each color proportion within 30%.
* Find all instances of the input image (less background color) in the output image.
* Check if the locations form a linear trajectory (possibly with “wrapping” such as in 91413438).
* If so, form a candidate rule, such as:

![image 1](image1.png?raw=true)

* Check whether the candidate rule works on all examples if we use the known output size.
* Try to additionally produce an expression for the output width and height.