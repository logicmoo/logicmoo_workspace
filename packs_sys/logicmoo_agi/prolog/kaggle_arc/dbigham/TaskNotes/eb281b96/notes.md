# eb281b96

## Examples

![ARC examples for eb281b96](examples.png?raw=true)

## Rules (DSL)

![DSL rules for eb281b96](rules.png?raw=true)

## Notes
* We start by looking for instances of the input pattern in the output, and we do find a repeating pattern, but it doesn’t cover all of the pixels of the output.

* We then look for instances of transformations of the input pattern (flips, rotations) and notice that if we flip the image vertically, we also find a repeating pattern.

* We then consider whether there is a combination of the above repeating patterns that cover all pixels of the outputs, and there is:

![image 1](image1.png?raw=true)

* We do this for both training examples, and in both cases, we find two-part PatternFill. For the second training example, it is:

![image 2](image2.png?raw=true)

* The final step is to combine and generalize the pattern fills. This uses the same generalization code that top-level scenes use when trying to generalize a group of rules.
   * Something new introduced here is finding generalizations that involve linear equations, such as: TrajectoryY = InputScene.Height * 2 - 2


![image 3](image3.png?raw=true)
