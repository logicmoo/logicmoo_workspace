# 8e5a5113

## Examples

![ARC examples for 8e5a5113](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 8e5a5113](rules.png?raw=true)

## Notes
* We notice there are two vertical lines that span the entire height of the input, so we are sure to segment those on their own such that they aren’t included as part of the object on the left.
* We notice that the objects in the input scene always occur unchanged in the output, but there are always two additional objects added.
* When producing a rule for the two added objects, we notice that:
   * The first added object is always a 90 degree rotation of the non-gray object from the input.
   * The second added object is always a 180 degree rotation of the non-gray object from the input.