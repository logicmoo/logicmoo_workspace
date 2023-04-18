# d631b094

## Examples

![ARC examples for d631b094](examples.png?raw=true)

## Rules (DSL)

![DSL rules for d631b094](rules.png?raw=true)

## Notes
* When we classify shapes, we should classify pixels also as “lines” of length 1. This is necessary so that the rule finder sees that all output objects are lines.
* To avoid bad rules, if our output scenes aren’t constant in width and height, then we should remove object properties like XInverse and X2Inverse, which are only usable if output dimensions are consistent.
* If output objects are larger than the default width/height of the output scene (which is inherited from the input), then instead of failing, we should gracefully increase the width or height of the output scene as appropriate.