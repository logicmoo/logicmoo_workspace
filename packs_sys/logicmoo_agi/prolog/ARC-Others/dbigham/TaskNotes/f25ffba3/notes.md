# f25ffba3

## Examples

![ARC examples for f25ffba3](examples.png?raw=true)

## Rules (DSL)

![DSL rules for f25ffba3](rules.png?raw=true)

## Notes
The challenge here is ensuring that the output image gets parsed as two different parts, even though there is a single-color contiguous connection between them.

To ensure that, we can pay attention to the fact that the input object is contained within the output object, using that as a clue to split the output object up into two parts.
