# A79310A0

## Examples

![ARC examples for A79310A0](examples.png?raw=true)

## Rules (DSL)

![DSL rules for A79310A0](rules.png?raw=true)

## Notes
Enhanced the object mapping code.

Previously, it would map objects that moved, or objects that changed in-place, but it wasn’t mapping objects that both moved and changed.

For the moment, we only produce the mapping in such cases if there’s a single object in the input/output, and they share the same shape.
