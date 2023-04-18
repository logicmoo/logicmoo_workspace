# a8c38be5

## Examples

![ARC examples for a8c38be5](examples.png?raw=true)

## Rules (DSL)

![DSL rules for a8c38be5](rules.png?raw=true)

## Notes
As of September 7 2022, the parsing scheme that ends up producing a working rule set is:

* Don’t form multi-color composite objects. The consequence of this is that the square objects in the input scenes are actually each parsed as two objects -- their colored part, and their gray part.
* Treat the output scenes as having a gray background color, such that the only objects in the scene are the colored pieces.
* Produce a mapping from the colored objects in the input to the colored objects in the output.
* Produce a rule set that specifies where to move the input objects when placing them in the output scene.
* One additional rule gets created to clean up the gray shapes in the input. (they get deleted)
* Remove empty space from the output.