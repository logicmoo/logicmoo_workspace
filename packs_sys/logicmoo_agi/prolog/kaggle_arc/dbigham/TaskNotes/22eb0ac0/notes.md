# 22eb0ac0

## Examples

![ARC examples for 22eb0ac0](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 22eb0ac0](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* Because we detect cases where multiple input objects (2) map to a single output object, we form “groups” in the input whereby we treat the pair of pixels as a single object with two components.

#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* Objects with a width of 1 stay the same.
* Objects with a width of 10 (groups of two pixels) become a line.
   * We notice that these input objects are “groups” of non-contiguous objects from the input, so we add a “Group” definition to the output to specify what groups of non-contiguous input objects should be combined as a single object.
   * The two objects should both be pixels, their image should be the same as each other’s, as should be their Y values. Together as a group, they should form an object with a width of 10, and span from X=1 to X2=10, etc.
* As of September 2022, my rule finding code is finding a further simplification, not requiring different rules based on the width of objects, because even single pixels can be expressed as having their shape set to line/rectangle, since a pixel is just a line/rectangle with a width of 1.


![image 2](image2.png?raw=true)
