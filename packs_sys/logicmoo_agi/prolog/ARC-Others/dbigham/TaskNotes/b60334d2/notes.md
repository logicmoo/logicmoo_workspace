# b60334d2

## Examples

![ARC examples for b60334d2](examples.png?raw=true)

## Rules (DSL)

![DSL rules for b60334d2](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* Avoiding unwanted object globulation:
   * When parsing the output scene of example 1, two of the objects are touching diagonally, which currently causes our system to form a single object rather than two. This is problematic.
   * Likewise, when parsing the test output scene, three of the objects are touching, causing them to form a single object rather than three. This also is problematic.
   * Strategy
       * Parse the input and output scenes and produce a library of notable objects that we’ve seen.
       * For each object in each scene, check whether the object could be reinterpreted as multiple objects from our library of notable objects.
       * If so, break the object up.

#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene that it is contained within, making it straightforward to produce a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

All object mappings involve replacing the input object with a common output image, making it straightforward to produce a single rule:



![image 2](image2.png?raw=true)
