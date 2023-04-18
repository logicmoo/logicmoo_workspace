# e76a88a6

## Examples

![ARC examples for e76a88a6](examples.png?raw=true)

## Rules (DSL)

![DSL rules for e76a88a6](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene with a matching position and  shape, making it straightforward to produce a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* We can find rules by grouping objects by color.
* When we do this, we notice that:
   * All gray objects are unchanged.
   * All objects that aren’t gray are replaced by another image.
       * However, the replacement image varies by example.
       * We notice that the replacement image is always the image of the non-gray object.


![image 2](image2.png?raw=true)

* As of September 2022, we have improved the rule finding code to see that in all cases the image of an output object can be determined via the same conclusion, so we don’t need a second rule that says “Same” -> True.


![image 3](image3.png?raw=true)
