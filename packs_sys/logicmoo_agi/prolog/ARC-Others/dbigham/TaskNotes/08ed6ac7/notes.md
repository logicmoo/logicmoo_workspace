# 08ed6ac7

## Examples

![ARC examples for 08ed6ac7](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 08ed6ac7](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene with a matching position and shape, making it straightforward to produce a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* Before looking for rules, we prune the outputs of the mappings to keep only those properties that are being modified. We notice that the shape is always the same, it is just the “Colors” property that is changing.
* When the objects are grouped by HeightRank, we notice that the “Colors” property can be determined.
   * If the HeightRank of an object is 1, it means that it is the shortest object.
   * If the HeightRank of an object is 2, it means that it is the second shortest object.
   * etc.


![image 2](image2.png?raw=true)

* As of September 2022, the rule finder is now producing a single rule because it notices that the color integer of the output object can be inferred using the HeightRank value. This is probably not desirable, but works.


![image 3](image3.png?raw=true)


#### Some Notes on Input Object Properties

* For each input object, we populate a long list of property values which can be used for potential rule formation. (e.g. Y, X, Height, Width, etc.)
* We then supplement those properties, if they are numeric, with corresponding “Rank” and “InverseRank” properties.
* Here is an example input object with each of its properties:


![image 4](image4.png?raw=true)
