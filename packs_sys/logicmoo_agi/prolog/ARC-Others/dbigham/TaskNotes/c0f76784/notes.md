# c0f76784

## Examples

![ARC examples for c0f76784](examples.png?raw=true)

## Rules (DSL)

![DSL rules for c0f76784](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.

The objects in the output scenes each have two component images.

e.g.



![image 1](image1.png?raw=true)


#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene with a matching color and shape, making it straightforward to produce a mapping from input objects to output objects. Each mapping involves an AddComponents transform.

e.g.



![image 2](image2.png?raw=true)


#### Finding Rules from Object Mappings

We notice that the mapping from an input object to its corresponding output object can be generalized in terms of the properties of the component to add:



![image 3](image3.png?raw=true)

Note that the ruler finder has noticed something curious about the color of an output object: We can take the width of the input object, add three to it, and that gives us the integer value of the output color. This is probably not desirable, although it could potentially provide stronger generalization if that’s what the task author was going for and they had made use of that in their test.
