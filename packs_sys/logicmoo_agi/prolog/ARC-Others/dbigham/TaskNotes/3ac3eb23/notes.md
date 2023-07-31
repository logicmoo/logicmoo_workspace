# 3ac3eb23

## Examples

![ARC examples for 3ac3eb23](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 3ac3eb23](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene that it is contained within, making it straightforward to produce a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

All object mappings involve replacing the input object’s shape with a common output shape, making it straightforward to produce the single required rule:



![image 2](image2.png?raw=true)
