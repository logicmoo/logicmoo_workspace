# 1f876c06

## Examples

![ARC examples for 1f876c06](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 1f876c06](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* For this example, the ability to parse 45 degree and 135 degree lines was added.
* Because we detect that multiple input objects (2) map to a single output object, we form “groups” in the input whereby we treat the pair of pixels as a single object with two components.

#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* The shape of output objects is always a line, but the angle of the line needs to be determined.
* I added a new property “Angle” to input objects, which can be used to infer the angle of the output lines. Currently this is only populated in the very specific case that we have a group of two pixels in the input, so this is pretty hacky at the moment.


![image 2](image2.png?raw=true)
