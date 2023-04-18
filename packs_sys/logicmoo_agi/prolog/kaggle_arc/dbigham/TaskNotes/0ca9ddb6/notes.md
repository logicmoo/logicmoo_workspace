# 0ca9ddb6

## Examples

![ARC examples for 0ca9ddb6](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 0ca9ddb6](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* In the output scenes, the multi-color objects are parsed into a form that specifies their colored components:


![image 1](image1.png?raw=true)


#### Mapping Input Objects to Output Objects

In cases where an output object contains a component which was an object in the input, we produce a mapping between the objects, and specify an AddComponents transform.

e.g.



![image 2](image2.png?raw=true)


#### Finding Rules from Object Mappings

* We notice that if we group the object mappings by the color of their input object, the transformation applied is consistent.
   * For red, a particular AddComponents transform is needed.
   * For blue, a particular AddComponents transform is needed.
   * For any other color, the output object is the same as the input object.