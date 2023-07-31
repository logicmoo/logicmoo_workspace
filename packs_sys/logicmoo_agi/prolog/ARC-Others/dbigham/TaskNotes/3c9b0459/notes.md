# 3c9b0459

## Examples

![ARC examples for 3c9b0459](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 3c9b0459](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* In this task, there is only one contiguous object per scene.
e.g.



![image 1](image1.png?raw=true)


#### Mapping Input Objects to Output Objects

When trying to map input objects to output objects, if we find an object at the same location, we consider whether applying a rotation produces a match.

e.g.



![image 2](image2.png?raw=true)


#### Finding Rules from Object Mappings

* We notice that in all cases, input objects can be mapped to output objects using a rotation of 180 degrees.
* Because this rule applies universally, its pattern (LHS) doesn’t contain any conditions. (<||>)


![image 3](image3.png?raw=true)
