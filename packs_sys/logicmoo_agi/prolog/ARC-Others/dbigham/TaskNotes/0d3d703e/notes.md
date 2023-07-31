# 0d3d703e

## Examples

![ARC examples for 0d3d703e](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 0d3d703e](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* When we group object mappings by color, we see that the color of the output object is predictable.
* Previously we were requiring each rule to have at least 2 examples so that we’d avoid accepting bad rules, but that doesn’t work here, because the majority of from/to pairs only have 1 example.
* We now start by trying to find a rule set where each rule has 2 or more supportive examples, but if that fails, we are willing to accept rules with only 1 supporting example.


![image 2](image2.png?raw=true)
