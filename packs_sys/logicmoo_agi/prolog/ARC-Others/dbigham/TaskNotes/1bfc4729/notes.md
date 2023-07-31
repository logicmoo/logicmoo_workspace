# 1bfc4729

## Examples

![ARC examples for 1bfc4729](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 1bfc4729](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.

The initial parse forms multi-color composite objects in the outputs, but that doesn’t lead to a workable rule set, so the scenes are parsed again not forming multi-color composite objects, which does lead to a workable rule set.


#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

When we group object mappings by the Y value of the input object, we see that the shape of the corresponding output object becomes predictable.



![image 2](image2.png?raw=true)
