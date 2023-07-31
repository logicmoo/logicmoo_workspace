# 0962bcdd

## Examples

![ARC examples for 0962bcdd](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 0962bcdd](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

* We build a mapping from input objects to output objects.
* At first, we try producing a mapping from multi-color composite objects in the input to multi-color composite objects in the output, but that doesn’t lead to a working rule set.
* We then try producing a mapping between single-color objects, which does lead to a working rule set.
* When I first tried this example, it was still failing to map input objects to output objects, because the heuristic I was using is that if an input object A was the only input object fully within an output object B, we’d create a mapping between them, but otherwise we would not.
   * For this example, that this not the case -- there are multiple input objects fully within the output objects -- so no mapping was getting created.
   * We introduce another heuristic which is that if an input object’s filled-in pixels are a subset of the filled-in pixels of an output object (or vice versa), and are of the same color, we create a mapping between the objects.
e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

When we group object mappings by the shape of the input object, we see that the shape of the corresponding output object becomes predictable.



![image 2](image2.png?raw=true)
