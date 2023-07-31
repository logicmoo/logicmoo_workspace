# 746b3537

## Examples

![ARC examples for 746b3537](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 746b3537](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.

Our first attempt is to form multi-color composite objects, but that doesn’t lead to a rule set, so we try again not forming multi-color objects.


#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

I needed to add a fallthrough strategy for this input whereby if we aren’t successful at forming a 1-to-1 mapping, but we notice that objects can be uniquely identified by color in both the input and output, we use color to create the 1-to-1 mapping.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

We see that all objects become pixels in the output, and their position can be inferred using X.InferseRank and Y.InverseRank.

This was an interesting example, because when I looked at it myself, I didn’t anticipate this inference.



![image 2](image2.png?raw=true)

Up until now, I haven’t attempted any examples where the size of the input image is different than the size of the output image. The above rules results in an output image that is the same size as the input image, with empty black space to the right and/or below the desired output pixels.

To accommodate for this, I’ve added a detector that notices if a rule set can be made to work by simply removing empty space after applying the rules. In such cases, “RemoveEmptySpace” -> True is added to the rule set.
