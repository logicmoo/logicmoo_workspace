# 46442a0e

## Examples

![ARC examples for 46442a0e](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 46442a0e](rules.png?raw=true)

## Notes
Upon noticing that the output size is always twice as wide and twice as high as the input, we try subdividing the output into a 2x2 grid and finding a rule set for each of the four parts.

As before, when we produce a mapping from input objects to output objects, we assign a transformation to the mapping, such as to indicate whether the image is rotated. However, upon there being ambiguity, we were previously just choosing the first possible transform, which was leading to it appearing that some of the subdivisions didn’t have a consistent transform. We now keep the full list of possible transforms, and then when we are trying to generalize that to a rule, we are able to see which transform is common.

A further challenge is that for one of the subdivisions, one of the images doesn’t change, because when it is rotated 180 degrees, it results in the same image. When we build our mapping from input objects to output objects, if the image doesn’t change, we don’t assign a transformation. This was causing the rule finder to not be able to see that all images in that subdivision were being rotated by 180 degrees. The code that tries to find a common transformation has been enhanced so that, if it’s unable to find a common transformation, it will add in “no-op” transforms such as the 180 degree rotation that doesn’t change the image, and then it will check again for a common transform.


![image 1](image1.png?raw=true)


![image 2](image2.png?raw=true)
