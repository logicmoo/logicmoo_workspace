# 868de0fa

## Examples

![ARC examples for 868de0fa](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 868de0fa](rules.png?raw=true)

## Notes

### Object Segmentation

When segmenting an image into objects, sometimes ambiguities arise wrt whether something should be split up or not. A technique we use to try to resolve some of these ambiguities is to keep track of whether a particular sub-image has been seen as an object, either within the same scene, or one of the other input/output scenes. If so, then if we see that same image within something that we’re considering whether to break up into pieces or not, that can be the deciding factor to conclude that we should break up a larger image.

During rule finding, we were using the above strategy, but when later applying rules to a scene, we *weren’t* using that strategy. This could result in the rule finder segmenting a scene correctly, but then later when checking to see if our found rules were working correctly, we’d parse an input scene incorrectly. For this task, this was causing problems with training example 4, which has two diagonally connected squares. During rule finding, because we saw a 5x5 blue square in other scenes, we were avoiding fusing the two squares. But after rule finding, when verifying our found rules, we don’t use these sub-images, which was resulting in the squares getting fused.

My first attempt at fixing this was simply to store the seen sub-images in the rules data structure, so that when evaluating the found rules, we’d split up objects in a manner consistent with how we do it during rule finding. This unfortunately broke tasks like 1caeab9d and aedd82e4. So, instead of doing that, if we notice that a task always has squares/rectangles in input scenes, then we add “FolowDiagonals” -> False to the rules. This is similar to what we do if training examples consist only of pixel objects.


### Even Size vs Odd Size

Clearly the intent of this task is to notice that evenly sized objects get filled in with red, while odd sized objects get filled in with orange. Our rule finder doesn’t know about the concept of even and odd, but as of October 2022, it gets by noticing that only odd sized objects have their XMiddle property defined. (perhaps in the future we’ll also define that property value for evenly sized objects, but that would result in values like 2.5, and as of now we avoid that)
