# 97999447

## Examples

![ARC examples for 97999447](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 97999447](rules.png?raw=true)

## Notes

### Unwanted Rule

As of October 2022, the second example output parses the contiguous lines as a single object. This results in an unwanted “rule” being found for producing that composite object. (it’s a rule with only 1 example to support it, so it’s very tenuous, but it’s the best the rule finder can do)

Ideally in the future the scene parser would be smart enough to not form a composite object for these two lines, but until then, we get away with it, because the test example doesn’t have contiguous lines, and the unwanted rule doesn’t cause any breakage for the test example.


### Lines with Pattern Fills

Building on [a5f85a15](https://github.com/dbigham/ARC/blob/main/TaskNotes/a5f85a15/notes.md), we detect when a line has a pattern fill.

We can then produce a rule that specifies that pixels in the input should become lines in the input with a pattern fill based on the input object color.


### Don’t Form Multi-color Objects or Follow Diagonals

By default, we form multi-color objects, but that causes a problem in the test example because the green and yellow pixels in the test input should not together form an object.  To avoid that, we use a heuristic that if all of the example inputs consist only of pixels, and a good rule set was found, then we specify in the output rule that:


![image 1](image1.png?raw=true)


![image 2](image2.png?raw=true)

This ensures that when the test example is solved, we avoid forming multi-color objects and we avoid following diagonals.


### Don’t Allow Pixels to Occlude Lines

Previously we were allowing single pixels to be treated as objects that are occluding lines, but that can cause pattern-filled lines to get treated as lines which are occluded by pixels, which we don’t want, so we will for now disallow single pixels to be treated as objects which are occluding other objects.
