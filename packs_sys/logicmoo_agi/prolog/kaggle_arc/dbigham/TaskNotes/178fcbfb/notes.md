# 178fcbfb

## Examples

![ARC examples for 178fcbfb](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 178fcbfb](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* The core challenge with this input is the occlusion of the red lines by the green and blue lines.
   * I think the ideal solution would be to train an ML model to detect these occlusions, but I didn’t want to put the time investment into that at this point.
   * For now, I’ve implemented a function that looks for these occlusions. At the moment it is specific to finding occlusions of lines by other lines.
   * I have introduced a new property called ZOrder to specify the Z ordering of objects.

#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

See [178fcbfb-easier](https://github.com/dbigham/ARC/blob/main/TaskNotes/178fcbfb-easier/notes.md) for details.


#### Rendering Output Scenes

After applying rules, when we render output scenes, we pay attention to the values of ZOrder properties to know which order we should draw objects.
