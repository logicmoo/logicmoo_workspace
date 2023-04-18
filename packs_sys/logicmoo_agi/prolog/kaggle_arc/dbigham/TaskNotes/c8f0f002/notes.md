# c8f0f002

## Examples

![ARC examples for c8f0f002](examples.png?raw=true)

## Rules (DSL)

![DSL rules for c8f0f002](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* As with 25d8a9c8, we notice that a rule set can’t be found if we parse the scenes are single composite objects, so we try interpreting the scene as independent objects, which leads to a working rule set.
* This example was still failing, because the heuristics we were using to detect the background color saw that in the test example input, there are twice as many orange pixels as any other color, so it interpreted the background color as orange, meaning that there was no orange object to turn gray.
   * I updated the background color heuristics so that if we are re-interpreting the scene as independent objects, it will be more conservative about choosing a non-black background color.

#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.


#### Finding Rules from Object Mappings

* We group the object mappings using the Color property, and find rules for transforming inputs to outputs.
* The resulting rules are unwieldy because they split up the different deletion cases by shape.
   * It discovers that vertical lines of length two can be deleted.
   * It discovers that squares can be deleted.
   * It discovers that, while other shapes don’t have enough examples to warrant individual rules, it can form an Except rule to cover the deletion of the other objects.


![image 1](image1.png?raw=true)
