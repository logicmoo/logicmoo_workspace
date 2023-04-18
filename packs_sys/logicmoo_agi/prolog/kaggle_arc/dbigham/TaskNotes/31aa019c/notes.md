# 31aa019c

## Examples

![ARC examples for 31aa019c](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 31aa019c](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* As with 25d8a9c8, we notice that a rule set can’t be found if we parse the scenes are single composite objects, so we try interpreting the scene as independent objects, which leads to a working rule set.
   * Previously we were being conservative and only doing this if the input scene was getting interpreted as a single composite object, but this input shows that we also need to try doing this when this is not the case.

#### Mapping Input Objects to Output Objects

* We build a mapping from input objects to output objects.
* One downside to re-interpreting the scenes without forming composite objects is that, for the output scenes, this means that we don’t get a composite object for the pixel plus its surrounding “box”, which prevents a mapping being formed from the pixel in the input to the pixel+box in the output.
   * What this ultimately means is that no input object ends up getting mapped to the boxes in the outputs.
   * To deal with this, we introduce the notion of adding an entry to the “mapping” list which isn’t a mapping per se but rather an indication of what objects need to get added to the output scene which don’t have a known corresponding object in the input scene:


![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* We add a new property, ColorUseCount, which we set if an object has a single color. It specifies how many times an object’s color was used in the scene.
* By grouping input objects by ColorUseCount, we can see that:
   * Objects with ColorUseCount of 1 are unmodified.
   * Objects without a ColorUseCount of 1 get deleted.
* To deal with the “AddObjects” item in the mapping list, the rule finding code has been enhanced to check whether all output scenes need exactly 1 object added. (it will be a future enhancement to deal with the case that all outputs have 2 added objects, or 3 added objects, etc.)
   * If so, we try to produce a generalized specification of what that output object’s properties should be, similar to how we do that during standard rule-finding.
   * One notable difference when producing a rule for an “AddObjects” item is that instead of considering properties from referenceable input objects, we consider properties from referenceable output objects.
   * In our case, there is a referenceable object in the output, the pixel, which we can use when inferring the location of the box.


![image 2](image2.png?raw=true)
