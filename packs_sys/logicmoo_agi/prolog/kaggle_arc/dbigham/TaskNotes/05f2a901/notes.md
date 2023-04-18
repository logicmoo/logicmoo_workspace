# 05f2a901

## Examples

![ARC examples for 05f2a901](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 05f2a901](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* Avoiding unwanted object globulation
   * We use the same strategy as in b60334d2.

#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene that has the same color and shape, making it straightforward to produce a mapping from input objects to output objects. One of the mappings involves a Move transform.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* This task involves quite a non-trivial move operation: The blue object should move towards the red object until it is “blocked” by that object.
* I struggled to balance two goals:
   * Making the DSL general enough that it would hopefully end up applying to more than just this task.
   * Making the DSL simple enough  that its parameters could be inferred.
* For the time being, I’ve felt gone with a pretty simple/specific DSL, but there seems to be a high risk that it’s too specific and thus won’t generalize.


![image 2](image2.png?raw=true)

* The way this works is that after finding the object mappings with basic move transform metadata, we do further analysis on the move transform to see if it fits the pattern of “move until blocked by object”, and if so, we indicate which concrete object would be considered the blocking object.
   * The reason we name the attribute “BlockedBy” and not “UntilBlockedBy” is that at this point we don’t actually know whether the movement was determined by blocking semantics, so we’re just making the observation that based on the direction and ending position of the move, it matches the pattern of “BlockedBy”.
* When finding rules, we notice that it is always the non-blue object that is the blocking object.