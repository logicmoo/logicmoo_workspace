# 25d487eb

## Examples

![ARC examples for 25d487eb](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 25d487eb](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

In each case, a component (a line) is added to the object from the input scene.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* As mentioned, each input object gets a line component added to it.
* One challenge with producing a generalized rule is that the ending position of the line isn’t really relative to any of the objects in the scene, but rather is relative to the scene boundary, and which scene boundary depends on the orientation of the object.
   * We introduce a new way to specify a line in cases like this:
       * “Outward” -> True specifies that the line extends outwards from its parent object.
       * “Direction” specifies the direction of the line.
   * With this, we only need to specify the lateral part of the line’s position.
* Another challenge with producing a generalized rule is how to specify the “lateral” position of the line, since that depends on the orientation of the object.
   * If the triangle is facing upwards or downwards, we need to specify the X component of the line’s position.
   * If the triangle is facing left or right, we need to specify the Y component of the line’s position.
   * But if we need to specify different properties in different cases, then we would need two rules rather than one. To do that, we could first group inputs by their PrimarySizeDimension (Y or X), and then find rules. Here we hit a snag, since our current approach is to require at least 2 examples to form a rule -- otherwise, if we only required 1 example to form a rule, we’d end up with too many crazy/incorrect rules. And if we group by PrimarySizeDimension, there is only one example of a horizontally oriented triangle, which isn’t sufficient to form a rule.
   * To deal with this, we introduce the concept of “rotational normalization”, which allows us to deal with all triangles uniformly regardless of their orientation.
       * If we are unable to find a generalized rule for a set of object mappings, we check whether they are objects which are the same shape but of differing rotation.
       * If so, we try normalizing their rotation to face upwards, and then see if we can find a generalized rule.
       * If so, we mark the resulting rule with “RotationNormalization” -> True so that any time that rule is applied, we first normalize the rotation of the object to point upwards,  apply the rule, then rotate the object back to its original orientation.


![image 2](image2.png?raw=true)
