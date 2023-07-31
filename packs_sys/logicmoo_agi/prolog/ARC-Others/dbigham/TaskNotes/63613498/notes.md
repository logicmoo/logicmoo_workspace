# 63613498

## Examples

![ARC examples for 63613498](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 63613498](rules.png?raw=true)

## Notes
* Notice that it’s always just a single object that changes.
* Confirm that the modified objects use a consistent, specifiable transformation. (in this case, they become gray)
* Check whether the modified objects can be referenced via any common property values such that no non-modified input objects use that property value.  In our case, there are no such property values.
* Next, check whether the modified objects can be referenced using any property who’s values are equal to some referenceable object’s property values.
   * To make this work, we need to introduce second order referenceable objects which make use of object relationships such as “within”. i.e. Object[<|”Within” -> Object[<|“Color” -> “Gray”|>]|>]
* If we discover a candidate reference for the modified objects, we check whether that pattern matches any objects that shouldn’t be modified.
   * For example, in our case, the reference “MonochromeImage” -> ObjectValue[<|”Within” -> Object[<|”Colors”->{“Gray”}|>]|>], ”MonochromeImage”] will match not only the object we want to modify, but also the object within the gray object, which we don’t want to modify.
   * If any objects are matched that we wish weren’t matched, we next try to find a way to reference those objects that doesn’t also match the to-be-modified objects. If we can find such a pattern, we form a final pattern such as the following, which makes use of a new “Except” key:

![image 1](image1.png?raw=true)
