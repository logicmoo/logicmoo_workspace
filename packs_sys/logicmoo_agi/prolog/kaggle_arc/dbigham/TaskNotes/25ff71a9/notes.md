# 25ff71a9

## Examples

![ARC examples for 25ff71a9](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 25ff71a9](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene with a matching color and shape, making it straightforward to produce a mapping from input objects to output objects. Each mapping involves a Move transform.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

We notice that all transforms are of type Move, and notice that the Offset values are always <|”Y” -> 1, “X” -> 0|>.



![image 2](image2.png?raw=true)

We have metadata for each transform type to specify what their “minimum property sets” are. For example, given a move transform, if we can produce a rule to infer its “Position” -> <|”Y” -> ..., “X” -> ...|> values, which is sufficient to specify the move. Or, if we can produce a rule to infer its “Offset” -> <|”Y” -> ..., “X” -> ...|> values, that is als sufficient to specify the move.


![image 3](image3.png?raw=true)

For suitable “sub properties”, we can also specify an ObjectGet function which can be used to compare the transform’s sub-property value to a property from the object being moved (or some other referenceable object in the scene). For example, if we were trying to produce a rule that inferred the “Position” <|”Y” -> ..., “X” -> ...|> of a move, it would be useful to notice that perhaps the “Y” (or “X”) value is always the same as the corresponding input object, in which case we could drop that value from our rule. Or, we might find that that value could be inferred from some other property of the input object or some other referenceable object.
