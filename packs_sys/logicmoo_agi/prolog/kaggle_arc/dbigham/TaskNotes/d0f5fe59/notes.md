# d0f5fe59

## Examples

![ARC examples for d0f5fe59](examples.png?raw=true)

## Rules (DSL)

![DSL rules for d0f5fe59](rules.png?raw=true)

## Notes
For this example, we introduce the notion of the scene itself being a “referenceable object”.

Previously we would allowed objects in the scene to be referenceable, such as Object[<|”Color” -> “Blue”|>] referring to “the blue object”, and so ObjectValue[<|”Color” -> “Blue”|>], “Y”] was a way to refer to “the Y position of the blue object”.  But now Object[“InputScene”] is a referenceable object, and a scene property ObjectCount has been added, so ObjectValue[“InputScene”, “ObjectCount”] can be used, for example, when producing generalized rules.
