# 2wfys5w64-relative-right-side

## Examples

![ARC examples for 2wfys5w64-relative-right-side](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 2wfys5w64-relative-right-side](rules.png?raw=true)

## Notes

### Finding Rules


#### Finding Rules from Object Mappings

* As noted elsewhere, we make use of metadata in $transformTypes to specify, for a given transform type, what the minimum property sets are. i.e. What parameters of a transform do we need to be able to dependably infer in order to be able to apply a rule to all instances that match its pattern?
* The AddComponents $transformType metadata looks like this:

![image 1](image1.png?raw=true)

* Things to notice:
   * We need both the Image and Position of a component to know where to add it to the output scene.
   * If the position is relative (”Position” -> <|”RelativePosition” -> ...|>), as would typically be the case with an AddComponents transform, then we need to be able to specify the parameters of the RelativePosition.
   * When we say “Y” | “YInverse”, we mean that we need to specify either “Y” or “YInverse”. (and likewise for “X” and “XInverse”)
       * “Y” is the Y component of the top of the object, typically relative to the top of the scene (or in this case, relative to the top of the image we’re adding components to).
       * “YInverse” is the Y component of the top of the object, typically relative to the top of the scene (or in this case, relative to the top of the image we’re adding components to).
   * Given a set of output objects that we’d like to cover with a rule, our implementation tries to find a minimum property set. It works its way down through the nested sub-properties of the transform, trying to determine how they can be inferred. Each time it gets to an Alternatives, it will consider whether one of the alternatives can be inferred, and if so, it will specify the inferrable sub-property.