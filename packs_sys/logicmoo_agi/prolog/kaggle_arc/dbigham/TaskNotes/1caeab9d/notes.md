# 1caeab9d

## Examples

![ARC examples for 1caeab9d](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 1caeab9d](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* When parsing the output scene of example 2, we only get 1 contiguous object since the rectangles are touching.
   * However, this object does have three (colored)  components. i.e. <|..., “Components” -> {..., ..., ...}, ...|>
   * Strategy 1: If there are any composite objects, do further analysis to decide whether they should be kept or any parts broken off and promoted to top-level objects in the scene.
       * For each composite object, if we can map it‘s entire image to an object with the same image in the other scene, keep it.
       * Otherwise, for each part of the component, check if it can uniquely map to a top-level object in the corresponding input/output scene.
       * If we can, we will remove it from the composite object and treat it as a top-level object.
   * Strategy 2: A simpler strategy -- which works in this case -- is that upon finding that we can’t find a rule set when forming multi-color contiguous objects, we try parsing the scene again, only forming single-color objects, after which a rule set can be found.

#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene with a matching color and shape, making it straightforward to produce a mapping from input objects to output objects. Each mapping involves a Move transform.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* It is straightforward to notice that if an object is blue, it stays constant, and that red and yellow objects always move.
* Before we can specify the rules for red and yellow, objects, we first need to notice that there are always the following objects:
   * A red object: Object[<|”Colors” -> {“Red”}|>]
   * A yellow object: Object[<|”Colors” -> {“Yellow”}|>]
   * A blue object: Object[<|”Colors” -> {“Blue”}|>]
* Given the above referenceable objects, upon considering the move transforms for one of the remaining colors (yellow or red), we can check whether the parameters of those move transforms can be reliably inferred using any of the properties of any of the referenceable objects.
   * In our case, we notice that ObjectValue[<|"Colors" -> {"Blue"}|>, "Y"] can be used to infer the "Y" value of the move transforms. 
   * We also notice that the “X” coordinate doesn’t change, so we can eliminate it from the move transforms.


![image 2](image2.png?raw=true)

* As of August 20 2022, we now have rule simplification code that groups rules with the same conclusion and a condition on the same property:


![image 3](image3.png?raw=true)

* As of September 9 2022, we’ve improved the rule finding code so that even without grouping objects by color, it notices that the Y values of the output objects are all that’s needed to form a single rule:


![image 4](image4.png?raw=true)
