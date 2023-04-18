# 25d8a9c8

## Examples

![ARC examples for 25d8a9c8](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 25d8a9c8](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* At first, the input objects are parsed as single, composite objects, since they are contiguous structures.
   * When we parse the scene this way, however, we don’t find a workable rule set.
   * We notice that all inputs are interpreted as a single composite object, so we take that as evidence that it would be worth re-interpreting the scene as individual objects to see if that yields a rule set. (which it does)
   * If we find that re-interpreting the scene leads to a good rule set, we add a key/value to the output to indicate that when applying the rules, composite objects shouldn’t be formed but rather left as individual objects:

* A challenge with training example 3 is that the output consists of two horizontal gray lines that form a single contiguous rectangle, but we need them to be represented as individual horizontal lines so that we can build a coherent mapping from input objects to output objects.
   * We notice that the gray rectangle in the output scene corresponds perfectly to multiple objects in the input, so we interpret it as two individual rectangles rather than 1.
   * Unfortunately this breaks 3c9b0459 because in that case, output objects get split up which we wish to remain together. This implies that my approach here is dangerous and doesn’t generalize very well. For now, I’ve added a condition so that it will only try splitting apart output objects in cases like this where we are doing a re-interpretation of the scene upon the initial interpretation failing to find a rule set.

#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

See the “Scene Parsing” section for notes about needing to split up the output rectangle for training example 3.


#### Finding Rules from Object Mappings

* We group the object mappings using the Shape property, and find rules for transforming inputs to outputs.
* The resulting rules are unwieldy because they split up the different deletion cases by shape.
   * It discovers that vertical lines of length two can be deleted.
   * It discovers that squares can be deleted.
   * It discovers that, while other shapes don’t have enough examples to warrant individual rules, it can form an Except rule to cover the deletion of the other objects.


![image 1](image1.png?raw=true)

* This is quite unsightly, and has the potential to cause future examples to fail because our use of Occam’s Razor may lead us to reject the best rules for rules that may not generalize to the test example.
* We employ a rule simplification step that looks for ways to combine Alternatives and Except[_Alternatives]:


![image 2](image2.png?raw=true)
