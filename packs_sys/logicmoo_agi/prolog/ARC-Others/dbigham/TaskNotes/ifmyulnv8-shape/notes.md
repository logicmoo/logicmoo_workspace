# ifmyulnv8-shape

## Examples

![ARC examples for ifmyulnv8-shape](examples.png?raw=true)

## Rules (DSL)

![DSL rules for ifmyulnv8-shape](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

* We create a mapping from input objects to output objects.
* There are a couple of ways this could be done:
   * Treat the colored object as being moved and resized. (we don’t do this)
   * Treat the colored object as something that is removed, but which informs the characteristics of the colored square that should be added inside of the gray square. (we do this)
e.g.



![image 1](image1.png?raw=true)

Note that the apparently duplicated shapes above are actually a formatted display of:



![image 2](image2.png?raw=true)


#### Finding Rules from Object Mappings

* When the object mapping is grouped by the color of the input object, we notice that there are two cases:
   * The gray object has a component added.
   * The non-gray object is deleted.
* The remaining challenge is to produce a generalized representation of the colored component that is added to the gray square.
   * We see that it is always a square.
   * We see that its width is always two less than the width of the input object it’s being added to.
   * We see that its height is always two less than the width of the input object it’s being added to.
   * We see that its color is always the same as non-gray object.
   * We see that it is always positioned at {1, 1} relative to the input object it’s being added to.


![image 3](image3.png?raw=true)

* Note that the expression Inactive[*head*][...] is used to avoid the Wolfram Language from actually trying to evaluate that expression, since writing Plus[a, b] would normally get simplified to the sum of a + b, etc.
* Previously the AddComponents transforms I’ve modeled have always been able to infer their Image directly from the image of another object in the scene. This is the first time we can’t do that, whereby we need to model the AddComponents transform’s shapes + width + height + color, and then use those to infer the image. To allow this, I enhanced the metadata for AddComponents:

![image 4](image4.png?raw=true)

* Notice the “MinimalPropertySets”, which says that we can specify an AddComponents component either by knowing:
   * Option 1: {“Image”, “Position”}.
   * Option 2: {“Shapes”, “Width”, “Height”, “Color”, “Position”}
* We also specify that the Shapes property is ClassList -> True, which means that its value is a list of classes. This helps us when trying to generalize the rule so that we know we can take the intersection of all of the output objects that we’re trying to produce a rule for, and also that we can prune that list of classes to keep just the most specific class when we’re in the context of specify a transform in a rule output.
* I have created a little data structure where I can start to keep track of which things are more general than other things:

![image 5](image5.png?raw=true)
