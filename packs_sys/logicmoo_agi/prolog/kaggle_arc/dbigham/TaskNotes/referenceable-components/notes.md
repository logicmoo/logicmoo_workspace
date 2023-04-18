# referenceable-components

## Examples

![ARC examples for referenceable-components](examples.png?raw=true)

## Rules (DSL)

![DSL rules for referenceable-components](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

We create a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* What we introduce here is the ability for rules to infer their conclusion property values using “referenceable objects” from the components of composite objects. We denote this with, for example:

* The “Context” -> “Component” means that we’re referring to the blue pixel that is a component of the current composite object that is being considered. (the one that is part of the current object mapping that is being transformed into a rule)
* In our example, we notice that the gray-and-blue objects always gains a red pixel, and the location of that red pixel is relative to the blue pixel component.
   * The red pixel’s relative Y value is equal to the blue pixel’s relative Y value.
       * Note: When we speak of "relative" values, we mean relative to the parent composite object's top-left corner.
   * The red pixel’s relative X value is equal to the blue pixel’s relative Y value + 1.


![image 2](image2.png?raw=true)

* I also needed to add support for referenceable object properties to be combined with arithmetic (e.g. + 1).
   * This broke some previous ARC examples, because some of them started using math expressions where that was undesirable.
   * To fix this, I needed to:
       * Update some rule finding code to be less greedy -- rather than using the first property value expression it could find, it should consider each possibility and choose the best.
       * I updated the code that chooses the best expression to take into account the “complexity” of the different options, and put some weight toward favoring simpler expressions. (Occam’s Razor)