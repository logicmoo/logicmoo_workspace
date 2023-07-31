# 29c11459

## Examples

![ARC examples for 29c11459](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 29c11459](rules.png?raw=true)

## Notes
If all inputs consisted of only one pair of pixels, this task would be substantially easier, but the test example has two pairs of pixels, where each pair needs to be transformed.


### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* We form multi-color composite objects in the output.

#### Mapping Input Objects to Output Objects

* Each of the pixels in the input get mapped to the corresponding composite object in the output.
* This results in a many-to-one mapping where each of the input pixels get mapped to a component of the corresponding output object.
* When we detect this many-to-one mapping, we defined a rule that specifies what pairs of input objects should be grouped in inputs to form non-contiguous composite objects:

![image 1](image1.png?raw=true)

* Upon forming this group, we see that we are mapping a composite object in the input to a composite object in the output, and the transformation doesn’t only involve adding new components, but also modifying components.  We thus form a MapComponents transform:


![image 2](image2.png?raw=true)


#### Finding Rules from Object Mappings

* We see that all input objects involve a MapComponents transform.
* We want to find a set of rules that can be used to map the components of any one of these input objects to the set of components of the corresponding output object.
* To do this, we can make a recursive call to our rule finding code, feeding it these component mappings, and it will find the following rules:

![image 3](image3.png?raw=true)

* Note that the above rules aren’t top-level rules, but rather rules used to map the components of a composite object to the components of the corresponding output object.
* We introduce a new referenceable object, “Parent”, to allow these rules to refer to the input composite object. The “Parent” object gets used here to infer the Y value of the gray pixel component that should be added to the output object:

![image 4](image4.png?raw=true)
