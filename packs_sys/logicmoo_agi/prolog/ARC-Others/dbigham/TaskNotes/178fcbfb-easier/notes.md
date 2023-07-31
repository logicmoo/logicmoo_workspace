# 178fcbfb-easier

## Examples

![ARC examples for 178fcbfb-easier](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 178fcbfb-easier](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

We see that the properties of output objects can be predicted:

* The shape is always a line.
* The X position becomes 1.
* The ending position of the line, relative to the right side of the image, is 1.


![image 2](image2.png?raw=true)

Getting this input to work required enhancing the rule finding code to be able to consider different combinations of output properties as sufficient for specifying an output object. The metadata looks like this:


![image 3](image3.png?raw=true)

In our case, we use the following top-level MinimalPropertySet:


This specifies that if the following is true, that is one valid way to specify an output object:

* Either Shape or Shapes is required.
   * In our case “Shapes” is known.
* Color is required.
* Position is required.
* Either Width or X2Inverse is required.
   * In our case, X2Inverse is known. (and equal to 1)
   * We also added X2Inverse to the list of object properties when implementing this example.
* Either Height or Y2Inverse is required.
   * In our case, Height is known.