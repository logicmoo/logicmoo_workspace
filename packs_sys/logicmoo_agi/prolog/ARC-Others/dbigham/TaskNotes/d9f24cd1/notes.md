# d9f24cd1

## Examples

![ARC examples for d9f24cd1](examples.png?raw=true)

## Rules (DSL)

![DSL rules for d9f24cd1](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into single-color objects.


#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene with a matching color and overlapping position, making it straightforward to produce a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* We notice that gray objects are unchanged in the output.
* We notice that there doesn’t seem to be a simple rule to map red input objects to their corresponding output objects, so we check whether the red output objects can be “generated” using a consistent set of rules, which they can be.

#### Finding Rules to Generate the Red Output Objects

* We notice that all of the red output objects are “paths”.
* For each path, we represent it as a sequence of pixel locations.
* Each pixel location has both “input attributes” (e.g. Position, IncommingDirection) and “output attributes” (e.g. TurnDegrees, OutgoingDirection). For example, the  third red output object in the first training example is represented using the following list of input-to-output mappings:


![image 2](image2.png?raw=true)

* Because we can frame the problem of finding rules to generate these objects as a series of input-to-output mappings, we can re-use the strategy that we use for finding top-level rules that map input objects to output objects. i.e. We can try grouping the inputs by various input properties to see if the outputs become predictable.
* As part of that, we can re-use the notion of minimal property sets. For example, if we can predict an output’s OutgoingDirection, we have sufficient information. Or, if we can predict its TurnDegrees, we have sufficient information:

![image 3](image3.png?raw=true)

* For our example, here are the rules found for generating red objects:


![image 4](image4.png?raw=true)

* These rules specify that:
   * If we don’t have a “ColorAhead” due to being on the first pixel and not knowing our direction yet, then we should move up.
   * If the color ahead is the background color (-1), then up.
       * Case 1: We are already moving upward, and continue to do so.
       * Case 2: We have just moved one the right to avoid a gray pixel, whereby this will correctly get us moving upward again.
   * If the color ahead is gray (5), then go to the right.
   * If the next pixel ahead is outside of the scene (-2), then stop.
* The full set of rules for mapping top-level input objects to output objects are:


![image 5](image5.png?raw=true)
