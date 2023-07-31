# e5790162

## Examples

![ARC examples for e5790162](examples.png?raw=true)

## Rules (DSL)

![DSL rules for e5790162](rules.png?raw=true)

## Notes
NOTE: My intention has been to not analyze and implement evaluation examples, but I came across this as a good and simple example of a “generator” task on https://volotat.github.io/ARC-Game/ and didn’t notice until I had implemented it that it was an evaluation example.


### Finding Rules


#### Scene Parsing

We parse the input and output scenes into single-color objects.


#### Mapping Input Objects to Output Objects

For each input object, there is only one object in the output scene with a matching color and overlapping position, making it straightforward to produce a mapping from input objects to output objects.

e.g.


![image 1](image1.png?raw=true)


![image 2](image2.png?raw=true)


#### Finding Rules from Object Mappings

* We notice that pink and blue objects are unchanged in the output.
* We notice that there doesn’t seem to be a simple rule to map green input objects to their corresponding output objects, so we check whether the green output objects can be “generated” using a consistent set of rules, which they can be.

#### Finding Rules to Generate the Green Output Objects

* We notice that all of the green output objects are “paths” involving turns.
* For each path, we represent it as a sequence of pixel locations.
* Each pixel location has both “input attributes” (e.g. Position, IncommingDirection) and “output attributes” (e.g. TurnDegrees, OutgoingDirection). For example, the green output object in the first training example is represented using the following list of input-to-output mappings:


![image 3](image3.png?raw=true)

* Because we can frame the problem as a series of input-to-output mappings, we can re-use the strategy that we use for finding top-level rules that map input objects to output objects. i.e. We can try grouping the inputs by various input properties to see if the outputs become predictable.
* As part of that, we can re-use the notion of minimal property sets. For example, if we can predict an output’s OutgoingDirection, we have sufficient information. Or, if we can predict its TurnDegrees, we have sufficient information:

![image 4](image4.png?raw=true)

* For our example, here are the rules found for generating green objects:


![image 5](image5.png?raw=true)

* These rules specify that:
   * If we don’t have a “ColorAhead” due to being on the first pixel and not knowing our direction yet, then we should move one to the right.
   * If the color ahead is the background color (-1), then move forward.
   * If the color ahead is pink (6), then turn right.
   * If the color ahead is blue (8), then turn left.
   * If the next pixel ahead is outside of the scene (-2), then stop.
* The full set of rules for mapping top-level input objects to output objects are:


![image 6](image6.png?raw=true)
