# ifmyulnv8-dynamic-shape

## Examples

![ARC examples for ifmyulnv8-dynamic-shape](examples.png?raw=true)

## Rules (DSL)

![DSL rules for ifmyulnv8-dynamic-shape](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

See the [ifmyulnv8-shape](https://github.com/dbigham/ARC/blob/main/TaskNotes/ifmyulnv8-shape/notes.md) example for details.


#### Finding Rules from Object Mappings

* See the [ifmyulnv8-shape](https://github.com/dbigham/ARC/blob/main/TaskNotes/ifmyulnv8-shape/notes.md) example for details.
* Building on ifmyulnv8-shape, what is needed here is a new property called “Shape”, in addition to the existing property “Shapes”.
* The new “Shape” property is the most specific shape from the “Shapes” list.
* This allows us to use the shape of the referenceable non-gray object as the shape for the component we add inside of the gray square.


![image 1](image1.png?raw=true)

* We also add a new minimum property set {“Shape”, “Width”, “Height”, “Color”, “Position”} in AddComponents.Components:

![image 2](image2.png?raw=true)

* A good enhancement would be to allow Alternatives in the MinimalPropertySets structure, which would allow avoiding adding the third row in the above list, instead using “Shapes” | “Shape” within:

![image 3](image3.png?raw=true)
