# feca6190

## Examples

![ARC examples for feca6190](examples.png?raw=true)

## Rules (DSL)

![DSL rules for feca6190](rules.png?raw=true)

## Notes
This example builds on [d13f3404](https://github.com/dbigham/ARC/blob/main/TaskNotes/d13f3404/notes.md).

* Support for output scene properties like Width and Height being functions of more general input scene properties like ColorCount.
* Support for PatternFill properties being functions of input scene properties.
e.g.


![image 1](image1.png?raw=true)


### Expressions Involving Times

Previously, expressions like the above had only made use of the Plus operator, so using Times is new to this example. To detect the need for times, we take the values needing to be inferred, such as:


![image 2](image2.png?raw=true)

... and the property values being considered for use to infer those values, such as:


![image 3](image3.png?raw=true)

... and we divide the two lists:


![image 4](image4.png?raw=true)


![image 5](image5.png?raw=true)

If we find that the quotient is consistent, then we can form an expression such as:


![image 6](image6.png?raw=true)
