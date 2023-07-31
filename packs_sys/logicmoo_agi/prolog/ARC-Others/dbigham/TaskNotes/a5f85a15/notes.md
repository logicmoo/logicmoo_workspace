# a5f85a15

## Examples

![ARC examples for a5f85a15](examples.png?raw=true)

## Rules (DSL)

![DSL rules for a5f85a15](rules.png?raw=true)

## Notes
When parsing lines, we now check if they contain a repeating pattern. If so, we represent their Shape property like:


![image 1](image1.png?raw=true)

Rule finding has been enhanced to support producing dynamic expression for this Fill sub-property, including its sub-parts:


![image 2](image2.png?raw=true)

As of October 11 2022, one improvement would be to instead refer to the InputObject in the Fill expression:


![image 3](image3.png?raw=true)
