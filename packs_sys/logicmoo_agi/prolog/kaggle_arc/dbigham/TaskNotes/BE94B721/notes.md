# BE94B721

## Examples

![ARC examples for BE94B721](examples.png?raw=true)

## Rules (DSL)

![DSL rules for BE94B721](rules.png?raw=true)

## Notes
The rule set that was being found didn’t generalize properly:


![image 1](image1.png?raw=true)

Specifically, the following rule wasn’t general enough, because in the test image, the 4th largest image also needs to be deleted:


![image 2](image2.png?raw=true)

I implemented a function to watch for cases like this so that a rule such as the above would get replaced with:


![image 3](image3.png?raw=true)
