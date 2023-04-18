# 1e0a9b12

## Examples

![ARC examples for 1e0a9b12](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 1e0a9b12](rules.png?raw=true)

## Notes
The challenge with this task is producing a mapping from input objects to output objects, especially since there are many-to-one relationships, where in the past, we have only formed those if the many objects in the input all overlap the corresponding object in the output.

Our strategy to ease our way into more liberal one-to-many object mappings is to add some code to detect when it appears that the columns (or rows) of the input/output scene pairs seem to have unchanging color counts. In that case, we will assume that any number of input objects in that column (or row if that is how things are segmented) will correspond with the objects of that color in the output. This results in the one-to-many object mappings that we need, and the standard rule finder logic is successful after that point.
