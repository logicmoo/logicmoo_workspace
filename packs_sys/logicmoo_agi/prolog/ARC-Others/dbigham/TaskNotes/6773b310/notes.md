# 6773b310

## Examples

![ARC examples for 6773b310](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 6773b310](rules.png?raw=true)

## Notes
If the number of rows/columns of the input grid area equal to the height/width of the output (in pixels), then we check if a rule set can be found that maps from grid cells to pixels:

* We first check whether a rule set can be found that is specific to each grid cell individually. (in theory that could work for this input, but there are insufficient examples when looking at grid cells in isolation to be sufficiently confident about the rules)
* Next, we check whether a single rule set can be found that can be applied to any grid cell to produce a corresponding output pixel.