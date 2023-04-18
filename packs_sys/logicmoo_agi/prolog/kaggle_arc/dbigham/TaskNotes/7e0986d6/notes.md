# 7e0986d6

## Examples

![ARC examples for 7e0986d6](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 7e0986d6](rules.png?raw=true)

## Notes
The denoising required by this example requires a bit more sophistication:

* There are cases where noise has landed on the corner of a rectangle. When this happens, we can’t denoise such a pixel by choosing the most common surrounding color, so I added special handling for the corners of rectangles.
* In the test example, there are two diagonally adjacent noise pixels. These were getting treated as a single object, which wasn’t a pixels, so my denoiser wasn’t deleting it. We now watch out for diagonally attached pixels and break them apart.