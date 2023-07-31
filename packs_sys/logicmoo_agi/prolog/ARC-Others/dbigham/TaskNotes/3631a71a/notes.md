# 3631a71a

## Examples

![ARC examples for 3631a71a](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 3631a71a](rules.png?raw=true)

## Notes

### Detecting Off-center Symmetry


![image 1](image1.png?raw=true)

* Loop over 3x3 image patches which form a 5x5 grid of 3x3 patches around the center. (so a total area of 15x15 pixels, consisting of 25 image patches)  See the white square in the above image to see what this 15x15 area looks like in the first example image, and see the top-left 3x3 red square for the first 3x3 patch within that area.
   * Only consider patches with at least three colors. (where the background color can also be considered a color)
   * Find the following two other orientations of the image patch:
       * Flipped horizontally
       * Flipped vertically
   * For the flipped patches, find if they have any instances in the image.
       * For the horizontally flipped image patches, they must be to the right of our un-flipped image patch. For example 1’s input image, we don’t find this flipped image patch.
       * For the vertically flipped image patches, they must be below our un-flipped image patch. See the bottom-left 3x3 red square in the above image for where we find this vertically flipped patch in example 1’s input image.
   * For each instance of the vertically flipped patch, check if there is either:
       * A corresponding horizontally flipped patch to the right of the un-flipped image patch. For example 1’s input image, we don’t find this horizontally flipped image patch.
       * A corresponding vertically and horizontally flipped patch below and to the right of the un-flipped patch. For example1’s input image, we do find this vertically and horizontally flipped image patch. See the bottom-right 3x3 red square in the image above.
   * If so, take the triangle formed by the group of three image patches to infer what square area of the full image might by symmetrical. See the light green 30x30 square in the image above for what this square area looks like for example 1.
   * Use the algorithm from [9ecd008a](https://github.com/dbigham/ARC/blob/main/TaskNotes/9ecd008a/notes.md) to check whether that square area is symmetrical, and detect any corruptions that need fixing within the square area, and what their expected values are.
       * See the image below for these corrupted pixels, highlighted in white.
       * Note also that the 6 brown pixels along the left side of the image fall outside of this 30x30 area.  See the section below “Fixing Corruptions Outside of Primary Symmetry Square” for details fixing those corruptions.

![image 2](image2.png?raw=true)


### Fixing Corruptions Outside of Primary Symmetry Square

* The above can find the square within the image that is symmetrical, but since the symmetry is off-center, there will be strips along the side that may also have corruptions.
* For the first training example, there is a two-pixel-wide stripe along the left and top of the image that falls outside of the primary symmetrical square, which are highlighted below with red: (and corrupted pixels in those areas made white for purposes of demonstration)

![image 3](image3.png?raw=true)

* Within these areas, we use the corruption color detected from the main symmetry square area to identify corrupted pixels, and we may need to settle for a single other pixel in the image to let us know what the expected color is.