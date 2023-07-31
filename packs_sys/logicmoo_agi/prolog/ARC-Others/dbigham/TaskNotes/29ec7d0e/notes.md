# 29ec7d0e

## Examples

![ARC examples for 29ec7d0e](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 29ec7d0e](rules.png?raw=true)

## Notes
We notice that the input images are corrupted versions of the output images, where the repetition of a pattern can be used to impute the corrupted areas of the input images.


### Algorithm

* When in rule-finding mode, we use the following fail-fast heuristics to avoid wasting time:
   * If the input and output images aren't the same size, abort.
   * If the image doesn't have at least 3 colors, abort.
   * If the image’s most common color fills > 50% of the space, abort.
   * If the percentage difference from input to output is more than 22.5%, abort.
* Loop over 3x3 patches from top-left, considering up to 7 image patches. For each image patch:
   * If it has at least 3 colors, check how many instances there are of the patch within the full image.
   * If there are at least 4 total instances, use the image patch and proceed.
* For each instance of the patch, produce an image centered on that location.
* Combine all images produced above as follows:
   * Find all pixel locations where:
       * All images agree.
       * Not all images agree.
   * For cases where not all images agree, keep track of the possible colors.
   * Determine which color(s) are present in all disagreements. If there is no single color that is present in all disagreements, abort. (for now we assume there is a single corruption color)
   * If there is a single color present in all disagreements, that is the corruption color.
       * If there are multiple colors present in all disagreements... I’m not sure whether this could happen or not. For now we will abort.
   * We can now combine all images produced by centering on the various locations of the image patch.
       * Where there are pixel disagreements, ignore the corruption color.
       * If there are still pixel disagreements after ignoring the corruption color, abort.
* With the combined image available, go back to each instance of the 3x3 patch and apply the combined image, filling in corrupted pixels.
* Optional extensions for additional robustness, not yet implemented:
   * Check if this has resulted in any new instances of the 3x3 patch, and if so, apply the combined image from those locations. 	(FixedPoint)
   * If the resulting image still has corrupted pixels, choose another 3x3 patch and see if it fills in those corrupted pixels.