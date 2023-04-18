# 363442ee

## Examples

![ARC examples for 363442ee](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 363442ee](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

* We parse the input and output scenes into objects.
* These inputs make use of a vertical gray “divider”, but because that is contiguous with the multi-colored object on its left, the two things were getting combined into a single composite object.
   * To prevent this, we now check whether an object appears to be a vertical or horizontal divider, or a grid, and if so, we don’t form composite objects with it.
* By default, the multi-color 3x3 objects in the outputs were getting fused into large composite objects.
   * To prevent this, the heuristics for deciding when to avoid forming composite objects needed to be enhanced.
   * Already, we were looking for “notable sub-images”, but that code was being too conservative about when to break up objects that consisted of multiple instances of those notable sub-images.

#### Mapping Input Objects to Output Objects

We build a mapping from input objects to output objects.

e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* When the object mappings are grouped by color, we see that:
   * Blue objects are always replaced by the square object’s image.
   * Non-blue objects are left unmodified.
* We can also group by Area.Rank when finding rules. (Area.Rank of 1 means “largest object”)


![image 2](image2.png?raw=true)
