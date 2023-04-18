# a61f2674

## Examples

![ARC examples for a61f2674](examples.png?raw=true)

## Rules (DSL)

![DSL rules for a61f2674](rules.png?raw=true)

## Notes

### Finding Rules


#### Scene Parsing

We parse the input and output scenes into objects.


#### Mapping Input Objects to Output Objects

For each input object, there are two cases:

* There is only one object in the output scene with a matching position and shape.
* There object doesn’t occur in the output and should be deleted.
e.g.



![image 1](image1.png?raw=true)


#### Finding Rules from Object Mappings

* Before looking for rules, we prune the outputs of the mappings to keep only those properties that are being modified. We notice that the shape is always the same, it is just the “Colors” property that is changing.
* When the objects are grouped by HeightRank, we notice that the “Colors” property can be determined if the HeightRank is 1 (shortest object), and we notice that the 2nd and 3rd shortest objects are always deleted.
* When the objects are grouped by HeightInverseRank, we notice that the “Colors” property can be determined if the HeightInverseRank is 1 (tallest object), and we notice that the 2nd and 3rd tallest objects are always deleted.


![image 2](image2.png?raw=true)

* Unfortunately, the rules currently being produced for the deletion cases rely on particular HeightRank or HeightInverseRank values, which will only work if there are at most 6 input objects. Ideally we’d like a single deletion rule that is sufficiently general.
* As of August 8th 2022, we are only accepting and returning a set of rules if the property used in rule patterns is the same for every rule. (e.g. All rules must only use HeightRank in their pattern, or only use HeightInverseRank in their pattern, but not a mixture) I know this is too restrictive, but I’m doing that for now since otherwise we can get an explosion of bad rules being discovered, and it’s not clear how to safely combine rules with patterns that use different properties without drowning in a sea of bad rulesets.
* The above is problematic here, because we need rules where some of them use HeightRank in their pattern, and some of them use HeightInverseRank.
* I’ve worked around this by adding a heuristic that says that using rules for both the Rank and InverseRank of the same property is OK.
* As of September 2022, the addition of rule simplification code means that a couple pairs of rules get combined:


![image 3](image3.png?raw=true)


#### Possible Improvements to Rules

* Ideally we'd like a single deletion rule, such as: <|"HeightRank" -> Except[1], "HeightInverseRank" -> Except[1]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
* Another approach here would be to notice that besides the HeightRank 1 and HeightInverseRank 1 objects, all other objects should be deleted, similar to an ELSE clause of an IF statement. i.e. This fallthrough rule could use a pattern like:  “Else” -> <|”Transform” -> <|”Type” -> “Delete”|>|>
* I’ll need to put more thought into how to detect when an ELSE rule could be used. Some initial thoughts:
   * Some property values can only be assigned to one object in a scene. The Rank and InverseRank properties are examples of such properties.
   * Thus, if there are a set of rules S with the same conclusion, and all other rules involve patterns that can match at most one object, perhaps the set of rules S could be unified into an ELSE rule.