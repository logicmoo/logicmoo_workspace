# ed36ccf7

## Examples

![ARC examples for ed36ccf7](examples.png?raw=true)

## Rules (DSL)

![DSL rules for ed36ccf7](rules.png?raw=true)

## Notes
When parsing the scene into objects, by default we form different objects for disconnected things. That is problematic here, because the entire scene is being rotated, and in some cases there are multiple disconnected objects which can’t each be rotated, but rather need to be rotated as a group / single object.

We now start by parsing the scene in the standard way, but if that fails to produce a good rule set, we try parsing the scene as a single object and then see if that yields a working rule set.
