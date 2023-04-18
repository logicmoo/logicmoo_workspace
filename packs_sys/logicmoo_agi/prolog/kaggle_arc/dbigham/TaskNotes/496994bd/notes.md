# 496994bd

## Examples

![ARC examples for 496994bd](examples.png?raw=true)

## Rules (DSL)

![DSL rules for 496994bd](rules.png?raw=true)

## Notes
When we see vertical lines that span either the width or the height of an input, we by default treat them as a “divider” and segment objects so as to not include the supposed divider they are contiguous with.

If we can’t find a working rule set, we try parsing the scene again ignoring dividers, which allows this example to work.
