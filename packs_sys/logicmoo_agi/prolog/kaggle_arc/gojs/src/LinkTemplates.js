import go from "gojs";

const $ = go.GraphObject.make;

const createLinkText = () =>
  $(
    "TextBlock",
    {
      font: "12px Arial",
      segmentOrientation: go.Link.OrientUpright,
      segmentOffset: new go.Point(0, -10)
    },
    new go.Binding("text", "category"),
    new go.Binding("font", "linkTextFont"),
    new go.Binding("stroke", "linkTextColor"),
    new go.Binding("scale", "linkTextScale")
  );

const createLinkArrowLine = () =>
  $(
    go.Shape,
    new go.Binding("stroke", "linkArrowColor"),
    new go.Binding("strokeWidth", "linkArrowStokeWidth")
  );

const createLinkArrow = () =>
  $(
    go.Shape,
    { toArrow: "Standard" },
    new go.Binding("stroke", "linkArrowColor"),
    new go.Binding("strokeWidth", "linkArrowStokeWidth"),
    new go.Binding("fill", "linkArrowColor")
  );

export const defaultLinkTemplate = $(
  go.Link,
  createLinkArrowLine(),
  createLinkArrow(),
  createLinkText()
);

export const defaultRadialLinkTemplate = $(
  go.Link,
  {
    routing: go.Link.Normal,
    curve: go.Link.Bezier,
    selectionAdorned: false
  },
  createLinkArrowLine(),
  createLinkArrow(),
  createLinkText()
);
