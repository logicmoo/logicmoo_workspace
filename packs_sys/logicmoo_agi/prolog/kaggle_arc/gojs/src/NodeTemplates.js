import go from "gojs";

const $ = go.GraphObject.make;

const geometryStringMapping = {
  Entity: "F M0,0 h25 v25 h-25 z",
  Calc:
    "F M28.807,10.149c-0.139-0.769-0.871-1.295-1.635-1.142l-2.087,0.367l-2.682-3.089l-0.133-0.179l0.023-0.069l0.723-1.987	c0.129-0.355,0.112-0.738-0.049-1.081c-0.161-0.34-0.444-0.6-0.795-0.726l-4.472-1.629c-0.732-0.254-1.539,0.119-1.805,0.845	L15.175,3.44l-4.017,0.799l-0.181,0.043l-0.085-0.074L9.527,2.58C9.03,1.983,8.136,1.906,7.542,2.408L3.9,5.467	C3.612,5.709,3.434,6.049,3.401,6.422C3.367,6.799,3.48,7.166,3.724,7.457l1.361,1.62l-1.32,3.867L3.688,13.1l-2.18,0.429	c-0.767,0.138-1.278,0.873-1.141,1.635l0.825,4.685c0.064,0.374,0.273,0.695,0.582,0.912c0.311,0.217,0.689,0.303,1.053,0.233	l2.087-0.367l2.681,3.089l0.128,0.167l-0.018,0.081l-0.721,1.981c-0.129,0.352-0.112,0.738,0.049,1.081	c0.159,0.343,0.444,0.6,0.795,0.729l4.472,1.629c0.158,0.055,0.317,0.083,0.476,0.083c0.577,0,1.121-0.358,1.329-0.928l0.721-1.981	l4.094-0.821c0.066,0,0.1-0.046,0.128-0.024l1.425,1.705c0.243,0.288,0.585,0.465,0.963,0.496c0.37,0.028,0.739-0.086,1.023-0.325	l3.648-3.059c0.589-0.499,0.667-1.39,0.171-1.984l-1.362-1.626l1.346-3.95c0.017-0.043,0.025-0.078,0.058-0.117l2.174-0.385	c0.764-0.138,1.277-0.87,1.141-1.632L28.807,10.149z M17.154,19.616c-0.693,0.321-1.425,0.475-2.145,0.475	c-1.921,0-3.764-1.087-4.626-2.939c-0.576-1.234-0.637-2.618-0.171-3.895c0.464-1.28,1.399-2.302,2.633-2.875	c2.544-1.182,5.585-0.083,6.774,2.465C20.805,15.393,19.699,18.431,17.154,19.616z",
  Fact:
    "F M15,0C6.7,0,0,6.7,0,15s6.7,15,15,15s15-6.7,15-15S23.3,0,15,0z M14.1,7.5c0.4-0.4,0.9-0.6,1.6-0.6c0.6,0,1,0.1,1.3,0.4	s0.5,0.7,0.5,1.2c0,0.6-0.2,1.1-0.6,1.5c-0.4,0.4-1,0.6-1.6,0.6c-0.5,0-1-0.1-1.3-0.4S13.5,9.5,13.5,9C13.5,8.4,13.7,7.9,14.1,7.5z	 M17.7,22.1c-1,0.3-1.8,0.6-2.5,0.7c-0.7,0.1-1.3,0.2-1.8,0.2c-0.6,0-1.1-0.2-1.5-0.5c-0.3-0.3-0.5-0.8-0.5-1.3	c0-0.2,0.1-0.5,0.1-0.8c0-0.3,0.1-0.7,0.2-1.1l1.1-5.1L11,14v-1l4.5-1h1.3l-1.3,6.5c-0.1,0.4-0.2,0.7-0.2,1	c-0.1,0.3-0.1,0.6-0.1,0.8c0,0.7,0.3,1,0.9,1c0.1,0,0.4,0,0.5,0c0.2,0,0.6-0.1,1.1-0.2V22.1z",
  Attribute:
    "F M15,0C6.7,0,0,6.7,0,15c0,8.3,6.7,15,15,15s15-6.7,15-15C30,6.7,23.3,0,15,0z M9.5,17.3c-1,0-1.9-0.8-1.9-1.9	s0.8-1.9,1.9-1.9s1.9,0.8,1.9,1.9S10.5,17.3,9.5,17.3z M15.2,17.3c-1,0-1.9-0.8-1.9-1.9s0.8-1.9,1.9-1.9s1.9,0.8,1.9,1.9	S16.3,17.3,15.2,17.3z M21,17.3c-1,0-1.9-0.8-1.9-1.9s0.8-1.9,1.9-1.9c1,0,1.9,0.8,1.9,1.9S22,17.3,21,17.3z",
  Thing:
    "F m12.5,5.5  l-1.25,-.5 a2,2 0 0,0 0,1.5 l1.25,-.5  a.25,.25 0 0,0  0,-.5z   M7.75,22.5 a15,15 0 0,0 9,0  L16,10 H9 Z  M8.5,10 H16.5 V8 H8.5 Z M9.5,8 H15.5 V5 a2,2 0 1,0 -6,0 V8 Z M10.25,11.5 h2.5 v1.5 l-2.7,.5 Z M10,15 l2.75,-.5 v.25 l-3,.5 Z M12.5,0 a12.5,12.5 0 0,1 0,25 a12.5,12.5 0 0,1 0,-25 Z ",
  Equity:
    "F M 12.5,7.5 11,7.5 11,17.5 7.5,15 12.5,22.5 17.5,15 14,17.5 14,7.5 12.5,7.5 Z M12.5,2.5 a2.5,2.5 0 0,0 0,5 a2.5,2.5 0 0,0 0,-5 Z M12.5,0 a12.5,12.5 0 0,1 0,25 a12.5,12.5 0 0,1 0,-25 Z ",
  Loan:
    "F M 7.5,12.5 7.5,14 17.5,14 15,17.5 22.5,12.5 15,7.5 17.5,11 7.5,11 7.5,12.5 Z M5,10 a2.5,2.5 0 0,0 0,5 a2.5,2.5 0 0,0 0,-5 Z M12.5,0 a12.5,12.5 0 0,1 0,25 a12.5,12.5 0 0,1 0,-25 Z ",
  InterCompany:
    "F M 10,17.5 7.5,14 17.5,14 15,17.5 22.5,12.5 15,7.5 17.5,11 7.5,11 10,7.5 2.5,12.5 10,17.5 Z M12.5,0 a12.5,12.5 0 0,1 0,25 a12.5,12.5 0 0,1 0,-25 Z ",
  Root: "F M0,0 h25 v25 h-25 z",
  Unknown:
    "F M7 11h2v2h-2zM11 4c0.552 0 1 0.448 1 1v3l-3 2h-2v-1l3-2v-1h-5v-2h6zM8 1.5c-1.736 0-3.369 0.676-4.596 1.904s-1.904 2.86-1.904 4.596c0 1.736 0.676 3.369 1.904 4.596s2.86 1.904 4.596 1.904c1.736 0 3.369-0.676 4.596-1.904s1.904-2.86 1.904-4.596c0-1.736-0.676-3.369-1.904-4.596s-2.86-1.904-4.596-1.904zM8 0v0c4.418 0 8 3.582 8 8s-3.582 8-8 8c-4.418 0-8-3.582-8-8s3.582-8 8-8z"
};

export const parseGeometryString = typeName => {
  if (geometryStringMapping[typeName]) {
    return go.Geometry.parse(geometryStringMapping[typeName]);
  }
  return go.Geometry.parse(geometryStringMapping.Unknown);
};
export const isFromPalette = isFromPalette => {
  //if node is in the Gojs palette, don't show selectionAdorned
  return !isFromPalette;
};

const createNodeCircle = () =>
  $(
    go.Shape,
    "Circle",
    {
      fill: "#00B5CB",
      strokeWidth: 4,
      stroke: "#888",
      width: 45,
      height: 45,
      scale: 1
    },
    new go.Binding("fill", "nodeCircleFill"),
    new go.Binding("strokeWidth", "nodeCircleStrokeWidth"),
    new go.Binding("stroke", "nodeCircleStroke"),
    new go.Binding("opacity", "nodeCircleOpacity"),
    new go.Binding("scale", "nodeScale")
  );

const createNodeShape = (isRoot = false) =>
  $(
    go.Shape,
    new go.Binding(
      "geometry",
      isRoot ? "__category" : "__typename",
      parseGeometryString
    ),
    {
      fill: "white",
      stroke: null,
      fromLinkableDuplicates: true,
      toLinkableDuplicates: true
    },
    new go.Binding("fill", "nodeFill"),
    new go.Binding("opacity", "nodeOpacity"),
    new go.Binding("scale", "nodeScale"),
    new go.Binding("geometry", "nodeSVGShape", shape =>
      go.Geometry.parse(shape)
    )
  );

const createNodeText = (customDefaults = {}, isRoot = false) =>
  $(
    "TextBlock",
    { font: "12px Arial", ...customDefaults },
    new go.Binding("text", isRoot ? "__category" : "__typename"),
    new go.Binding("font", "nodeTextFont"),
    new go.Binding("stroke", "nodeTextColor"),
    new go.Binding("scale", "nodeTextScale")
  );

const createNodeAdornment = () =>
  $(
    go.Adornment,
    "Auto",
    $(
      go.Shape,
      "Circle",
      { fill: "transparent", stroke: "#04cae0", strokeWidth: 2 },
      new go.Binding("stroke", "nodeAdornmentColor"),
      new go.Binding("strokeWidth", "nodeAdornmentStrokeWidth")
    ),
    $(go.Placeholder)
  );

export const legalEntitiesTemplate = $(
  go.Node,
  "Vertical",
  new go.Binding("selectionAdorned", "isFromPalette", isFromPalette),
  new go.Binding("location", "coordinates", go.Point.parse).makeTwoWay(
    go.Point.stringify
  ),
  {
    selectionAdornmentTemplate: createNodeAdornment(),
    mouseEnter: function(event, node) {
      node.findPort("initiatingPort").visible = !node.data.isFromPalette;
    },
    mouseLeave: function(event, node) {
      node.findPort("initiatingPort").visible = false;
    }
  },
  $(
    go.Panel,
    "Spot",
    { name: "node" },
    createNodeCircle(),
    createNodeShape(),
    $(go.Shape, "Circle", {
      fromLinkable: true,
      width: 6,
      height: 6,
      visible: false,
      portId: "initiatingPort"
    }),
    $(go.Shape, "Circle", {
      fromLinkable: true,
      toLinkable: true,
      width: 6,
      height: 6,
      visible: true,
      opacity: 0,
      portId: "toPort"
    })
  ),
  createNodeText()
);

export function radialRotateNode(node, angle) {
  // rotate the nodes and make sure the text is not upside-down
  node.angle = angle;
  const label = node.findObject("TEXTBLOCK");
  if (label !== null) {
    label.angle = (angle > 90 && angle < 270) || angle < -90 ? 180 : 0;
  }
}

export function radialCommitLayers() {
  // optional: add circles in the background
  // need to remove any old ones first
  // don't remove this references because it is to access to internal props inside of the diagram
  // @ts-ignore
  const diagram = this.diagram;
  const gridlayer = diagram.findLayer("Grid");
  if (gridlayer) {
    const circles = new go.Set(/*go.Part*/);
    // @ts-ignore
    gridlayer.parts.each(function(circle) {
      if (circle.name === "CIRCLE") circles.add(circle);
    });
    circles.each(function(circle) {
      diagram.remove(circle);
    });
    // add circles centered at the root
    const $ = go.GraphObject.make; // for conciseness in defining templates
    // @ts-ignore
    for (let lay = 1; lay <= this.maxLayers; lay++) {
      // @ts-ignore
      const radius = lay * this.layerThickness;
      const circle = $(
        go.Part,
        { name: "CIRCLE", layerName: "Grid" },
        // @ts-ignore
        { locationSpot: go.Spot.Center, location: this.root.location },
        $(
          go.Shape,
          "Circle",
          { width: radius * 2, height: radius * 2 },
          { fill: "rgba(200,200,200,0.2)", stroke: null }
        )
      );
      diagram.add(circle);
    }
  }
}

export const generateDefaultRadialNodes = settings =>
  $(
    go.Node,
    "Vertical",
    new go.Binding("location", "coordinates", go.Point.parse).makeTwoWay(
      go.Point.stringify
    ),
    {
      locationSpot: go.Spot.Center,
      selectionAdorned: false,
      ...settings
    },
    $(
      go.Panel,
      "Spot",
      { name: "radialNode" },
      createNodeCircle(),
      createNodeShape()
    ),
    createNodeText({
      name: "TEXTBLOCK",
      alignment: go.Spot.Right,
      alignmentFocus: go.Spot.Left
    })
  );

export const generateRootRadialNode = () =>
  $(
    go.Node,
    "Auto",
    {
      locationSpot: go.Spot.Center,
      selectionAdorned: false
    },
    $(
      go.Panel,
      "Spot",
      { name: "radialRootNode" },
      createNodeCircle(),
      createNodeShape(true),
      createNodeText(
        { name: "TEXTBLOCK", alignment: go.Spot.BottomCenter },
        true
      )
    )
  );
