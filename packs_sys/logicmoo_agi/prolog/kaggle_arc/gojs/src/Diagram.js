import go from "gojs";
import { legalEntitiesTemplate } from "./NodeTemplates";
import { defaultLinkTemplate } from "./LinkTemplates";
import uuid from "uuid";

let diagram;

function refreshModel() {
  const mock = mockModel();
  diagram.model.applyIncrementalJson({
    class: "go.GraphLinksModel",
    incremental: 1,
    nodeKeyProperty: "key",
    linkKeyProperty: "key",
    modifiedNodeData: mock.nodeDataArray,
    modifiedLinkData: mock.linkDataArray
  });
}

window.refreshModel = refreshModel;

function generateNodeTemplateMap() {
  const nodeTemplateMap = new go.Map();
  nodeTemplateMap.add("", legalEntitiesTemplate);
  return nodeTemplateMap;
}

function generateLinkTemplateMap() {
  const linkTemplateMap = new go.Map();
  linkTemplateMap.add("", defaultLinkTemplate);
  return linkTemplateMap;
}

const colors = {
  red: "#ffb3ba",
  orange: "#F47321",
  green: "#C8DA2B",
  yellow: "#fff5ba",
  purple: "#a79aff"
};

const arrayColors = [
  colors.red,
  colors.orange,
  colors.green,
  colors.yellow,
  colors.purple
];

function getRandomInt(max) {
  return Math.floor(Math.random() * Math.floor(max)) + 1;
}

const generateRandomColor = () => {
  return arrayColors[getRandomInt(5) - 1];
};

const generateOpacity = () => Math.random();

const generateNodeStyles = () => ({
  nodeCircleFill: generateRandomColor(),
  nodeCircleStrokeWidth: getRandomInt(4),
  nodeCircleStroke: generateRandomColor(),
  nodeCircleOpacity: generateOpacity(),
  nodeScale: getRandomInt(3),
  nodeFill: generateRandomColor(),
  nodeOpacity: generateOpacity(),
  nodeTextColor: generateRandomColor(),
  nodeTextScale: getRandomInt(3)
});

const generateLinkStyles = () => ({
  linkTextColor: generateRandomColor(),
  linkTextScale: getRandomInt(3),
  linkArrowColor: generateRandomColor(),
  linkArrowStokeWidth: getRandomInt(8) + 1
});

const mockModel = () => ({
  nodeDataArray: [
    {
      name: "Entity",
      key: "entity-1",
      __typename: "Entity",
      ...generateNodeStyles()
    },
    {
      name: "Thing",
      key: "thing-1",
      __typename: "Thing",
      ...generateNodeStyles()
    },
    {
      name: "Fact",
      key: "fact-1",
      __typename: "Fact",
      ...generateNodeStyles()
    },
    {
      name: "Calc",
      key: "calc-1",
      __typename: "Calc",
      ...generateNodeStyles()
    },
    {
      name: "Thing",
      key: "thing-2",
      __typename: "Thing",
      ...generateNodeStyles()
    },
    {
      name: "Calc",
      key: "calc-3",
      __typename: "Calc",
      ...generateNodeStyles()
    },
    {
      name: "Calc",
      key: "calc-4",
      __typename: "Calc",
      ...generateNodeStyles()
    },
    {
      name: "Attribute",
      key: "attr-1",
      __typename: "Attribute",
      ...generateNodeStyles()
    },
    {
      name: "InterCompany",
      key: "ic-1",
      __typename: "InterCompany",
      ...generateNodeStyles()
    },
    {
      name: "Fact",
      key: "fact-3",
      __typename: "Fact",
      ...generateNodeStyles()
    },
    {
      name: "Calc",
      key: "calc-2",
      __typename: "Calc",
      ...generateNodeStyles()
    },
    {
      name: "Fact",
      key: "fact-2",
      __typename: "Fact",
      ...generateNodeStyles()
    }
  ],
  linkDataArray: [
    {
      key: 1,
      from: "entity-1",
      to: "thing-1",
      category: "has",
      ...generateLinkStyles()
    },
    {
      key: 2,
      from: "fact-1",
      to: "calc-1",
      category: "aggregate",
      ...generateLinkStyles()
    },
    {
      key: 3,
      from: "thing-2",
      to: "calc-3",
      category: "has",
      ...generateLinkStyles()
    },
    {
      key: 4,
      from: "thing-2",
      to: "calc-4",
      category: "has",
      ...generateLinkStyles()
    },
    {
      key: 5,
      from: "thing-2",
      to: "attr-1",
      category: "attribute",
      ...generateLinkStyles()
    },
    {
      key: 6,
      from: "calc-3",
      to: "calc-4",
      category: "aggregate",
      ...generateLinkStyles()
    },
    {
      key: 7,
      from: "ic-1",
      to: "entity-1",
      category: "payee",
      ...generateLinkStyles()
    },
    {
      key: 8,
      from: "ic-1",
      to: "fact-3",
      category: "has",
      ...generateLinkStyles()
    },
    {
      key: 9,
      from: "fact-3",
      to: "calc-3",
      category: "aggregate",
      ...generateLinkStyles()
    },
    {
      key: 10,
      from: "fact-3",
      to: "calc-2",
      category: "aggregate",
      ...generateLinkStyles()
    },
    {
      key: 11,
      from: "calc-2",
      to: "calc-1",
      category: "aggregate",
      ...generateLinkStyles()
    },
    {
      key: 12,
      from: "thing-1",
      to: "calc-2",
      category: "has",
      ...generateLinkStyles()
    },
    {
      key: 13,
      from: "thing-1",
      to: "attr-1",
      category: "attribute",
      ...generateLinkStyles()
    },
    {
      key: 14,
      from: "thing-1",
      to: "calc-1",
      category: "has",
      ...generateLinkStyles()
    },
    {
      key: 15,
      from: "thing-1",
      to: "fact-2",
      category: "has",
      ...generateLinkStyles()
    },
    {
      key: 16,
      from: "thing-1",
      to: "fact-1",
      category: "has",
      ...generateLinkStyles()
    },
    {
      key: 17,
      from: "fact-2",
      to: "calc-1",
      category: "aggregate",
      ...generateLinkStyles()
    }
  ]
});

export function init() {
  const $ = go.GraphObject.make;

  diagram = $(go.Diagram, "myDiagramDiv", {
    initialContentAlignment: go.Spot.Center,
    allowDrop: true,
    isReadOnly: false,
    allowLink: true,
    allowHorizontalScroll: true,
    allowVerticalScroll: true,
    allowZoom: true,
    allowSelect: true,
    initialAutoScale: go.Diagram.Uniform,
    contentAlignment: go.Spot.Center,
    nodeTemplateMap: generateNodeTemplateMap(),
    linkTemplateMap: generateLinkTemplateMap(),
    layout: $(go.LayeredDigraphLayout, {
      setsPortSpots: false,
      direction: 0,
      layerSpacing: 25,
      columnSpacing: 25,
      layeringOption: go.LayeredDigraphLayout.LayerOptimalLinkLength,
      initializeOption: go.LayeredDigraphLayout.InitDepthFirstOut,
      aggressiveOption: go.LayeredDigraphLayout.AggressiveLess,
      packOption: go.LayeredDigraphLayout.PackMedian
    })
  });
  diagram.toolManager.panningTool.isEnabled = false;
  diagram.toolManager.mouseWheelBehavior = go.ToolManager.WheelScroll;
  const mock = mockModel();
  diagram.model.nodeDataArray = mock.nodeDataArray;
  diagram.model.linkDataArray = mock.linkDataArray;
  diagram.model = go.GraphObject.make(go.GraphLinksModel, {
    linkFromPortIdProperty: "fromPort",
    linkToPortIdProperty: "toPort",
    nodeDataArray: [...mock.nodeDataArray],
    linkDataArray: [...mock.linkDataArray],
    nodeCategoryProperty: "category",
    linkKeyProperty: "key"
  });
  console.log(diagram.model.toJson());
}
