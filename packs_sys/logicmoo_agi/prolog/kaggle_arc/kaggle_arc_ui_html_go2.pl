/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

test_arcui2:-
 gensym(myDiagramDiv,ID),
 arc_html_format([`

  <script src="https://unpkg.com/gojs@2.2.16/release/go.js"></script>
  <p>
    This is a minimalist HTML and JavaScript skeleton of the GoJS Sample
    <a href="https://gojs.net/latest/samples/treeMapper.html">treeMapper.html</a>. It was automatically generated from a button on the sample page,
    and does not contain the full HTML. It is intended as a starting point to adapt for your own usage.
    For many samples, you may need to inspect the
    <a href="https://github.com/NorthwoodsSoftware/GoJS/blob/master/samples/treeMapper.html">full source on Github</a>
    and copy other files or scripts.
  </p>
  <div id="allSampleContent" class="p-4 w-full">

    <script id="code">

    loadjsfile("https://unpkg.com/gojs@2.2.16/release/go.js");

  // Use a TreeNode so that when a node is not visible because a parent is collapsed,
  // connected links seem to be connected with the lowest visible parent node.
  // This also forces other links connecting with nodes in the group to be rerouted,
  // because collapsing/expanding nodes will cause many nodes to move and to appear or disappear.
  class TreeNode extends go.Node {
    constructor() {
      super();
      this.treeExpandedChanged = node => {
        if (node.containingGroup !== null) {
          node.containingGroup.findExternalLinksConnected().each(l => l.invalidateRoute());
        }
      };
    }

    findVisibleNode() {
      // redirect links to lowest visible "ancestor" in the tree
      var n = this;
      while (n !== null && !n.isVisible()) {
        n = n.findTreeParentNode();
      }
      return n;
    }
  }
  // end TreeNode

  // Control how Mapping links are routed:
  // - "Normal": normal routing with fixed fromEndSegmentLength & toEndSegmentLength
  // - "ToGroup": so that the link routes stop at the edge of the group,
  //     rather than going all the way to the connected nodes
  // - "ToNode": so that they go all the way to the connected nodes
  //     but only bend at the edge of the group
  var ROUTINGSTYLE = "Normal";

  // If you want the regular routing where the Link.[from/to]EndSegmentLength controls
  // the length of the horizontal segment adjacent to the port, don't use this class.
  // Replace MappingLink with a go.Link in the "Mapping" link template.
  class MappingLink extends go.Link {
    getLinkPoint(node, port, spot, from, ortho, othernode, otherport) {
      if (ROUTINGSTYLE !== "ToGroup") {
        return super.getLinkPoint(node, port, spot, from, ortho, othernode, otherport);
      } else {
        var r = port.getDocumentBounds();
        var group = node.containingGroup;
        var b = (group !== null) ? group.actualBounds : node.actualBounds;
        var op = othernode.getDocumentPoint(go.Spot.Center);
        var x = (op.x > r.centerX) ? b.right : b.left;
        return new go.Point(x, r.centerY);
      }
    }

    computePoints() {
      var result = super.computePoints();
      if (result && ROUTINGSTYLE === "ToNode") {
        var fn = this.fromNode;
        var tn = this.toNode;
        if (fn && tn) {
          var fg = fn.containingGroup;
          var fb = fg ? fg.actualBounds : fn.actualBounds;
          var fpt = this.getPoint(0);
          var tg = tn.containingGroup;
          var tb = tg ? tg.actualBounds : tn.actualBounds;
          var tpt = this.getPoint(this.pointsCount-1);
          this.setPoint(1, new go.Point((fpt.x < tpt.x) ? fb.right : fb.left, fpt.y));
          this.setPoint(this.pointsCount-2, new go.Point((fpt.x < tpt.x) ? tb.left : tb.right, tpt.y));
        }
      }
      return result;
    }
  }
  // end MappingLink


    function init() {

      // Since 2.2 you can also author concise templates with method chaining instead of GraphObject.make
      // For details, see https://gojs.net/latest/intro/buildingObjects.html
      const $ = go.GraphObject.make;  // for conciseness in defining templates

      myDiagram =
        $(go.Diagram, "`,ID,`",
          {
            "commandHandler.copiesTree": true,
            "commandHandler.deletesTree": true,
            // newly drawn links always map a node in one tree to a node in another tree
            "linkingTool.archetypeLinkData": { category: "Mapping" },
            "linkingTool.linkValidation": checkLink,
            "relinkingTool.linkValidation": checkLink,
            "undoManager.isEnabled": true,
            "ModelChanged": e => {
              if (e.isTransactionFinished) {  // show the model data in the page's TextArea
                document.getElementById("mySavedModel").textContent = e.model.toJson();
              }
            }
          });

      // All links must go from a node inside the "Left Side" Group to a node inside the "Right Side" Group.
      function checkLink(fn, fp, tn, tp, link) {
        // make sure the nodes are inside different Groups
        if (fn.containingGroup === null || fn.containingGroup.data.key !== -1) return false;
        if (tn.containingGroup === null || tn.containingGroup.data.key !== -2) return false;
        //// optional limit to a single mapping link per node
        //if (fn.linksConnected.any(l => l.category === "Mapping")) return false;
        //if (tn.linksConnected.any(l => l.category === "Mapping")) return false;
        return true;
      }

      // Each node in a tree is defined using the default nodeTemplate.
      myDiagram.nodeTemplate =
        $(TreeNode,
          { movable: false, copyable: false, deletable: false },  // user cannot move an individual node
          // no Adornment: instead change panel background color by binding to Node.isSelected
          {
            selectionAdorned: false,
            background: "white",
            mouseEnter: (e, node) => node.background = "aquamarine",
            mouseLeave: (e, node) => node.background = node.isSelected ? "skyblue" : "white"
          },
          new go.Binding("background", "isSelected", s => s ? "skyblue" : "white").ofObject(),
          // whether the user can start drawing a link from or to this node depends on which group it's in
          new go.Binding("fromLinkable", "group", k => k === -1),
          new go.Binding("toLinkable", "group", k => k === -2),
          $("TreeExpanderButton",  // support expanding/collapsing subtrees
            {
              width: 14, height: 14,
              "ButtonIcon.stroke": "white",
              "ButtonIcon.strokeWidth": 2,
              "ButtonBorder.fill": "goldenrod",
              "ButtonBorder.stroke": null,
              "ButtonBorder.figure": "Rectangle",
              "_buttonFillOver": "darkgoldenrod",
              "_buttonStrokeOver": null,
              "_buttonFillPressed": null
            }),
          $(go.Panel, "Horizontal",
            { position: new go.Point(16, 0) },
            //// optional icon for each tree node
            //$(go.Picture,
            //  { width: 14, height: 14,
            //    margin: new go.Margin(0, 4, 0, 0),
            //    imageStretch: go.GraphObject.Uniform,
            //    source: "images/defaultIcon.png" },
            //  new go.Binding("source", "src")),
            $(go.TextBlock,
              new go.Binding("text", "key", s => "item " + s))
          )  // end Horizontal Panel
        );  // end Node

      // These are the links connecting tree nodes within each group.

      myDiagram.linkTemplate = $(go.Link);  // without lines

      myDiagram.linkTemplate =  // with lines
        $(go.Link,
          {
            selectable: false,
            routing: go.Link.Orthogonal,
            fromEndSegmentLength: 4,
            toEndSegmentLength: 4,
            fromSpot: new go.Spot(0.001, 1, 7, 0),
            toSpot: go.Spot.Left
          },
          $(go.Shape,
            { stroke: "lightgray" }));

      // These are the blue links connecting a tree node on the left side with one on the right side.
      myDiagram.linkTemplateMap.add("Mapping",
        $(MappingLink,
          { isTreeLink: false, isLayoutPositioned: false, layerName: "Foreground" },
          { fromSpot: go.Spot.Right, toSpot: go.Spot.Left },
          { relinkableFrom: true, relinkableTo: true },
          $(go.Shape, { stroke: "blue", strokeWidth: 2 })
        ));

      myDiagram.groupTemplate =
        $(go.Group, "Auto",
          { deletable: false, layout: makeGroupLayout() },
          new go.Binding("position", "xy", go.Point.parse).makeTwoWay(go.Point.stringify),
          new go.Binding("layout", "width", makeGroupLayout),
          $(go.Shape, { fill: "white", stroke: "lightgray" }),
          $(go.Panel, "Vertical",
            { defaultAlignment: go.Spot.Left },
            $(go.TextBlock,
              { font: "bold 14pt sans-serif", margin: new go.Margin(5, 5, 0, 5) },
              new go.Binding("text")),
            $(go.Placeholder, { padding: 5 })
          )
        );

      function makeGroupLayout() {
        return $(go.TreeLayout,  // taken from samples/treeView.html
          {
            alignment: go.TreeLayout.AlignmentStart,
            angle: 0,
            compaction: go.TreeLayout.CompactionNone,
            layerSpacing: 16,
            layerSpacingParentOverlap: 1,
            nodeIndentPastParent: 1.0,
            nodeSpacing: 0,
            setsPortSpot: false,
            setsChildPortSpot: false,
            // after the tree layout, change the width of each node so that all
            // of the nodes have widths such that the collection has a given width
            commitNodes: function() {  // overriding TreeLayout.commitNodes
              go.TreeLayout.prototype.commitNodes.call(this);
              if (ROUTINGSTYLE === "ToGroup") {
                updateNodeWidths(this.group, this.group.data.width || 100);
              }
            }
          });
      }

      // Create some random trees in each group
      var nodeDataArray = [
        { isGroup: true, key: -1, text: "Left Side", xy: "0 0", width: 150 },
        { isGroup: true, key: -2, text: "Right Side", xy: "300 0", width: 150 }
      ];
      var linkDataArray = [
        { from: 6, to: 1012, category: "Mapping" },
        { from: 4, to: 1006, category: "Mapping" },
        { from: 9, to: 1004, category: "Mapping" },
        { from: 1, to: 1009, category: "Mapping" },
        { from: 14, to: 1010, category: "Mapping" }
      ];

      // initialize tree on left side
      var root = { key: 0, group: -1 };
      nodeDataArray.push(root);
      for (var i = 0; i < 11;) {
        i = makeTree(3, i, 17, nodeDataArray, linkDataArray, root, -1, root.key);
      }

      // initialize tree on right side
      root = { key: 1000, group: -2 };
      nodeDataArray.push(root);
      for (var i = 0; i < 15;) {
        i = makeTree(3, i, 15, nodeDataArray, linkDataArray, root, -2, root.key);
      }
      myDiagram.model = new go.GraphLinksModel(nodeDataArray, linkDataArray);
    }

    // help create a random tree structure
    function makeTree(level, count, max, nodeDataArray, linkDataArray, parentdata, groupkey, rootkey) {
      var numchildren = Math.floor(Math.random() * 10);
      for (var i = 0; i < numchildren; i++) {
        if (count >= max) return count;
        count++;
        var childdata = { key: rootkey + count, group: groupkey };
        nodeDataArray.push(childdata);
        linkDataArray.push({ from: parentdata.key, to: childdata.key });
        if (level > 0 && Math.random() > 0.5) {
          count = makeTree(level - 1, count, max, nodeDataArray, linkDataArray, childdata, groupkey, rootkey);
        }
      }
      return count;
    }
    window.addEventListener('DOMContentLoaded', init);


    function updateNodeWidths(group, width) {
      if (isNaN(width)) {
        group.memberParts.each(n => {
          if (n instanceof go.Node) n.width = NaN;  // back to natural width
        });
      } else {
        var minx = Infinity;  // figure out minimum group width
        group.memberParts.each(n => {
          if (n instanceof go.Node) {
            minx = Math.min(minx, n.actualBounds.x);
          }
        });
        if (minx === Infinity) return;
        var right = minx + width;
        group.memberParts.each(n => {
          if (n instanceof go.Node) n.width = Math.max(0, right - n.actualBounds.x);
        });
      }
    }

    // this function is only needed when changing the value of ROUTINGSTYLE dynamically
    function changeStyle() {
      // find user-chosen style name
      var stylename = "ToGroup";
      var radio = document.getElementsByName("MyRoutingStyle");
      for (var i = 0; i < radio.length; i++) {
        if (radio[i].checked) {
          stylename = radio[i].value; break;
        }
      }
      if (stylename !== ROUTINGSTYLE) {
        myDiagram.commit(diag => {
          ROUTINGSTYLE = stylename;
          diag.findTopLevelGroups().each(g => updateNodeWidths(g, NaN));
          diag.layoutDiagram(true);  // force layouts to happen again
          diag.links.each(l => l.invalidateRoute());
        });
      }
    }
  </script>

<div id="sample">
  <div id="`,ID,`" style="border: 1px filltype(solid) black; width: 700px; height: 350px; position: relative; cursor: auto;"><canvas tabindex="0" style="position: absolute; top: 0px; left: 0px; z-index: 2; user-select: none; touch-action: none; width: 700px; height: 350px; cursor: auto;" width="700" height="350">This text is displayed if your browser does not support the Canvas HTML element.</canvas><div style="position: absolute; overflow: auto; width: 700px; height: 350px; z-index: 1;"><div style="position: absolute; width: 1px; height: 1px;"></div></div></div>
  <p>
    This sample is like the <a href="records.html">Mapping Fields of Records</a> sample,
    but it has a collapsible tree of nodes on each side, rather than a simple list of items.
    The implementation of the trees comes from the <a href="treeView.html">Tree View</a> sample.
  </p>
  <p>
    Draw new links by dragging from any field (i.e. any tree node).
    Reconnect a selected link by dragging its diamond-shaped handle.
    A minor enhancement to this diagram supports operator nodes that transform data from various fields on the left
    to provide values for fields on the right.
  </p>
  <p>
    This sample supports three different routing styles:<br>
    <input type="radio" name="MyRoutingStyle" onclick="changeStyle()" value="Normal">
    "Normal"<br>
    <input type="radio" name="MyRoutingStyle" onclick="changeStyle()" value="ToGroup" checked="checked">
    "ToGroup", where the links stop at the border of the group<br>
    <input type="radio" name="MyRoutingStyle" onclick="changeStyle()" value="ToNode">
    "ToNode", where the links bend at the border of the group but go all the way to the node, as normal<br>
  </p>
  <p>
    There is a variation of this sample where the tree on the right is mirrored,
    so that the links naturally connect closer to the nodes in the tree.
  </p>
  <p>The model data, automatically updated after each change or undo or redo:</p>
  <textarea id="mySavedModel" style="width:100%;height:300px">{ "class": "GraphLinksModel",
  "nodeDataArray": [
{"isGroup":true,"key":-1,"text":"Left Side","xy":"0 0","width":150},
{"isGroup":true,"key":-2,"text":"Right Side","xy":"300 0","width":150},
{"key":0,"group":-1},
{"key":1,"group":-1},
{"key":2,"group":-1},
{"key":3,"group":-1}, {"key":4,"group":-1}, {"key":5,"group":-1},
{"key":6,"group":-1}, {"key":7,"group":-1}, {"key":8,"group":-1},
{"key":9,"group":-1}, {"key":10,"group":-1}, {"key":11,"group":-1}, {"key":12,"group":-1},
{"key":13,"group":-1}, {"key":14,"group":-1},
{"key":15,"group":-1},
{"key":16,"group":-1}, {"key":17,"group":-1},
{"key":1000,"group":-2}, {"key":1001,"group":-2},
{"key":1002,"group":-2},
{"key":1003,"group":-2},
{"key":1004,"group":-2},
{"key":1005,"group":-2},
{"key":1006,"group":-2},
{"key":1007,"group":-2},
{"key":1008,"group":-2},
{"key":1009,"group":-2},
{"key":1010,"group":-2},
{"key":1011,"group":-2},
{"key":1012,"group":-2},
{"key":1013,"group":-2},
{"key":1014,"group":-2},
{"key":1015,"group":-2}
],
  "linkDataArray": [
{"from":6,"to":1012,"category":"Mapping"},
{"from":4,"to":1006,"category":"Mapping"},
{"from":9,"to":1004,"category":"Mapping"},
{"from":1,"to":1009,"category":"Mapping"},
{"from":14,"to":1010,"category":"Mapping"},
{"from":0,"to":1},
{"from":0,"to":2},
{"from":0,"to":3},
{"from":0,"to":4},
{"from":4,"to":5},
{"from":5,"to":6},
{"from":6,"to":7},
{"from":6,"to":8},
{"from":5,"to":9},
{"from":4,"to":10},
{"from":10,"to":11},
{"from":10,"to":12},
{"from":12,"to":13},
{"from":12,"to":14},
{"from":12,"to":15},
{"from":12,"to":16},
{"from":12,"to":17},
{"from":1000,"to":1001},
{"from":1000,"to":1002},
{"from":1002,"to":1003},
{"from":1003,"to":1004},
{"from":1004,"to":1005},
{"from":1004,"to":1006},
{"from":1004,"to":1007},
{"from":1004,"to":1008},
{"from":1004,"to":1009},
{"from":1004,"to":1010},
{"from":1004,"to":1011},
{"from":1004,"to":1012},
{"from":1004,"to":1013},
{"from":1003,"to":1014},
{"from":1014,"to":1015}
]}</textarea>
<p class="text-xs">GoJS version 2.2.16. Copyright 1998-2022 by Northwoods Software.</p></div>
    <p><a href="https://github.com/NorthwoodsSoftware/GoJS/blob/master/samples/treeMapper.html" target="_blank">View this sample page's source on GitHub</a></p></div>
  
  
 `]).

