/*  Part of Logicmoo's SWISH

    Author:        Douglas Miles
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.logicmoo.org
    Copyright (C): 2014-2016, VU University Amsterdam
			      CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/
function mouseoverHilite(class_action, class_id, anchors, debug_item) {
    for (const anchor of anchors || []) {
        const sig = document.getElementById(signature_prefix + anchor);
        if (sig) {
            sig.classList[class_action](class_id); // sig.classList.{add,remove}(class_id)
        } else {
            if (class_action === 'add') {
                console.trace('No edge for anchor', anchor, debug_item);
                // alert('No edge for ' + debug_item);
            }
        }
    }
}
// butterfly_term.js
// Callback for a click on a token (anchor) in the source display
async function clickAnchor(mouse_target, source_item) {
    console.assert(mouse_target.id.startsWith(signature_prefix), 'Invalid signature_prefix', mouse_target.id, 'should start with:', signature_prefix);
    const signature = mouse_target.id.substr(signature_prefix.length);
    await fetchFromServer({
            anchor_xref: {
                signature: signature,
                corpus: source_item.corpus,
                root: source_item.root,
                path: source_item.path,
                language: 'python'
            }
        }, // TODO: don't hard-code language
        data => setXref(source_item, mouse_target.id, data));
}


function loadcssfile(filename) {
    var fileref = document.createElement("link")
    fileref.setAttribute("rel", "stylesheet")
    fileref.setAttribute("type", "text/css")
    fileref.setAttribute("href", filename)
    if (typeof fileref != "undefined")
        document.getElementsByTagName("head")[0].appendChild(fileref)
}

function loadjsfile<body>
  <!-- This top nav is not part of the sample code -->
  <nav id="navTop" class="w-full z-30 top-0 text-white bg-nwoods-primary">
    <div class="w-full container max-w-screen-lg mx-auto flex flex-wrap sm:flex-nowrap items-center justify-between mt-0 py-2">
      <div class="md:pl-4">
        <a class="text-white hover:text-white no-underline hover:no-underline
        font-bold text-2xl lg:text-4xl rounded-lg hover:bg-nwoods-secondary " href="../">
          <h1 class="mb-0 p-1 ">GoJS</h1>
        </a>
      </div>
      <button id="topnavButton" class="rounded-lg sm:hidden focus:outline-none focus:ring" aria-label="Navigation">
        <svg fill="currentColor" viewBox="0 0 20 20" class="w-6 h-6">
          <path id="topnavOpen" fill-rule="evenodd" d="M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM9 15a1 1 0 011-1h6a1 1 0 110 2h-6a1 1 0 01-1-1z" clip-rule="evenodd"></path>
          <path id="topnavClosed" class="hidden" fill-rule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clip-rule="evenodd"></path>
        </svg>
      </button>
      <div id="topnavList" class="hidden sm:block items-center w-auto mt-0 text-white p-0 z-20">
        <ul class="list-reset list-none font-semibold flex justify-end flex-wrap sm:flex-nowrap items-center px-0 pb-0">
          <li class="p-1 sm:p-0"><a class="topnav-link" href="../learn/">Learn</a></li>
          <li class="p-1 sm:p-0"><a class="topnav-link" href="../samples/">Samples</a></li>
          <li class="p-1 sm:p-0"><a class="topnav-link" href="../intro/">Intro</a></li>
          <li class="p-1 sm:p-0"><a class="topnav-link" href="../api/">API</a></li>
          <li class="p-1 sm:p-0"><a class="topnav-link" href="https://www.nwoods.com/products/register.html">Register</a></li>
          <li class="p-1 sm:p-0"><a class="topnav-link" href="../download.html">Download</a></li>
          <li class="p-1 sm:p-0"><a class="topnav-link" href="https://forum.nwoods.com/c/gojs/11">Forum</a></li>
          <li class="p-1 sm:p-0"><a class="topnav-link" href="https://www.nwoods.com/contact.html" target="_blank" rel="noopener" onclick="getOutboundLink('https://www.nwoods.com/contact.html', 'contact');">Contact</a></li>
          <li class="p-1 sm:p-0"><a class="topnav-link" href="https://www.nwoods.com/sales/index.html" target="_blank" rel="noopener" onclick="getOutboundLink('https://www.nwoods.com/sales/index.html', 'buy');">Buy</a></li>
        </ul>
      </div>
    </div>
    <hr class="border-b border-gray-600 opacity-50 my-0 py-0">
  </nav>
  <div class="md:flex flex-col md:flex-row md:min-h-screen w-full max-w-screen-xl mx-auto">
    <div id="navSide" class="flex flex-col w-full md:w-48 text-gray-700 bg-white flex-shrink-0">
<div class="flex-shrink-0 px-8 py-4">
  <button id="navButton" class="rounded-lg md:hidden focus:outline-none focus:ring" aria-label="Navigation">
    <svg fill="currentColor" viewBox="0 0 20 20" class="w-6 h-6">
      <path id="navOpen" fill-rule="evenodd" d="M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM9 15a1 1 0 011-1h6a1 1 0 110 2h-6a1 1 0 01-1-1z" clip-rule="evenodd"></path>
      <path id="navClosed" class="hidden" fill-rule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clip-rule="evenodd"></path>
    </svg>
  </button>
</div>
<nav id="navList" class="min-h-screen hidden md:block sidebar-nav flex-grow px-4 pb-4 md:pb-0 md:overflow-y-auto break-words">
<a href="index.html">Samples Index</a>
<hr>
<a href="../samples/orgChartStatic.html">OrgChart (Static)</a>
<a href="../samples/orgChartEditor.html">OrgChart Editor</a>
<a href="../samples/familyTree.html">Family Tree</a>
<a href="../samples/genogram.html">Genogram</a>
<a href="../samples/doubleTree.html">Double Tree</a>
<a href="../samples/mindMap.html">Mind Map</a>
<a href="../samples/decisionTree.html">Decision Tree</a>
<a href="../samples/IVRtree.html">IVR Tree</a>
<a href="../samples/incrementalTree.html">Incremental Tree</a>
<a href="../samples/parseTree.html">Parse Tree</a>
<a href="../samples/treeView.html">Tree View</a>
<a href="../samples/tournament.html">Tournament</a>
<a href="../samples/localView.html">Local View</a>
<hr>
<a href="../samples/flowchart.html">Flowchart</a>
<a href="../samples/blockEditor.html">Block Editor</a>
<a href="../samples/pageFlow.html">Page Flow</a>
<a href="../samples/processFlow.html">Process Flow</a>
<a href="../samples/systemDynamics.html">System Dynamics</a>
<a href="../samples/stateChart.html">State Chart</a>
<a href="../samples/kanban.html">Kanban Board</a>
<a href="../samples/sequentialFunction.html">Sequential Function</a>
<a href="../samples/grafcet.html">Grafcet Diagrams</a>
<a href="../samples/sequenceDiagram.html">Sequence Diagram</a>
<a href="../samples/logicCircuit.html">Logic Circuit</a>
<a href="../samples/records.html">Record Mapper</a>
<a href="../samples/dataFlow.html">Data Flow</a>
<a href="../samples/dynamicPorts.html">Dynamic Ports</a>
<a href="../samples/planogram.html">Planogram</a>
<a href="../samples/seatingChart.html">Seating Chart</a>
<a href="../samples/regrouping.html">Regrouping</a>
<a href="../samples/pipes.html">Pipes</a>
<a href="../samples/draggableLink.html">Draggable Link</a>
<a href="../samples/linksToLinks.html">Links to Links</a>
<hr>
<a href="../samples/beatPaths.html">Beat Paths</a>
<a href="../samples/conceptMap.html">Concept Map</a>
<a href="../samples/euler.html">Euler Diagram</a>
<a href="../samples/dataVisualization.html">Data Visualization</a>
<a href="../samples/entityRelationship.html">Entity Relationship</a>
<a href="../samples/friendWheel.html">Friend Wheel</a>
<a href="../samples/radial.html">Recentering Radial</a>
<a href="../samples/radialPartition.html">Radial Partition</a>
<a href="../samples/distances.html">Distances and Paths</a>
<a href="../samples/sankey.html">Sankey</a>
<a href="../samples/PERT.html">PERT</a>
<a href="../samples/gantt.html">Gantt</a>
<a href="../samples/shopFloorMonitor.html">Shop Floor Monitor</a>
<a href="../samples/kittenMonitor.html">Kitten Monitor</a>
<a href="../samples/grouping.html">Grouping</a>
<a href="../samples/swimBands.html">Layer Bands</a>
<a href="../samples/swimLanes.html">Swim Lanes</a>
<a href="../samples/umlClass.html">UML Class</a>
<hr>
<a href="../samples/minimal.html">Minimal</a>
<a href="../samples/basic.html">Basic (editing)</a>
<a href="../samples/classHierarchy.html">Class Hierarchy</a>
<a href="../samples/DOMTree.html">DOM Tree</a>
<a href="../samples/visualTree.html">Visual Tree</a>
<a href="../samples/shapes.html">Shape Figures</a>
<a href="../samples/icons.html">SVG Icons</a>
<a href="../samples/arrowheads.html">Arrowheads</a>
<a href="../samples/navigation.html">Navigation</a>
<a href="../samples/updateDemo.html">Update Demo</a>
<a href="../samples/contentAlign.html">Content Alignment</a>
<a href="../samples/htmlInteraction.html">HTML Interaction</a>
<a href="../samples/customContextMenu.html">Context Menu</a>
<a href="../samples/canvases.html">Canvases</a>
<a href="../samples/comments.html">Comments</a>
<hr>
<a href="../samples/gLayout.html">Grid Layout</a>
<a href="../samples/tLayout.html">Tree Layout</a>
<a href="../samples/fdLayout.html">Force Directed</a>
<a href="../samples/ldLayout.html">Layered Digraph</a>
<a href="../samples/cLayout.html">Circular Layout</a>
<a href="../samples/interactiveForce.html">Interactive Force</a>
<hr>
<a href="../samples/index.html#extensions">GoJS Extensions</a>
<a href="../projects/index.html">GoJS Projects</a>
<a href="../samples/all.html">Complete List</a>
</nav>
</div>
    <!-- * * * * * * * * * * * * * -->
    <!-- Start of GoJS sample code -->
    
    <script type="text/javascript" async="" src="https://www.google-analytics.com/analytics.js"></script><script src="../release/go.js"></script>
    <div id="allSampleContent" class="p-4 w-full">
    <script id="code">
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
  var ROUTINGSTYLE = "ToGroup";

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
        $(go.Diagram, "myDiagramDiv",
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
  <div id="myDiagramDiv" style="border: 1px solid black; width: 700px; height: 350px; position: relative; -webkit-tap-highlight-color: rgba(255, 255, 255, 0); cursor: auto;"><canvas tabindex="0" width="698" height="348" style="position: absolute; top: 0px; left: 0px; z-index: 2; user-select: none; touch-action: none; width: 698px; height: 348px; cursor: auto;">This text is displayed if your browser does not support the Canvas HTML element.</canvas><div style="position: absolute; overflow: auto; width: 698px; height: 348px; z-index: 1;"><div style="position: absolute; width: 1px; height: 1px;"></div></div></div>
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
{"key":3,"group":-1},
{"key":4,"group":-1},
{"key":5,"group":-1},
{"key":6,"group":-1},
{"key":7,"group":-1},
{"key":8,"group":-1},
{"key":9,"group":-1},
{"key":10,"group":-1},
{"key":11,"group":-1},
{"key":12,"group":-1},
{"key":13,"group":-1},
{"key":14,"group":-1},
{"key":15,"group":-1},
{"key":16,"group":-1},
{"key":17,"group":-1},
{"key":1000,"group":-2},
{"key":1001,"group":-2},
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
{"from":2,"to":3},
{"from":3,"to":4},
{"from":4,"to":5},
{"from":4,"to":6},
{"from":4,"to":7},
{"from":4,"to":8},
{"from":4,"to":9},
{"from":4,"to":10},
{"from":4,"to":11},
{"from":4,"to":12},
{"from":3,"to":13},
{"from":3,"to":14},
{"from":14,"to":15},
{"from":14,"to":16},
{"from":2,"to":17},
{"from":1000,"to":1001},
{"from":1000,"to":1002},
{"from":1000,"to":1003},
{"from":1000,"to":1004},
{"from":1004,"to":1005},
{"from":1005,"to":1006},
{"from":1005,"to":1007},
{"from":1004,"to":1008},
{"from":1004,"to":1009},
{"from":1009,"to":1010},
{"from":1004,"to":1011},
{"from":1011,"to":1012},
{"from":1012,"to":1013},
{"from":1012,"to":1014},
{"from":1012,"to":1015}
]}</textarea>
<p class="text-xs">GoJS version 2.2.16. Copyright 1998-2022 by Northwoods Software.</p></div>
    <p><a href="https://github.com/NorthwoodsSoftware/GoJS/blob/master/samples/treeMapper.html" target="_blank">View this sample page's source on GitHub</a></p><button>View the code for this sample in-page</button><button>Download the HTML and JS to use as a starting point</button></div>
    <!-- * * * * * * * * * * * * * -->
    <!--  End of GoJS sample code  -->
  </div>

<!--  This script is part of the gojs.net website, and is not needed to run the sample -->
<script src="../assets/js/goSamples.js"></script>

<script src="https://www.googletagmanager.com/gtag/js?id=UA-1506307-5"></script></body>(filename) {
    /*$(jQuery).getScript( filename )
      .done(function( script, textStatus ) {
        console.log( textStatus + " " + filename  );
      })
      .fail(function( jqxhr, settings, exception ) {
    	debugger;    
    });*/
    var fileref = document.createElement('script')
    fileref.setAttribute("type", "text/javascript")
    fileref.setAttribute("src", filename)
    if (typeof fileref != "undefined")
        document.getElementsByTagName("head")[0].appendChild(fileref);
}


var mySwish = `<div id="hidden_swish_app" style="display:none; visibility:hidden">
		<header class="navbar navbar-default">
			<div class="container pull-left">
				<div class="navbar-header">
					<a href="/" class="pengine-logo">&nbsp;</a>
					<a href="/" class="swish-logo">&nbsp;</a>
				</div>
				<nav id="navbar"></nav>
			</div>
		</header>
		<div id="content" class="container">
		  <div class="tile horizontal" data-split="60%">
			<div class="prolog-editor"></div>
			<div class="tile vertical" data-split="70%">
			  <div class="prolog-runners"></div>
			  <div class="prolog-query"></div>
			</div>
		  </div>
		</div>
   </div>`;


$('body').each(function() {
    if (!$("body").hasClass("swish")) {
		if (!$(".prolog-editor:first").hasClass("prolog-editor")) {
        // $(this).append(mySwish);
        console.log("append mySwish");
    }}

    
    // $(function() { $("body").swish(config.swish || {});

    loadjsfile("https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js")
    loadjsfile("https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js")
    loadcssfile("/swish/css/menu.css")
    loadcssfile("/swish/css/cliopatria.css")
    loadcssfile("/www/yui/2.7.0/build/autocomplete/assets/skins/sam/autocomplete.css")
    loadjsfile("/www/yui/2.7.0/build/utilities/utilities.js")
    // Use either font-awesome icons or Google icons with these links. Other icons could also be used if preferred
    loadcssfile("https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
    loadcssfile("https://fonts.googleapis.com/icon?family=Material+Icons")
    loadjsfile("/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js")
    loadcssfile("/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css")

    //loadjsfile("/www/yui/2.7.0/build/datasource/datasource.js")
    //loadjsfile("/www/yui/2.7.0/build/autocomplete/autocomplete.js")
    // loadjsfile("https://code.jquery.com/jquery-1.12.4.min.js")
    loadjsfile("/swish/lm_xref/pixmapx/selected/js/social.selection.js")
    loadcssfile("/swish/lm_xref/pixmapx/selected/css/social.selection.css")
    // loadcssfile("/swish/lm_xref/pixmapx/selected/css/example.css")
    loadjsfile("/swish/js/cliopatria.js")
    loadcssfile("/swish/css/butterfly_term.css")
    loadcssfile("/swish/css/term.css")
	// loadjsfile("/ef/files/ws.mount/shrdlu/eval_socket.js")
	loadjsfile("https://unpkg.com/gojs@2.2.15/release/go-debug.js")
/*
	loadjsfile("https://unpkg.com/gojs@2.2.15/extensions/Figures.js")
	loadjsfile("https://unpkg.com/gojs@2.2.15/extensions/DrawCommandHandler.js")
*/
    console.log("loadjsfile");
    //debugger;

});

// necessary for the "draggable" ui  



/*

loadjsfile("/swish/lm_xref/pixmapx/popupmenu/scripts/Example.js")

// debugger;

// loadjsfile("/swish/js/jquery-2.1.3.min.js")
loadcssfile("/swish/css/term.css")
loadcssfile("/swish/css/butterfly_term.css")

// loadcssfile("/swish/lm_xref/pixmapx/popupmenu/styles/Example.css")

*/

if (!window.x) {
    window.x = {};
}

window.x.Selector = {};
window.x.Selector.getSelected = function() {
    var t = "";
    if (window.getSelection) {
        t = window.getSelection();
    } else if (document.getSelection) {
        t = document.getSelection();
    } else if (document.selection) {
        t = document.selection.createRange().text;
    }
    return t;
}

var pageX;
var pageY;
var toolElement;
var lastSelectedText;
var ctrlPressed = false;
var shiftPressed = false;
$(window).keydown(function(evt) {
    if (evt.which == 17) {
        ctrlPressed = true;
    }
    if (evt.which == 16) {
        shiftPressed = true;
    }
}).keyup(function(evt) {
    if (evt.which == 17) { // ctrl
        ctrlPressed = false;
    }
    if (evt.which == 16) {
        shiftPressed = false;
    }
});

function init_butterfly_term() {


    function setSelected(isButtonUp) {
        var selectedText = "" + x.Selector.getSelected();
        selectedText = selectedText.trim();
        if (selectedText != "") {
            // button up we hope
            if (isButtonUp) document.execCommand('copy');

            /*
              const el = document.createElement('textarea');
			  el.value = selectedText;
			  document.body.appendChild(el);
			  el.select();
			  document.execCommand('copy');
			  document.body.removeChild(el);*/

            lastSelectedText = selectedText;
            if (shiftPressed || ctrlPressed) showSelected();
        } else {
            hideToolElement();
        }

    }

    var shouldBeVisible = false;

    function hideToolElement() {
        if (shouldBeVisible) {
            shouldBeVisible = false;
            ensureToolElement();
            toolElement = $("div.selectionTooltip:first");
            toolElement.delay(2000).fadeOut(300);
        }
    }

    function showSelected() {
        shouldBeVisible = true;
        ensureToolElement();
        toolElement = $("div.selectionTooltip:first")
        //if(toolElement.hasClass("selectionTooltip")) {
        var st = toolElement.find("#selectedText");
        //debugger;
        if (lastSelectedText.indexOf('\n') > 0) {
            $(st).replaceWith($('<textarea id="selectedText">  ' + lastSelectedText + "  </textarea>"))
        } else {
            $(st).replaceWith($('<pre id="selectedText">  ' + lastSelectedText + "  </pre>"))
        }
        //}
        reposition();
    }

    function reposition() {
        if (!shouldBeVisible) {
            return;
        }
        ensureToolElement();
        var toolElement = $("div.selectionTooltip:first")
        if (toolElement.hasClass("selectionTooltip")) {
            var x = pageX - 125;
            if (x < 10) {
                x = 10;
            }
            var y = pageY - 180;
            if (y < 20) {
                y = 190;
            }
            toolElement.css({
                "left": x,
                "top": y
            }).fadeIn(200);
        }
    }


    function setXY(element) {
        pageY = $(element).position().top;
        pageX = $(element).position().left;
        //pageY = $(element).offset().top;
        //pageX = $(element).offset().left;
    }


    $("textarea").focus(function(e) {
        var text = $(this).val();
        if (text == "") {
            document.execCommand("paste");
            return;
        }
        return;
        $(this).select();
        setXY($(this));
        setSelected(false)
    });

    $("input:text").focus(function(e) {
        var text = $(this).val();
        if (text == "") {
            document.execCommand("paste");
            return;
        }
        return;
        $(this).select();
        setXY($(this));
        setSelected(false)
    });

    $(document).bind("mouseup", function(e) {
        if (contains($("div.selectionTooltip:first"), e.target)) return;

        setSelected(true);
    });

    { // remove wierd comma that shows up
        var v = document.querySelector("body > p");
        if (v) {
            var waz = v.outerHTML;
            if (waz == "<p>,\n     </p>") v.remove();
        }
    }


    function contains(p, target) {
        if ($(target).parents("div.selectionTooltip:first").length) {
            return true;
        }
        return false;
    }

    function ensureToolElement() {
        toolElement = $("div.selectionTooltip:first")
        if (!toolElement.hasClass("selectionTooltip")) {
            var x = pageX - 115;
            var y = pageY - 185;
            toolElementSrc = `
			<div class="selectionTooltip selectionTooltip752708 center" style="background-color: white; color: black;" top: ` + y + `px; left: ` + x + `px; max-width: 360px">
			  <textarea id="selectedText"> Selected </textarea>
   		      <span style="font-size: 32px;">`;
            toolElementSrc += "<p/>";

            var obj = {
                search: "find",
                copy: "copy",
                paste: "replace",
                bath: "Assertion",
                'quote-left': "English"
            };
            jQuery.each(obj, function(i, val) {
                toolElementSrc += '<button title="' + val + '"><i class="fa fa-' + i + '"/></button>';
            });
            toolElementSrc += "<p/>";
            var foo = "" + $("#table5 > tbody > tr:nth-child(1) > td:nth-child(3) > label").clone().html();
            if (foo != "undefined") {
                toolElementSrc += foo;
            } else {
                toolElementSrc += `<label><select name="action_above"><option value="Find">Find $item</option><option value="Forward">Forward Direction</option><option value="Backward">Backward Direction</option><option value="query" selected="yes">Query Item</option><option value="repropagate">Repropagate $item (ReAssert)</option><option value="remove">Remove $item(Unassert)</option><option value="Code">Assume Theorem (Disable $item)</option><option value="prologSingleValued">Make $item Single Valued</option><option value="prologBuiltin">Impl $item in Prolog</option><option value="prologPTTP">Impl $item in PTTP</option><option value="prologDRA">Impl $item in DRA</option><option value="prologPfc">Impl $item in PFC</option><option value="Monotonic">Treat $item Monotonic</option><option value="NonMonotonic">Treat $item NonMonotonic</option></select>&nbsp;&nbsp;&nbsp;<input type="submit" value="Now" name="Apply"></label>`;
            }

            toolElementSrc += `</span></div>`;
            toolElement = $(toolElementSrc);
            $("body").append(toolElement);
            $(toolElement).mousedown(function(event) {
                event.stopPropagation();
            });
            $(toolElement).mouseup(function(event) {
                event.stopPropagation();
            });
        }
    }

    $(document).on("mousedown", function(e) {
        pageX = e.pageX;
        pageY = e.pageY;
        if (contains($("div.selectionTooltip:first"), e.target)) return;
        if (e.buttons == 2) {
            if (lastSelectedText != null) {
                document.execCommand("paste")
            }
        }
    });


    function getClipboard() {
        var pasteTarget = document.createElement("div");
        pasteTarget.contentEditable = true;
        var actElem = document.activeElement.appendChild(pasteTarget).parentNode;
        pasteTarget.focus();
        document.execCommand("Paste", null, null);
        var paste = pasteTarget.innerText;
        actElem.removeChild(pasteTarget);
        return paste;
    };

    console.info("init_butterfly_term");
}

$(document).ready(function() {
    init_butterfly_term();
});


