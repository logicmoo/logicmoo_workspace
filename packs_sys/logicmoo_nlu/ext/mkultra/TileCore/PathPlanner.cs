using System.Collections.Generic;
using UnityEngine;

public class PathPlanner : BindingBehaviour
{
    /// <summary>
    /// The map of the "world"
    /// We use it to find out what tiles are freespace and aren't.
    /// Also to draw overlays to visualize the tiles searched and the current path.
    /// </summary>
    [Bind(BindingScope.Global)]  // Tells the system to automatically find the map object and store it in the Map field.
    public TileMap Map;

    /// <summary>
    /// A priority queue that holds all the tiles we're searching.
    /// Also can tell us what search node corresponds to a given tile.
    /// </summary>
    private TileHeap heap;

    /// <summary>
    /// Visualization of the tiles that were searched during planning.
    /// Used for debugging.
    /// </summary>
    private readonly TileSetOverlay searchedTilesOverlay = new TileSetOverlay(new Color(1,1,0,0.3f));
    /// <summary>
    /// Visualization of the computed path.
    /// Used for debugging.
    /// </summary>
    private readonly TileSetOverlay computedPathOverlay = new TileSetOverlay(new Color(0,1,0, 0.4f));

    public bool DisplayOverlays;

    /// <summary>
    /// Called by Unity when the PathPlanner object is created.
    /// </summary>
    public void Start()
    {
        heap = new TileHeap(Map);
        this.UpdateOverlayVisibility();
    }

    private void UpdateOverlayVisibility()
    {
        if (DisplayOverlays)
        {
            this.Map.AddOverlay(this.searchedTilesOverlay);
            this.Map.AddOverlay(this.computedPathOverlay);
        }
        else
        {
            this.Map.RemoveOverlay(this.searchedTilesOverlay);
            this.Map.RemoveOverlay(this.computedPathOverlay);
        }
    }

    /// <summary>
    /// Find the shortest path between the specified tiles.
    /// </summary>
    /// <param name="start">Start tile</param>
    /// <param name="end">End tile</param>
    /// <returns>Computed path</returns>
    public TilePath Plan(TilePosition start, TilePosition end)
    {
        return Plan(start, new TileRect(end));
    }

    /// <summary>
    /// Find the shortest path using the A* algorithm and a Euclidean distance heuristic
    /// </summary>
    public TilePath Plan(TilePosition start, TileRect end)
    {
        heap.Clear();
        searchedTilesOverlay.Clear();
        var startNode = heap.NodeAt(start);
        startNode.Predecessor = null;
        heap.DecreaseKey(startNode, 0, 0);
        var currentNode = startNode;
        while (!heap.IsEmpty && !end.Contains((currentNode = heap.ExtractMin()).Position))
        {
            searchedTilesOverlay.Add(currentNode.Position);
            foreach (var n in currentNode.Neighbors)
            {
                float newDistanceFromStart = currentNode.DistanceFromStart                           // Cost so far
                                 + TilePosition.EuclideanDistance(currentNode.Position, n.Position)  // Edge cost
                                 + TilePosition.EuclideanDistance(n.Position, end);                  // Estimated cost to end

                if (n.DistanceFromStart > newDistanceFromStart)
                {
                    n.Predecessor = currentNode;
                    heap.DecreaseKey(n, newDistanceFromStart, newDistanceFromStart);
                }
            }
        }
        heap.SetOverlayToComputedPath(this.computedPathOverlay, currentNode);

        searchedTilesOverlay.Clear();
        searchedTilesOverlay.SetRect(end);

        if (!end.Contains(currentNode.Position))
            return null;

        return this.MakePath(currentNode);
    }

    ///// <summary>
    ///// Find the shortest path using Dijkstra's algorithm
    ///// </summary>
    //public TilePath Dijkstra(TilePosition start, TileRect end)
    //{
    //    heap.Clear();
    //    searchedTilesOverlay.Clear();
    //    var startNode = heap.NodeAt(start);
    //    startNode.Predecessor = null;
    //    heap.DecreaseKey(startNode, 0, 0);
    //    var currentNode = startNode;
    //    while (!heap.IsEmpty && !end.Contains((currentNode = heap.ExtractMin()).Position))
    //    {
    //        searchedTilesOverlay.Add(currentNode.Position);
    //        foreach (var n in currentNode.Neighbors)
    //        {
    //            float newDistanceFromStart = currentNode.DistanceFromStart
    //                             + TilePosition.EuclideanDistance(currentNode.Position, n.Position);
    //            if (n.DistanceFromStart > newDistanceFromStart)
    //            {
    //                n.Predecessor = currentNode;
    //                heap.DecreaseKey(n, newDistanceFromStart, newDistanceFromStart);
    //            }
    //        }
    //    }
    //    heap.SetOverlayToComputedPath(this.computedPathOverlay, currentNode);

    //    if (!end.Contains(currentNode.Position))
    //        return null;

    //    return this.MakePath(currentNode);
    //}

    /// <summary>
    /// Make a path object from the nodes from the search.
    /// Starts at the end node and tracks backward, following predecessor links, until it finds the start node.
    /// </summary>
    /// <param name="endNode">The ending node of the path</param>
    /// <returns>The reconstructed path</returns>
    private TilePath MakePath(TileHeap.Node endNode)
    {
        // Reconstruct the path to from the start to the end.
        var path = new TilePath(endNode.Position);
        while (endNode != null)
        {
            path.AddBefore(endNode.Position.TileCenter);
            endNode = endNode.Predecessor;
        }
        return path;
    }

    /// <summary>
    /// This is a min heap on nodes, with the node's Distance field used as priority.
    /// </summary>
    class TileHeap
    {
        public TileHeap(TileMap map)
        {
            this.map = map;
            nodes = new Node[map.MapColumns, map.MapRows];
        }

        private readonly List<Node> data = new List<Node>();
        private readonly TileMap map;

        /// <summary>
        /// The node in the heap with the smallest distance.
        /// </summary>
        //public Node Min
        //{
        //    get
        //    {
        //        return data[0];
        //    }
        //}

        #region Adding and removing
        public Node ExtractMin()
        {
            Node min = data[0];
            var lastLeaf = data[data.Count - 1];
            data.RemoveAt(data.Count - 1);
            if (data.Count > 0)
            {
                data[0] = lastLeaf;
                Heapify(0);
            }
            return min;
        }

        public bool IsEmpty
        {
            get
            {
                return data.Count == 0;
            }
        }

        private void Add(Node node)
        {
            if (node.HeapIndex < 0)
            {
                node.HeapIndex = data.Count;
                data.Add(node);
            }
        }

        public void Clear()
        {
            foreach (var node in nodes)
            {
                if (node != null)
                {
                    node.HeapIndex = -1;
                    node.Predecessor = null;
                    node.DistanceFromStart = float.PositiveInfinity;
                    node.HeapKey = float.PositiveInfinity;
                }
            }
            data.Clear();
        }
        #endregion

        public void DecreaseKey(Node n, float newDistanceFromStart, float newKey)
        {
            if (n.HeapIndex<0)
                this.Add(n);
            n.DistanceFromStart = newDistanceFromStart;
            n.HeapKey = newKey;
            while (n.HeapIndex != 0 && n.HeapKey < this.Parent(n).HeapKey)
            {
                this.Swap(n.HeapIndex, ParentIndex(n.HeapIndex));
            }
        }

        private void Heapify(int nodeIndex)
        {
            while (true)
            {
                var largest = nodeIndex;
                var left = LeftChildIndex(nodeIndex);
                var right = RightChildIndex(nodeIndex);
                var count = data.Count;

                if (left < count && data[left].HeapKey < data[largest].HeapKey)
                    largest = left;
                if (right < count && data[right].HeapKey < data[largest].HeapKey)
                    largest = right;
                if (largest != nodeIndex)
                {
                    // Swap data[largest] and data[nodeIndex]
                    this.Swap(nodeIndex, largest);

                    // Repeat, but the node we're bubbling down has moved to a new position.
                    nodeIndex = largest;
                }
                else
                    return;
            }
        }

        private void Swap(int nodeIndex1, int nodeIndex2)
        {
            var temp = this.data[nodeIndex2];
            this.data[nodeIndex2] = this.data[nodeIndex1];
            this.data[nodeIndex1] = temp;
            this.data[nodeIndex2].HeapIndex = nodeIndex2;
            this.data[nodeIndex1].HeapIndex = nodeIndex1;
        }

        #region Indexing
        static int ParentIndex(int nodeIndex)
        {
            return (nodeIndex - 1) / 2;
        }

        static int LeftChildIndex(int nodeIndex)
        {
            return nodeIndex * 2 + 1;
        }

        static int RightChildIndex(int nodeIndex)
        {
            return nodeIndex * 2 + 2;
        }

        Node Parent(Node n)
        {
            return data[ParentIndex(n.HeapIndex)];
        }
        #endregion

        #region Tracking node positions
        private readonly Node[,] nodes;

        public Node NodeAt(TilePosition position)
        {
            var node = this.nodes[position.Column, position.Row] ?? this.MakeNode(position);
            return node;
        }

        private Node MakeNode(TilePosition position)
        {
            var node = this.nodes[position.Column, position.Row] = new Node(position, this);
            return node;
        }
        #endregion

        private IEnumerable<TilePosition> Neighbors(TilePosition p)
        {
            var freeUp = map.IsFreespace(p.Up);
            var freeDown = map.IsFreespace(p.Down);
            var freeLeft = map.IsFreespace(p.Left);
            var freeRight = map.IsFreespace(p.Right);

            if (freeUp) yield return p.Up;
            if (freeDown) yield return p.Down;
            if (freeLeft) yield return p.Left;
            if (freeRight) yield return p.Right;

            var corner = p.Up.Left;
            if (freeUp && freeLeft && map.IsFreespace(corner))
                yield return corner;
            corner = p.Up.Right;
            if (freeUp && freeRight && map.IsFreespace(corner))
                yield return corner;
            corner = p.Down.Left;
            if (freeDown && freeLeft && map.IsFreespace(corner))
                yield return corner;
            corner = p.Down.Right;
            if (freeDown && freeRight && map.IsFreespace(corner))
                yield return corner;
        } 

        public class Node
        {
            /// <summary>
            /// The map tile this corresponds to.
            /// </summary>
            public readonly TilePosition Position;
            /// <summary>
            /// Distance of this node from the start position.
            /// </summary>
            public float DistanceFromStart = float.PositiveInfinity;
            /// <summary>
            /// They key (priority) with which this node appears within the heap
            /// Lower key values are removed first.
            /// </summary>
            public float HeapKey = float.PositiveInfinity;
            /// <summary>
            /// Index within heap.data at which this node is stored.
            /// </summary>
            public int HeapIndex=-1;
            /// <summary>
            /// The node before this node on a path from the start to the end.
            /// </summary>
            public Node Predecessor;
            private readonly TileHeap heap;

            private Node[] _neighbors;

            private static readonly List<Node> NeighborsTemp = new List<Node>(4);

            // NOTE: Not threadsafe!
// ReSharper disable ReturnTypeCanBeEnumerable.Local
            public Node[] Neighbors
// ReSharper restore ReturnTypeCanBeEnumerable.Local
            {
                get
                {
                    if (this._neighbors == null)
                    {
                        NeighborsTemp.Clear();
                        foreach (var neighbor in heap.Neighbors(Position))
                            NeighborsTemp.Add(heap.NodeAt(neighbor));
                        this._neighbors = NeighborsTemp.ToArray();
                    }
                    return this._neighbors;
                }
            }

            protected internal Node(TilePosition position, TileHeap heap)
            {
                this.Position = position;
                this.heap = heap;
            }
        }

        /// <summary>
        /// Overwrites overlay with the tiles in the computed path.
        /// </summary>
        /// <param name="o">The overlay to reset</param>
        /// <param name="end">The endpoint of the path</param>
        public void SetOverlayToComputedPath(TileSetOverlay o, Node end)
        {
            Node node = end;

            o.Clear();
            while (node != null)
            {
                o.Add(node.Position);
                node = node.Predecessor;
            }
        }
    }
}

