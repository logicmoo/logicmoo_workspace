using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

/// <summary>
/// A map (background) built out of tiles in a TileSet (a kind of sprite sheet).
/// </summary>
[AddComponentMenu("Tile/TileMap")]
public class TileMap : BindingBehaviour
{
    public static TileMap TheTileMap;

    public string WallTileNamePrefix = "Wall ";
    #region Map data
    private Tile[,] contents;

    private Room[,] tileRoom;

    private SpriteRenderer[,] renderers;

    public int MapRows { get; private set; }

    public int MapColumns { get; private set; }

    public Tile this[int column, int row]
    {
        get
        {
            return contents[column, row] ?? (contents[column, row] = new Tile());
        }
    }

    public Tile this[TilePosition p]
    {
        get
        {
            return this[p.Column, p.Row];
        }
    }

    IEnumerable<TilePosition> TilePositions()
    {
        for (int i=0; i<MapColumns; i++)
            for (int j = 0; j < MapRows; j++)
            {
                if (contents[i,j] != null)
                    yield return new TilePosition(i, j);
            }
    }
    #endregion

    #region Initialization
    public override void Awake()
    {
        base.Awake();
        this.EnsureMapBuilt();
    }

    private bool mapBuilt;
    public void EnsureMapBuilt()
    {
        if (mapBuilt)
            return;
        this.RebuildMap();
    }

    public void RebuildMap()
    {
        TheTileMap = this;
        var allTileSprites = this.transform.Find("Tiles").GetComponentsInChildren<SpriteRenderer>();
        this.GetMapDimensions(allTileSprites);
        this.PopulateMap(allTileSprites);

        this.MarkObstacles();
        this.mapBuilt = true;
    }

    public static void UpdateMapVariables()
    {
        FindObjectOfType<TileMap>().EnsureMapBuilt();
    }

    public void UpdateCamera(Camera c)
    {
        var delta = new Vector3(Tile.MapXMin, Tile.MapYMin, 0) - c.ScreenToWorldPoint(Vector3.zero);
        delta.z = 0;
        c.transform.position += delta;
    }

    /// <summary>
    /// Mark all tiles of all sprites for which IsStaticObstacle() is true.
    /// </summary>
    private void MarkObstacles()
    {
        foreach (var sprite in FindObjectsOfType<SpriteRenderer>())
        {
            if (IsStaticObstacle(sprite))
            {
                foreach (var tile in sprite.gameObject.FootprintTiles())
                {
                    this[tile].Type = TileType.Obstacle;
                }
            }
        }
    }

    public static void UpdateSortingOrder(SpriteRenderer sprite)
    {
        var minY = sprite.bounds.min.y;
        sprite.sortingOrder = (int)(-100*minY);
    }

    /// <summary>
    /// Test if this is an unmovable obstacle like furniture or an appliance.
    /// True if this is not a map tile, it has a box collider, and it doesn't have a RigidBody2D.
    /// </summary>
    private bool IsStaticObstacle(SpriteRenderer sprite)
    {
        if (sprite.sortingLayerName == "Map")
            return false;
        if (sprite.GetComponent<Door>() != null)
            return false;
        if (sprite.GetComponent<BoxCollider2D>() == null)
            return false;
        return sprite.GetComponent<Rigidbody2D>() == null;
    }

    internal void Start()
    {
        StartCoroutine(this.MakeWalls());
    }

    // ReSharper disable ParameterTypeCanBeEnumerable.Local
    private void PopulateMap(SpriteRenderer[] tileSprites)
        // ReSharper restore ParameterTypeCanBeEnumerable.Local
    {
        foreach (var spriteRenderer in tileSprites)
        {
            TilePosition p = spriteRenderer.bounds.center;
            var tile = this[p];
            var spriteName = spriteRenderer.sprite.name;
            tile.SpriteName = spriteName;
            tile.Type = spriteName.StartsWith(WallTileNamePrefix) ? TileType.Wall : TileType.Freespace;
            renderers[p.Column, p.Row] = spriteRenderer;
        }

        // Mark what tiles are in what rooms
        tileRoom = new Room[MapColumns, MapRows];
        foreach (var r in Registry<Room>())
        {
            r.Initialize();
            foreach (var tp in r.TileRect)
                tileRoom[tp.Column, tp.Row] = r;
            foreach (var p in r.Portals)
                foreach (var tp in p.TileRect)
                    tileRoom[tp.Column, tp.Row] = r;
        }
    }

    public void SetTileColor(TilePosition p, Color c)
    {
        var tileRenderer = renderers[p.Column, p.Row];
        if (tileRenderer != null)
            tileRenderer.color = c;
    }

    public void SetTileSprite(TilePosition p, Sprite s)
    {
        var tileRenderer = renderers[p.Column, p.Row];
        if (tileRenderer != null)
        {
            tileRenderer.sprite = s;
        }
    }

    public void SetTileColor(TileRect r, Color c)
    {
        foreach (var tile in r)
            SetTileColor(tile, c);
    }

    private void GetMapDimensions(SpriteRenderer[] tileSprites)
    {
        float tileSize = 2*tileSprites[0].bounds.extents.x;
        float minX = float.PositiveInfinity;
        float minY = float.PositiveInfinity;
        float maxX = float.NegativeInfinity;
        float maxY = float.NegativeInfinity;

        foreach (var spriteRenderer in tileSprites)
        {
            var bounds = spriteRenderer.bounds;
            if (Mathf.Abs((bounds.max.x-bounds.min.x)-tileSize) > 0.01
                || Mathf.Abs((bounds.max.y - bounds.min.y) - tileSize) > 0.01)
                throw new Exception("Map appears to have tiles of differing sizes");
            minX = Mathf.Min(minX, bounds.min.x);
            minY = Mathf.Min(minY, bounds.min.y);
            maxX = Mathf.Max(maxX, bounds.max.x);
            maxY = Mathf.Max(maxY, bounds.max.y);
        }

        Tile.SizeInSceneUnits = tileSize;
        Tile.MapXMin = minX;
        Tile.MapXMax = maxX;
        Tile.MapYMin = minY;
        Tile.MapYMax = maxY;
        MapColumns = Mathf.RoundToInt((maxY - minY) / tileSize);
        MapRows = Mathf.RoundToInt((maxX - minX) / tileSize);
        contents = new Tile[MapColumns, MapRows];
        renderers = new SpriteRenderer[MapColumns, MapRows];
    }
    #endregion

    #region Contents tracking
    public Tile[,] GetRegionTiles(TileRect r)
    {
        var tiles = new Tile[r.Width, r.Height];
        foreach (var p in r)
            tiles[p.Column - r.CMin, p.Row - r.RMin] = contents[p.Column, p.Row];
        return tiles;
    }

    public bool IsFreespace(TilePosition p)
    {
        if (p.Column < 0 || p.Row < 0 || p.Column >= MapColumns || p.Row >= MapRows)
            return false;
        return contents[p.Column, p.Row].Type == TileType.Freespace;
    }

    public bool IsFreespace(TileRect r)
    {
        foreach (var p in r)
            if (!IsFreespace(p))
                return false;
        return true;
    }

    /// <summary>
    /// Returns the room a given tile position appears in, or null if it doesn't correspond to any room.
    /// </summary>
    /// <param name="tp">Tile position to check</param>
    /// <returns>Room object or null</returns>
    public Room TileRoom(TilePosition tp)
    {
        if (tp.Column < 0 || tp.Row < 0 || tp.Column >= TheTileMap.MapColumns || tp.Row >= TheTileMap.MapRows)
            return null;

        return tileRoom[tp.Column, tp.Row];
    }

    /// <summary>
    /// Returns the room a given GameObject appears in, or null if it doesn't correspond to any room.
    /// </summary>
    /// <param name="o">GameObject to check</param>
    /// <returns>Room object or null</returns>
    public Room TileRoom(GameObject o)
    {
        return TileRoom(o.Position());
    }
    #endregion

    #region Map overlays (for debugging visualization)

    private Material overlayMaterial;
    public void OnRenderObject()
    {
        if (this.overlayMaterial == null)
            this.overlayMaterial = new Material(Shader.Find("GUI/Text Shader"));

        this.overlayMaterial.SetPass(0);
        GL.PushMatrix();
        GL.MultMatrix(transform.localToWorldMatrix);

        foreach (var overlay in overlays)
            overlay.Draw(this);

        GL.PopMatrix();
    }

    /// <summary>
    /// Overlays to display on top of the map.
    /// </summary>
    private readonly HashSet<MapOverlay> overlays = new HashSet<MapOverlay>();

    /// <summary>
    /// Adds an overlay to display on top of the map.
    /// Has no effect if the overlay is already being displayed.
    /// </summary>
    /// <param name="overlay">Overlay to display.</param>
    public void AddOverlay(MapOverlay overlay)
    {
        if (!overlays.Contains(overlay))
            overlays.Add(overlay);
    }

    /// <summary>
    /// Disables display of the overlay.
    /// </summary>
    /// <param name="overlay">Overlay to disable.</param>
    public void RemoveOverlay(MapOverlay overlay)
    {
        overlays.Remove(overlay);
    }

    /// <summary>
    /// Cancels display of all overlays
    /// </summary>
    public void RemoveAllOverlays()
    {
        overlays.Clear();
    }
    #endregion

    #region Collider generation
    HashSet<TilePosition> WallTiles
    {
        get
        {
            var walls = new HashSet<TilePosition>();
            foreach (var p in this.TilePositions())
            {
                if (this[p].Type == TileType.Wall)
                    walls.Add(p);
            }
            return walls;
        }
    }

    IEnumerator MakeWalls()
    {
        var walls = GameObject.Find("Map/Walls");
        var remainingWallTiles = WallTiles;

#if DEBUG_MAKEWALLS
        var allWallsOverlay = new TileSetOverlay(Color.green);
        var currentWallOverlay = new TileSetOverlay(Color.yellow);
        this.AddOverlay(allWallsOverlay);
        this.AddOverlay(currentWallOverlay);
        allWallsOverlay.Set(remainingWallTiles);
        yield return new WaitForSeconds(1);
#endif

        while (remainingWallTiles.Count > 0)
        {
            var seed = remainingWallTiles.First();
            var vwall = this.ScanVertically(seed, remainingWallTiles);
            var hwall = this.ScanHorizontally(seed, remainingWallTiles);
            var selectedWall = vwall.Area > hwall.Area ? vwall : hwall;
            foreach (var p in selectedWall)
                remainingWallTiles.Remove(p);

            var bcollider = walls.AddComponent<BoxCollider2D>();
            bcollider.offset = selectedWall.WorldCenter;
            bcollider.size = selectedWall.Size;

#if DEBUG_MAKEWALLS
            allWallsOverlay.Clear();
            allWallsOverlay.Set(remainingWallTiles);
            currentWallOverlay.Clear();
            currentWallOverlay.Set(selectedWall);
            yield return new WaitForSeconds(2.0f);
#endif
        }
        yield break;
    }

    TileRect ScanVertically(TilePosition seed, HashSet<TilePosition> wallTiles)
    {
        var col = seed.Column;
        var top = seed.Row;
        while (++top < MapRows && wallTiles.Contains(new TilePosition(col, top))) { }
        top--;

        var bottom = seed.Row;
        while (--bottom >= 0 && wallTiles.Contains(new TilePosition(col, bottom))) { }
        bottom++;

        return new TileRect(new TilePosition(col, top), new TilePosition(col, bottom));
    }

    TileRect ScanHorizontally(TilePosition seed, HashSet<TilePosition> wallTiles)
    {
        var row = seed.Row;
        var right = seed.Column;
        while (++right < MapColumns && wallTiles.Contains(new TilePosition(right, row))) { }
        right--;

        var left = seed.Column;
        while (--left >= 0 && wallTiles.Contains(new TilePosition(left, row))) { }
        left++;

        return new TileRect(new TilePosition(left, row), new TilePosition(right, row));
    }
    #endregion
}
