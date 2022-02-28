using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// A spatial visualization displayed over the TileMap
/// </summary>
public abstract class MapOverlay
{
    /// <summary>
    /// Draws the overlay.  Don't call this; it's called automatically.
    /// </summary>
    /// <param name="map">Map to draw over.</param>
    public abstract void Draw(TileMap map);
}

/// <summary>
/// Displays a set of tiles in a fixed color
/// </summary>
public class TileSetOverlay : MapOverlay
{
    /// <summary>
    /// Displays a set of tiles in a fixed color
    /// </summary>
    /// <param name="c">Color to display.  Should usually be transparent, i.e. alpha shouldn't be 1.</param>
    public TileSetOverlay(Color c)
    {
        Color = c;
    }

    #region Fields
    public Color Color;
    readonly List<TilePosition> tiles = new List<TilePosition>();
    #endregion

    #region Tile selection
    /// <summary>
    /// Set this overlay to contain exactly one tile.
    /// </summary>
    /// <param name="p">The tile to include</param>
    public void SetSingleton(TilePosition p)
    {
        this.tiles.Clear();
        this.tiles.Add(p);
    }

    /// <summary>
    /// Set this overlay to include exactly the tiles in the specified rectangle.
    /// </summary>
    /// <param name="r"></param>
    public void SetRect(TileRect r)
    {
        this.tiles.Clear();
        foreach (var p in r)
            this.tiles.Add(p);
    }

    /// <summary>
    /// Remove all tiles form the overlay.
    /// </summary>
    public void Clear()
    {
        this.tiles.Clear();
    }

    /// <summary>
    /// Sets the tilemap to an arbitrary set of tiles.
    /// </summary>
    /// <param name="tilesToInclude"></param>
    public void Set(IEnumerable<TilePosition> tilesToInclude)
    {
        this.tiles.Clear();
        this.tiles.AddRange(tilesToInclude);
    }

    /// <summary>
    /// Adds a tile position to the set of tiles
    /// </summary>
    /// <param name="tilePosition">Location of tile</param>
    internal void Add(TilePosition tilePosition)
    {
        this.tiles.Add(tilePosition);
    }
    #endregion

    /// <summary>
    /// Draws the overlay.  Users should not call this.
    /// </summary>
    /// <param name="map">The map to draw over.</param>
    public override void Draw(TileMap map)
    {
        GL.Begin(GL.QUADS);
		GL.Color(Color);

        foreach (var tile in this.tiles)
        {
            var rect = tile.WorldRect;
            GL.Vertex3(rect.xMin, rect.yMin, 0);
            GL.Vertex3(rect.xMin, rect.yMax, 0);
            GL.Vertex3(rect.xMax, rect.yMax, 0);
            GL.Vertex3(rect.xMax, rect.yMin, 0);
            //GL.Vertex3(rect.xMax, rect.yMin, 0);
            //GL.Vertex3(rect.xMax, rect.yMax, 0);
            //GL.Vertex3(rect.xMin, rect.yMax, 0);
            //GL.Vertex3(rect.xMin, rect.yMin, 0);
        }
        GL.End();
    }
}