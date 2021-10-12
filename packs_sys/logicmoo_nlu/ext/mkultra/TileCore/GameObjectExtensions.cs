using UnityEngine;

public static class GameObjectExtensions
{
    /// <summary>
    /// The object's position as a Vector2
    /// </summary>
    /// <param name="o">GameObject</param>
    /// <returns>Its position</returns>
    public static Vector2 Position(this GameObject o)
    {
        return o.transform.position;
    }

    /// <summary>
    /// The tile occupied by the "center" of the object
    /// </summary>
    /// <param name="o">GameObject to get the position of</param>
    /// <returns>The position as a TilePosition</returns>
    public static TilePosition TilePosition(this GameObject o)
    {
        return o.Position();
    }

    /// <summary>
    /// The object's footprint, i.e. the boudning box of its collider.
    /// </summary>
    /// <param name="o">The GameObject to get the footprint of</param>
    /// <returns>The TileRect for the footprint</returns>
    public static Rect Footprint(this GameObject o)
    {
        return o.GetComponent<Collider2D>().BoundingBox();
    }

    /// <summary>
    /// The tiles covered by the object's footprint
    /// </summary>
    /// <param name="o">The GameObject to get the footprint of</param>
    /// <returns>The TileRect for the footprint</returns>
    public static TileRect FootprintTiles(this GameObject o)
    {
        return new TileRect(o.Footprint());
    }

    /// <summary>
    /// The TileRect corresponding to the docking region of this object.
    /// </summary>
    /// <param name="o">GameObject to get docking region for</param>
    /// <returns>The docking region</returns>
    public static TileRect DockingTiles(this GameObject o)
    {
        var room = o.GetComponent<Room>();
        if (room != null)
            return room.TileRect;

        var dr = o.GetComponent<DockingRegion>();
        return dr == null ? o.FootprintTiles().Grow(1) : dr.DockingTiles;
    }

    /// <summary>
    /// Returns the parent GameObject of this GameObject
    /// </summary>
    /// <param name="o">The GameObject to get the parent of</param>
    /// <returns>The parent GameObject</returns>
    public static GameObject GetParent(this GameObject o)
    {
        return o.transform.parent.gameObject;
    }
}
