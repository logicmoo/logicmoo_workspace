using UnityEngine;

/// <summary>
/// A world object (character or prop) represented as a sprite.
/// Contains information about footprint and docking position.
/// </summary>
public class DockingRegion: MonoBehaviour
{
    /// <summary>
    /// The docking area of this object relative to its Position
    /// </summary>
    public Rect DockingRect;

    /// <summary>
    /// The docking area of this object in screen coordinates.
    /// </summary>
    public Rect WorldDockingRect
    {
        get
        {
            return transform.TransformRect(DockingRect);
        }
    }

    /// <summary>
    /// The tiles corresponding to the docking area.
    /// </summary>
    public TileRect DockingTiles
    {
        get
        {
            return new TileRect(this.WorldDockingRect);
        }
    }

    internal void OnDrawGizmos()
    {
        Gizmos.color = Color.magenta;
        GizmoUtils.Draw(this.WorldDockingRect);
    }
}
