using System;
using Prolog;
using UnityEngine;

/// <summary>
/// A component defining a room in the TileMap
/// </summary>
[AddComponentMenu("Tile/Room")]
public class Room : BindingBehaviour
{
    public int Left;

    public int Bottom;

    public int Width;

    public int Height;

    public FloorMaterial Floor;

    public WallMaterial Wall;

    public Portal[] Portals;

    [Serializable]
    public class Portal
    {
        public int Left;

        public int Bottom;

        public int Width;

        public int Height;

        public TileRect TileRect { get; private set; }

        public bool Contains(GameObject o)
        {
            return this.TileRect.Contains(o.Position());
        }

        public bool Contains(TilePosition tp)
        {
            return this.TileRect.Contains(tp);
        }

        public void Initialize()
        {
            this.TileRect = new TileRect(Left, Bottom, Width, Height);
        }
    }

    public void Initialize()
    {
        this.tileRect = new TileRect(Left, Bottom, Width, Height);
        foreach (var portal in Portals)
            portal.Initialize();
    }

    public void Start()
    {
        if (!KB.Global.IsTrue("register_room", gameObject, Symbol.Intern(name)))
            throw new Exception("Can't register prop " + name);
        transform.position = new Vector3(Left, Bottom, 0);
    }

    /// <summary>
    /// Boundardies of the room
    /// </summary>
    private TileRect tileRect;

    public TileRect TileRect
    {
        get
        {
            return tileRect;
        }
    }

    /// <summary>
    /// True iff the room contains this object.
    /// </summary>
    public bool Contains(GameObject o)
    {
        return this.Contains(o.Position());
    }

    /// <summary>
    /// True iff the room contains this TilePosition.
    /// </summary>
    public bool Contains(TilePosition tp)
    {
        if (this.tileRect.Contains(tp))
            return true;
        return this.WithinPortal(tp);
    }

    public bool WithinPortal(TilePosition tp)
    {
        foreach (var portal in Portals)
            if (portal.Contains(tp))
                return true;
        return false;
    }

    public bool IsBackWallTile(TilePosition tp)
    {
        return this.WithinPortal(tp.Up);
    }

    internal void OnDrawGizmosSelected()
    {
        TileMap.UpdateMapVariables();
        Gizmos.color = Color.yellow;
        GizmoUtils.Draw(new Rect(Tile.MapXMin+Left*Tile.SizeInSceneUnits,
            Tile.MapYMin+Bottom*Tile.SizeInSceneUnits,
            Width*Tile.SizeInSceneUnits,
            Height*Tile.SizeInSceneUnits));

        foreach (var portal in Portals)
        {
            GizmoUtils.Draw(new Rect(Tile.MapXMin + portal.Left * Tile.SizeInSceneUnits,
               Tile.MapYMin + portal.Bottom * Tile.SizeInSceneUnits,
               portal.Width * Tile.SizeInSceneUnits,
               portal.Height * Tile.SizeInSceneUnits));   
        }
    }

    //internal void OnValidate()
    //{
    //    Debug.Log("Room changed: "+name);
    //}
}
