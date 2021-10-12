using System;
using Prolog;
using UnityEngine;

public class Room : BindingBehaviour
{
    public int Left;

    public int Bottom;

    public int Width;

    public int Height;

    public override void Awake()
    {
        this.tileRect = new TileRect(Left, Bottom, Width, Height);
    }

    public void Start()
    {
        if (!KB.Global.IsTrue("register_room", gameObject, Symbol.Intern(name)))
            throw new Exception("Can't register prop " + name);
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
        return this.tileRect.Contains(o.Position());
    }

    internal void OnDrawGizmos()
    {
        Gizmos.color = Color.yellow;
        GizmoUtils.Draw(new Rect(Tile.MapXMin+Left*Tile.SizeInSceneUnits,
            Tile.MapYMin+Bottom*Tile.SizeInSceneUnits,
            Width*Tile.SizeInSceneUnits,
            Height*Tile.SizeInSceneUnits));
    }
}
