/// <summary>
/// General classification of tiles in  the TileMap and TileSets.
/// Mostly used to keep track of what things count as freespace.
/// </summary>
public enum TileType
{
    /// <summary>
    /// The tile is freespace; you can walk through it.
    /// </summary>
    Freespace,
    /// <summary>
    /// The tile is a generic obstacle.
    /// </summary>
    Obstacle,
    /// <summary>
    /// The tile is furniture you can sit on.
    /// </summary>
    Sittable,
    /// <summary>
    /// The tile is storage, like shelving or a table
    /// </summary>
    Storage,
    /// <summary>
    /// The tile is an appliance.
    /// </summary>
    Appliance,
    /// <summary>
    /// The tile is a wall.
    /// </summary>
    Wall,
    /// <summary>
    /// The tile is a character.
    /// </summary>
    Character
}
