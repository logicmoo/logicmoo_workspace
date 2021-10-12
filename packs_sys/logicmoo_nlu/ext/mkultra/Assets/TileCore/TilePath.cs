using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TilePath
{
    public TilePath(TilePosition finalTile)
    {
        this.FinalTile = finalTile;
    }

    /// <summary>
    /// The distance (in pixels) the character can be from
    /// the waypoint tile before it is considered to have
    /// arrivedat that tile.
    /// </summary>
    const float WaypointTolerance = 0.2f;

    /// <summary>
    /// The speed the character moves at.
    /// This should probably be a variable stored in the character, but
    /// you can make that generalization yourself.
    /// </summary>
    const float Speed = 50;

    /// <summary>
    /// Returns a coroutine that causes *character* to follow the path stored
    /// in this path object.
    /// </summary>
    /// <param name="steering">The steering controller for the character to move along the path</param>
    /// <returns>A coroutine you can run with StartCoroutine()</returns>
    public IEnumerator FollowPath(CharacterSteeringController steering)
    {
        while (this.WaypointsRemain(steering.Position))
        {
            steering.Seek(this.CurrentWaypoint, Speed);
            yield return null;
        }
        steering.Stop();
    }

    /// <summary>
    /// Updates the character's steering to steer along this path.
    /// </summary>
    /// <param name="steering">The steering controller for the character to move along the path</param>
    /// <returns>Whether the character has arrived at the destination.</returns>
    public bool UpdateSteering(CharacterSteeringController steering)
    {
        if (this.WaypointsRemain(steering.Position))
        {
            steering.Seek(this.CurrentWaypoint, Speed);
            return false;
        }
        steering.Stop();
        return true;
    }

    /// <summary>
    /// True if there are further waypoints, after removing any we've already arrived at it.
    /// </summary>
    /// <param name="position">Our current position</param>
    /// <returns>True if there are further waypoints after removing any we've already arrived at.</returns>
    private bool WaypointsRemain(Vector2 position)
    {
        while (!IsEmpty && ArrivedAtCurrentWaypoint(position))
            this.NextWaypoint();

        return !IsEmpty;
    }

    /// <summary>
    /// True if we're already near the current waypoint.
    /// </summary>
    /// <param name="position">Our current position</param>
    private bool ArrivedAtCurrentWaypoint(Vector2 position)
    {
        return Vector2.Distance(CurrentWaypoint, position) < WaypointTolerance;
    }

    /// <summary>
    /// Stores the actual tiles to pass through in the path.
    /// </summary>
    readonly Stack<Vector2> waypoints = new Stack<Vector2>();
    public readonly TilePosition FinalTile;

    /// <summary>
    /// Remove all waypoints from the path.
    /// </summary>
    public void Clear()
    {
        waypoints.Clear();
    }

    /// <summary>
    /// Prepend a new waypoint to the path.  Thus the path will first
    /// go through this waypoint, then those already added to the path.
    /// </summary>
    /// <param name="waypoint">The waypoint to add</param>
    public void AddBefore(Vector2 waypoint)
    {
        waypoints.Push(waypoint);
    }

    /// <summary>
    /// The waypoint the character should try to approach next.
    /// </summary>
    public Vector2 CurrentWaypoint
    {
        get
        {
            return waypoints.Peek();
        }
    }

    /// <summary>
    /// Remove CurrentWaypoint from the path, changing CurrentWaypoint to
    /// the next waypoint.
    /// </summary>
    public void NextWaypoint()
    {
        waypoints.Pop();
    }

    /// <summary>
    /// True if all the waypoints have been removed from this path.
    /// </summary>
    public bool IsEmpty
    {
        get
        {
            return waypoints.Count == 0;
        }
    }
}
