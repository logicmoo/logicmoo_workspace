using System;
using UnityEngine;

[Serializable]
public class RowSpriteAnimation : SpriteAnimation
{
    public RowSpriteAnimation(string name, int row, int columns, float stride, float seconds)
        : base(name)
    {
        this.row = row;
        this.columns = columns;
        this.stride = stride;
        this.seconds = seconds;
    }

    // ReSharper disable InconsistentNaming
    /// <summary>
    /// The row of the SpriteSheet in which the animation appears
    /// </summary>
    [HideInInspector]
    public int row;

    /// <summary>
    /// The distance this animation covers, if it's a distance-based animation
    /// </summary>
    public float stride;

    /// <summary>
    /// The time this animation covers, if it's time-based.
    /// </summary>
    public float seconds;

    public int columns;

    // ReSharper restore InconsistentNaming

    /// <summary>
    /// Number of frames in the animation (fixed at the number of columns for a RowAnimationSheet).
    /// </summary>
    public override int Frames
    {
        get
        {
            return columns;
        }
    }

    /// <summary>
    /// The i'th frame of this animation
    /// </summary>
    /// <param name="frameNumber">Frame number of the frame (0=first frame)</param>
    /// <returns>The frame, specified as a TilePosition in the underlying SpriteSheet.</returns>
    public override TilePosition Frame(int frameNumber)
    {
        return new TilePosition(frameNumber, this.row);
    }

    /// <summary>
    /// Distance the character has to move to cycle through the complete animation.
    /// </summary>
    public override float Stride
    {
        get
        {
            return stride;
        }
    }

    /// <summary>
    /// Number of seconds for the animation
    /// </summary>
    public override float Seconds
    {
        get
        {
            return seconds;
        }
    }

    /// <summary>
    /// Just returns the name of the animationsheet.
    /// </summary>
    /// <returns>the name</returns>
    public override string ToString()
    {
        return Name;
    }
}