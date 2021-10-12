/// <summary>
/// Represents one animation of an object from within an AnimationSheet.
/// </summary>
public abstract class SpriteAnimation
{
    protected SpriteAnimation(string name)
    {
        Name = name;
    }

    /// <summary>
    /// Name of the animation
    /// </summary>
    public string Name;
    /// <summary>
    /// Number of frames in the animation
    /// </summary>
    public abstract int Frames { get; }
    /// <summary>
    /// Number of seconds in the animation, if it is a timed animation
    /// </summary>
    public abstract float Seconds { get; }
    /// <summary>
    /// Distance covered by the animation if it's a distance animation.
    /// </summary>
    public abstract float Stride { get; }

    /// <summary>
    /// Returns the position within the AnimationSheet of the specified frame of this animation.
    /// </summary>
    /// <param name="frameNumber">Frame number (0=first frame)</param>
    /// <returns>Position of the frame.</returns>
    public abstract TilePosition Frame(int frameNumber);

    /// <summary>
    /// The frame that's phrase/period of the way through the animation.
    /// </summary>
    /// <param name="phase">How far into the animation to take a frame on an arbitrary scale from 0 to period.</param>
    /// <param name="period">Length of the animation.</param>
    /// <returns>Position of the frame within the AnimationSheet</returns>
    public TilePosition FrameAtPhase(float phase, float period)
    {
        var framePosition = ((phase / period) % 1f) * Frames;
        return this.Frame((int)framePosition);
    }

    /// <summary>
    /// The frame of the animation at the specified point in time (for timed animations)
    /// </summary>
    /// <param name="time">Number of seconds into the animation</param>
    /// <returns>Position of the frame within the animationsheet</returns>
    public TilePosition FrameAtTime(float time)
    {
        return this.FrameAtPhase(time, Seconds);
    }

    /// <summary>
    /// The frame of the animation at the specified distance, for distance-based animations.
    /// </summary>
    /// <param name="distance">Distance</param>
    /// <returns>Position of the frame within the AnimationSheet</returns>
    public TilePosition FrameAtDistance(float distance)
    {
        return this.FrameAtPhase(distance, Stride);
    }
}
