using System;
using System.Collections.Generic;

using UnityEngine;

[ExecuteInEditMode]
public class SpriteSheetAnimationController : MonoBehaviour
{
    #region Inspector-editable variables
    public Material Material;

    public Color Color = Color.white;

    public Texture2D SpriteSheet;

    public int Rows = 4;

    public int Columns = 4;
    #endregion

    #region State variables for animation

    /// <summary>
    /// Currently playing animation
    /// </summary>
    public SpriteAnimation CurrentAnimation { get; protected set; }

    /// <summary>
    /// Current state of the current animation.
    /// </summary>
    private AnimationMode mode = AnimationMode.Stopped;

    /// <summary>
    /// Time the current animation was started, if it is in Time mode.
    /// </summary>
    private float startTime;

    /// <summary>
    /// Position the current animation started, if it's in Distance mode.
    /// </summary>
    private Vector2 startPosition;

    /// <summary>
    /// Unit vector in the direction of current motion.
    /// Used to measure distance traveled when playing an animation in Distance mode.
    /// </summary>
    protected Vector2 MovementDirection;

    /// <summary>
    /// Current frame of the animation as determined by mode, time, and/or distance.
    /// </summary>
    public TilePosition CurrentFrame
    {
        get
        {
            if (this.CurrentAnimation == null)
                return new TilePosition(0, 0);

            switch (mode)
            {
                case AnimationMode.Stopped:
                    return this.CurrentAnimation.FrameAtPhase(0, 1);

                case AnimationMode.Time:
                    return this.CurrentAnimation.FrameAtPhase(Time.fixedTime - startTime, this.CurrentAnimation.Seconds);

                case AnimationMode.Distance:
                    float phase = Vector2.Dot((Vector2)transform.position - startPosition, this.MovementDirection);
                    return this.CurrentAnimation.FrameAtPhase(
                        phase, this.CurrentAnimation.Stride);

                default:
                    throw new InvalidOperationException("Bad spriteAnimation mode");
            }
        }
    }
    #endregion

    /// <summary>
    /// Controls how an animation updates
    /// </summary>
    enum AnimationMode
    {
        /// <summary>
        /// Animation stays at its current frame until further notice.
        /// </summary>
        Stopped,
        /// <summary>
        /// Animation changes frames based on distance moved by the sprite, rather than time
        /// The animation's Stride determines the distance the sprite has to move to cycle through the whole animation.
        /// </summary>
        Distance,
        /// <summary>
        /// The animation advances based on elapsed time.
        /// </summary>
        Time
    }

    static readonly Dictionary<string, SpriteAnimation> Animations = new Dictionary<string, SpriteAnimation>();

    static SpriteSheetAnimationController()
    {
        const float Stride = 0.75f;
        Animations["North"] = new RowSpriteAnimation("North", 0, 4, Stride, 0);
        Animations["East"] = new RowSpriteAnimation("East", 1, 4, Stride, 0);
        Animations["West"] = new RowSpriteAnimation("West", 2, 4, Stride, 0);
        Animations["South"] = new RowSpriteAnimation("South", 3, 4, Stride, 0);
    }

    #region User-callable procedures for controling animation.
    /// <summary>
    /// Halt the current animation, whatever it may be, on the current frame, whatever it may be.
    /// </summary>
    public void StopAnimation()
    {
        mode = AnimationMode.Stopped;
    }

    /// <summary>
    /// Switch to the first frame of the specified animation and stay there.
    /// </summary>
    /// <param name="animationName">Name of the animation</param>
    public void StartIdleAnimation(string animationName)
    {
        this.StartIdleAnimation(Animations[animationName]);
    }

    /// <summary>
    /// Switch to the first frame of the specified animation and stay there.
    /// </summary>
    /// <param name="spriteAnimation">The animation to switch to</param>
    public void StartIdleAnimation(SpriteAnimation spriteAnimation)
    {
        this.CurrentAnimation = spriteAnimation;
        mode = AnimationMode.Stopped;
    }

    /// <summary>
    /// Switch to the first frame of this animation and start it playing in timed mode.
    /// </summary>
    /// <param name="animationName">The animation to switch to.</param>
    public void StartTimedAnimation(string animationName)
    {
        StartTimedAnimation(Animations[animationName]);
    }

    /// <summary>
    /// Switch to the first frame of this animation and start it playing in timed mode.
    /// </summary>
    /// <param name="spriteAnimation">The animation to switch to.</param>
    public void StartTimedAnimation(SpriteAnimation spriteAnimation)
    {
        this.CurrentAnimation = spriteAnimation;
        mode = AnimationMode.Time;
        startTime = Time.fixedTime;
    }

    /// <summary>
    /// Switch to the first frame of this animation and start it playing in distance mode.
    /// </summary>
    /// <param name="animationName">The animation to switch to.</param>
    /// <param name="motionDirection">Direction in which to measure distance.</param>
    public void StartPositionalAnimation(string animationName, Vector2 motionDirection)
    {
        StartPositionalAnimation(Animations[animationName], motionDirection);
    }

    /// <summary>
    /// Switch to the first frame of this animation and start it playing in distance mode.
    /// </summary>
    /// <param name="spriteAnimation">The animation to switch to.</param>
    /// <param name="motionDirection">Direction in which to measure distance.</param>
    public void StartPositionalAnimation(SpriteAnimation spriteAnimation, Vector2 motionDirection)
    {
        this.CurrentAnimation = spriteAnimation;
        startPosition = this.transform.position;
        this.MovementDirection = motionDirection;
        mode = AnimationMode.Distance;
    }
    #endregion

    #region Unity message handlers
    internal void Awake()
    {
        Material = new Material(Shader.Find("Unlit/Transparent Cutout"))
                   {
                       mainTexture = this.SpriteSheet,
                       color = Color.white
                   };
        Material.SetTexture("Illumin", this.SpriteSheet);
        StartIdleAnimation("South");
    }

    public static Vector3 SpriteOffset = new Vector3(0.5f, 0.175f, 0);

    public void OnRenderObject()
    {
        var p = transform.position-SpriteOffset;
        var uSize = 1.0f / Columns;
        var vSize = 1.0f / Rows;
        var frame = CurrentFrame;
        var column = frame.Column;
        var row = frame.Row;
        // Kluge
        if (Color == Color.yellow)
            return;
        Material.color = Color;
        Material.SetPass(0);
        GL.Begin(GL.QUADS);
        GL.Color(Color);
        GL.TexCoord2(column*uSize, row*vSize);
        GL.Vertex3(p.x, p.y, p.z);
        GL.TexCoord2(column * uSize, (row+1) * vSize);
        GL.Vertex3(p.x, p.y+1.5f, p.z);
        GL.TexCoord2((column+1) * uSize, (row+1) * vSize);
        GL.Vertex3(p.x+1, p.y+1.5f, p.z);
        GL.TexCoord2((column+1) * uSize, row * vSize);
        GL.Vertex3(p.x+1, p.y, p.z);
        GL.End();
    }
    #endregion
}
