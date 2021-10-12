using UnityEngine;

public class CharacterSteeringController : BindingBehaviour
{
    public void Start()
    {
        this.debugArrowShader = new Material(Shader.Find("GUI/Text Shader"));
        this.Face(new Vector2(0,-1));
    }

    /// <summary>
    /// Called when character dies to stop character from moving
    /// </summary>
    public void CharacterDead()
    {
        enabled = false;
        GetComponent<Rigidbody2D>().velocity = Vector2.zero;
    }

    #region Fields and properties
    Material debugArrowShader;
    [Bind]
#pragma warning disable 649
    private SpriteSheetAnimationController spriteController;
#pragma warning restore 649

    /// <summary>
    /// The current location we're steering to.  If null, then character is stopped.
    /// </summary>
    private Vector2? targetLocation;

    private float maxSpeed;

    public float MaxForce = 1000;

    /// <summary>
    /// The current position of the character.
    /// </summary>
    public Vector2 Position
    {
        get
        {
            return transform.position;
        }
    }
    #endregion

    #region Externally callable control routines
    /// <summary>
    /// Stops the character.
    /// </summary>
    public void Stop()
    {
        targetLocation = null;
        GetComponent<Rigidbody2D>().velocity = Vector2.zero;
    }

    /// <summary>
    /// Switches the target of the seek behavior to be the specified location.
    /// </summary>
    /// <param name="target">New target location for Seek behavior</param>
    /// <param name="speed">Speed at which to drive in this direction.</param>
    public void Seek(Vector2 target, float speed)
    {
        targetLocation = target;
        maxSpeed = speed;
    }

    /// <summary>
    /// Face the specified direction, without moving.
    /// </summary>
    /// <param name="direction">Vector in the direction to face</param>
    public void Face(Vector2 direction)
    {
        this.SwitchToFacingAnimation(this.NearestCardinalDirection(direction.normalized));
    }
    #endregion

    #region Steering behaviors
    Vector2 SeekSteering()
    {
        if (targetLocation == null)
            return Vector2.zero;
        var offset = targetLocation.Value - Position;
        var distanceToTarget = offset.magnitude;
        if (distanceToTarget < 0.05f)
            return Vector2.zero;
        return offset * MaxForce / distanceToTarget;
    }

#if CollisionAvoidance
    private float collisionTime;
    private bool collisionDetected;
    Vector2? CollisionAvoidanceSteering()
    {
        float firstCollisionTime = 4;
        Vector2 firstCollisionOffset = Vector2.zero;
        foreach (var otherCharacter in Registry<CharacterSteeringController>())
        {
            if (otherCharacter != this)
            {
                var time = TimeOfClosestApproach(this, otherCharacter);
                //collisionTime = time;
                if (time > 0 && time < firstCollisionTime)
                {
                    var myPosition = PredictedPosition(this, time);
                    var theirPosition = this.PredictedPosition(otherCharacter, time);
                    var offset = myPosition - theirPosition;
                    if (offset.magnitude < 1.5)
                    {
                        firstCollisionTime = time;
                        firstCollisionOffset = offset;
                    }
                }
            }
        }

        //collisionDetected = false;
        if (firstCollisionTime > 3)
            return null;

        //collisionDetected = true;
        if (this.rigidbody2D.velocity == Vector2.zero)
        {
            return firstCollisionOffset.normalized.PerpCounterClockwise();
        }
        var separation = firstCollisionOffset.magnitude;
        var escapeDirection = firstCollisionOffset / separation;
        if (separation < 2)
            escapeDirection += escapeDirection.PerpClockwise().normalized;

        return 0.5f * this.MaxForce * escapeDirection;
    }

    private Vector2 PredictedPosition(Component sprite, float time)
    {
        return (Vector2)sprite.transform.position + time * sprite.rigidbody2D.velocity;
    }

    /// <summary>
    /// Computes the time of closest approach for the two characters, given their velocities
    /// Note: time could be negative (i.e. in the past)
    /// </summary>
    private float TimeOfClosestApproach(Component c1, Component c2)
    {
        var i = (Vector2)c1.transform.position - (Vector2)c2.transform.position;
        var deltaV = c1.rigidbody2D.velocity - c2.rigidbody2D.velocity;
        return -Vector2.Dot(i, deltaV) / deltaV.sqrMagnitude;
    }
    
    Vector2 MaybeAdd(Vector2 v1, Vector2? v2)
    {
        if (v2 == null)
            return v1;
        return v1 + v2.Value;
    }
#else
    private Vector2? CollisionAvoidanceSteering()
    {
        return null;
    }
#endif

    /// <summary>
    /// Connection to physics system.
    /// 
    /// Computes force and passes it on to the rigidBody2D
    /// Implements force can velocity caps.
    /// </summary>
    public void FixedUpdate()
    {
        var seekSteering = this.SeekSteering();
        var collisionAvoidanceSteering = this.CollisionAvoidanceSteering();

        //var force = this.MaybeAdd(seekSteering, collisionAvoidanceSteering);
        var force = seekSteering;

        // Throttle force at MaxForce
        var fMag = force.magnitude;
        if (fMag > MaxForce)
            force *= MaxForce / fMag;

        var rb = GetComponent<Rigidbody2D>();
        var vel = rb.velocity;
        var currentSpeed = vel.magnitude;

        // Inhibit acceleration in current motion direction if already at max speed.
        if (currentSpeed >= maxSpeed)
        {
            var heading = vel/currentSpeed;
            var forceInDirectionOfHeading = Vector2.Dot(heading, force);
            // Don't allow acceleration in the direction of our current motion
            if (forceInDirectionOfHeading > 0)
            {
                force -= heading * forceInDirectionOfHeading;
            }
        }

        BlueVector = seekSteering * 0.1f;
        RedVector = collisionAvoidanceSteering.HasValue ? (collisionAvoidanceSteering.Value * 0.1f) : Vector2.zero;
        GreenVector = force*0.1f;

        rb.AddForce(force);
        if (collisionAvoidanceSteering.HasValue)
            rb.AddForce(collisionAvoidanceSteering.Value);
    }
    #endregion

    #region Animation control
    private string currentState = "";
    private Vector2 currentDirection;
    private SpriteRenderer mySpriteRenderer;

    /// <summary>
    /// Connection to the animation system
    /// 
    /// Determines direction and speed of motion, and plays appropriate animation accordingly
    /// </summary>
    public void Update()
    {
        this.UpdateWalkAnimation(this.GetComponent<Rigidbody2D>().velocity);
        if (mySpriteRenderer == null)
             mySpriteRenderer = GetComponent<SpriteRenderer>();
    }

    Vector3 NearestCardinalDirection(Vector2 direction)
    {
        var result = direction;
        if (Mathf.Abs(direction.x) > Mathf.Abs(direction.y))
            result.y = 0;
        else
            result.x = 0;
        return result.normalized;
    }

    private bool wasStopped = true;
    private void UpdateWalkAnimation(Vector2 characterVelocity)
    {
        //this.animator.speed = characterVelocity.magnitude;

        if (characterVelocity.magnitude < 0.01)
        {
            spriteController.StopAnimation();
            wasStopped = true;
        }
        else
        {
            var desiredDirection = characterVelocity.normalized;
            if (Vector2.Dot(currentDirection, desiredDirection) > 0.7f && !wasStopped)
                // Close enough; don't change it.
                return;

            wasStopped = false;
            this.currentDirection = this.NearestCardinalDirection(desiredDirection);
            if (this.currentDirection.x > 0)
                currentState = "East";
            else if (this.currentDirection.x < 0)
                currentState = "West";
            else if (this.currentDirection.y > 0)
                currentState = "North";
            else
                currentState = "South";
            spriteController.StartPositionalAnimation(currentState, currentDirection);
        }
    }

    private void SwitchToFacingAnimation(Vector2 direction)
    {
        currentDirection = direction;
        if (this.currentDirection.x > 0)
            currentState = "East";
        else if (this.currentDirection.x < 0)
            currentState = "West";
        else if (this.currentDirection.y > 0)
            currentState = "North";
        else
            currentState = "South";
        spriteController.StartIdleAnimation(currentState);
    }
    #endregion

    #region DebugDrawing
    public Vector2 RedVector;

    public Vector2 GreenVector;

    public Vector2 BlueVector;

    public bool DisplayDebugVectors;

    internal void OnRenderObject()
    {
        if (DisplayDebugVectors)
        {
            this.debugArrowShader.SetPass(0);
            GL.Begin(GL.TRIANGLES);
            this.DrawArrowhead(RedVector, Color.red);
            this.DrawArrowhead(GreenVector, Color.green);
            this.DrawArrowhead(BlueVector, Color.blue);
            GL.End();
        }
    }

    private void DrawArrowhead(Vector2 vector, Color color)
    {
        if (vector != Vector2.zero)
        {
            var start = Position;
            var end = Position + vector;
            var perp = 0.05f * vector.PerpClockwise().normalized;
            GL.Color(color);
            GL.Vertex(start - perp);
            GL.Vertex(end);
            GL.Vertex(start + perp);
        }
    }
    #endregion
}
