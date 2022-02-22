using System;

using UnityEngine;

/// <summary>
/// These are just "extension methods", i.e. methods that appear within other classes,
/// that provide functionality that should probably have been within the original classes
/// to begin with.
/// </summary>
public static class GeometryExtensions
{
    /// <summary>
    /// True if this Rect overlaps with the other (including just touching the boundary)
    /// </summary>
    public static bool Overlaps(this Rect r1, Rect r2)
    {
        return (r1.xMin <= r2.xMax) && (r1.yMin <= r2.yMax) && (r1.xMax >= r2.xMin) && (r1.yMax >= r2.yMin);
    }

    /// <summary>
    /// Returns the vector v rotated 90 degrees clockwise
    /// </summary>
    public static Vector2 PerpClockwise(this Vector2 v)
    {
        return new Vector2(v.y, -v.x);
    }

    /// <summary>
    /// Returns the vector v rotated 90 degrees counter-clockwise
    /// </summary>
    public static Vector2 PerpCounterClockwise(this Vector2 v)
    {
        return new Vector2(-v.y, v.x);
    }

    /// <summary>
    /// Returns the Rect shifted by the specified offset vector.
    /// </summary>
    public static Rect Shift(this Rect r, Vector2 offset)
    {
        return new Rect(r.xMin+offset.x, r.yMin+offset.y, r.width, r.height);
    }

    public static Rect BoundingBox(this Collider2D c)
    {
        if (c == null)
            throw new ArgumentNullException();
        var t = c.transform;
        {
            var cc = c as CircleCollider2D;
            if (cc != null)
            {
                Vector2 center = t.TransformPoint(cc.offset);
                var halfSize = cc.radius * (Vector2)t.localScale;
                var lowerLeft = center - halfSize;
                return new Rect(lowerLeft.x, lowerLeft.y, 2 * halfSize.x, 2 * halfSize.y);
            }
        }
        {
            var bc = c as BoxCollider2D;
            if (bc != null)
            {
                Vector2 center = t.TransformPoint(bc.offset);
                var size = new Vector2(bc.size.x*t.localScale.x, bc.size.y*t.localScale.y);
                var lowerLeft = center - size*0.5f;
                return new Rect(lowerLeft.x, lowerLeft.y, size.x, size.y);
            }
        }
        throw new ArgumentException("Can't determine bounding box of collider: " + c);
    }

    public static Rect TransformRect(this Transform transform, Rect r)
    {
        var min = transform.TransformPoint(new Vector3(r.xMin, r.yMin, 0));
        var max = transform.TransformPoint(new Vector3(r.xMax, r.yMax, 0));
        return new Rect(min.x, min.y, max.x-min.x, max.y-min.y);
    }
}