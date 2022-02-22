using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;


public static class TileCoreExtensionMethods
{
    public static Vector2 ScreenPosition(this GameObject gameObject)
    {
        if (gameObject == null)
            throw new ArgumentNullException("Attempt to get screen position of null object");
        if (Camera.main == null)
            throw new InvalidOperationException("Camera.main null during call to ScreenPosition()");
        return Camera.main.WorldToScreenPoint(gameObject.transform.position);
    }

    public static Rect? ScreenRect(GameObject go)
    {
        var position = Camera.main.WorldToScreenPoint(go.transform.position);
        var sr = go.GetComponent<SpriteRenderer>();
        if (sr != null)
        {
            var height = sr.sprite.bounds.size.y * sr.sprite.pixelsPerUnit;
            return new Rect(position.x, position.y - height,
                sr.sprite.bounds.size.x * sr.sprite.pixelsPerUnit,
                height);
        }

        var sa = go.GetComponent<SpriteSheetAnimationController>();
        if (sa != null)
        {
            var height = 1.5f * Tile.SizeInSceneUnits;
            var width = Tile.TileSizeInPixels;
            return new Rect(position.x - 0.5f * width, position.y - height, width, height);
        }

        return null;
    }

    public static Vector2 GUIScreenPosition(this GameObject gameObject)
    {
        var screenPosition = ScreenPosition(gameObject);
        return new Vector2(screenPosition.x, Screen.height - screenPosition.y);
    }

    public static Rect? GUIScreenRect(this GameObject go)
    {
        var position = go.GUIScreenPosition();

        var sr = go.GetComponent<SpriteRenderer>();
        if (sr != null && sr.sprite != null)
        {
            var height = sr.sprite.bounds.size.y*sr.sprite.pixelsPerUnit;
            return new Rect(position.x, position.y - height,
                sr.sprite.bounds.size.x*sr.sprite.pixelsPerUnit,
                height);
        }

        var sa = go.GetComponent<SpriteSheetAnimationController>();
        if (sa != null)
        {
            var height = 1.5f*Tile.TileSizeInPixels;
            var width = Tile.TileSizeInPixels;
            return new Rect(position.x-0.5f*width, position.y-height, width, height);
        }
        
        return null;
    }

    public static void DrawThumbNail(this GameObject gameObject, Vector2 screenLocation)
    {
        var ss = gameObject.GetComponent<SpriteSheetAnimationController>();
        if (ss == null)
            throw new ArgumentNullException("ThumbNailImage() called on game object with not SpriteSheetAnimationController");
        ss.DrawThumbNail(screenLocation);
    }

    public static Vector2 Center(this Rect r)
    {
        return new Vector2(0.5f*(r.xMax + r.xMin), 0.5f*(r.yMax + r.yMin));
    }
}
