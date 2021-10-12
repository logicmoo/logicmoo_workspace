using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// A pie menu for selecting player actions
/// </summary>
public class PieMenu
{
    private readonly GUIStyle style;
    private readonly IList<string> tags;
    private readonly IList<object> actions; 
    private readonly Rect[] rects;
    private readonly Rect bounds = new Rect(0,0,0,0);
    private readonly Texture2D background;

    public PieMenu(IList<string> tags, IList<object> actions, GUIStyle style, float innerRadius, Texture2D background)
    {
        this.tags = tags;
        this.actions = actions;
        this.style = style;
        this.background = background;

        rects = new Rect[tags.Count];
        for (int i = 0; i < tags.Count; i++)
        {
            var tagRect = TagRect(innerRadius, i, tags.Count, style.CalcSize(new GUIContent(tags[i])).x);
            rects[i] = tagRect;
            if (tagRect.xMin < bounds.xMin)
                bounds.xMin = tagRect.xMin;
            if (tagRect.xMax > bounds.xMax)
                bounds.xMax = tagRect.xMax;
            if (tagRect.yMin < bounds.yMin)
                bounds.yMin = tagRect.yMin;
            if (tagRect.yMax > bounds.yMax)
                bounds.yMax = tagRect.yMax;
        }
    }

    /// <summary>
    /// Draw the menu using the immediate-mode GUI calls
    /// </summary>
    public void Draw(Vector2 center, float innerRadius)
    {
        center = CorrectCenter(center);
        for (int i = 0; i < tags.Count; i++)
        {
            var rect = ScreenRect(center, i);
            GUI.Box(rect, background);
            if (rect.Contains(Event.current.mousePosition))
                GUI.Label(rect, "<color=yellow>"+tags[i]+"</color>");
            else
                GUI.Label(rect, tags[i]);
        }
    }

    private Rect ScreenRect(Vector2 center, int i)
    {
        return new Rect(center.x+rects[i].x, center.y+rects[i].y, rects[i].width, rects[i].height);
    }

    /// <summary>
    /// Shifts center of menu to ensure entire menu is on screen
    /// </summary>
    /// <param name="center">original center</param>
    /// <returns>new center</returns>
    private Vector2 CorrectCenter(Vector2 center)
    {
        var leftEdge = center.x + bounds.xMin;
        if (leftEdge < 0)
            center.x = center.x - leftEdge;

        var topEdge = center.y + bounds.yMin;
        if (topEdge < 0)
            center.y = center.y - topEdge;

        var rightEdge = center.x + bounds.xMax;
        if (rightEdge > Screen.width)
            center.x = center.x - (rightEdge - Screen.width);

        var bottomEdge = center.y + bounds.yMax;
        if (bottomEdge > Screen.height)
            center.y = center.y - (bottomEdge - Screen.height);

        return center;
    }

    private Rect TagRect(float innerRadius, int position, int tagCount, float width)
    {
        var angle = position*2*Mathf.PI/tagCount;
        var offset = new Vector2(innerRadius*Mathf.Cos(angle), innerRadius*Mathf.Sin(angle));
        var left = (offset.x > 0) ? offset.x : (offset.x - width);
        return new Rect(left, offset.y, width, style.lineHeight*1.5f);
    }

    /// <summary>
    /// The "action" of the menu item selected by the mouse, if any
    /// </summary>
    public object SelectedAction(Vector2 center)
    {
        center = CorrectCenter(center);
        for (int i = 0; i < tags.Count; i++)
            if (ScreenRect(center, i).Contains(Event.current.mousePosition))
                return actions[i];
        return null;
    }
}
