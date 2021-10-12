using UnityEngine;

/// <summary>
/// An overlay to show some image or text when the player examines the object.
/// </summary>
public class ExaminationContent : MonoBehaviour
{
    protected ExaminationContent()
    {
        Content = new GUIContent { tooltip = "Press any key to continue" };
    }

    /// <summary>
    /// The actual data to be displayed (text or image).
    /// </summary>
    public GUIContent Content;
    /// <summary>
    /// Style in which to display the content.
    /// </summary>
    public GUIStyle Style;

    private int popUpTimestamp;
    public void PopUp()
    {
        Display = true;
        popUpTimestamp = Time.frameCount;
    }

    /// <summary>
    /// Whether to display the content.
    /// Displaying content pauses the game.
    /// </summary>
    protected bool Display
    {
        get
        {
            return mDisplay;
        }
        set
        {
            mDisplay = PauseManager.Paused = value;
        }
    }
    private bool mDisplay;

    private static Texture2D greyOutTexture;
    internal void Start()
    {
        if (greyOutTexture == null)
        {
            greyOutTexture = new Texture2D(1, 1);
            // For some reason, changing the alpha value here has no effect on the amount of greying out
            greyOutTexture.SetPixel(0, 0, new Color(0, 0, 0, 0.5f));
        }
    }

    private Vector2 scrollPosition;

    internal void OnGUI()
    {
        if (!Display)
            return;

        switch (Event.current.type)
        {
            case EventType.Repaint:
            case EventType.layout:
                GUI.depth = -1;

                var screenRect = new Rect(0, 0, Screen.width, Screen.height);
                // For some reason, changing the alpha on greyOutTexture has no effect on the greying out
                // but drawing the box twice does :(
                GUI.Box(screenRect, greyOutTexture);
                GUI.Box(screenRect, greyOutTexture);

                scrollPosition = GUILayout.BeginScrollView(scrollPosition);
                GUILayout.Label(Content, Style);
                GUILayout.EndScrollView();
                break;

            case EventType.KeyDown:
                if (Time.frameCount > popUpTimestamp+10)
                    Display = false;
                break;
        }
    }
}
