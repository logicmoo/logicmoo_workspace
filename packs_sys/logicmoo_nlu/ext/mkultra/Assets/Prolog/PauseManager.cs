using UnityEngine;

public class PauseManager : MonoBehaviour
{
    /// <summary>
    /// True if the game is paused
    /// </summary>
    public static bool Paused { get; set; }

    public static bool SingleStep { get; set; }

    static bool ReallyPaused
    {
        get
        {
            // ReSharper disable once CompareOfFloatsByEqualityOperator
            return Time.timeScale == 0;
        }

        set
        {
            Time.timeScale = value ? 0 : 1;
        }
    }

    internal void LateUpdate()
    {
        bool shouldBePaused = Paused && !SingleStep;
        // ReSharper disable once RedundantCheckBeforeAssignment
        if (ReallyPaused != shouldBePaused)
        {
            ReallyPaused = shouldBePaused;
        }
    }

    internal void OnGUI()
    {
        switch (Event.current.type)
        {
            case EventType.KeyDown:
                HandleKeypress();
                break;


            case EventType.Repaint:
                RepaintGUI();
                break;
        }
    }

    void HandleKeypress()
    {
        switch (Event.current.keyCode)
        {
            case KeyCode.F5:
                Paused = !Paused;
                break;

            case KeyCode.F10:
                SingleStep = true;
                break;
        }
    }

    public const int PauseGreyoutDepth = 100;

    void RepaintGUI()
    {
        GUI.depth = PauseGreyoutDepth;
        if (ReallyPaused)
        {
            //GUI.Label(new Rect(0, 0, 100, 100), "PAUSED");
            GUI.Box(new Rect(0, 0, Screen.width, Screen.height), GUIContent.none);
        }
    }
}

