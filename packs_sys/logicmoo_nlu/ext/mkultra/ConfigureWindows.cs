using System;
using Prolog;
using UnityEngine;

/// <summary>
/// Arranges areas on screen based on screen resolution.
/// </summary>
internal class ConfigureWindows : MonoBehaviour
{
    internal void Start()
    {
        // Set the camera viewport to be the left side of the screen, below the NLPrompt's input area.
        var prompt = FindObjectOfType<NLPrompt>();
        var theCamera = Camera.main;
        var bottomOfUi = Math.Min(prompt.InputRect.yMin, Math.Min(prompt.CommentaryRect.yMin, prompt.ResponseRect.yMin));
        var r = theCamera.pixelRect;
        r.height -= bottomOfUi + 50;
        theCamera.pixelRect = r;
        FindObjectOfType<TileMap>().UpdateCamera(theCamera);
        Tile.UpdateTileSize(theCamera);

        // Place the Prolog console in the upper-right-hand corner
        var console = FindObjectOfType<PrologConsole>();
        console.WindowRect.y = 0;
        console.WindowRect.x = Screen.width - console.WindowRect.width;

        // Place the EL inspector in the lower-right-hand corner
        var inspector = FindObjectOfType<ELInspector>();
        inspector.WindowRect.y = console.WindowRect.y + console.WindowRect.height;
        inspector.WindowRect.height = Screen.height - inspector.WindowRect.height;
        inspector.WindowRect.x = Screen.width - inspector.WindowRect.width;
    }
}
