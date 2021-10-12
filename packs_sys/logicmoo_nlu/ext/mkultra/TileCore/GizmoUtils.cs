using UnityEngine;

public static class GizmoUtils
{
    public static void Draw(Rect r)
    {
        Gizmos.DrawWireCube(r.center, new Vector3(r.width, r.height));
    }
}
