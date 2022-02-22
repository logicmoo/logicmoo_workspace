using System;
#if UNITY_EDITOR
using UnityEditor;
#endif
using UnityEngine;

[Serializable]
public class FloorMaterial : ScriptableObject
{
    public Sprite Sprite;

#if UNITY_EDITOR
    [MenuItem("Assets/Create/Floor Material")]
    internal static void Create()
    {
        AssetDatabase.CreateAsset(CreateInstance<FloorMaterial>(), "Assets/Floor Materials/New Floor Material.asset");
    }
#endif
}
