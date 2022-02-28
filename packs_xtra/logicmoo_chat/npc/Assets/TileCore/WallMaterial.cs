using System;
#if UNITY_EDITOR
using UnityEditor;
#endif
using UnityEngine;

[Serializable]
public class WallMaterial : ScriptableObject
{
    public Sprite LeftSprite;
    public Sprite CenterSprite;
    public Sprite RightSprite;
    public Sprite SingletonSprite;

#if UNITY_EDITOR
    [MenuItem("Assets/Create/Wall Material")]
    internal static void Create()
    {
        AssetDatabase.CreateAsset(CreateInstance<WallMaterial>(), "Assets/Wall Materials/New Wall Material.asset");
    }
#endif
}
