using System;
using System.Collections.Generic;
using System.Linq;
using Prolog;

using UnityEngine;

public class PropInfo : PhysicalObject
{
    /// <summary>
    /// True if this is a container that can hold other things.
    /// </summary>
    public bool IsContainer;

    /// <summary>
    /// True if this satisfies hunger
    /// </summary>
    public bool IsFood;

    /// <summary>
    /// True if this is quenches thirst.
    /// </summary>
    public bool IsBeverage;

    /// <summary>
    /// The word for this type of object
    /// </summary>
    public string Kind;

    /// <summary>
    /// Any adjectives to attach to this object
    /// </summary>
    public string[] Adjectives=new string[0];

    public override void Awake()
    {
        base.Awake();
        foreach (var o in Contents)
            o.Container = gameObject;
    }

    public void Start()
    {
        if (!KB.Global.IsTrue("register_prop",
                                gameObject, Symbol.Intern(Kind),
                                // Mono can't infer the type on this, for some reason
                                // ReSharper disable once RedundantTypeArgumentsOfMethod
                                Prolog.Prolog.IListToPrologList(new List<Symbol>(Adjectives.Select<string,Symbol>(Symbol.Intern))))
            )
            throw new Exception("Can't register prop "+name);
    }

    internal void OnGUI()
    {
        if (Camera.current != null && GetComponent<Renderer>() == null && !IsHidden)
        {
            var bubblelocation = (Vector2)Camera.current.WorldToScreenPoint(transform.position);
            var topLeft = new Vector2(bubblelocation.x, Camera.current.pixelHeight - bubblelocation.y);
            var size = new Vector2(300, 3000);
            var bubbleRect = new Rect(topLeft.x, topLeft.y, size.x, size.y);
            GUI.Label(bubbleRect, name);
        }
    }

    #region Container operations
    public IEnumerable<PhysicalObject> Contents
    {
        get
        {
            foreach (Transform child in transform)
                yield return child.GetComponent<PhysicalObject>();
        }
    } 
    #endregion
}
