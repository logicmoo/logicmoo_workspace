using System;
using Prolog;
using UnityEngine;

public abstract class PhysicalObject : BindingBehaviour
{
    /// <summary>
    /// If true, Sims won't notice this object unless they explicitly search for it.
    /// Also, suppresses rendering of the object until it's unhidden.
    /// </summary>
    public bool IsHidden;

    /// <summary>
    /// True if the object can be moved.
    /// </summary>
    public bool IsMovable = true;

    /// <summary>
    /// For containers only:
    /// Where, in local coordinates, objects placed on this object should appear.
    /// </summary>
    public Vector3 PlacedObjectPosition;

    public void SetHidden(bool state)
    {
        if (this.GetComponent<Renderer>() != null)
            this.GetComponent<Renderer>().enabled = !state;
        this.IsHidden = state;
    }

    public override void Awake()
    {
        base.Awake();
        if (IsHidden && this.GetComponent<Renderer>() != null)
            this.GetComponent<Renderer>().enabled = false;
    }

    #region Containment and destruction
    [HideInInspector]
    public GameObject Container;

    /// <summary>
    /// True if this object has not been destroyed.
    /// </summary>
    public bool Exists = true;

    public void MoveTo(GameObject newContainer)
    {
        if (!IsMovable)
            throw new InvalidOperationException("Attempt to move immovable object "+name);
        Container = newContainer;
        IsHidden = false;
        // Reparent our gameObject to newContainer
        // Because Unity is braindamaged, this has to be done by way of the transform.
        transform.parent = newContainer.transform;
        var physicalObject = newContainer.GetComponent<PhysicalObject>();
        if (physicalObject != null)
            physicalObject.ObjectAdded(this.gameObject);
    }

    public bool ContentsVisible;

    public void ObjectAdded(GameObject newObject)
    {
        if (newObject.GetComponent<Renderer>() != null)
        {
            newObject.GetComponent<Renderer>().enabled = ContentsVisible;
            var sr = newObject.GetComponent<Renderer>() as SpriteRenderer;
            if (sr != null && ContentsVisible)
                sr.sortingLayerName = "PlacedOnSurface";
        }
        else
        {
            var spriteController = newObject.GetComponent<SpriteSheetAnimationController>();
            if (spriteController != null)
            {
                spriteController.enabled = ContentsVisible;
                spriteController.Visible = ContentsVisible;
            }
        }
        newObject.transform.localPosition = PlacedObjectPosition;
    }

    public virtual void Destroy()
    {
        Exists = false;
        this.MoveTo(GameObject.Find("DestroyedObjects"));
        this.enabled = false;
        var spriteController = this.GetComponent<SpriteSheetAnimationController>();
        if (spriteController != null)
            spriteController.enabled = false;
        else
            this.GetComponent<Renderer>().enabled = false;
    }
    #endregion
}
