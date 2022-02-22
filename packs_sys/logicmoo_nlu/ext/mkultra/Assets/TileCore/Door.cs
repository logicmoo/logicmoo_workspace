using System;
using Prolog;
using UnityEngine;

[AddComponentMenu("Tile/Door")]
[RequireComponent(typeof(SpriteRenderer), typeof(BoxCollider2D))]
class Door : PhysicalObject
{
#pragma warning disable 0649
    public Sprite ClosedSprite;
    public Sprite OpenSprite;
    public bool InitiallyOpen;
    public bool LeaveClosed;
    public bool Locked;
    public GameObject ForceRoom;
#pragma warning restore 0649

    public bool Open
    {
        get
        {
            return this.GetComponent<Collider2D>().enabled;
        }

        set
        {
            if (Locked && value)
                // Can't open a locked door
                return;

            this.ForceDoorState(value);
        }
    }

    public void ForceDoorState(bool open)
    {
        this.GetComponent<SpriteRenderer>().sprite = open ? this.OpenSprite : this.ClosedSprite;
        this.GetComponent<Collider2D>().isTrigger = !Locked;
    }

    public override void Awake()
    {
        this.ForceDoorState(InitiallyOpen);
        base.Awake();
    }

    public void Start()
    {
        if (!KnowledgeBase.Global.IsTrue(new Structure("register_door", gameObject)))
            throw new Exception("Can't register door " + name);
    }

    internal void OnTriggerEnter2D(Collider2D enterer)
    {
        Open = true;
    }

    internal void OnTriggerExit2D(Collider2D exiter)
    {
        Open = !LeaveClosed;
    }
}
