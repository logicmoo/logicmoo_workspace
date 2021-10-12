using System;
using System.Collections.Generic;
using Prolog;
using UnityEngine;
using UnityEngine.Assertions;

/*
 * WORKING MEMORY INTERFACE
 * 
 * /perception/conversational_space/*
 * /perception/social_space/*
 * /perception/nobody_speaking
 * /perception/docked_with:OBJECT
 * 
 * /motor_root/walking_to:Destination
 * /motor_root/last_action:Action
 * /motor_root/i_am_speaking
 * 
 * /event_history/*
 */


/// <summary>
/// Mediates between Prolog code and Unity
/// - Controls locomotion
/// - Updates percepts and efference information in working memory (EL KB)
/// </summary>

[AddComponentMenu("Sims/Sim Controller")]
// ReSharper disable once CheckNamespace
public class SimController : PhysicalObject
{
    #region Public fields
    /// <summary>
    /// If true, the things the character says to themselves are displayed
    /// </summary>
    public bool ShowMentalMonologue;

    public float PositiveAffect;
    public float NegativeAffect;
    #endregion

    #region Constants
    /// <summary>
    /// The radius of the the circular region around the character that defines
    /// its "conversational space".  Two characters having a conversation should
    /// normally be within one anothers' conversational spaces.
    /// </summary>
    private const float ConversationalSpaceRadius = 2;
    private const float SocialSpaceRadius = 4;

    /// <summary>
    /// Maximum number of characters that can be within a character's conversational space.
    /// </summary>
    private const int MaxConversationalSpaceColliders = 30;

    private const float SpeechDelaySecondsPerChar = 0.05f;

    private const float SpeechDelayMinimum = 1f;
    private const float SpeechDelayMaximum = 6f;

    private readonly Symbol playerSymbol = Symbol.Intern("player");
    #endregion

    /// <summary>
    /// Whether to log actions as they're taken.
    /// </summary>
    public bool LogActions;

    /// <summary>
    /// Is this character currently talking?
    /// </summary>
    public bool IsSpeaking
    {
        get
        {
            return currentSpeechBubbleText != null;
        }
    }

    #region Bindings to other components
#pragma warning disable 649
    [Bind]
    private CharacterSteeringController steering;

    [Bind]
    private SpriteSheetAnimationController spriteController;

    [Bind(BindingScope.GameObject, BindingDefault.Ignore)]
    private NLPrompt nlPrompt;

    [Bind(BindingScope.Global, BindingDefault.Create)]
    private PathPlanner planner;

    [Bind(BindingScope.Global)]
    private TileMap tileMap;

    [Bind(BindingScope.GameObjectOrChildren, BindingDefault.Ignore)]
    private Sonifier sonifier;
#pragma warning restore 649
    #endregion

    #region Private fields

    internal static Texture2D GreyOutTexture;

    private ELNode elRoot;

    private ELNode perceptionRoot;

    private ELNode locationRoot;

    private ELNode conversationalSpace;

    private ELNode socialSpace;

    private ELNode lastDestination;

    private ELNode eventHistory;

    private ELNode motorRoot;

    private ELNode physiologicalStates;

    private Room myCurrentRoom;

    readonly Queue<Structure> eventQueue = new Queue<Structure>();

    /// <summary>
    /// Holds the current text for the character's speech bubble.
    /// Set to null is no active text.
    /// </summary>
    string currentSpeechBubbleText;

    private bool currentlySpeaking;
    private GameObject addressee;

    /// <summary>
    /// Time at which currentSpeechBubbleText should be set to null.
    /// </summary>
    private float clearSpeechTime;

    /// <summary>
    /// Current path being followed if the character is moving.  Null if no current locomotion goal.
    /// </summary>
    private TilePath currentPath;

    private GameObject currentDestination;

    /// <summary>
    /// Object being locomoted to, if any.
    /// </summary>
    /// <summary>
    /// Object being locomoted to, if any.
    /// </summary>
    public GameObject CurrentDestination
    {
        get
        {
            return currentDestination;
        }
        set
        {
            currentDestination = value;
            if (currentDestination == null)
            {
                motorRoot.DeleteKey(SWalkingTo);
            }
            else
                ELNode.Store(motorRoot/SWalkingTo%CurrentDestination);
        }
    }

    /// <summary>
    /// Object with which we're currently docked.
    /// </summary>
    public GameObject CurrentlyDockedWith { get; private set; }

    /// <summary>
    /// Time to wake character up and ask for an action.
    /// </summary>
    private float? pauseUntil;
    #endregion

    #region Event queue operations
    /// <summary>
    /// True if there are events waiting to be processed.
    /// </summary>
    bool EventsPending
    {
        get
        {
            return eventQueue.Count > 0;
        }
    }

    private static readonly object[] NullArgs = { null };
    /// <summary>
    /// Informs character of the specified event.  Does not copy the arguments.
    /// </summary>
    /// <param name="eventType">Type of event (functor of the Prolog structure describing the event)</param>
    /// <param name="args">Other information (arguments to the functor).
    /// WARNING: does not copy arguments, so they must either be ground or not used elsewhere.</param>
    public void QueueEvent(string eventType, params object[] args)
    {
        if (args == null)
            args = NullArgs;
        QueueEvent(new Structure(Symbol.Intern(eventType), args));
    }

    /// <summary>
    /// Informs character of the specified event.  Does not copy the eventDescription.
    /// </summary>
    /// <param name="eventDescription">A Prolog term describing the event.
    /// WARNING: does not copy the term, so it must either be ground or not used elsewhere.</param>
    public void QueueEvent(Structure eventDescription)
    {
        eventQueue.Enqueue((Structure)Term.CopyInstantiation(eventDescription));
    }

    Structure GetNextEvent()
    {
        return eventQueue.Dequeue();
    }
    #endregion

    #region Event handling
    /// <summary>
    /// Calls Prolog on all pending events and initiates any actions it specifies.
    /// </summary>
    private void HandleEvents()
    {
        if (EventsPending)
            pauseUntil = null;
        while (EventsPending)
            NotifyEvent(GetNextEvent());
    }

    /// <summary>
    /// Call into Prolog to respond to EVENTDESCRIPTION
    /// </summary>
    /// <param name="eventDescription">Term representing the event</param>
    private void NotifyEvent(object eventDescription)
    {
        ELNode.Store(eventHistory/Term.CopyInstantiation(eventDescription));
        if (!this.IsTrue(new Structure(SNotifyEvent, eventDescription)))
            Debug.LogError("notify_event/1 failed: "+ISOPrologWriter.WriteToString(eventDescription));
    }

    private static readonly Symbol SNotifyEvent = Symbol.Intern("notify_event");
    #endregion

    #region Unity hooks
    internal void Start()
    {
        updateConcernBids = UpdateConcernBids;
        updateStatusTextBids = UpdateStatusTextBids;
        elRoot = this.KnowledgeBase().ELRoot;
        perceptionRoot = elRoot / Symbol.Intern("perception");
        locationRoot = perceptionRoot / Symbol.Intern("location");
        conversationalSpace = perceptionRoot / Symbol.Intern("conversational_space");
        socialSpace = perceptionRoot / Symbol.Intern("social_space");
        motorRoot = elRoot / Symbol.Intern("motor_state");
        physiologicalStates = elRoot / Symbol.Intern("physiological_states");
        eventHistory = elRoot / Symbol.Intern("event_history");
        lastDestination = elRoot / Symbol.Intern("last_destination");
        ELNode.Store(lastDestination % null);  // Need a placeholder last destination so that /last_destination/X doesn't fail.
        if (!KB.Global.IsTrue("register_character", gameObject))
            throw new Exception("Can't register character " + name);
        if (GreyOutTexture == null) {
            GreyOutTexture = new Texture2D(1,1);
            GreyOutTexture.SetPixel(0,0, new Color(0,0,0, 128));
        }
    }

    private bool prologInitializationsExecuted;
    private void EnsureCharacterInitialized()
    {
        if (prologInitializationsExecuted)
            return;
        try
        {
            prologInitializationsExecuted = true;
            gameObject.IsTrue(Symbol.Intern("do_all_character_initializations"));
        }
        catch (Exception e)
        {
            Debug.LogError("Exception while initializing character " + gameObject.name);
            Debug.LogException(e);
        }
    }

    private Color flashColorA = Color.red;
    private Color flashColorB = Color.green;

    private float flashStartTime;

    private float flashEndTime;

    private float flashPeriod;

    public void AffectiveEvent(float positive, float pWeight, float negative, float nWeight)
    {
        PositiveAffect = positive*pWeight + PositiveAffect*(1 - pWeight);
        NegativeAffect = negative*nWeight + NegativeAffect*(1 - nWeight);
        if (float.IsNaN(PositiveAffect))
        {
            Debug.Log(gameObject.name+": positive Affect is not a number: "+Time.time);
            PositiveAffect = 0;
        }

        if (float.IsNaN(NegativeAffect))
        {
            Debug.Log(gameObject.name+": negative Affect is not a number: "+Time.time);
            NegativeAffect = 0;
        }
    }

    public float Arousal
    {
        get { return 0.5f*(PositiveAffect + NegativeAffect); }
    }

    public float Valence
    {
        get { return PositiveAffect - NegativeAffect; }
    }

    internal void Update()
    {
        AffectiveEvent(0, 0.001f, 0, 0.001f);
        var t = Time.time;
        if (t < flashEndTime)
        {
            var dT = t - flashStartTime;
            var phase = (dT / flashPeriod) % 1;
            spriteController.Color = phase < 0.5f ? flashColorA : flashColorB;
        }
        else
        {
            spriteController.Color = Color.white;
        }

        if (!PauseManager.Paused)
        {
            UpdatePerception();

            UpdateSpeechBubble();

            UpdateLocomotion();

            EnsureCharacterInitialized();

            HandleEvents();

            MaybeDoNextAction();
        }
    }

    internal void OnCollisionEnter2D(Collision2D collision)
    {
        QueueEvent("collision", collision.gameObject);
    }
    #endregion

    #region Perception update

    private void UpdatePerception()
    {
        UpdateLocations();

        UpdateSpace(
            conversationalSpaceColliders,
            ConversationalSpaceRadius,
            conversationalSpace,
            "enter_conversational_space",
            "exit_conversational_space",
            true);
        UpdateSpace(
            socialSpaceColliders,
            SocialSpaceRadius,
            socialSpace,
            "enter_social_space",
            "exit_social_space",
            false);
    }

    readonly Collider2D[] conversationalSpaceColliders = new Collider2D[MaxConversationalSpaceColliders];
    readonly Collider2D[] socialSpaceColliders = new Collider2D[MaxConversationalSpaceColliders];

    // ReSharper disable once InconsistentNaming
    private readonly Symbol SNobodySpeaking = Symbol.Intern("nobody_speaking");
    
    /// <summary>
    /// Update the set of character's within this characters' conversational space
    /// and generate any necessary enter/leave events.
    /// </summary>
    private void UpdateSpace(Collider2D[] colliders, float radius, ELNode statusNode, string enterEvent, string exitEvent, bool updateNobodySpeaking)
    {
        var characterCount = Physics2D.OverlapCircleNonAlloc(
            transform.position,
            radius,
            colliders,
            1 << gameObject.layer);
        if (characterCount==MaxConversationalSpaceColliders)
            throw new Exception("Too many colliders in conversational space!");

        // Clean out entries that are no longer in the area
        statusNode.DeleteAll(
            node =>
            {
                // Look to see if node's key (a character) appears in the colliders
                for (var i = 0; i<characterCount;i++)
                    if (ReferenceEquals(node.Key, colliders[i].gameObject))
                        return false;
                // It doesn't, so the character left this character's conversational space.

                // Tell this character about it
                QueueEvent(exitEvent, node.Key);

                // Remove the character
                return true;
            });

        // Add new entries
        for (var i = 0; i<characterCount;i++)
        {
            var character = colliders[i].gameObject;
            if (character != gameObject && !statusNode.ContainsKey(character) && myCurrentRoom != null && myCurrentRoom.Contains(character))
            {
                // The character just arrived in this character's conversational space

                // Tell this character
                QueueEvent(enterEvent, character);

                // Update the KB
                ELNode.Store(statusNode/character);
            }
        }

        if (updateNobodySpeaking)
        {
            bool nobodySpeaking = true;
            for (var i = 0; i<characterCount; i++)
                if (colliders[i].GetComponent<SimController>().IsSpeaking)
                    nobodySpeaking = false;
            if (currentlySpeaking)
                nobodySpeaking = false;
            if (nobodySpeaking)
                ELNode.Store(perceptionRoot/SNobodySpeaking);
            else
            {
                if (perceptionRoot.ContainsKey(SNobodySpeaking))
                {
                    perceptionRoot.DeleteKey(SNobodySpeaking);
                    pollActions = true;
                }
            }
        }
    }

    private readonly Symbol brainwashedSymbol = Symbol.Intern("brainwashed");

    private void UpdateLocations()
    {
        // TODO: FIX THIS SO THAT DESTROYED OBJECTS GET THEIR ENTRIES GC'ed.
        foreach (var p in Registry<PhysicalObject>())
        {
            var o = p.gameObject;
            var n = locationRoot.ChildWithKey(o);
            // Don't do anything if /perception/location/O:Location:override in database,
            // which would mean the character is brainwashed about O's position.
            if (n == null || n.Children.Count == 0 || !n.Children[0].ContainsKey(brainwashedSymbol))
            {
                if (!p.Exists)
                    // Dead; remove it.
                    locationRoot.DeleteKey(o);
                // Determine if it's inside something
                else if (p.Container == null)
                {
                    // It's not inside another object, so find what room it's in.

                    if (n == null || !n.ExclusiveKeyValue<GameObject>().GetComponent<Room>().Contains(o))
                    {
                        var r = TileMap.TheTileMap.TileRoom(o);
                        var location = r!=null?(r.gameObject):null;
                        if (location == null && p is Door)
                            location = ((Door) p).ForceRoom;
                        ELNode.Store(locationRoot / o % location);
                        if (o == gameObject)
                        {
                            myCurrentRoom = r;
                        }
                    }
                }
                else
                {
                    if (!p.IsHidden)
                        ELNode.Store((locationRoot / o % p.Container));
                }
            }
        }
    }
    #endregion

    #region Primitive actions handled by SimController

    private bool pollActions;
    private void MaybeDoNextAction()
    {
        if (pollActions || !pauseUntil.HasValue || pauseUntil.Value <= Time.time)
        {
            pollActions = false;
            pauseUntil = null;
            DoNextAction();
            DecisionCycleCount++;
        }
    }

    public int DecisionCycleCount;

    public int DecisionCycleAlloc;
    void DoNextAction()
    {
        var actionVar = new LogicVariable("Action");

        var beforeBytes = GC.GetTotalMemory(false);
        var action = this.SolveFor(actionVar, new Structure(SNextAction, actionVar));
        var allocBytes = GC.GetTotalMemory(false) - beforeBytes;
        if (allocBytes > 0)
            DecisionCycleAlloc = (int)allocBytes;
        InitiateAction(action);
    }

    private static readonly Symbol SNextAction = Symbol.Intern("next_action");

    private static readonly Symbol SWalkingTo = Symbol.Intern("walking_to");

    private static readonly Symbol SLastAction = Symbol.Intern("last_action");

    private void InitiateAction(object action)
    {
        if (action == null)
            return;

        var actionCopy = Term.CopyInstantiation(action);
        ELNode.Store(motorRoot / SLastAction % actionCopy);

        var structure = action as Structure;
        if (structure != null)
        {
            switch (structure.Functor.Name)
            {
                case "face":
                    Face(structure.Argument<GameObject>(0));
                    break;

                case "say":
                    // Say a fixed string
                    Say(structure.Argument<string>(0), gameObject);
                    break;

                case "cons":
                    // It's a list of actions to initiate.
                    InitiateAction(structure.Argument(0));
                    InitiateAction(structure.Argument(1));
                    break;

                case "pause":
                    pauseUntil = Time.time + Convert.ToSingle(structure.Argument(0));
                    break;

                case "pickup":
                {
                    var patient = structure.Argument<GameObject>(0);
                    if (patient == gameObject)
                        throw new InvalidOperationException(name+": tried to pickup() self!");
                    if (patient == gameObject)
                        return;
                    if (patient == null)
                        throw new NullReferenceException("Argument to pickup is not a gameobject");
                    var physob = patient.GetComponent<PhysicalObject>();
                    if (physob == null)
                        throw new NullReferenceException("Argument to pickup is not a physical object.");
                    physob.MoveTo(gameObject);
                    break;
                }

                case "ingest":
                {
                    var patient = structure.Argument<GameObject>(0);
                    if (patient == null)
                        throw new NullReferenceException("Argument to ingest is not a gameobject");
                    var physob = patient.GetComponent<PhysicalObject>();
                    if (physob == null)
                        throw new NullReferenceException("Argument to ingest is not a physical object.");
                    physob.Destroy();
                    // TODO: FIX LOCATION UPDATE SO WE DON'T NEED TO KLUGE THIS
                    locationRoot.DeleteKey(patient);
                    var propinfo = patient.GetComponent<PropInfo>();
                    if (propinfo != null)
                    {
                        if (propinfo.IsFood)
                            physiologicalStates.DeleteKey(Symbol.Intern("hungry"));
                        if (propinfo.IsBeverage)
                            physiologicalStates.DeleteKey(Symbol.Intern("thirsty"));
                    }
                    break;
                }

                case "putdown":
                {
                    var patient = structure.Argument<GameObject>(0);
                    if (patient == gameObject)
                        throw new InvalidOperationException(name+": tried to putdown() self!");
                    if (patient == null)
                        throw new NullReferenceException("Argument to putdown is not a gameobject");
                    var physob = patient.GetComponent<PhysicalObject>();
                    if (physob == null)
                        throw new NullReferenceException("Argument to putdown is not a physical object.");
                    var dest = structure.Argument<GameObject>(1);
                    if (dest == null)
                        throw new NullReferenceException("Argument to putdown is not a gameobject");
                    physob.MoveTo(dest);
                    break;
                }

                case "flash":
                {
                    flashColorA = structure.Argument<Color>(0);
                    flashColorB = structure.Argument<Color>(1);
                    flashPeriod = Convert.ToSingle(structure.Argument(2));
                    flashStartTime = Time.time;
                    flashEndTime = flashStartTime + Convert.ToSingle(structure.Argument(3));
                    break;
                }

                case "get_in":
                    GetIn(structure.Argument<GameObject>(0));
                    break;

                case "dismount":
                    Dismount();
                    break;

                case "end_game":
                    Application.Quit();
                    break;

                default:
                    // Assume it's dialog
                    this.IsTrue("log_dialog_act", structure);
                    GameObject thisAddressee = 
                        ((structure.Arity >= 2) ? 
                            structure.Argument(1) as GameObject
                            : gameObject) ?? gameObject;
                    var talkingToSelf = thisAddressee == gameObject;
                    if (!talkingToSelf || ShowMentalMonologue)
                    {
                        var textVar = new LogicVariable("DialogText");
                        object text = null;
                        try
                        {
                            text = gameObject.SolveFor(textVar, "generate_text", structure, textVar);
                        }
                        catch (Exception e)
                        {
                            Debug.LogError(string.Format("Exception while generating text for {0}", gameObject.name));
                            Debug.LogException(e);
                        }
                        var textString = text as string;
                        if (textString == null)
                            throw new Exception(
                                "generate_text returned " + ISOPrologWriter.WriteToString(text) + " for "
                                + ISOPrologWriter.WriteToString(structure));
                        var talkingToPlayer = structure.Arity >= 2
                                              && ReferenceEquals(structure.Argument(1), playerSymbol);
                        SetSpeechTimeout(textString);
                        if (talkingToPlayer)
                            // Character is talking to zhimself
                        {
                            if (nlPrompt != null)
                                nlPrompt.OutputToPlayer(textString);
                            else
                                Say(string.Format("({0})", textString), thisAddressee);
                        }
                        else
                            Say(textString, thisAddressee);

                        if (!talkingToPlayer && !talkingToSelf)
                        {
                            // Tell the other characters
                            foreach (var node in socialSpace.Children)
                            {
                                var character = (GameObject)(node.Key);
                                if (character != gameObject)
                                    character.QueueEvent((Structure)Term.CopyInstantiation(structure));
                            }
                            // TODO: fix it so that when characters appear, the system computes their social
                            // spaces from scratch.  Then we won't need this kluge.
                            if (!socialSpace.ContainsKey(thisAddressee))
                                thisAddressee.QueueEvent((Structure)Term.CopyInstantiation(structure));
                        }
                    }
                    break;
            }
            if (structure.Functor.Name != "pause")
                // Report back to the character that the action has occurred.
                QueueEvent(structure);
        }
        else
            throw new InvalidOperationException("Unknown action: " + ISOPrologWriter.WriteToString(action));
    }

    private void UpdateSpeechBubble()
    {
        // Clear speech bubble if it's time.
        if (currentlySpeaking && Time.time > clearSpeechTime)
        {
            currentSpeechBubbleText = null;
            currentlySpeaking = false;
            motorRoot.DeleteKey(SIAmSpeaking);
        }
    }

    public void GetIn(GameObject target)
    {
        var prop = target.GetComponent<PropInfo>();
        if (prop == null)
        {
            Debug.Log(name+": attempted to get in the non-prop "+target);
            return;
        }
        if (prop.LayAnimation == null)
        {
            Debug.Log(name + ": attempted to get in non-layable prop "+target);
            return;
        }
        GetComponent<PhysicalObject>().MoveTo(target);
	if (prop.ContentsVisible)
            spriteController.StartIdleAnimation(prop.LayAnimation);
    }

    /// <summary>
    /// Remove the character from their current physical container, and return them to its
    /// docking region in the enclosing room.
    /// 
    /// Internally, this means reparenting the character's gameobject back to the Characters
    /// gameobject and setting its position to the center of the original container's
    /// docking region.
    /// </summary>
    public void Dismount()
    {
        var container = gameObject.transform.parent.gameObject;
        if (container.GetComponent<PhysicalObject>() == null)
        {
            // we're not in a container; do nothing
            return;
        }

        var dockingRegion = container.GetComponent<DockingRegion>();
        if (dockingRegion == null)
            throw new InvalidOperationException(
                string.Format(
                    "{0} is dismounting from object {1}, which has no docking region defined.",
                    name,
                    container.name));

        transform.parent = (GameObject.Find("Characters")??new GameObject("Characters")).transform;
        transform.position = dockingRegion.WorldDockingRect.center;
	    spriteController.enabled = true;
        spriteController.Visible = true;
    }

    /// <summary>
    /// Turns character to face the specified GameObject
    /// </summary>
    /// <param name="target">Object to face</param>
    public void Face(GameObject target)
    {
        steering.Face(target.Position() - (Vector2)transform.position);
    }

    /// <summary>
    /// Displays the specified string.
    /// </summary>
    /// <param name="speech">String to display</param>
    /// <param name="thisAddressee">Who the speech is directed to.</param>
    public void Say(string speech, GameObject thisAddressee)
    {
        addressee = thisAddressee;
        currentSpeechBubbleText = speech;
    }

    private void SetSpeechTimeout(string speech)
    {
        currentlySpeaking = true;
        ELNode.Store(motorRoot / SIAmSpeaking);
        clearSpeechTime = Time.time
                          + Math.Max(
                              SpeechDelayMinimum,
                              Math.Min(SpeechDelayMaximum, speech.Length * SpeechDelaySecondsPerChar));
    }

    // ReSharper disable once InconsistentNaming
    private static readonly Symbol SIAmSpeaking = Symbol.Intern("i_am_speaking");
    #endregion

    #region Locomotion control
    private static readonly Symbol SDockedWith = Symbol.Intern("docked_with");
    private void UpdateLocomotion()
    {
        UpdateLocomotionBidsAndPath();

        if (CurrentlyDockedWith != null && !CurrentlyDockedWith.DockingTiles().Contains(transform.position))
        {
            // We were docked with an object, but are not anymore.
            perceptionRoot.DeleteKey(SDockedWith);
            CurrentlyDockedWith = null;
        }

        if (currentPath != null)
        {
            // Update the steering
            if (currentPath.UpdateSteering(steering)
                || (Vector2.Distance(transform.position, currentDestination.transform.position) < 0.75
                     && currentDestination.IsCharacter()))
            {
                // Finished the path
                CurrentlyDockedWith = CurrentDestination;
                ELNode.Store(perceptionRoot/SDockedWith%CurrentlyDockedWith);
                ELNode.Store(lastDestination % CurrentDestination);
                currentPath = null;
                currentDestination = null;
                (motorRoot / SWalkingTo).DeleteSelf();
                Face(CurrentlyDockedWith);
                steering.Stop();
                QueueEvent("arrived_at", CurrentlyDockedWith);
            }
        }
    }

    readonly Dictionary<GameObject, float> bidTotals = new Dictionary<GameObject, float>();

    // ReSharper disable once InconsistentNaming
    private readonly Symbol SConcerns = Symbol.Intern("concerns");

    void UpdateLocomotionBidsAndPath()
    {
        //foreach (var pair in bidTotals)
        //    bidTotals[pair.Key] = 0;
        bidTotals.Clear();
        elRoot.WalkTree(SConcerns, updateConcernBids);

        GameObject winner = null;
        float winningBid = 0;
        foreach (var pair in bidTotals)
            if (pair.Value > winningBid)
            {
                winningBid = pair.Value;
                winner = pair.Key;
            }

        if (winner != null)
        {
            // Replan if destination has changed or if destination has moved away from current path.
            var newDestination = (winner != CurrentDestination && winner != CurrentlyDockedWith);
            if (newDestination
                || (currentDestination != null && currentPath == null)
                || (currentDestination != null && currentPath != null && !CurrentDestination.DockingTiles().Contains(currentPath.FinalTile)))
            {
                if (newDestination)
                {
                    Dismount();
                    ELNode.Store(eventHistory / new Structure("goto", winner)); // Log change for debugging purposes.
                }
                CurrentDestination = winner;
                if (currentDestination.DockingTiles().Contains(gameObject.TilePosition()))
                {
                    QueueEvent("arrived_at", currentDestination);
                }
                else
                    currentPath = planner.Plan(gameObject.TilePosition(), CurrentDestination.DockingTiles());
            }
        }
    }

    private Action<ELNode> updateConcernBids;
    private static readonly Symbol SLocationBids = Symbol.Intern("location_bids");
    void UpdateConcernBids(ELNode concern)
    {
        // Make sure this isn't the EL root (it's not an actual concern node).
        if (concern.Key != null)
        {
            ELNode bids;
            if (concern.TryLookup(SLocationBids, out bids))
            {
                // Add its bids in
                foreach (var bid in bids.Children)
                {
                    var destination = bid.Key as GameObject;
                    if (destination == null)
                        throw new Exception("Location bid is not a GameObject: "+bid.Key);
                    var bidValue = Convert.ToSingle(bid.ExclusiveKeyValue<object>());
                    if (bidTotals.ContainsKey(destination))
                        bidTotals[destination] += bidValue;
                    else
                        bidTotals[destination] = bidValue;
                }
            }
        }
    }
    #endregion

    #region GUI and speech bubbles

    public GUIStyle SpeechBubbleStyle;

    /// <summary>
    /// Character width, in tiles
    /// </summary>
    private const int CharacterWidth = 1;

    /// <summary>
    /// Character higher, in tiles.
    /// </summary>
    private const int CharacterHeight = 2;

    internal void OnGUI()
    {
        var guiScreenPosition = gameObject.GUIScreenPosition();

        var statusText = GetStatusText();
        if (!IsHidden && !string.IsNullOrEmpty(statusText))
        {
            var status = new GUIContent(statusText);
            var style = GUIStyle.none;
            style.normal.textColor = Color.yellow;
            var size = style.CalcSize(status);
            var statusRect = new Rect(guiScreenPosition.x - 0.5f*size.x,
                guiScreenPosition.y - 0.9f*CharacterHeight*Tile.TileSizeInPixels, size.x, size.y);
            GUI.Box(statusRect, GreyOutTexture);
            GUI.Label(statusRect, status, style);
        }

        if (Camera.current != null && !string.IsNullOrEmpty(currentSpeechBubbleText))
        {
            var bubblelocation = guiScreenPosition;
            var addresseeOffset = addressee.transform.position - transform.position;

            if (addresseeOffset.x > 0 || addresseeOffset.y < 0)
                bubblelocation.y -= (CharacterHeight + 0.5f)*Tile.TileSizeInPixels;
            var size = SpeechBubbleStyle.CalcSize(new GUIContent(currentSpeechBubbleText));
            bubblelocation.x += (CharacterWidth*0.5f)*Tile.TileSizeInPixels;
            var bubbleRect = new Rect(bubblelocation.x, bubblelocation.y, size.x, size.y);

            // Handle rects that overshoot the map
            var overshoot = bubbleRect.xMax - Screen.width;
            if (overshoot > 0)
            {
                bubbleRect.xMin -= overshoot;
                bubbleRect.xMax -= overshoot;
                // Extend the background rectangle if need be
                if (bubbleRect.xMin < 0)
                {
                    bubbleRect.xMin = 0;
                    bubbleRect.yMin -= size.y;
                }
            }

            var vovershoot = bubbleRect.yMax - Screen.height;
            if (vovershoot > 0)
                bubbleRect.yMin -= vovershoot;

            GUI.Box(bubbleRect, GreyOutTexture);
            GUI.Label(bubbleRect, currentSpeechBubbleText, SpeechBubbleStyle);
        }
    }

    string maxBidStatus;
    float maxStatusBid;
    private Action<ELNode> updateStatusTextBids;
    private string GetStatusText()
    {
        maxStatusBid = -1;
        maxBidStatus = "";
        elRoot.WalkTree(SConcerns, updateStatusTextBids);
        return maxBidStatus;
    }

    private readonly Symbol SStatusText = Symbol.Intern("status_text");
    private void UpdateStatusTextBids(ELNode concern)
    {
        var textRoot = concern.ChildWithKey(SStatusText);
        if (textRoot != null && textRoot.Children.Count > 0)
        {
            var text = textRoot.Children[0];
            if (text.Children.Count > 0)
            {
                var bid = Convert.ToSingle(text.Children[0].Key);
                if (bid > maxStatusBid)
                {
                    maxStatusBid = bid;
                    maxBidStatus = text.Key.ToString();
                }
            }
        }
    }
    #endregion

    #region Sonification
    public void EmitGrain(string patchName, int ms)
    {
        if (sonifier != null)
            sonifier.EmitGrain(patchName, ms);
    }
    #endregion

    #region PhysicalObject methods
    public override void Destroy()
    {
        tileMap.SetTileColor(gameObject.DockingTiles(), Color.red);

        var deathEvent = new Structure("die", gameObject);
        foreach (var character in Registry<SimController>())
            character.QueueEvent(deathEvent);

        base.Destroy();

        steering.CharacterDead();
        transform.position = new Vector3(-9999999999, -9999999999, 0);
    }

    #endregion
}
