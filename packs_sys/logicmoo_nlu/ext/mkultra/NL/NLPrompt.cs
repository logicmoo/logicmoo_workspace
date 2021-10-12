using Prolog;
using UnityEngine;

// ReSharper disable once InconsistentNaming
public class NLPrompt : BindingBehaviour
{
    #region Public fields editable within the editor
    /// <summary>
    /// Location where user input is displayed
    /// </summary>
    public Rect InputRect = new Rect(0, 0, 1000, 100);
    /// <summary>
    /// Location where the decoded dialog act is displayed
    /// </summary>
    public Rect CommentaryRect = new Rect(0, 100, 1000, 100);
    /// <summary>
    /// Location where output from the character to the player is displayed
    /// </summary>
    public Rect ResponseRect = new Rect(0, 100, 1000, 100);

    /// <summary>
    /// Font, color, etc. for displaying the player's input.
    /// </summary>
    public GUIStyle InputGUIStyle = new GUIStyle();
    /// <summary>
    /// Font, color, etc. for displaying the decoded dialog act
    /// </summary>
    public GUIStyle CommentaryGUIStyle = new GUIStyle();
    #endregion

    #region Private fields
    /// <summary>
    /// Database node for storing when the user last typed.
    /// </summary>
    private ELNode lastPlayerActivity;

    /// <summary>
    /// What the user has typed so far.
    /// </summary>
    private string input = "";
    /// <summary>
    /// Text that completes the input to be a valid utterance of the grammar (if any)
    /// </summary>
    private string completion = "";
    /// <summary>
    /// Text describing the decoded dialog act
    /// </summary>
    private string commentary = "";
    /// <summary>
    /// Combined input+completion with colorization
    /// </summary>
    private string formatted = "";
    /// <summary>
    /// The dialog act as a Prolog term.
    /// </summary>
    private object dialogAct;
    /// <summary>
    /// Output form player character to player, if any.
    /// </summary>
    private string characterResponse = "";

    [Bind]
#pragma warning disable 649
    private SimController simController;
#pragma warning restore 649
    #endregion

    internal void Start()
    {
        this.lastPlayerActivity = KnowledgeBase.Global.ELRoot.StoreNonExclusive(Symbol.Intern("last_player_activity"));
        this.lastPlayerActivity.StoreExclusive(-1, true);
    }

    /// <summary>
    /// Display output from the player character to the player.
    /// </summary>
    /// <param name="formattedText">Text to display</param>
    public void OutputToPlayer(string formattedText)
    {
        characterResponse = formattedText;
    }

    internal void OnGUI()
    {
        GUI.depth = 0;
        var e = Event.current;
        switch (e.type)
        {
            case EventType.KeyDown:
                if (GUI.GetNameOfFocusedControl()=="")
                {
                    this.HandleKeyDown(e);
                    this.TryCompletionIfCompleteWord();
                }
                break;

            case EventType.Repaint:
                GUI.Label(InputRect, formatted, InputGUIStyle);
                GUI.Label(CommentaryRect, commentary, CommentaryGUIStyle);
                GUI.Label(ResponseRect, characterResponse, InputGUIStyle);
                break;
        }
    }

    private void HandleKeyDown(Event e)
    {
        if (e.keyCode != KeyCode.None)
        {
            if (e.alt || e.control || (e.keyCode >= KeyCode.F1 && e.keyCode <= KeyCode.F15))
            {
                object key = Symbol.Intern(e.keyCode.ToString().ToLower());
                if (e.alt)
                {
                    if (e.control)
                        key = new Structure(
                            "-",
                            new Structure("-", Symbol.Intern("control"), Symbol.Intern("alt")),
                            key);
                    else
                        key = new Structure("-", Symbol.Intern("alt"), key);
                }
                else if (e.control)
                    key = new Structure("-", Symbol.Intern("control"), key);

                KnowledgeBase.Global.IsTrue(new Structure("fkey_command", key));
                return;
            }

            // Update last user activity time
            this.lastPlayerActivity.StoreExclusive(Time.time, true);

            switch (e.keyCode)
            {
                case KeyCode.Escape:
                    this.input = this.formatted = this.commentary = "";
                    this.dialogAct = null;
                    PauseManager.Paused = false;
                    break;

                case KeyCode.Delete:
                case KeyCode.Backspace:
                    if (this.input != "")
                    {
                        this.formatted = this.input = this.input.Substring(0, this.input.Length - 1);
                        this.TryCompletionIfCompleteWord();
                    }
                    break;

                case KeyCode.Tab:
                    this.input = string.Format(
                        "{0}{1}{2}",
                        this.input,
                        (this.input.EndsWith(" ")
                         || (!string.IsNullOrEmpty(completion) && !char.IsLetterOrDigit(completion[0])))
                            ? ""
                            : " ",
                        this.completion);
                    break;

                case KeyCode.Return:
                case KeyCode.KeypadEnter:
                    if (this.dialogAct != null)
                    {
                        simController.QueueEvent("player_input", dialogAct);
                        this.IsTrue("log_dialog_act", dialogAct);
                        this.formatted = this.input = this.completion = this.commentary = "";
                        this.dialogAct = null;
                        PauseManager.Paused = false;
                    }
                    Event.current.Use();
                    break;
            }
        }

        if (e.character > 0 && !e.alt && !e.control && e.character >= ' ')
        {
            this.AddToInput(e.character);
        }
    }

    private void AddToInput(char c)
    {
        if (c == '\n')
            return;

        PauseManager.Paused = true;
        characterResponse = "";
        if (c != ' ' || (this.input != "" && !this.input.EndsWith(" "))) // don't let them type redundant spaces
            this.input = this.input + c;

        this.TryCompletionIfCompleteWord();
    }

    /// <summary>
    /// True if the input ends with a character that can't be part of a word.
    /// </summary>
    bool InputEndsWithCompleteWord
    {
        get
        {
            if (input == "")
                return false;
            if (!input.EndsWith("'")
                && !char.IsLetterOrDigit(input[input.Length-1]))
                return true;
            // Check for cases like "who're"
            foreach (var e in Prolog.Prolog.EnglishEnclitics)
            {
                if (input.EndsWith(e) && input.Length > e.Length && input[input.Length - 1 - e.Length] == '\'')
                    return true;
            }
            return false;
        }
    }

    private void TryCompletionIfCompleteWord()
    {
        this.formatted = null;
        if (InputEndsWithCompleteWord)
            this.TryCompletion();
        else
        {
            var lastSpace = this.input.LastIndexOf(' ');
            var lastWord = lastSpace < 0 ? this.input : this.input.Substring(lastSpace + 1);
            lastWord = lastWord.Trim('(', ')', '.', ',', '?', '!', ';', ':', '\'', '"');

            if (Prolog.Prolog.IsLexicalItem(lastWord))
            {
                this.TryCompletion();
            }
        }

        if (this.formatted == null)
        {
            this.formatted = this.input;
            this.dialogAct = null;
        }
    }

    private void TryCompletion()
    {
        var completionVar = new LogicVariable("Output");
        var dialogActVar = new LogicVariable("DialogAct");
        bool completionSuccess = false;
        try
        {
            completionSuccess = this.IsTrue("input_completion", this.input, completionVar, dialogActVar);
        }
        catch (InferenceStepsExceededException e)
        {
            Debug.LogError("Completion took too many steps for input: "+this.input);
            Debug.LogException(e);
        }
        if (completionSuccess)
        {
            this.completion = (string)completionVar.Value;
            this.dialogAct = Term.CopyInstantiation(dialogActVar.Value);
            if (this.IsTrue("well_formed_dialog_act", this.dialogAct))
            {
                this.formatted = this.completion == "" ?
                    string.Format("<b><color=lime>{0}</color></b>", this.input)
                    : string.Format("<color=lime>{0}{1}<i>{2}</i></color>",
                                    this.input,
                                    ( this.input.EndsWith(" ") || this.input.EndsWith("'") 
                                      || !char.IsLetterOrDigit(this.completion[0])) 
                                    ? "" : " ",
                                    this.completion);
                var da = this.dialogAct as Structure;
                if (da != null && da.Arity > 1)
                {
                    var a = da.Argument<GameObject>(1);
                    this.commentary = string.Format("{0} to {1}\n{2}", da.Functor, (a == this) ? "myself" : a.name,
                                                    ISOPrologWriter.WriteToString(dialogActVar.Value));
                }
                else
                {
                    this.commentary = ISOPrologWriter.WriteToString(dialogActVar.Value);
                }
            }
            else
            {
                // Input is grammatical but not well formed.
                this.formatted = this.completion == "" ?
                    string.Format("<b><color=yellow>{0}</color></b>", this.input)
                    : string.Format("<color=yellow>{0}{1}</color><color=grey><i>{2}</i></color>",
                                    this.input,
                                    (this.input.EndsWith(" ") || !char.IsLetterOrDigit(this.completion[0])) ? "" : " ",
                                    this.completion);
                if (this.completion == "")
                    this.commentary = string.Format(
                        "This input is grammatical, but doesn't make sense to me\n{0}",
                        ISOPrologWriter.WriteToString(dialogActVar.Value));
                else
                {
                    this.commentary = "This is grammatical but nonsensical\n" + ISOPrologWriter.WriteToString(dialogActVar.Value);
                }
            }

        }
        else
        {
            this.formatted = string.Format("<color=red>{0}</color>", this.input);
            this.commentary = "Sorry; I don't understand any sentences beginning with those words.";
        }
    }
}
