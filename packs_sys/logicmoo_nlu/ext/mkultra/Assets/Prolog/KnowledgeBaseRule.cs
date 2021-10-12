using System;
using System.Collections.Generic;

using Debug=UnityEngine.Debug;

namespace Prolog
{
    /// <summary>
    /// Base class of Horn-clause rules.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "KnowledgeBase")]
    public abstract class KnowledgeBaseRule : KnowledgeBaseEntry
    {
        /// <summary>
        /// Creates a KnowledgedBaseRule given a Term object for a :- expression.
        /// </summary>
        public static KnowledgeBaseRule FromTerm(Structure structure, bool checkSingletons, string source, int line)
        {
            if (structure == null) throw new ArgumentNullException("structure");
            if (structure.IsFunctor(Symbol.Implication, 2))
            {
                var body = new List<Structure>();
                UnwindCommaExpression(structure.Argument(1), body);
                if (structure.Argument(0) == null)
                    throw new ArgumentException("head of rule is null");
                Structure head = Term.Structurify(structure.Argument(0), "Head of :- must be a valid proposition or predicate.");
                if (head == null)
                    throw new ArgumentException("Head of rule is not a term.");
                return MakeRule(head, body, checkSingletons, source, line);
            }
            return MakeRule(structure, null, checkSingletons, source, line);
        }

        internal static void UnwindCommaExpression(object subgoal, List<Structure> body)
        {
            if (subgoal == null) throw new ArgumentNullException("subgoal", "Subgoal of rule is null");
            Structure structure;
            if (subgoal is LogicVariable)
                structure = new Structure(Symbol.Call, subgoal);
            else
                structure = Term.Structurify(subgoal, "Not a valid subgoal.");
            if (structure.IsFunctor(Symbol.Comma, 2))
            {
                UnwindCommaExpression(structure.Argument(0), body);
                UnwindCommaExpression(structure.Argument(1), body);
            }
            else
                body.Add(structure);
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1305:SpecifyIFormatProvider", MessageId = "System.String.Format(System.String,System.Object)")]
        static KnowledgeBaseRule MakeRule(Structure head, List<Structure> body, bool checkSingletons, string source, int line)
        {
            if (body == null)
                return new KnowledgeBaseRule0(head, new Structure[0], checkSingletons, source, line);
            switch (body.Count)
            {
                case 0:
                    return new KnowledgeBaseRule0(head, body.ToArray(), checkSingletons, source, line);

                case 1:
                    return new KnowledgeBaseRule1(head, body.ToArray(), checkSingletons, source, line);

                case 2:
                    return new KnowledgeBaseRule2(head, body.ToArray(), checkSingletons, source, line);

                case 3:
                    return new KnowledgeBaseRule3(head, body.ToArray(), checkSingletons, source, line);

                case 4:
                    return new KnowledgeBaseRule4(head, body.ToArray(), checkSingletons, source, line);

                case 5:
                    return new KnowledgeBaseRule5(head, body.ToArray(), checkSingletons, source, line);

                case 6:
                    return new KnowledgeBaseRule6(head, body.ToArray(), checkSingletons, source, line);

                case 7:
                    return new KnowledgeBaseRule7(head, body.ToArray(), checkSingletons, source, line);

                case 8:
                    return new KnowledgeBaseRule8(head, body.ToArray(), checkSingletons, source, line);

                default:
                    throw new ArgumentException(string.Format("Rules with {0} clauses are not supported.", body.Count));
            }
        }

        /// <summary>
        /// Fills in fields given head and body terms.
        /// </summary>
        protected KnowledgeBaseRule(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
        {
            SourceFile = source;
            SourceLineNumber = line;
            head = ruleHead;
            if (ruleHead == null) throw new ArgumentNullException("ruleHead");
            HeadArgs = ruleHead.Arguments;
            headIndexers = PredicateArgumentIndexer.ArglistIndexers(HeadArgs);
            BodyGoals = ruleBody;
            FreeVariables = new List<LogicVariable>();
            var singletons = new List<LogicVariable>();
            foreach (var a in HeadArgs)
                FindVariables(a, singletons);
            foreach (var g in BodyGoals)
                FindVariables(g, singletons);
            if (checkSingletons && singletons.Count > 0)
            {
                foreach (var v in singletons)
                    PrintWarning("singleton variable: {0}", v.Name);
            }
            Functor = ruleHead.Functor;
        }

        void FindVariables(object obj, List<LogicVariable> singletons)
        {
            if (obj == null)
                return;
            var v = obj as LogicVariable;
            if (v != null)
            {
                bool inSingletons = singletons.Contains(v);
                
                if (FreeVariables.Contains(v))
                {
                    if (inSingletons)
                        singletons.Remove(v);
                }
                else
                {
                    FreeVariables.Add(v);
                    if (!v.Name.Name.StartsWith("_"))
                        singletons.Add(v);
                }
            }
            else
            {
                var t = obj as Structure;
                if (t != null)
                    foreach (var a in t.Arguments)
                        FindVariables(a, singletons);
            }
        }

        /// <summary>
        /// Functor of the head for this rule.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Functor")]
        protected Symbol Functor { get; private set; }

        /// <summary>
        /// The arglist of the head of this rule
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields")]
        internal object[] HeadArgs;
        /// <summary>
        /// The body (subgoals) of this rule
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields")]
        internal Structure[] BodyGoals;

        private readonly PredicateArgumentIndexer[] headIndexers;

        private readonly object head;

        /// <summary>
        /// Returns the head of this rule.
        /// </summary>
        public override object Head
        {
            get { return head; }
        }

        /// <summary>
        /// Returns the body of this rule.
        /// </summary>
        public override object Body
        {
            get
            {
                if (BodyGoals.Length == 0)
                    return Symbol.True;
                object body = BodyGoals[BodyGoals.Length-1];
                for (int i = BodyGoals.Length - 2; i >= 0; i--)
                {
                    object goal = (BodyGoals[i].Arguments.Length == 0) ? (object)BodyGoals[i].Functor : BodyGoals[i];
                    body = new Structure(Symbol.Comma, goal, body);
                }
                return body;
            }
        }

        /// <summary>
        /// The free variables of this rule.
        /// This is in fact all the variables appearing in the head and/or body.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1002:DoNotExposeGenericLists")]
        protected List<LogicVariable> FreeVariables;

        public override bool Prematch(PredicateArgumentIndexer[] argIndexers)
        {
            return PredicateArgumentIndexer.PotentiallyMatchable(argIndexers, headIndexers);
        }

        internal void PrintWarning(string formatString, params object[] formatArgs)
        {
            //Repl.StartWarnings();
            Debug.LogException(
                new PrologWarning(string.Format(formatString, formatArgs),
                string.Format("{0} (at {1}:{2})\n", ISOPrologWriter.WriteToString(this.Head), this.SourceFile, this.SourceLineNumber))
                );
            //Console.Write(" in predicate {0}:{1}/{2}.", kb.Name, HeadFunctor, HeadArity);
            
        }

        public override string ToString()
        {
            return ISOPrologWriter.WriteToString(new Structure(":-", head, Body));
        }
    }

    sealed class KnowledgeBaseRule0 : KnowledgeBaseRule
    {
        public KnowledgeBaseRule0(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
            : base(ruleHead, ruleBody, checkSingletons, source, line)
        { }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "System.Console.WriteLine(System.String)"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "System.Console.WriteLine(System.String,System.Object)")]
        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            var newVars = new LogicVariable[FreeVariables.Count];
            object[] newArgs = Term.AlphaConvertArglist(HeadArgs, FreeVariables, newVars, context, true);
            // ReSharper disable UnusedVariable
#pragma warning disable 414, 168, 219
            foreach (bool ignore in Term.UnifyArrays(args, newArgs))
#pragma warning restore 414, 168, 219
                // ReSharper restore UnusedVariable
                yield return CutState.Continue;
        }
    }

    sealed class KnowledgeBaseRule1 : KnowledgeBaseRule
    {
        public KnowledgeBaseRule1(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
            : base(ruleHead, ruleBody, checkSingletons, source, line)
        { }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "System.Console.WriteLine(System.String)"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "System.Console.WriteLine(System.String,System.Object)")]
        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            object[] goal1Args = null;
            var newVars = new LogicVariable[FreeVariables.Count];
            object[] newArgs = Term.AlphaConvertArglist(HeadArgs, FreeVariables, newVars, context, true);
            // ReSharper disable UnusedVariable
#pragma warning disable 414, 168, 219
            foreach (bool ignore in Term.UnifyArraysFast(args, newArgs, context))
#pragma warning restore 414, 168, 219
            {
                if (goal1Args == null)
                    goal1Args = Term.AlphaConvertArglist(BodyGoals[0].Arguments, FreeVariables, newVars, context, false);

#pragma warning disable 414, 168, 219
                foreach (CutState ignoreFreeze in context.ProveAllWokenGoals())
#pragma warning restore 414, 168, 219
                    // ReSharper restore UnusedVariable
                    foreach (CutState state1 in context.KnowledgeBase.Prove(BodyGoals[0].Functor, goal1Args, context, parentFrame))
                        yield return state1;
            }
        }
    }

    sealed class KnowledgeBaseRule2 : KnowledgeBaseRule
    {
        public KnowledgeBaseRule2(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
            : base(ruleHead, ruleBody, checkSingletons, source, line)
        { }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "System.Console.WriteLine(System.String)"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "System.Console.WriteLine(System.String,System.Object)")]
        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            object[] goal1Args = null;
            object[] goal2Args = null;
            var newVars = new LogicVariable[FreeVariables.Count];
            object[] newArgs = Term.AlphaConvertArglist(HeadArgs, FreeVariables, newVars, context, true);
            // ReSharper disable UnusedVariable
#pragma warning disable 414, 168, 219
            foreach (bool ignore in Term.UnifyArraysFast(args, newArgs, context))
#pragma warning restore 414, 168, 219
            {
                if (goal1Args == null)
                    goal1Args = Term.AlphaConvertArglist(BodyGoals[0].Arguments, FreeVariables, newVars, context, false);

#pragma warning disable 414, 168, 219
                foreach (CutState ignoreFreeze in context.ProveAllWokenGoals())
                    // ReSharper restore UnusedVariable
                    foreach (CutState state1 in context.KnowledgeBase.Prove(BodyGoals[0].Functor, goal1Args, context, parentFrame))
#pragma warning restore 414, 168, 219
                    {
                    if (state1 == CutState.ForceFail) yield return CutState.ForceFail;
                    if (goal2Args == null)
                        goal2Args = Term.AlphaConvertArglist(BodyGoals[1].Arguments, FreeVariables, newVars, context, false);

                    foreach (CutState state2 in context.KnowledgeBase.Prove(BodyGoals[1].Functor, goal2Args, context, parentFrame))
                        yield return state2;
                }
            }
        }
    }

    sealed class KnowledgeBaseRule3 : KnowledgeBaseRule
    {
        public KnowledgeBaseRule3(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
            : base(ruleHead, ruleBody, checkSingletons, source, line)
        { }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "System.Console.WriteLine(System.String)"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1303:Do not pass literals as localized parameters", MessageId = "System.Console.WriteLine(System.String,System.Object)")]
        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            object[] goal1Args = null;
            object[] goal2Args = null;
            object[] goal3Args = null;
            var newVars = new LogicVariable[FreeVariables.Count];
            object[] newArgs = Term.AlphaConvertArglist(HeadArgs, FreeVariables, newVars, context, true);
            // ReSharper disable UnusedVariable
#pragma warning disable 414, 168, 219
            foreach (bool ignore in Term.UnifyArraysFast(args, newArgs, context))
#pragma warning restore 414, 168, 219
            {
                if (goal1Args == null)
                    goal1Args = Term.AlphaConvertArglist(BodyGoals[0].Arguments, FreeVariables, newVars, context, false);

#pragma warning disable 414, 168, 219
                foreach (CutState ignoreFreeze in context.ProveAllWokenGoals())
                    // ReSharper restore UnusedVariable
                    foreach (CutState state1 in context.KnowledgeBase.Prove(BodyGoals[0].Functor, goal1Args, context, parentFrame))
#pragma warning restore 414, 168, 219
                    {
                    if (state1 == CutState.ForceFail) yield return CutState.ForceFail;
                    if (goal2Args == null)
                        goal2Args = Term.AlphaConvertArglist(BodyGoals[1].Arguments, FreeVariables, newVars, context, false);

#pragma warning disable 414, 168, 219
                    foreach (CutState state2 in context.KnowledgeBase.Prove(BodyGoals[1].Functor, goal2Args, context, parentFrame))
#pragma warning restore 414, 168, 219
                    {
                        if (state2 == CutState.ForceFail) yield return CutState.ForceFail;
                        if (goal3Args == null)
                            goal3Args = Term.AlphaConvertArglist(BodyGoals[2].Arguments, FreeVariables, newVars, context, false);

                        foreach (CutState state3 in context.KnowledgeBase.Prove(BodyGoals[2].Functor, goal3Args, context, parentFrame))
                            yield return state3;
                    }
                }
            }
        }
    }

    sealed class KnowledgeBaseRule4 : KnowledgeBaseRule
    {
        public KnowledgeBaseRule4(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
            : base(ruleHead, ruleBody, checkSingletons, source, line)
        { }

        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            object[] goal1Args = null;
            object[] goal2Args = null;
            object[] goal3Args = null;
            object[] goal4Args = null;
            var newVars = new LogicVariable[FreeVariables.Count];
            object[] newArgs = Term.AlphaConvertArglist(HeadArgs, FreeVariables, newVars, context, true);
            // ReSharper disable UnusedVariable
#pragma warning disable 414, 168, 219
            foreach (bool ignore in Term.UnifyArraysFast(args, newArgs, context))
            {
                if (goal1Args == null)
                    goal1Args = Term.AlphaConvertArglist(BodyGoals[0].Arguments, FreeVariables, newVars, context, false);

                foreach (CutState ignoreFreeze in context.ProveAllWokenGoals())
                    // ReSharper restore UnusedVariable
                    foreach (CutState state1 in context.KnowledgeBase.Prove(BodyGoals[0].Functor, goal1Args, context, parentFrame))
                {
                    if (state1 == CutState.ForceFail) yield return CutState.ForceFail;
                    if (goal2Args == null)
                        goal2Args = Term.AlphaConvertArglist(BodyGoals[1].Arguments, FreeVariables, newVars, context, false);

                    foreach (CutState state2 in context.KnowledgeBase.Prove(BodyGoals[1].Functor, goal2Args, context, parentFrame))
                    {
                        if (state2 == CutState.ForceFail) yield return CutState.ForceFail;
                        if (goal3Args == null)
                            goal3Args = Term.AlphaConvertArglist(BodyGoals[2].Arguments, FreeVariables, newVars, context, false);

                        foreach (CutState state3 in context.KnowledgeBase.Prove(BodyGoals[2].Functor, goal3Args, context, parentFrame))
                        {
                            if (state3 == CutState.ForceFail) yield return CutState.ForceFail;
                            if (goal4Args == null)
                                goal4Args = Term.AlphaConvertArglist(BodyGoals[3].Arguments, FreeVariables, newVars, context, false);

                            foreach (CutState state4 in context.KnowledgeBase.Prove(BodyGoals[3].Functor, goal4Args, context, parentFrame))
                            {
                                yield return state4;
#pragma warning disable 414, 168, 219
                            }
                        }
                    }
                }
            }
        }
    }

    sealed class KnowledgeBaseRule5 : KnowledgeBaseRule
    {
        public KnowledgeBaseRule5(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
            : base(ruleHead, ruleBody, checkSingletons, source, line)
        { }

        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            object[] goal1Args = null;
            object[] goal2Args = null;
            object[] goal3Args = null;
            object[] goal4Args = null;
            object[] goal5Args = null;
            var newVars = new LogicVariable[FreeVariables.Count];
            object[] newArgs = Term.AlphaConvertArglist(HeadArgs, FreeVariables, newVars, context, true);
// ReSharper disable UnusedVariable
            foreach (bool ignore in Term.UnifyArraysFast(args, newArgs, context))
            {
                if (goal1Args == null)
                    goal1Args = Term.AlphaConvertArglist(BodyGoals[0].Arguments, FreeVariables, newVars, context, false);

                foreach (CutState ignoreFreeze in context.ProveAllWokenGoals())
                    // ReSharper restore UnusedVariable
                foreach (CutState state1 in context.KnowledgeBase.Prove(BodyGoals[0].Functor, goal1Args, context, parentFrame))
                {
                    if (state1 == CutState.ForceFail) yield return CutState.ForceFail;
                    if (goal2Args == null)
                        goal2Args = Term.AlphaConvertArglist(BodyGoals[1].Arguments, FreeVariables, newVars, context, false);

                    foreach (CutState state2 in context.KnowledgeBase.Prove(BodyGoals[1].Functor, goal2Args, context, parentFrame))
                    {
                        if (state2 == CutState.ForceFail) yield return CutState.ForceFail;
                        if (goal3Args == null)
                            goal3Args = Term.AlphaConvertArglist(BodyGoals[2].Arguments, FreeVariables, newVars, context, false);

                        foreach (CutState state3 in context.KnowledgeBase.Prove(BodyGoals[2].Functor, goal3Args, context, parentFrame))
                        {
                            if (state3 == CutState.ForceFail) yield return CutState.ForceFail;
                            if (goal4Args == null)
                                goal4Args = Term.AlphaConvertArglist(BodyGoals[3].Arguments, FreeVariables, newVars, context, false);

                            foreach (CutState state4 in context.KnowledgeBase.Prove(BodyGoals[3].Functor, goal4Args, context, parentFrame))
                            {
                                if (state4 == CutState.ForceFail) yield return CutState.ForceFail;
                                if (goal5Args == null)
                                    goal5Args = Term.AlphaConvertArglist(BodyGoals[4].Arguments, FreeVariables, newVars, context, false);

                                foreach (CutState state5 in context.KnowledgeBase.Prove(BodyGoals[4].Functor, goal5Args, context, parentFrame))
                                    yield return state5;
                            }
                        }
                    }
                }
            }
        }
    }

    sealed class KnowledgeBaseRule6 : KnowledgeBaseRule
    {
        public KnowledgeBaseRule6(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
            : base(ruleHead, ruleBody, checkSingletons, source, line)
        { }

        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            object[] goal1Args = null;
            object[] goal2Args = null;
            object[] goal3Args = null;
            object[] goal4Args = null;
            object[] goal5Args = null;
            object[] goal6Args = null;
            var newVars = new LogicVariable[FreeVariables.Count];
            object[] newArgs = Term.AlphaConvertArglist(HeadArgs, FreeVariables, newVars, context, true);
// ReSharper disable UnusedVariable
            foreach (bool ignore in Term.UnifyArraysFast(args, newArgs, context))
            {
                if (goal1Args == null)
                    goal1Args = Term.AlphaConvertArglist(BodyGoals[0].Arguments, FreeVariables, newVars, context, false);

                foreach (CutState ignoreFreeze in context.ProveAllWokenGoals())
                    // ReSharper restore UnusedVariable
                    foreach (CutState state1 in context.KnowledgeBase.Prove(BodyGoals[0].Functor, goal1Args, context, parentFrame))
                {
                    if (state1 == CutState.ForceFail) yield return CutState.ForceFail;
                    if (goal2Args == null)
                        goal2Args = Term.AlphaConvertArglist(BodyGoals[1].Arguments, FreeVariables, newVars, context, false);

                    foreach (CutState state2 in context.KnowledgeBase.Prove(BodyGoals[1].Functor, goal2Args, context, parentFrame))
                    {
                        if (state2 == CutState.ForceFail) yield return CutState.ForceFail;
                        if (goal3Args == null)
                            goal3Args = Term.AlphaConvertArglist(BodyGoals[2].Arguments, FreeVariables, newVars, context, false);

                        foreach (CutState state3 in context.KnowledgeBase.Prove(BodyGoals[2].Functor, goal3Args, context, parentFrame))
                        {
                            if (state3 == CutState.ForceFail) yield return CutState.ForceFail;
                            if (goal4Args == null)
                                goal4Args = Term.AlphaConvertArglist(BodyGoals[3].Arguments, FreeVariables, newVars, context, false);

                            foreach (CutState state4 in context.KnowledgeBase.Prove(BodyGoals[3].Functor, goal4Args, context, parentFrame))
                            {
                                if (state4 == CutState.ForceFail) yield return CutState.ForceFail;
                                if (goal5Args == null)
                                    goal5Args = Term.AlphaConvertArglist(BodyGoals[4].Arguments, FreeVariables, newVars, context, false);

                                foreach (CutState state5 in context.KnowledgeBase.Prove(BodyGoals[4].Functor, goal5Args, context, parentFrame))
                                {
                                    if (state5 == CutState.ForceFail) yield return CutState.ForceFail;
                                    if (goal6Args == null)
                                        goal6Args = Term.AlphaConvertArglist(BodyGoals[5].Arguments, FreeVariables, newVars, context, false);

                                    foreach (CutState state6 in context.KnowledgeBase.Prove(BodyGoals[5].Functor, goal6Args, context, parentFrame))
                                        yield return state6;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    sealed class KnowledgeBaseRule7 : KnowledgeBaseRule
    {
        public KnowledgeBaseRule7(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
            : base(ruleHead, ruleBody, checkSingletons, source, line)
        { }

        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            object[] goal1Args = null;
            object[] goal2Args = null;
            object[] goal3Args = null;
            object[] goal4Args = null;
            object[] goal5Args = null;
            object[] goal6Args = null;
            object[] goal7Args = null;
            var newVars = new LogicVariable[FreeVariables.Count];
            object[] newArgs = Term.AlphaConvertArglist(HeadArgs, FreeVariables, newVars, context, true);
// ReSharper disable UnusedVariable
            foreach (bool ignore in Term.UnifyArraysFast(args, newArgs, context))
            {
                if (goal1Args == null)
                    goal1Args = Term.AlphaConvertArglist(BodyGoals[0].Arguments, FreeVariables, newVars, context, false);

                foreach (CutState ignoreFreeze in context.ProveAllWokenGoals())
                    // ReSharper restore UnusedVariable
                    foreach (CutState state1 in context.KnowledgeBase.Prove(BodyGoals[0].Functor, goal1Args, context, parentFrame))
                {
                    if (state1 == CutState.ForceFail) yield return CutState.ForceFail;
                    if (goal2Args == null)
                        goal2Args = Term.AlphaConvertArglist(BodyGoals[1].Arguments, FreeVariables, newVars, context, false);

                    foreach (CutState state2 in context.KnowledgeBase.Prove(BodyGoals[1].Functor, goal2Args, context, parentFrame))
                    {
                        if (state2 == CutState.ForceFail) yield return CutState.ForceFail;
                        if (goal3Args == null)
                            goal3Args = Term.AlphaConvertArglist(BodyGoals[2].Arguments, FreeVariables, newVars, context, false);

                        foreach (CutState state3 in context.KnowledgeBase.Prove(BodyGoals[2].Functor, goal3Args, context, parentFrame))
                        {
                            if (state3 == CutState.ForceFail) yield return CutState.ForceFail;
                            if (goal4Args == null)
                                goal4Args = Term.AlphaConvertArglist(BodyGoals[3].Arguments, FreeVariables, newVars, context, false);

                            foreach (CutState state4 in context.KnowledgeBase.Prove(BodyGoals[3].Functor, goal4Args, context, parentFrame))
                            {
                                if (state4 == CutState.ForceFail) yield return CutState.ForceFail;
                                if (goal5Args == null)
                                    goal5Args = Term.AlphaConvertArglist(BodyGoals[4].Arguments, FreeVariables, newVars, context, false);

                                foreach (CutState state5 in context.KnowledgeBase.Prove(BodyGoals[4].Functor, goal5Args, context, parentFrame))
                                {
                                    if (state5 == CutState.ForceFail) yield return CutState.ForceFail;
                                    if (goal6Args == null)
                                        goal6Args = Term.AlphaConvertArglist(BodyGoals[5].Arguments, FreeVariables, newVars, context, false);

                                    foreach (CutState state6 in context.KnowledgeBase.Prove(BodyGoals[5].Functor, goal6Args, context, parentFrame))
                                    {
                                        if (state6 == CutState.ForceFail) yield return CutState.ForceFail;
                                        if (goal7Args == null)
                                            goal7Args = Term.AlphaConvertArglist(BodyGoals[6].Arguments, FreeVariables, newVars, context, false);

                                        foreach (CutState state7 in context.KnowledgeBase.Prove(BodyGoals[6].Functor, goal7Args, context, parentFrame))
                                        {
                                            yield return state7;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    sealed class KnowledgeBaseRule8 : KnowledgeBaseRule
    {
        public KnowledgeBaseRule8(Structure ruleHead, Structure[] ruleBody, bool checkSingletons, string source, int line)
            : base(ruleHead, ruleBody, checkSingletons, source, line)
        { }

        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            object[] goal1Args = null;
            object[] goal2Args = null;
            object[] goal3Args = null;
            object[] goal4Args = null;
            object[] goal5Args = null;
            object[] goal6Args = null;
            object[] goal7Args = null;
            object[] goal8Args = null;
            
            var newVars = new LogicVariable[FreeVariables.Count];
            object[] newArgs = Term.AlphaConvertArglist(HeadArgs, FreeVariables, newVars, context, true);
// ReSharper disable UnusedVariable
            foreach (bool ignore in Term.UnifyArraysFast(args, newArgs, context))
            {
                if (goal1Args == null)
                    goal1Args = Term.AlphaConvertArglist(BodyGoals[0].Arguments, FreeVariables, newVars, context, false);

                foreach (CutState ignoreFreeze in context.ProveAllWokenGoals())
                    // ReSharper restore UnusedVariable
                    foreach (CutState state1 in context.KnowledgeBase.Prove(BodyGoals[0].Functor, goal1Args, context, parentFrame))
                {
                    if (state1 == CutState.ForceFail) yield return CutState.ForceFail;
                    if (goal2Args == null)
                        goal2Args = Term.AlphaConvertArglist(BodyGoals[1].Arguments, FreeVariables, newVars, context, false);

                    foreach (CutState state2 in context.KnowledgeBase.Prove(BodyGoals[1].Functor, goal2Args, context, parentFrame))
                    {
                        if (state2 == CutState.ForceFail) yield return CutState.ForceFail;
                        if (goal3Args == null)
                            goal3Args = Term.AlphaConvertArglist(BodyGoals[2].Arguments, FreeVariables, newVars, context, false);

                        foreach (CutState state3 in context.KnowledgeBase.Prove(BodyGoals[2].Functor, goal3Args, context, parentFrame))
                        {
                            if (state3 == CutState.ForceFail) yield return CutState.ForceFail;
                            if (goal4Args == null)
                                goal4Args = Term.AlphaConvertArglist(BodyGoals[3].Arguments, FreeVariables, newVars, context, false);

                            foreach (CutState state4 in context.KnowledgeBase.Prove(BodyGoals[3].Functor, goal4Args, context, parentFrame))
                            {
                                if (state4 == CutState.ForceFail) yield return CutState.ForceFail;
                                if (goal5Args == null)
                                    goal5Args = Term.AlphaConvertArglist(BodyGoals[4].Arguments, FreeVariables, newVars, context, false);

                                foreach (CutState state5 in context.KnowledgeBase.Prove(BodyGoals[4].Functor, goal5Args, context, parentFrame))
                                {
                                    if (state5 == CutState.ForceFail) yield return CutState.ForceFail;
                                    if (goal6Args == null)
                                        goal6Args = Term.AlphaConvertArglist(BodyGoals[5].Arguments, FreeVariables, newVars, context, false);

                                    foreach (CutState state6 in context.KnowledgeBase.Prove(BodyGoals[5].Functor, goal6Args, context, parentFrame))
                                    {
                                        if (state6 == CutState.ForceFail) yield return CutState.ForceFail;
                                        if (goal7Args == null)
                                            goal7Args = Term.AlphaConvertArglist(BodyGoals[6].Arguments, FreeVariables, newVars, context, false);

                                        foreach (CutState state7 in context.KnowledgeBase.Prove(BodyGoals[6].Functor, goal7Args, context, parentFrame))
                                        {
                                            if (state7 == CutState.ForceFail) yield return CutState.ForceFail;
                                            if (goal8Args == null)
                                                goal8Args = Term.AlphaConvertArglist(BodyGoals[7].Arguments, FreeVariables, newVars, context, false);

                                            foreach (CutState state8 in context.KnowledgeBase.Prove(BodyGoals[7].Functor, goal8Args, context, parentFrame))
                                            {
                                                yield return state8;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
