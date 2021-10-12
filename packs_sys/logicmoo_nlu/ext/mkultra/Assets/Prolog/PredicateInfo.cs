using System;
using System.Collections.Generic;

namespace Prolog
{
    /// <summary>
    /// Represents all the KB information about a predicate
    /// </summary>
    [System.Diagnostics.DebuggerDisplay("{Name}")]
    class PredicateInfo
    {
        /// <summary>
        /// Creates a blank DB entry for the specified functor
        /// </summary>
        public PredicateInfo(Symbol functorName, int arity, KnowledgeBase kb)
        {
            Name = functorName;
            Arity = arity;
            Entries = new List<KnowledgeBaseEntry>();
            KnowledgeBase = kb;
        }
        /// <summary>
        /// Name of the predicate
        /// </summary>
        public Symbol Name { get; private set; }
        /// <summary>
        /// Arity of the predicate
        /// </summary>
        public int Arity { get; private set; }
        /// <summary>
        /// Whether the predicate should be traced
        /// </summary>
        public bool Trace { get; set; }
        /// <summary>
        /// Whether the predicate can be randomized
        /// </summary>
        public bool Randomizable { get; set; }
        /// <summary>
        /// Whether the predicate can shadow definitions in the parent KB
        /// </summary>
        public bool Shadow { get; set; }
        /// <summary>
        /// This PredicateInfo is a placeholder for a predicate that is either optionally defined or is defined in a different KB.
        /// </summary>
        public bool External { get; set; }
        /// <summary>
        /// This predicate is expected to be called from outside, so don't worry that it's unreferenced
        /// </summary>
        public bool Public { get; set; }
        /// <summary>
        /// The KB within which this predicate is defined.
        /// </summary>
        public KnowledgeBase KnowledgeBase { get; private set; }

        /// <summary>
        /// Indicies of the predicate's arguments that are higher order (i.e. goals to be called).
        /// </summary>
        public int[] HigherOrderArguments { get; set; }

        /// <summary>
        /// Specific KnowledgeBaseEntry objects for this predicate.
        /// </summary>
        public List<KnowledgeBaseEntry> Entries { get; private set; }

        /// <summary>
        /// True if this predicate has been called since the last time it is modified.
        /// If it's false, then it's safe to overwrite the current list in place.
        /// </summary>
        private bool entriesListUsed;

        public bool Compiled
        {
            get { return Entries.Count>0 && Entries[0] is ByteCompiledRule; }
        }

        /// <summary>
        /// Byte compiles all the rules in this predicate.
        /// </summary>
        public void Compile()
        {
            for (int i=0; i<Entries.Count; i++)
            {
                var entry = Entries[i] as KnowledgeBaseRule;
                if (entry != null && !(entry is ByteCompiledRule))
                    Entries[i] = new ByteCompiledRule(this, Term.Structurify(entry.Head, "Malformed head."), entry.BodyGoals, entry.SourceFile, entry.SourceLineNumber);
            }
        }

        /// <summary>
        /// Prints to the console the disassembled bytecode for all rules in this predicate.
        /// </summary>
        public void Disassemble()
        {
            foreach (var knowledgeBaseEntry in Entries)
            {
                var rule = (ByteCompiledRule)knowledgeBaseEntry;
                Console.WriteLine("");
                Console.Write(ISOPrologWriter.WriteToString(rule.Head));
                Console.Write(" :- \n    ");
                Console.Write(ISOPrologWriter.WriteToString(rule.Body));
                Console.WriteLine(".");
                rule.Disassemble();
            }
        }

        internal IEnumerable<CutState> StackCall(PrologContext context)
        {
            if (Compiled)
                return TestCompiledClauses(context);
            return Prove(context.GetCallArgumentsAsArray(Arity), context);
        }

        private IEnumerable<CutState> TestCompiledClauses(PrologContext context)
        {
            foreach (var knowledgeBaseEntry in Entries)
            {
                var rule = (ByteCompiledRule)knowledgeBaseEntry;
                foreach (var result in rule.StackCall(context))
                    if (result == CutState.ForceFail)
                        yield break;
                    else
                        yield return result;
            }
        }

        internal IEnumerable<CutState> Prove(object[] args, PrologContext context)
        {
            var myFrame = context.CurrentFrame;
            if (KnowledgeBase.Trace || Trace)
                context.TraceOutput("Goal: {0}", new Structure(Name, args));
            if (Compiled)
            {
                context.PushArguments(args);
                return StackCall(context);
            }
            if (context.Randomize && Randomizable && Entries.Count > 1)
                return TestShuffledClauses(args, context, myFrame);
            return TestClausesInOrder(args, context, myFrame);
        }

        /// <summary>
        /// Tests clauses in the order they appear in the database.
        /// </summary>
        IEnumerable<CutState> TestClausesInOrder(object[] args, PrologContext context, ushort myFrame)
        {
            var mark = context.MarkTrace();
            var argIndexers = PredicateArgumentIndexer.ArglistIndexers(args);
            entriesListUsed = true;
            foreach (var entry in Entries)
            {
                if (entry.Prematch(argIndexers))
                {
                    context.SetCurrentRule(entry);
                    foreach (var cutState in entry.Prove(args, context, myFrame))
                    {
                        if (cutState == CutState.ForceFail)
                        {
                            if (KnowledgeBase.Trace || Trace)
                                context.TraceOutput("Cut: {0}", new Structure(Name, args));
                            goto fail;
                        }
                        if (KnowledgeBase.Trace || Trace)
                            context.TraceOutput("Succeed: {0}", new Structure(Name, args));
                        yield return CutState.Continue;
                        if (KnowledgeBase.Trace || Trace)
                            context.TraceOutput("Retry: {0}", new Structure(Name, args));
                    }
                }
            }
        fail:
            context.RestoreVariables(mark);
            if (KnowledgeBase.Trace || Trace)
                context.TraceOutput("Fail: {0}", new Structure(Name, args));
            //context.UnwindStack(Name, args);
            context.UnwindStack(myFrame);
        }

        /// <summary>
        /// Tests clauses in a randomized order (but still exhaustively).
        /// Uses Shuffler to generate a random permutation.
        /// </summary>
        IEnumerable<CutState> TestShuffledClauses(object[] args, PrologContext context, ushort myFrame)
        {
            entriesListUsed = true;
            var mark = context.MarkTrace();
            var shuffler = new Shuffler((ushort)Entries.Count);
            var argIndexers = PredicateArgumentIndexer.ArglistIndexers(args);
            while (!shuffler.Done)
            {
                var entry = Entries[shuffler.Next()];
                if (entry.Prematch(argIndexers))
                {
                    // This shouldn't be here...
                    //context.PushGoalStack(Name, args, myFrame);
                    context.SetCurrentRule(entry);
                    foreach (var cutState in entry.Prove(args, context, myFrame))
                    {
                        if (cutState == CutState.ForceFail)
                        {
                            if (KnowledgeBase.Trace || Trace)
                                context.TraceOutput("Cut: {0}", new Structure(Name, args));
                            goto fail;
                        }
                        if (KnowledgeBase.Trace || Trace)
                            context.TraceOutput("Succeed: {0}", new Structure(Name, args));
                        yield return CutState.Continue;
                        if (KnowledgeBase.Trace || Trace)
                            context.TraceOutput("Retry: {0}", new Structure(Name, args));
                    }
                }
            }
        fail:
            context.RestoreVariables(mark);
            if (KnowledgeBase.Trace || Trace)
                context.TraceOutput("Fail: {0}", new Structure(Name, args));
            //context.UnwindStack(Name, args);
            context.UnwindStack(myFrame);
        }

        List<KnowledgeBaseEntry> GetEntriesListForUpdate()
        {
            List<KnowledgeBaseEntry> entries = Entries;
            if (entriesListUsed)
            {
                entriesListUsed = false;
                return Entries = new List<KnowledgeBaseEntry>(entries);
            }
            return this.Entries;
        }

        /// <summary>
        /// Adds a KnowledgeBaseRule to the predicate.
        /// NOT THREADSAFE!
        /// </summary>
        /// <param name="assertion">The rule to add</param>
        /// <param name="atEnd">If true, adds to be beginning, else the end.</param>
        public void Assert(KnowledgeBaseRule assertion, bool atEnd)
        {
            var entries = GetEntriesListForUpdate();

            if (atEnd)
                entries.Add(assertion);
            else
                entries.Insert(0, assertion);
        }

        public void RetractAll(Structure head)
        {
            var entries = GetEntriesListForUpdate();

            for (int i = entries.Count - 1; i >= 0; i--)
            {
                if (Term.Unifiable(head, entries[i].Head))
                {
                    entries.RemoveAt(i);
                }
            }
        }

        public IEnumerable<CutState> Retract(Structure head, object body)
        {
            var entries = GetEntriesListForUpdate();
            bool gotOne = true;
            while (gotOne && entries.Count>0)
            {
                gotOne = false;
                for (int i = 0; !gotOne && i < entries.Count; i++)
                {
                    var entry = entries[i];
                    // Have to recopy the rule just in case it's being used in a pending subgoal.
                    // If it is, then the subgoal will see a modified version of the rule.
                    var rule = (Structure)Term.CopyInstantiation(new Structure(Symbol.Implication, entry.Head, entry.Body));
#pragma warning disable 168
                    // ReSharper disable UnusedVariable
                    foreach (var ignore1 in Term.Unify(head, rule.Argument(0)))
                        foreach (var ignore2 in Term.Unify(body, rule.Argument(1)))
                        // ReSharper restore UnusedVariable
#pragma warning restore 168
                        {
                            gotOne = true;
                            entries.RemoveAt(i);
                            yield return CutState.Continue;
                            entries = GetEntriesListForUpdate();
                        }
                }
            }
        }

        public IEnumerable<CutState> FindClauses(Structure head, object body)
        {
            foreach (var entry in Entries)
            {
                var rule =
                    (Structure) Term.CopyInstantiation(new Structure(Symbol.Implication, entry.Head, entry.Body));
#pragma warning disable 414, 168, 219
                // ReSharper disable UnusedVariable
                foreach (var ignore1 in Term.Unify(rule.Argument(0), head))
                {
                    foreach (var ignroe2 in Term.Unify(rule.Argument(1), body))
#pragma warning restore 414, 168, 219
                        // ReSharper restore UnusedVariable
                        yield return CutState.Continue;
                }
            }
        }

        public override string ToString()
        {
            return string.Format("PredicateInfo({0}/{1})", this.Name, this.Arity);
        }
    }
}
