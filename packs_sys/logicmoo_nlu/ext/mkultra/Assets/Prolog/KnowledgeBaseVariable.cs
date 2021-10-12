using System;
using System.Collections.Generic;

namespace Prolog
{
    /// <summary>
    /// A KBE whose only purpose is to act like a global variable, e.g. for state machines, discourse state, etc.
    /// </summary>
    sealed class KnowledgeBaseVariable : KnowledgeBaseEntry
    {
        /// <summary>
        /// Current value of the "variable".
        /// </summary>
        public object CurrentValue { get; set; }

        KnowledgeBaseVariable(object initialValue)
        {
            CurrentValue = initialValue;
        }

        public override object Head
        {
            get { throw new NotImplementedException(); }
        }

        public override object Body
        {
            get { throw new NotImplementedException(); }
        }

        internal override IEnumerable<CutState> Prove(object[] args, PrologContext context, ushort parentFrame)
        {
            if (args.Length != 1) throw new ArgumentCountException("variable", args, new object[] { "Value" });
            return Term.UnifyAndReturnCutState(CurrentValue, args[0]);
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2208:InstantiateArgumentExceptionsCorrectly")]
        internal static IEnumerable<CutState> SetImplementation(object[] args, PrologContext context)
        {
            if (args.Length != 2) throw new ArgumentCountException("set", args, new object[] { "Variable", "NewValue"});

            object value = Term.CopyInstantiation(args[1]);
            if (value is LogicVariable) throw new UninstantiatedVariableException((LogicVariable)args[1], "Value argument should be a data object, not an uninstantiated (unbound) variable.");
            var functor = Term.Deref(args[0]) as Symbol;
            if (functor == null) throw new ArgumentTypeException("set", "functor", args[0], typeof (Symbol));

            List<KnowledgeBaseEntry> entries = context.KnowledgeBase.EntryListForStoring(new PredicateIndicator(functor, 1));

            switch (entries.Count)
            {
                case 0:
                    entries.Add(new KnowledgeBaseVariable(value));
                    return CutStateSequencer.Succeed();

                case 1:
                    var v = entries[0] as KnowledgeBaseVariable;
                    if (v==null)
                        throw new ArgumentException("Functor is not a variable; it has another entry defined for it.");
                    v.CurrentValue = value;
                    return CutStateSequencer.Succeed();

                default:
                    throw new ArgumentException("Functor is not a variable; it has multiple entries defined for it.");
            }
        }
    }
}
