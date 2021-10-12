using System;
using System.Collections.Generic;

namespace Prolog
{
    /// <summary>
    /// Prolog-like logic variable
    /// </summary>
    [System.Diagnostics.DebuggerDisplay("{DebuggerName}")]
    public sealed class LogicVariable : AlphaConvertibleTerm
    {
        #region Constructor
        /// <summary>
        /// Creates a new logic variable
        /// </summary>
        public LogicVariable(Symbol name)
        {
            Name = name;
            UID = ++UIDCounter;
            mValue = this;
        }

        /// <summary>
        /// Creates a new logic variable
        /// </summary>
        /// <param name="name">Print name for the variable</param>
        public LogicVariable(string name)
            : this(Symbol.Intern(name))
        { }
        #endregion

        #region Instance fields and non-derived properties
        /// <summary>
        /// Name (for debugging purposes) of variable.
        /// </summary>
        public Symbol Name { get; private set; }
        /// <summary>
        /// UID of the variable.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1709:IdentifiersShouldBeCasedCorrectly", MessageId = "UID")]
        // ReSharper disable once InconsistentNaming
        public uint UID { get; private set; }

        /// <summary>
        /// The internal value slot of the variable
        /// DO NOT USE THIS UNLESS YOU ARE THE LOGIC VARIABLE CODE OR THE TRAIL CODE
        /// </summary>
        // ReSharper disable once InconsistentNaming
        internal object mValue;

        /// <summary>
        /// True if the variable is bound to a value or another logic variable
        /// </summary>
        public bool IsBound
        {
            get { return mValue != this && !(mValue is Metastructure); }
        }

        #endregion

        #region Derived properties
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1305:SpecifyIFormatProvider", MessageId = "System.String.Format(System.String,System.Object,System.Object)"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1305:SpecifyIFormatProvider", MessageId = "System.String.Format(System.String,System.Object,System.Object,System.Object)"), System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Globalization", "CA1305:SpecifyIFormatProvider", MessageId = "System.String.Format(System.String,System.Object)")]
        // ReSharper disable UnusedMember.Local
        string DebuggerName
        // ReSharper restore UnusedMember.Local
        {
            get
            {
                if (IsBound)
                    return string.Format("{0}{1}={2}", Name.Name, UID, Value);
                return string.Format("{0}{1}", Name.Name, UID);
            }
        }


        /// <summary>
        /// Current value of the variable.  If the variable is part of a chain of aliased variables,
        /// returns the variable at the end of the chain, if it is unbound, or its value, if its bound.
        /// </summary>
        public object Value
        {
            get
            {
                return Deref(mValue);
            }
            set
            {
                mValue = value;
                //IsBound = true;
            }
        }

        internal object UncanonicalizedValue
        {
            get { return mValue; }
        }

        internal Metastructure MetaBinding
        {
            get { return (mValue == null) ? null : (mValue as Metastructure); }
        }
        #endregion

        #region Static variables
        /// <summary>
        /// Global counter for allocating UIDs to logic variables
        /// </summary>
        // ReSharper disable InconsistentNaming
        static uint UIDCounter;
        // ReSharper restore InconsistentNaming
        #endregion

        #region Trace-based Unification
        /// <summary>
        /// Saves variable to the trail and updates it.
        /// </summary>
        void SaveAndUpdate(object value, PrologContext context)
        {
            context.SaveVariable(this);
            mValue = value;
        }

        internal bool UnifyWithCanonicalValue(object value, PrologContext context)
        {
            if (!IsBound)
            {
                Metastructure m = MetaBinding;
                if (m == null)
                {
                    // We're binding a truly unbound variable to something.
                    if (value != this)
                    {
                        var xl = value as LogicVariable;
                        if (xl != null)
                        {
                            Metastructure xm2;
                            if ((xm2 = xl.MetaBinding) != null)
                            {
                                // We're binding a truly unbound variable to a meta-bound variable
                                xl.UnifyMetaVar(xm2, this, context);
                                return true;
                            }
                        }
                        SaveAndUpdate(value, context); // sets IsBound
                    }
                    return true;
                }
                // This is an attributed (metabound) variable
                var l = value as LogicVariable;
                if (l == null)
                {
                    UnifyMetaTerm(m, value, context);
                    return true;
                }
                Metastructure m2 = l.MetaBinding;
                if (m2 == null)
                {
                    // Need to alias l to this, that's most easily done by letting l unify to this.
                    UnifyMetaVar(m, l, context);
                    return true;
                }
                UnifyMetaMeta(m, m2, l, context);
                return true;
            }
            return Value.Equals(value);
        }

        /// <summary>
        /// This is an attributed variable with attribute m, unify it with value.
        /// </summary>
        /// <param name="m">Current attribute</param>
        /// <param name="value">Value to give it</param>
        /// <param name="context">Prolog context (to get trail and wakeup list)</param>
        /// <returns>Success</returns>
        void UnifyMetaTerm(Metastructure m, object value, PrologContext context)
        {
            Value = value;
            m.MetaTermUnify(value, context);
        }

        /// <summary>
        /// This is an attributed variable with attribute myMetaStructure, unify it with attributed variable them, with attribute theirMetaStructure.
        /// </summary>
        /// <param name="myMetaStructure">This variable's metavalue</param>
        /// <param name="theirMetaStructure">The meta-value of the variable we're unifying with</param>
        /// <param name="them">The variable to unify with</param>
        /// <param name="context">Prolog context</param>
        void UnifyMetaMeta(Metastructure myMetaStructure, Metastructure theirMetaStructure, LogicVariable them, PrologContext context)
        {
            SaveAndUpdate(myMetaStructure.MetaMetaUnify(theirMetaStructure, context), context);
            them.SaveAndUpdate(this, context);
        }

        /// <summary>
        /// This is an atributed variable with metastructure m, unify with unattributed and unbound variable v.
        /// </summary>
        /// <param name="m">This variable's metavalue</param>
        /// <param name="l">The completely unbound logic variable to which we are binding.</param>
        /// <param name="context">Prolog context</param>
        void UnifyMetaVar(Metastructure m, LogicVariable l, PrologContext context)
        {
            System.Diagnostics.Debug.Assert(l.MetaBinding == null);
            l.SaveAndUpdate(this, context);
            this.SaveAndUpdate(m.MetaVarUnify(l, context), context);
        }

        internal override bool UnifyWithStructure(Structure value, PrologContext context)
        {
            return UnifyWithCanonicalValue(value, context);
        }

        internal override bool UnifyWithTerm(Term term, PrologContext context)
        {
            return UnifyWithCanonicalValue(term, context);
        }

        internal override bool UnifyWithAtomicConstant(object value, PrologContext context)
        {
            return UnifyWithCanonicalValue(value, context);
        }

        /// <summary>
        /// Attempt to unify the logic variable against the specified value (may be another logic variable or not).
        /// </summary>
        public bool Unify(object value, PrologContext context)
        {
            return Unify(this, value, context);
        }
        #endregion

        #region Iterator-based unification
        internal IEnumerable<bool> UnifyWithCanonicalValue(object value)
        {
            if (!IsBound)
            {
                Metastructure m = MetaBinding;
                if (m == null)
                {
                    // We're binding a truly unbound variable to something.
                    if (value != this)
                    {
                        var xl = value as LogicVariable;
                        if (xl != null)
                        {
                            Metastructure xm2;
                            if ((xm2 = xl.MetaBinding) != null)
                            {
                                // We're binding a truly unbound variable to a meta-bound variable
                                return xl.UnifyMetaVar(xm2, this);
                            }
                        }
                        Value = value; // sets IsBound
                    }
                    return SucceedOnceAndThenUnBind();
                }
                // This is an attributed (metabound) variable
                var l = value as LogicVariable;
                if (l==null)
                    return UnifyMetaTerm(m, value);
                Metastructure m2 = l.MetaBinding;
                if (m2 == null)
                    // Need to alis l to this, that's most easily done by letting l unify to this.
                    return UnifyMetaVar(m, l);
                return UnifyMetaMeta(m, m2, l);
            }
            return ToEnumerator(Value.Equals(value));
        }

        IEnumerable<bool> UnifyMetaTerm(Metastructure m, object value)
        {
            Value = value;
            try
            {
#pragma warning disable 414, 168, 219
                // ReSharper disable UnusedVariable
                foreach (var ignore in m.MetaTermUnify(value))
                    // ReSharper restore UnusedVariable
#pragma warning restore 414, 168, 219
                    yield return false;
            }
            finally
            {
                mValue = m;
                //IsBound = false;
            }
        }

        IEnumerable<bool> UnifyMetaMeta(Metastructure myMetaStructure, Metastructure theirMetaStructure, LogicVariable them)
        {
            IEnumerable<CutState> filter;
            mValue = myMetaStructure.MetaMetaUnify(theirMetaStructure, out filter);
            them.Value = this;
#pragma warning disable 414, 168, 219
            // ReSharper disable UnusedVariable
            foreach (var ignore in filter)
                // ReSharper restore UnusedVariable
#pragma warning restore 414, 168, 219
                yield return false;
            mValue = myMetaStructure;
            them.mValue = theirMetaStructure; //null;
            //them.IsBound = false;
        }

        IEnumerable<bool> UnifyMetaVar(Metastructure m, LogicVariable l)
        {
            System.Diagnostics.Debug.Assert(l.MetaBinding == null);
            l.Value = this;
            IEnumerable<CutState> goal;
            mValue = m.MetaVarUnify(l, out goal);
            try
            {
#pragma warning disable 414, 168, 219
                // ReSharper disable UnusedVariable
                foreach (var ignore in goal)
                    // ReSharper restore UnusedVariable
#pragma warning restore 414, 168, 219
                    yield return false;
            }
            finally
            {
                // Reset our own binding to its previous binding
                mValue = m;
                // Reset binding of l
                l.mValue = l;
                //l.IsBound = false;
            }
        }

        internal override IEnumerable<bool> UnifyWithStructure(Structure value)
        {
            return UnifyWithCanonicalValue(value);
        }

        internal override IEnumerable<bool> UnifyWithTerm(Term term)
        {
            return UnifyWithCanonicalValue(term);
        }

        internal override IEnumerable<bool> UnifyWithAtomicConstant(object value)
        {
            return UnifyWithCanonicalValue(value);
        }

        /// <summary>
        /// Attempt to unify the logic variable against the specified value (may be another logic variable or not).
        /// </summary>
        public IEnumerable<bool> Unify(object value)
        {
            return Unify(this, value);
        }

        IEnumerable<bool> SucceedOnceAndThenUnBind()
        {
            try
            {
                yield return false;
            }
            finally
            {
                //IsBound = false;
                this.ForciblyUnbind();
            }
        }

        internal void ForciblyUnbind()
        {
            this.mValue = this;
        }

        internal IEnumerable<CutState> MetaUnify(Metastructure m)
        {
            if (IsBound) throw new InvalidOperationException("Cannot meta-unify a bound variable");
            Metastructure old = MetaBinding;
            IEnumerable<CutState> filter = null;
            mValue = old != null ? old.MetaMetaUnify(m, out filter) : m;
            try
            {
                if (filter != null)
#pragma warning disable 414, 168, 219
                    // ReSharper disable UnusedVariable
                    foreach (var ignore in filter)
                        // ReSharper restore UnusedVariable
#pragma warning restore 414, 168, 219
                        yield return CutState.Continue;
                else
                    yield return CutState.Continue;
            }
            finally
            {
                mValue = old;
            }
        }
        #endregion

        #region Other methods
        /// <summary>
        /// All we have to do here is check whether this is one of the variables we're looking for.
        /// </summary>
        public override object AlphaConvert(List<LogicVariable> oldVars, LogicVariable[] newVars, PrologContext context, bool evalIndexicals)
        {
            var index = oldVars.IndexOf(this);
            if (index < 0)
                return this;
            return newVars[index] ?? (newVars[index] = new LogicVariable(this.Name));
        }

        /// <summary>
        /// The name and UID of the object
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            if (IsBound)
                return (Value == null) ? "null" : Value.ToString();
            return string.Format("{0}{1}", Name.Name, UID);
        }
        #endregion
    }
}
