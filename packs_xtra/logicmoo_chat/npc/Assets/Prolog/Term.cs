using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace Prolog
{
    /// <summary>
    /// Represents a Prolog term.
    /// Currently includes Structures (not structs), Symbols (what Prolog calls Atoms), and LogicVariables.
    /// Will likely be extended in the future to include boxed constants.
    /// </summary>
    [DebuggerDisplay("{DebuggerDisplay}")]
    public abstract class Term
    {
        #region Canonicalization
        /// <summary>
        /// Returns value, unless it is a LogicVariable, in which case it returns the variable's value.
        /// </summary>
        public static object Deref(object value)
        {
            //LogicVariable alias = value as LogicVariable;
            //if (alias != null && alias.IsBound)
            //    return alias.Value;
            //return value;
            var v = value as LogicVariable;
            while (v != null && v.IsBound)
            {
                value = v.UncanonicalizedValue;
                v = value as LogicVariable;
            }
            return value;
        }

        private static readonly object[] NoArgs = new object[0];

        internal static Structure Structurify(object term, string errorMessage)
        {
            term = Deref(term);
            if (term is bool)
            {
                term = (bool)term?Symbol.True:Symbol.Fail;
            }
            var t = term as Structure;
            if (t != null)
                return t;
            var s = term as Symbol;
            if (s != null)
                return new Structure(s, NoArgs);
            var offendingVariable = term as LogicVariable;
            if (offendingVariable != null)
                throw new InstantiationException(offendingVariable, errorMessage);
            throw new GoalException(term, errorMessage);
        }
        #endregion

        #region Ground testing
        /// <summary>
        /// True if term contains no unbound variables
        /// </summary>
        public static bool IsGround(object term)
        {
            term = Deref(term);
            var s = term as Structure;
            if (s != null)
            {
                foreach (var arg in s.Arguments)
                    if (!IsGround(arg))
                        return false;
                return true;
            }
            return !(term is LogicVariable);
        }

        /// <summary>
        /// Returns first uninstantiated variable found in term.
        /// </summary>
        public static LogicVariable FindUninstantiatedVariable(object term)
        {
            term = Deref(term);
            var s = term as Structure;
            if (s != null)
            {
                LogicVariable v;
                foreach (var arg in s.Arguments)
                    if ((v = FindUninstantiatedVariable(arg)) != null)
                        return v;
                return null;
            }
            return (term as LogicVariable);
        }
        #endregion

        #region Comparison and sorting
        /// <summary>
        /// Compares to prolog terms for purposes of sorting
        /// </summary>
        public static int Compare(object term1, object term2)
        {
            int type1 = TypeNumber(term1);
            int comp = type1-TypeNumber(term2);
            if (comp != 0)
                return comp;
            switch (type1)
            {
                case 0: // logicvariables
                    return (int)((LogicVariable) term1).UID - (int)((LogicVariable) term2).UID;
                case 1: // floats
                    return Convert.ToDouble(term1).CompareTo(Convert.ToDouble(term2));
                case 2: // ints
                    return (int) term1 - (int) term2;
                case 3: // null
                    return 0; // they're both null
                case 4: // symbols/atoms
// ReSharper disable StringCompareToIsCultureSpecific
                    return ((Symbol) term1).Name.CompareTo(((Symbol) term2).Name);
                case 5: // strings
                    return ((string) term1).CompareTo((string) term2);
                case 6: // structures
                    var s1 = (Structure) term1;
                    var s2 = (Structure) term2;
                    if (s1.Arguments.Length != s2.Arguments.Length)
                        return s1.Arguments.Length.CompareTo(s2.Arguments.Length);
                    if (s1.Functor != s2.Functor)
                        return s1.Functor.Name.CompareTo(s2.Functor.Name);
// ReSharper restore StringCompareToIsCultureSpecific
                    for (int i=0; i<s1.Arguments.Length; i++)
                    {
                        int c = Compare(s1.Arguments[i], s2.Arguments[i]);
                        if (c != 0)
                            return c;
                    }
                    return 0;
                case 7: // Other kind of .NET object unfamiliar to ISO Prolog
                    // Total kluge; hope it sort of works.
                    return term1.GetHashCode().CompareTo(term2.GetHashCode());
                default:
                    throw new Exception("Term had an undefined type number.");
            }
        }

        static int CompareKeys(object o1, object o2)
        {
            var s1 = o1 as Structure;
            var s2 = o2 as Structure;
            if (s1 == null || s2 == null || !s1.IsFunctor(Symbol.Minus, 2) || !s2.IsFunctor(Symbol.Minus, 2))
            {
                throw new ArgumentException("Attempt to compare keys of objects that were not Prolog terms of the form KEY-VALUE");
            }
            return Compare(s1.Argument(0), s2.Argument(0));
        }

        static int TypeNumber(object term)
        {
            if (term is LogicVariable)
                return 0;
            if ((term is float) || (term is double))
                return 1;
            if (term is int)
                return 2;
            if (term == null)
                return 3;
            if (term is Symbol)
                return 4;
            if (term is string)
                return 5;
            if (term is Structure)
                return 6;
            return 7;
        }

        /// <summary>
        /// Sorts arbitrary list of Prolog terms
        /// </summary>
        public static void Sort(List<object> terms, bool deleteDuplicates)
        {
            if (terms.Count == 0)
                return;
            terms.Sort(Compare);
            if (deleteDuplicates)
            {
                for (int i=terms.Count-2; i>0; i--)
                    if (Compare(terms[i], terms[i+1])==0)
                        terms.RemoveAt(i+1);
                if (terms.Count>1 && Compare(terms[0], terms[1])==0)
                    terms.RemoveAt(1);
            }
        }

        /// <summary>
        /// Sorts arbitrary list of Prolog terms
        /// </summary>
        public static void KeySort(List<object> terms, bool deleteDuplicates)
        {
            if (terms.Count == 0)
                return;
            terms.Sort(CompareKeys);
            if (deleteDuplicates)
            {
                for (int i = terms.Count - 2; i > 0; i--)
                    if (Compare(terms[i], terms[i + 1]) == 0)
                        terms.RemoveAt(i + 1);
                if (terms.Count > 1 && Compare(terms[0], terms[1]) == 0)
                    terms.RemoveAt(1);
            }
        }

        public static object SortPrologList(object list, bool deleteDuplicates)
        {
            var l = Prolog.PrologListToIList(list);
            Sort(l, deleteDuplicates);
            return Prolog.IListToPrologList(l);
        }

        public static object KeySortPrologList(object list, bool deleteDuplicates)
        {
            var l = Prolog.PrologListToIList(list);
            KeySort(l, deleteDuplicates);
            return Prolog.IListToPrologList(l);
        }
        #endregion

        #region Trail-based Unification
        /// <summary>
        /// Attempt to unify the two values.  Either, both, or neither may be logic variables.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "v")]
        public static bool Unify(object v1, object v2, PrologContext context)
        {
            object o1 = Deref(v1);
            object o2 = Deref(v2);
            if (o1 == o2)
                // Fast path
                return true;
            var t1 = o1 as Term;
            var t2 = o2 as Term;
            if (t1 != null)
            {
                if (t2 != null)
                    return t1.UnifyWithTerm(t2, context);
                return t1.UnifyWithAtomicConstant(o2, context);
            }
            // o1 isn't a Term
            if (t2 != null)
                // o2 is a Term
                return t2.UnifyWithAtomicConstant(o1, context);
            // Neither is a Term
            if (o1 == null)
            {
                return o2 == null;
            }
            return o1.Equals(o2);
        }

        internal virtual bool UnifyWithAtomicConstant(object value, PrologContext context)
        {
            return Equals(value);
        }

        internal virtual bool UnifyWithStructure(Structure value, PrologContext context)
        {
            return false;
        }

        internal abstract bool UnifyWithTerm(Term term, PrologContext context);
        #endregion

        #region Iterator-based Unification
        /// <summary>
        /// Attempt to unify the two values.  Either, both, or neither may be logic variables.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "v")]
        public static IEnumerable<bool> Unify(object v1, object v2)
        {
            object o1 = Deref(v1);
            object o2 = Deref(v2);
            var t1 = o1 as Term;
            var t2 = o2 as Term;
            if (t1 != null)
            {
                if (t2 != null)
                    return t1.UnifyWithTerm(t2);
                return t1.UnifyWithAtomicConstant(o2);
            }
            // o1 isn't a Term
            if (t2 != null)
                // o2 is a Term
                return t2.UnifyWithAtomicConstant(o1);
            // Neither is a Term
            if (o1 == null)
            {
                return ToEnumerator(o2 == null);
            }
            return ToEnumerator(o1.Equals(o2));
        }

        internal virtual IEnumerable<bool> UnifyWithAtomicConstant(object value)
        {
            return ToEnumerator(Equals(value));
        }

        internal virtual IEnumerable<bool> UnifyWithStructure(Structure value)
        {
            return FailEnumerator;
        }

        internal abstract IEnumerable<bool> UnifyWithTerm(Term term);

            /// <summary>
        /// Attempt to unify the two values.  Either, both, or neither may be logic variables.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "v")]
        internal static IEnumerable<CutState> UnifyAndReturnCutState(object v1, object v2)
        {
            // ReSharper disable UnusedVariable
#pragma warning disable 414, 168, 219
            foreach (var ignore in Unify(v1, v2))
#pragma warning restore 414, 168, 219
                // ReSharper restore UnusedVariable
                yield return CutState.Continue;

            //object o1 = Canonicalize(v1);
            //object o2 = Canonicalize(v2);
            //LogicVariable lv = o1 as LogicVariable;
            //if (lv != null)
            //{
            //    // ReSharper disable UnusedVariable
            //    foreach (bool ignore in lv.Unify(o2))
            //        // ReSharper restore UnusedVariable
            //        yield return CutState.Continue;
            //}
            //else
            //{
            //    lv = o2 as LogicVariable;
            //    if (lv != null)
            //    {
            //        // ReSharper disable UnusedVariable
            //        foreach (bool ignore in lv.Unify(o1))
            //            // ReSharper restore UnusedVariable
            //            yield return CutState.Continue;
            //    }
            //    else
            //    {
            //        Structure t1 = o1 as Structure;
            //        Structure t2 = o2 as Structure;
            //        if (t1 == null || t2 == null)
            //        {
            //            if (o1 == null)
            //            {
            //                if (o2 == null) yield return CutState.Continue;
            //            }
            //            else if (o1.Equals(o2))
            //                yield return CutState.Continue;
            //        }
            //        else
            //        {
            //            if (t1.Functor == t2.Functor && t1.Arguments.Length == t2.Arguments.Length)
            //            {
            //                // ReSharper disable UnusedVariable
            //                foreach (bool ignore in UnifyArrays(t1.Arguments, t2.Arguments))
            //                    // ReSharper restore UnusedVariable
            //                    yield return CutState.Continue;
            //            }
            //        }
            //    }
            //}
        }
        #endregion

        #region Array unification
#pragma warning disable 414, 168, 219
        /// <summary>
        /// Unifies arrays using trailing
        /// </summary>
        /// <returns>Success</returns>
        internal static bool UnifyArrays(object[] a1, object[] a2, PrologContext context)
        {
            if (a1.Length != a2.Length)
                return false;
            for (int i = 0; i < a1.Length; i++)
                if (!Unify(a1[i], a2[i], context))
                    return false;
            return true;
        }

        internal static IEnumerable<bool> UnifyArraysFast(object[] a1, object[] a2, PrologContext context)
        {
            int mark = context.MarkTrace();
            if (UnifyArrays(a1, a2, context))
                yield return false;
            context.RestoreVariables(mark);
        }

        /// <summary>
        /// Unifies two arrays of variable length
        /// </summary>
        internal static IEnumerable<bool> UnifyArrays(object[] a1, object[] a2)
        {
            if (a1.Length == a2.Length)
            {
                switch (a1.Length)
                {
                    case 0:
                        return UnifyArrays0(a1, a2);

                    case 1:
                        return UnifyArrays1(a1, a2);

                    case 2:
                        return UnifyArrays2(a1, a2);

                    case 3:
                        return UnifyArrays3(a1, a2);

                    case 4:
                        return UnifyArrays4(a1, a2);

                    case 5:
                        return UnifyArrays5(a1, a2);

                    case 6:
                        return UnifyArrays6(a1, a2);

                    case 7:
                        return UnifyArrays7(a1, a2);

                    case 8:
                        return UnifyArrays8(a1, a2);

                    case 9:
                        return UnifyArrays9(a1, a2);

                    case 10:
                        return UnifyArrays10(a1, a2);

                    default:
                        throw new ArgumentException("Attempting to unify arrays that are too long.");
                }
            }
            return FailEnumerator;
        }

        [SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "a2"), SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "a1")]
        // ReSharper disable UnusedParameter.Local
        private static IEnumerable<bool> UnifyArrays0(object[] a1, object[] a2)
        // ReSharper restore UnusedParameter.Local
        {
            yield return false;
        }

        private static IEnumerable<bool> UnifyArrays1(object[] a1, object[] a2)
        {
            return Unify(a1[0], a2[0]);
        }

        private static IEnumerable<bool> UnifyArrays2(object[] a1, object[] a2)
        {
            // ReSharper disable UnusedVariable
            foreach (var i00 in Unify(a1[0], a2[0]))
                foreach (var i10 in Unify(a1[0], a2[0]))
                    foreach (var i11 in Unify(a1[1], a2[1]))
                        yield return false;
            // ReSharper restore UnusedVariable
        }

        private static IEnumerable<bool> UnifyArrays3(object[] a1, object[] a2)
        {
            // ReSharper disable UnusedVariable
            foreach (var i20 in Unify(a1[0], a2[0]))
                foreach (var i21 in Unify(a1[1], a2[1]))
                    foreach (var i22 in Unify(a1[2], a2[2]))
                        yield return false;
            // ReSharper restore UnusedVariable
        }

        private static IEnumerable<bool> UnifyArrays4(object[] a1, object[] a2)
        {
            // ReSharper disable UnusedVariable
            foreach (var i40 in Unify(a1[0], a2[0]))
                foreach (var i41 in Unify(a1[1], a2[1]))
                    foreach (var i42 in Unify(a1[2], a2[2]))
                        foreach (var i43 in Unify(a1[3], a2[3]))
                            yield return false;
        }

        private static IEnumerable<bool> UnifyArrays5(object[] a1, object[] a2)
        {
            foreach (var i50 in Unify(a1[0], a2[0]))
                foreach (var i51 in Unify(a1[1], a2[1]))
                    foreach (var i52 in Unify(a1[2], a2[2]))
                        foreach (var i53 in Unify(a1[3], a2[3]))
                            foreach (var i54 in Unify(a1[4], a2[4]))
                                yield return false;
        }

        private static IEnumerable<bool> UnifyArrays6(object[] a1, object[] a2)
        {
            foreach (var i60 in Unify(a1[0], a2[0]))
                foreach (var i61 in Unify(a1[1], a2[1]))
                    foreach (var i62 in Unify(a1[2], a2[2]))
                        foreach (var i63 in Unify(a1[3], a2[3]))
                            foreach (var i64 in Unify(a1[4], a2[4]))
                                foreach (var i65 in Unify(a1[5], a2[5]))
                                    yield return false;
        }

        private static IEnumerable<bool> UnifyArrays7(object[] a1, object[] a2)
        {
            foreach (var i70 in Unify(a1[0], a2[0]))
                foreach (var i71 in Unify(a1[1], a2[1]))
                    foreach (var i72 in Unify(a1[2], a2[2]))
                        foreach (var i73 in Unify(a1[3], a2[3]))
                            foreach (var i74 in Unify(a1[4], a2[4]))
                                foreach (var i75 in Unify(a1[5], a2[5]))
                                    foreach (var i76 in Unify(a1[6], a2[6]))
                                        yield return false;
        }

        private static IEnumerable<bool> UnifyArrays8(object[] a1, object[] a2)
        {
            foreach (var i80 in Unify(a1[0], a2[0]))
                foreach (var i81 in Unify(a1[1], a2[1]))
                    foreach (var i82 in Unify(a1[2], a2[2]))
                        foreach (var i83 in Unify(a1[3], a2[3]))
                            foreach (var i84 in Unify(a1[4], a2[4]))
                                foreach (var i85 in Unify(a1[5], a2[5]))
                                    foreach (var i86 in Unify(a1[6], a2[6]))
                                        foreach (var i87 in Unify(a1[7], a2[7]))
                                            yield return false;
            // ReSharper restore UnusedVariable
        }

        private static IEnumerable<bool> UnifyArrays9(object[] a1, object[] a2)
        {
            // ReSharper disable UnusedVariable
            foreach (var i80 in Unify(a1[0], a2[0]))
                foreach (var i81 in Unify(a1[1], a2[1]))
                    foreach (var i82 in Unify(a1[2], a2[2]))
                        foreach (var i83 in Unify(a1[3], a2[3]))
                            foreach (var i84 in Unify(a1[4], a2[4]))
                                foreach (var i85 in Unify(a1[5], a2[5]))
                                    foreach (var i86 in Unify(a1[6], a2[6]))
                                        foreach (var i87 in Unify(a1[7], a2[7]))
                                            foreach (var i88 in Unify(a1[8], a2[8]))
                                                yield return false;
            // ReSharper restore UnusedVariable
        }

        private static IEnumerable<bool> UnifyArrays10(object[] a1, object[] a2)
        {
            // ReSharper disable UnusedVariable
            foreach (var i80 in Unify(a1[0], a2[0]))
                foreach (var i81 in Unify(a1[1], a2[1]))
                    foreach (var i82 in Unify(a1[2], a2[2]))
                        foreach (var i83 in Unify(a1[3], a2[3]))
                            foreach (var i84 in Unify(a1[4], a2[4]))
                                foreach (var i85 in Unify(a1[5], a2[5]))
                                    foreach (var i86 in Unify(a1[6], a2[6]))
                                        foreach (var i87 in Unify(a1[7], a2[7]))
                                            foreach (var i88 in Unify(a1[8], a2[8]))
                                                foreach (var i89 in Unify(a1[9], a2[9]))
                                                yield return false;
            // ReSharper restore UnusedVariable
        }
#pragma warning restore 414, 168, 219
        #endregion

        #region Checking unifiability
        /// <summary>
        /// Returns true if its arguments are unifiable but does not actually unify them.
        /// </summary>
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "v"), SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Unifiable")]
        public static bool Unifiable(object v1, object v2)
        {
            List<LogicVariable> vars = null;
            List<object> values = null;
            return Unifiable(v1, v2, ref vars, ref values);
        }

        internal static bool Unifiable(object v1, object v2, ref List<LogicVariable> vars, ref List<object> values)
        {
            v1 = CanonicalizeWithExplicitBindingList(v1, vars, values);
            v2 = CanonicalizeWithExplicitBindingList(v2, vars, values);
            if (v1 == v2)
                // Fast path
                return true;
            var l1 = v1 as LogicVariable;
            if (l1 != null)
            {
                // v1 is an unbound variable; bind it.
                AddBinding(l1, v2, ref vars, ref values);
                return true;
            }
            var l2 = v2 as LogicVariable;
            if (l2 != null)
            {
                // v2 is an unbound variable; bind it
                AddBinding(l2, v1, ref vars, ref values);
                return true;
            }
            // Neither is an unbound variable
            var s1 = v1 as Structure;
            var s2 = v2 as Structure;
            if (s1 != null && s2 != null)
            {
                // They're both structures
                if (s1.Functor != s2.Functor || s1.Arguments.Length != s2.Arguments.Length)
                    return false;
                for (int i = 0; i < s1.Arguments.Length; i++)
                    if (!Unifiable(s1.Arguments[i], s2.Arguments[i], ref vars, ref values))
                        return false;
                return true;
            }
            // None of the above
            if (v1 == null)
                return v2 == null;
            return v1.Equals(v2);
        }

        static object CanonicalizeWithExplicitBindingList(object value, List<LogicVariable> vars, List<object> values)
        {
            value = Deref(value);
            if (vars == null)
                return value;
            var lv = value as LogicVariable;
            while (lv != null)
            {
                int bindingPosition;
                if ((bindingPosition = vars.IndexOf(lv)) >= 0)
                {
                    // it's aliased by the binding list
                    value = values[bindingPosition];
                    lv = value as LogicVariable;
                }
                else
                    // It's not in the binding list, so we're done.
                    return value;
            }
            return value;
        }

        static void AddBinding(LogicVariable lv, object value, ref List<LogicVariable> vars, ref List<object> values)
        {
            if (vars == null)
            {
                vars = new List<LogicVariable> { lv };
                values = new List<object> { value };
            }
            else
            {
                vars.Add(lv);
                values.Add(value);
            }
        }
        #endregion

        #region Alpha conversion and copying
        /// <summary>
        /// Substitutes occurances of newVars for all occurances of oldVars in argList.
        /// Returns new array, if substitutions were made, the original array if not.
        /// Original array is not modified
        /// </summary>
        internal static object[] AlphaConvertArglist(object[] argList, List<LogicVariable> oldVars, LogicVariable[] newVars, PrologContext context, bool evalIndexicals)
        {
            object[] newArgs = null;
            for (int i = 0; i < argList.Length; i++)
            {
                var term = argList[i] as AlphaConvertibleTerm;
                if (term != null)
                {
                    object converted = term.AlphaConvert(oldVars, newVars, context, evalIndexicals);
                    if (converted != argList[i])
                    {
                        if (newArgs == null)
                        {
                            newArgs = new object[argList.Length];
                            argList.CopyTo(newArgs, 0);
                        }
                        newArgs[i] = converted;
                    }
                }
            }
            // Return newArgs unless it turns out it was never actually allocated
            // (in which case none of the args had renamed vars)
            return newArgs ?? argList;
        }

        /// <summary>
        /// Recopy the term to replace bound variables with their values and alpha convert any unbound variables.
        /// This has the effect of removing interference between this term and the copy should one or the other
        /// have its bindings changed (either through unification or backtracking).
        /// </summary>
        public static object CopyInstantiation(object term)
        {
            return CopyInstantiation(term, new Dictionary<LogicVariable, LogicVariable>());
        }

        static object CopyInstantiation(object term, Dictionary<LogicVariable,LogicVariable> subs)
        {
            term = Deref(term);
            var l = term as LogicVariable;
            if (l != null)
            {
                LogicVariable sub;
                if (subs.TryGetValue(l, out sub))
                    return sub;
                var l2 = new LogicVariable(l.Name);
                subs[l] = l2;
                return l2;
            }
            var t = term as Structure;
            if (t == null)
                return term;
            var newArgs = new object[t.Arguments.Length];
            for (int i = 0; i < newArgs.Length; i++)
                newArgs[i] = CopyInstantiation(t.Argument(i), subs);
            return new Structure(t.Functor, newArgs);
        }
        #endregion

        #region Syntactic equality
        /// <summary>
        /// True if the two objects are syntactically identical.
        /// </summary>
        /// <returns></returns>
        public static bool Identical(object a, object b)
        {
            a = Deref(a);
            b = Deref(b);
            var x = a as Structure;
            var y = b as Structure;

            if (x == null)
            {
                if (a == null)
                    return null == b;
                return a.Equals(b);
            }
            if (y != null)
            {
                // They're both structures
                if (x.Functor != y.Functor || x.Arguments.Length != y.Arguments.Length)
                    return false;
                for (int i = 0; i < x.Arguments.Length; i++)
                    if (!Identical(x.Arguments[i], y.Arguments[i]))
                        return false;
                return true;
            }
            // a is a structure, b is not
            return false;
        }
        #endregion

        #region Prolog-format output
#if !OldPrologWriter
        /// <summary>
        /// Converts an arbitrary object to a string in Prolog format.
        /// </summary>
        public static string ToStringInPrologFormat(object value)
        {
            return ISOPrologWriter.WriteToString(value);
        }
#else
        public static string ToStringInPrologFormat(object value)
        {
            var sb = new StringBuilder();
            Write(sb, value);
            return sb.ToString();
        }

        internal static void Write(StringBuilder s, object term)
        {
            term = Canonicalize(term);
            if (term == null)
                s.Append("null");
            else
            {
                Structure t = term as Structure;
                if (t == null)
                {
                    Symbol sym = term as Symbol;
                    if (sym != null)
                    {
                        if (IsQuoteNeeded(sym))
                        {
                            s.Append('\'');
                            s.Append(sym.Name);
                            s.Append('\'');
                        }
                        else
                            s.Append(sym.Name);
                    }
                    else
                    {
                        string str = term as string;
                        if (str != null)
                        {
                            s.Append('"');
                            foreach (char ch in str)
                                switch (ch)
                                {
                                    case '"':
                                        s.Append("\\\"");
                                        break;

                                    case '\\':
                                        s.Append("\\\\");
                                        break;

                                    default:
                                        s.Append(ch);
                                        break;
                                }
                            s.Append('"');
                        }
                        else
                            s.Append(term);
                    }
                }
                else
                    t.ToStringBuilder(s);
            }
        }

        internal static void WriteAndPossiblyParenthesize(StringBuilder s, object o)
        {
            Structure t = o as Structure;
            if (t != null && t.IsFunctor(Symbol.Comma, 2))
            {
                s.Append('(');
                Write(s, o);
                s.Append(')');
            }
            else
            {
                Write(s, o);
            }
        }
        
        static bool IsQuoteNeeded(Symbol s)
        {
            string name = s.Name;
            if (Char.IsUpper(name[0]))
                return true;
            foreach (char ch in name)
                if (Char.IsWhiteSpace(ch))
                    return true;
            return false;
        }
#endif

        internal string DebuggerDisplay
        {
            get
            {
                return ToStringInPrologFormat(this);
            }
        }
        #endregion

        #region Utilities
        /// <summary>
        /// An iterator that always fails.
        /// </summary>
        static IEnumerable<bool> MakeFailEnumerator()
        {
            yield break;
        }

        internal static IEnumerable<bool> FailEnumerator = MakeFailEnumerator(); 

        internal static IEnumerable<bool> MakeSucceedEnumerator()
        {
            yield return false;
        }

        internal static IEnumerable<bool> ToEnumerator(bool succeed)
        {
            return succeed ? MakeSucceedEnumerator() : FailEnumerator;
        }
        #endregion

        internal static Structure PredicateIndicatorExpression(Structure term)
        {
            return new Structure(Symbol.Slash, term.Functor, term.Arguments.Length);
        }

        internal static Structure PredicateIndicatorExpression(Symbol functor, int arity)
        {
            return new Structure(Symbol.Slash, functor, arity);
        }
    }
}
