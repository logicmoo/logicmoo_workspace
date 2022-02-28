using System;
using System.Collections.Generic;

using UnityEngine;

namespace Prolog
{
    internal static class ELProlog
    {
        public const string NonExclusiveOperator = "/";
        public static readonly Symbol SNonExclusiveOperator = Symbol.Intern(NonExclusiveOperator);
        public const string ExclusiveOperator = ":";
        public static readonly Symbol SExclusiveOperator = Symbol.Intern(ExclusiveOperator);
        public const string BindNodeOperator = ">>";
        public static readonly Symbol SBindNodeOperator = Symbol.Intern(BindNodeOperator);

        public static bool IsELTerm(object term)
        {
            var s = term as Structure;
            return s != null
                && s.Arity==2
                && (s.Functor == SBindNodeOperator 
                    || s.Functor == SExclusiveOperator
                    || s.Functor == SNonExclusiveOperator);
        }

        #region Queries
        public static bool TryQuery(object term, PrologContext context, out ELNode foundNode, out ELNodeEnumerator enumerator)
        {
            // Dereference any top-level variables.
            var t = Term.Deref(term);

            // Dereference indexicals
            var i = t as Indexical;
            if (i != null)
                t = i.GetValue(context);

            // A game object means the gameobject's EL KB.
            var g = t as GameObject;
            if (g != null)
                t = g.KnowledgeBase().ELRoot;

            // If it's already an ELNode, use that.
            var n = t as ELNode;
            if (n != null)
            {
                foundNode = n;
                enumerator = null;
                return true;
            }

            // Otherwise, it's an expression, so evaluate it.
            var s = t as Structure;
            if (s != null)
                return TryQueryStructure(s, context, out foundNode, out enumerator);

            var v = t as LogicVariable;
            if (v != null && !v.IsBound)
                throw new Exception("EL query root is an unbound variable: " + v);
            throw new Exception("Malformed EL query: " + ISOPrologWriter.WriteToString(term));
        }

        public static bool TryQueryStructure(
            Structure term,
            PrologContext context,
            out ELNode foundNode,
            out ELNodeEnumerator enumerator)
        {
            //
            // Dispatch based on the functor and arity.
            //

            // Handle root queries, i.e. /Key
            if (term.IsFunctor(Symbol.Slash, 1))
                return TryRootQuery(term, context, out foundNode, out enumerator);

            if (!IsELTerm(term))
                throw new Exception("Malformed EL query: " + ISOPrologWriter.WriteToString(term));

            if (term.IsFunctor(SBindNodeOperator, 2))
            {
                var variableToBind = term.Argument(1) as LogicVariable;
                if (variableToBind == null)
                    throw new ArgumentException("RHS of >> must be an uninstantiated variable: "+ ISOPrologWriter.WriteToString(term.Argument(1)));
                foundNode = null;
                return TryNodeBindingQuery(out enumerator, term.Argument(0), variableToBind, context);
            }

            return TryChildQuery(
                out foundNode,
                out enumerator,
                term.Argument(0),
                term.Argument(1),
                term.Functor == Symbol.Colon,
                context);
        }

        public static bool TryNodeBindingQuery(
            out ELNodeEnumerator enumerator,
            object nodeExpression,
            LogicVariable variableToBind,
            PrologContext context)
        {
            // Decode the node expression
            ELNode foundNode;
            ELNodeEnumerator nodeEnumerator;
            if (!TryQuery(nodeExpression, context, out foundNode, out nodeEnumerator))
            {
                // Parent failed, so we fail
                enumerator = null;
                return false;
            }
            enumerator = (foundNode != null)
                ? (ELNodeEnumerator)new ELNodeEnumeratorBindFixedNodeToVariable(foundNode, variableToBind)
                : new ELNodeEnumeratorBindEnumeratedNodesToVariable(nodeEnumerator, variableToBind);
            return true;
        }

        /// <summary>
        /// Binds a specific node rather than a key (i.e. >> rather than / or :) to a variable
        /// </summary>
        class ELNodeEnumeratorBindFixedNodeToVariable : ELNodeEnumerator
        {
            private readonly ELNode node;
            private readonly LogicVariable variableToBind;

            public ELNodeEnumeratorBindFixedNodeToVariable(ELNode node, LogicVariable variableToBind)
            {
                this.node = node;
                this.variableToBind = variableToBind;
            }

            public override bool BindsVar(LogicVariable v)
            {
                return v == variableToBind;
            }

            public override bool MoveNext()
            {
                if (Current == null)
                {
                    // first time through
                    variableToBind.Value = Current = node;
                    return true;
                }
                variableToBind.ForciblyUnbind();
                return false;
            }
        }

        /// <summary>
        /// Binds a set of nodes rather than keys (i.e. >> rather than / or :) to a variable
        /// </summary>
        class ELNodeEnumeratorBindEnumeratedNodesToVariable : ELNodeEnumerator
        {
            readonly ELNodeEnumerator nodeEnumerator;

            readonly LogicVariable variableToBind;

            public ELNodeEnumeratorBindEnumeratedNodesToVariable(ELNodeEnumerator nodeEnumerator, LogicVariable variableToBind)
            {
                this.nodeEnumerator = nodeEnumerator;
                this.variableToBind = variableToBind;
                if (nodeEnumerator.BindsVar(variableToBind))
                    throw new InvalidOperationException("Variable appears on both the LHS and RHS of >>: "+ variableToBind.Name);
            }

            public override bool BindsVar(LogicVariable v)
            {
                return v == variableToBind || nodeEnumerator.BindsVar(v);
            }

            public override bool MoveNext()
            {
                if (nodeEnumerator.MoveNext())
                {
                    Current = nodeEnumerator.Current;
                    variableToBind.Value = Current;
                    return true;
                }
                variableToBind.ForciblyUnbind();
                return false;
            }
        }

        public static bool TryChildQuery(
            out ELNode foundNode,
            out ELNodeEnumerator enumerator,
            object parentExpression,
            object keyExpression,
            bool isExclusive,
            PrologContext context)
        {
            // Decode the parent expression
            ELNode parentNode;
            ELNodeEnumerator parentEnumerator;
            if (!TryQuery(parentExpression, context, out parentNode, out parentEnumerator))
            {
                // Parent failed, so we fail
                enumerator = null;
                foundNode = null;
                return false;
            }

            //
            // Decode the key argument
            //
            var key = keyExpression;
            var v = key as LogicVariable;

            return isExclusive?TryExclusiveQuery(out foundNode, out enumerator, parentNode, parentEnumerator, key, v)
                : TryNonExclusiveQuery(out foundNode, out enumerator, parentNode, key, v, parentEnumerator);
        }

        private static bool TryExclusiveQuery(
            out ELNode foundNode,
            out ELNodeEnumerator enumerator,
            ELNode parentNode,
            ELNodeEnumerator parentEnumerator,
            object key,
            LogicVariable v)
        {
            //
            // Expression is Parent:Something
            //
            if (parentNode != null)
            {
                return TryExclusiveQueryDeterministicParent(out foundNode, out enumerator, parentNode, key, v);
            }

            return TryExclusiveQueryEnumeratedParent(out foundNode, out enumerator, parentEnumerator, key, v);
        }

        private static bool TryExclusiveQueryEnumeratedParent(
            out ELNode foundNode,
            out ELNodeEnumerator enumerator,
            ELNodeEnumerator parentEnumerator,
            object key,
            LogicVariable v)
        {
            // Non-deterministic parent path
            // NonUniqueParent:Something
            foundNode = null;

            enumerator = (v == null)
                ? new ELNodeEnumeratorEnumerateParentAndLookupExclusiveKey(parentEnumerator, key)
                : (parentEnumerator.BindsVar(v)? (ELNodeEnumerator)new ELNodeEnumeratorPreboundVariable(parentEnumerator, v, true)
                                                 : new ELNodeEnumeratorEnumerateParentAndBindVariable(parentEnumerator, v));
            return true;
        }

        private static bool TryExclusiveQueryDeterministicParent(
            out ELNode foundNode,
            out ELNodeEnumerator enumerator,
            ELNode parentNode,
            object key,
            LogicVariable v)
        {
            // Deterministic parent path
            // UniqueParent:Something

            if (parentNode.IsNonExclusive)
            {
                throw new ELNodeExclusionException("Exclusive query of an non-exclusive node", parentNode, key);
            }

            if (v == null)
            {
                // Deterministic child path
                // UniqueParent:Key
                enumerator = null;
                return parentNode.TryLookup(key, out foundNode);
            }

            // Enumerated child path
            // UniqueParent:Variable
            if (parentNode.Children.Count > 0)
            {
                foundNode = null;
                enumerator = new ELNodeEnumeratorBindAndUnbindVariable(parentNode.Children[0], v);
                return true;
            }

            // parentNode is exclusive, but is childless, so we can't match.
            foundNode = null;
            enumerator = null;
            return false;
        }

        private static bool TryNonExclusiveQuery(out ELNode foundNode, out ELNodeEnumerator enumerator, ELNode parentNode, object key, LogicVariable v, ELNodeEnumerator parentEnumerator)
        {
            if (parentNode != null)
            {
                return TryNonExclusiveQueryDeterministicParent(out foundNode, out enumerator, parentNode, key, v);
            }
            return TryNonExclusiveQueryEnumeratedParent(out foundNode, out enumerator, key, v, parentEnumerator);
        }

        private static bool TryNonExclusiveQueryEnumeratedParent(
            out ELNode foundNode,
            out ELNodeEnumerator enumerator,
            object key,
            LogicVariable v,
            ELNodeEnumerator parentEnumerator)
        {
            // Enumerated parent path
            // NonUniqueParent/Something
            foundNode = null;
            if (v == null)
            {
                // NonUniqueParent/Key
                // Enumerate parent, then do deterministic lookup for child.
                enumerator = new ELNodeEnumeratorFixedChildFromParentEnumerator(parentEnumerator, key);
                return true;
            }
            if (parentEnumerator.BindsVar(v))
            {
                // We're doing a search for a variable that's aready bound.
                enumerator = new ELNodeEnumeratorPreboundVariable(parentEnumerator, v, false);
                return true;
            }
            // NonUniqueParent/Variable
            // Enumerate both parent and child.
            enumerator = new ELNodeEnumeratorLogicVariableFromParentEnumerator(parentEnumerator, v);
            return true;
        }

        private static bool TryNonExclusiveQueryDeterministicParent(
            out ELNode foundNode,
            out ELNodeEnumerator enumerator,
            ELNode parentNode,
            object key,
            LogicVariable v)
        {
            // Deterministic parent path
            // The expression is UniqueParent/Something
            if (parentNode.IsExclusive)
            {
                throw new ELNodeExclusionException("Non-exclusive query of an exclusive node", parentNode, key);
            }

            if (v == null)
            {
                // fully deterministic path
                // UniqueParent/Key corresponds to at most one ELNode.
                enumerator = null;
                return parentNode.TryLookup(key, out foundNode);
            }
            // UniqueParent/Variable, so do a singly-nested iteration.
            foundNode = null;
            enumerator = new ELNodeEnumeratorLogicVariableFromNode(parentNode, v);
            return true;
        }

        private static bool TryRootQuery(Structure term, PrologContext context, out ELNode foundNode, out ELNodeEnumerator enumerator)
        {
            // Expression is /Key.
            var arg0 = term.Argument(0);

            // This is a "/constant" expression, i.e. a top-level lookup.
            if (arg0 is LogicVariable)
            {
                throw new NotImplementedException("Lookups of the form /Variable are not supported.");
            }
            enumerator = null;
            return context.KnowledgeBase.ELRoot.TryLookup(arg0, out foundNode);
        }

        /// <summary>
        /// Enumerate the nodes whose keys are the same as the value in a (prebound) variable.
        /// </summary>
        class ELNodeEnumeratorPreboundVariable : ELNodeEnumerator
        {
            public ELNodeEnumeratorPreboundVariable(ELNodeEnumerator parentEnumerator, LogicVariable variable, bool exclusive)
            {
                this.parentEnumerator = parentEnumerator;
                this.variable = variable;
                this.exclusive = exclusive;
            }

            private readonly ELNodeEnumerator parentEnumerator;

            private readonly LogicVariable variable;

            private readonly bool exclusive;

            public override bool MoveNext()
            {
                while (parentEnumerator.MoveNext())
                {
                    if (exclusive)
                    {
                        if (parentEnumerator.Current.IsNonExclusive)
                            throw new ELNodeExclusionException("Exclusive query of an non-exclusive node", parentEnumerator.Current, variable.Value);
                    }
                    else if (parentEnumerator.Current.IsExclusive)
                        throw new ELNodeExclusionException("Non-exclusive query of an exclusive node", parentEnumerator.Current, variable.Value);
                    foreach (var c in parentEnumerator.Current.Children)
                    {
                        if (c.Key.Equals(variable.Value))
                        {
                            Current = c;
                            return true;
                        }
                    }
                }
                return false;
            }

            public override bool BindsVar(LogicVariable v)
            {
                return parentEnumerator.BindsVar(v);
            }
        }

        /// <summary>
        /// Enumerate the (non-exclusive) children of a node and bind their keys to a logic variable.
        /// </summary>
        class ELNodeEnumeratorLogicVariableFromNode : ELNodeEnumerator
        {
            public ELNodeEnumeratorLogicVariableFromNode(ELNode parentNode, LogicVariable v)
            {
                this.parentNode = parentNode;
                this.variable = v;
                childIndex = 0;
            }

            private int childIndex;

            private readonly ELNode parentNode;

            private readonly LogicVariable variable;

            public override bool MoveNext()
            {
                if (this.childIndex < parentNode.Children.Count)
                {
                    Current = parentNode.Children[this.childIndex++];
                    this.variable.Value = Term.CopyInstantiation(Current.Key);
                    return true;
                }
                this.variable.ForciblyUnbind();
                return false;
            }

            public override bool BindsVar(LogicVariable v)
            {
                return v == this.variable;
            }
        }

        /// <summary>
        /// For each node enumerated by the parent, find its unique child with the specified key, if any.
        /// </summary>
        class ELNodeEnumeratorFixedChildFromParentEnumerator : ELNodeEnumerator
        {
            public ELNodeEnumeratorFixedChildFromParentEnumerator(ELNodeEnumerator parentEnumerator, object childKey)
            {
                this.parentEnumerator = parentEnumerator;
                this.childKey = childKey;
            }

            private readonly ELNodeEnumerator parentEnumerator;

            private readonly object childKey;

            public override bool MoveNext()
            {
                while (parentEnumerator.MoveNext())
                {
                    // ReSharper disable once PossibleNullReferenceException
                    if (parentEnumerator.Current.IsExclusive)
                        throw new ELNodeExclusionException("Non-exclusive query of an exclusive node", parentEnumerator.Current, childKey);
                    // ReSharper disable once PossibleNullReferenceException
                    if (parentEnumerator.Current.TryLookup(childKey, out Current))
                        return true;
                }
                return false;
            }

            public override bool BindsVar(LogicVariable v)
            {
                return parentEnumerator.BindsVar(v);
            }
        }

        /// <summary>
        /// For each node enumerated by the parent, enumerate all its child nodes,
        /// and bind a LogicVariable to their keys.
        /// </summary>
        class ELNodeEnumeratorLogicVariableFromParentEnumerator : ELNodeEnumerator
        {
            public ELNodeEnumeratorLogicVariableFromParentEnumerator(ELNodeEnumerator parentEnumerator, LogicVariable v)
            {
                this.parentEnumerator = parentEnumerator;
                this.variable = v;
                childIndex = -1;
            }

            private readonly ELNodeEnumerator parentEnumerator;

            private readonly LogicVariable variable;

            private int childIndex;

            public override bool MoveNext()
            {
                retry:

                // First, try the next child of the current parent.
                if (childIndex >= 0)
                {
                    Current = parentEnumerator.Current.Children[childIndex--];
                    this.variable.Value = Term.CopyInstantiation(Current.Key);
                    return true;
                }

                // Ran out of children on the current parent.
                if (parentEnumerator.MoveNext())
                {
                    // ReSharper disable once PossibleNullReferenceException
                    if (parentEnumerator.Current.IsExclusive)
                        throw new ELNodeExclusionException(
                            "Non-exclusive query of an exclusive node",
                            parentEnumerator.Current,
                            this.variable);

                    childIndex = parentEnumerator.Current.Children.Count - 1;

                    goto retry;
                }
                this.variable.ForciblyUnbind();
                return false;
            }

            public override bool BindsVar(LogicVariable v)
            {
                return v == this.variable || parentEnumerator.BindsVar(v);
            }
        }

        /// <summary>
        /// This doesn't really enumerate children, since there's only a single, fixed child.
        /// But we structure it as an enumerator so that we get a callback after the one child
        /// is processed, and that lets us unbind the variable we bound.
        /// </summary>
        class ELNodeEnumeratorBindAndUnbindVariable : ELNodeEnumerator
        {
            public ELNodeEnumeratorBindAndUnbindVariable(ELNode child, LogicVariable v)
            {
                this.child = child;
                this.variable = v;
            }

            private readonly ELNode child;
            private readonly LogicVariable variable;

            public override bool MoveNext()
            {
                if (this.variable.IsBound)
                {
                    // We've already been through it once, so unbind the variable and fail.
                    this.variable.ForciblyUnbind();
                    return false;
                }
                
                // This is our first time through, so bind the variable and succeed.
                Current = child;
                this.variable.Value = Term.CopyInstantiation(child.Key);
                return true;
            }

            public override bool BindsVar(LogicVariable v)
            {
                return v == this.variable;
            }
        }

        /// <summary>
        /// For each node enumerated by parent, check if it has a specific key.
        /// </summary>
        class ELNodeEnumeratorEnumerateParentAndLookupExclusiveKey : ELNodeEnumerator
        {
            public ELNodeEnumeratorEnumerateParentAndLookupExclusiveKey(ELNodeEnumerator parentEnumerator, object key)
            {
                this.parentEnumerator = parentEnumerator;
                this.key = key;
            }

            private readonly ELNodeEnumerator parentEnumerator;

            private readonly object key;

            public override bool MoveNext()
            {
                while (parentEnumerator.MoveNext())
                {
                    if (parentEnumerator.Current.IsNonExclusive)
                    {
                        throw new ELNodeExclusionException("Exclusive query of an non-exclusive node", parentEnumerator.Current, key);
                    }
                    if (parentEnumerator.Current.TryLookup(key, out Current))
                        return true;
                }
                return false;
            }

            public override bool BindsVar(LogicVariable v)
            {
                return parentEnumerator.BindsVar(v);
            }
        }

        /// <summary>
        /// For each node enumerated by parent, bind LogicVariable to its unique child
        /// </summary>
        class ELNodeEnumeratorEnumerateParentAndBindVariable : ELNodeEnumerator
        {
            public ELNodeEnumeratorEnumerateParentAndBindVariable(ELNodeEnumerator parentEnumerator, LogicVariable variable)
            {
                this.parentEnumerator = parentEnumerator;
                this.variable = variable;
            }

            private readonly ELNodeEnumerator parentEnumerator;

            private readonly LogicVariable variable;

            public override bool MoveNext()
            {
                while (parentEnumerator.MoveNext())
                {
                    if (parentEnumerator.Current.IsNonExclusive)
                    {
                        throw new ELNodeExclusionException("Exclusive query of an non-exclusive node", parentEnumerator.Current, this.variable);
                    }
                    if (parentEnumerator.Current.Children.Count > 0)
                    {
                        Current = parentEnumerator.Current.Children[0];
                        this.variable.Value = Term.CopyInstantiation(Current.Key);
                        return true;
                    }
                }
                this.variable.ForciblyUnbind();
                return false;
            }

            public override bool BindsVar(LogicVariable v)
            {
                return v == this.variable || parentEnumerator.BindsVar(v);
            }
        }
        #endregion

        #region Assertion
        //
        // these write a single nodes, so they don't need to loop like queries do.
        //

        /// <summary>
        /// Write TERM to EL KB, creating any nodes that need to be cerated.
        /// </summary>
        /// <param name="term">Prolog-format term to store into KB</param>
        /// <param name="knowledgeBase">KB in which to assert the term.</param>
        /// <returns></returns>
        public static ELNode Update(object term, KnowledgeBase knowledgeBase)
        {
            term = Term.Deref(term);
            var s = term as Structure;
            if (s != null)
                return UpdateStructure(s, knowledgeBase);
            var n = term as ELNode;
            if (n != null)
                return n;

            throw new Exception("Malformed EL assertion: " + ISOPrologWriter.WriteToString(term));
        }

        public static ELNode UpdateStructure(Structure term, KnowledgeBase knowledgeBase)
        {
            if (term.Functor == Symbol.Slash)
            {
                if (term.Arity == 1)
                    return knowledgeBase.ELRoot.StoreNonExclusive(Term.CopyInstantiation(term.Argument(0)));
                return Update(term.Argument(0), knowledgeBase).StoreNonExclusive(Term.CopyInstantiation(term.Argument(1)));
            }
            if (term.Functor == Symbol.Colon)
            {
                return Update(term.Argument(0), knowledgeBase).StoreExclusive(Term.CopyInstantiation(term.Argument(1)), true); 
            }
            throw new Exception("Malformed EL assertion: "+ISOPrologWriter.WriteToString(term));
        }
        #endregion

        #region Retraction
        internal static System.Collections.Generic.IEnumerable<CutState> Retract(object term, PrologContext context)
        {
            ELNode foundNode;
            ELNodeEnumerator enumerator;
            if (!TryQuery(term, context, out foundNode, out enumerator))
                return CutStateSequencer.Fail();
            if (foundNode != null)
            {
                foundNode.DeleteSelf();
                return CutStateSequencer.Succeed();
            }
            return DeleteSuccessive(enumerator);
        }

        private static System.Collections.Generic.IEnumerable<CutState> DeleteSuccessive(ELNodeEnumerator enumerator)
        {
            while (enumerator.MoveNext())
            {
                enumerator.Current.DeleteSelf();
                yield return CutState.Continue;
            }
        }

        internal static void RetractAll(object term, PrologContext context)
        {
            ELNode foundNode;
            ELNodeEnumerator enumerator;
            if (!TryQuery(term, context, out foundNode, out enumerator))
                return;
            if (foundNode != null)
                foundNode.DeleteSelf();
            else
                while (enumerator.MoveNext()) enumerator.Current.DeleteSelf();
        }
        #endregion
    }
}
