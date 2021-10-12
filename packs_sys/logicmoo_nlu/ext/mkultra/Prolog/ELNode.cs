// // --------------------------------------------------------------------------------------------------------------------
// // <copyright file="KnowledgeBaseEntry.cs" company="Ian Horswill">
// //   Copyright (C) 2013 Ian Horswill
// //   
// //   Permission is hereby granted, free of charge, to any person obtaining a copy of
// //   this software and associated documentation files (the "Software"), to deal in the
// //   Software without restriction, including without limitation the rights to use, copy,
// //   modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
// //   and to permit persons to whom the Software is furnished to do so, subject to the
// //   following conditions:
// //   
// //   The above copyright notice and this permission notice shall be included in all
// //   copies or substantial portions of the Software.
// //   
// //   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// //   INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// //   PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// //   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// //   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// //   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// // </copyright>
// // --------------------------------------------------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Prolog
{
    /// <summary>
    /// A node in the trie representing a collection of exclusion logic assertions
    /// </summary>
    [DebuggerDisplay("{Name}")]
    public class ELNode
    {
        #region Printing
        /// <summary>
        /// Unparses the entry into key+key+key format
        /// </summary>
        /// <returns>Name in key+key+key format</returns>
        public override string ToString()
        {
            return Name;
        }

        /// <summary>
        /// Unparses the entry into key+key+key format
        /// </summary>
        public string Name
        {
            get
            {
                if (Parent == null)
                    return "/";
                var b = new StringBuilder();
                this.BuildName(b);
                return b.ToString();
            }
        }

        void BuildName(StringBuilder b)
        {
            if (Parent != null && Parent.Parent != null)
            {
                Parent.BuildName(b);
                b.Append(Parent.mode == ExclusionMode.Exclusive ? ELProlog.ExclusiveOperator : ELProlog.NonExclusiveOperator);
            }
            b.Append(Key);
        }
        #endregion
        
        internal ELNode(ELNode parent, object key)
        {
            Key = key;
            Parent = parent;
            Children = EmptyChildren;
        }

        #region Fields and properties
        /// <summary>
        /// Parent node of this KB node.  Used primarily for printing.
        /// </summary>
        public ELNode Parent { get; private set; }

        /// <summary>
        /// The value associated with this node.
        /// </summary>
        public object Key;
        /// <summary>
        /// List of the child nodes of this node.
        /// </summary>
        public List<ELNode> Children { get; private set; }

        /// <summary>
        /// An empty list.  Cached so there need only be one lying around.
        /// </summary>
        static readonly List<ELNode> EmptyChildren = new List<ELNode>(); 

        private enum ExclusionMode { Empty, Exclusive, NonExclusive };

        private ExclusionMode mode;

        public bool IsExclusive
        {
            get
            {
                return mode == ExclusionMode.Exclusive;
            }
        }

        public bool IsNonExclusive
        {
            get
            {
                return mode == ExclusionMode.NonExclusive;
            }
        }

        public string ModeString
        {
            get
            {
                switch (mode)
                {
                    case ExclusionMode.Empty:
                        return "";

                        case ExclusionMode.Exclusive:
                        return ":";
                        case ExclusionMode.NonExclusive:
                        return "/";

                    default:
                        return "<invalid exclusion mode!>";
                }
            }
        }
        #endregion

        #region Accessors
        /// <summary>
        /// Test if this node contains a child with a given key.
        /// </summary>
        /// <param name="key">Key to search for</param>
        /// <returns>True if there is a child with the specified key.</returns>
        public bool ContainsKey(object key)
        {
            foreach (var n in Children)
                if (Equals(key, n.Key))
                    return true;
            return false;
        }

        /// <summary>
        /// Attempts to look up a child with the specified key.
        /// </summary>
        /// <param name="key">Key to search for</param>
        /// <param name="child">Child with that key, if found, else null.</param>
        /// <returns>True if child was found, else false and child is null.</returns>
        public bool TryLookup(object key, out ELNode child)
        {
            foreach (var c in Children)
            {
                if (Equals(c.Key, key))
                {
                    child = c;
                    return true;
                }
            }
            child = null;
            return false;
        }

        /// <summary>
        /// Returns the child with the specified key or null is no such child.
        /// </summary>
        /// <param name="key">Key to search for</param>
        /// <returns>The child, if there is a child with that key, otherwise null.</returns>
        public ELNode ChildWithKey(object key)
        {
            foreach (var c in Children)
                if (Equals(c.Key, key))
                {
                    return c;
                }
            return null;
        }

        /// <summary>
        /// Return the value of this node's exclusive child's key.
        /// Throw exception if its not exclusive, doesn't have a child, or the key has the wrong type.
        /// </summary>
        /// <typeparam name="T">Type expected for the key</typeparam>
        /// <returns>Value of the child key.</returns>
        public T ExclusiveKeyValue<T>()
        {
            if (!IsExclusive)
                throw new ArgumentException("ExclusiveKeyValue called on non-exclusive or empty node: "+this);
            var child = Children[0];
            if (!(child.Key is T))
                throw new ArgumentException(string.Format("Node {0} has wrong type; should be {1}.", child, typeof(T).Name));
            return (T)child.Key;
        }
        #endregion

        #region Mutators
        /// <summary>
        /// A do-nothing procedure used to make clear that a / % expression in C# is really intended to do a store.
        /// </summary>
        /// <param name="ignore">The ELNode that got stored.</param>
        public static void Store(ELNode ignore)
        {
            // Does nothing
        }
        ///// <summary>
        ///// Store an exclusive child inside this node.
        ///// </summary>
        ///// <param name="v">Bound variable holding the key for the child</param>
        ///// <param name="overwrite">If true, this will overwrite any existing child with a different value.</param>
        ///// <returns>The child node</returns>
        ///// <exception cref="KBExclusionException">If a non-exclusive child has already been written.</exception>
        //public ELNode StoreExclusive(Variable v, bool overwrite)
        //{
        //    return StoreExclusive(v.Value, overwrite);
        //}

        /// <summary>
        /// Store an exclusive child inside this node.
        /// </summary>
        /// <param name="key">Key for the child</param>
        /// <param name="overwrite">If true, this will overwrite any existing child with a different value.</param>
        /// <returns>The child node</returns>
        /// <exception cref="ELNodeExclusionException">If a non-exclusive child has already been written.</exception>
        public ELNode StoreExclusive(object key, bool overwrite)
        {
            switch (mode)
            {
                case ExclusionMode.Empty:
                    {
                        mode = ExclusionMode.Exclusive;
                        var result = new ELNode(this, key);
                        if (this.Children.Count==0)
                            this.Children = new List<ELNode> { result };
                        else
                            this.Children.Add(result);

                        return result;
                    }

                case ExclusionMode.NonExclusive:
                    throw new ELNodeExclusionException("Exclusive store on non-exclusive node.", this, key);


                case ExclusionMode.Exclusive:
                    if (overwrite)
                    {
                        if (this.Children.Count>0)
                            this.Children[0].OverwriteExclusive(key);
                        else
                        {
                            this.Children.Add(new ELNode(this, key));
                        }
                    }
                    else if (key != this.Children[0].Key)
                        throw new ELNodeExclusionException("Exclusive store doesn't match previous store.", this, key);
                    return this.Children[0];

                default:
                    throw new InvalidOperationException("Invalid exclusion mode");
            }
        }

        ///// <summary>
        ///// Store a non-exclusive child inside this node.
        ///// </summary>
        ///// <param name="v">Bound variable holding the key for the child</param>
        ///// <returns>The child node</returns>
        ///// <exception cref="KBExclusionException">If an exclusive child has already been written.</exception>
        //public ELNode StoreNonExclusive(Variable v)
        //{
        //    return StoreNonExclusive(v.Value);
        //}

        /// <summary>
        /// Store a non-exclusive child inside this node.
        /// </summary>
        /// <param name="key">Key for the new child</param>
        /// <returns>The child node</returns>
        /// <exception cref="ELNodeExclusionException">If an exclusive child has already been written.</exception>
        public ELNode StoreNonExclusive(object key)
        {
            ELNode result =null;
            switch (mode)
            {
                case ExclusionMode.Empty:
                    {
                        mode = ExclusionMode.NonExclusive;
                        result = new ELNode(this, key);
                        if (this.Children == EmptyChildren)
                            this.Children = new List<ELNode> { result };
                        else
                            this.Children.Add(result);
                    }
                    break;

                case ExclusionMode.Exclusive:
                    throw new ELNodeExclusionException("Non-exclusive store on exclusive node.", this, key);

                case ExclusionMode.NonExclusive:
                    foreach (var c in this.Children)
                    {
                        if (c.Key == key)
                            return c;
                    }
                    this.Children.Add(result = new ELNode(this, key));
                    break;
            }
            return result;
        }

        private void OverwriteExclusive(object key)
        {
            if (key != Key)
            {
                Key = key;
                this.Children.Clear();
                mode = ExclusionMode.Empty;
   
            }
        }

        /// <summary>
        /// Deletes this node from its parent.
        /// </summary>
        public void DeleteSelf()
        {
            Parent.Children.Remove(this);
        }

        /// <summary>
        /// Deletes the first child matching KEY.
        /// </summary>
        /// <param name="key">Key to search for</param>
        public void DeleteKey(object key)
        {
            for (int i = 0; i<Children.Count; i++)
                if (Equals(key, Children[i].Key))
                {
                    Children.RemoveAt(i);
                    return;
                }

        }

        /// <summary>
        /// Removes all the nodes satisfying the specified predicate
        /// </summary>
        /// <param name="predicate"></param>
        public void DeleteAll(Predicate<ELNode> predicate)
        {
            Children.RemoveAll(predicate);
        }
        #endregion

        #region Tree walk
        /// <summary>
        /// Run VISITOR on all the nodes in a subtree rooted at this node and whose
        /// children are stored in nodes with the key CHILDKEY
        /// </summary>
        /// <param name="childKey">Key to look up to get node containing children of this node</param>
        /// <param name="visitor">Procedure to call on each node.</param>
        public void WalkTree(object childKey, Action<ELNode> visitor)
        {
            visitor(this);
            ELNode childrenNode;
            if (this.TryLookup(childKey, out childrenNode))
            {
                // It has children
                foreach (var child in childrenNode.Children)
                    child.WalkTree(childKey, visitor);
            }
        }
        #endregion

        #region Mutators overloaded as C# operators
        ///// <summary>
        ///// Write the specified key as a non-exclusive child.  If key is already a child, this has no effect
        ///// </summary>
        ///// <param name="e">KB node</param>
        ///// <param name="v">Bound variable holding the key for the child</param>
        ///// <returns>The child node containing key.</returns>
        ///// <exception cref="KBExclusionException">If an exclusive child has already been written in this node.</exception>
        //public static ELNode operator +(ELNode e, Variable v)
        //{
        //    return e.StoreNonExclusive(v);
        //}

        /// <summary>
        /// Write the specified key as a non-exclusive child.  If key is already a child, this has no effect
        /// </summary>
        /// <param name="e">KB node</param>
        /// <param name="key">Key to write</param>
        /// <returns>The child node containing key.</returns>
        /// <exception cref="ELNodeExclusionException">If an exclusive child has already been written in this node.</exception>
        public static ELNode operator /(ELNode e, object key)
        {
            return e.StoreNonExclusive(key);
        }

        ///// <summary>
        ///// Write the specified key as an exclusive child.
        ///// If key is already the child, this has no effect.
        ///// Otherwise, the current child is replaced with this key.
        ///// </summary>
        ///// <param name="e">KB node</param>
        ///// <param name="v">Bound variable holding the key for the child</param>
        ///// <returns>The child node containing key.</returns>
        ///// <exception cref="KBExclusionException">If a non-exclusive child has already been written in this node.</exception>
        //public static ELNode operator -(ELNode e, Variable v)
        //{
        //    return e.StoreExclusive(v, true);
        //}

        /// <summary>
        /// Write the specified key as an exclusive child.
        /// If key is already the child, this has no effect.
        /// Otherwise, the current child is replaced with this key.
        /// </summary>
        /// <param name="e">KB node</param>
        /// <param name="key">Key to write</param>
        /// <returns>The child node containing key.</returns>
        /// <exception cref="ELNodeExclusionException">If a non-exclusive child has already been written in this node.</exception>
        public static ELNode operator %(ELNode e, object key)
        {
            return e.StoreExclusive(key, true);
        }
        #endregion
    }
}
