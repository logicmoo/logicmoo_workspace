/*  $Id$
*  
*  Project: Swicli.Library - Two Way Interface for .NET and MONO to SWI-Prolog
*  Author:        Douglas R. Miles
*  E-mail:        logicmoo@gmail.com
*  WWW:           http://www.logicmoo.com
*  Copyright (C):  2010-2012 LogicMOO Developement
*
*  This library is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  This library is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with this library; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*********************************************************/

using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using SbsSW.SwiPlCs;

namespace Swicli.Library
{
    public partial class PrologCLR
    {
        static public IDictionary<TKey, TValue> CreatePrologBackedDictionary<TKey, TValue>(PlTerm pred)
        {
            string p = PredicateName(pred);
            return new PrologBackedDictionary<TKey, TValue>(
                PredicateModule(pred), p + "_get",
                CreatePrologBackedCollection<TKey>(pred),
                p + "_set", p + "_remove", p + "_clear");
        }

        static public ICollection<T> CreatePrologBackedCollection<T>(PlTerm pred)
        {
            string p = PredicateName(pred);
            return new PrologBackedCollection<T>(
                PredicateModule(pred), p + "_get",
                p + "_add", p + "_remove", p + "_clear");
        }

        [PrologVisible]
        static public bool cliTestPbd(PlTerm pred, PlTerm counted)
        {
            var id = CreatePrologBackedDictionary<string, object>(pred);
            string s = String.Empty;
            var enumer = id.GetEnumerator();
            while (enumer.MoveNext())
            {
                var o = enumer.Current;
                s += String.Format("{0}={1},", o.Key, o.Value);
            }
            counted.UnifyAtom(s);
            return true;
        }
        [PrologVisible]
        static public bool cliTestPbdt(PlTerm pred, PlTerm counted)
        {
            var id = CreatePrologBackedDictionary<string, object>(pred);
            string s = String.Empty;
            AutoResetEvent are = new AutoResetEvent(false);
            (new Thread(() =>
                           {
                               var enumer = id.GetEnumerator();
                               while (enumer.MoveNext())
                               {
                                   var o = enumer.Current;
                                   s += String.Format("{0}={1},", o.Key, o.Value);
                               }
                               are.Set();
                           })).Start();
            are.WaitOne();
            counted.UnifyAtom(s);
            return true;
        }
        [PrologVisible]
        static public bool cliTestPbct(PlTerm pred, PlTerm counted)
        {
            var id = CreatePrologBackedCollection<object>(pred);
            string s = String.Empty;
            AutoResetEvent are = new AutoResetEvent(false);
            (new Thread(() =>
            {
                var enumer = id.GetEnumerator();
                while (enumer.MoveNext())
                {
                    var o = enumer.Current;
                    s += String.Format("{0},", o);
                }
                are.Set();
            })).Start();
            are.WaitOne();
            counted.UnifyAtom(s);
            return true;
        }
        [PrologVisible]
        static public bool cliTestPbc(PlTerm pred, PlTerm counted)
        {
            var id = CreatePrologBackedCollection<object>(pred);
            string s = String.Empty;
            IEnumerator<object> enumer = id.GetEnumerator();
            while (enumer.MoveNext())
            {
                s += String.Format("{0},", enumer.Current);
            }
            counted.UnifyAtom(s);
            return true;
        }
    }

    public abstract class PrologBacked<TKey, TValue>
    {
        public void InForiegnFrame(Action action)
        {
            PrologCLR.RegisterCurrentThread();
            uint fid = libpl.PL_open_foreign_frame();
            try
            {
                action();
            }
            finally
            {
               // if (fid > 0) libpl.PL_close_foreign_frame(fid);
            }
        }

        public static bool PlCall(string module, string querypred, PlTermV termV)
        {
            return PrologCLR.PlCall(module, querypred, termV);
        }
        public static PlTerm KeyToTerm(TKey key)
        {
            if (key.Equals(default(TValue))) return PlTerm.PlVar();
            return PrologCLR.ToProlog(key);
        }

        public static PlTerm ValueToTerm(TValue value)
        {
            if (value.Equals(default(TValue))) return PlTerm.PlVar();
            return PrologCLR.ToProlog(value);
        }

        public static PlTermV TermVOf(KeyValuePair<TKey, TValue> item)
        {
            return new PlTermV(KeyToTerm(item.Key), ValueToTerm(item.Value));
        }

        protected Exception NewNotImplementedException()
        {
            throw new NotImplementedException("NewNotImplementedException");
        }

        public abstract string ToDebugString();
    }

    public class PrologBackedDictionary<TKey, TValue> : PrologBacked<TKey, TValue>, IDictionary<TKey, TValue>
    {
        private readonly string _module = null;//"user";
        private readonly string _getvalue;
        private ICollection<TKey> Keyz;
        private readonly Type valueType;
        private string _assertPred;
        private string _retractPred;
        private string _retractall;
        private Type keyType;

        public PrologBackedDictionary(string module, string get_value, ICollection<TKey> keyz, string assertPred, string retractPred, string retractall)
        {
            _module = module ?? "user";
            _getvalue = get_value;
            Keyz = keyz;
            _assertPred = assertPred;
            _retractPred = retractPred;
            _retractall = retractall;
            keyType = typeof(TKey);
            valueType = typeof(TValue);
        }

        #region Implementation of IEnumerable

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator()
        {
            PrologCLR.RegisterCurrentThread();
            return new PrologBackedDictionaryEnumerator(this);
        }

        public Dictionary<TKey, TValue> Copy()
        {
            var copy = new Dictionary<TKey, TValue>();
            foreach (var e in this)
            {
                copy.Add(e.Key,e.Value);
            }
            return copy;
        }

        public class PrologBackedDictionaryEnumerator : IEnumerator<KeyValuePair<TKey, TValue>>
        {
            private readonly PrologBackedDictionary<TKey, TValue> _dictionary;
            private uint fframe = 0;
            private PlTermV termV;
            private PlQuery plQuery;

            public PrologBackedDictionaryEnumerator(PrologBackedDictionary<TKey, TValue> dictionary)
            {
                _dictionary = dictionary;
                Reset();
            }

            #region Implementation of IDisposable

            /// <summary>
            /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
            /// </summary>
            /// <filterpriority>2</filterpriority>
            public void Dispose()
            {
                if (plQuery != null) plQuery.Dispose();
                plQuery = null;
                if (fframe != 0) libpl.PL_close_foreign_frame(fframe);
                fframe = 0;
            }

            #endregion

            #region Implementation of IEnumerator

            /// <summary>
            /// Advances the enumerator to the next element of the collection.
            /// </summary>
            /// <returns>
            /// true if the enumerator was successfully advanced to the next element; false if the enumerator has passed the end of the collection.
            /// </returns>
            /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. 
            ///                 </exception><filterpriority>2</filterpriority>
            public bool MoveNext()
            {
                if (!plQuery.NextSolution())
                {
                    Dispose();
                    return false;
                }
                return true;
            }

            /// <summary>
            /// Sets the enumerator to its initial position, which is before the first element in the collection.
            /// </summary>
            /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. 
            ///                 </exception><filterpriority>2</filterpriority>
            public void Reset()
            {
                Dispose();
                //fframe = libpl.PL_open_foreign_frame();
                termV = new PlTermV(2);
                plQuery = new PlQuery(_dictionary._module, _dictionary._getvalue, termV);
            }

            /// <summary>
            /// Gets the element in the collection at the current position of the enumerator.
            /// </summary>
            /// <returns>
            /// The element in the collection at the current position of the enumerator.
            /// </returns>
            public KeyValuePair<TKey, TValue> Current
            {
                get
                {
                    return new KeyValuePair<TKey, TValue>(
                        (TKey)PrologCLR.CastTerm(plQuery.Args[0], _dictionary.keyType),
                        (TValue)PrologCLR.CastTerm(plQuery.Args[1], _dictionary.valueType));
                }
            }

            /// <summary>
            /// Gets the current element in the collection.
            /// </summary>
            /// <returns>
            /// The current element in the collection.
            /// </returns>
            /// <exception cref="T:System.InvalidOperationException">The enumerator is positioned before the first element of the collection or after the last element.
            ///                 </exception><filterpriority>2</filterpriority>
            object IEnumerator.Current
            {
                get { return Current; }
            }

            #endregion
        }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.IEnumerator"/> object that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion

        #region Implementation of ICollection<KeyValuePair<TKey,TValue>>

        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public void Add(KeyValuePair<TKey, TValue> item)
        {
            if (_assertPred == null) throw new NotSupportedException("add " + this); 
            InForiegnFrame(() =>
            {
                PlTerm newPlTermV = PrologCLR.PlC(_getvalue, TermVOf(item));
                PlCall(_module, _assertPred, new PlTermV(newPlTermV));
            });

        }

        /// <summary>
        /// Removes all items from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only. 
        ///                 </exception>
        public void Clear()
        {
            if (_retractall == null) throw new NotSupportedException("clear " + this);
            InForiegnFrame(() =>
            {
                PlTerm newPlTermV = PrologCLR.PlC(_getvalue, new PlTermV(2));
                PlCall(_module, _retractall, new PlTermV(newPlTermV));
            });
        }

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.ICollection`1"/> contains a specific value.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> is found in the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param>
        public bool Contains(KeyValuePair<TKey, TValue> item)
        {
            bool found = false;
            InForiegnFrame(() =>
            {
                found = PlCall(_module, _getvalue, TermVOf(item));
            });
            return found;
        }

        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.Generic.ICollection`1"/> to an <see cref="T:System.Array"/>, starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">The one-dimensional <see cref="T:System.Array"/> that is the destination of the elements copied from <see cref="T:System.Collections.Generic.ICollection`1"/>. The <see cref="T:System.Array"/> must have zero-based indexing.
        ///                 </param><param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null.
        ///                 </exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="arrayIndex"/> is less than 0.
        ///                 </exception><exception cref="T:System.ArgumentException"><paramref name="array"/> is multidimensional.
        ///                     -or-
        ///                 <paramref name="arrayIndex"/> is equal to or greater than the length of <paramref name="array"/>.
        ///                     -or-
        ///                     The number of elements in the source <see cref="T:System.Collections.Generic.ICollection`1"/> is greater than the available space from <paramref name="arrayIndex"/> to the end of the destination <paramref name="array"/>.
        ///                     -or-
        ///                     Type <paramref name="T"/> cannot be cast automatically to the type of the destination <paramref name="array"/>.
        ///                 </exception>
        public void CopyTo(KeyValuePair<TKey, TValue>[] array, int arrayIndex)
        {
            throw NewNotImplementedException();
        }

        /// <summary>
        /// Removes the first occurrence of a specific object from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> was successfully removed from the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false. This method also returns false if <paramref name="item"/> is not found in the original <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        /// <param name="item">The object to remove from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public bool Remove(KeyValuePair<TKey, TValue> item)
        {
            bool removed = false;
            InForiegnFrame(() =>
            {
                PlTerm newPlTermV = PrologCLR.PlC(_getvalue, TermVOf(item));
                removed = PlCall(_module, _retractPred, new PlTermV(newPlTermV));
            });
            return removed;

        }

        /// <summary>
        /// Gets the number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// The number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        public int Count
        {
            get { return Keyz.Count; }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only; otherwise, false.
        /// </returns>
        public bool IsReadOnly
        {
            get { return _retractPred != null; }
        }

        #endregion

        #region Implementation of IDictionary<TKey,TValue>

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the specified key.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the key; otherwise, false.
        /// </returns>
        /// <param name="key">The key to locate in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception>
        public bool ContainsKey(TKey key)
        {
            if (Keyz != null) return Keyz.Contains(key);
            bool found = false;
            InForiegnFrame(() =>
                               {
                                   found = PlCall(_module, _getvalue, new PlTermV(KeyToTerm(key), PlTerm.PlVar()));
                               });
            return found;
        }

        /// <summary>
        /// Adds an element with the provided key and value to the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <param name="key">The object to use as the key of the element to add.
        ///                 </param><param name="value">The object to use as the value of the element to add.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.ArgumentException">An element with the same key already exists in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public void Add(TKey key, TValue value)
        {
            Add(new KeyValuePair<TKey, TValue>(key, value));
        }

        /// <summary>
        /// Removes the element with the specified key from the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// true if the element is successfully removed; otherwise, false.  This method also returns false if <paramref name="key"/> was not found in the original <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        /// <param name="key">The key of the element to remove.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public bool Remove(TKey key)
        {
            if (Keyz != null)
            {
                if (!Keyz.IsReadOnly) return Keyz.Remove(key);
            }
            if (_retractPred == null) throw new NotSupportedException("remove " + this);
            bool removed = false;
            InForiegnFrame(() =>
                               {

                                   PlTerm newPlTermV = PrologCLR.PlC(_getvalue, KeyToTerm(key), PlTerm.PlVar());
                                   removed = PlCall(_module, _retractPred, new PlTermV(newPlTermV));
                               });
            return removed;
        }

        /// <summary>
        /// Gets the value associated with the specified key.
        /// </summary>
        /// <returns>
        /// true if the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/> contains an element with the specified key; otherwise, false.
        /// </returns>
        /// <param name="key">The key whose value to get.
        ///                 </param><param name="value">When this method returns, the value associated with the specified key, if the key is found; otherwise, the default value for the type of the <paramref name="value"/> parameter. This parameter is passed uninitialized.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception>
        public bool TryGetValue(TKey key, out TValue value)
        {
            TValue value0 = default(TValue);
            bool res = false;
            InForiegnFrame(() =>
                               {
                                   PlTerm plTermPlVar = PlTerm.PlVar();
                                   PlTermV newPlTermV = new PlTermV(KeyToTerm(key), plTermPlVar);
                                   res = PlCall(_module, _getvalue, newPlTermV);
                                   if (res)
                                   {
                                       value0 = (TValue)PrologCLR.CastTerm(newPlTermV[1], valueType);
                                   }
                                   else
                                   {

                                   }
                               });
            value = value0;
            return res;
        }

        /// <summary>
        /// Gets or sets the element with the specified key.
        /// </summary>
        /// <returns>
        /// The element with the specified key.
        /// </returns>
        /// <param name="key">The key of the element to get or set.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="key"/> is null.
        ///                 </exception><exception cref="T:System.Collections.Generic.KeyNotFoundException">The property is retrieved and <paramref name="key"/> is not found.
        ///                 </exception><exception cref="T:System.NotSupportedException">The property is set and the <see cref="T:System.Collections.Generic.IDictionary`2"/> is read-only.
        ///                 </exception>
        public TValue this[TKey key]
        {
            get
            {
                TValue tvalue = default(TValue);
                InForiegnFrame(() =>
                {
                    PlTerm newPlTermV = PrologCLR.PlC(_getvalue, KeyToTerm(key), PlTerm.PlVar());
                    bool res = PlCall(_module, _getvalue, new PlTermV(newPlTermV));
                    if (res)
                    {
                        tvalue = (TValue)PrologCLR.CastTerm(newPlTermV.Arg(1), valueType);
                    }
                    else
                    {
                        // tvalue = default(TValue);
                    }
                });
                return tvalue;

            }
            set
            {
                Remove(key);
                Add(new KeyValuePair<TKey, TValue>(key, value));
            }
        }

        /// <summary>
        /// Gets an <see cref="T:System.Collections.Generic.ICollection`1"/> containing the keys of the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.Generic.ICollection`1"/> containing the keys of the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        public ICollection<TKey> Keys
        {
            get { return Keyz; }
        }

        /// <summary>
        /// Gets an <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.Generic.ICollection`1"/> containing the values in the object that implements <see cref="T:System.Collections.Generic.IDictionary`2"/>.
        /// </returns>
        public ICollection<TValue> Values
        {
            get { throw NewNotImplementedException(); }
        }

        #endregion

        #region Overrides of PrologBacked<TKey,TValue>

        public override string ToDebugString()
        {
            string ds = "" + Count;
            foreach (var kv in this)
            {
                ds += "," + kv.Key + "=" + kv.Value;
            }
            return ds;
        }

        #endregion
    }


    public class PrologBackedCollection<T> : PrologBacked<T,object>, ICollection<T>, ICollection
    {
        private readonly string _module = null;//"user";
        private readonly string _querypred;
        private readonly Type keyType;
        private readonly Type valueType;
        private string _assertPred;
        private string _retractPred;
        private string _retractall;

        public PrologBackedCollection(string module, string querypred, string assertPred, string retractPred, string retractall)
        {
            _module = module ?? "user";
            _querypred = querypred;
            _assertPred = assertPred;
            _retractPred = retractPred;
            _retractall = retractall;
            keyType = typeof (T);
        }

        #region ICollection<T> Members

        public void Add(T item)
        {
            if (_assertPred == null) throw new NotSupportedException("add " + this);
            InForiegnFrame(() =>
            {
                PlTerm newPlTermV = PrologCLR.PlC(_querypred, new PlTermV(KeyToTerm(item)));
                PlCall(_module, _assertPred, new PlTermV(newPlTermV));
            });
        }

        public void Clear()
        {
            InForiegnFrame(() =>
                               {
                                   PlTerm newPlTermV = PrologCLR.PlC(_querypred, new PlTermV(1));
                                   PlCall(_module, _retractall, new PlTermV(newPlTermV));
                               });
        }

        public bool Contains(T item)
        {
            bool found = false;
            InForiegnFrame(() =>
            {
                found = PlCall(_module, _querypred, new PlTermV(KeyToTerm(item)));
            });
            return found;
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            throw NewNotImplementedException();
        }

        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.ICollection"/> to an <see cref="T:System.Array"/>, starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">The one-dimensional <see cref="T:System.Array"/> that is the destination of the elements copied from <see cref="T:System.Collections.ICollection"/>. The <see cref="T:System.Array"/> must have zero-based indexing. 
        ///                 </param><param name="index">The zero-based index in <paramref name="array"/> at which copying begins. 
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null. 
        ///                 </exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="index"/> is less than zero. 
        ///                 </exception><exception cref="T:System.ArgumentException"><paramref name="array"/> is multidimensional.
        ///                     -or- 
        ///                 <paramref name="index"/> is equal to or greater than the length of <paramref name="array"/>.
        ///                     -or- 
        ///                     The number of elements in the source <see cref="T:System.Collections.ICollection"/> is greater than the available space from <paramref name="index"/> to the end of the destination <paramref name="array"/>. 
        ///                 </exception><exception cref="T:System.ArgumentException">The type of the source <see cref="T:System.Collections.ICollection"/> cannot be cast automatically to the type of the destination <paramref name="array"/>. 
        ///                 </exception><filterpriority>2</filterpriority>
        public void CopyTo(Array array, int index)
        {
            throw NewNotImplementedException();
        }

        public int Count
        {
            get
            {
                var copy = 0;
                foreach (var e in this)
                {
                    copy++;
                }
                return copy;
            }
        }

        /// <summary>
        /// Gets an object that can be used to synchronize access to the <see cref="T:System.Collections.ICollection"/>.
        /// </summary>
        /// <returns>
        /// An object that can be used to synchronize access to the <see cref="T:System.Collections.ICollection"/>.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        public object SyncRoot
        {
            get { return this; }
        }

        /// <summary>
        /// Gets a value indicating whether access to the <see cref="T:System.Collections.ICollection"/> is synchronized (thread safe).
        /// </summary>
        /// <returns>
        /// true if access to the <see cref="T:System.Collections.ICollection"/> is synchronized (thread safe); otherwise, false.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        public bool IsSynchronized
        {
            get { throw NewNotImplementedException(); }
        }

        public bool IsReadOnly
        {
            get { return _retractPred == null; }
        }

        public bool Remove(T item)
        {
            if (_retractPred == null) throw new NotSupportedException("remove " + this);
            bool found = false;
            InForiegnFrame(() =>
            {
                found = PlCall(_module, _retractPred, new PlTermV(KeyToTerm(item)));
            });
            return found;
        }

        #endregion

        public List<T> Copy()
        {
            var copy = new List<T>();
            foreach (var e in this)
            {
                copy.Add(e);
            }
            return copy;
        }

        public override string ToDebugString()
        {
            string ds = "" + Count;
            foreach (var kv in this)
            {
                ds += "," + kv;
            }
            return ds;
        }
        #region IEnumerable<T> Members

        public IEnumerator<T> GetEnumerator()
        {
            PrologCLR.RegisterCurrentThread(); 
            return new PrologBackedCollectionEnumerator(this);
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return new PrologBackedCollectionEnumerator(this);
        }

        public class PrologBackedCollectionEnumerator : IEnumerator<T>
        {
            private readonly PrologBackedCollection<T> _dictionary;
            private uint fframe = 0;
            private PlTermV termV;
            private PlQuery plQuery;
            private bool nonLeft = true;
            private object currentValue;

            public PrologBackedCollectionEnumerator(PrologBackedCollection<T> dictionary)
            {
                _dictionary = dictionary;
                Reset();
            }

            #region Implementation of IDisposable

            /// <summary>
            /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
            /// </summary>
            /// <filterpriority>2</filterpriority>
            public void Dispose()
            {
                if (plQuery != null)
                {
                    plQuery.Dispose();
                    plQuery = null;
                }
                nonLeft = true;
                currentValue = null;
                if (fframe != 0) libpl.PL_close_foreign_frame(fframe);
                fframe = 0;
            }

            #endregion

            #region Implementation of IEnumerator

            /// <summary>
            /// Advances the enumerator to the next element of the collection.
            /// </summary>
            /// <returns>
            /// true if the enumerator was successfully advanced to the next element; false if the enumerator has passed the end of the collection.
            /// </returns>
            /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. 
            ///                 </exception><filterpriority>2</filterpriority>
            public bool MoveNext()
            {
                if(!plQuery.NextSolution())
                {
                    Dispose();
                    return false;
                }
                nonLeft = false;
                PlTerm plQueryArgs = plQuery.Args[0];
                currentValue = PrologCLR.CastTerm(plQueryArgs, _dictionary.keyType); ;
                return true;
            }

            /// <summary>
            /// Sets the enumerator to its initial position, which is before the first element in the collection.
            /// </summary>
            /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. 
            ///                 </exception><filterpriority>2</filterpriority>
            public void Reset()
            {
                Dispose();
                fframe = libpl.PL_open_foreign_frame();
                termV = new PlTermV(1);
                plQuery = new PlQuery(_dictionary._module, _dictionary._querypred, termV);
                nonLeft = false;
            }

            /// <summary>
            /// Gets the element in the collection at the current position of the enumerator.
            /// </summary>
            /// <returns>
            /// The element in the collection at the current position of the enumerator.
            /// </returns>
            public T Current
            {
                get
                {
                    if (nonLeft)
                    {
                        throw new Exception("no current element");
                    }
                    return (T) currentValue;
                }
            }

            /// <summary>
            /// Gets the current element in the collection.
            /// </summary>
            /// <returns>
            /// The current element in the collection.
            /// </returns>
            /// <exception cref="T:System.InvalidOperationException">The enumerator is positioned before the first element of the collection or after the last element.
            ///                 </exception><filterpriority>2</filterpriority>
            object IEnumerator.Current
            {
                get { return Current; }
            }

            #endregion
        }

        #endregion
    }
}
