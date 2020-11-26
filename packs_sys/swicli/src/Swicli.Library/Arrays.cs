#region
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
#endregion

#if USE_MUSHDLR
using MushDLR223.Utilities;
#endif
#if USE_IKVM
//using jpl;
using Type = System.Type;
#else
using System.Reflection;
using JClass = System.Type;
#endif
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using SbsSW.SwiPlCs;

using PlTerm = SbsSW.SwiPlCs.PlTerm;

namespace Swicli.Library
{
    public partial class PrologCLR
    {
        /// <summary>
        /// ?- cliNewArray(long,10,Out),cliToString(Out,Str).
        /// </summary>
        /// <param name="ElementType"></param>
        /// <param name="indexes">10 or [10] or [10,2]</param>
        /// <param name="valueOut">Tagged ref to the array</param>
        /// <returns></returns>
        [PrologVisible]
        public static bool cliNewArray(PlTerm clazzSpec, PlTerm indexes, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliNewArray(clazzSpec, indexes, plvar) && SpecialUnify(valueOut, plvar);
            }
            Type c = getInstanceTypeFromClass(GetType(clazzSpec));
            if (c == null)
            {
                Embedded.Warn("Cant find type {0}", clazzSpec);
                return false;
            }
            var dims = ToTermArray(indexes);
            var arrayType = c.MakeArrayType(dims.Length);
            var value = CreateArrayOfType(dims, c);
            return UnifyTagged(value, valueOut);
        }

        [PrologVisible]
        public static bool cliArrayFill(PlTerm arrayValue, PlTerm valueIn)
        {
            object getInstance;
            Type c;
            if (!GetInstanceAndType(arrayValue, out getInstance, out c)) return false;
            Array al = (Array) getInstance;
            var initValue = CastTerm(valueIn, c.GetElementType());
            var idxIter = new ArrayIndexEnumerator(al);
            while (idxIter.MoveNext())
            {
                al.SetValue(initValue, idxIter.Current);
            }
            return true;
        }

        [PrologVisible]
        public static bool cliArrayFillValues(PlTerm arrayValue, PlTerm valueIn)
        {
            object getInstance;
            Type c;
            if (!GetInstanceAndType(arrayValue, out getInstance, out c)) return false;
            Array al = (Array) getInstance;
            FillArray(ToTermArray(valueIn), c.GetElementType(), al);
            return true;
        }

        [PrologVisible]
        public static bool cliArrayToTerm(PlTerm arrayValue, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliArrayToTerm(arrayValue, plvar) && SpecialUnify(valueOut, plvar);
            }
            object getInstance = GetInstance(arrayValue);
            if (getInstance == null) return valueOut.Unify(PLNULL);
            Array value = GetArrayValue(getInstance);
            if (value == null)
            {
                Embedded.Error("Cant find array from {0} as {1}", arrayValue, getInstance.GetType());
                return false;
            }
            return unifyArrayToTerm(value, valueOut);
        }
        public static bool unifyArrayToTerm(Array value, PlTerm valueOut)
        {
            int len = value.Length;
            Type arrayType = value.GetType();
            var termv = NewPlTermV(len);
            int rank = arrayType.GetArrayRank();
            if (rank != 1)
            {
                var indexesv = new PlTermV(rank);
                for (int i = 0; i < rank; i++)
                {
                    indexesv[i].Put(value.GetLength(i));
                }
                var idxIter = new ArrayIndexEnumerator(value);
                int putAt = 0;
                while (idxIter.MoveNext())
                {
                    bool pf = termv[putAt++].FromObject((value.GetValue(idxIter.Current)));
                    if (!pf)
                    {
                        return false;
                    }
                }
                return /// array/3 
                    valueOut.Unify(PlC("array", typeToSpec(arrayType), PlC("indexes", indexesv), PlC("values", termv)));
            }
            else
            {
                for (int i = 0; i < len; i++)
                {
                    bool pf = termv[i].FromObject((value.GetValue(i)));
                    if (!pf)
                    {
                        return false;
                    }
                }
                return valueOut.Unify(PlC("array", typeToSpec(arrayType.GetElementType()), PlC("values", termv)));
            }
        }

        [PrologVisible]
        [PrologTest]
        public static bool cliTestArrayToTerm1(PlTerm valueOut)
        {
            return cliArrayToTerm(ToProlog(new[] {1, 2, 3, 4,}), valueOut);
        }

        [PrologVisible]
        [PrologTest]
        public static bool cliTestArrayToTerm2(PlTerm valueOut)
        {
            return cliArrayToTerm(ToProlog(new[,,] {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}), valueOut);
        }

        [PrologVisible]
        public static bool cliArrayToTermlist(PlTerm arrayValue, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliArrayToTermlist(arrayValue, plvar) && SpecialUnify(valueOut, plvar);
            }

            object getInstance = GetInstance(arrayValue);
            if (getInstance == null) return valueOut.Unify(PLNULL);
            Array value = GetArrayValue(getInstance);
            if (value == null)
            {
                Embedded.Error("Cant find array from {0} as {1}", arrayValue, getInstance.GetType());
                return false;
            }
            Type arrayType = value.GetType();
            if (arrayType.GetArrayRank() != 1)
            {
                Embedded.Error("Non rank==1 " + arrayType);
            }
            int len = value.Length;
            var termv = ATOM_NIL;
            for (int i = len - 1; i >= 0; i--)
            {
                termv = PlC(".", ToProlog((value.GetValue(i))), termv);
            }
            //Type et = value.GetType().GetElementType();
            return valueOut.Unify(termv);
        }

        [PrologVisible]
        public static bool cliTermToArray(PlTerm arrayValue, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliTermToArray(arrayValue, plvar) && SpecialUnify(valueOut, plvar);
            }
            if (arrayValue.Name == "array")
            {
                return valueOut.FromObject(GetInstance(arrayValue));
            }
            Type elementType = ResolveType(arrayValue.Name);
            if (elementType == null)
            {
                Embedded.Error("Cant find vector from {0}", arrayValue);
                return false;
            }
            var value = CreateArrayOfTypeRankOneFilled(arrayValue, elementType.MakeArrayType());
            return valueOut.FromObject((value));
        }


        private static void ArraySet(Array array, object idx, object value)
        {
            if (idx is int)
            {
                array.SetValue(value, (int) idx);
            }
            else
            {
                array.SetValue(value, (int[]) idx);
            }
        }

        private static Array GetArrayValue(object getInstance)
        {
            if (getInstance == null) return null;
            lock (getInstance)
            {
                try
                {
                    return GetArrayValue0(getInstance);
                }
                catch (Exception ex)
                {
                    Embedded.WriteException(ex);
                    throw;
                }
            }
        }

        private static Array GetArrayValue0(object getInstance)
        {
            if (getInstance is Array)
            {
                return (Array) getInstance;
            }
            Type t = getInstance.GetType();
            Type et = typeof (object);
            if (t.IsGenericType)
            {
                Type[] typeArguments = t.GetGenericArguments();
                if (typeArguments.Length == 1)
                {
                    et = typeArguments[0];
                }
            }
            if (getInstance is ArrayList)
            {
                return ((ArrayList) getInstance).ToArray(et);
            }
            if (getInstance is ICollection)
            {
                var collection = ((ICollection) getInstance);
                int count = collection.Count;
                var al = Array.CreateInstance(et, count);
                try
                {
                    collection.CopyTo(al, 0);
                    return al;
                }
                catch (Exception ex)
                {
                    string warn = "CopyTo " + ex;
                    Embedded.Warn(warn);
                    int count2 = collection.Count;
                    if (count2 != count)
                    {
                        Embedded.ConsoleWriteLine("Collection Modified while in CopyTo! " + count + "->" + count2 + " of " +
                                         collection);
                        throw;
                    }
                    int index = 0;
                    foreach (var e in collection)
                    {
                        al.SetValue(e, index++);
                    }
                    return al;
                }
            }
            if (getInstance is IEnumerable)
            {
                var collection = ((IEnumerable) getInstance).GetEnumerator();
                var al = new ArrayList();
                while (collection.MoveNext())
                {
                    al.Add(collection.Current);
                }
                return al.ToArray(et);
            }
            else if (getInstance is IEnumerator)
            {
                var collection = (IEnumerator) getInstance;
                var al = new ArrayList();
                while (collection.MoveNext())
                {
                    al.Add(collection.Current);
                }
                return al.ToArray(et);
            }
            else
            {
                // this one is probly null
                return getInstance as Array;
            }
        }

        public static Object[] ToObjectArray(PlTerm[] a/*CONTEXT*/)
        {
            if (a == null) return null;
            Object[] ret = new Object[a.Length];
            int i = 0;
            foreach (var term in a)
            {
                ret[i++] = GetInstance(term/*ctx*/);
            }
            return ret;
        }
        public static PlTerm[] ToTermArray(IEnumerable<PlTerm> enumerable)
        {
            if (enumerable is PlTerm[]) return (PlTerm[]) enumerable;
            if (enumerable is PlTermV)
            {
                PlTermV tv = (PlTermV) enumerable;
                return tv.ToArray();
            }
            if (enumerable is PlTerm)
            {
                // I guess IsList makes a copy
                PlTerm tlist = (PlTerm) enumerable;
                if (tlist.IsNil) return ZERO_PLTERMS;
                if (tlist.IsVar) return new PlTerm[] {tlist};
                if (tlist.IsList)
                {
                    enumerable = tlist.Copy();
                }
                if (tlist.Name == "{}")
                {
                    var t = tlist.Arg(0);
                    var terms = new List<PlTerm>();
                    while (t.Arity == 2)
                    {
                        terms.Add(t.Arg(0));
                        t = t.Arg(1);
                    }
                    // last Item
                    terms.Add(t);
                    return terms.ToArray();
                }
                if (tlist.IsAtomic)
                {
                    if (tlist.Name == "[]") return ZERO_PLTERMS;
                    return new PlTerm[] {tlist};
                }
            }
            return enumerable.ToArray();
        }

        public static PlTerm[] ZERO_PLTERMS  = new PlTerm[0];

        /// <summary>
        /// Construct an array of some type
        /// </summary>
        /// <param name="arrayValue"></param>
        /// <param name="arrayType">The parent array type .. not the Element type</param>
        /// <returns></returns>
        private static Array CreateArrayOfType(PlTerm[] dims, Type arrayType)
        {
            if (!arrayType.IsArray)
            {
                Embedded.Error("Not Array Type! " + arrayType);
            }
            Type elementType = arrayType.GetElementType();
            int rank = arrayType.GetArrayRank();
            int[] lengths = new int[rank];
            for (int i = 0; i < rank; i++)
            {
                lengths[i] = dims[i].intValue();
            }
            Array al = Array.CreateInstance(elementType, lengths);
            return al;
        }

        private static Array CreateArrayOfTypeRankOneFilled(PlTerm arrayValue, Type arrayType)
        {
            if (!arrayType.IsArray)
            {
                Embedded.Error("Not Array Type! " + arrayType);
            }
            Type elementType = arrayType.GetElementType();
            if (arrayType.GetArrayRank() != 1)
            {
                Embedded.Warn("Non rank==1 " + arrayType);
            }
            PlTerm[] terms = ToTermArray(arrayValue);
            Array al = Array.CreateInstance(elementType, terms.Length);
            FillArray(terms, elementType, al);
            return al;
        }

        private static Array CreateArrayNarrowest(object[] values)
        {
            if (values == null || values.Length == 0) return values;
            List<Type> elementType = null;
            Type lastType = null;
            foreach (var o in values)
            {
                if (o == null) continue;
                if (elementType == null)
                {
                    lastType = o.GetType();
                    elementType = GetInheritsType(lastType);
                    if (elementType.Count == 0) return values;
                    continue;
                }
                if (lastType.IsInstanceOfType(o) || elementType[0].IsInstanceOfType(o)) continue;
                var ot = o.GetType();
                elementType = new List<Type>(elementType.Intersect(GetInheritsType(ot)));
                if (elementType.Count == 0) return values;
                lastType = ot;
            }
            if (elementType == null) return values;
            Type elementType1 = elementType[0];
            Array al = Array.CreateInstance(elementType1, values.Length);
            int at = 0;
            foreach (var o in values)
            {
                if (o != null) al.SetValue(o, at);
            }
            return al;
        }

        private static List<Type> exThese;
        private static List<Type> GetInheritsType(Type t1)
        {
            Type[] t1GetInterfaces = t1.GetInterfaces();
            List<Type> l1 = new List<Type>(t1GetInterfaces.Length+2);
            while (t1 != null)
            {
                if (OkType(t1)) l1.Add(t1);
                t1 = t1.BaseType;
            }
            foreach(var it in t1GetInterfaces)
            {
                if (OkType(it)) l1.Add(it);  
            }
            return l1;
        }

        static bool OkType(Type t)
        {
            if (t == null) return false;
            if (t == typeof(object)) return false;
            if (t == typeof(ValueType)) return false;
            if (t == typeof(IComparable)) return false;
            if (t == typeof(IFormattable)) return false;
            if (exThese == null)
            {
                exThese = new List<Type>();
                //exThese = GetInheritsType(typeof (int));
               // exThese.Remove(typeof (int));
            }
            return !exThese.Contains(t);
        }


        private static void FillArray(PlTerm[] terms, Type elementType, Array al)
        {
            int termsLength = terms.Length;
            var idxIter = new ArrayIndexEnumerator(al);
            for (int i = 0; i < termsLength; i++)
            {
                idxIter.MoveNext();
                PlTerm term = terms[i];
                al.SetValue(CastTerm(term, elementType), idxIter.Current);
            }
        }
    }

    public class ArrayIndexEnumerator : IEnumerator<int[]>
    {
        private readonly int[] idx;
        private readonly int len = 1;
        private readonly int[] lowers;
        private readonly int rank;
        private readonly int[] uppers;
        private int at = -1;

        public ArrayIndexEnumerator(Array value)
        {
            Type arrayType = value.GetType();
            rank = arrayType.GetArrayRank();
            uppers = new int[rank];
            lowers = new int[rank];
            idx = new int[rank];

            len = 1;
            for (int i = 0; i < rank; i++)
            {
                int high = uppers[i] = value.GetUpperBound(i);
                int low = lowers[i] = value.GetLowerBound(i);
                if (low != 0)
                {
                    Embedded.Error("LowerBound !=0 in " + arrayType);
                }
                int lenSize = (high - low + 1);
                len *= lenSize;
            }
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
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
            at++;
            if (at == 0)
            {
                return true;
            }
            if (at >= len) return false;
            for (int i = rank - 1; i >= 0; i--)
            {
                if (idx[i] < uppers[i])
                {
                    idx[i]++;
                    return true;
                }
                idx[i] = lowers[i];
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
            for (int i = 0; i < rank; i++)
            {
                idx[i] = 0;
            }
            at = -1;
        }

        /// <summary>
        /// Gets the element in the collection at the current position of the enumerator.
        /// </summary>
        /// <returns>
        /// The element in the collection at the current position of the enumerator.
        /// </returns>
        public int[] Current
        {
            get
            {
                if (at < 0) throw new InvalidOperationException("forgot MoveNext");
                return idx;
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
}