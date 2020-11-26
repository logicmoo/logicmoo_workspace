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
#if USE_MUSHDLR
using MushDLR223.Utilities;
#endif
#if USE_IKVM
//using JavaClass = java.lang.Class;
using Type = System.Type;
#else
using JClass = System.Type;
using Type = System.Type;
#endif
using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using SbsSW.SwiPlCs;
using PlTerm = SbsSW.SwiPlCs.PlTerm;

namespace Swicli.Library
{
    internal class PrologTermLayout
    {
        public string Name;
        public int Arity;
        public Type ObjectType;
        public MemberInfo[] FieldInfos;
        public MemberInfo ToType;
    }

    internal class PrologTermRecomposer
    {
        public string module;
        public string Name;
        public int Arity;
        public String obj2r;
        public String r2obj;
        //public MemberInfo[] FieldInfos;
        public Type ToType;
    }
    
    public partial class PrologCLR
    {
        [PrologVisible]
        static public bool cliGetterm(PlTerm valueCol, PlTerm valueIn, PlTerm valueOut)
        {
            List<object> objs;
            if (valueCol.IsVar)
            {
                objs = new List<object>();
                valueCol.FromObject(objs);
            }
            else
            {
                objs = (List<object>)CastTerm(valueCol, typeof(ICollection));
            }
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliGetterm(valueCol, valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            if (IsTaggedObject(valueIn))
            {
                object val = GetInstance(valueIn);
                int index = objs.IndexOf(val);
                if (index < 0)
                {
                    index = objs.Count;
                    objs.Add(val);
                    var type = val.GetType();
                    if (type.IsArray)
                    {
                        return valueIn.Unify(valueOut);
                    }
                    return ToFieldLayout("object", typeToName(type), val, type, valueOut, false, false) != libpl.PL_fail;
                }
            }
            return valueIn.Unify(valueOut);
        }

        [PrologVisible]
        static public bool cliIsLayout(PlTerm predIndic)
        {
            string key = PredicateName(predIndic) + "/" + PredicateArity(predIndic);
            lock (FunctorToLayout)
            {
                return FunctorToLayout.ContainsKey(key);
            }
        }

        [PrologVisible]
        static public bool cliAddLayout(PlTerm clazzSpec, PlTerm memberSpec, PlTerm toSpec)
        {
            Type type = GetType(clazzSpec);
            string name = memberSpec.Name;
            int arity = memberSpec.Arity;
            MemberInfo[] fieldInfos = new MemberInfo[arity];
            for (int i = 0; i < arity; i++)
            {
                var arg = memberSpec.Arg(i);
                fieldInfos[i] = findMember(arg, type);
            }
            var toMemb = findMember(toSpec, type);
            AddPrologTermLayout(type, name, fieldInfos, toMemb);
            return true;
        }

        [PrologVisible]
        static public bool cliAddLayout(PlTerm clazzSpec, PlTerm memberSpec)
        {
            Type type = GetType(clazzSpec);
            string name = memberSpec.Name;
            int arity = memberSpec.Arity;
            MemberInfo[] fieldInfos = new MemberInfo[arity];
            for (int i = 0; i < arity; i++)
            {
                var arg = memberSpec.Arg(i);
                fieldInfos[i] = findMember(arg, type);
            }
            AddPrologTermLayout(type, name, fieldInfos, null);
            return true;
        }

        readonly private static Dictionary<string, PrologTermLayout> FunctorToLayout = new Dictionary<string, PrologTermLayout>();
        readonly private static Dictionary<Type, PrologTermLayout> TypeToLayout = new Dictionary<Type, PrologTermLayout>();

        static public void AddPrologTermLayout(Type type, string name, MemberInfo[] fieldInfos, MemberInfo toType)
        {
            PrologTermLayout layout = new PrologTermLayout();
            layout.FieldInfos = fieldInfos;
            layout.Name = name;
            layout.ObjectType = type;
            layout.ToType = toType;
            int arity = fieldInfos.Length;
            if (toType != null)
            {
                arity = 1;
            }
            layout.Arity = arity;
            lock (FunctorToLayout)
            {
                FunctorToLayout[name + "/" + arity] = layout;
                TypeToLayout[type] = layout;
            }
        }

        [PrologVisible]
        static public bool cliAddRecomposer(PlTerm clazzSpec, PlTerm memberSpec, PlTerm obj2r, PlTerm r2obj)
        {
            Type type = GetType(clazzSpec);
            string name = memberSpec.Name;
            int arity = memberSpec.Arity;
            AddPrologTermRecomposer(type, "user", obj2r.Name, r2obj.Name, name, arity);
            return true;
        }

        readonly private static Dictionary<string, PrologTermRecomposer> FunctorToRecomposer = new Dictionary<string, PrologTermRecomposer>();
        readonly private static Dictionary<Type, PrologTermRecomposer> TypeToRecomposer = new Dictionary<Type, PrologTermRecomposer>();

        static public void AddPrologTermRecomposer(Type type, string module, string obj2r, string r2obj, string functorWrapper, int arityWrapper)
        {
            PrologTermRecomposer layout = new PrologTermRecomposer();
            layout.obj2r = obj2r;
            layout.module = module;
            layout.r2obj = r2obj;
            layout.Name = functorWrapper;
            layout.Arity = arityWrapper;
            layout.ToType = type;
            lock (FunctorToRecomposer)
            {
                FunctorToRecomposer[functorWrapper + "/" + arityWrapper] = layout;
                TypeToRecomposer[type] = layout;
            }
        }
        public static T GetTypeMap<T>(Type t, IDictionary<Type, T> mapped)
        {
            T layout;
            if (mapped.TryGetValue(t, out layout))
            {
                return layout;
            }

            Type bestType = null;
            foreach (KeyValuePair<Type, T> map in mapped)
            {
                Type mapKey = map.Key;
                if (mapKey.IsAssignableFrom(t))
                {
                    if (bestType == null || mapKey.IsAssignableFrom(bestType))
                    {
                        bestType = mapKey;
                        layout = map.Value;
                    }
                }
            }
            return layout;// default(T);
        }

        private static MemberInfo[] GetStructFormat(Type t)
        {
            if (false)
            {
                lock (FunctorToLayout)
                {
                    PrologTermLayout layout;
                    if (TypeToLayout.TryGetValue(t, out layout))
                    {
                        return layout.FieldInfos;
                    }
                }
            }
            bool specialXMLType = false;
            if (!t.IsEnum)
            {
                var ta = t.GetCustomAttributes(typeof(XmlTypeAttribute), false);
                if (ta != null && ta.Length > 0)
                {
                    XmlTypeAttribute xta = (XmlTypeAttribute)ta[0];
                    specialXMLType = true;
                }
            }
            MemberInfo[] tGetFields = null;
            if (specialXMLType)
            {
                List<MemberInfo> fis = new List<MemberInfo>();
                foreach (var e in t.GetFields(InstanceFields))
                {
                    var use = e.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                    if (use == null || use.Length < 1)
                    {
                        continue;
                    }
                    fis.Add(e);
                }
                foreach (var e in t.GetProperties(InstanceFields))
                {
                    var use = e.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                    if (use == null || use.Length < 1)
                    {
                        continue;
                    }
                    fis.Add(e);
                }
                tGetFields = fis.ToArray();
            }
            else
            {
                // look for [StructLayout(LayoutKind.Sequential)]

                var ta = t.GetCustomAttributes(typeof(StructLayoutAttribute), false);
                if (ta != null && ta.Length > 0)
                {
                    StructLayoutAttribute xta = (StructLayoutAttribute)ta[0];
                    // ReSharper disable ConditionIsAlwaysTrueOrFalse
                    if (xta.Value == LayoutKind.Sequential || true /* all sequential layouts*/)
                    // ReSharper restore ConditionIsAlwaysTrueOrFalse
                    {
                        tGetFields = t.GetFields(InstanceFields);
                    }
                }
                if (tGetFields == null)
                    tGetFields = t.GetFields(InstanceFields);
            }
            if (tGetFields.Length == 0)
            {
                Embedded.Warn("No fields in {0}", t);
            }
            return tGetFields;
        }


        public static int ToFieldLayout(string named, string arg1, object o, Type t, PlTerm term, bool childs, bool addNames)
        {

            MemberInfo[] tGetFields = GetStructFormat(t);

            int len = tGetFields.Length;
            PlTermV tv = NewPlTermV(len + 1);
            tv[0].UnifyAtom(arg1);
            int tvi = 1;
            for (int i = 0; i < len; i++)
            {
                object v = GetMemberValue(tGetFields[i], o);
                if (v is IList)
                {
                    v.GetType();
                }
                tv[tvi++].FromObject((v));
            }
            if (true)
            {
                return PlSucceedOrFail(term.Unify(PlC(named, tv)));
            }
            uint termTermRef = term.TermRef;

            uint temp = libpl.PL_new_term_ref();
            libpl.PL_cons_functor_v(temp,
                                    libpl.PL_new_functor(libpl.PL_new_atom(named), tv.Size),
                                    tv.A0);
            return libpl.PL_unify(termTermRef, temp);
        }

        private static object GetMemberValue(MemberInfo field, object o)
        {
            if (field is FieldInfo)
            {
                return ((FieldInfo)field).GetValue(o);
            }
            if (field is PropertyInfo)
            {
                return ((PropertyInfo)field).GetGetMethod(true).Invoke(o, null);
            }
            if (field is MethodInfo)
            {
                MethodInfo mi = (MethodInfo)field;
                if (mi.IsStatic) return mi.Invoke(null, new object[] { o });
                return ((MethodInfo)field).Invoke(o, null);
            }
            if (field is ConstructorInfo)
            {
                return ((ConstructorInfo)field).Invoke(new object[] { o });
            }
            throw new IndexOutOfRangeException("" + field);
        }

        public static void SetMemberValue(MemberInfo field, object o, object value)
        {
            if (field is FieldInfo)
            {
                ((FieldInfo)field).SetValue(o, value);
                return;
            }
            if (field is PropertyInfo)
            {
                MethodInfo setterMethod = ((PropertyInfo)field).GetSetMethod(true);
                if (setterMethod == null)
                {
                    Embedded.Warn("No setter method on {0}", field);
                    return;
                }
                setterMethod.Invoke(o, new object[] { value });
                return;
            }
            if (field is MethodInfo)
            {
                MethodInfo mi = (MethodInfo)field;
                ParameterInfo[] pms = mi.GetParameters();
                if (mi.IsStatic)
                {
                    if (pms.Length == 1)
                    {
                        mi.Invoke(null, new object[] { value });
                    }
                    mi.Invoke(null, new object[] { o, value });
                    return;
                }
                if (pms.Length == 1)
                {
                    mi.Invoke(o, new object[] { value });
                }
                else
                {

                }
                return;
            }
            throw new IndexOutOfRangeException("" + field);
            if (field is ConstructorInfo)
            {                
                ((ConstructorInfo)field).Invoke(new object[] { o, value });
                return;
            }
        }

        private static Type FieldType(MemberInfo field, bool asSetter)
        {
            if (field is FieldInfo)
            {
                return ((FieldInfo)field).FieldType;
            }
            if (field is PropertyInfo)
            {
                return ((PropertyInfo)field).PropertyType;
            }
            if (field is MethodInfo)
            {
                MethodInfo mi = (MethodInfo)field;
                if (asSetter)
                {
                    var pms = mi.GetParameters();
                    if (pms.Length == 0) return mi.ReturnType;
                    return pms[pms.Length - 1].ParameterType;
                }
                return mi.ReturnType;
            }
            if (field is ConstructorInfo)
            {
                return ((ConstructorInfo)field).DeclaringType;
            }
            throw new IndexOutOfRangeException("" + field);
        }

        private static bool IsStructRecomposable(Type t)
        {
            return t.IsValueType && !t.IsEnum && !t.IsPrimitive &&
                   (!t.Namespace.StartsWith("System") || t == typeof(DateTime)) &&
                   !typeof(IEnumerator).IsAssignableFrom(t) &&
                   !typeof(ICloneable).IsAssignableFrom(t) &&
                   !typeof(IEnumerable).IsAssignableFrom(t) &&
                   !typeof(ICollection).IsAssignableFrom(t);
        }
        private static object MakeDefaultInstance(Type type)
        {
            try
            {
                return Activator.CreateInstance(type);
            }
            catch (Exception e)
            {
                Embedded.Error("MakeDefaultInstance: " + type + " caused " + e);
                throw;
            }
        }

        private static Type argOneType(MemberInfo info)
        {
            {
                var mi = info as MethodInfo;
                if (mi != null)
                {

                    ParameterInfo[] miGetParameters = mi.GetParameters();
                    if (miGetParameters.Length > 0)
                    {
                        return miGetParameters[0].ParameterType;
                    }
                }
            }
            {
                var mi = info as ConstructorInfo;
                if (mi != null)
                {

                    ParameterInfo[] miGetParameters = mi.GetParameters();
                    if (miGetParameters.Length > 0)
                    {
                        return miGetParameters[0].ParameterType;
                    }
                }
            }
            var fi = info as FieldInfo;
            if (fi != null)
            {
                return fi.FieldType;
            }
            return typeof(object);
        }

        private static object CreateInstance(Type type, MemberInfo[] fis, PlTerm orig, int plarg)
        {
            int fisLength = fis.Length;
            if (orig.Arity < fisLength)
            {
                fisLength = orig.Arity;
                Embedded.Warn("Struct length mismatch");
            }
            object[] paramz = new object[fisLength];
            for (int i = 0; i < fisLength; i++)
            {
                MemberInfo fi = fis[i];
                PlTerm origArg = orig[plarg];
                paramz[i] = CastTerm(origArg, FieldType(fi, true));
                plarg++;
            }
            object newStruct = null;
            try
            {
                newStruct = MakeDefaultInstance(type);
            }
            catch (MissingMethodException)
            {
                foreach (ConstructorInfo ci in type.GetConstructors(BindingFlagsALL))
                {
                    if (ci.GetParameters().Length != paramz.Length) continue;
                    if (ci.IsStatic) continue;
                    newStruct = ci.Invoke(paramz);
                    cliTLMem()["ci"] = ci;
                }
                if (newStruct != null) return newStruct;
            }
            for (int i = 0; i < fis.Length; i++)
            {
                MemberInfo fi = fis[i];
                SetMemberValue(fi, newStruct, paramz[i]);
            }


            return newStruct;
        }

        static IEnumerable Unfold(object value, out bool unFolded)
        {
            IList<object> results = new List<object>();
            var type = value.GetType();
            var utype = Enum.GetUnderlyingType(type);
            var values = Enum.GetValues(type);
            if (utype == typeof(byte) || utype == typeof(sbyte) || utype == typeof(Int16) || utype == typeof(UInt16) || utype == typeof(Int32))
            {
                unFolded = true;
                var num = (Int32)Convert.ChangeType(value, typeof(Int32));
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (Int32)Convert.ChangeType(val, typeof(Int32));
                    if (v == 0) continue;
                    if ((v & num) == v)
                    {
                        results.Add(Enum.ToObject(value.GetType(), val));
                    }
                }
            }
            else if (utype == typeof(UInt32))
            {
                unFolded = true;
                var num = (UInt32)value;
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (UInt32)Convert.ChangeType(val, typeof(UInt32));
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else if (utype == typeof(Int64))
            {
                unFolded = true;
                var num = (Int64)value;
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (Int64)Convert.ChangeType(val, typeof(Int64));
                    if (v == 0L)
                    {
                        continue;
                    }
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else if (utype == typeof(UInt64))
            {
                unFolded = true;
                var num = (UInt64)value;
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (UInt64)Convert.ChangeType(val, typeof(UInt64));
                    if (v == 0U)
                    {
                        continue;
                    }
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else
            {
                throw new NotSupportedException("Unfold: " + utype );
            }
            return results;
        }

        private delegate void WithEnum(PlTerm p);
        private void ForEachEnumValue(WithEnum withValue, object p)
        {
            Type pType = p.GetType();
            if (!CycTypeInfo.IsFlagType(pType))
            {
                PlTerm fort = (PlTerm)ToFort(p);
                withValue(fort);
                return;
            }
            Array pTypeValues = Enum.GetValues(pType);
            Array.Reverse(pTypeValues);

            if (p is byte)
            {
                byte b = (byte)p;
                if (b == 0)
                {
                    withValue((PlTerm)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    byte bv = (byte)v;
                    if (bv >= b)
                    {
                        withValue((PlTerm)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is sbyte)
            {
                sbyte b = (sbyte)p;
                if (b == 0)
                {
                    withValue((PlTerm)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    sbyte bv = (sbyte)v;
                    if (bv >= b)
                    {
                        withValue((PlTerm)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt16)
            {
                ushort b = (UInt16)p;
                if (b == 0)
                {
                    withValue((PlTerm)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    ushort bv = (ushort)v;
                    if (bv >= b)
                    {
                        withValue((PlTerm)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int16)
            {
                short b = (Int16)p;
                if (b == 0)
                {
                    withValue((PlTerm)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    short bv = (short)v;
                    if (bv >= b)
                    {
                        withValue((PlTerm)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt32)
            {
                uint b = (UInt32)p;
                if (b == 0)
                {
                    withValue((PlTerm)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    uint bv = (uint)v;
                    if (bv >= b)
                    {
                        withValue((PlTerm)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int32)
            {
                int b = (Int32)p;
                if (b == 0)
                {
                    withValue((PlTerm)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    int bv = (int)v;
                    if (bv >= b)
                    {
                        withValue((PlTerm)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt64)
            {
                ulong b = (UInt64)p;
                if (b == 0)
                {
                    withValue((PlTerm)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    ulong bv = (ulong)v;
                    if (bv >= b)
                    {
                        withValue((PlTerm)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int64)
            {
                long b = (Int64)p;
                if (b == 0)
                {
                    withValue((PlTerm)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    long bv = (long)v;
                    if (bv >= b)
                    {
                        withValue((PlTerm)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            string s = p.ToString();
            bool unfolded;
            foreach (var unfold in Unfold(p, out unfolded))
            {
                withValue((PlTerm)ToFort(unfold));
                //return;
            }
            if (unfolded) return;
            Trace();
            if (p is IConvertible)
            {
                withValue((PlTerm)ToFort(p));
                return;
            }

            if (p is Enum)
            {
                withValue((PlTerm)ToFort(p));
                return;
            }
            withValue((PlTerm)ToFort(p));
        }

    }

}
