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
using org.jpl7;
using SbsSW.SwiPlCs;
#if USE_IKVM
//using jpl;
//using JClass = java.lang.JavaClass;
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
    public partial class PrologCLR
    {
        public static PlTerm ATOM_NIL { get { return PlTerm.PlAtom("[]"); } }
        public static PlTerm PLNULL { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("null")); } }
        public static PlTerm PLVOID { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("void")); } }
        public static PlTerm PLTRUE { get { return PlTerm.PlCompound("@", PlTerm.PlAtom(PreserveObjectType ? object_to_tag(true) : "true")); } }
        public static PlTerm PLFALSE { get { return PlTerm.PlCompound("@", PlTerm.PlAtom(PreserveObjectType ? object_to_tag(false) : "false")); } }

        [PrologVisible]
        public static bool cliMakeDefault(PlTerm typeSpec, PlTerm valueOut)
        {
            CheckMI();
            MethodInfo rc = MakeDefaultViaReflectionInfo.MakeGenericMethod(GetType(typeSpec));
            return UnifyTagged(rc.Invoke(null, ZERO_OBJECTS), valueOut);
        }

        public static object GetInstance(PlTerm classOrInstance)
        {
            if (classOrInstance.IsVar)
            {
                Embedded.Warn("GetInstance(PlVar) {0}", classOrInstance);
                return null;
            }
            if (!classOrInstance.IsCompound)
            {
                if (classOrInstance.IsString)
                {
                    String str = (string)classOrInstance;
                    return str;
                }
                if (classOrInstance.IsNil)
                {
                    return CastCompoundTerm("[]", 0, classOrInstance, classOrInstance, null);
                }
                if (classOrInstance.IsAtom)
                {
                    Type t = GetType(classOrInstance);
                    // we do this for static invokations like: cliGet('java.lang.Integer','MAX_VALUE',...)
                    // the arg1 denotes a type, then return null!
                    if (t != null) return null;
                    Embedded.Warn("GetInstance(atom) {0}", classOrInstance);
                    // possibly should always return null?!
                }

                return CastTerm(classOrInstance, null);
            }
            string name = classOrInstance.Name;
            int arity = classOrInstance.Arity;
            return CastCompoundTerm(name, arity, classOrInstance[1], classOrInstance, null);
        }

        /// <summary>
        /// Returns the Type when denoated by a 'namespace.type' (usefull for static instance specification)
        ///    if a @C#234234  the type of the object unless its a a class
        ///    c(a) => System.Char   "sdfsdf" =>  System.String   uint(5) => System.UInt32
        /// 
        ///    instanceMaybe maybe Null.. it is passed in so the method code doesn't have to call GetInstance again
        ///       on classOrInstance
        /// </summary>
        /// <param name="instanceMaybe"></param>
        /// <param name="classOrInstance"></param>
        /// <returns></returns>
        private static Type GetTypeFromInstance(object instanceMaybe, PlTerm classOrInstance)
        {
            if (!classOrInstance.IsNil)
            {
                if (classOrInstance.IsAtom)
                {
                    return GetType(classOrInstance);
                }
                if (classOrInstance.IsString)
                {
                    if (instanceMaybe != null) return instanceMaybe.GetType();
                    return typeof (string);
                }
                if (classOrInstance.IsCompound)
                {
                    if (classOrInstance.Name == "static")
                    {
                        return GetType(classOrInstance[1]);
                    }
                }
            }
            object val = instanceMaybe ?? GetInstance(classOrInstance);
            //if (val is Type) return (Type)val;
            if (val == null)
            {
                Embedded.Warn("GetTypeFromInstance: {0}", classOrInstance);
                return null;
            }
            return val.GetType();
        }

        private static bool SpecialUnify(PlTerm valueOut, PlTerm plvar)
        {
            bool b = valueOut.Unify(plvar);
            if (b) return true;
            object obj1 = GetInstance(plvar);
            if (ReferenceEquals(obj1, null))
            {
                return false;
            }
            Type t1 = obj1.GetType();
            object obj2 = CastTerm(valueOut, t1);
            if (ReferenceEquals(obj2, null))
            {
                return false;
            }
            Type t2 = obj2.GetType();
            if (obj1.Equals(obj2))
            {
                return true;
            }
            if (t1 == t2)
            {
                return false;
            }
            return false;
        }

        public static int UnifyAtom(uint TermRef, string s)
        {
            uint temp = libpl.PL_new_term_ref();
            libpl.PL_put_atom(temp, libpl.PL_new_atom_wchars(s.Length, s));
            return libpl.PL_unify(temp, TermRef);
        }

        private static bool UnifySpecialObject(PlTerm plTerm, object ret1)
        {
            if (plTerm.IsVar)
            {
                return plTerm.FromObject(ret1);
            }
            else
            {
                var plvar = PlTerm.PlVar();
                return plvar.FromObject(ret1) && SpecialUnify(plTerm, plvar);
            }
        }
        public static int UnifyToProlog(object o, PlTerm term)
        {
            if (!term.IsVar)
            {
                Embedded.Warn("Not a free var {0}", term);
                return libpl.PL_fail;
            }
            uint TermRef = term.TermRef;
            if (TermRef == 0)
            {
                Embedded.Warn("Not a allocated term {0}", o);
                return libpl.PL_fail;
            }

#if USE_IKVM
            org.jpl7.Term args = o as org.jpl7.Term;
            if (args != null) return UnifyToProlog(ToPLCS(args), term);
#endif
            if (PreserveObjectType)
            {
                return PlSucceedOrFail(UnifyTagged(o, term));
            }
            return UnifyToPrologImmediate(o, term);
        }

        public static object ToFromConvertLock = new object();
        public static int UnifyToPrologImmediate(object o, PlTerm term)
        {
            uint TermRef = term.TermRef;
            if (o is PlTerm)
            {
                return libpl.PL_unify(TermRef, ((PlTerm)o).TermRef);
            } 
            if (o is string)
            {
                string s = (string)o;
                switch (Embedded.VMStringsAsAtoms)
                {
                    case libpl.CVT_STRING:
                        {
                            try
                            {
                                return libpl.PL_unify_string_chars(TermRef, (string)o);
                            }
                            catch (Exception)
                            {

                                return UnifyAtom(TermRef, s);
                            }
                        }
                    case libpl.CVT_ATOM:
                        try
                        {
                            return libpl.PL_unify_atom_chars(TermRef, (string)o);
                        }
                        catch (Exception)
                        {

                            return UnifyAtom(TermRef, s);
                        }
                    case libpl.CVT_LIST:
                        return libpl.PL_unify_list_chars(TermRef, (string)o);
                    default:
                        Embedded.Warn("UNKNOWN VMStringsAsAtoms {0}", Embedded.VMStringsAsAtoms);
                        return libpl.PL_fail;
                }
            }
            if (o == null)
            {
                return AddTagged(TermRef, "null");
            }

            if (o is Type || o is Type)
            {
                if (true)
                {
                    //lock (ToFromConvertLock)
                    {
                        var tag = object_to_tag(o);
                        AddTagged(TermRef, tag);
                        return libpl.PL_succeed;
                    }
                }
                return PlSucceedOrFail(term.Unify(typeToSpec((Type)o)));
            }

            Type t = o.GetType();
            if (t == typeof(void))
            {
                return AddTagged(TermRef, "void");
            }
            if (o is ValueType)
            {
                if (o is bool)
                {
                    bool tf = (bool)o;
                    return AddTagged(TermRef, tf ? "true" : "false");
                }
                if (o is char)
                {
                    try
                    {
                        char ch = (char)o;
                        string cs = new string(ch, 1);
                        switch (Embedded.VMStringsAsAtoms)
                        {
                            case libpl.CVT_STRING:
                                return libpl.PL_unify_atom_chars(TermRef, cs);
                            case libpl.CVT_ATOM:
                                return libpl.PL_unify_atom_chars(TermRef, cs);
                            case libpl.CVT_LIST:
                                return libpl.PL_unify_integer(TermRef, (int)ch);
                            default:
                                Embedded.Warn("UNKNOWN VMStringsAsAtoms {0}", Embedded.VMStringsAsAtoms);
                                return libpl.PL_fail;
                        }
                    }
                    catch (Exception e)
                    {
                        Embedded.Warn("@TODO unmappable errors? {0} type {1}", o, t);
                        //
                    }
                }
                if (t.IsEnum)
                {
                    int res = FromEnum(TermRef, o, t);
                    ///term.ToString();
                    return res;
                }
                if (t.IsPrimitive)
                {
                    try
                    {
                        int res = ToVMNumber(o, term);
                        if (res == libpl.PL_succeed) return res;
                        if (res == libpl.PL_fail) return res;
                        if (res != -1)
                        {
                            // Warn("@TODO Missing code for ToVmNumber? " + o + " type " + t);
                            return res;
                        }
                        if (t.IsPrimitive)
                        {
                            Embedded.Warn("@TODO Missing code for primitive? {0} type {1}", o, t);
                        }
                    }
                    catch (Exception e)
                    {
                        Embedded.Warn("@TODO unmappable errors? {0} type {1}", o, t);
                    }
                }
            }
            lock (FunctorToLayout)
            {
                PrologTermLayout layout;
                if (TypeToLayout.TryGetValue(t, out layout))
                {
                    MemberInfo[] tGetFields = layout.FieldInfos;// GetStructFormat(t);
                    int len = tGetFields.Length;
                    PlTermV tv = NewPlTermV(len);
                    for (int i = 0; i < len; i++)
                    {
                        object v = GetMemberValue(tGetFields[i], o);
                        tv[i].FromObject((v));
                    }
                    return PlSucceedOrFail(term.Unify(PlC(layout.Name, tv)));
                }
            }
            lock (FunctorToRecomposer)
            {
                PrologTermRecomposer layout = GetTypeMap(t, TypeToRecomposer);
                if (layout != null)
                {
                    lock (ToFromConvertLock)
                    {
                        var tag = object_to_tag(o);
                        uint newref = libpl.PL_new_term_refs(2);
                        AddTagged(newref, tag);
                        PlTerm into = new PlTerm(newref);
                        PlTerm outto = new PlTerm(newref + 1);
                        var ret = PlQuery.PlCall(layout.module, layout.obj2r, new PlTermV(@into, outto));
                        if (ret)
                        {
                            return term.Unify(outto) ? libpl.PL_succeed
                                   : libpl.PL_fail;

                        }
                    }
                }
            }
            if (o is IList)
            {

            }
            if (IsStructRecomposable(t))
            {
                return ToFieldLayout("struct", typeToName(t), o, t, term, false, false);
            }
            if (o is EventArgs)
            {
                return ToFieldLayout("event", typeToName(t), o, t, term, false, false);
            }
            if (t.IsArray)
            {
                Array al = (Array) o;
                if (false && al.Length > 0 && al.Length < 1024)
                {
                    Type et = t.GetElementType();
                    object firstNonNull = null;
                    foreach (var ele in al)
                    {
                        if (ele!=null)
                        {
                            firstNonNull = ele;
                        }
                    }
                    var needMake = firstNonNull != null;
                    if (needMake)
                    {
                        PlTerm newVar = PlTerm.PlVar();
                        needMake = NeedsToMakeRef(firstNonNull, newVar);
                    }
                    if (!needMake)
                    {
                        return PlSucceedOrFail(unifyArrayToTerm(al, term));
                    }
                }
            }
            return PlObject(TermRef, o);
        }


        private static bool NeedsToMakeRef(object al, PlTerm newVar)
        {
            var resoreMadeARef = MadeARef;
            var restoreMakeNoRefs = MakeNoRefs;
            try
            {

                MadeARef = false;
                MakeNoRefs = true;
                int doit = UnifyToPrologImmediate(al, newVar);
                if (!MadeARef)
                {
                    return false;
                }
                return true;
            }
            finally
            {
                MadeARef = resoreMadeARef;
                MakeNoRefs = restoreMakeNoRefs;
            }
        }

        public static PlTerm C(string collection)
        {
            return PlTerm.PlAtom(collection);
        }

        private static int FromEnum(uint TermRef, object o, Type t)
        {
            uint temp = libpl.PL_new_term_ref();
            libpl.PL_cons_functor_v(temp,
                                    ENUM_2,
                                    new PlTermV(typeToSpec(t), PlTerm.PlAtom(o.ToString())).A0);
            return libpl.PL_unify(TermRef, temp);
        }

        private static uint _enum2;
        private static uint _obj1;

        protected static uint ENUM_2
        {
            get
            {
                if (_enum2 == 0)
                {
                    _enum2 = libpl.PL_new_functor(libpl.PL_new_atom("enum"), 2);
                }
                return _enum2;
            }
        }

        static object ToBigInteger(string value)
        {
            Type t;
            // Just Mono
            t = Type.GetType("Mono.Math.BigInteger");
            if (t != null)
            {
                var m = t.GetMethod("Parse", ONE_STRING);
                if (m != null) return m.Invoke(null, new object[] { value });
            }
            // .net 4.0 and Mono
            t = ResolveType("System.Numerics.BigInteger");
            if (t != null)
            {
                var m = t.GetMethod("Parse", ONE_STRING);
                if (m != null) return m.Invoke(null, new object[] { value });
            }
            // Just Mono Android
            t = ResolveType("Java.Math.BigInteger");
            if (t != null)
            {
                var m = t.GetMethod("Parse", ONE_STRING);
                if (m != null) return m.Invoke(null, new object[] { value });
            }

            // IKVM         
            t = ResolveType("java.math.BigInteger");
            if (t != null)
            {
                var m = t.GetConstructor(ONE_STRING);
                if (m != null) return m.Invoke(new object[] { value });
            }
#if USE_IKVM
            return new java.math.BigInteger(value);
#else
            if (!value.StartsWith("-")) return UInt64.Parse(value);
            return Int64.Parse(value);
#endif
        }
        static object ToBigDecimal(string value)
        {
            Type t;
            // Just Mono
            t = Type.GetType("Mono.Math.BigDecimal");
            if (t != null)
            {
                var m = t.GetMethod("Parse", ONE_STRING);
                if (m != null) return m.Invoke(null, new object[] { value });
            }
            // .net 4.0 and Mono
            t = ResolveType("System.Numerics.BigDecimal");
            if (t != null)
            {
                var m = t.GetMethod("Parse", ONE_STRING);
                if (m != null) return m.Invoke(null, new object[] { value });
            }
            // Just Mono Android
            t = ResolveType("Java.Math.BigDecimal");
            if (t != null)
            {
                var m = t.GetMethod("Parse", ONE_STRING);
                if (m != null) return m.Invoke(null, new object[] { value });
            }
            // IKVM   
            t = ResolveType("java.math.BigDecimal");
            if (t != null)
            {
                var m = t.GetConstructor(ONE_STRING);
                if (m != null) return m.Invoke(new object[] { value });
            }
#if USE_IKVM
            return new java.math.BigDecimal(value);
#else
            return Double.Parse(value);
#endif
        }

    }

}
