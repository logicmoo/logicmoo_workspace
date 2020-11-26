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
#if USE_IKVM
//using JClass = java.lang.JClass;
#else
using JClass = System.Type;
#endif
using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using SbsSW.SwiPlCs;
using PlTerm = SbsSW.SwiPlCs.PlTerm;
using OBJ_TO_OBJ = System.Func<object, object>;

namespace Swicli.Library
{
 /*   public delegate void Action();
    public delegate void Action<T>(T arg);
    public delegate TResult Func<TResult>();
    public delegate TResult Func<T, TResult>(T arg);
    public delegate TResult Func_22<T, TResult>(T arg);
    public delegate object OBJ_TO_OBJ(object from);
    */
    public partial class PrologCLR
    {

        [PrologVisible]
        static public bool cliCast(PlTerm valueIn, PlTerm clazzSpec, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliCast(valueIn, clazzSpec, plvar) && SpecialUnify(valueOut, plvar);
            }
            Type type = GetType(clazzSpec);
            if (type == null)
            {
                return Embedded.Error("Cant find class {0}", clazzSpec);
            }
            if (valueIn.IsVar)
            {
                return Embedded.Error("Cant find instance {0}", valueIn);
            }
            object retval = CastTerm(valueIn, type);
            return UnifyTagged(retval, valueOut);
        }

        [PrologVisible]
        static public bool cliCastImmediate(PlTerm valueIn, PlTerm clazzSpec, PlTerm valueOut)
        {
            if (valueIn.IsVar)
            {
                return Embedded.Warn("Cant find instance {0}", valueIn);
            }
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliCastImmediate(valueIn, clazzSpec, plvar) && SpecialUnify(valueOut, plvar);
            }
            Type type = GetType(clazzSpec);
            object retval = CastTerm(valueIn, type);
            return valueOut.FromObject(retval);
        }

        public static Object CastTerm(PlTerm o, Type pt)
        {
            if (pt == typeof(object)) pt = null;
            object r = CastTerm0(o, pt);
            if (pt == null || r == null)
                return r;
            Type fr = r.GetType();
            if (pt.IsInstanceOfType(r)) return r;
            return RecastObject(pt, r, fr);
        }

        public static readonly Dictionary<string, List<OBJ_TO_OBJ>> convertCache =
            new Dictionary<string, List<OBJ_TO_OBJ>>();

        public static object RecastObject(Type pt, object r, Type fr)
        {
            Exception ce = null;
            try
            {
                if (r is double && pt == typeof(float))
                {
                    double d = (double)r;
                    return Convert.ToSingle(d);
                }
            }
            catch (InvalidCastException e)
            {
                Embedded.Debug("conversion {0} to {1} resulted in {2}", fr, pt, e);
                ce = e;
            }
            catch (Exception e)
            {
                Embedded.Debug("conversion {0} to {1} resulted in {2}", fr, pt, e);
                ce = e;
            }
            try
            {
                var tryAll = GetConvertCache(fr, pt);
                if (tryAll != null && tryAll.Count > 0)
                {
                    ce = null;
                    int wn = 0;
                    bool somethingWorked = false;
                    object retVal = null;
                    foreach (var ti in tryAll)
                    {
                        try
                        {
                            retVal = ti(r);
                            somethingWorked = true;
                            if (pt.IsInstanceOfType(retVal))
                            {
                                if (wn != 0)
                                {
                                    // put this conversion method first in list
                                    tryAll.RemoveAt(wn);
                                    tryAll.Insert(0, ti);
                                }
                                return retVal;
                            }
                        }
                        catch (Exception ee)
                        {
                            ce = ce ?? ee;
                        }
                        wn++;
                    }
                    if (somethingWorked)
                    {
                        // probly was a null->null conversion
                        return retVal;
                    }
                }
            }
            catch (Exception e)
            {
                Embedded.Debug("conversion {0} to {1} resulted in {2}", fr, pt, e);
                ce = ce ?? e;
            }
            Embedded.Warn("Having a time converting {0} to {1} why {2}", r, pt, ce);

            return r;
        }

        private static List<OBJ_TO_OBJ> GetConvertCache(Type from, Type to)
        {
            string key = "" + @from + "->" + to;
            List<OBJ_TO_OBJ> found;
            bool wasNew = true;
            lock (convertCache)
            {
                wasNew = !convertCache.TryGetValue(key, out found);
                if (wasNew)
                {
                    found = convertCache[key] = new List<OBJ_TO_OBJ>();
                }
            }
            if (wasNew)
            {
                findConversions(@from, to, found);
            }
            return found;
        }
        private static void registerConversion(MethodInfo converterMethod, Type from, Type to)
        {
            int fromArg = 0;
            if (to == null)
            {
                to = converterMethod.ReturnType;
            }
            if (@from == null)
            {
                @from = (converterMethod.GetParameters()[fromArg]).ParameterType;
            }
            OBJ_TO_OBJ meth = (a) => converterMethod.Invoke(null, new[] { a });
            string key = "" + @from + "->" + to;
            List<OBJ_TO_OBJ> found;
            bool wasNew = true;
            lock (convertCache)
            {
                wasNew = !convertCache.TryGetValue(key, out found);
                if (wasNew)
                {
                    found = convertCache[key] = new List<OBJ_TO_OBJ>();
                }
                found.Add(meth);
            }
        }

        private static readonly List<Type> ConvertorClasses = new List<Type>();
        public static T ReflectiveCast<T>(object o)
        {
            T t =  (T) o;
            return t;
        }

        public static T ReflectiveNull<T>()
        {
            T t = default(T);
            return t;
        }

        private static void CheckMI()
        {
            if (MissingMI == null)
            {
                MissingMI = typeof(PrologCLR).GetMethod("ReflectiveCast", BindingFlagsJustStatic);
            }
            if (MakeDefaultViaReflectionInfo == null)
            {
                MakeDefaultViaReflectionInfo = typeof(PrologCLR).GetMethod("ReflectiveNull", BindingFlagsJustStatic);
            }
        }

        private static OBJ_TO_OBJ findConversions(Type from, Type to, ICollection<OBJ_TO_OBJ> allMethods)
        {
            OBJ_TO_OBJ meth;
            if (to.IsAssignableFrom(@from))
            {
                meth = (r) => r;
                if (allMethods != null) allMethods.Add(meth);
                else return meth;
            }
            OBJ_TO_OBJ sysmeth = (r) =>
                                               {
                                                   CheckMI();
                                                   if (r == null)
                                                   {
                                                       MethodInfo rc = MakeDefaultViaReflectionInfo.MakeGenericMethod(to);
                                                       return rc.Invoke(null, ZERO_OBJECTS);
                                                   }
                                                   else
                                                   {
                                                       MethodInfo rc = MissingMI.MakeGenericMethod(to);
                                                       return rc.Invoke(null, new[] { r });
                                                   }
                                               };
            if (to.IsValueType)
            {
                if (allMethods != null) allMethods.Add(sysmeth);
                else
                {
                    // dont return .. this is the fallthru at bottem anyhow
                    // return sysmeth;
                }
            }
            if (to.IsEnum)
            {
                meth = (r) =>
                           {
                               if (r == null)
                               {
                                   return null;
                               }
                               string rs = r.ToString();
                               if (rs.Length == 0) return null;
                               return Enum.Parse(to, rs);
                           };
                if (allMethods != null) allMethods.Add(meth);
                else return meth;
            }
            if (to.IsPrimitive)
            {
                meth = ((r) => Convert.ChangeType(r, to));
                if (allMethods != null) allMethods.Add(meth);
                else return meth;
            }
            if (to.IsArray && @from.IsArray)
            {
                var eto = to.GetElementType();
                var efrom = @from.GetElementType();
                meth = ((r) =>
                            {
                                Array ar = ((Array)r);
                                int len = ar.Length;
                                Array ret = Array.CreateInstance(eto, len);
                                for (int i = 0; i < len; i++)
                                {
                                    ret.SetValue(RecastObject(eto, ar.GetValue(i), efrom), i);
                                }
                                return ret;
                            });
                if (allMethods != null) allMethods.Add(meth);
                else return meth;
            }
            ConstructorInfo ci = to.GetConstructor(new Type[] { @from });
            if (ci != null)
            {
                meth = (r) => ci.Invoke(new object[] { r });
                if (allMethods != null) allMethods.Add(meth);
                else return meth;
            }
            ConstructorInfo pc = null;
            foreach (ConstructorInfo mi in to.GetConstructors(BindingFlagsALL))
            {
                var ps = mi.GetParameters();
                if (ps.Length == 0 && !mi.IsStatic)
                {
                    pc = mi;
                    continue;
                }
                if (ps.Length == 1)
                {
                    Type pt = ps[0].ParameterType;
                    if (pt.IsAssignableFrom(@from))
                    {
                        ConstructorInfo info = mi;
                        meth = (r) => info.Invoke(new object[] { r });
                        if (allMethods != null) allMethods.Add(meth);
                        else return meth;
                    }
                }
            }
            // search for op_Implicit/Explicit
            var someStatic = SomeConversionStaticMethod(to, to, @from, allMethods, false);
            if (someStatic != null) return someStatic;
            // search for op_Implicit/Explicit
            someStatic = SomeConversionStaticMethod(to, @from, @from, allMethods, false);
            if (someStatic != null) return someStatic;

            if (ConvertorClasses.Count == 0)
            {
                cliRegisterConvertor(typeof(Convert),false);
                cliRegisterConvertor(typeof(PrologConvert),true);
                //ConvertorClasses.Add(typeof(PrologCLR));
            }
            foreach (Type convertorClasse in ConvertorClasses)
            {
                var someStaticM = SomeConversionStaticMethod(to, convertorClasse, @from, allMethods, false);
                if (someStaticM != null) return someStatic;
            }
            //if (PrologBinder.CanConvertFrom(from, to))
            {
                meth = (r) => Convert.ChangeType(r, to);
                if (allMethods != null) allMethods.Add(meth);
                else return meth;
            }
            // search for toWhatnot (very bad should be done last)
            foreach (MethodInfo mi in @from.GetMethods(BindingFlagsInstance))
            {
                if (!mi.IsStatic)
                {
                    var ps = mi.GetParameters();
                    if (ps.Length == 0)
                    {
                        if (!to.IsAssignableFrom(mi.ReturnType)) continue;
                        //Type pt = ps[0].ParameterType;
                        //if (pt.IsAssignableFrom(from))
                        {
                            MethodInfo info = mi;
                            meth = (r) => info.Invoke(r, ZERO_OBJECTS);
                            if (allMethods != null) allMethods.Add(meth);
                            else return meth;
                        }
                    }
                }
            }
            // search for to.Whatnot (very very bad should be done last)
            if (pc != null)
            {
                meth = null;
                int fieldCount = 0;
                foreach (var f in to.GetFields(BindingFlagsInstance))
                {
                    fieldCount++;
                    if (fieldCount > 1)
                    {
                        // too many fields
                        break;
                    }
                    FieldInfo info = f;
                    meth = (r) =>
                               {
                                   var ret = pc.Invoke(null);
                                   info.SetValue(ret, r);
                                   return ret;
                               };
                }
                if (fieldCount == 1 && meth != null)
                {
                    if (allMethods != null) allMethods.Add(meth);
                    else return meth;
                }
            }

            return sysmeth;
        }

        public static void cliRegisterConvertor(Type p0)
        {
            cliRegisterConvertor(p0,true);
        }
        private static void cliRegisterConvertor(Type p0, bool addAll)
        {
            lock (ConvertorClasses)
            {
                if (ConvertorClasses.Contains(p0)) return;
                ConvertorClasses.Insert(0,p0);
            }
            MethodInfo[] methods = p0.GetMethods(BindingFlagsJustStatic);
            if (!addAll)
            {

                addAll = hadAttribute(p0, typeof(TypeConversionAttribute),false);
   
            }

            foreach (var m in methods)
            {
                if (!addAll)
                {
                    if (!hadAttribute(m,typeof(TypeConversionAttribute), false)) continue;
                }
                registerConversion(m, null, null);
            }

            
            

        }


        private static bool hadAttribute(MemberInfo p0, Type attribute, bool inherit)
        {
            object[] f = p0.GetCustomAttributes(attribute, inherit);
            if (f == null || f.Length == 0) return false;
            return true;
        }


        /// <summary>
        /// This finds things like op_Implicit/op_Explicition to
        /// </summary>
        /// <param name="to"></param>
        /// <param name="srch"></param>
        /// <param name="from"></param>
        /// <param name="allMethods"></param>
        /// <param name="onlyConverionAttribute"></param>
        /// <returns></returns>
        private static OBJ_TO_OBJ SomeConversionStaticMethod(Type to, Type srch, Type from, ICollection<OBJ_TO_OBJ> allMethods, bool onlyConverionAttribute)
        {
            OBJ_TO_OBJ meth;
            foreach (MethodInfo mi in srch.GetMethods(BindingFlagsJustStatic))
            {
                if (mi.IsStatic)
                {
                    var ps = mi.GetParameters();
                    if (ps.Length == 1)
                    {
                        if (!to.IsAssignableFrom(mi.ReturnType)) continue;
                        if (onlyConverionAttribute)
                        {
                            if(!hadAttribute(mi,typeof(TypeConversionAttribute), true)) continue;
                        }
                        Type pt = ps[0].ParameterType;
                        if (pt.IsAssignableFrom(@from))
                        {
                            MethodInfo info = mi;
                            meth = (r) => info.Invoke(null, new object[] { r });
                            if (allMethods != null) allMethods.Add(meth); else return meth;
                        }
                    }
                }
            }
            return null;
        }

        public static Object CastTerm0(PlTerm o, Type pt)
        {
            return CastTerm1(o, pt);
        }

        public static Object CastTerm1(PlTerm o, Type pt)
        {
            if (pt == typeof(PlTerm)) return o;
            if (pt == typeof(string))
            {
                if (IsTaggedObject(o))
                {
                    return "" + GetInstance(o);
                }
                return (string)o;
            }
            if (pt != null && pt.IsSubclassOf(typeof(Delegate)))
            {
                return cliNewDelegateTerm(pt, o, false);
            }
            if (pt == typeof(Type))
            {
                return GetType(o);
            }
            PlType plType = o.PlType;
            switch (plType)
            {
                case PlType.PlUnknown:
                    {
                        return (string)o;
                    }
                    break;
                case PlType.PlVariable:
                    {
                        return o;
                    }
                    break;
                case PlType.PlInteger:
                    {
                        int i = 0;
                        if (0 != libpl.PL_get_integer(o.TermRef, ref i))
                            return i;
                        try
                        {
                            return (long)o;
                        }
                        catch (Exception)
                        {
                        }
                        try
                        {
                            return (ulong)o;
                        }
                        catch (Exception)
                        {
                            return ToBigInteger((string)o);
                        }
                    }
                    break;
                case PlType.PlFloat:
                    {
                        try
                        {
                            return (double)o;
                        }
                        catch (Exception)
                        {
                            return ToBigDecimal((string)o);
                        }
                    }
                    break;
                case PlType.PlNil:
                {
                    return CoerceNil(pt);
                }
                case PlType.PlAtom:
                case PlType.PlString:
                    {
                        if (plType == PlType.PlAtom && o.Name == "[]")
                        {
                            return CoerceNil(pt);
                        }
                        string s = (string)o;
                        if (pt == null) return s;
                        var constructor = pt.GetConstructor(ONE_STRING);
                        if (constructor != null)
                        {
                            return constructor.Invoke(new object[] { s });
                        }
                        foreach (var m in pt.GetMethods(BindingFlagsJustStatic))
                        {

                            ParameterInfo[] mGetParameters = m.GetParameters();
                            if (pt.IsAssignableFrom(m.ReturnType) && mGetParameters.Length == 1 &&
                                mGetParameters[0].ParameterType.IsAssignableFrom(typeof(string)))
                            {
                                Embedded.Debug("using " + m);
                                try
                                {
                                    return m.Invoke(null, new object[] { s });
                                }
                                catch (Exception la)
                                {
                                    Exception why = cliInnerException(la, -1);
                                    var issue = "Cast failed converion " + why;
                                    Embedded.Debug(issue);
                                    continue;
                                }
                            }
                        } foreach (var m in pt.GetFields(BindingFlagsJustStatic))
                        {

                            if (pt.IsAssignableFrom(m.FieldType) && m.Name == s)
                            {
                                Embedded.Debug("using static field " + m);
                                return m.GetValue(null);
                            }                     
                        }
                        return s;
                    }
                    break;
                case PlType.PlTerm:
                case PlType.PlListPair:
                    {
                        lock (ToFromConvertLock)
                        {
                            var o1 = o[1];
                            return CastCompoundTerm(o.Name, o.Arity, o1, o, pt);
                        }
                    }
                    break;
                default:
                    throw new ArgumentOutOfRangeException("plType=" + plType + " " + o.GetType() + " -> " + pt);
            }
        }

        private static object CoerceNil( Type pt)
        {
            if (pt != null && pt.IsArray)
            {
                return Array.CreateInstance(pt.GetElementType(), 0);
            } 
            return GetDefault(pt);
        }

        public static object GetDefault(Type type)
        {
            if (type.IsValueType)
            {
                return Activator.CreateInstance(type);
            }
            return null;
        }

        public static Exception cliInnerException(Exception la, int depth)
        {
            Exception why = la.InnerException;
            if (why == null) return la;
            if (why != la)
            {
                if (depth < -0 || depth > 0)
                {
                    depth--;
                    return cliInnerException(why, depth);
                }
            }
            return why;
        }

        private static int ToVMNumber(object o, PlTerm term)
        {
            if (o is int)
                return libpl.PL_unify_integer(term.TermRef, (int)Convert.ToInt32(o));
            if (PreserveObjectType)
            {
                return PlSucceedOrFail(UnifyTagged(o, term));
            }
            // signed types
            if (o is short || o is sbyte)
                return libpl.PL_unify_integer(term.TermRef, (int)Convert.ToInt32(o));
            if (o is long)
                return libpl.PL_unify_integer(term.TermRef, (long)Convert.ToInt64(o));
            if (o is decimal || o is Single || o is float || o is double)
                return libpl.PL_unify_float(term.TermRef, (double)Convert.ToDouble(o));
            // unsigned types
            if (o is ushort || o is byte)
                return libpl.PL_unify_integer(term.TermRef, (int)Convert.ToInt32(o));
            if (o is UInt32)
                return libpl.PL_unify_integer(term.TermRef, (long)Convert.ToInt64(o));
            // potentually too big?!
            if (o is ulong)
            {
                ulong u64 = (ulong)o;
                if (u64 <= Int64.MaxValue)
                {
                    return libpl.PL_unify_integer(term.TermRef, (long)Convert.ToInt64(o));
                }
                return PlSucceedOrFail(term.Unify(u64));
                //return libpl.PL_unify_float(term.TermRef, (double)Convert.ToDouble(o));
            }
            if (o is IntPtr)
            {
                return libpl.PL_unify_intptr(term.TermRef, (IntPtr)o);
            }
            if (o is UIntPtr)
            {
                return libpl.PL_unify_intptr(term.TermRef, (IntPtr)o);
            }
            return -1;
        }

        /*
         
  jpl_is_ref(@(Y)) :-
	atom(Y),        % presumably a (garbage-collectable) tag
	Y \== void,     % not a ref
	Y \== false,    % not a ref
	Y \== true.     % not a ref
         
         */
        private static object CastCompoundTerm(string name, int arity, PlTerm arg1, PlTerm orig, Type pt)
        {
            if (pt != null)
            {
                object tagObj = findTaggedObject(pt, orig, arg1, orig[arity]);
                if (tagObj != null && pt.IsInstanceOfType(tagObj)) return tagObj;
            }
            string key = name + "/" + arity;
            lock (FunctorToLayout)
            {
                PrologTermLayout pltl;
                if (FunctorToLayout.TryGetValue(key, out pltl))
                {
                    Type type = pltl.ObjectType;
                    MemberInfo[] fis = pltl.FieldInfos;
                    MemberInfo toType = pltl.ToType;
                    if (toType != null)
                    {
                        return GetMemberValue(toType, CastTerm(arg1, argOneType(toType)));
                    }
                    return CreateInstance(type, fis, orig, 1);
                }
            }
            lock (FunctorToRecomposer)
            {
                PrologTermRecomposer layout;
                if (FunctorToRecomposer.TryGetValue(key, out layout))
                {
                    Type type = layout.ToType;
                    uint newref = libpl.PL_new_term_ref();
                    PlTerm outto = new PlTerm(newref);
                    var ret = PlQuery.PlCall(layout.module, layout.r2obj, new PlTermV(orig, outto));
                    if (ret)
                    {
                        object o = CastTerm(outto, type);
                        if (!pt.IsInstanceOfType(o))
                        {
                            Embedded.Warn(type + " (" + o + ") is not " + pt);
                        }
                        return o;
                    }
                }
            }
            if (key == "[]/0")
            {
                if (pt != null)
                {
                    if (pt.IsArray)
                    {
                        return Array.CreateInstance(pt.GetElementType(), 0);
                    }
                    return MakeDefaultInstance(pt);
                }
                Embedded.Debug("Not sure what to convert `[]` too");
                return null;
            }
            if (key == "static/1")
            {
                return null;
            }
            if (key == "delegate/1")
            {
                return CastTerm0(arg1, pt);
            }
            if (key == "delegate/2")
            {
                return cliNewDelegateTerm(pt, orig, false);
            }
            if (key == "{}/1")
            {
                return arg1;
            }
            if (pt == typeof(object))
            {
                pt = null;
            }
            //{T}
            //@(_Tag)
            if (key == "@/1" && arg1.IsAtom)
            {
                name = arg1.Name;
                switch (name)
                {
                    case "true":
                        {
                            return true;
                        }
                    case "false":
                        {
                            return false;
                        }
                    case "null":
                        {
                            if (pt != null && pt.IsValueType)
                            {
                                return MakeDefaultInstance(pt);
                            }
                            return null;
                        }
                    case "void":
                        {
#if USE_IKVM
                            if (pt == typeof(void)) return JPL.JVOID;
#endif
                            return null;
                        }
                    default:
                        {
                            
                            {
                                object o = tag_to_object(name);
                                if (o == null)
                                {
                                    Embedded.Warn("Null from tag " + name);
                                }
                                return o;
#if plvar_pins                                
                                lock (ToFromConvertLock) lock (atomToPlRef)
                                {
                                    PlRef oldValue;
                                    if (!atomToPlRef.TryGetValue(name, out oldValue))
                                    {
                                        //Warn("no value for tag=" + name);
                                        if (pt != null && pt.IsInstanceOfType(o))
                                        {
                                            return o;
                                        }
                                        return o;
                                    }
                                    var v = oldValue.Value;
                                    if (pt != null && pt.IsInstanceOfType(v))
                                    {
                                        return v;
                                    }
                                    return v;
                                }
#endif
                            }
                        }
                }
            }
#if plvar_pins
            if (name == "$cli_object")
            {
                lock (ToFromConvertLock)
                {
                    lock (termToObjectPins)
                    {
                        PlRef oldValue;
                        Int64 ohandle = (long)arg1;
                        if (!termToObjectPins.TryGetValue(ohandle, out oldValue))
                        {
                            Warn("no value for ohandle=" + ohandle);
                        }
                        return oldValue.Value;
                    }
                }
            }
#endif
            if (key == "enum/2")
            {
                Type type = GetType(arg1);
                PlTerm arg2 = orig.Arg(1);
                object value = Enum.Parse(type, arg2.Name, true);
                if (value == null) Embedded.Warn("cant parse enum: {0} for type {1}", arg2, type);
                return value;
            }
            if (key == "array/2")
            {
                Type type = GetType(arg1);
                return CreateArrayOfTypeRankOneFilled(orig.Arg(1), type.MakeArrayType());
            }
            if (key == "array/3")
            {
                Type type = GetType(arg1);
                var ar = CreateArrayOfType(ToTermArray(orig.Arg(1)), type);
                FillArray(ToTermArray(orig.Arg(2)), type.GetElementType(), ar);
                return ar;
            }
            if (name == "values")
            {
                Embedded.Warn("Values array");
            }
            if (name == "struct" || name == "event" || name == "object")
            {
                Type type = GetType(arg1);
                MemberInfo[] fis = GetStructFormat(type);
                return CreateInstance(type, fis, orig, 2);
            }
            if (orig.IsList)
            {
                if (arg1.IsInteger || arg1.IsAtom)
                {
                    Embedded.Debug("maybe this is a string {0}", orig);
                }
                if (pt == null)
                {
                    var o1 = GetInstance(arg1);
                    if (false && o1 != null && IsTaggedObject(arg1) && arg1.IsCompound && !o1.GetType().IsPrimitive)
                    {                        
                        Embedded.Warn(" send a list into cliGet0 ", orig);
                        bool found;
                        var res = cliGet0(arg1, orig.Arg(1), o1.GetType(), out found,
                                          BindingFlagsALL3 | BindingFlagsALL);
                        if (found) return res;
                    }
                    Embedded.Debug("Return as array of object[]?", orig);
                    var o = CreateArrayNarrowest(ToObjectArray(ToTermArray(orig)));
                    return o;
                }
                else
                {
                    if (pt.IsArray)
                    {
                        return CreateArrayOfTypeRankOneFilled(orig, pt);
                    }
                    if (!typeof(IEnumerable).IsAssignableFrom(pt))
                    {
                        Embedded.Warn("Return as collection?", orig);
                    }
                    return CreateCollectionOfType(orig, pt);
                }

            }
            if (pt != null && pt.IsArray)
            {
                return CreateArrayOfTypeRankOneFilled(orig, pt);
            }
            Type t = ResolveType(name);
            if (t == null)
            {
                if (pt != null)
                {
                    object tagObj = findTaggedObject(pt, arg1, orig[arity]);
                    if (tagObj != null && pt.IsInstanceOfType(tagObj)) return tagObj;
                }
                else
                {
                    object tagObj = findTaggedObject(pt, arg1, orig[arity]);
                    if (tagObj != null) return tagObj;
                }
                Embedded.WarnMissing(String.Format("Cant GetInstance from {0}", orig));
                return orig;
            }
            if (pt == null || pt.IsAssignableFrom(t))
            {
                if (arity == 1)
                {
                    return CastTerm(arg1, t);
                }
                foreach (var m in t.GetConstructors())
                {
                    ParameterInfo[] mGetParameters = m.GetParameters();
                    if (mGetParameters.Length == arity)
                    {
                        Action postCallHook;
                        try
                        {
                            Embedded.WarnMissing("using contructor {0}", m);
                            var values = PlListToCastedArray(orig, mGetParameters, out postCallHook);
                            var retval = m.Invoke(values);
                            Dictionary<string, object> threadLocals = cliTLMem();
                            CommitPostCall(postCallHook);
                            return retval;
                        }
                        catch (Exception)
                        {
                        }

                    }
                }
            }
            Embedded.Debug("Get Instance fallthru");
            MemberInfo[] ofs = GetStructFormat(t);
            return CreateInstance(t, ofs, orig, 1);
        }
        
        private static object findTaggedObject(Type pt, params PlTerm[] args)
        {
            foreach (var arg1 in args)
            {
                if (IsTaggedObject(arg1))
                {
                    object tagObj = tag_to_object(arg1[1].Name);
                    if (tagObj != null)
                    {
                        if (pt == null || pt.IsInstanceOfType(tagObj)) return tagObj;
                    }
                }
            }
            return null;
        }
        
        [ThreadStatic] private static Dictionary<string, object> threadLocaldict;
        private static Dictionary<string, object> cliTLMem()
        {
            if (threadLocaldict==null) threadLocaldict = new Dictionary<string, object>();
            return threadLocaldict;
        }

    }
    public class TypeConversionAttribute : Attribute
    {
    }
    [TypeConversion]
    internal class PrologConvert //: OpenMetaverse.UUIDFactory
    {
        [TypeConversion]
        static public Guid ToGuid(object from)
        {
            return new Guid("" + from);
        }
        [TypeConversion]
        static public String ToStr(object from)
        {
            return "" + from;
        }
        [TypeConversion]
        static public Type ToType(PlTerm typeSpec)
        {
            return PrologCLR.GetTypeThrowIfMissing(typeSpec);
        }

        [TypeConversion]
        unsafe static public string Convert(char* from)
        {
            return new string(from);
        }

        [TypeConversion]
        unsafe static public char* Convert(Pointer from)
        {
            void* vp = Pointer.Unbox(from);
            char* cp;
            cp = (char*) vp;
            return cp;
        }

        [TypeConversion]
        public static unsafe char* Convert(String from)
        {
            fixed (char* p = from)
                return p;
        }
    }


}
