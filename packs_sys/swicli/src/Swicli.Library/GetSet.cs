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
using Class = java.lang.Class;
#endif

#region

using System;
using System.Reflection;
using SbsSW.SwiPlCs;

#endregion

namespace Swicli.Library
{
    public partial class PrologCLR
    {
        [PrologVisible]
        public static bool cliGetField(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliGetField(clazzOrInstance, memberSpec, plvar) && SpecialUnify(valueOut, plvar);
            }
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            if (!CheckBound(memberSpec)) return false;
            bool found;
            foreach (var searchFlags in getInstance == null ? BindingFlags_SEARCHS : BindingFlags_SEARCHIS)
            {
                object cliGet01 = cliGet0(getInstance, memberSpec, c, out found, searchFlags | BindingFlags.GetField);

                if (found)
                {
                    return valueOut.FromObject(cliGet01);
                }
            }
            return false;
        }

        [PrologVisible]
        public static bool cliSetField(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueIn)
        {
            if (!valueIn.IsVar)
            {
                return Embedded.Error("Cant set property with a var {0}", valueIn);
            }
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            if (!CheckBound(memberSpec)) return false;
            bool found;
            foreach (var searchFlags in getInstance == null ? BindingFlags_SEARCHS : BindingFlags_SEARCHIS)
            {
                if (cliSet0(getInstance, memberSpec, valueIn, c, searchFlags | BindingFlags.SetField)) return true;
            }
            return false;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="clazzOrInstance"></param>
        /// <param name="memberSpec">[] = 'Item'</param>
        /// <param name="indexValues"></param>
        /// <param name="valueOut"></param>
        /// <returns></returns>
        [PrologVisible]
        public static bool cliGetProperty(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm indexValues, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliGetProperty(clazzOrInstance, memberSpec, indexValues, plvar) && SpecialUnify(valueOut, plvar);
            }
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            Type[] paramz = null;
            BindingFlags searchFlags = BindingFlagsALL;
            if (!CheckBound(memberSpec, indexValues)) return false;
            var pi = findPropertyInfo(memberSpec, c, false, true, ref paramz, searchFlags);
            if (pi == null)
            {
                Embedded.Error("Cant find property {0} on {1}", memberSpec, c);
                return false;
            }
            Action postCallHook;
            var ps = PlListToCastedArray(indexValues, pi.GetIndexParameters(), out postCallHook);
            object cliGet01 = pi.GetValue(getInstance, ps);
            CommitPostCall(postCallHook);
            return valueOut.FromObject(cliGet01);
        }

        [PrologVisible]
        public static bool cliSetProperty(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm indexValues, PlTerm valueIn)
        {
            if (!valueIn.IsVar)
            {
                return Embedded.Error("Cant set property with a var {0}", valueIn);
            }
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            Type[] paramz = null;
            if (!CheckBound(memberSpec, indexValues, valueIn)) return false;
            BindingFlags searchFlags = BindingFlagsALL;
            var pi = findPropertyInfo(memberSpec, c, false, true, ref paramz, searchFlags);
            if (pi == null)
            {
                Embedded.Error("Cant find property {0} on {1}", memberSpec, c);
                return false;
            }
            Action postCallHook;
            var ps = PlListToCastedArray(indexValues, pi.GetIndexParameters(), out postCallHook);
            pi.SetValue(getInstance, CastTerm(valueIn, pi.PropertyType), ps);
            CommitPostCall(postCallHook);
            return true;
        }


        [PrologVisible]
        public static bool cliGetRaw(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliGetRaw(clazzOrInstance, memberSpec, plvar) && SpecialUnify(valueOut, plvar);
            }
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            if (!CheckBound(memberSpec)) return false;
            bool found;
            foreach (var searchFlags in getInstance == null ? BindingFlags_SEARCHS : BindingFlags_SEARCHIS)
            {
                object cliGet01 = cliGet0(getInstance, memberSpec, c, out found, searchFlags | BindingFlagsALL3);

                if (found)
                {
                    return valueOut.FromObject(cliGet01);
                }
            }
            return false;
        }

        public static object cliGet0(object getInstance, PlTerm memberSpec, Type c, out bool found, BindingFlags icbf)
        {
            Type[] paramz = null;
            paramz = GetParamSpec(memberSpec) ?? ZERO_TYPES;
            if ((icbf & BindingFlags.GetProperty) != 0)
            {
                var pi = findPropertyInfo(memberSpec, c, false, true, ref paramz, icbf);
                if (pi != null && pi.CanRead)
                {
                    var mi = pi.GetGetMethod();
                    if (mi != null)
                    {
                        found = true;
                        return ((InvokeCaught(mi, mi.IsStatic ? null : getInstance, ZERO_OBJECTS) ?? VoidOrNull(mi)));
                    }
                    Embedded.Warn("Cant find getter for property " + memberSpec + " on " + c + " for " + pi);
                    found = false;
                    return null;
                }
            }
            if ((icbf & BindingFlags.GetField) != 0)
            {
                FieldInfo fi = findField(memberSpec, c, icbf);
                if (fi != null)
                {
                    object fiGetValue = fi.GetValue(fi.IsStatic ? null : getInstance);
                    found = true;
                    return (fiGetValue);
                }
            }
            if ((icbf & BindingFlags.InvokeMethod) != 0)
            {
                if (memberSpec.IsVar)
                {
                    Embedded.Warn("cliGet0 on IsVar={0} on {1} for {2}", memberSpec, c, getInstance);
                    found = false;
                    return getInstance;
                }
                string fn = memberSpec.Name;
                MethodInfo mi = findMethodInfo(memberSpec, -1, c, ref paramz, icbf) ??
                                GetMethod(c, fn, icbf) ??
                                GetMethod(c, "get_" + fn, icbf) ??
                                GetMethod(c, "Get" + fn, icbf) ??
                                GetMethod(c, "Is" + fn, icbf) ??
                                GetMethod(c, "To" + fn, icbf);
                if (mi == null)
                {
                    Embedded.WarnMissing("Cant find getter " + memberSpec + " on " + c);
                    found = false;
                    return null;
                }
                Action postCallHook;
                object[] value = PlListToCastedArray(memberSpec, mi.GetParameters(), out postCallHook);
                object target = mi.IsStatic ? null : getInstance;
                object retval = InvokeCaught(mi, target, value, postCallHook) ?? VoidOrNull(mi);
                found = true;
                return retval;
            }
            else
            {
                found = false;
                return null;
            }
        }

        [PrologVisible]
        public static bool cliSetRaw(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm paramIn)
        {
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            foreach (var searchFlags in getInstance == null ? BindingFlags_SEARCHS : BindingFlags_SEARCHIS)
            {
                if (cliSet0(getInstance, memberSpec, paramIn, c, searchFlags | BindingFlagsALL3)) return true;
            }
            return false;
        }


        public static bool cliSet0(object getInstance, PlTerm memberSpec, PlTerm paramIn, Type c,
                                   BindingFlags searchFlags)
        {
            Type[] paramz = null;
            if ((searchFlags & BindingFlags.SetProperty) != 0)
            {
                var pi = findPropertyInfo(memberSpec, c, false, true, ref paramz, searchFlags);

                if (pi != null)
                {
                    var mi = pi.GetSetMethod();
                    if (mi != null)
                    {
                        object value = CastTerm(paramIn, pi.PropertyType);
                        object target = mi.IsStatic ? null : getInstance;
                        InvokeCaught(mi, target, new[] {value});
                        return true;
                    }
                    return Embedded.WarnMissing("Cant find setter for property " + memberSpec + " on " + c);
                }
            }
            if ((searchFlags & BindingFlags.SetField) != 0)
            {
                FieldInfo fi = findField(memberSpec, c, searchFlags);
                if (fi != null)
                {
                    object value = CastTerm(paramIn, fi.FieldType);
                    object target = fi.IsStatic ? null : getInstance;
                    fi.SetValue(target, value);
                    return true;
                }
            }
            if ((searchFlags & BindingFlags.InvokeMethod) != 0)
            {
                string fn = memberSpec.Name;
                MethodInfo mi = findMethodInfo(memberSpec, -1, c, ref paramz, searchFlags) ??
                                GetMethod(c, "set_" + fn, searchFlags) ??
                                GetMethod(c, "Set" + fn, searchFlags) ??
                                GetMethod(c, "from" + fn, searchFlags);
                if (mi == null)
                {
                    Embedded.WarnMissing("Cant find setter " + memberSpec + " on " + c);
                    return false;
                }
                Action postCallHook;
                object[] value = PlListToCastedArray(paramIn, mi.GetParameters(), out postCallHook);
                object target = mi.IsStatic ? null : getInstance;
                object retval = InvokeCaught(mi, target, value, postCallHook);
                return true; // valueOut.FromObject(retval);
            }
            Embedded.WarnMissing("Cant find setter " + memberSpec + " on " + c);
            return false;
        }


        private static MethodInfo GetMethod(Type type, string s, BindingFlags flags)
        {
            try
            {
                return type.GetMethod(s, flags);
            }
            catch (AmbiguousMatchException)
            {
                return null;
            }
            catch (MissingMethodException)
            {
                return null;
            }
            catch (Exception)
            {
                return null;
            }
        }

        private static MemberInfo findMember(PlTerm memberSpec, Type c)
        {
            return findMember(memberSpec, c, InstanceFields) ??
                   findMember(memberSpec, c, InstanceFields | BindingFlags.IgnoreCase);
        }

        private static MemberInfo findMember(PlTerm memberSpec, Type c, BindingFlags searchFlags)
        {
            if (IsTaggedObject(memberSpec))
            {
                var r = GetInstance(memberSpec) as MemberInfo;
                if (r != null) return r;
            }
            Type[] paramz = GetParamSpec(memberSpec) ?? ZERO_TYPES;
            return findField(memberSpec, c, searchFlags) ??
                   findPropertyInfo(memberSpec, c, true, true, ref paramz, searchFlags) ??
                   findMethodInfo(memberSpec, -1, c, ref paramz, searchFlags) ??
                   (MemberInfo) findPropertyInfo(memberSpec, c, false, false, ref paramz, searchFlags);
            //findConstructor(memberSpec, c));
        }

        private static FieldInfo findField(PlTerm memberSpec, Type c, BindingFlags searchFlags)
        {
            if (c == null)
            {
                Embedded.Error("findField no class for {0}", memberSpec);
                return null;
            }
            if (memberSpec.IsVar)
            {
                Embedded.Error("findField IsVar {0} on type {1}", memberSpec, c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                int ordinal = memberSpec.intValue();
                var mis = c.GetFields(BindingFlagsALL);
                if (ordinal < 0 || ordinal >= mis.Length) return null;
                return mis[ordinal];
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as FieldInfo;
                if (r != null) return r;
            }
            if (memberSpec.IsCompound)
            {
                if (memberSpec.Name != "f")
                {
                    return null;
                }
                return findField(memberSpec.Arg(0), c, searchFlags);
            }
            string fn = memberSpec.Name;
            if (fn == "[]") fn = "Get";
            FieldInfo fi = c.GetField(fn, searchFlags);
            return fi;
        }


        private static PropertyInfo findPropertyInfo(PlTerm memberSpec, Type c, bool mustHaveP, bool assumeParamTypes,
                                                     ref Type[] paramz, BindingFlags searchFlags)
        {
            if (c == null)
            {
                Embedded.Error("findProperty no class for {0}", memberSpec);
                return null;
            }
            if (memberSpec.IsVar)
            {
                Embedded.Error("findProperty IsVar {0} on type {1}", memberSpec, c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                int ordinal = memberSpec.intValue();
                var mis = c.GetProperties(BindingFlagsALL);
                if (ordinal < 0 || ordinal >= mis.Length) return null;
                return mis[ordinal];
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as PropertyInfo;
                if (r != null) return r;
            }
            if (memberSpec.IsCompound)
            {
                if (memberSpec.Name == "p")
                {
                    Type[] paramzN = null;
                    return findPropertyInfo(memberSpec.Arg(0), c, false, assumeParamTypes, ref paramzN, searchFlags);
                }
                if (mustHaveP) return null;
            }
            if (paramz == null)
            {
                //  Warn("using paramSpec {0}", ToString(memberSpec));
                paramz = GetParamSpec(memberSpec) ?? ZERO_TYPES;
            }
            string fn = memberSpec.Name;
            if (fn == "[]") fn = "Item";
            if (paramz == null || paramz.Length == 0)
                return c.GetProperty(fn, searchFlags) ?? c.GetProperty("Is" + fn, searchFlags);
            var ps = c.GetProperties(searchFlags);
            int len = paramz.Length;
            PropertyInfo nameMatched = null;
            bool ignoreCase0 = (BindingFlags.IgnoreCase & searchFlags) != 0;
            if (ignoreCase0)
            {
                fn = fn.ToLower();
            }
            foreach (PropertyInfo info in ps)
            {
                if (info.Name == fn || (ignoreCase0 && info.Name.ToLower() == fn))
                {
                    nameMatched = nameMatched ?? info;
                    ParameterInfo[] indexParameters = info.GetIndexParameters();
                    if (assumeParamTypes)
                    {
                        if (len == indexParameters.Length)
                        {
                            if (IsCompatTypes(paramz, GetObjectTypes(indexParameters)))
                            {
                                return info;
                            }
                            // incompat but ok
                            nameMatched = info;
                        }
                    }
                }
            }
            return c.GetProperty(fn, searchFlags) ?? c.GetProperty("Is" + fn, searchFlags) ?? nameMatched;
        }
    }
}