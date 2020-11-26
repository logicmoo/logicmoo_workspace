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
using JavaClass = java.lang.Class;
#endif

#region

using System;
using System.Collections.Generic;
using System.Reflection;
using SbsSW.SwiPlCs;

#endregion

namespace Swicli.Library
{
    public partial class PrologCLR
    {

        private static Type[] GetParamSpecFromObjects(PlTerm[] objectList)
        {
            if (objectList.Length == 0) return Type.EmptyTypes;
            return GetParamSpec(ToTermArray(objectList), true);
        }

        private static Type[] GetParamSpec(PlTerm memberSpec)
        {
            if (memberSpec.IsNil) return new Type[0]; 
            return memberSpec.IsCompound ? GetParamSpec(ToTermArray(memberSpec), false) : null;
        }
        private static Type[] GetParamSpecFromObjects(PlTerm memberSpec)
        {
            if (memberSpec.IsNil) return new Type[0];
            return memberSpec.IsCompound ? GetParamSpec(ToTermArray(memberSpec), false) : null;
        }

        private static Type[] GetParamSpec(PlTerm[] memberSpec, bool isObjects)
        {
            return GetParamSpec(memberSpec, isObjects, 0, -1);
        }

        private static Type[] GetParamSpec(PlTerm[] memberSpec, bool isObjects, int start, int length)
        {
            var specArray = memberSpec;
            int arity = specArray.Length - start;
            if (length > -1)
            {
                arity = length - start;
            }
            int end = start + arity;
            Type[] paramz = new Type[arity];
            for (int i = start; i < end; i++)
            {
                PlTerm info = specArray[i];
                if (!info.IsVar)
                {
                    var t = GetType(info, isObjects);
                    if (t == null)
                    {
                        t = typeof (object);
                    }
                    paramz[i] = t;
                }
            }
            return paramz;
        }

        private static EventInfo findEventInfo(PlTerm memberSpec, Type c, ref Type[] paramz, BindingFlags searchFlags)
        {
            if (memberSpec.IsVar)
            {
                Embedded.WarnMissing("findEventInfo IsVar {0} on type {1}", memberSpec, c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                int ordinal = memberSpec.intValue();
                var mis = c.GetEvents(BindingFlagsALL);
                if (ordinal < 0 || ordinal >= mis.Length) return null;
                return mis[ordinal];
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as EventInfo;
                if (r != null) return r;
            }
            if (memberSpec.IsCompound)
            {
                if (memberSpec.Name == "e")
                {
                    var arg1 = memberSpec.Arg(0);
                    if (arg1.IsInteger)
                    {
                        Type[] paramzN = null;
                        return findEventInfo(arg1, c, ref paramzN, searchFlags);
                    }
                }
            }
            if (c == null) return null;
            EventInfo ei = c.GetEvent(memberSpec.Name, searchFlags);
            if (ei != null) return ei;
            var members = c.GetEvents(searchFlags);
            if (members.Length == 0) return null;
            int arity = (paramz != null) ? paramz.Length : memberSpec.Arity;
            EventInfo candidate = null;
            foreach (var info in members)
            {
                var infos = info.GetRaiseMethod();
                ParameterInfo[] paramTypes = infos.GetParameters();
                if (paramTypes.Length == arity)
                {
                    if (ParamsMatch(paramz, paramTypes)) return info;
                    if (infos.IsStatic)
                    {
                        if (candidate == null)
                        {
                            candidate = info;
                        }
                    }
                    else
                    {
                        if (candidate == null)
                        {
                            candidate = info;
                        }
                    }
                }
            }
            return candidate ?? members[0];
        }

        private static MethodInfo findMethodInfo(PlTerm memberSpec, int arity, Type c, ref Type[] paramz,
                                                 BindingFlags searchFlags)
        {
            var mi = findMethodInfo0(memberSpec, arity, c, ref paramz, searchFlags);
            if (mi == null)
            {
                return null;
            }
            if (!mi.IsGenericMethodDefinition) return mi;
            var typeparams = mi.GetGenericArguments() ?? ZERO_TYPES;
            if (typeparams.Length == 0) return mi;
            if (memberSpec.IsAtomOrString)
            {
                {
                    var ps = mi.GetParameters();
                    bool missingTypePAramInArgs = false;
                    foreach (Type type in typeparams)
                    {
                        Type type1 = type;
                        var pi = Array.Find(ps, p => p.ParameterType == type1);
                        if (pi != null)
                        {
                            continue;
                        }
                        Embedded.Warn("Trying to find a generic methods without type specifiers {0}",
                             ToString(typeparams));
                    }
                }
                return mi;
            }
            else
            {
                if (memberSpec.IsCompound)
                {
                    Type[] t = GetParamSpec(ToTermArray(memberSpec), false, 0, typeparams.Length);
                    if (t.Length == typeparams.Length)
                    {
                        mi = mi.MakeGenericMethod(t);
                    }
                }
                return mi;
            }
        }

        private static MethodInfo findMethodInfo0(PlTerm memberSpec, int arity, Type c, ref Type[] paramz,
                                                  BindingFlags searchFlags)
        {
            if (c == null)
            {
                Embedded.Warn("findMethod no class for {0}", memberSpec);
                return null;
            }
            if (memberSpec.IsVar)
            {
                Embedded.Warn("findMethod IsVar {0} on type {1}", memberSpec, c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                var mis = c.GetMethods(BindingFlagsALL);
                return mis[memberSpec.intValue()];
            }
            if (IsTaggedObject(memberSpec))
            {
                object o = tag_to_object(memberSpec[1].Name);
                var r = o as MethodInfo;
                if (r != null) return r;
                var d = o as Delegate;
                if (d != null) return d.Method ?? d.GetType().GetMethod("Invoke");
            }
            string fn = memberSpec.Name;
            MethodInfo mi = null;
            BindingFlags icbf = searchFlags;
            if (arity < 1)
            {
                mi = GetMethod(c, fn, icbf);
                if (mi != null) return mi;
            }
            if (paramz == null)
            {
                Embedded.Warn("using paramSpec {0}", ToString(memberSpec));
                paramz = GetParamSpec(memberSpec) ?? ZERO_TYPES;
            }
            try
            {
                bool hasNull = false;
                foreach (var s in paramz)
                {
                    if (s == null)
                    {
                        hasNull = true;
                        break;
                    }
                }
                if (!hasNull)
                {
                    mi = c.GetMethod(fn, icbf, null, CallingConventions.Any, paramz, null);
                }
                if (mi != null)
                {
                    return mi;
                }
            }
            catch ( /*AmbiguousMatch*/ Exception e)
            {
                Embedded.Debug("AME: {0} fn = {1}", e, fn);
            }
            MethodInfo[] members = c.GetMethods(searchFlags);
            if (arity < 0) arity = (paramz != null) ? paramz.Length : memberSpec.Arity;

            string fnLower = fn.ToLower();
            var candidates = new MethodInfo[2];
            foreach (var infos in members)
            {
                var methodParams = infos.GetParameters();
                if (methodParams.Length == arity)
                {
                    if (infos.Name == fn)
                    {
                        candidates[0] = infos;
                    }
                    else if (infos.Name.ToLower() == fnLower)
                    {
                        candidates[1] = infos;
                    }
                    else
                    {
                        continue;
                    }
                    if (ParamsMatch(paramz, methodParams)) return infos;
                }
            }
            return candidates[0] ?? candidates[1];
        }


        private static ConstructorInfo findConstructorInfo(PlTerm memberSpec, Type c, ref Type[] paramz)
        {
            if (c == null)
            {
                Embedded.Warn("findConstructor no class for {0}", memberSpec);
                return null;
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as ConstructorInfo;
                if (r != null) return r;
            }
            if (memberSpec.IsInteger)
            {
                var mis = c.GetConstructors(BindingFlagsALL);
                return mis[memberSpec.intValue()];
            }
            if (paramz == null)
            {
                Embedded.Warn("using paramSpec {0}", ToString(memberSpec));
                paramz = GetParamSpec(memberSpec) ?? ZERO_TYPES;
            }
            if (paramz != null)
            {
                var mi = c.GetConstructor(paramz);
                if (mi != null) return mi;
            }
            ConstructorInfo[] members = c.GetConstructors(BindingFlagsALL);
            if (members.Length == 0) return null;
            int arity = (paramz != null) ? paramz.Length : memberSpec.Arity;
            ConstructorInfo candidate = null;
            foreach (var info in members)
            {
                var infos = info;
                ParameterInfo[] paramTypes = infos.GetParameters();
                if (paramTypes.Length == arity)
                {
                    if (ParamsMatch(paramz, paramTypes)) return info;
                    if (infos.IsStatic)
                    {
                        if (candidate == null)
                        {
                            candidate = info;
                        }
                    }
                    else
                    {
                        if (candidate == null)
                        {
                            candidate = info;
                        }
                    }
                }
            }
            return candidate ?? members[0];
        }

        private static ParameterInfo[] GetParmeters(EventInfo ei)
        {
            ParameterInfo[] parme = null;
            var rm = ei.GetRaiseMethod();
            var erm = ei.EventHandlerType;
            if (rm == null && erm != null)
            {
                rm = erm.GetMethod("Invoke");
            }
            if (rm != null)
            {
                parme = rm.GetParameters();
            }
            return parme;
        }


        [PrologVisible]
        public static bool cliFindConstructor(PlTerm clazzSpec, PlTerm memberSpec, PlTerm methodOut)
        {
            Type c = GetType(clazzSpec);
            Type[] paramz = null;
            MethodBase mi = findConstructorInfo(memberSpec, c, ref paramz);
            if (mi != null)
            {
                return methodOut.FromObject((mi));
            }
            return false;
        }

        /// <summary>
        /// ?- cliNew('java.lang.Long',[long],[44],Out),cliToString(Out,Str).
        /// </summary>
        /// <param name="memberSpec"></param>
        /// <param name="paramIn"></param>
        /// <param name="valueOut"></param>
        /// <returns></returns>
        [PrologVisible]
        public static bool cliNew(PlTerm clazzSpec, PlTerm memberSpec, PlTerm paramsIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliNew(clazzSpec, memberSpec, paramsIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            Type c = GetType(clazzSpec);
            if (c == null)
            {
                Embedded.Error("Cant resolve clazzSpec {0}", clazzSpec);
                {

                    return false;
                }
            }
            Action postCallHook;
            object res;
            
            if (!TryConstructObject(c, memberSpec, paramsIn, out postCallHook, out res))
            {
                Embedded.Error("Cant TryConstructObject {0}", clazzSpec);
                return false;
            }
            var ret = valueOut.FromObject(res);
            if (ret)
            {
                CommitPostCall(postCallHook);
                return true;
            }
            BP();
            return ret;
        }

        private static bool TryConstructObject(Type c, PlTerm memberSpec, PlTerm paramsIn, //PlTerm valueOut,
            out Action postCallHook, out object res)
        {
            // res = null;
            postCallHook = null;

            Type[] paramz = GetParamSpec(memberSpec) ?? ZERO_TYPES;
            PlTerm[] paramIn = ToTermArray(paramsIn);
            if (paramz.Length == 0 && paramIn.Length > paramz.Length)
            {
                paramz = GetParamSpecFromObjects(paramIn);
            }
            MethodBase mi = null;
            if (!c.IsAbstract) mi = findConstructorInfo(memberSpec, c, ref paramz);
            object target = null;
            if (mi == null)
            {
                int arity = paramz.Length;
                if (arity == 1)
                {
                    mi = c.GetMethod("op_Implicit", (BindingFlags.Public | BindingFlags.Static), null, paramz,
                        new ParameterModifier[0]);
                    if (mi == null)
                    {
                        mi = c.GetMethod("op_Explicit", (BindingFlags.Public | BindingFlags.Static), null, paramz,
                            new ParameterModifier[0]);
                    }
                    if (mi == null)
                    {
                        if (c.IsPrimitive)
                        {
                            //Warn("Trying to constuct a primitive type");
                            {
                                res = Convert.ChangeType(GetInstance(paramIn[0]), c);
                                if (res != null) return true;
                            }
                        }
                    }
                }
                if (mi == null)
                {
                    MethodInfo[] members = c.GetMethods(BindingFlagsJustStatic);
                    mi = BestMethod(paramz, members, c, true);
                }
            }
            if (mi == null)
            {
                Embedded.Error("Cant find constructor {0} on {1}", memberSpec, c);

                res = null;
                return false;

            }


            object[] values = PlListToCastedArray(paramIn, mi.GetParameters(), out postCallHook);
            // mono doesnt mind..
            //  typeof(System.Text.StringBuilder).GetConstructor(new[]{typeof(System.String)}).Invoke(null,new object[]{"hi there"}).ToString();
            // .NET doesnt
            if (mi is ConstructorInfo)
            {
                res = ((ConstructorInfo) mi).Invoke(values);
            }
            else
            {
                res = mi.Invoke(null, values);
            }
            return res!=null;
        }

        private static MethodBase BestMethod(Type[] paramz, IEnumerable<MethodInfo> members, Type returnType, bool mustStatic)
        {
            var maybes = new MethodBase[2];
            foreach (var infos in members)
            {
                if (mustStatic && !infos.IsStatic) continue;
                ParameterInfo[] testParams = infos.GetParameters();
                if (testParams.Length == paramz.Length)
                {
                    if (returnType.IsAssignableFrom(infos.ReturnType))
                    {
                        if (ParamsMatch(paramz, testParams))
                        {
                            return infos;
                        }
                        if (maybes[0] == null) maybes[0] = infos;
                    }
                    else
                    {
                        if (infos.ReturnType.IsAssignableFrom(returnType))
                        {
                            if (ParamsMatch(paramz, testParams))
                            {
                                return infos;
                            }
                            if (maybes[1] == null) maybes[1] = infos;
                        }
                        else
                        {
                            if (ParamsMatch(paramz, testParams))
                            {
                                if (maybes[1] == null) maybes[1] = infos;
                            }
                        }
                    }
                }
            }
            return maybes[0] ?? maybes[1];
        }

        private static bool ParamsMatch(Type[] paramz, ParameterInfo[] paramInfos)
        {
            int i = 0;
            foreach (ParameterInfo info in paramInfos)
            {
                if (paramz[i] == null) continue;
                if (!info.ParameterType.IsAssignableFrom(paramz[i])) return false;
                i++;
            }
            return true;
        }

        [PrologVisible]
        public static bool cliFindMethod(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm methodOut)
        {
            BindingFlags searchFlags = BindingFlagsALL;
            if (!methodOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliFindMethod(clazzOrInstance, memberSpec, plvar) && SpecialUnify(methodOut, plvar);
            }
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            if (!CheckBound(memberSpec)) return false;
            Type[] paramz = null;
            var mi = findMethodInfo(memberSpec, -1, c, ref paramz, searchFlags);
            if (mi != null)
            {
                return methodOut.FromObject((mi));
            }
            return false;
        }

        [PrologVisible]
        public static bool cliCallRaw(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm paramsIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                Type returnTypeHint = GuessReturnType(valueOut, typeof(void));
                return cliCallRawForVar(clazzOrInstance, memberSpec, paramsIn, plvar, returnTypeHint) && SpecialUnify(valueOut, plvar);
            }
            return cliCallRawForVar(clazzOrInstance, memberSpec, paramsIn, valueOut, typeof(void));
        }
        public static bool cliCallRawForVar(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm paramsIn, PlTerm valueOut, Type returnTypeHint)
        {
            BindingFlags searchFlags = BindingFlagsALL;
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            if (!CheckBound(memberSpec, paramsIn)) return false;
            Type[] paramz = GetParamSpec(memberSpec) ?? ZERO_TYPES;
            if (paramz.Length == 0)
            {
                if (c != null)
                {
                    string mspecName = GetMemberName(memberSpec);
                    var candidates = c.GetMember(mspecName);
                    if (candidates.Length == 1 && candidates[0] is MethodInfo)
                    {

                        MethodInfo mi0 = (MethodInfo) candidates[0];
                        Action postCallHook0;
                        object[] value0 = PlListToCastedArray(ToTermArray(paramsIn), mi0.GetParameters(), out postCallHook0);
                        object target0 = mi0.IsStatic ? null : getInstance;
                        object retval0 = InvokeCaught0(mi0, target0, value0, postCallHook0);
                        return valueOut.FromObject(retval0 ?? VoidOrNull(mi0));
                    }
                }
            }
            PlTerm[] paramIn = ToTermArray(paramsIn);
            if (paramz.Length == 0 && paramIn.Length > paramz.Length)
            {
                paramz = GetParamSpecFromObjects(paramIn);
            }
            var mi = findMethodInfo(memberSpec, paramz.Length, c, ref paramz, searchFlags);
            if (mi == null)
            {
                if (getInstance is PInvokeMetaObject)
                {
                    PInvokeMetaObject pi = getInstance as PInvokeMetaObject;
                    string mspecName = GetMemberName(memberSpec);
                    mi = pi.GetInvoke(mspecName, paramz, returnTypeHint);
                }

            }
            if (mi == null)
            {
                var ei = findEventInfo(memberSpec, c, ref paramz, searchFlags);
                if (ei != null) return RaiseEvent(getInstance, memberSpec, paramIn, valueOut, ei, c);
                if (paramsIn.IsNil) return cliGetRaw(clazzOrInstance, memberSpec, valueOut);
                Embedded.Warn("Cant find method {0} on {1}", memberSpec, c);
                return false;
            }
            Action postCallHook;
            object[] value = PlListToCastedArray(paramIn, mi.GetParameters(), out postCallHook);
            object target = mi.IsStatic ? null : getInstance;
            object retval = InvokeCaught0(mi, target, value, postCallHook);
            return valueOut.FromObject(retval ?? VoidOrNull(mi));
        }

        private static Type GuessReturnType(PlTerm valueOut, Type type)
        {
            if (valueOut.IsVar) return type;
            Type guess = GetType(valueOut, true);
            if (guess != null) return guess;
            return type;
        }

        
        private static string GetMemberName(PlTerm memberSpec)
        {
            if (memberSpec.IsVar)
            {
                Embedded.Error("GetMemberName IsVar {0} ", memberSpec);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                Embedded.Error("GetMemberName IsInteger {0} ", memberSpec);
                return null;
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as string;
                if (r != null) return r;
            }
            if (memberSpec.IsCompound)
            {
                return memberSpec.Name;
            }
            string fn = memberSpec.Name;
            if (fn == "[]") fn = "Get";
            return fn;        
        }

        [PrologVisible]
        public static bool cliRaiseEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm paramsIn,
                                                PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliRaiseEventHandler(clazzOrInstance, memberSpec, paramsIn, plvar) &&
                       SpecialUnify(valueOut, plvar);
            }
            object getInstance;
            Type c;
            if (!GetInstanceAndType(clazzOrInstance, out getInstance, out c)) return false;
            Type[] paramz = GetParamSpec(memberSpec) ?? ZERO_TYPES;
            if (!CheckBound(memberSpec, paramsIn)) return false;
            PlTerm[] paramIn = ToTermArray(paramsIn);
            if (paramz.Length == 0 && paramIn.Length > paramz.Length)
            {
                paramz = GetParamSpecFromObjects(paramIn);
            }
            EventInfo evi = findEventInfo(memberSpec, c, ref paramz, BindingFlagsALL);
            return RaiseEvent(getInstance, memberSpec, paramIn, valueOut, evi, c);
        }

        public static bool RaiseEvent(object getInstance, PlTerm memberSpec, PlTerm[] paramIn, PlTerm valueOut,
                                      EventInfo evi, Type c)
        {
            if (evi == null)
            {
                return Embedded.Warn("Cant find event {0} on {1}", memberSpec, c);
            }
            ParameterInfo[] paramInfos = GetParmeters(evi);
            MethodInfo mi = evi.GetRaiseMethod();
            string fn = evi.Name;
            if (mi == null)
            {
                FieldInfo fi = c.GetField(fn, BindingFlagsALLNC);
                if (fi != null)
                {
                    Delegate del = (Delegate) fi.GetValue(getInstance);
                    if (del != null)
                    {
                        Action postCallHook;
                        var ret = valueOut.FromObject((del.DynamicInvoke(
                                                          PlListToCastedArray(paramIn, paramInfos,
                                                                              out postCallHook))));
                        CommitPostCall(postCallHook);
                        return ret;
                    }
                }
                string fn1 = fn.Substring(1);
                int len = fn.Length;
                foreach (FieldInfo info in c.GetFields(BindingFlagsALL))
                {
                    if (info.Name.EndsWith(fn1))
                    {
                        if (info.Name.Length - len < 3)
                        {
                            Delegate del = (Delegate) info.GetValue(info.IsStatic ? null : getInstance);
                            if (del != null)
                            {
                                Action postCallHook;
                                var ret = valueOut.FromObject((del.DynamicInvoke(
                                                                  PlListToCastedArray(paramIn, paramInfos,
                                                                                      out postCallHook))));
                                CommitPostCall(postCallHook);
                                return ret;
                            }
                        }
                    }
                }
            }
            if (mi == null)
            {
                Type eviEventHandlerType = evi.EventHandlerType;
                if (eviEventHandlerType != null) mi = eviEventHandlerType.GetMethod("Invoke");
            }
            if (mi == null)
            {
                Embedded.Warn("Cant find event raising for  {0} on {1}", evi, c);
                return false;
            }
            Action postCallHook0;
            object[] value = PlListToCastedArray(paramIn, mi.GetParameters(), out postCallHook0);
            object target = mi.IsStatic ? null : getInstance;
            return valueOut.FromObject(InvokeCaught(mi, target, value, postCallHook0) ?? VoidOrNull(mi));
        }

        private static object VoidOrNull(MethodInfo info)
        {
            return info.ReturnType == typeof (void) ? (object) PLVOID : PLNULL;
        }

        private static object[] PlListToCastedArray(IEnumerable<PlTerm> term, ParameterInfo[] paramInfos,
                                                    out Action todo)
        {
            return PlListToCastedArray(0, term, paramInfos, out todo);
        }

        private static object[] PlListToCastedArray(int skip, IEnumerable<PlTerm> term, ParameterInfo[] paramInfos,
                                                    out Action todo)
        {
            todo = Do_NOTHING;
            int len = paramInfos.Length;
            if (len == 0) return ZERO_OBJECTS;
            MethodInfo methodInfo = paramInfos[0].Member as MethodInfo;
            bool isVarArg = methodInfo != null && (methodInfo.CallingConvention & CallingConventions.VarArgs) != 0;
            object[] ret = new object[len];
            PlTerm[] ta = ToTermArray(term);
            int termLen = ta.Length;
            int lenNeeded = len;
            int termN = skip;
            for (int idx = 0; idx < len; idx++)
            {
                ParameterInfo paramInfo = paramInfos[idx];
                PlTerm arg = ta[termN];
                Type type = GetParameterType(paramInfo);
                bool isByRef = IsByRef(paramInfo);
                bool lastArg = (idx + 1 == len);
                if (lastArg && isVarArg)
                {
                    if (arg.IsList)
                    {
                        ret[idx] = CastTerm(arg, type);
                        termN++;
                        continue;
                    }
                    Type arrayElementType = type.GetElementType();
                    if (termN < termLen)
                    {
                        int slack = termLen - termN;
                        var sa = (object[]) Array.CreateInstance(arrayElementType, slack);
                        ret[idx] = sa;
                        for (int i = 0; i < slack; i++)
                        {
                            sa[i] = CastTerm(ta[termN++], arrayElementType);
                        }
                        continue;
                    }
                    ret[idx] = Array.CreateInstance(arrayElementType, 0);
                }
                if (IsOptionalParam(paramInfo))
                {
                    if (termLen < lenNeeded)
                    {
                        lenNeeded--;
                        object paramInfoDefaultValue = paramInfo.DefaultValue;
                        if (type.IsInstanceOfType(paramInfoDefaultValue))
                        {
                            ret[idx] = paramInfoDefaultValue;
                        }
                        else
                        {
                            //paramInfo.ParameterType.IsValueType
                            //default()
                        }
                        //termN stays the same!
                        continue;
                    }
                }
                bool wasOut = paramInfo.IsOut || paramInfo.IsRetval;
                if (wasOut) isByRef = false;
                if (isByRef && !wasOut)
                {
                    var ooo = CastTerm(arg, type);
                    if (ooo == null)
                    {
                        Embedded.Debug("idx {0} ({1}) for {2} is null", idx, type, arg);
                    }
                    ret[idx] = ooo;
                    wasOut = true;
                }
                if (wasOut)
                {
                    var sofar = todo;
                    int index0 = idx;
                    int termM = termN;
                    PlTerm plTerm = arg;
                    todo = () =>
                               {
                                   object ret1 = ret[index0];
                                   int ii = termM;
                                   UnifySpecialObject(plTerm, ret1);
                                   sofar();
                               };
                    if (isByRef)
                    {
                        termN++;
                        continue;
                    }
                }
                if (paramInfo.IsIn)
                {
                    var ooo1 = CastTerm(arg, type);
                    if (ooo1 == null)
                    {
                        Embedded.Debug("idx {0} ({1}) for {2} is null", idx, type, arg);
                    }
                    ret[idx] = ooo1;
                }
                else
                {
                    if (!wasOut)
                    {
                        var ooo1 = CastTerm(arg, type);
                        if (ooo1 == null)
                        {
                            Embedded.Debug("idx {0} ({1}) for {2} is null", idx, type, arg);
                        }
                        ret[idx] = ooo1;
                    }
                    else
                    {
                        ret[idx] = null; // CastTerm(arg, paramInfo.ParameterType);                        
                    }
                }
                termN++;
            }
            return ret;
        }

        public static Type GetParameterType(ParameterInfo paramInfo)
        {
            Type paramType = paramInfo.ParameterType;
            return paramType.IsByRef ? paramType.GetElementType() : paramType;
        }

        public static bool IsByRef(ParameterInfo paramInfo)
        {
            Type paramType = paramInfo.ParameterType;
            return paramType.IsByRef;
        }

        private static bool IsOptionalParam(ParameterInfo info)
        {
            if ((info.Attributes & ParameterAttributes.Optional) != 0)
            {
                return true;
            }
            if ((info.Attributes & ParameterAttributes.HasDefault) != 0)
            {
                return true;
            }
            return info.IsOptional || (info.Name != null && info.Name.ToLower().StartsWith("optional"));
        }

        private static void Do_NOTHING()
        {
        }
    }
}