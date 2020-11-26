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
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
using PlTerm = SbsSW.SwiPlCs.PlTerm;
//using Func = Swicli.Library.Func;
namespace Swicli.Library
{
    /*
         
 5.6.1.1 Non-deterministic Foreign Predicates

By default foreign predicates are deterministic. Using the PL_FA_NONDETERMINISTIC attribute (see PL_register_foreign()) it is possible to register a predicate as a non-deterministic predicate. Writing non-deterministic foreign predicates is slightly more complicated as the foreign function needs context information for generating the next solution. Note that the same foreign function should be prepared to be simultaneously active in more than one goal. Suppose the natural_number_below_n/2 is a non-deterministic foreign predicate, backtracking over all natural numbers lower than the first argument. Now consider the following predicate:

quotient_below_n(Q, N) :- natural_number_below_n(N, N1), natural_number_below_n(N, N2), Q =:= N1 / N2, !.

In this predicate the function natural_number_below_n/2 simultaneously generates solutions for both its invocations.

Non-deterministic foreign functions should be prepared to handle three different calls from Prolog:

* Initial call (PL_FIRST_CALL)
Prolog has just created a frame for the foreign function and asks it to produce the first answer.
* Redo call (PL_REDO)
The previous invocation of the foreign function associated with the current goal indicated it was possible to backtrack. The foreign function should produce the next solution.
* Terminate call (PL_CUTTED)
The choice point left by the foreign function has been destroyed by a cut. The foreign function is given the opportunity to clean the environment. 

Both the context information and the type of call is provided by an argument of type control_t appended to the argument list for deterministic foreign functions. The macro PL_foreign_control() extracts the type of call from the control argument. The foreign function can pass a context handle using the PL_retry*() macros and extract the handle from the extra argument using the PL_foreign_context*() macro.

void PL_retry(long)
The foreign function succeeds while leaving a choice point. On backtracking over this goal the foreign function will be called again, but the control argument now indicates it is a `Redo' call and the macro PL_foreign_context() will return the handle passed via PL_retry(). This handle is a 30 bits signed value (two bits are used for status indication).

void PL_retry_address(void *)
As PL_retry(), but ensures an address as returned by malloc() is correctly recovered by PL_foreign_context_address().

int PL_foreign_control(control_t)
Extracts the type of call from the control argument. The return values are described above. Note that the function should be prepared to handle the PL_CUTTED case and should be aware that the other arguments are not valid in this case.

long PL_foreign_context(control_t)
Extracts the context from the context argument. In the call type is PL_FIRST_CALL the context value is 0L. Otherwise it is the value returned by the last PL_retry() associated with this goal (both if the call type is PL_REDO as PL_CUTTED).

void * PL_foreign_context_address(control_t)
Extracts an address as passed in by PL_retry_address(). 

Note: If a non-deterministic foreign function returns using PL_succeed or PL_fail, Prolog assumes the foreign function has cleaned its environment. No call with control argument PL_CUTTED will follow.

The code of figure 6 shows a skeleton for a non-deterministic foreign predicate definition.

typedef struct // define a context structure  { ... } context; 
 foreign_t my_function(term_t a0, term_t a1, foreign_t handle) { struct context * ctxt; switch( PL_foreign_control(handle) ) { case PL_FIRST_CALL: ctxt = malloc(sizeof(struct context)); ... PL_retry_address(ctxt); case PL_REDO: ctxt = PL_foreign_context_address(handle); ... PL_retry_address(ctxt); case PL_CUTTED: free(ctxt); PL_succeed; } } 
         
 */
    public delegate object AnyMethod(params object[] any);
    public class IKVMBased : Attribute
    {

    }

    public class PrologInvisible : Attribute
    {
    }

    public class PrologVisible : Attribute
    {
        public string ModuleName;
        public string Name;
        public int Arity = -1;
        public Type TypeOf;
        public PlForeignSwitches ForeignSwitches = PlForeignSwitches.None;
        public Delegate Delegate
        {
            get
            {
                if (_delegate == null) _delegate = Delegate.CreateDelegate(DelegateType, Method);
                return _delegate;
            }
            set { this._delegate = value; }
        }

        public MethodInfo Method;
        public Type DelegateType;
        private Delegate _delegate;

        public PrologVisible()
        {

        }
    }
    public class NonDet : PrologVisible
    {
        public NonDet()
        {
            ForeignSwitches = PlForeignSwitches.Nondeterministic;
        }
    }
    public class PrologTest : Attribute
    {
        public string ModuleName;
        public string Name;
        public PrologTest()
        {

        }
    }
    public partial class PrologCLR
    {

        private static void CheckRequiredPrefix(MethodInfo m, PrologVisible f1, string requiredPrefix)
        {
            if (f1.Name != null || requiredPrefix == null) return;
            string proposal = ComputeName(f1, m);
            if (!proposal.StartsWith(requiredPrefix))
            {
                proposal = requiredPrefix + proposal;
            }
            f1.Name = proposal;
        }

        public static List<MethodInfo> ExportedMethodInfos = new List<MethodInfo>();
        [PrologVisible]
        private static void AddForeignMethods(Type t, bool onlyAttributed, string requiredPrefix)
        {
            lock (TypesMethodsLoaded)
            {
                if (TypesLoading.Contains(t)) return;
                if (TypesMethodsLoaded.Contains(t))
                {
                    return;
                }
                TypesLoading.Add(t);
                AddForeignMethods0(t, onlyAttributed, requiredPrefix);
                TypesLoading.Remove(t);                
            }
        }
        private static void AddForeignMethods0(Type t, bool onlyAttributed, string requiredPrefix)
        {
            if (!onlyAttributed)
            {
                TypesMethodsLoaded.Add(t);
            }
            MethodInfo[] methods = t.GetMethods(BindingFlagsJustStatic);
            foreach (var m in methods)
            {
                if (hadAttribute(m, typeof(PrologInvisible), false)) continue;
                var f = hadAttribute(m, typeof(PrologVisible), false);
                if (onlyAttributed) if (!f) continue;
                InternMethod(m, requiredPrefix);
            }
            foreach (var m in methods)
            {                
               if (!hadAttribute(m, typeof (TypeConversionAttribute), false)) continue;
                registerConversion(m, null, null);
            }
            if (onlyAttributed) return;
            foreach (var m in methods)
            {
                if (ExportedMethodInfos.Contains(m)) continue;
                if (m.IsAbstract) continue;
                var pm = m.GetParameters();
                if (pm.Length == 0) continue;
                bool skip = false;
                int argNum = 0;
                if (pm[0].ParameterType != typeof(PlTerm)) continue;
 
                if (pm.Length == 3 && pm[1].ParameterType == typeof(int) && pm[2].ParameterType == typeof(IntPtr) && m.ReturnType == typeof(int))
                {
                    InternMethod(m, requiredPrefix);
                    continue;
                }
                var ForeignSwitches = PlForeignSwitches.None;

                if (m.ReturnType != typeof(bool)) continue;

                for (int i = 1; i < pm.Length; i++)
                {
                    ParameterInfo info = pm[i];
                    if (info.ParameterType != typeof(PlTerm))
                    {
                        skip = true;
                        break;
                    }
                }
                if (!skip)
                {
                    string mname = m.Name;
                    if (mname.StartsWith("op_")) continue;
                    InternMethod(m, requiredPrefix);
                    continue;
                }
            }
        }
        private static void InternMethod(MethodInfo m, string requiredPrefix)
        {
            if (ExportedMethodInfos.Contains(m)) return;
            if (m.IsAbstract) return;
            object[] f = m.GetCustomAttributes(typeof(PrologVisible), false);
            if (f != null && f.Length > 0)
            {
                PrologVisible f1 = (PrologVisible)f[0];
                f1.Method = m;
                try
                {
                    CheckRequiredPrefix(m, f1, requiredPrefix);
                    InternMethod(m, f1);
                    ExportedMethodInfos.Add(m);
                }
                catch (Exception e)
                {
                    Embedded.Error("{0} caused {1}", m, e);
                }
                return;
            }
            var ForeignSwitches = PlForeignSwitches.None;
            var pm = m.GetParameters();
            if (pm.Length == 3)
            {
                if (pm[1].ParameterType == typeof(int) && pm[2].ParameterType == typeof(IntPtr) &&
                    m.ReturnType == typeof(int))
                {
                    ForeignSwitches |= PlForeignSwitches.VarArgs | PlForeignSwitches.Nondeterministic;
                    PrologVisible f1 = new NonDet();
                    f1.Method = m;
                    try
                    {
                        CheckRequiredPrefix(m, f1, requiredPrefix);
                        InternMethod(m, f1);
                        ExportedMethodInfos.Add(m);
                    }
                    catch (Exception e)
                    {
                        Embedded.Error("{0} caused {1}", m, e);
                    }
                    return;
                }
            }
            else
            {
                PrologVisible f1 = (PrologVisible)new PrologVisible();
                f1.ForeignSwitches = ForeignSwitches;
                f1.Method = m;
                try
                {
                    CheckRequiredPrefix(m, f1, requiredPrefix);
                    InternMethod(m, f1);
                    ExportedMethodInfos.Add(m);
                }
                catch (Exception e)
                {
                    Embedded.Error("{0} caused {1}", m, e);
                }
            }
        }

        private static string ComputeName(PrologVisible pm, MethodInfo m)
        {
            if (pm.Name == null)
            {
                if (Char.IsLower(m.Name[0]))
                {
                    string mName = m.Name;
                    if (ForceJanCase)
                    {
                        return ToPrologCase(mName);
                    }
                    else
                    {
                        return mName;
                    }
                }
                else
                {
                    string mName = m.Name;
                    return ToPrologCase(mName);
                }
            }
            else
            {
                if (ForceJanCase) return ToPrologCase(pm.Name);
            }
            return pm.Name;
        }
        public static void InternMethod(MethodInfo m, PrologVisible pm)
        {
            pm.Name = ComputeName(pm, m);
            if (pm.DelegateType != null)
            {
                ExportedMethodInfos.Add(m);
                PlEngine.RegisterForeign(pm.ModuleName, pm.Name, pm.Arity, pm.Delegate, pm.ForeignSwitches);
                return;
            }
            InternMethod(pm.ModuleName, pm.Name, m);
        }
        public static void InternMethod(string module, string pn, AnyMethod d)
        {
            if (!PlEngine.SaveRegisterForeign(module, pn.ToString(), -1, d)) return;
            InternMethod(module, pn, d.Method);
        }
        public static void InternMethod(string module, string pn, MethodInfo info)
        {
            InternMethod(module, pn, info, null);
        }
        public static bool ForceJanCase = true;
        public static void InternMethod(string module, string pn, MethodInfo minfo, object defaultInstanceWhenMissing)
        {
            if (minfo == null)
            {
                return;
            }
            if (!minfo.IsStatic && defaultInstanceWhenMissing == null)
            {
                throw new NotSupportedException(String.Format(
                                                    "Interning a dynamic method without a target {0}:{1} -> {2}", module,
                                                    pn, minfo));
            }
            ExportedMethodInfos.Add(minfo);
            Type type = minfo.DeclaringType;
            pn = pn ?? (type.Name + "." + minfo.Name);
            if (ForceJanCase)
            {
                var pn2 = ToPrologCase(pn);
                if (pn2 != pn)
                {
                    pn = pn2;
                }
            }
            ParameterInfo[] ps = minfo.GetParameters();
            Type rt = minfo.ReturnType;
            int paramlen = ps.Length;
            bool nonvoid = rt != typeof(void);
            bool isbool = rt == typeof(bool);
            bool hasReturnValue = nonvoid && !isbool;
            bool isStatic = minfo.IsStatic;
            bool isVanilla = true;
            int maxOptionals = 0;
            foreach (ParameterInfo info in ps)
            {
                if (info.ParameterType != typeof(PlTerm))
                {
                    isVanilla = false;
                }
                if (IsOptionalParam(info))
                {
                    isVanilla = false;
                    maxOptionals++;
                }
            }
            if (isbool && isStatic)
            {
                if (isVanilla)
                {
                    RegisterInfo(pn, paramlen, minfo);
                    Delegate d = null;
                    switch (paramlen)
                    {
                        case 0:
                            {
                                d = new DelegateParameter0(() => (bool)InvokeCaughtB(minfo, null, ZERO_OBJECTS));
                                PlEngine.RegisterForeign(module, pn, paramlen, d, PlForeignSwitches.None);
                                return;
                            }
                        case 1:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter1(
                                                         (p1) => (bool)InvokeCaughtB(minfo, null, new object[] { p1 })),
                                                     PlForeignSwitches.None);
                            return;
                        case 2:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter2(
                                                         (p1, p2) =>
                                                         (bool)InvokeCaughtB(minfo, null, new object[] { p1, p2 })),
                                                     PlForeignSwitches.None);
                            return;
                        case 3:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter3(
                                                         (p1, p2, p3) =>
                                                         (bool)InvokeCaughtB(minfo, null, new object[] { p1, p2, p3 })),
                                                     PlForeignSwitches.None);
                            return;
                        case 4:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter4(
                                                         (p1, p2, p3, p4) =>
                                                         (bool)InvokeCaughtB(minfo, null, new object[] { p1, p2, p3, p4 })),
                                                     PlForeignSwitches.None);
                            return;
                        case -5: // use the default please
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter5(
                                                         (p1, p2, p3, p4, p5) =>
                                                         (bool)InvokeCaughtB(minfo, null, new object[] { p1, p2, p3, p4, p5 })),
                                                     PlForeignSwitches.None);
                            return;
                        default:
                            break;
                    }
                }
            }
            int plarity = paramlen + (hasReturnValue ? 1 : 0) + (isStatic ? 0 : 1);

            RegisterInfo(pn, plarity, minfo);
            DelegateParameterVarArgs del = GetDelV(minfo, type, nonvoid, isbool, isStatic, plarity, defaultInstanceWhenMissing);
            PlEngine.RegisterForeign(module, pn, plarity, del, PlForeignSwitches.VarArgs);
            while (maxOptionals > 0)
            {
                RegisterInfo(pn, plarity - maxOptionals, minfo); 
                del = GetDelV(minfo, type, nonvoid, isbool, isStatic, plarity - maxOptionals, defaultInstanceWhenMissing);
                PlEngine.RegisterForeign(module, pn, plarity - maxOptionals, del, PlForeignSwitches.VarArgs);
                maxOptionals--;
            }
        }

        public static Dictionary<string, MethodInfo> AutoDocInfos = new Dictionary<string, MethodInfo>();
        public static void RegisterInfo(string pn, int paramlen, MethodInfo info)
        {
            string key = pn + "/" + paramlen;
            MethodInfo minfo;
            lock (AutoDocInfos)
            {
                if (!AutoDocInfos.TryGetValue(key, out minfo))
                {
                    AutoDocInfos[key] = info;
                }
            }
        }

        public static string ToPrologCase(string pn)
        {
            bool cameCased = false;
            foreach (char c in pn)
            {
                if (Char.IsUpper(c) || c == '.' || c == '-')
                {
                    cameCased = true;
                    break;
                }
            }
            if (!cameCased) return pn;
            StringBuilder newname = new StringBuilder();
            bool lastCapped = true;
            bool lastUnderscored = true;
            foreach (char c in pn)
            {

                if (Char.IsUpper(c))
                {
                    if (lastCapped)
                    {
                        newname.Append(CharToLower(c));
                    }
                    else
                    {
                        if (!lastUnderscored) newname.Append('_');
                        newname.Append(CharToLower(c));
                        lastCapped = true;
                    }
                    lastUnderscored = false;
                }
                else
                {
                    if (c == '_' || c == '-')
                    {
                        lastCapped = false;
                        if (lastUnderscored) continue;
                        newname.Append('_');
                        lastUnderscored = true;
                        continue;
                    }
                    newname.Append(CharToLower(c));
                    lastCapped = false;
                    lastUnderscored = false;
                }
            }
            return newname.ToString();
        }

        private static char CharToLower(char c)
        {
            if (c == '-') return '_';
            return Char.ToLower(c);

        }

        private static DelegateParameterVarArgs GetDelV(MethodInfo list, Type type, bool nonvoid, bool isbool, bool isStatic, int plarity, object defaultInstanceWhenMissing)
        {
            DelegateParameterVarArgs d;
            d = (PlTermV termVector) =>
            {
                if (termVector.Size != plarity)
                {
                    //return false;
                    termVector.Resize(plarity);
                }
                object target = isStatic ? null : CastTerm(termVector[0], type) ?? defaultInstanceWhenMissing;
                Action postCallHook;
                int tvargnum = isStatic ? 0 : 1;
                object[] newVariable = PlListToCastedArray(tvargnum, termVector, list.GetParameters(),
                                                           out postCallHook);
                object result = InvokeCaught0(list, target, newVariable, postCallHook);

                if (isbool)
                {
                    return (bool)result;
                }
                if (nonvoid)
                {
                    return termVector[plarity - 1].FromObject(result);
                }
                return true;

            };
            return d;
        }

        private static object InvokeCaught(MethodInfo info, object o, object[] os)
        {
            return InvokeCaught0(info, o, os, Do_NOTHING);
        }
        private static bool InvokeCaughtB(MethodInfo info, object o, object[] os)
        {
            Object obj = InvokeCaught0(info, o, os, Do_NOTHING);
            if (obj == null)
            {
                return false;
            }
            bool tf = (bool)obj;
            if (tf) return true;
            BP();
            obj = InvokeCaught0(info, o, os, Do_NOTHING);
            return false;
        }
        private static object InvokeCaught(MethodInfo info, object o, object[] os, Action todo)
        {
            return InvokeCaught0(info, o, os, todo);
        }

        private static object InvokeCaught0(MethodInfo info, object o, object[] os, Action todo)
        {
            if (!ClientReady)
            {
                Embedded.Warn("Client not Ready");
                return null;
            }
            Thread threadCurrentThread = Thread.CurrentThread;
            bool add1FrameCount = false;
            bool openFFI = false;
            uint fid = 0;
            object to = o;
            if (add1FrameCount)
            {
                int fidCount = IncrementUseCount(threadCurrentThread, ForiegnFrameCounts);
                if (SaneThreadWorld) if (fidCount == 1) fid = libpl.PL_open_foreign_frame();
            } else
            {
                if (openFFI) fid = libpl.PL_open_foreign_frame();
            }
            try
            {
                if (info.IsGenericMethodDefinition)
                {
                    Type[] paramTypes = GetObjectTypes(info.GetParameters());
                    Type[] t = GetObjectTypes(os, paramTypes);
                    info = info.MakeGenericMethod(t);
                }
                var ps = info.GetParameters();
                int psLengthM1 = ps.Length - 1;
                bool isVarArg = (info.CallingConvention & CallingConventions.VarArgs) != 0;
                if (isVarArg)
                {
                    int usedUp = 0;
                    object[] ao = new object[psLengthM1 + 1];
                    for (int i = 0; i < psLengthM1; i++)
                    {
                        ao[i] = RecastObject(ps[i].ParameterType, os[i], null);
                        usedUp++;
                    }
                    int slack = os.Length - usedUp;
                    object[] lastArray = new object[slack];
                    int fillAt = 0;
                    while (slack-- > 0)
                    {
                        lastArray[fillAt++] = os[usedUp++];
                    }
                    ao[psLengthM1] = lastArray;
                    os = ao;
                }
                if (ps.Length != os.Length)
                {
                    Embedded.Warn("ArgCount mismatch " + info + ": call count=" + os.Length);
                }
                Type dt = info.DeclaringType;
                if (o != null)
                {
                    if (dt != null && !dt.IsInstanceOfType(o))
                    {
                        // Assume static
                        to = null;
                    }
                }
                object ret = info.Invoke(to, os);
                CommitPostCall(todo);
                if (ret == null)
                {
                    if (info.ReturnType == typeof(void)) return null;
                    if (info.ReturnType == typeof (Type))
                    {
                        return null;
                    }
                    Embedded.Warn("VoidOrNull " + info);
                    return VoidOrNull(info);
                }
                return ret;
            }
            catch (Exception ex)
            {
                string s = ExceptionString(ex);
                var callTerm = MakeCallTerm(info, to, os);
                Embedded.Error("{0} caused {1}", info, s);
                //throw pe;
                return false;// pe;
            }
            finally
            {
                if (add1FrameCount) DecrementUseCount(threadCurrentThread, ForiegnFrameCounts);
                if (fid > 0) libpl.PL_close_foreign_frame(fid);
            }
        }

        private static object MakeCallTerm(MethodInfo info, object to, object[] os)
        {
            return "callTerm: " + info + " " + ToString(to)  + " " + ToString(os);
        }
        public static string ExceptionString(Exception ex)
        {
            var pe = ToPlException(ex);
            var ie = InnerMostException(ex);
            return ie.ToString() + "\n" + ie.StackTrace;
        }
        private static Type[] GetObjectTypes(ParameterInfo[] parameterInfos)
        {
            int parameterInfosLength = parameterInfos.Length;
            Type[] t = new Type[parameterInfosLength];
            for (int i = 0; i < parameterInfosLength; i++)
            {
                t[i] = parameterInfos[i].ParameterType;
            }
            return t;
        }
        public static Type[] GetObjectTypes(object[] objects, Type[] otherwise)
        {
            Type[] t = new Type[objects.Length];
            for (int i = 0; i < objects.Length; i++)
            {
                var obj = objects[i];
                if (obj != null) t[i] = objects[i].GetType();
                else t[i] = otherwise[i];
            }
            return t;
        }

        private static Exception InnerMostException(Exception ex)
        {
            if (ex is ReflectionTypeLoadException)
            {
                var ile = ((ReflectionTypeLoadException) ex).LoaderExceptions;
                if (ile.Length == 1) return InnerMostException(ile[0]);
            }
            var ie = ex.InnerException;
            if (ie != null && ie != ex)
            {
                return InnerMostException(ie);
            }
            return ex;
        }
        private static PlException ToPlException(Exception ex)
        {
            if (ex is PlException) return (PlException)ex;
            var ie = InnerMostException(ex);
            if (ie != null && ie != ex)
            {
                return ToPlException(ie);
            }
            return new PlException(ex.GetType() + ": " + ex.Message, ex);
        }

        private static Type GetArityType(int paramlen)
        {
            switch (paramlen)
            {
                case 0:
                    return typeof(DelegateParameter0);
                case 1:
                    return typeof(DelegateParameter1);
                case 2:
                    return typeof(DelegateParameter2);
                case 3:
                    return typeof(DelegateParameter3);
                case 4:
                    return typeof(DelegateParameter3);
                default:
                    return null;
            }
        }

        [PrologVisible]
        private static bool testOut(int incoming, out int outbound)
        {
            outbound = incoming;
            return true;
        }
        [PrologVisible]
        private static bool testOpt(int incoming, string optionalstr, out int outbound)
        {
            outbound = incoming;
            return true;
        }
        [PrologVisible]
        private static bool testRef(int incoming, ref string optionalstr, out int outbound)
        {
            outbound = incoming;
            optionalstr = "" + incoming;
            return true;
        }
        [PrologVisible]
        private static bool testVarArg(out int outbound, params int[] incoming)
        {
            outbound = 0;
            foreach (int i in incoming)
            {
                outbound += i;
            }
            return true;
        }

        public class PinnedObject<T> : IDisposable where T : struct
        {
            public T managedObject;
            protected GCHandle handle;
            protected IntPtr ptr;
            protected bool disposed;

            public T ManangedObject
            {
                get
                {
                    return (T)handle.Target;
                }
                set
                {
                    managedObject = value;
                    Marshal.StructureToPtr(value, ptr, false);
                }
            }

            public IntPtr Pointer
            {
                get { return ptr; }
            }

            public PinnedObject()
            {
                handle = GCHandle.Alloc(managedObject, GCHandleType.Pinned);
                ptr = handle.AddrOfPinnedObject();
            }

            ~PinnedObject()
            {
                Dispose();
            }

            public void Dispose()
            {
                if (!disposed)
                {
                    handle.Free();
                    ptr = IntPtr.Zero;
                    disposed = true;
                }
            }

            public void Recopy()
            {
                handle = GCHandle.Alloc(managedObject, GCHandleType.Pinned);
                ptr = handle.AddrOfPinnedObject();
                Marshal.StructureToPtr(managedObject, ptr, false);
            }
        }


        public static T InvokeFromC<T>(Func<T> action, bool discard)
        {
            Thread threadCurrentThread = Thread.CurrentThread;
            int fidCount = IncrementUseCount(threadCurrentThread, ForiegnFrameCounts);
            //lock (SafeThreads)
            {
                try
                {
                    return InvokeFromC0(action, discard, fidCount == 1);
                }
                catch (AccessViolationException)
                {
                    throw;
                }
                catch (Exception)
                {
                    throw;
                } finally
                {
                    DecrementUseCount(threadCurrentThread, ForiegnFrameCounts);
                }
            } 
        }
        public static T InvokeFromC0<T>(Func<T> action, bool discard, bool useFrame)
        {
            discard = true;
            useFrame = true;
            Thread threadCurrentThread = Thread.CurrentThread;
            RegisterThread(threadCurrentThread);
            uint fid = 0;
            if (useFrame) fid = libpl.PL_open_foreign_frame();

            try
            {
                return action();
            }
            catch (AccessViolationException)
            {
                throw;
            }
            catch (Exception e)
            {
                throw e;
            }
            finally
            {
                if (discard && useFrame)
                {
                    libpl.PL_close_foreign_frame(fid);
                }
                DeregisterThread(threadCurrentThread);
            }
        }

        private static string PredicateName(PlTerm term)
        {
            if (term.Name == "{}")
            {
                if (term.Arity == 1)
                {
                    return PredicateName(term.Arg(0));
                }
            }
            if (term.Name == ":")
            {
                if (term.Arity == 2)
                {
                    return PredicateName(term.Arg(1));
                }
            }
            if (term.Name == "/")
            {
                if (term.Arity == 2)
                {
                    return PredicateName(term.Arg(0));
                }
            }
            return term.Name;
        }

        private static string PredicateModule(PlTerm term)
        {
            if (term.Name == "{}")
            {
                if (term.Arity == 1)
                {
                    return PredicateModule(term.Arg(0));
                }
            }
            if (term.Name == ":")
            {
                if (term.Arity == 2)
                {
                    return PredicateName(term.Arg(0));
                }
            }
            return "user";
        }

        private static int PredicateArity(PlTerm term)
        {
            if (term.Name == "{}")
            {
                if (term.Arity == 1)
                {
                    return PredicateArity(term.Arg(0));
                }
            } 
            if (term.Name == ":")
            {
                if (term.Arity == 2)
                {
                    return PredicateArity(term.Arg(1));
                }
            }
            if (term.Name == "/")
            {
                if (term.Arity == 2)
                {
                    return term.Arg(1).intValue();
                }
            }
            return term.Arity;
        }

        private static void CommitPostCall(Action action)
        {
            if (action != null) action();
        }
    }

    public delegate int NonDetDelegate(PlTerm term, PlTerm term2);
}
