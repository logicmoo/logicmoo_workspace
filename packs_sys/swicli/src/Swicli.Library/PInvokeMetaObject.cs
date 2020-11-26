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
#define USESAFELIB
using System;
#if NET40
using System.Dynamic;
#endif

using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Threading;
//using System.Linq.Expressions;
using System.Collections.Generic;
using System.IO;
// See http://tirania.org/blog/archive/2009/Aug-11.html
using SbsSW.SwiPlCs;

namespace Swicli.Library
{

    public partial class PrologCLR
    {
        [PrologVisible]
        static public
#if NET40
         dynamic
#else
 PInvokeMetaObject
#endif
 cliGetDll(String dll)
        {
            return new PInvokeMetaObject(dll);
        }

        [PrologVisible]
        public static void cliDynTest_1()
        {
            Embedded.Debug("System.Dynamic.DynamicObject=" + Type.GetType("System.Dynamic.DynamicObject"));
        }

        [PrologVisible]
        public static void cliDynTest_2()
        {
            var d = cliGetDll("glibc");

            for (int i = 0; i < 2; ++i)
            {
#if NET40
                d.printf("Hello, World %d\n", i);
#endif
                d.GetInvoke("printf", new Type[] { typeof(String) }, null).Invoke(d, new object[] { "Hello GetInvoke.Invoke: %d\n", i });
            }

        }

        [PrologVisible]
        public static T cliDynTest_3<T>()
        {
            T was = default(T);
            var d = cliGetDll("libc");

            for (int i = 0; i < 2; ++i)
            {
#if NET40
                d.printf("Hello, World %d\n", i);
#else
                was = d.Invoke<T>("printf", new Type[] { typeof(String) }, null, d, new object[] { "Hello GetInvoke.Invoke: %d\n", i });
#endif
                was = d.InvokeDLL<T>("printf", new object[] { "Hello Invoke: %d\n", i });
            }
            return was;
        }


        public static void MainLinux(String[] args)
        {
            var d = cliGetDll("libc");

            for (int i = 0; i < 2; ++i)
            {
#if NET40
                d.printf("Hello, World %d\n", i);
#else
                d.GetInvoke("printf", new Type[] { typeof(String) }, null).Invoke(d, new object[] { "Hello, World %d\n", i });
#endif
                d.GetInvoke("printf", new Type[] { typeof(String) }, null).Invoke(d, new object[] { "Hello GetInvoke.Invoke: %d\n", i });
            }
        }
    }










    #region P/Invoke
    // Adapted from http://tirania.org/blog/archive/2009/Aug-11.html
    // This is a hack to work around Linux not being able to use libld.so to dynamically
    // load .so modules...
    public partial class PInvokeMetaObject
#if NET40
        , DynamicObject
#endif
    {
        static int clid_gen;
        string assembName = "ctype_";
        

        private PInvokeMetaObject _pInvokeDll = null;
        private SafeLibraryHandle _hLibrary; 
        // private IntPtr _hDLL;
        private string _dllName;
        private AssemblyBuilder _assemblyBuilder;
        private ModuleBuilder _moduleBuilder;
        int _idGen;
        public override string ToString()
        {
            return assembName;
        }
        public PInvokeMetaObject(string name)
        {
            _dllName = name;
            assembName = _dllName;
            foreach (string clip in new String[] { ".dll", ".so", ".exe", ".dynlib" })
            {
                if (assembName.ToLower().EndsWith(clip)) assembName = assembName.Substring(0, assembName.Length - clip.Length);
            }
            //_hLibrary = NativeMethods.LoadUnmanagedLibrary(_dllName, true);
            assembName = "dynlib_0" + Interlocked.Increment(ref clid_gen) + "_" + assembName;
            EnsureBuilders();
        }

        public bool IsLibraryValid
        {
            get
            {
#if USESAFELIB                
                return _hLibrary != null && !_hLibrary.IsInvalid;
#else
                return true;
#endif
            }
        }

 

        private void EnsureBuilders()
        {
            if (_assemblyBuilder == null)
            {
                AssemblyName aname = new AssemblyName(assembName);
                _assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(aname, AssemblyBuilderAccess.RunAndSave);
                _moduleBuilder = _assemblyBuilder.DefineDynamicModule(assembName);
            }
            
        }

        private Delegate GetNativeDelegate(string funcName, Type delegateType)
        {
            if (_pInvokeDll != null)
                return _pInvokeDll.CreateDelegate(funcName, delegateType);
            IntPtr _hDLL = _hLibrary.DangerousGetHandle();
            IntPtr funcPtr = NativeMethodsWindows.GetProcAddress(_hLibrary, funcName);
            if (funcPtr.Equals(IntPtr.Zero)) funcPtr = NativeMethodsLinux.GetProcAddress(_hLibrary, funcName);
            if (funcPtr.Equals(IntPtr.Zero)) return null;
            return Marshal.GetDelegateForFunctionPointer(funcPtr, delegateType);
        }

        public Delegate CreateDelegate(string entry_point, Type delegateType)
        {
            MethodInfo methodInfo = delegateType.GetMethod("Invoke");
            MethodInfo returnMethodInfo //
                = CreateMethodInfoForDelegate(entry_point, methodInfo);
            return Delegate.CreateDelegate(delegateType, returnMethodInfo);
        }
        public MethodInfo CreateMethodInfoForDelegate(string entry_point, MethodInfo methodInfo)
        {
            // Get parameter signature information from delegate Invokation
            ParameterInfo returnParameter = methodInfo.ReturnParameter;
            ParameterInfo[] parameters = methodInfo.GetParameters();
            Type[] parameterTypes = new Type[parameters.Length];
            Type[][] parameterTypeRequiredCustomModifiers = new Type[parameters.Length][];
            Type[][] parameterTypeOptionalCustomModifiers = new Type[parameters.Length][];
            for (int i = 0; i < parameters.Length; ++i)
            {
                parameterTypes[i] = parameters[i].ParameterType;
                parameterTypeRequiredCustomModifiers[i] = parameters[i].GetRequiredCustomModifiers();
                parameterTypeOptionalCustomModifiers[i] = parameters[i].GetOptionalCustomModifiers();
            }


            EnsureBuilders();

            TypeBuilder typeBuilder = _moduleBuilder.DefineType(assembName + Interlocked.Increment(ref _idGen) + "_" + entry_point);

            MethodBuilder mb = typeBuilder.DefinePInvokeMethod("Invoke", _dllName, entry_point,
               MethodAttributes.Static | MethodAttributes.PinvokeImpl, CallingConventions.Standard,
               returnParameter.ParameterType, returnParameter.GetRequiredCustomModifiers(), returnParameter.GetOptionalCustomModifiers(),
               parameterTypes, parameterTypeRequiredCustomModifiers, parameterTypeOptionalCustomModifiers,
               CallingConvention.StdCall, CharSet.Auto);

            Type builtType = typeBuilder.CreateType();
            MethodInfo returnMethodInfo = builtType.GetMethod("Invoke", BindingFlags.Static | BindingFlags.NonPublic) ??
                                          builtType.GetMethod("Invoke", BindingFlags.Static | BindingFlags.Public);

            return returnMethodInfo;
        }

        public MethodInfo GetInvoke(string entry_point, Type[] arg_types, Type returnType)
        {
            EnsureBuilders();

            String entry_point_invoke = assembName + "_ctype_" + Interlocked.Increment(ref _idGen) + "_" + entry_point;


            // Can't use DynamicMethod as they don't support custom attributes
            TypeBuilder typeBuilder = _moduleBuilder.DefineType(entry_point_invoke);

            MethodBuilder mb = typeBuilder.DefinePInvokeMethod(entry_point_invoke, _dllName, entry_point,
                       MethodAttributes.Static | MethodAttributes.PinvokeImpl, CallingConventions.Standard, 
                       returnType, arg_types,
                       CallingConvention.StdCall, CharSet.Auto);

            var t = typeBuilder.CreateType();
            var m = t.GetMethod(entry_point_invoke, BindingFlags.Static | BindingFlags.NonPublic);

            return m;
        }


        #endregion


        public T Invoke<T>(string mspecName, Type[] paramz, Type returnType, Object targetIn, params object[] args)
        {
            if (paramz == null) paramz = PrologCLR.GetObjectTypes(args, null);
            var mi = GetInvoke(mspecName, paramz, returnType);
            Object target = mi.IsStatic ? null : (targetIn ?? this);
            return (T)mi.Invoke(target, args);
        }


#if NET40
        public override DynamicMetaObject GetMetaObject(Expression parameter)
        {
            return new PInvokeMetaObject(parameter, this);
        }
#endif

        #region PInvokeMetaObject Members


        public T InvokeDLL<T>(string mspecName, params object[] args)
        {
            return Invoke<T>(mspecName, (Type[])null, null, this, args);
        }

        #endregion
#if DEAD

        //
        // Our factory method
        //

        public static T CreateZZZ<T>(string modulePath) where T : INativeImport
        {
            AssemblyName aName = new AssemblyName(Guid.NewGuid().ToString());
            AssemblyBuilder aBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(aName, AssemblyBuilderAccess.Run);

            ModuleBuilder mBuilder = aBuilder.DefineDynamicModule(aName.Name);

            Type interfaceType = typeof(T);

            TypeBuilder tBuilder = mBuilder.DefineType(Guid.NewGuid().ToString(),
                TypeAttributes.Public, typeof(NativeImportBase), new Type[] { typeof(T) });

            // Define default constructor
            DefineConstructor(tBuilder);

            // Storage for modules to load
            List<string> moduleNames = new List<string>();

            MethodInfo[] interfaceMethods = interfaceType.GetMethods();

            foreach (MethodInfo methodInfo in interfaceMethods)
            {
                // Get custom attribute
                ImportFunction[] attrs = (ImportFunction[])methodInfo.GetCustomAttributes(typeof(ImportFunction), false);

                if (attrs == null || attrs.Length < 1)
                    throw new InvalidOperationException(
                        String.Format(
                            "ImportFunction attribute not specified for function {0}", methodInfo.Name));

                ImportFunction attr = attrs[0];
                ParameterInfo[] paramInfos = methodInfo.GetParameters();
                Type[] parameters = new Type[paramInfos.Length];

                for (int i = 0; i < paramInfos.Length; ++i)
                    parameters[i] = paramInfos[i].ParameterType;

                //
                // Check if module path is defined
                //
                string moduleName;
                if (!String.Empty.Equals(modulePath))
                    moduleName = Path.Combine(modulePath, attr.ModuleName);
                else
                    moduleName = attr.ModuleName;

                //
                // Define PInvokeMetaObject method
                //
                MethodBuilder pInvokeMethod = DefinePInvokeMethod(tBuilder, methodInfo.Name, moduleName,
                    attr.CallingConvention, attr.CharSet,
                    methodInfo.ReturnType, parameters);

                //
                // Define proxy method
                //
                MethodBuilder proxyMethod = DefineProxyMethod<T>(tBuilder, methodInfo.Name,
                    methodInfo.ReturnType, parameters, pInvokeMethod);

                //
                // Add module to list if not already added
                //
                if (!moduleNames.Exists(
                    delegate(string str)
                    {
                        return String.Equals(str, moduleName, StringComparison.InvariantCultureIgnoreCase);
                    }))
                {
                    moduleNames.Add(moduleName);
                }

            }

            //
            // Create type
            //
            Type t = tBuilder.CreateType();

            //
            // Create instance using constructor with List<string> parameters
            // Pass module names as parameter
            //
            object o = Activator.CreateInstance(t, new object[] { moduleNames });

            return (T)o;
        }

        static Module aBuilder_ModuleResolve(object sender, ResolveEventArgs e)
        {
            Console.WriteLine("asdASD");
            return null;
        }

        private static void DefineConstructor(TypeBuilder tBuilder)
        {
            // Ctor takes List<string> as module file names to load
            Type[] ctorParameters = new Type[] { typeof(List<string>) };

            ConstructorBuilder ctorBuilder = tBuilder.DefineConstructor(
                MethodAttributes.Public, CallingConventions.Standard, ctorParameters);
            ILGenerator ctorIL = ctorBuilder.GetILGenerator();
            ctorIL.Emit(OpCodes.Ldarg_0);
            ctorIL.Emit(OpCodes.Ldarg_1);
            ctorIL.Emit(OpCodes.Call, typeof(NativeImportBase).GetConstructor(ctorParameters));
            ctorIL.Emit(OpCodes.Ret);
        }

        private static MethodBuilder DefinePInvokeMethod(TypeBuilder tBuilder, string methodName, string moduleName,
            CallingConvention callingConvention, CharSet charset, Type returnType, Type[] parameters)
        {
            MethodBuilder pInvokeMethod = tBuilder.DefinePInvokeMethod(methodName, moduleName,
                MethodAttributes.Private | MethodAttributes.Static | MethodAttributes.PinvokeImpl,
                CallingConventions.Standard,
                returnType,
                parameters,
                callingConvention, charset);

            if (!returnType.Equals(typeof(void)))
                pInvokeMethod.SetImplementationFlags(pInvokeMethod.GetMethodImplementationFlags() | MethodImplAttributes.PreserveSig);

            return pInvokeMethod;
        }

        private static MethodBuilder DefineProxyMethod<T>(TypeBuilder tBuilder, string methodName, Type returnType, Type[] parameters, MethodBuilder proxiedMethod)
        {
            MethodBuilder proxyMethod = tBuilder.DefineMethod(methodName,
                MethodAttributes.Public | MethodAttributes.Virtual,
                returnType,
                parameters);

            ILGenerator proxyMethodIL = proxyMethod.GetILGenerator();

            //
            // Pass parameters, since this is proxy for a static method, skip first arg
            //
            for (int i = 1; i <= parameters.Length; ++i)
            {
                switch (i)
                {
                    case 1:
                        proxyMethodIL.Emit(OpCodes.Ldarg_1);
                        break;
                    case 2:
                        proxyMethodIL.Emit(OpCodes.Ldarg_2);
                        break;
                    case 3:
                        proxyMethodIL.Emit(OpCodes.Ldarg_3);
                        break;
                    default:
                        proxyMethodIL.Emit(OpCodes.Ldarg, i);
                        break;
                }
            }

            proxyMethodIL.Emit(OpCodes.Call, proxiedMethod);
            proxyMethodIL.Emit(OpCodes.Ret);

            //
            // Override method defined in the interface
            //
            tBuilder.DefineMethodOverride(proxyMethod, typeof(T).GetMethod(methodName));

            return proxyMethod;

        }
#endif

        public void UnLoadUnmanagedLibrary()
        {
#if USESAFELIB
            if (_hLibrary != null && !_hLibrary.IsClosed)
            {
                _hLibrary.Close();
                _hLibrary.UnLoad();
                _hLibrary.Dispose();
                _hLibrary = null;
            }
#endif
        }
    }
    // #if NET40
    /*
    public class PInvokeMetaObject

#if NET40
        : DynamicMetaObject
#endif
    {
        public PInvokeMetaObject(Expression parameter, object o)
#if NET40

            :            base(parameter, BindingRestrictions.Empty, o) { } 
#else
        {
            i_parameter = parameter;
            i_Obj = o;
        }
        Expression i_parameter;
        Object i_Obj;
#endif
#if NET40
        public override DynamicMetaObject BindInvokeMember(InvokeMemberBinder binder, DynamicMetaObject[] args)
        {

            var self = this.Expression;
            var pinvoke = (PInvokeMetaObject)base.Value;

            var arg_types = new Type[args.Length];
            var arg_exps = new Expression[args.Length];
        
            for (int i = 0; i < args.Length; ++i)
            {
                arg_types[i] = args[i].LimitType;
                arg_exps[i] = args[i].Expression;
            }

            var m = pinvoke.GetInvoke(binder.Name, arg_types, binder.ReturnType);
            if (m == null)
                return base.BindInvokeMember(binder, args);
 
            var target = Expression.Block(
                       Expression.Call(m, arg_exps),
                       Expression.Default(typeof(object)));
            var restrictions = BindingRestrictions.GetTypeRestriction(self, typeof(PInvokeMetaObject));

            var dynamicMetaObject = new DynamicMetaObject(target, restrictions);

            return dynamicMetaObject;
        }
    
#endif
    }*/

}
