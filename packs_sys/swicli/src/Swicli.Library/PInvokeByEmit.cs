using System;
using System.Collections.Generic;
using System.IO;
//using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Text;
//using System.Threading.Tasks;

namespace Swicli.Library
{



#if false
    //
    // Base interface definition
    //
    public interface INativeImport : IDisposable
    {

    }
    //
    // Attribute class definition
    //
    public class ImportFunction : System.Attribute
    {
        public string ModuleName { get; set; }

        public CharSet CharSet { get; set; }

        public CallingConvention CallingConvention { get; set; }

        public ImportFunction(string moduleName)
        {
            if (String.IsNullOrEmpty(moduleName))
                throw new ArgumentException("Module name null");

            this.ModuleName = moduleName;
        }
    }



    public class NativeImportBase : IDisposable
    {
        //
        // List of module names
        private List<string> moduleNames;

        //
        // Define native functions that need to unload native libraries
        //
        [DllImport("kernel32.dll")]
        private static extern bool FreeLibrary(IntPtr hModule);

        private static uint GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT = 0x00000002;

        [DllImport("kernel32.dll")]
        private static extern bool GetModuleHandleEx(uint flags, string moduleName, out IntPtr hModule);

        public NativeImportBase(List<string> pModuleNames)
        {
            if (pModuleNames == null)
                throw new ArgumentException("Module names null");

            moduleNames = pModuleNames;
        }

        // Implement Dispose pattern in base class
        //
        ~NativeImportBase()
        {
            Dispose(false);
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        private void Dispose(bool disposing)
        {
            //
            // Dispose loaded modules
            //

            IntPtr hModule = new IntPtr();
            foreach (string moduleName in moduleNames)
            {
                if (true == GetModuleHandleEx(
                        GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                        moduleName, out hModule) && hModule != IntPtr.Zero)
                {
                    if (FreeLibrary(hModule) == false)
                    {
                        throw new SystemException(
                            String.Format("Unable to unload module {0}. Error code: {1}",
                                moduleName,
                                Marshal.GetLastWin32Error()));
                    }
                }

            }

        }
    }

    //
    // Factory class implementation
    //
    public static class NativeImport
    {
        //
        // Our factory method
        //
        
        public static T Create<T>(string modulePath ) where T : INativeImport
        {
            AssemblyName aName = new AssemblyName("NativeImport" + Guid.NewGuid().ToString());
            AssemblyBuilder aBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(aName, AssemblyBuilderAccess.Run);

            ModuleBuilder mBuilder = aBuilder.DefineDynamicModule(aName.Name);

            Type interfaceType = typeof(T);

            TypeBuilder tBuilder = mBuilder.DefineType(Guid.NewGuid().ToString(),
                TypeAttributes.Public, typeof(NativeImportBase), new Type[]{ typeof(T) });

            // Define default constructor
            DefineConstructor(tBuilder);

            // Storage for modules to load
            List<string> moduleNames = new List<string>();
            
            MethodInfo [] interfaceMethods = interfaceType.GetMethods();

            foreach (MethodInfo methodInfo in interfaceMethods)
            {
                // Get custom attribute
                ImportFunction[] attrs = (ImportFunction[])methodInfo.GetCustomAttributes(typeof(ImportFunction), false);

                if (attrs == null || attrs.Length<1)
                    throw new InvalidOperationException(
                        String.Format(
                            "ImportFunction attribute not specified for function {0}", methodInfo.Name));

                ImportFunction attr = attrs[0];
                ParameterInfo [] paramInfos = methodInfo.GetParameters();
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
                // Define PInvoke method
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
            Type [] ctorParameters = new Type[] { typeof(List<string>) };
            
            ConstructorBuilder ctorBuilder = tBuilder.DefineConstructor(
                MethodAttributes.Public, CallingConventions.Standard, ctorParameters);
            ILGenerator ctorIL = ctorBuilder.GetILGenerator();
            ctorIL.Emit(OpCodes.Ldarg_0);
            ctorIL.Emit(OpCodes.Ldarg_1);
            ctorIL.Emit(OpCodes.Call, typeof(NativeImportBase).GetConstructor(ctorParameters));
            ctorIL.Emit(OpCodes.Ret);
        }

        private static MethodBuilder DefinePInvokeMethod(TypeBuilder tBuilder, string methodName, string moduleName, 
            CallingConvention callingConvention, CharSet charset, Type returnType, Type [] parameters)
        {
            MethodBuilder pInvokeMethod = tBuilder.DefinePInvokeMethod(methodName, moduleName,
                MethodAttributes.Private | MethodAttributes.Static | MethodAttributes.PinvokeImpl,
                CallingConventions.Standard, 
                returnType,
                parameters,
                callingConvention, charset);
        
            if(!returnType.Equals(typeof(void)))
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
            for(int i=1; i <= parameters.Length; ++i)
            {
                switch(i) {
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

    }
#endif
}
