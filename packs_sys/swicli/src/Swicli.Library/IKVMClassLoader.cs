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

using System.Diagnostics;
//using java.lang;
using org.jpl7;
using sun.reflect.misc;
#if USE_IKVM
using JavaClass = java.lang.Class;
using Type = System.Type;
using ClassLoader = java.lang.ClassLoader;
//using sun.reflect.misc;
//using IKVM.Internal;
using Hashtable = java.util.Hashtable;
using ikvm.runtime;
using java.net;
#else
using JClass = System.Type;
using Type = System.Type;
#endif
using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Callback;
using Exception = System.Exception;
using PlTerm = SbsSW.SwiPlCs.PlTerm;
using Process = java.lang.Process;
using String = System.String;

namespace Swicli.Library
{
    public partial class PrologCLR
    {
        public static ScriptingClassLoader scriptingClassLoader = null;
#if USE_IKVM


        public static bool cliAddPath(String url)
        {
            cliSetupIKVM();
            scriptingClassLoader.AddPath(url);
            return true;
        }

        public static bool cliAddClasspath(String url)
        {
            cliSetupIKVM();
            scriptingClassLoader.AddClasspath(url);
            return true;
        }

        private static bool IKVMSetupAlready = false;

        public static void cliSetupIKVM()
        {
            if (IKVMSetupAlready) return;
            IKVMSetupAlready = true;
            if (!IsIKVMDir(IKVMHome)) IKVMHome = Environment.GetEnvironmentVariable("IKVM_BINDIR");
            if (!IsIKVMDir(IKVMHome))
                IKVMHome = new FileInfo(typeof (ikvm.runtime.Util).Assembly.Location).DirectoryName;
            if (!IsIKVMDir(IKVMHome)) IKVMHome = Environment.CurrentDirectory;
            Environment.SetEnvironmentVariable("IKVM_BINDIR", IKVMHome);
            DirectoryInfo destination = new DirectoryInfo(IKVMHome);
            DirectoryInfo source;
            if (Embedded.Is64BitRuntime())
            {
                source = new DirectoryInfo(IKVMHome + "/x64-win64/");
            }
            else
            {
                source = new DirectoryInfo(IKVMHome + "/i386-win32/");
            }
            Embedded.ConsoleWriteLine("Source of swicli=" + source);
            scriptingClassLoader = new ScriptingClassLoader(ClassLoader.getSystemClassLoader());

            //  if (source.Exists) CopyFiles(source, destination, true, "*.*", false);
        }

        public static void cliEnsureClasspath()
        {
            if (true) return;
            String overloadClasspath = Environment.GetEnvironmentVariable("LOGICMOO_CLASSPATH");
            if (overloadClasspath != null)
            {
                cliAddClasspath(overloadClasspath);
            }
            else
            {
                String javaClasspath = java.lang.System.getProperty("java.class.path");
                cliAddClasspath(javaClasspath);
                String javaEnvClasspath = Environment.GetEnvironmentVariable("CLASSPATH");
                if (javaEnvClasspath != null && !javaEnvClasspath.Equals(javaClasspath))
                {
                    cliAddClasspath(javaEnvClasspath);
                }

            }
        }


        private static
            bool IsIKVMDir(string ikvmHome)
        {
            return File.Exists(ikvmHome + "/ikvm.exe");
        }

#pragma warning disable 414, 3021
        [CLSCompliant(false)]
        public class ScriptingClassLoader : URLClassLoader
        {
            private readonly IList<ClassLoader> lc = new List<ClassLoader>();
            //  IList<AppDomainAssemblyClassLoader> AppDomainAssemblyClassLoaders = new List<AppDomainAssemblyClassLoader>();
            // IList<AssemblyClassLoader> AssemblyClassLoaders = new List<AssemblyClassLoader>();
            //IList<ClassPathAssemblyClassLoader> ClassPathAssemblyClassLoaders = new List<ClassPathAssemblyClassLoader>();
            // IList<URLClassLoader> URLClassLoaders = new List<URLClassLoader>();
            private IList<MethodUtil> MethodUtils = new List<MethodUtil>();

            public ScriptingClassLoader(ClassLoader cl)
                : base(new URL[0], cl)
            {
                //AddLoader(cl);
            }

            public void AddDirectory(string cl)
            {
                java.io.File f = new java.io.File(cl);
                addURL(f.toURL());
            }

            public void AddClasspath(string cl)
            {
                String[] paths = cl.Split(';');
                foreach (string path in paths)
                {
                    AddPath(path);
                }
            }

            public void AddPath(string path)
            {
                Embedded.ConsoleWriteLine("Adding path: '" + path + "'");
                if (String.IsNullOrEmpty(path))
                {
                    return;
                }
                if (Directory.Exists(path))
                {
                    AddDirectory(path);
                    AddAssemblySearchPath(path);
                }
                else if (path.EndsWith(".abcl") || path.EndsWith(".jar") || path.EndsWith(".zip"))
                {
                    AddJar(path);
                }
                else
                {
                    try
                    {
                        AddAssembly(path);
                    }
                    catch (Exception e)
                    {
                        AddAssemblySearchPath(path);
                    }
                }

            }

            public void AddJar(string cl)
            {
                java.io.File f = new java.io.File(cl);
                if (f.exists())
                {
                    String path = f.getPath();
                    int lastDot = path.LastIndexOf('.');
                    if (lastDot > 1)
                    {
                        String ext = path.Substring(lastDot + 1);
                        path = path.Substring(0, lastDot);
                        if (ext == "jar" && !path.Contains(@"appdapter"))
                        {

                            String compileTo = IKVMHome;
                            String newDll = path + ".dll";
                            if (File.Exists(newDll))
                            {
                                if (ResolveAssembly(Assembly.LoadFile(newDll)))
                                {
                                    Embedded.ConsoleWriteLine("ResolveAssembly: " + newDll);
                                    addURL(f.toURL());
                                    return;
                                }
                            }
                            String commandArgs = "" + cl + " -out:" + newDll;
                            ProcessStartInfo cmdsi = new ProcessStartInfo(compileTo + "\\ikvmc.exe");
                            cmdsi.Arguments = commandArgs;
                            if(false) StartProcessNoActivate("...");
                            System.Diagnostics.Process cmd = System.Diagnostics.Process.Start(cmdsi);
                            cmd.WaitForExit();
                            if (File.Exists(newDll))
                            {
                                if (ResolveAssembly(Assembly.LoadFile(newDll)))
                                {
                                    Embedded.ConsoleWriteLine("CompiledAssembly: " + newDll);
                                    addURL(f.toURL());
                                    return;
                                }
                            }
                        }
                    }

                }
                addURL(f.toURL());
            }

            public void AddAssembly(string cl)
            {
                AddLoader(new AssemblyClassLoader(PrologCLR.LoadAssemblyByFile(cl)));
            }

            public bool AddLoader(ClassLoader cl)
            {
                lock (lc)
                {
                    if (cl == null) return false;
                    {
                        bool added = AddLoader(cl.getParent());
                        if (lc.Contains(cl)) return added;
                        lc.Insert(0, cl);
                        if (cl is MethodUtil)
                        {
                            MethodUtils.Add((MethodUtil) cl);
                            // added = true;
                        }
                        //added = true;
                        /* 
                        
           
                        if (cl is AppDomainAssemblyClassLoader)
                        {
                           // AppDomainAssemblyClassLoaders.Add((AppDomainAssemblyClassLoader)cl);
                            added = true;
                        }
                        if (cl is AssemblyClassLoader)
                        {
                          //  AssemblyClassLoaders.Add((AssemblyClassLoader)cl);
                            added = true;
                        }
                        if (cl is ClassPathAssemblyClassLoader)
                        {
                           // ClassPathAssemblyClassLoaders.Add((ClassPathAssemblyClassLoader)cl);
                            added = true;
                        }
                        if (!added)
                        {
                            if (cl is MethodUtil)
                            {
                                MethodUtils.Add((MethodUtil)cl);
                                added = true;
                            }
                            else
                                if (cl is URLClassLoader)
                                {
                              //      URLClassLoaders.Add((URLClassLoader)cl);
                                    added = true;
                                }
                        }
                        */
                        return true;

                    }
                }
            }

            public static
                void Check()
            {

            }

            public string FindLibrary(string libname)
            {
                return base.findLibrary(libname);
            }

            public JavaClass LoadClass(string name, bool resolve)
            {
                return base.loadClass(name, resolve);
            }

            public JavaClass ResolveClass(JavaClass clz)
            {
                base.resolveClass(clz);
                return clz;
            }

            public override JavaClass loadClass(string name)
            {
                if (true) return base.loadClass(name);
                JavaClass c = base.loadClass(name, false);
                if (c != null) ResolveClass(c);
                return c;
            }

            protected override JavaClass findClass(string name)
            {
                if (true) return base.findClass(name);
                ClassLoader was = java.lang.Thread.currentThread().getContextClassLoader();
                try
                {
                    // java.lang.Thread.currentThread().setContextClassLoader(this);
                    JavaClass t = findClass0(name);
                    if (t != null)
                    {
                        return t;
                    }
                    return t;

                }
                finally
                {
                    java.lang.Thread.currentThread().setContextClassLoader(was);

                }

            }

            protected JavaClass findClass0(string name)
            {
                java.lang.ClassNotFoundException cnfG = null;
                lock (lc)
                {
                    JavaClass t = null;

                    foreach (ClassLoader loader in lc)
                    {
                        try
                        {
                            t = loader.loadClass(name);

                        }
                        catch (java.lang.ClassNotFoundException cnf)
                        {
                            if (cnfG == null) cnfG = cnf;
                        }
                        if (t != null)
                        {


                            return t;
                        }
                    }
                    //if (cnfG == null) 
                    return base.findClass(name);
                    //throw cnfG;
                }

            }



        }


#pragma warning restore 414, 3021

        private static void TestClassLoader()
        {
            //using java.lang;
            //IKVM.Internal.BootstrapClassLoader()
            ScriptingClassLoader cl = new ScriptingClassLoader(ClassLoader.getSystemClassLoader());

            string s = "jpl.fli.term_t";
            JavaClass c;
            try
            {
                c = cl.loadClass(s);
            }
            catch (java.lang.ClassNotFoundException e)
            {
            }
            catch (java.security.PrivilegedActionException e)
            {

            }

            foreach (
                var s1 in
                    new Type[]
                    {
                        1.GetType(), true.GetType(), "".GetType(), typeof (void), 'a'.GetType(), typeof (Type[]),
                        typeof (IComparable<Type>)
                    })
            {
                c = getFriendlyClassFromType(s1);
                if (c != null)
                {
                    ConsoleTrace("class: " + c + " from type " + s1.FullName);
                    continue;
                }
                ConsoleTrace("cant get " + s1.FullName);
            }

            foreach (var s1 in new JPL().GetType().Assembly.GetTypes())
            {
                c = getInstanceTypeFromClass(s1);
                c = getFriendlyClassFromType(s1);
                if (c != null)
                {
                    //ConsoleTrace("" + c);
                    continue;
                }
                ConsoleTrace("cant get " + s1.FullName);
            }
            return;
        }

        private static string clasPathOf(JPL jpl1)
        {
            string s = null;
            var cl = jpl1.getClass().getClassLoader();
            if (cl != null)
            {
                var r = cl.getResource(".");
                if (r != null)
                {
                    s = r.getFile();
                }
                else
                {
                    var a = jpl1.GetType().Assembly;
                    if (a != null)
                    {
                        s = a.Location;
                    }
                }
            }
            return s;
        }
#endif





// This is possible but only via pinvoke, which unfortunately requires about 70 lines of code:

        [StructLayout(LayoutKind.Sequential)]
        private struct STARTUPINFO
        {
            public Int32 cb;
            public string lpReserved;
            public string lpDesktop;
            public string lpTitle;
            public Int32 dwX;
            public Int32 dwY;
            public Int32 dwXSize;
            public Int32 dwYSize;
            public Int32 dwXCountChars;
            public Int32 dwYCountChars;
            public Int32 dwFillAttribute;
            public Int32 dwFlags;
            public Int16 wShowWindow;
            public Int16 cbReserved2;
            public IntPtr lpReserved2;
            public IntPtr hStdInput;
            public IntPtr hStdOutput;
            public IntPtr hStdError;
        }

        [StructLayout(LayoutKind.Sequential)]
        internal struct PROCESS_INFORMATION
        {
            public IntPtr hProcess;
            public IntPtr hThread;
            public int dwProcessId;
            public int dwThreadId;
        }

        [DllImport("kernel32.dll")]
        private static extern bool CreateProcess(
            string lpApplicationName,
            string lpCommandLine,
            IntPtr lpProcessAttributes,
            IntPtr lpThreadAttributes,
            bool bInheritHandles,
            uint dwCreationFlags,
            IntPtr lpEnvironment,
            string lpCurrentDirectory,
            [In] ref STARTUPINFO lpStartupInfo,
            out PROCESS_INFORMATION lpProcessInformation
            );

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool CloseHandle(IntPtr hObject);

        private const int STARTF_USESHOWWINDOW = 1;
        private const int SW_SHOWNOACTIVATE = 4;
        private const int SW_SHOWMINNOACTIVE = 7;


        public static void StartProcessNoActivate(string cmdLine)
        {
            STARTUPINFO si = new STARTUPINFO();
            si.cb = Marshal.SizeOf(si);
            si.dwFlags = STARTF_USESHOWWINDOW;
            si.wShowWindow = SW_SHOWMINNOACTIVE;

            PROCESS_INFORMATION pi = new PROCESS_INFORMATION();

            CreateProcess(null, cmdLine, IntPtr.Zero, IntPtr.Zero, true,
                0, IntPtr.Zero, null, ref si, out pi);

            CloseHandle(pi.hProcess);
            CloseHandle(pi.hThread);
        }
    }
}