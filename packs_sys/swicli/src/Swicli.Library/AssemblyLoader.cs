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
using System.Collections;
using System.Runtime.InteropServices;
#if USE_IKVM
/*
using ikvm.extensions;
using IKVM.Internal;
using ikvm.runtime;
using java.net;
using java.util;*/
//using jpl;
//using jpl;
using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
//using sun.reflect.misc;
//using Util = ikvm.runtime.Util;
using Exception = System.Exception;
//using JClass = java.lang.JClass;
using Type = System.Type;
#else
using System;
using JClass = System.Type;
#endif
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using PlTerm = SbsSW.SwiPlCs.PlTerm;

namespace Swicli.Library
{
}
namespace Swicli.Library
{
    public partial class PrologCLR
    {
        static public bool ManagedHalt()
        {
            KillPrologThreads();
            Environment.Exit(0);
            return true;
        }
        static private Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            var domain = (AppDomain)sender;
            foreach (var assembly in CopyOf(domain.GetAssemblies()))
            {
                if (assembly.FullName == args.Name)
                    return assembly;
                if (assembly.ManifestModule.Name == args.Name)
                    return assembly;
            }

            foreach (Assembly assembly in CopyOf(AssembliesLoaded))
            {
                if (assembly.FullName == args.Name)
                    return assembly;
                if (assembly.ManifestModule.Name == args.Name)
                    return assembly;
            }
            string assemblyName = args.Name;
            int comma = assemblyName.IndexOf(",");
            if (comma > 0)
            {
                assemblyName = assemblyName.Substring(0, comma);
            }
            return LoadAssemblyByFile(assemblyName);
        }

        public static Assembly LoadAssemblyByFile(string assemblyName)
        {
            if (File.Exists(assemblyName))
            {
                try
                {
                    var fi = new FileInfo(assemblyName);
                    if (fi.Exists) return Assembly.LoadFile(fi.FullName);
                }
                catch (Exception)
                {
                    throw;
                }
            }
            IList<string> sp = CopyOf((IEnumerable<string>)AssemblySearchPaths);
            foreach (var dir in new[] { AppDomain.CurrentDomain.BaseDirectory, new DirectoryInfo(".").FullName, Path.GetDirectoryName(typeof(PrologCLR).Assembly.CodeBase),Environment.CurrentDirectory })
            {
                if (!sp.Contains(dir)) sp.Add(dir);
            }
            string lastTested = "";
            foreach (var s in CopyOf(AppDomain.CurrentDomain.GetAssemblies()))
            {
                try
                {
                    if (s is AssemblyBuilder) continue;
                    string dir = Path.GetDirectoryName(s.CodeBase);
                    if (dir.StartsWith("file:\\"))
                    {
                        dir = dir.Substring(6);
                    }
                    if (dir.StartsWith("file://"))
                    {
                        dir = dir.Substring(7);

                    }
                    if (dir == lastTested) continue;
                    lastTested = dir;
                    if (!sp.Contains(dir)) sp.Add(dir);
                }
                catch (NotSupportedException)
                {
                    // Reflected Assemblies do this
                }
            }
            foreach (string pathname in sp)
            {
                var assemj = FindAssemblyByPath(assemblyName, pathname);
                if (assemj != null) return assemj;
            }

            return null;
        }

        private static string NormalizePath(string dirname1)
        {
            string dirname = dirname1;
            if (dirname.StartsWith("file:\\"))
            {
                dirname = dirname.Substring(6);
            }
            if (dirname.StartsWith("file://"))
            {
                dirname = dirname.Substring(7);
            }
            dirname = new FileInfo(dirname).FullName;
            if (dirname != dirname1)
            {
                return dirname;
            }
            return dirname1;
        }


        private static readonly List<string> LoaderExtensionStrings = new List<string> { "dll", "exe", "jar", "lib", "dynlib", "class", "so" };
        public static Assembly FindAssemblyByPath(string assemblyName, string dirname)
        {

            dirname = NormalizePath(dirname);
            string filename = Path.Combine(dirname, assemblyName);
            string loadfilename = filename;
            bool tryexts = !File.Exists(loadfilename);
            string filenameLower = filename.ToLower();
            List<string> LoaderExtensions = new List<string>();
            lock (LoaderExtensionStrings)
            {
                LoaderExtensions.AddRange(LoaderExtensionStrings);
            }
            foreach (string extension in LoaderExtensions)
            {
                if (filenameLower.EndsWith("." + extension))
                {
                    tryexts = false;
                    break;
                }
            }

            if (tryexts)
            {
                foreach (var s in LoaderExtensions)
                {
                    string testfile = loadfilename + "." + s;
                    if (File.Exists(testfile))
                    {
                        loadfilename = testfile;
                        break;
                    }

                }
            }
            if (File.Exists(loadfilename))
            {
                try
                {
                    return Assembly.LoadFile(new FileInfo(loadfilename).FullName);
                }
                catch (Exception)
                {
                    throw;
                }
            }
            return null;
        }

        public static Assembly AssemblyLoad(string assemblyName)
        {
            Assembly assembly = null;
            try
            {
                assembly = Assembly.Load(assemblyName);
            }
            catch (FileNotFoundException fnf)
            {
                if (fnf.FileName != assemblyName) throw fnf;
            }
            catch (Exception)
            {
                throw;
            }
            if (assembly != null) return assembly;
            assembly = LoadAssemblyByFile(assemblyName);
            if (assembly != null) return assembly;
            return Assembly.LoadFrom(assemblyName);

        }

        public static List<string> AssemblySearchPaths = new List<string>();
        public static List<Assembly> AssembliesLoaded = new List<Assembly>();
        public static List<string> AssembliesLoading = new List<string>();
        //public static List<Type> TypesLoaded = new List<Type>();
        public static List<Type> TypesMethodsLoaded = new List<Type>();
        public static List<Type> TypesLoading = new List<Type>();

        public static bool ResolveAssembly(Assembly assembly)
        {
            string assemblyName = assembly.FullName;
            lock (AssembliesLoaded)
            {
                if (AssembliesLoading.Contains(assemblyName))
                {
                    return true;
                }
                AssembliesLoading.Add(assemblyName);
                LoadReferencedAssemblies(assembly);
                // push to the front
                AssembliesLoaded.Remove(assembly);
                AssembliesLoaded.Insert(0, assembly);
            }
            return true;
        }

        private static void LoadReferencedAssemblies(Assembly assembly)
        {
            foreach (var refed in assembly.GetReferencedAssemblies())
            {
                string assemblyName = refed.FullName;
                try
                {
                    Assembly assemblyLoad = Assembly.Load(refed);
                    ResolveAssembly(assemblyLoad);
                    continue;
                }
                catch (Exception e)
                {
                    var top = Path.Combine(Path.GetDirectoryName(assembly.Location), refed.Name);
                    try
                    {
                        Assembly assemblyLoad = Assembly.LoadFrom(top + ".dll");
                        ResolveAssembly(assemblyLoad);
                        continue;
                    }
                    catch (Exception)
                    {
                    }
                    try
                    {
                        Assembly assemblyLoad = Assembly.LoadFrom(top + ".exe");
                        ResolveAssembly(assemblyLoad);
                        continue;
                    }
                    catch (Exception)
                    {
                    }
                    Embedded.Warn("LoadReferencedAssemblies:{0} caused {1}", assemblyName, e);
                }
            }
        }

        [PrologVisible]
        public static void cliLoadAssemblyMethods(Assembly assembly, bool onlyAttributed, string requiredPrefix)
        {
            string assemblyName = assembly.FullName;
            try
            {
                foreach (Type t in assembly.GetTypes())
                {
                    try
                    {
                        AddForeignMethods(t, onlyAttributed, requiredPrefix);
                    }
                    catch (Exception te)
                    {
                        // Warn(e.Message);
                    }
                }
            }
            catch (Exception e)
            {
                // get types problem
                // Warn(e.Message);
            }
        }

        /// <summary>
        /// cliLoadAssembly('SwiPlCs.dll').
        /// cliLoadAssembly('Cogbot.exe').
        /// </summary>
        /// <param name="term1"></param>
        /// <returns></returns>
        public static bool cliLoadAssembly(PlTerm term1)
        {
            PingThreadFactories();
            try
            {
                return cliLoadAssemblyUncaught(term1);
            }
            catch (Exception e)
            {
                Embedded.Warn("cliLoadAssembly: {0} caused {1}", term1, e);
                return false;
            }
        }
        /// <summary>
        /// ?- cliAddAssemblySearchPath('.').
        /// </summary>
        /// <param name="term1"></param>
        /// <returns></returns>
        public static bool cliAddAssemblySearchPath(PlTerm term1)
        {
            PingThreadFactories();
            try
            {
                string name = AsString(term1);
                AddAssemblySearchPath(name);
            }
            catch (Exception e)
            {
                Embedded.Warn("cliAddAssemblySearchPath: {0} caused {1}", term1, e);
                return false;
            }
            return true;
        }

        public static bool AddAssemblySearchPath(String name)
        {

            lock (AssemblySearchPaths)
            {
                if (!AssemblySearchPaths.Contains(name))
                {
                    AssemblySearchPaths.Add(name);
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// cliRemoveAssemblySearchPath('.').
        /// </summary>
        /// <param name="term1"></param>
        /// <returns></returns>
        public static bool cliRemoveAssemblySearchPath(PlTerm term1)
        {
            PingThreadFactories();
            try
            {
                string name = AsString(term1);
                lock (AssemblySearchPaths)
                {
                    AssemblySearchPaths.Remove(name);
                }
            }
            catch (Exception e)
            {
                Embedded.Warn("cliRemoveAssemblySearchPath: {0} caused {1}", term1, e);
                return false;
            }
            return true;
        }

        private static string AsString(PlTerm term1)
        {
            if (term1.IsVar)
            {
                Embedded.Warn("AsString IsVar {0}", term1);
                return null;
            }
            if (IsTaggedObject(term1))
            {
                var obj = GetInstance(term1);
                if (obj != null) return "" + obj;
            }
            if (term1.IsCompound) return term1.Name;
            if (term1.IsString) return (string)term1;
            if (term1.IsAtomOrNil) return term1.Name;            
            return (string)term1;
        }

        public static bool cliLoadAssemblyUncaught(PlTerm term1)
        {
            try
            {
                if (term1.IsVar)
                {
                    Embedded.Warn("cliLoadAssembly IsVar {0}", term1);
                    return false;
                }
                if (IsTaggedObject(term1))
                {
                    var assemblyOrType = GetInstance(term1);
                    if (assemblyOrType is Assembly)
                    {
                        return ResolveAssembly((Assembly)assemblyOrType);
                    }
                    if (assemblyOrType is Type)
                    {
                        return ResolveAssembly(((Type)assemblyOrType).Assembly);
                    }
                    return Embedded.Warn("Cannot get assembly from {0} for {1}", assemblyOrType, term1);
                }
                string name = term1.Name;
                Assembly assembly = null;
                try
                {
                    assembly = AssemblyLoad(name);
                    if (assembly != null) return ResolveAssembly(assembly);
                }
                catch (Exception e)
                {
                    //Warn("Load: " + name + " caused " + e);
                }
                var fi = new FileInfo(name);
                if (fi.Exists)
                {
                    string fiFullName = fi.FullName;
                    try
                    {
                        assembly = Assembly.LoadFile(fiFullName);
                        if (assembly != null) return ResolveAssembly(assembly);
                    }
                    catch (Exception e)
                    {
                        Embedded.Warn("LoadFile: {0} caused {1}", fiFullName, e);
                    }
                }
                try
                {
                    if (assembly == null) assembly = Assembly.LoadWithPartialName(name);
                }
                catch (Exception e)
                {
                    Embedded.Warn("LoadWithPartialName: {0} caused {1}", name, e);
                }
                if (assembly == null) return Embedded.Warn("Cannot get assembly from {0} for {1}", name, term1);
                return ResolveAssembly(assembly);

            }
            catch (Exception exception)
            {
                throw ToPlException(exception);
            }
            return true;
        }


        public static IList<T> CopyOf<T>(List<T> list)
        {
            if (list == null) return new List<T>();
            lock (list)
            {
                return list.ToArray();
            }
        }
        public static IEnumerable<object> CopyOf<T>(ICollection list)
        {
            var copy = new List<object>();
            if (list == null) return copy;
            lock (list)
            {
                foreach (var o in copy)
                {
                    copy.Add(o);
                }
            }
            return copy;
        }

        public static IList<T> CopyOf<T>(IEnumerable<T> list)
        {
            var copy = new List<T>();
            if (list == null) return copy;
            lock (list)
            {
                copy.AddRange(list);
            }
            return copy;
        }

        public static IDictionary<K, V> CopyOf<K, V>(IDictionary<K, V> list)
        {
            var copy = new Dictionary<K, V>();
            if (list == null) return copy;
            lock (list)
            {
                foreach (var kv in list)
                {
                    copy.Add(kv.Key, kv.Value);
                }
            }
            return copy;
        }

        internal static bool BP()
        {
            return false;
        }
    }
}