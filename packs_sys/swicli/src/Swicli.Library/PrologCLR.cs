/*  $Id$
*  
*  Project: Swicli.Library - Two Way Interface for .NET and MONO to SWI-Prolog
*  Author:        Douglas R. Miles
*  E-mail:        logicmoo@gmail.com
*  WWW:           http://www.logicmoo.org
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
using JavaClass = java.lang.Class;
#endif
using System;
using System.Collections;
using System.Collections.Generic;
//using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using SbsSW.SwiPlCs;
using PlTerm = SbsSW.SwiPlCs.PlTerm;

namespace Swicli.Library
{
    public partial class PrologCLR
    {
        protected string ClientPrefix { get; set; }
        private string _clientModule = null;
        protected string ClientModule
        {
            get { return _clientModule; }
            set { if (value != "user") _clientModule = value; }
        }

        private static PrologCLR _singleInstance;
        public static PrologCLR SingleInstance
        {
            get
            {
                if (_singleInstance == null) _singleInstance = new PrologCLR();
                return _singleInstance;
            }
        }

        public PrologCLR()
        {
            _singleInstance = this;
            ClientModule = null;
            ClientPrefix = "cli_";
            PrologCLR.SetupProlog();
        }

        public readonly static Type[] ZERO_TYPES = new Type[0];

        public readonly static Object[] ZERO_OBJECTS = new Object[0];

        public static readonly Type[] ONE_STRING = new[] {typeof (string)};

        public static BindingFlags BindingFlagsJustStatic = BindingFlags.Public | BindingFlags.NonPublic |
                                                            BindingFlags.Static | BindingFlags.FlattenHierarchy;
        public static BindingFlags BindingFlagsInstance = BindingFlags.Public | BindingFlags.NonPublic |
                                                            BindingFlags.Instance | BindingFlags.FlattenHierarchy;

        public static BindingFlags BindingFlagsALL = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static |
                                                     BindingFlags.Instance | BindingFlags.IgnoreCase | BindingFlags.IgnoreReturn
                                                     | BindingFlags.FlattenHierarchy;
       
        public static BindingFlags InstanceFields = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic |
                                                    BindingFlags.FlattenHierarchy;

        public static BindingFlags BindingFlagsALLNC = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static |
                                                     BindingFlags.Instance | BindingFlags.IgnoreReturn
                                                     | BindingFlags.FlattenHierarchy;

        public static BindingFlags BindingFlagsALL3 = BindingFlags.InvokeMethod | BindingFlags.GetField |
                                                      BindingFlags.GetProperty | BindingFlags.SetField |
                                                      BindingFlags.SetProperty;
        public static BindingFlags ICASE = BindingFlags.IgnoreCase;

        private static readonly BindingFlags[] BindingFlags_SEARCHIS = new[]
                                                                {
                                                                    BindingFlagsInstance,
                                                                    BindingFlagsJustStatic,
                                                                    BindingFlagsInstance | ICASE,
                                                                    BindingFlagsJustStatic | ICASE,
                                                                };
        private static readonly BindingFlags[] BindingFlags_SEARCHS = new[]
                                                                {
                                                                    BindingFlagsALL3 | BindingFlagsJustStatic,
                                                                    BindingFlagsALL3 | BindingFlagsJustStatic | ICASE,
                                                                };
        
        [PrologVisible]
        public static bool cliThrow(PlTerm ex)
        {
            throw (Exception) CastTerm(ex, typeof (Exception));
        }
        [PrologVisible]
        public static bool cliBreak(PlTerm ex)
        {
            Trace();
            return Embedded.WarnMissing(ToString(ex)) || true;
        }
        private static void Trace()
        {
            //throw new NotImplementedException();
        }

        private static object ToFort(object o)
        {
            return ToProlog(o);
        }

        public static int PlSucceedOrFail(bool p)
        {
            return p ? libpl.PL_succeed : libpl.PL_fail;
        }
        public static bool PlSucceedOrFailOrError(int p)
        {
            return p != libpl.PL_fail;
        }

        private static string ToString(object o)
        {
            try
            {
                return ToString0(o);
            }
            catch (Exception)
            {
                return "" + o;
            }
        }
        private static string ToString0(object o)
        {
            if (o == null) return "null";
            if (o is IConvertible || o is PlTerm || o is ValueType) return o.ToString();
            if (o is IEnumerable)
            {
                var oc = (IEnumerable)o;
                int count = 0;
                string ret = "[";
                foreach (var o1 in oc)
                {
                    if (count > 1) ret += ",";
                    count++;
                    ret += ToString0(o1);
                }
                return ret + "]";
            }
            return o.ToString();
        }
        /// <summary>
        /// 1 ?- cliToString(-1,X).
        /// X = "4294967295".
        /// </summary>
        /// <param name="obj"></param>
        /// <param name="str"></param>
        /// <returns></returns>
        [PrologVisible]
        public static bool cliToStrRaw(PlTerm obj, PlTerm str)
        {
            try
            {
                if (!str.IsVar)
                {
                    var plvar = PlTerm.PlVar();
                    return cliToStrRaw(obj, plvar) && SpecialUnify(str, plvar);
                }
                if (obj.IsString) return str.Unify(obj);
                if (obj.IsVar) return str.Unify((string)obj);
                object o = GetInstance(obj);
                if (o == null) return str.FromObject("" + obj);
                return str.FromObject(ToString(o));
            }
            catch (Exception e)
            {
                Embedded.Warn("cliToString: {0}", e);
                object o = GetInstance(obj);
                if (o == null) return str.FromObject("" + obj);
                return str.FromObject(ToString(o));
            }
        }
        [IKVMBased]
        [PrologVisible]
        static public bool cliJavaToString(PlTerm paramIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliJavaToString(paramIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            object getInstance = GetInstance(paramIn);
            if (getInstance == null) return valueOut.Unify(PlTerm.PlString("null"));
#if USE_IKVM
            //object val = getInstance as java.lang.Object;
         /*   if (val == null)
            {
                JClass c = ikvm.runtime.Util.getClassFromObject(getInstance);
                string s = (string)c.getMethod("toString", new JClass[0]).invoke(getInstance, ZERO_OBJECTS);
                return valueOut.Unify(PlTerm.PlString(s));
            }*/
            return valueOut.Unify(PlTerm.PlString(ikvm.extensions.ExtensionMethods.instancehelper_toString(getInstance)));
#else
            object val = getInstance;
            return valueOut.Unify(PlTerm.PlString(val.ToString()));
#endif
        }

        private static bool CheckBound(params PlTerm[] terms)
        {
            foreach (PlTerm term in terms)
            {
                if (term.IsVar)
                {
                    return Embedded.Error("Is var {0}", term);
                }
            }
            return true;
        }

        private static bool IsCompatTypes(Type[] supplied, Type[] required)
        {
            int len = supplied.Length;
            if (required.Length != len) return false;
            int considered = 0;
            foreach (Type type in required)
            {
                Type consider = supplied[considered];
                if (!IsCompatType(consider,type))
                {                    
                    return false;
                }
                considered++;               
            }
            return true;
        }

        private static bool IsCompatType(Type consider, Type type)
        {
            if (consider == null || type == null) return true;
            if (consider == typeof(object) || type == typeof(object)) return true;
            if (type.IsAssignableFrom(consider)) return true;
            if (typeof(IConvertible).IsAssignableFrom(type)
                && typeof(IConvertible).IsAssignableFrom(consider))
                return true;
            return false;
        }


        [PrologVisible]
        static public bool cliLockEnter(PlTerm lockObj)
        {
            object getInstance = GetInstance(lockObj);
            Monitor.Enter(getInstance);
            return true;
        }
        [PrologVisible]
        static public bool cliLockExit(PlTerm lockObj)
        {
            object getInstance = GetInstance(lockObj);
            Monitor.Exit(getInstance);
            return true;
        }

        private static bool GetInstanceAndType(PlTerm clazzOrInstance, out object getInstance, out Type c)
        {
            if (clazzOrInstance.IsVar)
            {
                c = null;
                getInstance = null;
                return Embedded.Error("Cant find instance {0}", clazzOrInstance);
            }
            getInstance = GetInstance(clazzOrInstance);
            c = GetTypeFromInstance(getInstance, clazzOrInstance);
            if (getInstance == null && c == null)
            {
                return Embedded.Error("Cant find instance or type {0}", clazzOrInstance);
            }
            return true;
        }

    }
}