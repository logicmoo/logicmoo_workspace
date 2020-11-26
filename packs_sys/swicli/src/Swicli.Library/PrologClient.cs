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

using System.Collections;

#if USE_IKVM
using org.jpl7;
using IKVM.Internal;
using ikvm.runtime;
using java.net;
using Iterator= java.util.Iterator;
using java.util;
//using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
using Class = java.lang.Class;
using sun.reflect.misc;
//usign Query = org.jpl7.Query;
#endif
using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Callback;
using Hashtable = java.util.Hashtable;
using PlTerm = SbsSW.SwiPlCs.PlTerm;
namespace Swicli.Library
{
    public partial class PrologCLR
    {
#if USE_IKVM

        internal static Term[] ToJPL(PlTermV args)
        {
            int UPPER = args.Size;
            Term[] target = new Term[UPPER];
            for (int i = 0; i < UPPER; i++)
            {
                target[i] = ToJPL(args[i]);
            }
            return target;
        }

        internal static org.jpl7.fli.term_t ToFLI(PlTermV args)
        {
            return ToFLI(args.A0);
        }

        internal static org.jpl7.fli.term_t ToFLI(PlTerm args)
        {
            return ToFLI(args.TermRef);
        }

        internal static PlTerm ToPLCS(Term args)
        {
            if (args is Atom) return new PlTerm(args.name());
            if (args is org.jpl7.Variable) return new PlTerm((uint)GetField(args, "term_"));
            if (args is org.jpl7.Float) return new PlTerm(args.doubleValue());
            if (args is org.jpl7.Integer) return new PlTerm(args.longValue());
            if (args is org.jpl7.Compound) return PlTerm.PlCompound(args.name(), ToPLCSV(args.args()));
            if (args is org.jpl7.JRef)
            {
                var jref = (org.jpl7.JRef)args;// return new PlTerm(args.doubleValue());
                var obj = jref.@ref();
                var t = new PlTerm();
                t.FromObject(obj);
                return t;
            }
            throw new ArgumentOutOfRangeException();
        }

        private static PlTermV ToPLCSV(Term[] terms)
        {
            int size = terms.Length;
            PlTermV target = NewPlTermV(size);
            for (int i = 0; i < size; i++)
            {
                target[i] = ToPLCS(terms[i]);
            }
            return target;
        }
#endif

        private static PlTermV ToPLCSV(PlTerm[] terms)
        {
            int size = terms.Length;
            PlTermV target = NewPlTermV(size);
            for (int i = 0; i < size; i++)
            {
                target[i] = terms[i];
            }
            return target;
        }

        private static PlTermV ToPLCSV1(PlTerm a0, PlTerm[] terms)
        {
            int size = terms.Length;
            PlTermV target = NewPlTermV(size + 1);
            int to = 1;
            target[0] = a0;
            for (int i = 0; i < size; i++)
            {
                target[to++] = terms[i];
            }
            return target;
        }

        private static object GetField(object term, string s)
        {
            return term.GetType().GetField(s, BindingFlagsALL).GetValue(term);
        }

#if USE_IKVM
        private static org.jpl7.fli.term_t ToFLI(uint hndle)
        {
            org.jpl7.fli.term_t t = new org.jpl7.fli.term_t();
            t.value = hndle;
            return t;
        }

        internal static Term ToJPL(PlTerm o)
        {
            switch (o.PlType)
            {
                case PlType.PlAtom:
                    {
                        return new Atom((string)o);
                    }
                    break;
                case PlType.PlInteger:
                    {
                        return new org.jpl7.Integer((long)o);
                    }
                    break;
                case PlType.PlFloat:
                    {
                        return new org.jpl7.Float((double)o);
                    }
                    break;
                case PlType.PlString:
                    {
                        return new org.jpl7.Atom((string)o);
                    }
                    break;
                case PlType.PlTerm:
                case PlType.PlListPair:
                    {
                        var a = o.Arity;
                        Term[] termArray = new Term[a];
                        var c = new org.jpl7.Compound(o.Name, termArray);
                        for (int i = 1; i <= a; i++)
                        {
                            c.setArg(i, ToJPL(o[i]));
                        }
                        return c;
                    }
                    break;
                case PlType.PlVariable:
                    {
                        var v = new org.jpl7.Variable();
                        SetField(v, "term_", o.TermRef);
                        return v;
                    }
                    break;
                case PlType.PlUnknown:
                    {
                        return org.jpl7.Util.textToTerm((string)o);
                    }
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

#endif
        private static void SetField(object target, string name, object value)
        {
            FieldInfo field = target.GetType().GetField(name, BindingFlagsALL);
            //if (!field.IsPublic) field..IsPublic = true;
            field.SetValue(field.IsStatic ? null : target, value);
        }

#if USE_IKVM
        public static org.jpl7.Term InModule(string s, org.jpl7.Term o)
        {
            if (s == null || s == "" || s == "user") return o;
            return new org.jpl7.Compound(":", new Term[] { new org.jpl7.Atom(s), o });
        }
#endif
        ///<summary>
        ///</summary>
        ///<exception cref="NotImplementedException"></exception>
        public void Dispose()
        {

        }

        protected PlTerm ThisClientTerm
        {
            get
            {
                return ToProlog(this);
            }
        }

        public static PlTerm ToProlog(object value)
        {
            PlTerm t = PlTerm.PlVar();
            t.FromObject(value);
            return t;
        }
        private static PlTerm ToPlList(PlTerm[] terms)
        {
            int termLen = terms.Length;
            if (termLen == 0) return ATOM_NIL;
            termLen--;
            PlTerm ret = listOfOne(terms[termLen]);
            while (--termLen >= 0)
            {
                ret = PlTerm.PlList(terms[termLen], ret);
            }
            return ret;
        }
        private static PlTerm ToPlArray(PlTerm[] terms)
        {
            int termLen = terms.Length;
            if (termLen == 0) return ATOM_NIL;
            return PlTerm.PlCompound("[]", terms);
        }

        private static PlTermV ToPlTermV(PlTerm[] terms)
        {
            var tv = NewPlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = terms[i];
            }
            return tv;
        }

        private static PlTermV NewPlTermV(int length)
        {
            return new PlTermV(length);
        }

        private static PlTermV ToPlTermVParams(ParameterInfo[] terms)
        {
            var tv = NewPlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = typeToSpec(terms[i].ParameterType);
            }
            return tv;
        }
        private static PlTerm ToPlListParams(ParameterInfo[] terms)
        {
            PlTerm listOf = ATOM_NIL;
            for (int i = terms.Length - 1; i >= 0; i--)
            {
                PlTerm term = typeToSpec(terms[i].ParameterType);
                listOf = PlTerm.PlCompound(".", term, listOf);
            }
            return listOf;
        }
        private static PlTerm ToPlListTypes(Type[] terms)
        {
            PlTerm listOf = ATOM_NIL;
            for (int i = terms.Length - 1; i >= 0; i--)
            {
                PlTerm term = typeToSpec(terms[i]);
                listOf = PlTerm.PlCompound(".", term, listOf);
            }
            return listOf;
        }
        private static PlTermV ToPlTermVSpecs(Type[] terms)
        {
            var tv = NewPlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = typeToSpec(terms[i]);
            }
            return tv;
        }

        private static PlTerm listOfOne(PlTerm term)
        {
            return PlTerm.PlCompound(".", term, ATOM_NIL);
        }


        public static bool IsDefined(string module, string functor, int arity)
        {
            if (!ClientReady)
            {
                return false;
            }
            return InvokeFromC(() => PlQuery.PlCall(null, "predicate_property",
                                                    new PlTermV(
                                                        ModuleTerm(module,
                                                                   FunctorTerm(functor, arity)),
                                                        PlTerm.PlVar())), true);
        }

        private static PlTerm ModuleTerm(string module, PlTerm term)
        {
            if (module == null) return term;
            if (term.IsCompound && term.Arity == 2 && term.Name == ":") return term;
            return PlC(":", new[] {PlTerm.PlAtom(module), term});
        }

        private static PlTerm FunctorTerm(string functor, int arity)
        {
            return PlTerm.PlCompound(functor, new PlTermV(arity));
        }

        public static object CallProlog(object target, string module, string name, int arity, object origin, object[] paramz, Type returnType, bool discard)
        {
            if (!ClientReady)
            {
                return null;
            }
            return InvokeFromC(() =>
            {

                PlTermV args = NewPlTermV(arity);
                int fillAt = 0;
                if (origin != null)
                {
                    args[fillAt++].FromObject(origin);
                }
                for (int i = 0; i < paramz.Length; i++)
                {
                    args[fillAt++].FromObject(paramz[i]);
                }
                bool IsVoid = returnType == typeof(void);
                if (!IsVoid)
                {
                    //args[fillAt] = PlTerm.PlVar();
                }
                if (!PlQuery.PlCall(module, name, args))
                {
                    if (!IsVoid) Embedded.Warn("Failed Event Handler {0} failed", target);
                }
                if (IsVoid) return null;
                object ret = CastTerm(args[fillAt], returnType);
                return ret;
            }, discard);
        }

        private bool ModuleCall0(string s, PlTermV termV)
        {
            return PlQuery.PlCall(ClientModule, ClientPrefix + s, termV);
        }

        private bool ModuleCall(string s, params PlTerm[] terms)
        {
            return PlQuery.PlCall(ClientModule, ClientPrefix + s, ToPLCSV1(ThisClientTerm, terms));
        }
        public static PlTerm PlNamed(string name)
        {
            return PlTerm.PlAtom(name);
        }

        public object Eval(object obj)
        {
            PlTerm termin = PlTerm.PlVar();
            if (obj is PlTerm)
            {
                termin.Unify((PlTerm)obj);
            }
            else
            {
                termin.FromObject(obj);
            }
            PlTerm termout = PlTerm.PlVar();
            if (!ModuleCall("Eval", termin, termout)) return null;
            return CastTerm(termout, typeof(Object));
        }

        public void Intern(string varname, object value)
        {
            PlTerm termin = PlTerm.PlVar();
            termin.FromObject(value);
            ModuleCall("Intern", PlNamed(varname), termin);
        }


        public bool IsDefined(string name)
        {
            return ModuleCall("IsDefined", PlNamed(name));
        }

        public object GetSymbol(string name)
        {
            PlTerm termout = PlTerm.PlVar();
            if (!ModuleCall("GetSymbol", PlNamed(name), termout)) return null;
            return CastTerm(termout, typeof(Object));
        }

        public object Read(string line, TextWriter @delegate)
        {
            return new Nullable<PlTerm>(PlTerm.PlCompound(line));
        }

        public static bool PlCall(string s)
        {
            try
            {
                if (!Embedded.JplDisabled)
                {
                    return DoQuery(s);
                }
                if (Embedded.PlCsDisabled)
                {
                    WriteDebug("Disabled PlCall " + s);
                    return false;
                }
                return PlQuery.PlCall(s);
            }
            catch (Exception e)
            {
                Embedded.WriteException(e);
                throw e;
            }
        }

        public static bool PlCall(string m, string f, PlTermV args)
        {
            try
            {
                if (!Embedded.JplDisabled)
                {
#if USE_IKVM
                    return DoQuery(m, f, args);
#endif
                }
                if (Embedded.PlCsDisabled)
                {
                    WriteDebug("Disabled PlCall " + f);
                    return false;
                }
                return PlQuery.PlCall(m, f, args);
            }
            catch (Exception e)
            {
                Embedded.WriteException(e);
                throw e;
            }
        }

        public static bool DoQuery(string query)
        {
            if (Embedded.JplDisabled) return PlCall(query);
#if USE_IKVM
            Query q;
            try
            {
                q = new Query(query);
            }
            catch (Exception e)
            {
                Embedded.WriteException(e);
                return false;
            }
            return DoQuery(q);
#else
            return PlCall(query);
#endif
        }

#if USE_IKVM
        public static bool DoQuery(string m, string f, PlTermV args)
        {
            if (Embedded.JplDisabled) return PlCall(m, f, args);
            Query q;
            try
            {
                q = new Query(InModule(m, new Compound(f, ToJPL(args))));
            }
            catch (Exception e)
            {
                Embedded.WriteException(e);
                return false;
            }
            return DoQuery(q);
        }

        private static bool DoQuery(Query query)
        {
            try
            {
                bool any = false;
                //if (!query.isOpen()) query.open();
                while (query.hasMoreSolutions())
                {
                    any = true;
                    var ht = query.nextSolution();
                    foreach (var list in ht.entrySet().toArray())
                    {
                        string s = "" + list;
                        ConsoleTrace(s);
                    }
                }
                return any;
            }
            catch (Exception exception)
            {
                Embedded.WriteException(exception);
                return false;
            }

        }
#endif

#if USE_IKVM
        private static IEnumerable ToEnumer(java.util.Enumeration enumeration)
        {
            List<object> list = new List<object>();
            while (enumeration.hasMoreElements())
            {
                list.Add(enumeration.nextElement());
            }
            return list;
        }
        private static IEnumerable ToEnumer(java.util.Iterator enumeration)
        {
            List<object> list = new List<object>();
            while (enumeration.hasNext())
            {
                list.Add(enumeration.next());
            }
            return list;
        }
#endif
        //[TestMethod]
        public void StreamRead()
        {
            PlEngine.SetStreamReader(Sread);
            // NOTE: read/1 needs a dot ('.') at the end
            PlQuery.PlCall("assert( (test_read(A) :- read(A)) )");
            PlTerm t = PlQuery.PlCallQuery("test_read(A)");
            //     Assert.AreEqual(ref_string_read, t.ToString() + ".");
        }


        public static string ToCSString(PlTermV termV)
        {
            int s = termV.Size;

            //var a0= termV.A0;
            PlTerm v0 = termV[0];
            PlTerm v1 = termV[1];
            PlQuery.PlCall("write", new PlTermV(v0));
            PlQuery.PlCall("nl");
            PlQuery.PlCall("writeq", new PlTermV(v1));
            PlQuery.PlCall("nl");
            return "";
        }

        public static void PlAssert(string s)
        {
            if (Embedded.PlCsDisabled)
            {
                WriteDebug("Disabled PlAssert " + s);
                return;
            }
            PlQuery.PlCall("assert((" + s + "))");
        }

        private static void WriteDebug(string s)
        {
            ConsoleTrace(s);
        }


        public bool Consult(string filename)
        {
            // atomic quote the filename
            string replace = "'" + filename.Replace("\\", "/").Replace("'", "\\'") + "'";
            return PlCall("[" + replace + "]");
        }


        public bool ConsultIfExists(string file)
        {
            return InvokeFromC(() => File.Exists(file) && Consult(file), false);
        }
    }
}