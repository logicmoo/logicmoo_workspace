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
#if (!STANDARD)
#define debugging
#define arg1index
#define mswindows
#define newor
#define partialengine
#endif

#if (!VISUAL_STUDIO)
#undef mswindows
#endif

#define debugging
/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2010 Douglas R. miles

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU General Public License as published by the Free Software Foundation; either version
  2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for details, or enter 'license.' at the command prompt.

-------------------------------------------------------------------------------------------*/

using System.Runtime.InteropServices;
using System.Threading;
#if USE_IKVM
using JavaClass = java.lang.Class;
using Type = System.Type;
using Object = System.Object;
#else
using JClass = System.Type;
#endif
using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using Exception = System.Exception;
//SbsSW.SwiPlCs.PrologEngine;
using Term = SbsSW.SwiPlCs.PlTerm;
public class A
{
    static string[] DoIt(string foo)
    {
        return new string[] {foo, foo};
    }
}
namespace Swicli.Library
{
    /*
      ----------------
      Prolog extensions
      ----------------
    */

#if partialengine
    public partial class PrologEngine
#else        
    public class Ext
#endif
    {
        private static readonly Dictionary<JavaClass, Func<Term, /*CONTEXT,*/ Object>> TermTypeConvertor =
            new Dictionary<JavaClass, Func<Term,  /*CONTEXT,*/ object>>();

        static Binder defaultBinder = System.Type.DefaultBinder;
        static PrologBinder prologBinder = new PrologBinder();

        private static PrologEngine engine;
        public Object[] EMPTY_VECTOR = new Object[0];
#if !(partialengine)
        private static VarStack varStack
        {
            get { return engine.varStack; }
        }
#endif

#if partialengine
        public void InitEngine(PrologEngine e)
#else        
   public Ext(PrologEngine e)
#endif
        {
            engine = e;

            MethodInfo[] getTypeGetMethods =
                GetType().GetMethods(BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Instance |
                                     BindingFlags.Static);
            foreach (var func in getTypeGetMethods)
            {
                var ps = func.GetParameters();
                if (ps.Length == 1 && ps[0].ParameterType == typeof (Term))
                {
                    Type res = func.ReturnType;
                    if (res != typeof (void))
                    {
                        MethodInfo info = func;
                        TermTypeConvertor[res] =
                            new Func<Term, /*CONTEXT,*/ object>(
                                (term /*ctx*/) =>
                                info.Invoke(info.IsStatic ? null : this, new object[] {term /*ctx*/}));
                    }
                }
            }
        }


        [TermConversion]
        static public Type ResolveToType(Term t0)
        {
            return PrologCLR.GetTypeThrowIfMissing(t0);
#if false
            t0 = t0.Value;
            object obj = t0._functor;
            if (t0.IsObjectRef)
            {
                if (obj != null)
                {
                    return obj.GetType();
                }
                return typeof(object);
            }
            if (t0.IsObject)
            {
                if (obj != null)
                {
                    return obj.GetType();
                }
            }

            string typeName = t0.Name;
            if (typeName == "static")
            {
                return ResolveToType(t0.Arg(0));
            }
            Type type = ResolveType(typeName);
            if (t0.IsCompound)
            {
                int i = 0;
                Type[] args = new Type[t0.Arity];
                foreach (var o in t0.Args)
                {
                    args[i++] = ResolveToType(o);
                }
                Type type0 = type.MakeGenericType(args);
                if (type0 != null) type = type0;
            }
            return type;
#endif
        }


        private static Type ResolveType0(string typeName)
        {
            Type type = Type.GetType(typeName, false, false) ?? Type.GetType(typeName, false, true);
            if (type == null)
            {
                Type obj = null;
                JavaClass jc = null;
                
                try
                {
#if USE_IKVM
                    jc = JavaClass.forName(typeName);
#endif
                }
                catch (Exception e)
                {
                }
                if (jc != null)
                {
#if USE_IKVM
                    type = ikvm.runtime.Util.getInstanceTypeFromClass(jc);
#endif
                }
                if (type == null && !Embedded.IsLinux)
                {
                    type = Type.GetTypeFromProgID(typeName);
                }
                if (type == null)
                {
                    type = PrologCLR.GetPrimitiveType(typeName);
                }               
                if (type == null)
                {
                    try
                    {
                        type = Type.GetTypeFromCLSID(new Guid(typeName));
                    }
                    catch (FormatException)
                    {
                    }
                }
            }
            return type;
        }

        private static Type ResolveType(string typeName)
        {
            Type type = ResolveType0(typeName);
            if (type == null)
            {
                int len = typeName.Length;
                if (typeName[0] == '\'')
                {
                    return ResolveType(typeName.Substring(1, len - 2));
                }
            }
            if (type == null)
            {
                if (!typeName.Contains("."))
                {
                    foreach (string s in nameSpaceSearch)
                    {
                        type = ResolveType0(s + "." + typeName);
                        if (type != null) return type;
                    }
                }
            }
            return type;
        }

        private static ICollection<string> nameSpaceSearch = new string[]
                                                                 {
                                                                     "System",
                                                                     "java.lang",
                                                                     "System.Collections",
                                                                     "java.util",
                                                                 };

        public bool JCALL0(Term term, PrologCLR engine)
        {
            throw new NotImplementedException("JCALL0");
            return JCALL0(term.Name, term.Args/*ctx*/);
        }
        public bool JCALL0(string functor, Term[] args/*CONTEXT*/)
        {
            throw new NotImplementedException("JCALL0/2");
#if false
            Term t0, t1;
            Object o = null;
            Type t = null;
           // TermSet ts;
            int arity = args.Length;
            t0 = args[0].Value;


            o = ObjectFromTerm(t0/*ctx*/);
            if (t0.Name == "static")
            {
                t0 = t0.Value.Arg(0).Value;
                t = ResolveToType(t0);
                o = null;
            }
            else if (t0.IsObjectRef)
            {
                if (o != null) t = o.GetType();
            }
            t1 = args[1];

            string mname = t1.Name;

            if (arity == 3)
            {
                object r = JCALL0(t, o, mname, null, t1.Args/*ctx*/);
                return ObjectUnify(r, args[2]/*ctx*/);
            }
            else if (arity == 4)
            {
                object r = JCALL0(t, o, mname, t1.Args, toTermList(args[2])/*ctx*/);
                return ObjectUnify(r, args[3]/*ctx*/);
            }
#endif
            return false;
        }

        [TermConversion]
        static public Term[] toTermListTerm(Term t)
        {
            if (t.Arity == 2 && t.IsList)
            {
                var terms = new System.Collections.Generic.List<Term>();
                while (t.Arity == 2 && t.IsList)
                {
                    terms.Add(t.Arg(0));
                    t = t.Arg(1);
                }
                return terms.ToArray();
            }
            if (t.IsNil)
            {
                return new Term[0];
            }
            return new Term[] { t };
        }


        [TermConversion]
        public ICollection CollectionFromTerm(Term term/*CONTEXT*/)
        {
#if false
            if (term.IsObjectRef)
            {
                return (ICollection)term.ExprValue.AsObject;
            }
#endif
            if (term.IsString)
            {
                return ((string) term).ToCharArray();
            }
            if (term.IsList)
            {
                return terms_to_objects(toTermListTerm(term)/*ctx*/);
            }
            if (term.IsAtom)
            {
                return (ICollection)AtomAsObject(term.Name);
            }
            if (term.IsCompound)
            {
                return (ICollection)CompoundToObject(term.Name, term.Args/*ctx*/);
            }
#if false
            if (term.IsNumber)
            {
                return (ICollection)term.ExprValue.AsObject;
            }
            if (false)
            {
                if (term.IsBool)
                {
                    //  return term.ExprValue.AsBool;
                }
                if (term.IsTimeSpan)
                {
                    //  return term.ExprValue.AsTimeSpan;
                }
                if (term.IsDateTime)
                {
                    //  return term.ExprValue.AsDateTime;
                }
            }
            if (term.IsObject)
            {
                return (ICollection)term.ExprValue.AsObject;
            }
#endif
            return (ICollection)ObjectFromTerm(term/*ctx*/);
        }
        private object CompoundToObject(string value, Term[] args/*CONTEXT*/)
        {
            int arity = args.Length;
            if (arity == 1)
            {
                if (value == "{}")
                {
                    return args[0];
                }
                if (value == "@")
                {
                    return ObjectFromTerm(args[0]/*ctx*/);
                }
                if (value == "typeof")
                {
                    return ResolveToType(args[0]);
                }
                if (value == "static")
                {
                    return ResolveToType(args[0]);
                }
            }
            // object[] array
            if (value == "[]")
            {
                return terms_to_objects(args/*ctx*/);
            }
            if (value == "array")
            {
                Type t = ResolveToType(args[0]);
                var tas = terms_to_objects(vector_rest(args)/*ctx*/);

            }
            if (value == "jcall")
            {
                return JCALL0(value, args/*ctx*/);
            }
            if (value == "new")
            {
                Type t = ResolveToType(args[0]);
                return JNEW(t, vector_rest(args)/*ctx*/);
            }
            if (arity > 1) return ObjectFromTerm(args[2]/*ctx*/);
            return ObjectFromTerm(args[1]/*ctx*/);
        }

        private static object AtomAsObject(string value)
        {
            int len = value.Length;
            switch (len)
            {
                case 1:
                    return value[0];
                case 6:
                    switch (value)
                    {
                        case "@false":
                            return false;
                        default:
                            break;
                    }
                    break;
                case 5:
                    switch (value)
                    {
                        case "@true":
                            return true;
                        case "@void":
                            return null;
                        case "@null":
                            return null;
                        default:
                            break;
                    }
                    break;
                default:
                    break;
            }
            return value;
        }

        [TermConversion]
        static public object ObjectFromTerm(Term term/*CONTEXT*/)
        {
#if false
            if (term.IsObjectRef)
            {
                return term.ExprValue.AsObject;
            }
#endif
            if (term.IsInteger)
            {
                return term.longValue();
            }
            if (term.IsAtomOrString)
            {
                return AtomAsObject(term.Name);
            }
            if (term.IsCompound)
            {
                int arity = term.Arity;
                if (arity == 1)
                {
                    string value = term.Name;
                    if (value == "{}")
                    {
                        return term.Arg(0);
                    }
                    if (value == "@")
                    {
                        return ObjectFromTerm(term.Arg(0)/*ctx*/);
                    }
                    if (value == "typeof")
                    {
                        return ResolveToType(term.Arg(0));
                    }
                    if (value == "static")
                    {
                        return ResolveToType(term.Arg(0));
                    }
                }
            }
#if false
            if (term.IsBool)
            {
                return term.ExprValue.AsBool;
            }
            if (term.IsTimeSpan)
            {
                return term.ExprValue.AsTimeSpan;
            }
            if (term.IsDateTime)
            {
                return term.ExprValue.AsDateTime;
            }
            if (term.IsObject)
            {
                return term.ExprValue.AsObject;
            }
#endif
            return PrologCLR.GetInstance( term);
        }

        static public Term ToTermList(IEnumerable ie)
        {
            if (ie is IList) return ToTermIList((IList)ie);
            if (ie is ICollection) return ToTermListICol((ICollection)ie);
            var lst = new System.Collections.Generic.List<object>();
            foreach (var e in ie)
            {
                lst.Add(e);
            }
            return ToTermIList(lst);
        }

        static public Term ToTermListICol(ICollection ie)
        {
            if (ie is IList) return ToTermIList((IList)ie);
            int cnt = ie.Count;
            var array = new object[cnt];
            ie.CopyTo(array, 0);
            return ToTermIList(array);
        }

        static public Term ToTermIList(IList l)
        {
            Term n = Term.PlAtom("[]");// NULLLIST;
            int i = l.Count;
            while (i-- > 0)
            {
                n = Term.PlCompound(".", ObjectToTerm(l[i]), n);
            }
            return n;
        }

        public bool ObjectUnify(object r, Term t2/*CONTEXT*/)
        {
            return PrologCLR.UnifyToProlog(r, t2) != 0;
        }

        static public Term ObjectToTerm(object o)
        {
            return PrologCLR.ToProlog(o);
        }

        static public Object GetDefaultIndexedProperty(Object target, Object[] args)
        {
            Type targetType = target.GetType();
            return targetType.InvokeMember("", BindingFlags.Default | BindingFlags.GetProperty, null,
                                                     target, args);
        }

        public Object SetDefaultIndexedProperty(Object target, Object[] args)
        {
            Type targetType = target.GetType();
            return targetType.InvokeMember("", BindingFlags.Default | BindingFlags.SetProperty, null,
                                                     target, args);
        }

        public bool JPRED0(Term term, PrologCLR engine)
        {
            return true;
        }

        public object JNEW(Type clz, Term[] termArray/*CONTEXT*/)
        {
            Type type = clz;
            //object[] termArray = toTermList(lis);
            Exception lastException = null;
            object[] argarray = terms_to_objects(termArray/*ctx*/);
            Type[] argtypes = Type.GetTypeArray(argarray);
            ConstructorInfo[] typeGetConstructors = ((_Type)(object)type).GetConstructors();
            foreach (var search in typeGetConstructors)
            {
                try
                {
                    ParameterInfo[] paramInfos = search.GetParameters();
                    if (argarray.Length == paramInfos.Length)
                    {
                        object[] parray;
                        if (Coerce(termArray, argarray, argtypes, paramInfos, out parray/*ctx*/))
                        {

                            argarray = parray;
                            return search.Invoke(parray);
                        }
                        else
                        {
                            continue;
                        }
                    }
                }
                catch (Exception e)
                {
                    lastException = e;
                }
            }
            if (lastException != null) throw lastException;
            throw new Exception("Can't find matching costructor for: " + type +
                                " taking those arguments " + argtypes.Length);
        }


        public object JCALL0(Type type, object objectOrNull, string name, Term[] typeHints, Term[] termArray/*CONTEXT*/)
        {
            //
            // Summary:
            //     Invokes the specified member, using the specified binding constraints and
            //     matching the specified argument list.
            //
            // Parameters:
            //   name:
            //     The System.String containing the name of the constructor, method, property,
            //     or field member to invoke.-or- An empty string ("") to invoke the default
            //     member. -or-For IDispatch members, a string representing the DispID, for
            //     example "[DispID=3]".
            //
            //   invokeAttr:
            //     A bitmask comprised of one or more System.Reflection.BindingFlags that specify
            //     how the search is conducted. The access can be one of the BindingFlags such
            //     as Public, NonPublic, Private, InvokeMethod, GetField, and so on. The type
            //     of lookup need not be specified. If the type of lookup is omitted, BindingFlags.Public
            //     | BindingFlags.Instance | BindingFlags.Static are used.
            //
            //   binder:
            //     A System.Reflection.Binder object that defines a set of properties and enables
            //     binding, which can involve selection of an overloaded method, coercion of
            //     argument types, and invocation of a member through reflection.-or- null,
            //     to use the System.Type.DefaultBinder. Note that explicitly defining a System.Reflection.Binder
            //     object may be requird for successfully invoking method overloads with variable
            //     arguments.
            //
            //   target:
            //     The System.Object on which to invoke the specified member.
            //
            //   args:
            //     An array containing the arguments to pass to the member to invoke.
            //
            // Returns:
            //     An System.Object representing the return value of the invoked member.
            //
            // Exceptions:
            //   System.ArgumentNullException:
            //     invokeAttr contains CreateInstance and typeName is null.
            //
            //   System.ArgumentException:
            //     args is multidimensional.-or- invokeAttr is not a valid System.Reflection.BindingFlags
            //     attribute.-or- invokeAttr contains CreateInstance combined with InvokeMethod,
            //     GetField, SetField, GetProperty, or SetProperty.-or- invokeAttr contains
            //     both GetField and SetField.-or- invokeAttr contains both GetProperty and
            //     SetProperty.-or- invokeAttr contains InvokeMethod combined with SetField
            //     or SetProperty.-or- invokeAttr contains SetField and args has more than one
            //     element.-or- This method is called on a COM object and one of the following
            //     binding flags was not passed in: BindingFlags.InvokeMethod, BindingFlags.GetProperty,
            //     BindingFlags.SetProperty, BindingFlags.PutDispProperty, or BindingFlags.PutRefDispProperty.-or-
            //     One of the named parameter arrays contains a string that is null.
            //
            //   System.MethodAccessException:
            //     The specified member is a class initializer.
            //
            //   System.MissingFieldException:
            //     The field or property cannot be found.
            //
            //   System.MissingMethodException:
            //     The method cannot be found.-or- The current System.Type object represents
            //     a type that contains open type parameters, that is, System.Type.ContainsGenericParameters
            //     returns true.
            //
            //   System.Reflection.TargetException:
            //     The specified member cannot be invoked on target.
            //
            //   System.Reflection.AmbiguousMatchException:
            //     More than one method matches the binding criteria.
            //
            //   System.NotSupportedException:
            //     The .NET Compact Framework does not currently support this method.
            //
            //   System.InvalidOperationException:
            //     The method represented by name has one or more unspecified generic type parameters.
            //     That is, the method's System.Reflection.MethodInfo.ContainsGenericParameters
            //     property returns true.
            Object target = objectOrNull;
            type = type ?? objectOrNull.GetType();
            //MethodInfo mi = type.GetMethods(methodName);

            bool isStatic = objectOrNull == null;

            MethodInfo mi = null;
            try
            {
                mi = type.GetMethod(name);
            }
            catch (Exception)
            {
            }
            if (mi != null) isStatic = mi.IsStatic;

            BindingFlags flags = BindingFlags.Public | BindingFlags.NonPublic;
            if (objectOrNull == null) flags |= BindingFlags.Static;
            flags |= BindingFlags.Instance;
            object[] argarray = terms_to_objects(termArray/*ctx*/);
            if (argarray == null) argarray = this.EMPTY_VECTOR;
            Type[] argtypes =  Type.GetTypeArray(argarray);

            Exception lastException = null;
            ///*
            //todo cache result?
            //n.b. we are not specifying static/instance here - hmmm...
            mi = type.GetMethod(name, argtypes);
            // found method
            try
            {
                if (mi != null) return mi.Invoke(target, argarray);
            }
            catch (SystemException e)
            {
                lastException = e;
                PrologCLR.ConsoleTrace("ignoring " + e.ToString());
            }
            MethodInfo[] methods = type.GetMethods(flags);
            if (false)
            {
                if (methods.Length == 1)	//it's not overloaded
                {
                    mi = methods[0];
                    try
                    {
                        ParameterInfo[] paramInfos = mi.GetParameters();
                        if (argarray.Length == paramInfos.Length)
                        {
                            object[] parray;
                            if (Coerce(termArray, argarray, argtypes, paramInfos, out parray/*ctx*/))
                            {
                                argarray = parray;
                            }
                            return mi.Invoke(target, parray);
                        }
                    }
                    catch (Exception e)
                    {
                        lastException = e;
                    }
                }

                //this should always work, but seems to have problems, i.e. String.Concat
                if (containsNull(argarray))
                {
                    try
                    {
                        return type.InvokeMember(name,
                            /*BindingFlags.Public |*/
                                                 BindingFlags.InvokeMethod //|BindingFlags.FlattenHierarchy
                                                 | (isStatic ? BindingFlags.Static : BindingFlags.Instance)
                                                 , defaultBinder, target, argarray);
                    }
                    catch (Exception e)
                    {
                        PrologCLR.ConsoleTrace("ignoring " + e.ToString());
                    }
                }

                ///*
                //todo cache result?
                //n.b. we are not specifying static/instance here - hmmm...
                mi = type.GetMethod(name, argtypes);
                // found method
                try
                {
                    if (mi != null) return mi.Invoke(target, argarray);
                }
                catch (SystemException e)
                {
                    lastException = e;
                    PrologCLR.ConsoleTrace("ignoring " + e.ToString());
                }
                object result = null;
                if (TryInvokeWithBinder(type, name, defaultBinder, argarray, target, isStatic, ref lastException,
                                        ref result))
                {
                    return result;
                }
                if (TryInvokeWithBinder(type, name, prologBinder, argarray, target, isStatic, ref lastException,
                                        ref result))
                {
                    return result;
                }
            }

            //     The method represented by name has one or more unspecified generic type parameters.
            //     That is, the method's System.Reflection.MethodInfo.ContainsGenericParameters
            //     property returns true.

            object[] parameters;
            string lowername = name.ToLower();
            methods = type.GetMethods(BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Instance |
                                         BindingFlags.Static);
            foreach (MethodInfo m in methods)
            {
                try
                {
                    if (m.Name.ToLower() != lowername) continue;
                    ParameterInfo[] paramInfos = m.GetParameters();
                    if (Coerce(termArray, argarray, argtypes, paramInfos, out parameters/*ctx*/))
                        return m.Invoke(target, parameters);
                }
                catch (Exception e)
                {
                    lastException = e;
                }
            }

            PropertyInfo[] pinfos = type.GetProperties(flags);
            foreach (PropertyInfo m in pinfos)
            {
                try
                {
                    if (m.Name.ToLower() != lowername) continue;
                    ParameterInfo[] paramInfos = m.GetIndexParameters();
                    if (Coerce(termArray, argarray, argtypes, paramInfos, out parameters/*ctx*/))
                        return m.GetValue(target, parameters);
                }
                catch (Exception e)
                {
                    lastException = e;
                }
            }

            if (lastException != null)
            {
                PrologCLR.ConsoleTrace("rethrowing " + lastException.ToString());
                throw lastException;
            }
            throw new Exception("Can't find matching method: " + name + " for: " + type.Name +
                                      " taking those arguments " + argtypes.Length);
            //*/


        }

        bool TryInvokeWithBinder(Type type, string name, Binder methodBinder0, object[] argarray, object target, bool isStatic, ref Exception lastException, ref object result)
        {
            try
            {
                result = type.InvokeMember(name,
                    /*BindingFlags.Public |*/
                                         BindingFlags.InvokeMethod //|BindingFlags.FlattenHierarchy
                                         | (isStatic ? BindingFlags.Static : BindingFlags.Instance)
                                         , methodBinder0, target, argarray);
                return true;
            }
            // Exceptions:
            catch (System.ArgumentNullException e) { lastException = e; }
            //   System.ArgumentNullException:
            //     invokeAttr contains CreateInstance and typeName is null.
            //
            catch (System.ArgumentException e) { lastException = e; }

                //   System.ArgumentException:
            //     args is multidimensional.-or- invokeAttr is not a valid System.Reflection.BindingFlags
            //     attribute.-or- invokeAttr contains CreateInstance combined with InvokeMethod,
            //     GetField, SetField, GetProperty, or SetProperty.-or- invokeAttr contains
            //     both GetField and SetField.-or- invokeAttr contains both GetProperty and
            //     SetProperty.-or- invokeAttr contains InvokeMethod combined with SetField
            //     or SetProperty.-or- invokeAttr contains SetField and args has more than one
            //     element.-or- This method is called on a COM object and one of the following
            //     binding flags was not passed in: BindingFlags.InvokeMethod, BindingFlags.GetProperty,
            //     BindingFlags.SetProperty, BindingFlags.PutDispProperty, or BindingFlags.PutRefDispProperty.-or-
            //     One of the named parameter arrays contains a string that is null.
            //
            catch (System.MethodAccessException e) { lastException = e; }
            //     The specified member is a class initializer.
            //
            catch (System.MissingFieldException e) { lastException = e; }
            //     The field or property cannot be found.
            //
            catch (System.MissingMethodException) { }
            //     The method cannot be found.-or- The current System.Type object represents
            //     a type that contains open type parameters, that is, System.Type.ContainsGenericParameters
            //     returns true.
            //
            catch (System.Reflection.TargetException e) { lastException = e; }
            //     The specified member cannot be invoked on target.
            //
            catch (System.Reflection.AmbiguousMatchException e) { lastException = e; }
            //     More than one method matches the binding criteria.
            //
            catch (System.NotSupportedException e) { lastException = e; }
            //     The .NET Compact Framework does not currently support this method.
            //
            catch (System.InvalidOperationException e) { lastException = e; }
            return false;
        }

        static bool containsNull(object[] argarray)
        {
            foreach (var o in argarray)
                if (o == null) return true;
            return false;
        }

        static public bool Coerce(Term[] terms, object[] argarray, Type[] argtypes, ParameterInfo[] paramInfos, out object[] parameters/*CONTEXT*/)
        {
            int paramInfosLength = paramInfos.Length;
            int argarrayLength = argarray.Length;
            if (paramInfosLength > argarrayLength)
            {
                parameters = null;
                return false;
            }
            if (paramInfosLength == argarrayLength)
            {
                bool wont = false;
                int i = 0;
                parameters = argarray;
                foreach (var info in paramInfos)
                {
                    object value;
                    if (CanConvert(terms[i], info.ParameterType, ref argarray[i], out value/*ctx*/))
                    {
                        parameters[i] = value;
                    }
                    else wont = true;
                    i++;
                }
                return !wont;
            }
            parameters = new object[paramInfosLength];
            int currentParam = 0;
            for (int i = 0; i < paramInfosLength; i++)
            {
                ParameterInfo p = paramInfos[i];
                if (currentParam > argarrayLength)
                {
                    parameters[i] = p.DefaultValue;
                }
                else if (p.ParameterType.IsArray)
                {
                    if (!argtypes[currentParam].IsArray)
                    {
                        // the last arg is an array fill it with the rest and return
                        if (i + 1 == paramInfosLength)
                        {
                            object[] pas = new object[argarrayLength - currentParam];
                            parameters[i] = pas;
                            i = 0;
                            while (currentParam < argarrayLength)
                            {
                                pas[i++] = argarray[currentParam++];
                            }
                            return true;
                        }
                    }

                }
                else
                {
                    parameters[i] = argarray[currentParam];
                }
                currentParam++;
            }
            return true;
        }

        private static bool CanConvert(Term term, Type type, ref object argarray, out object value/*CONTEXT*/)
        {
            if (DoesConvert(type, argarray, out value)) return true;
            //if (DoesConvert(type, term.Value, out value)) return true;

            Func<Term, /*CONTEXT,*/ object> conv;
            if (TermTypeConvertor.TryGetValue(type, out conv))
            {
                try
                {
                    argarray = value = conv(term/*ctx*/);
                    return true;
                }
                catch (Exception e)
                {
                }
            }
            if (!term.IsCompound)
            {
                string v = term.Name;
                if (typeof(char[]).IsAssignableFrom(type))
                {
                    value = v.ToCharArray();
                    return true;
                }
                if (type.IsAssignableFrom(typeof(char)))
                {
                    char ch;
                    if (char.TryParse(v, out ch))
                    {
                        value = ch;
                        return true;
                    }

                    if (v.Length == 1)
                    {
                        value = v[0];
                        return true;
                    }
                }
                else if (type.IsAssignableFrom(typeof(string)))
                {
                    value = v;
                    return true;
                }
            }
            value = null;
            return false;
        }

        private static bool DoesConvert(Type type, object argarray, out object value)
        {
            if (type.IsInstanceOfType(argarray))
            {
                value = argarray;
                return true;
            }
            try
            {
                value = PrologCLR.RecastObject(null,argarray, type);
                return (value != null);
            }
            catch
            {
                value = null;
                return false;
            }
        }


        public static T[] vector_push<T>(T x, T[] a)
        {
            T[] ret = new T[a.Length + 1];
            Array.Copy(a, 0, ret, 1, a.Length);
            ret[0] = x;
            return ret;
        }

        public static T[] vector_rest<T>(T[] a)
        {
            T[] ret = new T[a.Length - 1];
            Array.Copy(a, 1, ret, 0, a.Length - 1);
            return ret;
        }

        public static Object[] terms_to_objects(Term[] a/*CONTEXT*/)
        {
            if (a == null) return null;
            Object[] ret = new Object[a.Length];
            int i = 0;
            foreach (var term in a)
            {
                ret[i++] = ObjectFromTerm(term/*ctx*/);
            }
            return ret;
        }
    }

    public class TermConversionAttribute : Attribute
    {
    }

    public class PrologBinder : Binder
    {
        public PrologBinder()
            : base()
        {
        }
        private class BinderState
        {
            public object[] args;
        }
        public override FieldInfo BindToField(
            BindingFlags bindingAttr,
            FieldInfo[] match,
            object value,
            CultureInfo culture
            )
        {
            if (match == null)
                throw new ArgumentNullException("match");
            // Get a field for which the value parameter can be converted to the specified field type.
            for (int i = 0; i < match.Length; i++)
                if (ChangeType(value, match[i].FieldType, culture) != null)
                    return match[i];
            return null;
        }
        public override MethodBase BindToMethod(
            BindingFlags bindingAttr,
            MethodBase[] match,
            ref object[] args,
            ParameterModifier[] modifiers,
            CultureInfo culture,
            string[] names,
            out object state
            )
        {
            // Store the arguments to the method in a state object.
            BinderState myBinderState = new BinderState();
            object[] arguments = new Object[args.Length];
            args.CopyTo(arguments, 0);
            myBinderState.args = arguments;
            state = myBinderState;
            if (match == null)
                throw new ArgumentNullException();
            // Find a method that has the same parameters as those of the args parameter.
            for (int i = 0; i < match.Length; i++)
            {
                // Count the number of parameters that match.
                int count = 0;
                ParameterInfo[] parameters = match[i].GetParameters();
                // Go on to the next method if the number of parameters do not match.
                if (args.Length != parameters.Length)
                    continue;
                // Match each of the parameters that the user expects the method to have.
                for (int j = 0; j < args.Length; j++)
                {
                    // If the names parameter is not null, then reorder args.
                    if (names != null)
                    {
                        if (names.Length != args.Length)
                            throw new ArgumentException("names and args must have the same number of elements.");
                        for (int k = 0; k < names.Length; k++)
                        {
                            string ns = names[k];
                            if (ns == null) continue;
                            if (System.String.CompareOrdinal(parameters[j].Name, ns) == 0)
                                args[j] = myBinderState.args[k];
                        }
                    }
                    // Determine whether the types specified by the user can be converted to the parameter type.
                    Object changedTo = ChangeType(args[j], parameters[j].ParameterType, culture);
                    if (changedTo != null)
                        count += 1;
                    else
                        break;
                }
                // Determine whether the method has been found.
                if (count == args.Length)
                    return match[i];
            }
            return null;
        }

        public override object ChangeType(object value, Type myChangeType, CultureInfo culture)
        {
            // Determine whether the value parameter can be converted to a value of type myType.
            if (CanConvertFrom(value.GetType(), myChangeType))
                // Return the converted object.
            {
                try
                {
                    object o = Convert.ChangeType(value, myChangeType, culture);
                    if (myChangeType.IsInstanceOfType(o)) return o;
                }
                catch (InvalidCastException e)
                {
                }

                return PrologCLR.RecastObject(null, value, myChangeType);
            }
            //else
                // Return null.
                return null;
        }

        public override void ReorderArgumentArray(
            ref object[] args,
            object state
            )
        {
            // Return the args that had been reordered by BindToMethod.
            ((BinderState)state).args.CopyTo(args, 0);
        }
        public override MethodBase SelectMethod(
            BindingFlags bindingAttr,
            MethodBase[] match,
            Type[] types,
            ParameterModifier[] modifiers
            )
        {
            if (match == null)
                throw new ArgumentNullException("match");
            for (int i = 0; i < match.Length; i++)
            {
                // Count the number of parameters that match.
                int count = 0;
                ParameterInfo[] parameters = match[i].GetParameters();
                // Go on to the next method if the number of parameters do not match.
                if (types.Length != parameters.Length)
                    continue;
                // Match each of the parameters that the user expects the method to have.
                for (int j = 0; j < types.Length; j++)
                    // Determine whether the types specified by the user can be converted to parameter type.
                    if (CanConvertFrom(types[j], parameters[j].ParameterType))
                        count += 1;
                    else
                        break;
                // Determine whether the method has been found.
                if (count == types.Length)
                    return match[i];
            }
            return null;
        }
        public override PropertyInfo SelectProperty(
            BindingFlags bindingAttr,
            PropertyInfo[] match,
            Type returnType,
            Type[] indexes,
            ParameterModifier[] modifiers
            )
        {
            if (match == null)
                throw new ArgumentNullException("match");
            for (int i = 0; i < match.Length; i++)
            {
                // Count the number of indexes that match.
                int count = 0;
                ParameterInfo[] parameters = match[i].GetIndexParameters();
                // Go on to the next property if the number of indexes do not match.
                if (indexes.Length != parameters.Length)
                    continue;
                // Match each of the indexes that the user expects the property to have.
                for (int j = 0; j < indexes.Length; j++)
                    // Determine whether the types specified by the user can be converted to index type.
                    if (CanConvertFrom(indexes[j], parameters[j].ParameterType))
                        count += 1;
                    else
                        break;
                // Determine whether the property has been found.
                if (count == indexes.Length)
                    // Determine whether the return type can be converted to the properties type.
                    if (CanConvertFrom(returnType, match[i].PropertyType))
                        return match[i];
                    else
                        continue;
            }
            return null;
        }

        // Determines whether type1 can be converted to type2. Check only for primitive types.
        public static bool CanConvertFrom(Type type1, Type type2)
        {
            if (type1.IsPrimitive && type2.IsPrimitive)
            {
                TypeCode typeCode1 = Type.GetTypeCode(type1);
                TypeCode typeCode2 = Type.GetTypeCode(type2);
                // If both type1 and type2 have the same type, return true.
                if (typeCode1 == typeCode2)
                    return true;
                // Possible conversions from Char follow.
                if (typeCode1 == TypeCode.Char)
                    switch (typeCode2)
                    {
                        case TypeCode.UInt16: return true;
                        case TypeCode.UInt32: return true;
                        case TypeCode.Int32: return true;
                        case TypeCode.UInt64: return true;
                        case TypeCode.Int64: return true;
                        case TypeCode.Single: return true;
                        case TypeCode.Double: return true;
                        default: return false;
                    }
                // Possible conversions from Byte follow.
                if (typeCode1 == TypeCode.Byte)
                    switch (typeCode2)
                    {
                        case TypeCode.Char: return true;
                        case TypeCode.UInt16: return true;
                        case TypeCode.Int16: return true;
                        case TypeCode.UInt32: return true;
                        case TypeCode.Int32: return true;
                        case TypeCode.UInt64: return true;
                        case TypeCode.Int64: return true;
                        case TypeCode.Single: return true;
                        case TypeCode.Double: return true;
                        default: return false;
                    }
                // Possible conversions from SByte follow.
                if (typeCode1 == TypeCode.SByte)
                    switch (typeCode2)
                    {
                        case TypeCode.Int16: return true;
                        case TypeCode.Int32: return true;
                        case TypeCode.Int64: return true;
                        case TypeCode.Single: return true;
                        case TypeCode.Double: return true;
                        default: return false;
                    }
                // Possible conversions from UInt16 follow.
                if (typeCode1 == TypeCode.UInt16)
                    switch (typeCode2)
                    {
                        case TypeCode.UInt32: return true;
                        case TypeCode.Int32: return true;
                        case TypeCode.UInt64: return true;
                        case TypeCode.Int64: return true;
                        case TypeCode.Single: return true;
                        case TypeCode.Double: return true;
                        default: return false;
                    }
                // Possible conversions from Int16 follow.
                if (typeCode1 == TypeCode.Int16)
                    switch (typeCode2)
                    {
                        case TypeCode.Int32: return true;
                        case TypeCode.Int64: return true;
                        case TypeCode.Single: return true;
                        case TypeCode.Double: return true;
                        default: return false;
                    }
                // Possible conversions from UInt32 follow.
                if (typeCode1 == TypeCode.UInt32)
                    switch (typeCode2)
                    {
                        case TypeCode.UInt64: return true;
                        case TypeCode.Int64: return true;
                        case TypeCode.Single: return true;
                        case TypeCode.Double: return true;
                        default: return false;
                    }
                // Possible conversions from Int32 follow.
                if (typeCode1 == TypeCode.Int32)
                    switch (typeCode2)
                    {
                        case TypeCode.Int64: return true;
                        case TypeCode.Single: return true;
                        case TypeCode.Double: return true;
                        default: return false;
                    }
                // Possible conversions from UInt64 follow.
                if (typeCode1 == TypeCode.UInt64)
                    switch (typeCode2)
                    {
                        case TypeCode.Single: return true;
                        case TypeCode.Double: return true;
                        default: return false;
                    }
                // Possible conversions from Int64 follow.
                if (typeCode1 == TypeCode.Int64)
                    switch (typeCode2)
                    {
                        case TypeCode.Single: return true;
                        case TypeCode.Double: return true;
                        default: return false;
                    }
                // Possible conversions from Single follow.
                if (typeCode1 == TypeCode.Single)
                    switch (typeCode2)
                    {
                        case TypeCode.Double: return true;
                        default: return false;
                    }
            }
            return false;
        }
    }
}

