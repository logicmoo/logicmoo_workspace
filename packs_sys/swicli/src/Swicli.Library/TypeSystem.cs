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

//using System.Linq;

using System.Linq;
#if USE_IKVM
using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
using JClass = Dead.JClass;
using JavaClass = java.lang.Class;
using System;
#else
using SbsSW.SwiPlCs.Callback;
using Type = System.Type;
using System;
#endif

using System.Collections.Generic;
using System.Reflection;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Exceptions;
using PlTerm = SbsSW.SwiPlCs.PlTerm;

namespace Dead
{
    public class JClass
    {
    }

}

namespace Swicli.Library
{
    public partial class PrologCLR
    {

        

        [TypeConversion]
        public static System.Type getInstanceTypeFromClass(JavaClass classObject)
        {
            try
            {
                classObject.ToString();                
                return ikvm.runtime.Util.getInstanceTypeFromClass(classObject);
            }
            catch (Exception odd)
            {
                Embedded.Error("getInstanceTypeFromClass", odd);
                return null;
            }
        }
        [TypeConversion]
        public static JavaClass getFriendlyClassFromType(Type classObject)
        {
            classObject.ToString();                
            //getInstanceTypeFromClass(classObject);
            return ikvm.runtime.Util.getFriendlyClassFromType(classObject);
        }

        
        [PrologVisible]
        static public bool cliTypeToTypespec(PlTerm clazzSpec, PlTerm valueOut)
        {
            return valueOut.Unify(typeToSpec(GetType(clazzSpec)));
        }

        public static Type GetTypeThrowIfMissing(PlTerm clazzSpec)
        {
            Type fi = GetType(clazzSpec,false);
            if (fi == null)
            {
                throw new PlException("cant find class" + clazzSpec);
            }
            return fi;
        }
        [PrologInvisible]
        private static Type GetType(PlTerm clazzSpec)
        {
            return GetType(clazzSpec, false);
        }
        [PrologInvisible]
        private static Type GetType(PlTerm clazzSpec, bool canBeObjects)
        {
            if (clazzSpec.IsVar)
            {
                Embedded.Error("GetType IsVar {0}", clazzSpec);
                return null;
            }
            if (IsTaggedObject(clazzSpec))
            {
                object tagObj = tag_to_object(clazzSpec[1].Name);
                if (tagObj is Type)
                {
                    return (Type)tagObj;
                }
                if (tagObj is JavaClass)
                {
                    return getInstanceTypeFromClass((JavaClass) tagObj);
                }
                if (!canBeObjects)
                {
                    Embedded.Warn("cant find tagged object as class: {0}=>{1}", clazzSpec, tagObj);
                }
                if (tagObj != null)
                {
                    return tagObj.GetType();
                }
                return null;
            }
            Type type = null;
            Boolean wasString = clazzSpec.IsString;
            if (clazzSpec.IsAtomOrString)
            {
                if (canBeObjects && wasString) return typeof(string);
                string name = (string)clazzSpec;
                type = ResolveType(name);
                if (type != null) return type;
                if (!canBeObjects)
                {
                    Embedded.Warn("cant find atom/string as class: {0}", clazzSpec);
                    type = ResolveType(name);
                }
                return null;
            }
            if (clazzSpec.IsCompound)
            {
                string clazzName = clazzSpec.Name;
                int arity = clazzSpec.Arity;
                if (clazzName == "arrayOf")
                {
                    if (arity != 1)
                    {
                        return GetType(clazzSpec[1]).MakeArrayType(clazzSpec[2].intValue());
                    }
                    return GetType(clazzSpec[1]).MakeArrayType();
                }
                if (clazzName == "type")
                {
                    return (GetInstance(clazzSpec[1]) ?? NEW_OBJECTFORTYPE).GetType();
                }
                if (clazzName == "static" || clazzName == "typeof")
                {
                    return GetType(clazzSpec[1]);
                }
                if (clazzName == "{}")
                {
                    return typeof (PlTerm);
                }
                if (clazzName == "pointer")
                {
                    return GetType(clazzSpec[1]).MakePointerType();
                }
                if (clazzName == "byref")
                {
                    return GetType(clazzSpec[1]).MakeByRefType();
                }
                if (clazzName == "nullable")
                {
                    return typeof(Nullable<>).MakeGenericType(new[] { GetType(clazzSpec[1]) });
                }
                type = ResolveType(clazzName + "`" + arity);
                if (type != null)
                {
                    // 'Dictionary'('Int32','string').
                    if (type.IsGenericType)
                    {
                        Type[] genr = type.GetGenericArguments();
                        Type[] genrc = null;
                        Type genrb = null;
                        try
                        {
                            if (type.IsGenericParameter)
                            {
                                genrc = type.GetGenericParameterConstraints();
                            }
                        }
                        catch (Exception e)
                        {
                            Embedded.Warn("GetGenericParameterConstraints: {0}", e);
                        }
                        try
                        {
                            genrb = type.GetGenericTypeDefinition();
                        }
                        catch (Exception e)
                        {
                            Embedded.Warn("GetGenericTypeDefinition: {0}", e);
                        }

                        if (arity == genr.Length)
                        {
                            var vt = GetParamSpec(ToTermArray(clazzSpec), false);
                            return type.MakeGenericType(vt);
                        }
                    }
                    //  return type;
                }
                string key = clazzName + "/" + arity;
                lock (FunctorToLayout)
                {
                    PrologTermLayout pltl;
                    if (FunctorToLayout.TryGetValue(key, out pltl))
                    {
                        return pltl.ObjectType;
                    }
                }
                lock (FunctorToRecomposer)
                {
                    PrologTermRecomposer layout;
                    if (FunctorToRecomposer.TryGetValue(key, out layout))
                    {
                        return layout.ToType;
                    }
                }
                Embedded.WarnMissing("cant find compound as class: " + clazzSpec);
            }
            object toObject = GetInstance(clazzSpec);
            if (toObject is Type) return (Type)toObject;
            if (toObject != null)
            {
                return toObject.GetType();
            }
            Embedded.Warn("@TODO cant figure type from {0}", clazzSpec);
            return typeof(object);
            //return null;
        }

        [PrologVisible]
        public static bool cliFindType(PlTerm clazzSpec, PlTerm classRef)
        {
            //            if (term1.IsAtom)
            {
                string className = (string)clazzSpec;//.Name;
                Type s1 = GetType(clazzSpec);
                if (s1 != null)
                {
                    var c = s1;// ikvm.runtime.Util.getFriendlyClassFromType(s1);
                    if (c != null)
                    {
                       // ConsoleTrace("name:" + className + " type:" + s1.FullName + " class:" + c);
                        return UnifyTagged(c, classRef);
                    }
                    ConsoleTrace("cant getFriendlyClassFromType " + s1.FullName);
                    return false;
                }
                ConsoleTrace("cant ResolveType " + className);
                return false;
            }
            ConsoleTrace("cant IsAtom " + clazzSpec);
            return false;
        }

        public static void ConsoleTrace(object s)
        {
			try {
				System.Console.WriteLine(s);
			} catch (System.TypeInitializationException e) {
				// @TODO
			} catch (Exception e) {
				// @TODO
			}
        }

        [IKVMBased]
        [PrologVisible]
        public static bool cliFindClass(PlTerm clazzName, PlTerm clazzObjectOut)
        {
            if (clazzName.IsAtomOrString)
            {
                string className = clazzName.Name;
                Type c = ResolveType(className);
                if (c != null)
                {
                    ConsoleTrace("cliFindClass:" + className + " class:" + c);
                    string tag = object_to_tag(c);
                    return AddTagged(clazzObjectOut.TermRef, tag) != 0;
                }
                ConsoleTrace("cant ResolveClass " + className);
                return false;
            }
            Type t = GetType(clazzName);
            if (t != null)
            {
                Type c = null;
#if USE_IKVM
                c = t;
              //  JavaClass t = getFriendlyClassFromType(t);
#else
                c = t;
#endif
                string tag = object_to_tag(c);
                return AddTagged(clazzObjectOut.TermRef, tag) != 0;
            }
            return false;
        }

        private static IDictionary<string, Type> ShortNameType;
        private static IDictionary<string, Type> LongNameType = new Dictionary<string,Type>();
        private static readonly Dictionary<Type, string> TypeShortName = new Dictionary<Type, string>();
        private static object NEW_OBJECTFORTYPE = new object();

        private static PlTerm typeToSpec(Type type)
        {
            if (type == null) return PLNULL;
            if (type.IsArray && type.HasElementType)
            {
                if (type.GetArrayRank() != 1)
                {
                    return PlC("arrayOf", typeToSpec(type.GetElementType()), ToProlog(type.GetArrayRank()));
                }
                return PlC("arrayOf", typeToSpec(type.GetElementType()));
            }
            if (type.IsGenericParameter)
            {
                Type[] gt = type.GetGenericParameterConstraints();
                return PlC("<" + type.FullName ?? type.Name + ">", ToPlTermVSpecs(gt));
            }
            if (type.IsPointer)
            {
                Type gt = type.GetElementType();
                return PlC("pointer", typeToSpec(gt));
            }
            if (type.IsByRef)
            {
                Type gt = type.GetElementType();
                return PlC("byref", typeToSpec(gt));
            }
            // @todo if false , use IsGenericType
            if (false) if (typeof(Nullable<>).IsAssignableFrom(type))
            {
                Embedded.Error("@todo Not Implemented NULLABLE");
                Type gt = type.GetElementType();
                return PlC("nullable", typeToSpec(gt));
            }

            if (type.IsGenericType )
            {
                Type gt = type.GetGenericTypeDefinition();
                Type[] gtp = type.GetGenericArguments();
                PlTermV vt = ToPlTermVSpecs(gtp);
                string typeName = type.FullName ?? type.Name;
                int gtpLength = gtp.Length;
                int indexOf = typeName.IndexOf("`" + gtpLength);
                if (indexOf > 0)
                {
                    typeName = typeName.Substring(0, indexOf);
                }
                else
                {
                    Embedded.Debug("cant chop arity {0} off string '{1}' ", gtpLength, typeName);
                }
                return PlC(typeName, vt);
            }
            if (type.HasElementType)
            {
                string named = typeToName(type);
                Embedded.Error("@todo Not Implemented " + named);
                Type gt = type.GetElementType();
                if (gt == type) gt = typeof(object);
                return PlC("elementType", PlTerm.PlAtom(named), typeToSpec(gt));
            }
            if (type.IsSpecialName || String.IsNullOrEmpty(type.Name) || String.IsNullOrEmpty(type.FullName) || String.IsNullOrEmpty(type.Namespace))
            {
                string named = typeToName(type);
                Embedded.Error("@todo Not Implemented " + named);
                Type gt = type.UnderlyingSystemType;
                if (gt == type) gt = typeof (object);
                return PlC("static", PlTerm.PlAtom(named), typeToSpec(gt));
            }
            return PlTerm.PlAtom(typeToName(type));
        }

        [PrologVisible]
        static public bool cliGetType(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliGetType(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
         /*   if (valueIn.IsAtom)
            {
                Type t = GetType(valueIn);
                return valueOut.FromObject(t);
            }*/
            object val = GetInstance(valueIn);
            if (val == null)
            {
                Embedded.Error("Cannot get object for {0}", valueIn);
                return false;
            }
            return valueOut.FromObject((val.GetType()));
        }

        [IKVMBased]
        [PrologVisible]
        static public bool cliGetClass(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliGetClass(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            object val = GetInstance(valueIn);
            // extension method
#if USE_IKVM

            return valueOut.FromObject((ikvm.extensions.ExtensionMethods.instancehelper_getClass(val)));
#else
            return valueOut.FromObject((val.GetType()));
#endif
        }
        [IKVMBased]
        [PrologVisible]
        static public bool cliClassFromType(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliClassFromType(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            Type val = GetType(valueIn);
            if (val == null) return false;
#if USE_IKVM
            JavaClass c = getFriendlyClassFromType(val);
            return valueOut.FromObject((c));
#else
            return valueOut.FromObject((val));
#endif
        }
        [IKVMBased]
        [PrologVisible]
        static public bool cliTypeFromClass(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliTypeFromClass(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            Type val = GetType(valueIn);
            if (val == null) return false;
#if USE_IKVM
            Type c = getInstanceTypeFromClass(val);
            return valueOut.FromObject((c));
#else
            return valueOut.FromObject(val);
#endif
        }
        [PrologVisible]
        static public bool cliAddShorttype(PlTerm valueName, PlTerm valueIn)
        {
            if (!valueName.IsAtomOrString) return Embedded.Warn("valueName must be string or atom {0}", valueName);
            string name = valueName.Name;
            Type otherType;
            lock (ShortNameType)
            {
                if (ShortNameType.TryGetValue(name, out otherType))
                {
                    if (valueIn.IsNumber)
                    {
                        ShortNameType.Remove(name);
                        TypeShortName.Remove(otherType);
                        return true;
                    }
                    if (valueIn.IsVar)
                    {
                        return valueIn.UnifyAtom(otherType.FullName);
                    }
                    Type val = GetType(valueIn);
                    if (val == otherType) return true;
                    return false;
                }
                else
                {
                    if (valueIn.IsNumber)
                    {
                        return true;
                    }
                    if (valueIn.IsVar)
                    {
                        return true;
                    }
                    Type val = GetType(valueIn);
                    if (val == null) return false;
                    ShortNameType[name] = val;
                    TypeShortName[val] = name;
                    return true;
                }
            }
        }
        [PrologVisible]
        [IKVMBased]
        static public bool cliTypeToClassname(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliTypeToClassname(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            //JClass val = CastTerm(valueIn, typeof(JClass)) as JClass;
            Type val0 = GetTypeThrowIfMissing(valueIn);
            if (val0 == null) return false;
            JavaClass val = getFriendlyClassFromType(val0);

#if USE_IKVM
            return valueOut.Unify(val.getName());
#else
            return valueOut.Unify(val.GetType().Name);
#endif
        }
        [PrologVisible]
        static public bool cliTypeToFullname(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliTypeToFullname(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            Type val = GetTypeThrowIfMissing(valueIn);
            //Type val = CastTerm(valueIn, typeof(Type)) as Type;
            if (val == null) return false;
            return valueOut.Unify(val.FullName);
        }

        private static string typeToName(Type type)
        {
            if (type.IsArray && type.HasElementType)
            {
                return typeToSpec(type.GetElementType()) + "[]";
            }
            lock (ShortNameType)
            {
                string shortName;
                if (TypeShortName.TryGetValue(type, out shortName))
                {
                    return shortName;
                }
                string typeName = type.Name;
                Type otherType;
                if (ShortNameType.TryGetValue(type.Name, out otherType))
                {
                    if (type == otherType)
                    {
                        return typeName;
                    }
                    return type.FullName;
                }
                ShortNameType[typeName] = type;
                TypeShortName[type] = typeName;
                return typeName;
            }
        }
        /*
        private static Type ResolveClass(string name)
        {
            if (name == "@" || name == "$cli_object" || name == "array" || name == null) return null;
            Type t = ResolveClassAsType(name);
#if USE_IKVM
            JClass c = getFriendlyClassFromType((Type)t);
            return c;
#else
            return t;
#endif

        }*/
        private static Type ResolveClassAsType(string name)
        {
            Type s1 = ResolveType(name);
            if (s1 != null) return s1;
            if (name.EndsWith("[]"))
            {
                Type t1 = ResolveClassAsType(name.Substring(0, name.Length - 2));
                return t1.MakeArrayType();
            }
            var name2 = name.Replace("/", ".");
            if (name2 != name)
            {
                s1 = ResolveType(name2);
                if (s1 != null) return s1;
            }
            name2 = name.Replace("cli.", "");
            if (name2 != name)
            {
                s1 = ResolveType(name2);
                if (s1 != null) return s1;
            }
            return null;
        }

        static readonly private Dictionary<string, Type> typeCache = new Dictionary<string, Type>();
        static public Boolean MayUseTypeCache = true;

        public static Type ResolveType(string name)
        {
            if (!MayUseTypeCache || typeCache == null) return ResolveType0(name);
            lock (typeCache)
            {
                Type type;
                if (!typeCache.TryGetValue(name, out type))
                {
                    return typeCache[name] = ResolveType0(name);
                }
                return type;
            }
        }

        private static Type ResolveType0(string name)
        {
            if (name == "@" || name == "[]" || name == "$cli_object" || name == "array" || name == null) return null;
            if (name.EndsWith("[]"))
            {
                Type t = ResolveType(name.Substring(0, name.Length - 2));
                return t.MakeArrayType();
            }
            if (name.EndsWith("?"))
            {
                return typeof(Nullable<>).MakeGenericType(new[] { ResolveType(name.Substring(0, name.Length - 1)) });
            }
            if (name.EndsWith("&"))
            {
                Type t = ResolveType(name.Substring(0, name.Length - 1));
                return t.MakeByRefType();
            }
            if (name.EndsWith("*"))
            {
                Type t = ResolveType(name.Substring(0, name.Length - 1));
                return t.MakePointerType();
            }

            var s1 = ResolveType1(name);
            if (s1 != null) return s1;
            var name2 = name.Replace("/", ".");
            if (name2 != name)
            {
                s1 = ResolveType1(name2);
                if (s1 != null) return s1;
            }
            name2 = name.Replace("cli.", "");
            if (name2 != name)
            {
				s1 = ResolveType1(name2);
				if (s1 != null) return s1;
            }
            return null;
        }

        public static Type ResolveType1(string typeName)
        {
            try
            {
                Type s1 = ResolveType2(typeName);
                if (s1 != null) return s1;
                return null;
            }
            catch (NotSupportedException e)
            {
                return null;
                throw new NotSupportedException("ResolveType12", e);
            }
            catch (NotImplementedException e)
            {
                return null;
                throw new NotSupportedException("ResolveType12", e);
            }
            catch (NullReferenceException e)
            {
                return null;
                throw new NotSupportedException("ResolveType12", e);
            }
            catch (Exception e)
            {
                throw new NotSupportedException("ResolveType123", e);
            }
        }

        public static Type ResolveType2(string typeName)
        {
            Type type;
            if (!typeName.Contains("."))
            {
                lock (ShortNameType)
                {
                    if (ShortNameType.TryGetValue(typeName, out type))
                    {
                        return type;
                    }
                }
                type = ResolveTypeInNameSpaces(typeName, false) ?? ResolveTypeInNameSpaces(typeName, true);
                if (type == null)
                {
                    type = GetPrimitiveType(typeName);
                }


                if (type != null)
                {
                    lock (ShortNameType)
                    {
                        ShortNameType[typeName] = type;
                    }
                    return type;
                }
            }
            type = Type.GetType(typeName);
            if (type != null) return type;

            type = Type.GetTypeFromProgID(typeName);
            if (type != null) return type;


            try
            {
                type = Type.GetTypeFromCLSID(new Guid(typeName));
                if (type != null) return type;
            }
            catch (FormatException)
            {
            }


            type = ResolveTypeWithoutNameSpaces(typeName, false) ?? ResolveTypeWithoutNameSpaces(typeName, true);
            return type;
        }

        public static Type ResolveTypeWithoutNameSpaces(string typeName0, bool ignoreCase)
        {
           
            Type t =  Type.GetType(typeName0, false, ignoreCase) ??
                     AssembliesLoaded.Select(loaded => loaded.GetType(typeName0, false, ignoreCase))
                       .FirstOrDefault(tt => tt != null);
            if (t != null)
            {
                return t;
            }
            if (ignoreCase) return null;
#if USE_IKVM
            try
            {
                JavaClass javaClass = java.lang.Class.forName(typeName0, true, scriptingClassLoader);
                if (javaClass != null)
                {
                    Type type = getInstanceTypeFromClass(javaClass);
                    LongNameType[typeName0] = type;
                    String lower = typeName0.ToLower();
                    if (!lower.Equals(typeName0))
                    {
                        LongNameType[lower] = type;
                    }
                    return type;
                }

            }
            catch (java.lang.ClassNotFoundException e)
            {
                //  java.lang.Class.forName(typeName0, true, scriptingClassLoader);
            }
#endif
            return null;
        }
        public static Type ResolveTypeInNameSpaces(string typeName, bool ignoreCase)
        {
            string name0 = typeName;
            Type t = t = ResolveTypeWithoutNameSpaces(typeName, ignoreCase); 
            if (t != null) return t;

            System.Diagnostics.Debug.Assert(PrefixStrings != null, "PrefixStrings != null");
            if (PrefixStrings != null && PrefixStrings.Count != 0)
                foreach (string prefix0 in PrefixStrings)
                {
                    t = ResolveTypeWithoutNameSpaces(prefix0 + typeName, ignoreCase);
                    if (t != null) return t;

                }
            return null;
        }

        [ThreadStatic] public static List<string> _PrefixStrings;
        public static List<string> PrefixStrings
        {
            get { return _PrefixStrings ?? (_PrefixStrings = new List<string>() {"System."}); }
        }


        public static Type GetPrimitiveType(String name)
        {
            if (name.StartsWith("["))
            {
                Type t = ResolveType(name.Substring(1));
                return t.MakeArrayType();
            }
            switch (name)
            {
                case "byte":
                case ":byte":
                case "B":
                case "uint8":
                case "ubyte":
                case ":unsigned-byte":
                    return typeof(byte);
                case "int16":
                    return typeof(Int16);
                case "int":
                case ":int":
                case "int32":
                case "I":
                    return typeof(int);
                case "long":
                case "int64":
                case "J":
                    return typeof(long);
                case "short":
                case "S":
                    return typeof(short);
                case "sbyte":
                case "int8":
                    return typeof(sbyte);
                case "uint":
                case ":unsigned-int":
                case "uint32":
                    return typeof(uint);
                case "uint16":
                    return typeof(UInt16);
                case "uint64":
                case "ulong":
                    return typeof(ulong);
                case "ushort":
                    return typeof(ushort);
                case "decimal":
                    return typeof(decimal);
                case "double":
                case "D":
                    return typeof(double);
                case "float":
                case "F":
                    return typeof(float);
                case "object":
                    return typeof(object);
                case "string":
                    return typeof(string);
                case "void":
                case "V":
                    return typeof(void);
                case "char":
                case "C":
                    return typeof(char);
                case "bool":
                case "boolean":
                case "bit":
                case "Z":
                    return typeof(bool);
                default:
                    return null;
            }
        }
    }
}