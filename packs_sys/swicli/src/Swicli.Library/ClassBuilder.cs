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
using System.ComponentModel;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
using PlTerm = SbsSW.SwiPlCs.PlTerm;

namespace Swicli.Library
{


    partial class PrologCLR
    {
        [PrologVisible]
        public static void CreateEnumClassTest()
        {
            // Define two members, "High" and "Low".
            //CreateEnumClassTest("enumTst.Elevation", typeof(int), new List<object[]> ({ "Low", 0 }, { "High", 1 } ,},),null);
        }

        // ?- cli_compile_enum(int,'MyEnum',['Low'(0),'High'(100)]).
        // ?- cli_compile_enum(int,'MyEnum',['Low'(0),'High'(100)],[],O).
        [PrologVisible]
        public static object cliCompileEnum(Type baseType, String memberName, List<PlTerm> members, PlTerm typeAnnotAttribs)
        {
            // Get the current application domain for the current thread.
            AppDomain currentDomain = AppDomain.CurrentDomain;

            // Create a dynamic assembly in the current application domain,  
            // and allow it to be executed and saved to disk.
            AssemblyName aName = new AssemblyName("TempAssembly");
            AssemblyBuilder m_assemblyBuilder = currentDomain.DefineDynamicAssembly(
                aName, AssemblyBuilderAccess.RunAndSave);

            // Define a dynamic module in "TempAssembly" assembly. For a single-
            // module assembly, the module has the same name as the assembly.
            ModuleBuilder mb = m_assemblyBuilder.DefineDynamicModule(aName.Name, aName.Name + ".dll");

            // Define a public enumeration with the name "Elevation" and an 
            // underlying type of Integer.
            EnumBuilder typeBuilder = mb.DefineEnum(memberName, TypeAttributes.Public, baseType);

            if (!IsEmpty(typeAnnotAttribs))
            {
                foreach (var annoteDef in typeAnnotAttribs)
                {
                    var clattribs = cliCustomAttributeBuilder(annoteDef);
                    typeBuilder.SetCustomAttribute(clattribs);                    
                }
            }

            object nextVal = -1; // default(baseType);
            string nextname = "ENUMVAL_";
            int fn = -1;

            foreach (var k in members)
            {
                fn++;
                nextname = (k.Name ?? nextname + "_" + fn).ToString();
                object value = nextVal;
                if (k.Arity > 0)
                {
                    value = GetInstance(k.Arg(0));
                }
                FieldBuilder fb = typeBuilder.DefineLiteral(nextname, value);
                if (k.Arity > 1)
                {
                    var cab = cliCustomAttributeBuilder(k.Arg(1));
                    fb.SetCustomAttribute(cab);
                }
                nextVal = cliIncr(value);
                //fb.SetCustomAttribute(new CustomAttributeBuilder
            }


            // Create the type and save the assembly.
            Type finished = typeBuilder.CreateType();
            m_assemblyBuilder.Save(aName.Name + ".dll");

            foreach (object o in Enum.GetValues(finished))
            {
                Embedded.Debug("{0}.{1} = {2}", finished, o, (int) o);
            }
            return finished;
        }

        [PrologVisible]
        public static void CreateTypeClassTest()
        {
            // Define two members, "High" and "Low".
            cliCompileType("MyType", typeof (object), new PlTerm("['Low'(0),'High'(100)],[]"));
            cliCompileType("MyEnum", typeof (ValueType), new PlTerm("['Low'(0),'High'(100)],[annot('FlagsAttribute')]"));
        }

        [PrologVisible]
        public static void cliCompileType(string p, Type type, PlTerm plTerm)
        {
            throw new NotImplementedException();
#warning "TODO"
        }

        // DECL enum(FieldsList,BaseType,...) TypeAnnotations,EnumAttributes,Name
        // DECL union(TypesList,..)   TypeAnnotations,Name
        // DECL struct(MemberList,...)  BaseTypes,TypeAnnotations,TypeAttributes,TypeNameWithGenerics
        // DECL class(MembersList,...)  BaseTypes,TypeAnnotations,TypeAttributes,TypeNameWithGenerics
        // DECL interface(MembersList,...)  BaseTypes,TypeAnnotations,TypeAttributes,TypeNameWithGenerics
        // DECL delegate(ReturnType,ParameterListWithGenerics,...)  MethodAttributes,TypeNameWithGenerics
        // MEMBER method(ReturnType,ParameterListWithGenerics,MethodNameWithGenerics,...) MethodAttributes
        // MEMBER field(NameWithGenerics,ReturnTypeOrInit,...) FieldAttributes
        // MEMBER property(ReturnType,ParameterListWithGenerics,Option), PropertyAttributes,NameWithGenerics)
        // MEMBER event(DelegateType,Option), MemberAttributes,NameWithGenerics)

        // ?- cli_compile_enum(int,'MyType',['Low'(0),'High'(100)]).
        // ?- cli_compile_enum(int,'MyType',['Low'(0),'High'(100)],[annot('FlagsAttribute')]).
        // ?- cli_compile_type([int],[],"MyType",[f('Low',int(0)),f('High',int(100))],['FlagsAttribute'],O).
        // ?- cli_compile_type(class(foo(f(intValue,int(3))),NewClass)
        [PrologVisible]
        public static Type cliCompileTypeRaw(PlTerm baseTypesPL, PlTerm typeAttrsPL, String typeName,
            PlTerm members, PlTerm typeAnnotAttribs)
        {
            Type[] refT =
            {
                typeof (EventAttributes), typeof (MethodAttributes), typeof (EventAttributes),
                typeof (PropertyAttributes),
                typeof (FieldAttributes),
                typeof (TypeAttributes), typeof (CallingConventions), typeof (CallingConvention),
                typeof (ParameterAttributes)
            };

            AssemblyBuilder m_assemblyBuilder;
            ModuleBuilder mb = GetModuleBuilder(out m_assemblyBuilder);

            // Define a public enumeration with the name "Elevation" and an 
            // underlying type of Integer.
            TypeAttributes total = GetEnumFlags<TypeAttributes>(typeAttrsPL);

            List<Type> baseTypes = new List<Type>(GetParamSpec(baseTypesPL));
            Type baseType = baseTypes.Find((f) => !f.IsInterface);
            Type[] interfaces = baseTypes.FindAll((f) => f.IsInterface).ToArray();
            if (total == TypeAttributes.SequentialLayout)
                total |= (TypeAttributes.Sealed | TypeAttributes.SequentialLayout | TypeAttributes.Serializable);
            TypeBuilder typeBuilder = mb.DefineType(typeName, total, baseType, interfaces);

            foreach (CustomAttributeBuilder clattr in cliCustomAttributeBuilderS(typeAnnotAttribs))
            {
                typeBuilder.SetCustomAttribute(clattr);
            }
            object nextVal = default(object);
            string nextname = "MEMBER_";
            int fn = -1;
            foreach (var k in members)
            {
                Object o = cliCompileMember(k, typeBuilder);
                UnifyTagged(o, k.Arg(0));
            }


            // Create the type and save the assembly.
            Type finished = typeBuilder.CreateType();
            // m_assemblyBuilder.Save(aName.Name + ".dll");

            foreach (var o in finished.GetMembers(BindingFlagsALL3))
            {
                Embedded.Debug("{0}.{1} = {2}", finished, o, o.GetType());
            }
            return finished;
        }

        private static List<CustomAttributeBuilder> cliCustomAttributeBuilderS(PlTerm typeAnnotAttribs)
        {
            if (IsEmpty(typeAnnotAttribs)) return new List<CustomAttributeBuilder>(new CustomAttributeBuilder[0]);

            var caBuilders = new List<CustomAttributeBuilder>();
            foreach (var annoteDef in typeAnnotAttribs)
            {
                var clattribs = cliCustomAttributeBuilder(annoteDef);
                caBuilders.Add(clattribs);

            }
            return caBuilders;
        }

        private static ModuleBuilder GetModuleBuilder(out AssemblyBuilder m_assemblyBuilder)
        {
            // Get the current application domain for the current thread.
            AppDomain currentDomain = AppDomain.CurrentDomain;

            // Create a dynamic assembly in the current application domain,  
            // and allow it to be executed and saved to disk.
            AssemblyName aName = new AssemblyName("TempAssembly");
            m_assemblyBuilder = currentDomain.DefineDynamicAssembly(
                aName, AssemblyBuilderAccess.RunAndSave);

            // Define a dynamic module in "TempAssembly" assembly. For a single-
            // module assembly, the module has the same name as the assembly.
            ModuleBuilder mb = m_assemblyBuilder.DefineDynamicModule(aName.Name, aName.Name + ".dll");
            return mb;
        }

        [PrologVisible]
        public static object cliCompileMember(PlTerm memberTerm)
        {
            var cm = cliCompileMember(memberTerm, null);
            return cm;
        }

        [PrologVisible]
        public static object cliCompileMember(PlTerm memberTerm, TypeBuilder typeBuilder)
        {
            if (typeBuilder == null) typeBuilder = newTypeBuilder();
            string mn = memberTerm.Name + "/" + memberTerm.Arity;
            switch (mn)
            {
                case "c/7":
                {
                    var ordinal = memberTerm.Arg(0);
                    var memberName = memberTerm.Arg(1).Name;
                    Type[] paramTypes = GetParamSpec(memberTerm.Arg(2));
                    var o_CM = GetParamSpec(memberTerm.Arg(3));
                    var decl = memberTerm.Arg(4);
                    var IsStatic = cliIsFlagTrue(decl.Arg(0));
                    var DeclaringType = GetType(decl.Arg(1));
                    var access_pafv = memberTerm.Arg(5);
                    var info = memberTerm.Arg(6);
                    CallingConventions callingConventions = CallingConventions.Standard;
                    if (IsStatic) callingConventions |= CallingConventions.HasThis;
                    var cb = typeBuilder.DefineConstructor(getMethodAttributes(access_pafv), callingConventions,
                        paramTypes);
                    return cb;
                }

                case "m/8":
                {
                    var ordinal = memberTerm.Arg(0);
                    var memberName = memberTerm.Arg(1).Name;
                    Type rt = GetType(memberTerm.Arg(2));
                    Type[] paramTypes = GetParamSpec(memberTerm.Arg(3));
                    // var o_CM = GetGenericParamSpec(memberTerm.Arg(4));
                    var decl = memberTerm.Arg(5);
                    var IsStatic = cliIsFlagTrue(decl.Arg(0));
                    var DeclaringType = GetType(decl.Arg(1));
                    var access_pafv = memberTerm.Arg(6);
                    var info = memberTerm.Arg(7);
                    /* Type[][] paramTypes_OCM = null;
                        Type[][] paramTypes_RCM = null;
                        CallingConventions callingConventions = CallingConventions.Standard; 
                        if (IsStatic) callingConventions |= CallingConventions.HasThis;
                                                
                        var cb = typeBuilder.DefineMethod(memberName, PrologCLR.getMethodAttributes(access_pafv),
                            callingConventions, rt, returnRTypeCM , returnType_OCM, paramTypes,
                            paramTypes_RCM, paramTypes_OCM);
                        */
                    var cb = typeBuilder.DefineMethod(memberName, getMethodAttributes(access_pafv), rt,
                        paramTypes);
                    //cb.DefineParameter
                    return cb;
                }
                case "e/8":
                {
                    var ordinal = memberTerm.Arg(0);
                    var memberName = memberTerm.Arg(1).Name;
                    Type rt = GetType(memberTerm.Arg(2));
                    Type[] paramTypes = GetParamSpec(memberTerm.Arg(3));
                   // var o_CM = GetGenericParamSpec(memberTerm.Arg(4));
                    var decl = memberTerm.Arg(5);
                    var IsStatic = cliIsFlagTrue(decl.Arg(0));
                    var DeclaringType = GetType(decl.Arg(1));
                    var access_pafv = memberTerm.Arg(6);
                    var info = memberTerm.Arg(7);
                    var cb = typeBuilder.DefineEvent(memberName, GetEnumFlags<EventAttributes>(access_pafv), rt);
                    return cb;
                }
                case "f/6":
                {
                    var ordinal = memberTerm.Arg(0);
                    var memberName = memberTerm.Arg(1).Name;
                    Type rt = GetType(memberTerm.Arg(2));
                    var decl = memberTerm.Arg(3);
                    var IsStatic = cliIsFlagTrue(decl.Arg(0));
                    var DeclaringType = GetType(decl.Arg(1));
                    var access_pafv = memberTerm.Arg(4);
                    var info = memberTerm.Arg(5);
                    var cb = typeBuilder.DefineField(memberName, rt,
                        GetEnumFlags<FieldAttributes>(decl.Arg(0)) |
                        ((FieldAttributes)(uint)(MethodAttributes)getMethodAttributes(access_pafv)));
                    return cb;
                }      
                case "f/4":
                {
                    var memberName = memberTerm.Arg(0).Name;
                    PlTerm arg2 = memberTerm.Arg(1);
                    Type rt = GetType(arg2);
                    Object init = CastTerm(arg2,rt);
                    PlTerm arg3 = memberTerm.Arg(2);
                    var fieldAttrs = GetEnumFlags<FieldAttributes>(arg3, true);                  
                    var cb = typeBuilder.DefineField(memberName, rt, fieldAttrs);
                    cb.SetConstant(init);
                    foreach (var a in cliCustomAttributeBuilderS(memberTerm.Arg(3)))
                        cb.SetCustomAttribute(a);
                    return cb;

                }
                case "p/10":
                {
                    var ordinal = memberTerm.Arg(0);
                    var memberName = memberTerm.Arg(1).Name;
                    var propType = GetType(memberTerm.Arg(2));
                    Type[] paramTypes = GetParamSpec(memberTerm.Arg(3));
                    var o_CM = GetParamSpec(memberTerm.Arg(4));
                    var CanRead = cliIsFlagTrue(memberTerm.Arg(5));
                    var CanWrite = cliIsFlagTrue(memberTerm.Arg(6));
                    var decl = memberTerm.Arg(7);
                    var access_pafv = memberTerm.Arg(8);
                    var IsStatic = cliIsFlagTrue(decl.Arg(0));
                    var DeclaringType = GetType(decl.Arg(1));
                    var info = memberTerm.Arg(9);
                    var cb = typeBuilder.DefineProperty(memberName,
                        GetEnumFlags<PropertyAttributes>(decl.Arg(0)) |
                        ((PropertyAttributes) (uint) (MethodAttributes) getMethodAttributes(access_pafv)), propType,
                        paramTypes);
                    return cb;
                }
                default:
                    throw new ArgumentOutOfRangeException("" + mn + " " + memberTerm);
            }
        }


        private static TypeBuilder newTypeBuilder()
        {
            // Get the current application domain for the current thread.
            AppDomain currentDomain = AppDomain.CurrentDomain;
            String typeName = "NewTypeBuilder_" + Guid.NewGuid().ToString();
            // Create a dynamic assembly in the current application domain,  
            // and allow it to be executed and saved to disk.
            AssemblyName aName = new AssemblyName("TempAssembly_" + typeName);
            AssemblyBuilder m_assemblyBuilder = currentDomain.DefineDynamicAssembly(
                aName, AssemblyBuilderAccess.RunAndSave);

            // Define a dynamic module in "TempAssembly" assembly. For a single-
            // module assembly, the module has the same name as the assembly.
            ModuleBuilder mb = m_assemblyBuilder.DefineDynamicModule(aName.Name, aName.Name + ".dll");

            return mb.DefineType(typeName);
        }

        private static T GetEnumFlags<T>(PlTerm attrs)
        {
            return GetEnumFlags<T>(attrs, false);
        }

        private static T GetEnumFlags<T>(PlTerm attrs, bool maySkipSome)
        {
            if (attrs.IsNil) return default(T);
            Type enumType = typeof (T);
            if (attrs.IsList )
            {

                int at = 0;

                foreach (var attr in attrs)
                {
                    at |= (int) CastTerm(attr,enumType);

                }
                return (T) Enum.ToObject(enumType, at);
            }
            Object o = CastTerm(attrs, typeof (T));
            if (o == null) return default(T);
            return (T) o;
        }

        private static MethodAttributes getMethodAttributes(PlTerm access_pafv)
        {
            if (access_pafv.Name == "access_pafv")
            {
                MethodAttributes total = default(MethodAttributes);
                if (cliIsFlagTrue(access_pafv.Arg(0))) total |= MethodAttributes.Public;
                if (cliIsFlagTrue(access_pafv.Arg(1))) total |= MethodAttributes.Assembly;
                if (cliIsFlagTrue(access_pafv.Arg(2))) total |= MethodAttributes.Family;
                if (cliIsFlagTrue(access_pafv.Arg(3))) total |= MethodAttributes.Private;
                return total;
            }
            return GetEnumFlags<MethodAttributes>(access_pafv);
        }

        private static bool cliIsFlagTrue(PlTerm plTerm)
        {
            if (plTerm.IsAtom) return plTerm.Name.ToLower().StartsWith("t");
            if (plTerm.IsCompound) return cliIsFlagTrue(plTerm.Arg(0));
            return false;
        }

        [PrologVisible]
        public static CustomAttributeBuilder cliCustomAttributeBuilder(PlTerm plattr)
        {
            if (IsEmpty(plattr))
            {
                return null;
            }
            cliTLMem()["ci"] = null;
            Type type = GetType(plattr);
            if (type == null) throw new NullReferenceException("toType(" + plattr + ")");
            
            object res;
            Action postCallHook;
            Attribute attr;
            ConstructorInfo ci = type.GetConstructor(ZERO_TYPES);
            if (TryConstructObject(type, plattr, plattr, out postCallHook, out res))
            {
                attr = (Attribute) res;
                CommitPostCall(postCallHook);
            }
            else
            {
                if (ci == null)
                {
                    ci = (ConstructorInfo) (cliTLMem()["ci"] ?? ci);
                }
                if (typeof (Attribute).IsAssignableFrom(type))
                {
                    attr = (Attribute) ci.Invoke(ZERO_OBJECTS);
                    //  attr = (Attribute) CastTerm(plattr, type);
                }
                else
                {
                    attr = cliAsAttibute(plattr);
                }
            }
            var fis = object_to_fieldinfo_values(attr, true);
            var pis = object_to_property_values(attr, true, true);

            object[] cvalue = PlListToCastedArray(plattr, ci.GetParameters(), out postCallHook);
            CustomAttributeBuilder cab0 = new CustomAttributeBuilder(ci, cvalue, pis.Keys.ToArray(),
                pis.Values.ToArray(), fis.Keys.ToArray(), fis.Values.ToArray());
            CommitPostCall(postCallHook);
            return cab0;
        }

        private CustomAttributeBuilder BuildCustomAttribute(Attribute attribute)
        {
            Type type = attribute.GetType();
            var constructor = type.GetConstructor(Type.EmptyTypes);
            var properties = type.GetProperties(BindingFlags.Public | BindingFlags.Instance);
            var fields = type.GetFields(BindingFlags.Public | BindingFlags.Instance);

            var propertyValues = from p in properties
                select p.GetValue(attribute, null);
            var fieldValues = from f in fields
                select f.GetValue(attribute);

            return new CustomAttributeBuilder(constructor,
                Type.EmptyTypes,
                properties,
                propertyValues.ToArray(),
                fields,
                fieldValues.ToArray());
        }

        private static bool IsEmpty(PlTerm plattr)
        {
            return (plattr.IsVar || plattr.IsNil || plattr.ToString() == "@null");
        }

        public static Attribute cliAsAttibute(PlTerm plattr)
        {
            if (plattr.IsCompound)
            {
                if (plattr.Name == "attr")
                {
                }
            }
            Type type = GetType(plattr);
            object retval = CastTerm(plattr, type);
            return (Attribute) retval;
        }

        private static Dictionary<FieldInfo, object> object_to_fieldinfo_values(Object attr, bool concrete)
        {
            var dict = new Dictionary<FieldInfo, object>();
            Type tdict = attr.GetType();

            foreach (
                var fi in
                    tdict.GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public
                        /*do NOT| BindingFlags.DeclaredOnly*/))
            {
                dict[fi] = fi.GetValue(attr);
            }
            return dict;
        }

        private static Dictionary<PropertyInfo, object> object_to_property_values(Object attr, bool concrete, bool mustBeWriteable)
        {
            var dict = new Dictionary<PropertyInfo, object>();
            Type tdict = attr.GetType();

            foreach (
                var fi in
                    tdict.GetProperties(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public
                        /*do NOT| BindingFlags.DeclaredOnly*/))
            {
                if (mustBeWriteable) if (!fi.CanWrite) continue;
                dict[fi] = fi.GetValue(attr, null);
            }
            return dict;
        }

        private static object cliIncr(object nextVal)
        {
            if (nextVal == null) return null;
            Type type = nextVal.GetType();
            return RecastObject(nextVal.GetType(), ((int) RecastObject(typeof (int), nextVal, typeof (int))) + 1, type);
        }
    }

    /// <summary>
    /// THIS IS ALL @TODO
    /// </summary>
    public class ClassBuilder
    {
        public class ParentClass
        {
        }

        public interface IImplementMe
        {
            double Value { get; set; }
        }


        public Type Build(string DataBuilderAssembly, string DataBuilderModule, string NewClass, Type parentClass,
            Type[] interfaces)
        {
            try
            {
                AssemblyName assemblyName = new AssemblyName(DataBuilderAssembly);
                AssemblyBuilder assemBuilder = Thread.GetDomain()
                    .DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run);
                ModuleBuilder moduleBuilder = assemBuilder.DefineDynamicModule(DataBuilderModule);
                TypeBuilder typeBuilder = moduleBuilder.DefineType(NewClass, TypeAttributes.Class, parentClass);
                AddOverrides(typeBuilder, parentClass);
                foreach (var interf in interfaces)
                {
                    typeBuilder.AddInterfaceImplementation(interf);
                    AddImpl(typeBuilder, interf);
                }
                Type type = typeBuilder.CreateType();

                return type;
            }
            catch (Exception e)
            {
                return null;
            }
        }

        /// <summary>Creates one constructor for each public constructor in the base class. Each constructor simply
        /// forwards its arguments to the base constructor, and matches the base constructor's signature.
        /// Supports optional values, and custom attributes on constructors and parameters.
        /// Does not support n-ary (variadic) constructors</summary>
        public static void CreatePassThroughConstuctors(TypeBuilder builder, Type baseType)
        {
            foreach (
                var constructor in
                    baseType.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance))
            {
                var parameters = constructor.GetParameters();
                if (parameters.Length > 0 && parameters.Last().IsDefined(typeof (ParamArrayAttribute), false))
                {
                    //throw new InvalidOperationException("Variadic constructors are not supported");
                    continue;
                }

                var parameterTypes = parameters.Select(p => p.ParameterType).ToArray();
                var r_CM = parameters.Select(p => p.GetRequiredCustomModifiers()).ToArray();
                var o_CM = parameters.Select(p => p.GetOptionalCustomModifiers()).ToArray();

                var ctor = builder.DefineConstructor(MethodAttributes.Public, constructor.CallingConvention,
                    parameterTypes, r_CM, o_CM);
                for (var i = 0; i < parameters.Length; ++i)
                {
                    var parameter = parameters[i];
                    var parameterBuilder = ctor.DefineParameter(i + 1, parameter.Attributes, parameter.Name);
                    if (((int) parameter.Attributes & (int) ParameterAttributes.HasDefault) != 0)
                    {
                        parameterBuilder.SetConstant(parameter.RawDefaultValue);
                    }
                    var pcustomAttributes = CustomAttributeDatas(parameter);
                    foreach (var attribute in BuildCustomAttributes(null /*parameter.GetCustomAttributesData*/))
                    {
                        parameterBuilder.SetCustomAttribute(attribute);
                    }
                }
                var customAttributes = CustomAttributeDatas(constructor);
                if (customAttributes != null)
                {
                    foreach (var attribute in BuildCustomAttributes(customAttributes))

                    {
                        ctor.SetCustomAttribute(attribute);
                    }
                }


                var emitter = ctor.GetILGenerator();
                emitter.Emit(OpCodes.Nop);

                // Load `this` and call base constructor with arguments
                emitter.Emit(OpCodes.Ldarg_0);
                for (var i = 1; i <= parameters.Length; ++i)
                {
                    emitter.Emit(OpCodes.Ldarg, i);
                }
                emitter.Emit(OpCodes.Call, constructor);

                emitter.Emit(OpCodes.Ret);
            }
        }

        private static IEnumerable<CustomAttributeData> CustomAttributeDatas(Object constructor)
        {
            IEnumerable<CustomAttributeData> customAttributes = null;
            try
            {
                customAttributes =
                    (IEnumerable<CustomAttributeData>)
                        constructor.GetType()
                            .GetMethod("GetCustomAttributesData")
                            .Invoke(constructor, new object[0]);
            }
            catch
            {
            }
            return customAttributes;
        }

        private static CustomAttributeBuilder[] BuildCustomAttributes(IEnumerable<CustomAttributeData> customAttributes)
        {
            return customAttributes.Select(attribute =>
                                           {
                                               var attributeArgs =
                                                   attribute.ConstructorArguments.Select(a => a.Value).ToArray();
                                               var namedPropertyInfos =
                                                   attribute.NamedArguments.Select(a => a.MemberInfo)
                                                       .OfType<PropertyInfo>()
                                                       .ToArray();
                                               var namedPropertyValues =
                                                   attribute.NamedArguments.Where(a => a.MemberInfo is PropertyInfo)
                                                       .Select(a => a.TypedValue.Value)
                                                       .ToArray();
                                               var namedFieldInfos =
                                                   attribute.NamedArguments.Select(a => a.MemberInfo)
                                                       .OfType<FieldInfo>()
                                                       .ToArray();
                                               var namedFieldValues =
                                                   attribute.NamedArguments.Where(a => a.MemberInfo is FieldInfo)
                                                       .Select(a => a.TypedValue.Value)
                                                       .ToArray();
                                               return new CustomAttributeBuilder(attribute.Constructor, attributeArgs,
                                                   namedPropertyInfos, namedPropertyValues, namedFieldInfos,
                                                   namedFieldValues);
                                           }).ToArray();
        }


        private void AddImpl(TypeBuilder builder, Type interf)
        {
            BuildProperty(builder, "Value", typeof (double));
        }

        private void AddOverrides(TypeBuilder builder, Type @class)
        {
            throw new NotImplementedException("AddOverrides");
        }

        private void BuildProperty(TypeBuilder typeBuilder, string name, Type type)
        {
            FieldBuilder field = typeBuilder.DefineField("m_" + name, type, FieldAttributes.Private);
            PropertyBuilder propertyBuilder = typeBuilder.DefineProperty(name, PropertyAttributes.None, type, null);

            //MethodAttributes getSetAttr = MethodAttributes.Public | MethodAttributes.HideBySig;
            MethodAttributes getSetAttr = MethodAttributes.Public |
                                          MethodAttributes.HideBySig | MethodAttributes.SpecialName |
                                          MethodAttributes.Virtual;

            MethodBuilder getter = typeBuilder.DefineMethod("get_" + name, getSetAttr, type, Type.EmptyTypes);

            ILGenerator getIL = getter.GetILGenerator();
            getIL.Emit(OpCodes.Ldarg_0);
            getIL.Emit(OpCodes.Ldfld, field);
            getIL.Emit(OpCodes.Ret);

            MethodBuilder setter = typeBuilder.DefineMethod("set_" + name, getSetAttr, null, new Type[] {type});

            ILGenerator setIL = setter.GetILGenerator();
            setIL.Emit(OpCodes.Ldarg_0);
            setIL.Emit(OpCodes.Ldarg_1);
            setIL.Emit(OpCodes.Stfld, field);
            setIL.Emit(OpCodes.Ret);


            propertyBuilder.SetGetMethod(getter);
            propertyBuilder.SetSetMethod(setter);
        }
    }

}