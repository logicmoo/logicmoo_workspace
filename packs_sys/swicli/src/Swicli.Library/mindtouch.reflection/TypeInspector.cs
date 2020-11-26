/*
 * MindTouch Core - open source enterprise collaborative networking
 * Copyright (c) 2006-2010 MindTouch Inc.
 * www.mindtouch.com  oss@mindtouch.com
 *
 * For community documentation and downloads visit wiki.developer.mindtouch.com;
 * please review the licensing section.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Linq;
using System.Runtime.CompilerServices;
//using log4net;

namespace Swicli.Library {
    public class TypeInspector : MarshalByRefObject, ITypeInspector {

        //--- Class Fields ---
        //private static readonly ILog PrologCLR = LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

        //--- Fields ---
        private readonly HashSet<Type> _builtTypes = new HashSet<Type>();
        private Queue<Type> _typesToBuild = new Queue<Type>();
        private readonly Dictionary<string, ReflectedTypeInfo> _seenTypes = new Dictionary<string, ReflectedTypeInfo>();
        private readonly List<ReflectedTypeInfo> _documentedTypes = new List<ReflectedTypeInfo>();

        //--- Properties ---
        public IEnumerable<ReflectedTypeInfo> Types {
            get {
                Embedded.Debug("getting types");
                return _documentedTypes;
            }
        }

        //--- Methods ---
        public string InspectAssembly(string assemblyPath) {
            Embedded.Debug("starting inspection");
            var assembly = Assembly.LoadFrom(assemblyPath);

            // handle any downstream dependencies that the preload doesn't handle
            AppDomain.CurrentDomain.AssemblyResolve += (s, args) => {
                Assembly dependency = null;
                try {
                    var assemblyName = new AssemblyName(args.Name);
                    var assemblyFile = Path.Combine(Path.GetDirectoryName(assemblyPath), assemblyName.Name + ".dll");
                    if(File.Exists(assemblyFile)) {
                        dependency = Assembly.LoadFrom(assemblyFile);
                    }
                } catch { }
                if(dependency == null) {
                    dependency = Assembly.Load(args.Name);
                }
                return dependency;
            };
            var dependentAssemblies = new List<Assembly>();

            // pre load dependencies that will be inspected
            foreach(var dependencyName in assembly.GetReferencedAssemblies()) {
                Assembly dependency = null;
                try {
                    var assemblyFile = Path.Combine(Path.GetDirectoryName(assemblyPath), dependencyName.Name + ".dll");
                    if(File.Exists(assemblyFile)) {
                        dependency = Assembly.LoadFrom(assemblyFile);
                    }
                } catch { }
                if(dependency == null) {
                    dependency = Assembly.Load(dependencyName.FullName);
                }
                dependentAssemblies.Add(dependency);
            }

            // queue up all types that should be visible
            _typesToBuild = new Queue<Type>(assembly.GetExportedTypes());
            Embedded.Debug("discover types");

            foreach(var dependency in dependentAssemblies) {
                foreach(var type in dependency.GetExportedTypes()) {
                    _typesToBuild.Enqueue(type);
                }
            }
            Embedded.Debug("build types");
            foreach(var type in GetUnBuiltTypes()) {
                BuildType(type);
            }
            Embedded.Debug("post build markup");
            foreach(var typeInfo in _documentedTypes) {
                foreach(var methodInfo in typeInfo.Methods.Where(x => x.IsInherited)) {
                    var newMethod = typeInfo.Methods.Where(x => !x.IsOverride && !x.IsInherited && x.LocalSignature == methodInfo.LocalSignature).FirstOrDefault();
                    if(newMethod == null) {
                        continue;
                    }
                    methodInfo.IsHidden = true;
                    newMethod.IsNew = true;
                }
                foreach(var propertyInfo in typeInfo.Properties.Where(x => x.IsInherited)) {
                    var newProperty = typeInfo.Methods.Where(x => !x.IsOverride && !x.IsInherited && x.LocalSignature == propertyInfo.LocalSignature).FirstOrDefault();
                    if(newProperty == null) {
                        continue;
                    }
                    propertyInfo.IsHidden = true;
                    newProperty.IsNew = true;
                }
            }
            Embedded.Debug("finished inspection");
            return assembly.GetName().Name;
        }

        private IEnumerable<Type> GetUnBuiltTypes() {
            while(_typesToBuild.Count > 0) {
                var type = _typesToBuild.Dequeue();
                if(!_builtTypes.Contains(type)) {
                    yield return type;
                }
            }
        }

        private void BuildType(Type type) {
            GetType(type, true);
        }

        private ReflectedTypeInfo GetType(Type type) {
            return GetType(type, false);
        }

        private ReflectedTypeInfo GetType(Type type, bool build) {
            if(type == null) {
                return null;
            }
            var qualifiedName = type.ToString();
            if(type.IsGenericType) {
                qualifiedName = type.GetGenericTypeDefinition().ToString();
            }
            ReflectedTypeInfo typeInfo = null;
            if(!_seenTypes.TryGetValue(qualifiedName, out typeInfo)) {
                if(!type.IsArray) {
                    if(type.IsGenericType) {
                        _typesToBuild.Enqueue(type.GetGenericTypeDefinition());
                    } else {
                        _typesToBuild.Enqueue(type);
                    }
                }
                var simpleName = type.Name;
                if(type.IsGenericType) {
                    var tickIndex = type.Name.IndexOf('`');
                    if(tickIndex > 0) {
                        simpleName = type.Name.Substring(0, tickIndex);
                    }
                }
                typeInfo = new ReflectedTypeInfo() {
                    Assembly = type.Assembly.GetName().Name,
                    Name = simpleName,
                    Namespace = type.Namespace,
                    IsDelegate = typeof(Delegate).IsAssignableFrom(type),
                    Kind = type.IsInterface 
                        ? TypeKind.Interface 
                        : (type.IsValueType && !type.IsClass) 
                            ? TypeKind.Struct 
                            : type.IsEnum ? TypeKind.Enum : TypeKind.Class,
                    IsAbstract = type.IsAbstract && !type.IsSealed,
                    IsStatic = type.IsAbstract && type.IsSealed,
                    IsSealed = type.IsSealed && !type.IsAbstract
                };
                if(type.DeclaringType != null) {
                    typeInfo.DeclaringType = GetType(type.DeclaringType);
                }
                _seenTypes[qualifiedName] = typeInfo;
            }
            if(build) {
                BuildType(type, typeInfo);
            }
            return typeInfo;
        }

        private void BuildType(Type type, ReflectedTypeInfo typeInfo) {
            _builtTypes.Add(type);
            _documentedTypes.Add(typeInfo);
            var typeParameterBuilder = new ParameterBuilder(typeInfo, GetType);
            if(type.IsGenericType) {
                var genericTypeArguments = type.GetGenericArguments();
                typeInfo.IsGeneric = true;
                typeInfo.GenericParameters = GetGenericParameters(genericTypeArguments, false);

                // since generic parameter constraints can refer to each other and parameterBuilder.BuildType gets these references
                // by retrieving the registered instance of the type, building the types for GenericParameters has to occur after
                // the skeleton is built and assigned to the typeInfo instance.
                for(int i = 0; i < genericTypeArguments.Length; i++) {
                    foreach(var constraintType in genericTypeArguments[i].GetGenericParameterConstraints()) {
                        if(constraintType == typeof(ValueType)) {
                            continue;
                        }
                        typeInfo.GenericParameters[i].Types.Add(typeParameterBuilder.BuildType(constraintType));
                    }
                }
            }
            typeInfo.BaseType = typeParameterBuilder.BuildType(type.BaseType);
            typeInfo.Interfaces = (from i in type.GetInterfaces()
                                   where !i.Name.StartsWith("_")
                                   select typeParameterBuilder.BuildType(i)).ToList();
            if(typeInfo.IsDelegate) {
                return;
            }

            // find constructors
            foreach(var constructor in type.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)) {
                if(constructor.IsPrivate || constructor.IsAssembly) {
                    continue;
                }
                var isProtected = constructor.IsFamily && !constructor.IsPublic && !constructor.IsPrivate;
                var constructorInfo = new ReflectedConstructorInfo() {
                    Type = typeInfo,
                    IsStatic = constructor.IsStatic,
                    MemberAccess = isProtected ? MemberAccess.Protected : MemberAccess.Public,
                    DeclaringType = GetType(constructor.DeclaringType),
                };
                constructorInfo.Parameters = GetParameters(constructor.GetParameters(), typeParameterBuilder);
                typeInfo.Constructors.Add(constructorInfo);
            }

            // find methods
            foreach(var method in type.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)) {
                if(method.IsSpecialName || method.IsPrivate || method.IsAssembly || method.Name == "Finalize") {
                    continue;
                }
                var methodInfo = BuildMethodInfo(typeInfo, method);
                typeInfo.Methods.Add(methodInfo);
            }

            // find properties
            foreach(var property in type.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)) {
                var getMethod = property.GetGetMethod(true);
                var setMethod = property.GetSetMethod(true);
                if((getMethod == null || getMethod.IsPrivate || getMethod.IsAssembly)
                    && (setMethod == null || setMethod.IsPrivate || setMethod.IsAssembly)
                ) {
                    continue;
                }
                var accessorMethod = getMethod == null || getMethod.IsPrivate ? setMethod : getMethod;
                var methodInfo = BuildMethodInfo(typeInfo, accessorMethod);
                var propertyInfo = new ReflectedPropertyInfo() {
                    Name = property.Name,
                    IsIndexer = property.Name == "Item",
                    Type = typeInfo,
                    DeclaringType = GetType(property.DeclaringType),
                    ReturnType = typeParameterBuilder.BuildType(property.PropertyType),
                    IsOverride = methodInfo.IsOverride,
                    IsStatic = methodInfo.IsStatic,
                    IsVirtual = methodInfo.IsVirtual,
                    GetAccess = DetermineAccess(getMethod),
                    SetAccess = DetermineAccess(setMethod)
                };
                propertyInfo.MemberAccess = propertyInfo.GetAccess == MemberAccess.Public || propertyInfo.SetAccess == MemberAccess.Public
                    ? MemberAccess.Public
                    : MemberAccess.Protected;
                if(propertyInfo.IsIndexer) {
                    methodInfo.Name = "Item";
                    propertyInfo.IndexerParameters = GetParameters(property.GetIndexParameters(), typeParameterBuilder);
                }
                typeInfo.Properties.Add(propertyInfo);
            }

            // find fields
            foreach(var field in type.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)) {
                if(field.IsPrivate || field.IsAssembly) {
                    continue;
                }
                var isProtected = field.IsFamily && !field.IsPublic && !field.IsPrivate;
                var fieldInfo = new ReflectedFieldInfo() {
                    Name = field.Name,
                    Type = typeInfo,
                    IsStatic = field.IsStatic,
                    MemberAccess = isProtected ? MemberAccess.Protected : MemberAccess.Public,
                    DeclaringType = GetType(field.DeclaringType),
                    ReturnType = typeParameterBuilder.BuildType(field.FieldType)
                };
                typeInfo.Fields.Add(fieldInfo);
            }

            // find events
            foreach(var ev in type.GetEvents(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)) {
                var addMethod = ev.GetAddMethod(true);
                var removeMethod = ev.GetRemoveMethod(true);
                if((addMethod == null || addMethod.IsPrivate || addMethod.IsAssembly)
                    && (removeMethod == null || removeMethod.IsPrivate || removeMethod.IsAssembly)
                ) {
                    continue;
                }
                var accessorMethod = addMethod == null || addMethod.IsPrivate ? removeMethod : addMethod;
                var methodInfo = BuildMethodInfo(typeInfo, accessorMethod);
                var eventInfo = new ReflectedEventInfo() {
                    Name = ev.Name,
                    Type = typeInfo,
                    DeclaringType = GetType(ev.DeclaringType),
                    ReturnType = typeParameterBuilder.BuildType(ev.EventHandlerType),
                    IsOverride = methodInfo.IsOverride,
                    IsStatic = methodInfo.IsStatic,
                    IsVirtual = methodInfo.IsVirtual,
                    AddAccess = DetermineAccess(addMethod),
                    RemoveAccess = DetermineAccess(removeMethod)
                };
                typeInfo.Events.Add(eventInfo);
            }
        }

        private MemberAccess DetermineAccess(MethodInfo method) {
            if(method == null) {
                return MemberAccess.Private;
            }
            return method.IsPrivate
                ? MemberAccess.Private
                : method.IsFamily && !method.IsPublic && !method.IsPrivate
                    ? MemberAccess.Protected
                    : MemberAccess.Public;
        }

        private ReflectedMethodInfo BuildMethodInfo(ReflectedTypeInfo typeInfo, MethodInfo method) {
            var isNewSlot = ((method.Attributes & MethodAttributes.NewSlot) == MethodAttributes.NewSlot);
            var isReusedSlot = ((method.Attributes & MethodAttributes.ReuseSlot) == MethodAttributes.ReuseSlot);
            var isOverride = method.IsVirtual && !isNewSlot && isReusedSlot;
            var methodInfo = new ReflectedMethodInfo() {
                Name = method.Name,
                Type = typeInfo,
                DeclaringType = GetType(method.DeclaringType),
                IsOverride = isOverride,
                MemberAccess = DetermineAccess(method),
                IsVirtual = method.IsVirtual && !isOverride,
                IsStatic = method.IsStatic,
                IsExtensionMethod = method.GetCustomAttributes(typeof(ExtensionAttribute), false).Any()
            };
            var parameterBuilder = new ParameterBuilder(methodInfo, GetType);
            if(method.IsGenericMethod) {
                var genericArgs = method.GetGenericArguments();
                methodInfo.IsGenericMethod = true;
                methodInfo.GenericParameters = GetGenericParameters(genericArgs, true);
                for(int i = 0; i < genericArgs.Length; i++) {
                    foreach(var constraintType in genericArgs[i].GetGenericParameterConstraints()) {
                        if(constraintType == typeof(ValueType)) {
                            continue;
                        }
                        methodInfo.GenericParameters[i].Types.Add(parameterBuilder.BuildType(constraintType));
                    }
                }
            }
            methodInfo.ReturnType = parameterBuilder.BuildType(method.ReturnType);
            methodInfo.Parameters = GetParameters(method.GetParameters(), parameterBuilder);
            if(methodInfo.IsExtensionMethod) {
                methodInfo.Parameters[0].IsExtensionParameter = true;
            }
            return methodInfo;
        }

        private IList<ReflectedParameterInfo> GetParameters(ParameterInfo[] parameters, ParameterBuilder builder) {
            var parameterList = new List<ReflectedParameterInfo>();
            foreach(var parameter in parameters) {
                var param = builder.BuildParameter(parameter);
                parameterList.Add(param);
            }
            return parameterList;
        }

        private List<ReflectedGenericParameterInfo> GetGenericParameters(Type[] parameters, bool isMethod) {
            var list = new List<ReflectedGenericParameterInfo>();
            foreach(var genericParameter in parameters) {
                var constraints = genericParameter.GenericParameterAttributes;
                var parameterInfo = new ReflectedGenericParameterInfo {
                    Name = genericParameter.Name,
                    MethodParameter = isMethod,
                    ParameterPosition = genericParameter.GenericParameterPosition,
                    MustBeReferenceType = GetConstraint(constraints, GenericParameterAttributes.ReferenceTypeConstraint),
                    MustBeValueType = GetConstraint(constraints, GenericParameterAttributes.NotNullableValueTypeConstraint),
                    MustHaveDefaultConstructor = GetConstraint(constraints, GenericParameterAttributes.DefaultConstructorConstraint),
                };
                if(parameterInfo.MustBeValueType) {
                    parameterInfo.MustHaveDefaultConstructor = false;
                }
                list.Add(parameterInfo);
            }
            return list;
        }

        private bool GetConstraint(GenericParameterAttributes constraintMask, GenericParameterAttributes constraint) {
            return (constraintMask & constraint) == constraint;
        }

        public void Dispose() { }
    }
}