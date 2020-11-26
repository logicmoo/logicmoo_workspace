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
using System.Text;
using System.Linq;
using System.Web;

namespace Swicli.Library {

    public class ReflectedTypeInfo : MarshalByRefObject, IReflectedInfo, IGenericParameterProvider {

        //--- Class Fields ---
        private static readonly Dictionary<string, string> _typeNameAliases = new Dictionary<string, string>() {
            {"Boolean","bool"},
            {"Byte","byte"},
            {"SByte","sbyte"},
            {"Char","char"},
            {"Decimal","decimal"},
            {"Double","double"},
            {"Single","float"},
            {"Int32","int"},
            {"UInt32","uint"},
            {"Int64","long"},
            {"UInt64","ulong"},
            {"Object","object"},
            {"Int16","short"},
            {"UInt16","ushort"},
            {"String","string"},
            {"Void","void"},
        };

        //--- Fields ---
        public IList<ReflectedParameterTypeInfo> Interfaces = new List<ReflectedParameterTypeInfo>();
        public IList<ReflectedGenericParameterInfo> GenericParameters = new List<ReflectedGenericParameterInfo>();
        public readonly IList<ReflectedConstructorInfo> Constructors = new List<ReflectedConstructorInfo>();
        public readonly IList<ReflectedMethodInfo> Methods = new List<ReflectedMethodInfo>();
        public readonly IList<ReflectedPropertyInfo> Properties = new List<ReflectedPropertyInfo>();
        public readonly IList<ReflectedFieldInfo> Fields = new List<ReflectedFieldInfo>();
        public readonly IList<ReflectedEventInfo> Events = new List<ReflectedEventInfo>();
        private bool _checkedGenericParamInheritance;

        //--- Properties ---
        public string Assembly { get; set; }
        public string Namespace { get; set; }
        public string Name { get; set; }
        public string DisplayName { get { return GetRecursiveName(); } }
        public string LongDisplayName { get { return Namespace + "." + DisplayName; } }
        public string LocalSignature { get { return GetRecursiveLocalSignature(); } }
        public string Signature { get { return "T:" + Namespace + "." + LocalSignature; } }
        public string UriPath { get { return "//" + Namespace + "/" + System.Web.HttpUtility.UrlEncode(LocalSignature); } }
        public string FilePath { get { return Path.Combine(Namespace, LocalSignature + ".xml"); } }
        public string MemberPath { get { return Path.Combine(Namespace, LocalSignature); } }
        public ReflectedTypeInfo Type { get { return this; } }
        public bool IsAbstract { get; set; }
        public bool IsDelegate { get; set; }
        public TypeKind Kind { get; set; }
        public bool IsGeneric { get; set; }
        public bool IsStatic { get; set; }
        public bool IsSealed { get; set; }
        public ReflectedTypeInfo DeclaringType { get; set; }
        public ReflectedParameterTypeInfo BaseType { get; set; }
        public string CodeSignature { get { return BuildCodeSignature(); } }

        public string AliasedName {
            get {
                string alias;
                return _typeNameAliases.TryGetValue(Name, out alias) ? alias : Name;
            }
        }

        public IEnumerable<ReflectedGenericParameterInfo> LocalGenericParameters {
            get {
                CheckGenericParamInheritance();
                return GenericParameters.Where(x => !x.IsInherited);
            }
        }

        //--- Methods ---
        public override string ToString() { return "{" + Signature + "}"; }

        private string BuildCodeSignature() {
            var builder = new StringBuilder();
            builder.Append("public ");
            builder.Append(IsStatic ? "static " : "");
            builder.Append(IsAbstract ? "abstract " : "");
            builder.Append(IsSealed ? "sealed " : "");
            builder.Append(Kind.ToString().ToLower());
            builder.Append(" ");
            builder.Append(DisplayName);
            builder.Append("");
            return builder.ToString();
        }

        public ReflectedGenericParameterInfo GetGenericParameter(string name) {
            return GenericParameters.Where(x => x.Name == name && !x.IsInherited).FirstOrDefault();
        }

        private string GetRecursiveName() {
            return ((DeclaringType != null) ? DeclaringType.GetRecursiveName() + "." : "") + Name + GetGenericArgs();
        }

        private string GetRecursiveLocalSignature() {
            return ((DeclaringType != null) ? DeclaringType.GetRecursiveLocalSignature() + "." : "") + Name + GetGenericParamSignature();
        }

        private string GetGenericParamSignature() {
            var parameterCount = LocalGenericParameters.OrderBy(x => x.ParameterPosition).Count();
            return parameterCount == 0 ? "" : "`" + parameterCount;
        }

        private string GetGenericArgs() {
            var parameters = LocalGenericParameters.OrderBy(x => x.ParameterPosition).ToList();
            if(parameters.Count == 0) {
                return "";
            }
            var builder = new StringBuilder();
            builder.Append("<");
            bool first = true;
            foreach(var param in parameters) {
                if(!first) {
                    builder.Append(",");
                }
                builder.Append(param.Name);
                first = false;
            }
            builder.Append(">");
            return builder.ToString();
        }

        private void CheckGenericParamInheritance() {
            if(!_checkedGenericParamInheritance) {
                DetermineGenericParamInheritance();
            }
        }

        private int DetermineGenericParamInheritance() {
            if(DeclaringType == null) {
                return GenericParameters.Where(x => !x.IsInherited).Count();
            }
            var ancestorParamCount = DeclaringType.DetermineGenericParamInheritance();
            if(!_checkedGenericParamInheritance) {
                for(var i = 0; i < ancestorParamCount; i++) {
                    GenericParameters[i].IsInherited = true;
                }
            }
            _checkedGenericParamInheritance = true;
            return ancestorParamCount + GenericParameters.Where(x => !x.IsInherited).Count();
        }
    }

    public enum TypeKind {
        Interface,
        Struct,
        Class,
        Enum
    }
}