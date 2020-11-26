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
using System.Linq;
using System.Text;

namespace Swicli.Library {

    public class ReflectedMethodInfo : ReflectedMemberInfo, IGenericParameterProvider {

        //--- Fields ---
        public IList<ReflectedGenericParameterInfo> GenericParameters = new List<ReflectedGenericParameterInfo>();
        public IList<ReflectedParameterInfo> Parameters = new List<ReflectedParameterInfo>();

        //--- Properties ---
        protected override string Prefix { get { return "M"; } }
        public override string LocalSignature { get { return BuildSignature(); } }
        public override string DisplayName { get { return GetParameterizedName(); } }
        public bool IsOverride { get; set; }
        public bool IsGenericMethod { get; set; }
        public bool IsExtensionMethod { get; set; }

        //--- Methods ---
        public ReflectedGenericParameterInfo GetGenericParameter(string name) {
            return (from parameter in GenericParameters.Union(Type.GenericParameters)
                    where parameter.Name == name && !parameter.IsInherited
                    select parameter).FirstOrDefault();
        }

        protected override string BuildCodeSignature() {
            var builder = new StringBuilder();
            builder.Append(MemberAccess.ToString().ToLower());
            builder.Append(" ");
            builder.Append(IsStatic ? "static " : "");
            builder.Append(IsVirtual ? "virtual " : "");
            builder.Append(IsOverride ? "override " : "");
            builder.Append(IsNew ? "new " : "");
            builder.Append(ReturnType.DisplayName);
            builder.Append(" ");
            builder.Append(DisplayName);
            builder.Append(";");
            return builder.ToString();
        }

        private string BuildSignature() {
            var builder = new StringBuilder();
            builder.Append(Name);
            if(IsGenericMethod) {
                builder.AppendFormat("``{0}", GenericParameters.Count);
            }
            if(Parameters.Count > 0) {
                builder.Append("(");
                bool first = true;
                foreach(var parameter in Parameters) {
                    if(!first) {
                        builder.Append(",");
                    }
                    first = false;
                    builder.Append(parameter.Signature);
                }
                builder.Append(")");
            }
            return builder.ToString();
        }

        protected virtual string GetParameterizedName() {
            var builder = new StringBuilder();
            builder.Append(base.DisplayName);
            builder.Append("(");
            bool first = true;
            foreach(var parameter in Parameters) {
                if(!first) {
                    builder.Append(", ");
                }
                first = false;
                builder.Append(parameter.DisplayName);
            }
            builder.Append(")");
            return builder.ToString();
        }
    }
}