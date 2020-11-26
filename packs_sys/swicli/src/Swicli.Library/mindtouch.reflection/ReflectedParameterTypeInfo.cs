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

    public class ReflectedParameterTypeInfo : MarshalByRefObject {

        //--- Properties ---
        public bool IsGenericParameter { get; set; }
        public ReflectedGenericParameterInfo GenericParameter { get; set; }
        public ReflectedTypeInfo Type { get; set; }
        public bool IsGenericType { get; set; }
        public string DisplayName { get { return IsGenericParameter ? GenericParameter.Name : GetParametrizedType(); } }
        public IList<ReflectedParameterTypeInfo> Parameters = new List<ReflectedParameterTypeInfo>();
        public string Signature { get { return IsGenericParameter ? GenericParameter.ParameterSignature : GetParametrizedTypeSignature(); } }

        //--- Methods ---
        private string GetParametrizedTypeSignature() {
            var builder = new StringBuilder();
            builder.Append(Type.Namespace);
            GetNameSignatures(Type, builder, new Queue<ReflectedParameterTypeInfo>(Parameters));
            return builder.ToString();
        }

        private void GetNameSignatures(ReflectedTypeInfo type, StringBuilder builder, Queue<ReflectedParameterTypeInfo> parameterQueue) {
            if(type == null) {
                return;
            }
            GetNameSignatures(type.DeclaringType, builder, parameterQueue);
            builder.Append(".");
            builder.Append(type.Name);
            if(IsGenericType) {
                var paramCount = type.LocalGenericParameters.Count();
                if(paramCount > 0) {
                    builder.Append("{");
                    bool first = true;
                    for(int i = 0; i < paramCount; i++) {
                        if(!first) {
                            builder.Append(",");
                        }
                        var param = parameterQueue.Dequeue();
                        builder.Append(param.Signature);
                        first = false;
                    }
                    builder.Append("}");
                }
            }
        }

        private string GetParametrizedType() {
            var builder = new StringBuilder();
            GetParametrizedNames(Type, builder, new Queue<ReflectedParameterTypeInfo>(Parameters),true);
            return builder.ToString();
        }

        private void GetParametrizedNames(ReflectedTypeInfo type, StringBuilder builder, Queue<ReflectedParameterTypeInfo> parameterQueue, bool isEntry) {
            if(type == null) {
                return;
            }
            GetParametrizedNames(type.DeclaringType, builder, parameterQueue,false);
            builder.Append(type.AliasedName);
            if(IsGenericType) {
                var paramCount = type.LocalGenericParameters.Count();
                if(paramCount > 0) {
                    builder.Append("<");
                    bool first = true;
                    for(int i = 0; i < paramCount; i++) {
                        if(!first) {
                            builder.Append(",");
                        }
                        var param = parameterQueue.Dequeue();
                        builder.Append(param.DisplayName);
                        first = false;
                    }
                    builder.Append(">");
                }
            }
            if(!isEntry) {
                builder.Append(".");
            }
        }
    }
}