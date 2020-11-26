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
using System.Text;

namespace Swicli.Library {

    public class ReflectedParameterInfo : MarshalByRefObject {

        //--- Properties ---
        public string Name { get; set; }
        public int ParameterPosition { get; set; }
        public bool IsOut { get; set; }
        public bool IsRef { get; set; }
        public bool IsParams { get; set; }
        public bool IsExtensionParameter { get; set; }
        public ReflectedParameterTypeInfo Type { get; set; }
        public string DisplayName { get { return BuildDisplayName(); } }
        public string Signature { get { return Type.Signature + (IsOut || IsRef ? "@" : ""); } }

        //--- Methods ---
        private string BuildDisplayName() {
            var builder = new StringBuilder();
            if(IsExtensionParameter) {
                builder.Append("this ");
            }
            if(IsOut) {
                builder.Append("out ");
            } else if(IsRef) {
                builder.Append("ref ");
            } else if(IsParams) {
                builder.Append("params ");
            }
            builder.AppendFormat("{0} {1}", Type.DisplayName, Name);
            return builder.ToString();
        }
    }
}