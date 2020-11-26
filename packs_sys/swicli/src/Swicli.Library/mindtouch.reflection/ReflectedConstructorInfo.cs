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

using System.IO;
using System.Text;

namespace Swicli.Library {

    public class ReflectedConstructorInfo : ReflectedMethodInfo {

        //--- Constructors ---
        public ReflectedConstructorInfo() { Name = "#ctor"; }

        //--- Properties ---
        public override string UriPath { get { return DeclaringType.UriPath + "/" + Type.Name; } }
        public override string FilePath { get { return Path.Combine(Type.MemberPath, Type.Name + ".xml"); } }

        //--- Methods ---
        protected override string GetParameterizedName() {
            var builder = new StringBuilder();
            builder.Append(Type.Name);
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

        protected override string BuildCodeSignature() {
            var builder = new StringBuilder();
            if(IsStatic) {
                builder.Append("static ");
            } else {
                builder.Append(MemberAccess.ToString().ToLower());
                builder.Append(" ");
            }
            builder.Append(DisplayName);
            builder.Append(";");
            return builder.ToString();
        }


    }
}