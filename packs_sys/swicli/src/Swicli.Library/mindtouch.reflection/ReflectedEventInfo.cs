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

using System.Text;

namespace Swicli.Library {

    public class ReflectedEventInfo : ReflectedMemberInfo {
    
        //--- Properties ---
        protected override string Prefix { get { return "E"; } }
        public bool IsOverride { get; set; }
        public MemberAccess AddAccess { get; set; }
        public MemberAccess RemoveAccess { get; set; }

        //--- Methods ---
        protected override string BuildCodeSignature() {
            var builder = new StringBuilder();
            builder.Append(MemberAccess.ToString().ToLower());
            builder.Append(" ");
            builder.Append(IsStatic ? "static " : "");
            builder.Append("event ");
            builder.Append(ReturnType.DisplayName);
            builder.Append(" ");
            builder.Append(DisplayName);
            builder.Append(";");
            return builder.ToString();
        }
    }
}