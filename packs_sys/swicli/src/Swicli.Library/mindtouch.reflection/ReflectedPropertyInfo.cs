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

using System.Collections.Generic;
using System.Text;

namespace Swicli.Library {

    public class ReflectedPropertyInfo : ReflectedMemberInfo {

        //--- Fields ---
        public IList<ReflectedParameterInfo> IndexerParameters = new List<ReflectedParameterInfo>();

        //--- Properties ---
        protected override string Prefix { get { return "P"; } }
        public override string DisplayName { get { return !IsIndexer ? base.DisplayName : GetParameterizedName(); } }
        public override string LocalSignature { get { return BuildSignature(); } }
        public bool IsOverride { get; set; }
        public bool IsIndexer { get; set; }
        public MemberAccess SetAccess { get; set; }
        public MemberAccess GetAccess { get; set; }

        //--- Methods ---
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
            builder.Append(" { ");
            if(MemberAccess != GetAccess) {
                if(GetAccess == MemberAccess.Protected) {
                    builder.Append("protected get; ");
                }
            } else {
                builder.Append("get; ");
            }
            if(MemberAccess != SetAccess) {
                if(SetAccess == MemberAccess.Protected) {
                    builder.Append("protected set; ");
                }
            } else {
                builder.Append("set; ");
            }
            builder.Append("}");
            return builder.ToString();
        }

        private string BuildSignature() {
            if(!IsIndexer) {
                return base.LocalSignature;
            }
            var builder = new StringBuilder();
            builder.Append(Name);
            builder.Append("(");
            var first = true;
            foreach(var parameter in IndexerParameters) {
                if(!first) {
                    builder.Append(",");
                }
                first = false;
                builder.Append(parameter.Signature);
            }
            builder.Append(")");
            return builder.ToString();
        }

        private string GetParameterizedName() {
            var builder = new StringBuilder();
            builder.Append("this[");
            bool first = true;
            foreach(var parameter in IndexerParameters) {
                if(!first) {
                    builder.Append(", ");
                }
                first = false;
                builder.Append(parameter.DisplayName);
            }
            builder.Append("]");
            return builder.ToString();
        }
    }
}