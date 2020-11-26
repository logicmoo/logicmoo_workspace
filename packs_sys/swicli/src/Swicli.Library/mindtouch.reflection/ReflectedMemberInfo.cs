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
using System.IO;

namespace Swicli.Library {

    public abstract class ReflectedMemberInfo : MarshalByRefObject, IReflectedInfo {
    
        //--- Properties ---
        protected abstract string Prefix { get; }
        public virtual string LocalSignature { get { return Name; } }
        public virtual string Name { get; set; }
        public ReflectedTypeInfo Type { get; set; }
        public ReflectedTypeInfo DeclaringType { get; set; }
        public MemberAccess MemberAccess { get; set; }
        public virtual bool IsInherited { get { return Type != DeclaringType; } }
        public bool IsNew { get; set; }
        public bool IsHidden { get; set; }
        public string Assembly { get { return Type.Assembly; } }
        public virtual string DisplayName { get { return Name; } }
        public string LongDisplayName { get { return Type.DisplayName + "." + DisplayName; } }
        public string Signature { get { return Prefix + ":" + DeclaringType.Namespace + "." + DeclaringType.LocalSignature + "." + LocalSignature; } }
        public virtual string UriPath { get { return DeclaringType.UriPath + "/" + Name; } }
        public virtual string FilePath { get { return Path.Combine(Type.MemberPath, Name + ".xml"); } }
        public ReflectedParameterTypeInfo ReturnType { get; set; }
        public bool IsVirtual { get; set; }
        public bool IsStatic { get; set; }
        public string CodeSignature { get { return BuildCodeSignature(); } }

        //--- Methods ---
        protected abstract string BuildCodeSignature();
        public override string ToString() { return "{" + Signature + "}"; }
    }
}