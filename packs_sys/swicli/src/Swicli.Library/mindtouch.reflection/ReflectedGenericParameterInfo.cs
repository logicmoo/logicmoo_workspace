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

namespace Swicli.Library {
    
    public class ReflectedGenericParameterInfo : MarshalByRefObject {

        //--- Fields ---
        public readonly IList<ReflectedParameterTypeInfo> Types = new List<ReflectedParameterTypeInfo>();

        //--- Properties ---
        public bool IsInherited { get; set; }
        public int ParameterPosition { get; set; }
        public string Name { get; set; }
        public bool MethodParameter { get; set; }
        public bool MustBeReferenceType { get; set; }
        public bool MustBeValueType { get; set; }
        public bool MustHaveDefaultConstructor { get; set; }
        public string ParameterSignature { get { return (MethodParameter ? "``" : "`") + ParameterPosition; } }
    }
}