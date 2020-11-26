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
using System.Linq;
//using log4net;

namespace Swicli.Library {

    public class IsolatedTypeInspector : ITypeInspector {

        //--- Class Fields ---
        //private static readonly ILog PrologCLR = LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

        //--- Fields ---
        private readonly AppDomain _assemblyInspectorDomain;
        private readonly ITypeInspector _typeInspector;
        private IEnumerable<ReflectedTypeInfo> _types;

        //--- Constructors ---
        public IsolatedTypeInspector() {
            Embedded.Debug("set up appdomain");
            var appDomainSetup = new AppDomainSetup();
            string currentBase = Path.GetFullPath(AppDomain.CurrentDomain.BaseDirectory);
            appDomainSetup.ApplicationBase = currentBase;
            _assemblyInspectorDomain = AppDomain.CreateDomain("assembly-inspector", null, appDomainSetup);
            _typeInspector = (ITypeInspector)_assemblyInspectorDomain.CreateInstanceAndUnwrap("mindtouch.reflection", "MindTouch.Reflection.TypeInspector");
            Embedded.Debug("app domain created");
        }

        //--- Properties ---
        public IEnumerable<ReflectedTypeInfo> Types {
            get {
                if(_types == null) {
                    Embedded.Debug("get types");
                    _types = _typeInspector.Types.ToList();
                    Embedded.Debug("got types");
                }
                return _types;
            }
        }

        //--- Methods ---
        public string InspectAssembly(string assemblyPath) {
            Embedded.Debug("start inspect");
            _types = null;
            var assembly = _typeInspector.InspectAssembly(assemblyPath);
            Embedded.Debug("finished inspect");
            return assembly;
        }

        public void Dispose() {
            AppDomain.Unload(_assemblyInspectorDomain);
        }
    }
}