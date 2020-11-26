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
using System.Linq;
using System.Reflection;

namespace Swicli.Library {

    public class ParameterBuilder {

        //--- Fields ---
        private readonly IGenericParameterProvider _genericParameterProvider;
        private readonly Func<Type, ReflectedTypeInfo> _getType;

        //--- Constructors ---
        public ParameterBuilder(IGenericParameterProvider genericParameterProvider, Func<Type, ReflectedTypeInfo> getType) {
            _genericParameterProvider = genericParameterProvider;
            _getType = getType;
        }

        //--- Methods ---
        public ReflectedParameterInfo BuildParameter(ParameterInfo parameter) {
            var isByRef = parameter.ParameterType.IsByRef;
            var type = isByRef ? parameter.ParameterType.GetElementType() : parameter.ParameterType;
            var parameterInfo = new ReflectedParameterInfo {
                Name = parameter.Name,
                ParameterPosition = parameter.Position,
                IsOut = parameter.IsOut,
                IsRef = !parameter.IsOut && isByRef,
                IsParams = parameter.GetCustomAttributes(typeof(ParamArrayAttribute), false).Any(),
                Type = BuildType(type)
            };
            return parameterInfo;
        }

        public ReflectedParameterTypeInfo BuildType(Type type) {
            if(type == null) {
                return null;
            }
            var typeInfo = new ReflectedParameterTypeInfo();
            if(type.IsGenericParameter) {
                typeInfo.IsGenericParameter = true;
                typeInfo.GenericParameter = _genericParameterProvider.GetGenericParameter(type.Name);
                return typeInfo;
            }
            typeInfo.Type = _getType(type);
            if(!type.IsGenericType) {
                return typeInfo;
            }
            typeInfo.IsGenericType = true;
            typeInfo.Parameters = (from parameter in type.GetGenericArguments() select BuildType(parameter)).ToList();
            return typeInfo;
        }
    }
}