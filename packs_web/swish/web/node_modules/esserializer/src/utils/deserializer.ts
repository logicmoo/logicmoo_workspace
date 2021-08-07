/**
 * Created by cshao on 2021-02-19.
 */


'use strict';

import {isClass, notObject} from './general';
import {CLASS_NAME_FIELD} from './constant';

const REGEXP_BEGIN_WITH_CLASS = /^\s*class\s+/;

function deserializeFromParsedObj(parsedObj:any, classes?:Array<any>): any {
  return deserializeFromParsedObjWithClassMapping(parsedObj, getClassMappingFromClassArray(classes));
}

function deserializeFromParsedObjWithClassMapping(parsedObj:any, classMapping:object): any {
  if (notObject(parsedObj)) {
    return parsedObj;
  }

  let deserializedObj:object = {};
  const classNameInParsedObj:string = parsedObj[CLASS_NAME_FIELD];
  if (classNameInParsedObj) {
    if (classNameInParsedObj === 'Date') {
      return typeof parsedObj.timestamp === 'number' ? new Date(parsedObj.timestamp) : null;
    }

    const classObj = classMapping[classNameInParsedObj];
    if (REGEXP_BEGIN_WITH_CLASS.test(classObj.toString())) {
      Object.setPrototypeOf(deserializedObj, classObj ? classObj.prototype : Object.prototype);
    } else {// It's class in function style.
      deserializedObj = Object.create(classObj.prototype.constructor.prototype);
      classObj.prototype.constructor.call(deserializedObj)
    }
  }

  for (const k in parsedObj) {
    if (k === CLASS_NAME_FIELD) {
      continue;
    }
    const v = parsedObj[k];

    if (Array.isArray(v)) {
      // @ts-ignore
      deserializedObj[k] = v.map((item) => {
        return deserializeFromParsedObjWithClassMapping(item, classMapping)
      });
    } else {
      // @ts-ignore
      deserializedObj[k] = deserializeFromParsedObjWithClassMapping(v, classMapping);
    }
  }
  return deserializedObj;
}

/**
 *
 * @param classes It's an array of Class definition. "any" is used in code only
 * because there is no TypeScript type definition for Class.
 */
function getClassMappingFromClassArray(classes:Array<any> = []): object {
  const classMapping:object = {};
  [Date].concat(classes).forEach((c) => {
    if (!isClass(c)) {
      return;
    }
    // @ts-ignore
    classMapping[c.name] = c;
  });

  return classMapping;
}

/**
 *
 * @param classObj It's a Class definition. "any" is used in code only
 * because there is no TypeScript type definition for Class.
 */
function getParentClassName(classObj:any): string {
  return classObj.prototype.__proto__.constructor.name;
}

export {
  deserializeFromParsedObj,
  deserializeFromParsedObjWithClassMapping,
  getClassMappingFromClassArray,
  getParentClassName
};