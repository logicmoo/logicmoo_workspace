/**
 * Created by cshao on 2021-02-18.
 */


'use strict';

import {CLASS_NAME_FIELD} from './constant';
import {notObject} from './general';

function getSerializeValueWithClassName(target:any): any {
  if (notObject(target)) {
    return target;
  }

  if (Array.isArray(target)) {
    return target.map((t:any) => {
      return getSerializeValueWithClassName(t);
    });
  }

  const serializedObj = {};
  for (const k in target) {
    // @ts-ignore
    serializedObj[k] = getSerializeValueWithClassName(target[k]);
  }

  const className:string = target.__proto__.constructor.name;
  if (className !== 'Object') {
    // @ts-ignore
    serializedObj[CLASS_NAME_FIELD] = className;

    if (className === 'Date') {
      serializedObj['timestamp'] = (target as Date).getTime();
    }
  }
  return serializedObj;
}

export {
  getSerializeValueWithClassName
};
