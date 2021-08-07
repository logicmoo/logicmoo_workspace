/**
 * Created by cshao on 2021-02-19.
 */

'use strict';

function notObject(target:any): boolean {
  return target === null || typeof target !== 'object';
}

function isSupportedBuiltinClass(target: any): boolean {
  return [Date].indexOf(target) >= 0;
}

function isClass(target:any): boolean {
  if (isSupportedBuiltinClass(target)) {
    return true;
  }

  // Adopt solution from https://stackoverflow.com/a/46759625/707451
  try {
    Reflect.construct(String, [], target);
  } catch (e) {
    return false;
  }
  return true;
}

export {
  notObject,
  isClass
};