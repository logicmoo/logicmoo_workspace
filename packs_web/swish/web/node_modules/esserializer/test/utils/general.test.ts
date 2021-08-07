/**
 * Created by cshao on 2021-02-19.
 */


'use strict';

import {notObject, isClass} from "../../src/utils/general";

import ClassA from '../env/ClassA';

describe('Test notObject', () => {
  test('null is not object', () => {
    expect(notObject(null)).toBe(true);
  });

  test('number is not object', () => {
    expect(notObject(42)).toBe(true);
  });

  test('boolean is not object', () => {
    expect(notObject(false)).toBe(true);
  });

  test('undefined is not object', () => {
    expect(notObject(undefined)).toBe(true);
  });

  test('string is not object', () => {
    expect(notObject('TEXT')).toBe(true);
  });

  test('object is object', () => {
    expect(notObject({name: 'Mike'})).toBe(false);
  });
});

describe('Test isClass', () => {
  test('imported class variable is class', () => {
    expect(isClass(ClassA)).toBe(true);
  });

  test('plain object is not class', () => {
    expect(isClass({name: 'Mike'})).toBe(false);
  });

  test('plain function is not class', () => {
    expect(isClass((x, y) => {return x+y;})).toBe(false);
  });
});
