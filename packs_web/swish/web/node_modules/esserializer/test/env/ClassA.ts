/**
 * Created by cshao on 2021-02-19.
 */


'use strict';

import SuperClassA from './SuperClassA';

class ClassA extends SuperClassA{
  private _name: string;
  public age: number;

  constructor() {
    super();
    this._name = null;
    this.age = 28;
  }

  get name(): string {
    return this._name;
  }

  set name(value: string) {
    this._name = value;
  }

  static sayHello() {
    console.log('Hello');
  }

  getAge() {
    return this.age;
  }
}

export default ClassA;