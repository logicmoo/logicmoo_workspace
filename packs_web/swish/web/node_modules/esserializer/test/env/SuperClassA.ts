/**
 * Created by cshao on 2021-02-19.
 */

'use strict';

class SuperClassA {
  private _size: number;
  constructor() {
    this._size = 0;
  }

  get size(): number {
    return this._size;
  }

  set size(value: number) {
    this._size = value;
  }
}

export default SuperClassA;