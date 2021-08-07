/**
 * Created by cshao on 2021-02-19.
 */

'use strict';

class ClassC {
  private _height: number;
  constructor() {
    this._height = 19;
  }

  get height(): number {
    return this._height;
  }

  set height(value: number) {
    this._height = value;
  }
}

export default ClassC;