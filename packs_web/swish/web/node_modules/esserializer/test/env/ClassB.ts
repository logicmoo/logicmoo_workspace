/**
 * Created by cshao on 2021-02-19.
 */

'use strict';

import ClassC from './ClassC';

class ClassB {
  private _hobby: string;
  public toy: ClassC;

  constructor() {
    this._hobby = null;
    this.toy = new ClassC();
  }

  get hobby(): string {
    return this._hobby;
  }

  set hobby(value: string) {
    this._hobby = value;
  }
}

export default ClassB;