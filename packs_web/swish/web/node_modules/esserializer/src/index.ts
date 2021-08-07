/**
 * Created by cshao on 2021-02-09.
 */


'use strict';

import {getSerializeValueWithClassName} from './utils/serializer';
import {deserializeFromParsedObj} from './utils/deserializer';

class ESSerializer {
  /**
   * @param target
   */
  public static serialize(target:any): string {
    return JSON.stringify(getSerializeValueWithClassName(target));
  }

  /**
   * @param serializedText
   * @param classes [ExampleClassA, ExampleClassB, ...] It's an array of Class definition. "any" is used in code only
   * because there is no TypeScript type definition for Class.
   */
  public static deserialize(serializedText:string, classes?:Array<any>): any {
    return deserializeFromParsedObj(JSON.parse(serializedText), classes);
  }
}

module.exports = ESSerializer;