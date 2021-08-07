import { AnyT } from './type-descriptor';
export const LAZY_TYPE_EXPLANATION = `If the type is not yet defined, for example due to circular \
references, add '() => ' before it. E.g. @jsonMember(() => Foo)`;
export const MISSING_REFLECT_CONF_MSG = 'Make sure that you have both "experimentalDecorators"'
    + ' and "emitDecoratorMetadata" enabled in your tsconfig.json';
export function isDirectlySerializableNativeType(type) {
    return [Date, Number, String, Boolean].indexOf(type) !== -1;
}
export function isDirectlyDeserializableNativeType(type) {
    return [Number, String, Boolean].indexOf(type) !== -1;
}
export function isTypeTypedArray(type) {
    return [
        Float32Array,
        Float64Array,
        Int8Array,
        Uint8Array,
        Uint8ClampedArray,
        Int16Array,
        Uint16Array,
        Int32Array,
        Uint32Array,
    ].indexOf(type) !== -1;
}
export function isObject(value) {
    return typeof value === 'object';
}
export function shouldOmitParseString(jsonStr, expectedType) {
    const expectsTypesSerializedAsStrings = expectedType === String
        || expectedType === ArrayBuffer
        || expectedType === DataView;
    const hasQuotes = jsonStr.length >= 2
        && jsonStr[0] === '"'
        && jsonStr[jsonStr.length - 1] === '"';
    if (expectedType === Date) {
        const isNumber = !isNaN(Number(jsonStr.trim()));
        return !hasQuotes && !isNumber;
    }
    return expectsTypesSerializedAsStrings && !hasQuotes;
}
export function parseToJSObject(json, expectedType) {
    if (typeof json !== 'string' || shouldOmitParseString(json, expectedType)) {
        return json;
    }
    return JSON.parse(json);
}
export function isSubtypeOf(A, B) {
    return A === B || A.prototype instanceof B;
}
export function logError(message, ...optionalParams) {
    if (typeof console === 'object' && typeof console.error === 'function') {
        console.error(message, ...optionalParams);
    }
    else if (typeof console === 'object' && typeof console.log === 'function') {
        console.log(`ERROR: ${message}`, ...optionalParams);
    }
}
export function logMessage(message, ...optionalParams) {
    if (typeof console === 'object' && typeof console.log === 'function') {
        console.log(message, ...optionalParams);
    }
}
export function logWarning(message, ...optionalParams) {
    if (typeof console === 'object' && typeof console.warn === 'function') {
        console.warn(message, ...optionalParams);
    }
    else if (typeof console === 'object' && typeof console.log === 'function') {
        console.log(`WARNING: ${message}`, ...optionalParams);
    }
}
export function isValueDefined(value) {
    return !(typeof value === 'undefined' || value === null);
}
export function isInstanceOf(value, constructor) {
    if (constructor === AnyT.ctor) {
        return true;
    }
    else if (typeof value === 'number') {
        return constructor === Number;
    }
    else if (typeof value === 'string') {
        return constructor === String;
    }
    else if (typeof value === 'boolean') {
        return constructor === Boolean;
    }
    else if (isObject(value)) {
        return value instanceof constructor;
    }
    return false;
}
export const isReflectMetadataSupported = typeof Reflect === 'object' && typeof Reflect.getMetadata === 'function';
export function nameof(fn) {
    if (typeof fn.name === 'string') {
        return fn.name;
    }
    return 'undefined';
}
export function identity(arg) {
    return arg;
}
//# sourceMappingURL=helpers.js.map