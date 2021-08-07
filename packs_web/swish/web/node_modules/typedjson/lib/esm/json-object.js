import { JsonObjectMetadata } from './metadata';
import { extractOptionBase } from './options-base';
export function jsonObject(optionsOrTarget) {
    let options;
    if (typeof optionsOrTarget === 'function') {
        options = {};
    }
    else {
        options = optionsOrTarget !== null && optionsOrTarget !== void 0 ? optionsOrTarget : {};
    }
    function decorator(target) {
        const objectMetadata = JsonObjectMetadata.ensurePresentInPrototype(target.prototype);
        objectMetadata.isExplicitlyMarked = true;
        objectMetadata.onDeserializedMethodName = options.onDeserialized;
        objectMetadata.beforeSerializationMethodName = options.beforeSerialization;
        if (options.typeResolver != null) {
            objectMetadata.typeResolver = options.typeResolver;
        }
        if (options.typeHintEmitter != null) {
            objectMetadata.typeHintEmitter = options.typeHintEmitter;
        }
        objectMetadata.initializerCallback = options.initializer;
        if (options.name != null) {
            objectMetadata.name = options.name;
        }
        const optionsBase = extractOptionBase(options);
        if (optionsBase !== undefined) {
            objectMetadata.options = optionsBase;
        }
        if (options.knownTypes != null) {
            options.knownTypes
                .filter(knownType => Boolean(knownType))
                .forEach(knownType => objectMetadata.knownTypes.add(knownType));
        }
    }
    if (typeof optionsOrTarget === 'function') {
        decorator(optionsOrTarget);
    }
    else {
        return decorator;
    }
}
//# sourceMappingURL=json-object.js.map