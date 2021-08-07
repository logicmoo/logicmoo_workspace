import { isDirectlySerializableNativeType, isTypeTypedArray, logError, nameof } from './helpers';
export const METADATA_FIELD_KEY = '__typedJsonJsonObjectMetadataInformation__';
export class JsonObjectMetadata {
    constructor(classType) {
        this.dataMembers = new Map();
        this.knownTypes = new Set();
        this.knownTypesDeferred = [];
        this.isExplicitlyMarked = false;
        this.isHandledWithoutAnnotation = false;
        this.classType = classType;
    }
    static getJsonObjectName(ctor) {
        const metadata = JsonObjectMetadata.getFromConstructor(ctor);
        return metadata === undefined ? nameof(ctor) : nameof(metadata.classType);
    }
    static getFromConstructor(ctor) {
        const prototype = ctor.prototype;
        if (prototype == null) {
            return;
        }
        let metadata;
        if (Object.prototype.hasOwnProperty.call(prototype, METADATA_FIELD_KEY)) {
            metadata = prototype[METADATA_FIELD_KEY];
        }
        if ((metadata === null || metadata === void 0 ? void 0 : metadata.isExplicitlyMarked) === true) {
            return metadata;
        }
        if (JsonObjectMetadata.doesHandleWithoutAnnotation(ctor)) {
            const primitiveMeta = new JsonObjectMetadata(ctor);
            primitiveMeta.isExplicitlyMarked = true;
            return primitiveMeta;
        }
    }
    static ensurePresentInPrototype(prototype) {
        if (Object.prototype.hasOwnProperty.call(prototype, METADATA_FIELD_KEY)) {
            return prototype[METADATA_FIELD_KEY];
        }
        const objectMetadata = new JsonObjectMetadata(prototype.constructor);
        const parentMetadata = prototype[METADATA_FIELD_KEY];
        if (parentMetadata !== undefined) {
            parentMetadata.dataMembers.forEach((memberMetadata, propKey) => {
                objectMetadata.dataMembers.set(propKey, memberMetadata);
            });
            parentMetadata.knownTypes.forEach((knownType) => {
                objectMetadata.knownTypes.add(knownType);
            });
            objectMetadata.typeResolver = parentMetadata.typeResolver;
            objectMetadata.typeHintEmitter = parentMetadata.typeHintEmitter;
        }
        Object.defineProperty(prototype, METADATA_FIELD_KEY, {
            enumerable: false,
            configurable: false,
            writable: false,
            value: objectMetadata,
        });
        return objectMetadata;
    }
    static getKnownTypeNameFromType(constructor) {
        const metadata = JsonObjectMetadata.getFromConstructor(constructor);
        return metadata === undefined ? nameof(constructor) : nameof(metadata.classType);
    }
    static doesHandleWithoutAnnotation(ctor) {
        return isDirectlySerializableNativeType(ctor) || isTypeTypedArray(ctor)
            || ctor === DataView || ctor === ArrayBuffer;
    }
    processDeferredKnownTypes() {
        this.knownTypesDeferred.forEach(typeThunk => {
            typeThunk().getTypes().forEach(ctor => this.knownTypes.add(ctor));
        });
        this.knownTypesDeferred = [];
    }
}
export function injectMetadataInformation(prototype, propKey, metadata) {
    const decoratorName = `@jsonMember on ${nameof(prototype.constructor)}.${String(propKey)}`;
    if (typeof prototype === 'function') {
        logError(`${decoratorName}: cannot use a static property.`);
        return;
    }
    if (typeof prototype[propKey] === 'function') {
        logError(`${decoratorName}: cannot use a method property.`);
        return;
    }
    if (metadata == null
        || (metadata.type === undefined && metadata.deserializer === undefined)) {
        logError(`${decoratorName}: JsonMemberMetadata has unknown type.`);
        return;
    }
    const objectMetadata = JsonObjectMetadata.ensurePresentInPrototype(prototype);
    if (metadata.deserializer === undefined) {
        objectMetadata.knownTypesDeferred.push(metadata.type);
    }
    Object.keys(metadata)
        .forEach((key) => (metadata[key] === undefined) && delete metadata[key]);
    objectMetadata.dataMembers.set(metadata.name, metadata);
}
//# sourceMappingURL=metadata.js.map