import { LAZY_TYPE_EXPLANATION } from './helpers';
export class TypeDescriptor {
    constructor(ctor) {
        this.ctor = ctor;
    }
    getTypes() {
        return [this.ctor];
    }
    hasFriendlyName() {
        return this.ctor.name !== 'Object';
    }
}
export class ConcreteTypeDescriptor extends TypeDescriptor {
    constructor(ctor) {
        super(ctor);
    }
}
export class GenericTypeDescriptor extends TypeDescriptor {
    constructor(ctor) {
        super(ctor);
    }
}
export class ArrayTypeDescriptor extends GenericTypeDescriptor {
    constructor(elementType) {
        super(Array);
        this.elementType = elementType;
    }
    getTypes() {
        return super.getTypes().concat(this.elementType.getTypes());
    }
}
export function ArrayT(elementType) {
    return new ArrayTypeDescriptor(ensureTypeDescriptor(elementType));
}
export class SetTypeDescriptor extends GenericTypeDescriptor {
    constructor(elementType) {
        super(Set);
        this.elementType = elementType;
    }
    getTypes() {
        return super.getTypes().concat(this.elementType.getTypes());
    }
}
export function SetT(elementType) {
    return new SetTypeDescriptor(ensureTypeDescriptor(elementType));
}
export class MapTypeDescriptor extends GenericTypeDescriptor {
    constructor(keyType, valueType, options) {
        super(Map);
        this.keyType = keyType;
        this.valueType = valueType;
        this.options = options;
    }
    getTypes() {
        return super.getTypes().concat(this.keyType.getTypes(), this.valueType.getTypes());
    }
    getCompleteOptions() {
        var _a, _b;
        return {
            shape: (_b = (_a = this.options) === null || _a === void 0 ? void 0 : _a.shape) !== null && _b !== void 0 ? _b : 0,
        };
    }
}
export function MapT(keyType, valueType, options) {
    return new MapTypeDescriptor(ensureTypeDescriptor(keyType), ensureTypeDescriptor(valueType), options);
}
export const AnyT = new ConcreteTypeDescriptor(() => undefined);
export function isTypelike(type) {
    return type != null && (typeof type === 'function' || type instanceof TypeDescriptor);
}
export function isTypeThunk(candidate) {
    return typeof candidate === 'function' && candidate.name === '';
}
export function ensureTypeDescriptor(type) {
    return type instanceof TypeDescriptor ? type : new ConcreteTypeDescriptor(type);
}
export function ensureTypeThunk(typeThunkOrSerializable, decoratorName) {
    if (typeThunkOrSerializable == null) {
        throw new Error(`No type given on ${decoratorName}. ${LAZY_TYPE_EXPLANATION}`);
    }
    if (isTypeThunk(typeThunkOrSerializable)) {
        return typeThunkOrSerializable;
    }
    return () => typeThunkOrSerializable;
}
//# sourceMappingURL=type-descriptor.js.map