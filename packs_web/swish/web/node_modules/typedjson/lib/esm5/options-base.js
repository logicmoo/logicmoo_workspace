import { __assign } from "tslib";
var kAllOptions = [
    'preserveNull',
];
export function extractOptionBase(from) {
    var options = Object.keys(from)
        .filter(function (key) { return kAllOptions.indexOf(key) > -1; })
        .reduce(function (obj, key) {
        obj[key] = from[key];
        return obj;
    }, {});
    return Object.keys(options).length > 0 ? options : undefined;
}
export function getDefaultOptionOf(key) {
    switch (key) {
        case 'preserveNull':
            return false;
    }
    return null;
}
export function getOptionValue(key, options) {
    if (options != null && options[key] != null) {
        return options[key];
    }
    return getDefaultOptionOf(key);
}
export function mergeOptions(existing, moreSpecific) {
    return moreSpecific == null
        ? existing
        : __assign(__assign({}, existing), moreSpecific);
}
//# sourceMappingURL=options-base.js.map