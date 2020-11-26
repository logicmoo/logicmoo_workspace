/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
import { Disposable } from '../../../base/common/lifecycle';
import { IBulkEditService } from '../../browser/services/bulkEditService';
import { ICodeEditorService } from '../../browser/services/codeEditorService';
import { IEditorWorkerService } from '../../common/services/editorWorkerService';
import { EditorWorkerServiceImpl } from '../../common/services/editorWorkerServiceImpl';
import { IModeService } from '../../common/services/modeService';
import { ModeServiceImpl } from '../../common/services/modeServiceImpl';
import { IModelService } from '../../common/services/modelService';
import { ModelServiceImpl } from '../../common/services/modelServiceImpl';
import { ITextResourceConfigurationService, ITextResourcePropertiesService } from '../../common/services/textResourceConfigurationService';
import { SimpleBulkEditService, SimpleConfigurationService, SimpleDialogService, SimpleNotificationService, SimpleEditorProgressService, SimpleResourceConfigurationService, SimpleResourcePropertiesService, SimpleUriLabelService, SimpleWorkspaceContextService, StandaloneCommandService, StandaloneKeybindingService, StandaloneTelemetryService, SimpleLayoutService } from './simpleServices';
import { StandaloneCodeEditorServiceImpl } from './standaloneCodeServiceImpl';
import { StandaloneThemeServiceImpl } from './standaloneThemeServiceImpl';
import { IStandaloneThemeService } from '../common/standaloneThemeService';
import { IMenuService } from '../../../platform/actions/common/actions';
import { ICommandService } from '../../../platform/commands/common/commands';
import { IConfigurationService } from '../../../platform/configuration/common/configuration';
import { ContextKeyService } from '../../../platform/contextkey/browser/contextKeyService';
import { IContextKeyService } from '../../../platform/contextkey/common/contextkey';
import { ContextMenuService } from '../../../platform/contextview/browser/contextMenuService';
import { IContextMenuService, IContextViewService } from '../../../platform/contextview/browser/contextView';
import { ContextViewService } from '../../../platform/contextview/browser/contextViewService';
import { IDialogService } from '../../../platform/dialogs/common/dialogs';
import { IInstantiationService, createDecorator } from '../../../platform/instantiation/common/instantiation';
import { InstantiationService } from '../../../platform/instantiation/common/instantiationService';
import { ServiceCollection } from '../../../platform/instantiation/common/serviceCollection';
import { IKeybindingService } from '../../../platform/keybinding/common/keybinding';
import { ILabelService } from '../../../platform/label/common/label';
import { IListService, ListService } from '../../../platform/list/browser/listService';
import { ILogService, NullLogService } from '../../../platform/log/common/log';
import { MarkerService } from '../../../platform/markers/common/markerService';
import { IMarkerService } from '../../../platform/markers/common/markers';
import { INotificationService } from '../../../platform/notification/common/notification';
import { IEditorProgressService } from '../../../platform/progress/common/progress';
import { IStorageService, InMemoryStorageService } from '../../../platform/storage/common/storage';
import { ITelemetryService } from '../../../platform/telemetry/common/telemetry';
import { IThemeService } from '../../../platform/theme/common/themeService';
import { IWorkspaceContextService } from '../../../platform/workspace/common/workspace';
import { MenuService } from '../../../platform/actions/common/menuService';
import { IMarkerDecorationsService } from '../../common/services/markersDecorationService';
import { MarkerDecorationsService } from '../../common/services/markerDecorationsServiceImpl';
import { IAccessibilityService } from '../../../platform/accessibility/common/accessibility';
import { ILayoutService } from '../../../platform/layout/browser/layoutService';
import { getSingletonServiceDescriptors } from '../../../platform/instantiation/common/extensions';
import { AccessibilityService } from '../../../platform/accessibility/common/accessibilityService';
export var StaticServices;
(function (StaticServices) {
    var _serviceCollection = new ServiceCollection();
    var LazyStaticService = /** @class */ (function () {
        function LazyStaticService(serviceId, factory) {
            this._serviceId = serviceId;
            this._factory = factory;
            this._value = null;
        }
        Object.defineProperty(LazyStaticService.prototype, "id", {
            get: function () { return this._serviceId; },
            enumerable: true,
            configurable: true
        });
        LazyStaticService.prototype.get = function (overrides) {
            if (!this._value) {
                if (overrides) {
                    this._value = overrides[this._serviceId.toString()];
                }
                if (!this._value) {
                    this._value = this._factory(overrides);
                }
                if (!this._value) {
                    throw new Error('Service ' + this._serviceId + ' is missing!');
                }
                _serviceCollection.set(this._serviceId, this._value);
            }
            return this._value;
        };
        return LazyStaticService;
    }());
    StaticServices.LazyStaticService = LazyStaticService;
    var _all = [];
    function define(serviceId, factory) {
        var r = new LazyStaticService(serviceId, factory);
        _all.push(r);
        return r;
    }
    function init(overrides) {
        // Create a fresh service collection
        var result = new ServiceCollection();
        // make sure to add all services that use `registerSingleton`
        for (var _i = 0, _a = getSingletonServiceDescriptors(); _i < _a.length; _i++) {
            var _b = _a[_i], id = _b[0], descriptor = _b[1];
            result.set(id, descriptor);
        }
        // Initialize the service collection with the overrides
        for (var serviceId in overrides) {
            if (overrides.hasOwnProperty(serviceId)) {
                result.set(createDecorator(serviceId), overrides[serviceId]);
            }
        }
        // Make sure the same static services are present in all service collections
        _all.forEach(function (service) { return result.set(service.id, service.get(overrides)); });
        // Ensure the collection gets the correct instantiation service
        var instantiationService = new InstantiationService(result, true);
        result.set(IInstantiationService, instantiationService);
        return [result, instantiationService];
    }
    StaticServices.init = init;
    StaticServices.instantiationService = define(IInstantiationService, function () { return new InstantiationService(_serviceCollection, true); });
    var configurationServiceImpl = new SimpleConfigurationService();
    StaticServices.configurationService = define(IConfigurationService, function () { return configurationServiceImpl; });
    StaticServices.resourceConfigurationService = define(ITextResourceConfigurationService, function () { return new SimpleResourceConfigurationService(configurationServiceImpl); });
    StaticServices.resourcePropertiesService = define(ITextResourcePropertiesService, function () { return new SimpleResourcePropertiesService(configurationServiceImpl); });
    StaticServices.contextService = define(IWorkspaceContextService, function () { return new SimpleWorkspaceContextService(); });
    StaticServices.labelService = define(ILabelService, function () { return new SimpleUriLabelService(); });
    StaticServices.telemetryService = define(ITelemetryService, function () { return new StandaloneTelemetryService(); });
    StaticServices.dialogService = define(IDialogService, function () { return new SimpleDialogService(); });
    StaticServices.notificationService = define(INotificationService, function () { return new SimpleNotificationService(); });
    StaticServices.markerService = define(IMarkerService, function () { return new MarkerService(); });
    StaticServices.modeService = define(IModeService, function (o) { return new ModeServiceImpl(); });
    StaticServices.standaloneThemeService = define(IStandaloneThemeService, function () { return new StandaloneThemeServiceImpl(); });
    StaticServices.logService = define(ILogService, function () { return new NullLogService(); });
    StaticServices.modelService = define(IModelService, function (o) { return new ModelServiceImpl(StaticServices.configurationService.get(o), StaticServices.resourcePropertiesService.get(o), StaticServices.standaloneThemeService.get(o), StaticServices.logService.get(o)); });
    StaticServices.markerDecorationsService = define(IMarkerDecorationsService, function (o) { return new MarkerDecorationsService(StaticServices.modelService.get(o), StaticServices.markerService.get(o)); });
    StaticServices.codeEditorService = define(ICodeEditorService, function (o) { return new StandaloneCodeEditorServiceImpl(StaticServices.standaloneThemeService.get(o)); });
    StaticServices.editorProgressService = define(IEditorProgressService, function () { return new SimpleEditorProgressService(); });
    StaticServices.storageService = define(IStorageService, function () { return new InMemoryStorageService(); });
    StaticServices.editorWorkerService = define(IEditorWorkerService, function (o) { return new EditorWorkerServiceImpl(StaticServices.modelService.get(o), StaticServices.resourceConfigurationService.get(o), StaticServices.logService.get(o)); });
})(StaticServices || (StaticServices = {}));
var DynamicStandaloneServices = /** @class */ (function (_super) {
    __extends(DynamicStandaloneServices, _super);
    function DynamicStandaloneServices(domElement, overrides) {
        var _this = _super.call(this) || this;
        var _a = StaticServices.init(overrides), _serviceCollection = _a[0], _instantiationService = _a[1];
        _this._serviceCollection = _serviceCollection;
        _this._instantiationService = _instantiationService;
        var configurationService = _this.get(IConfigurationService);
        var notificationService = _this.get(INotificationService);
        var telemetryService = _this.get(ITelemetryService);
        var themeService = _this.get(IThemeService);
        var ensure = function (serviceId, factory) {
            var value = null;
            if (overrides) {
                value = overrides[serviceId.toString()];
            }
            if (!value) {
                value = factory();
            }
            _this._serviceCollection.set(serviceId, value);
            return value;
        };
        var contextKeyService = ensure(IContextKeyService, function () { return _this._register(new ContextKeyService(configurationService)); });
        ensure(IAccessibilityService, function () { return new AccessibilityService(contextKeyService, configurationService); });
        ensure(IListService, function () { return new ListService(themeService); });
        var commandService = ensure(ICommandService, function () { return new StandaloneCommandService(_this._instantiationService); });
        var keybindingService = ensure(IKeybindingService, function () { return _this._register(new StandaloneKeybindingService(contextKeyService, commandService, telemetryService, notificationService, domElement)); });
        var layoutService = ensure(ILayoutService, function () { return new SimpleLayoutService(domElement); });
        var contextViewService = ensure(IContextViewService, function () { return _this._register(new ContextViewService(layoutService)); });
        ensure(IContextMenuService, function () {
            var contextMenuService = new ContextMenuService(telemetryService, notificationService, contextViewService, keybindingService, themeService);
            contextMenuService.configure({ blockMouse: false }); // we do not want that in the standalone editor
            return _this._register(contextMenuService);
        });
        ensure(IMenuService, function () { return new MenuService(commandService); });
        ensure(IBulkEditService, function () { return new SimpleBulkEditService(StaticServices.modelService.get(IModelService)); });
        return _this;
    }
    DynamicStandaloneServices.prototype.get = function (serviceId) {
        var r = this._serviceCollection.get(serviceId);
        if (!r) {
            throw new Error('Missing service ' + serviceId);
        }
        return r;
    };
    DynamicStandaloneServices.prototype.set = function (serviceId, instance) {
        this._serviceCollection.set(serviceId, instance);
    };
    DynamicStandaloneServices.prototype.has = function (serviceId) {
        return this._serviceCollection.has(serviceId);
    };
    return DynamicStandaloneServices;
}(Disposable));
export { DynamicStandaloneServices };
