var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var A4IntentionActionFactory = /** @class */ (function (_super) {
    __extends(A4IntentionActionFactory, _super);
    function A4IntentionActionFactory() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    A4IntentionActionFactory.prototype.loadFromXML = function (xml, ai) {
        if (xml.getAttribute("type") == "A4AnswerHow_IntentionAction")
            return A4AnswerHow_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "A4AnswerWhere_IntentionAction")
            return AnswerWhere_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "A4Locate_IntentionAction")
            return A4Locate_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "EtaoinHelp_IntentionAction")
            return EtaoinHelp_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "EtaoinClose_IntentionAction")
            return EtaoinClose_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "EtaoinOpen_IntentionAction")
            return EtaoinOpen_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "EtaoinSwitchOff_IntentionAction")
            return EtaoinSwitchOff_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "EtaoinSwitchOn_IntentionAction")
            return EtaoinSwitchOn_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "EtaoinTalk_IntentionAction")
            return EtaoinTalk_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "EtaoinConnectTo_IntentionAction")
            return EtaoinConnectTo_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "Etaoin3DPrint_IntentionAction")
            return Etaoin3DPrint_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "EtaoinRead_IntentionAction")
            return EtaoinRead_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotFollow_IntentionAction")
            return RobotFollow_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotGive_IntentionAction")
            return RobotGive_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotGo_IntentionAction")
            return RobotGo_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotOpenClose_IntentionAction")
            return RobotOpenClose_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotStop_IntentionAction")
            return RobotStop_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotTake_IntentionAction")
            return RobotTake_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotTakeTo_IntentionAction")
            return RobotTakeTo_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotPutIn_IntentionAction")
            return RobotPutIn_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotTalk_IntentionAction")
            return RobotTalk_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotHelp_IntentionAction")
            return RobotHelp_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotPushPull_IntentionAction")
            return RobotPushPull_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotEnter_IntentionAction")
            return RobotEnter_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotExit_IntentionAction")
            return RobotExit_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotReboot_IntentionAction")
            return RobotReboot_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotTurn_IntentionAction")
            return RobotTurn_IntentionAction.loadFromXML(xml, ai);
        if (xml.getAttribute("type") == "RobotStay_IntentionAction")
            return RobotStay_IntentionAction.loadFromXML(xml, ai);
        return _super.prototype.loadFromXML.call(this, xml, ai);
    };
    return A4IntentionActionFactory;
}(IntentionActionFactory));
