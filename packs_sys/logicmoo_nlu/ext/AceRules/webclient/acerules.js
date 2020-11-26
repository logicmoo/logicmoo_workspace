function process() {

	var wsUrl = "http://attempto.ifi.uzh.ch/ws/acerules/acerulesws.perl";
	
	// BUG: temporary hack to use the unizh-URI as the webservice URI
	// in case the client uses the unizh-URI. The problem is that due to
	// security constraints the two URIs must match. 
	if ((window.location.href).match(/\.unizh\./)) {
		wsUrl = wsUrl.replace(/\.uzh\./, ".unizh.");
	}
	
	var i = document.getElementById('input').value;
	i = i.replace(/\r\n/g, "\n");
	var req = makeXMLHttpRequestObject();
	
	var p = "";
	
	if (document.getElementById('stableMode').checked) {
		p += '<ar:Mode>stable</ar:Mode>\n';
	}
	if (document.getElementById('stableStrongMode').checked) {
		p += '<ar:Mode>stable_strong</ar:Mode>\n';
	}
	if (document.getElementById('guessOn').checked) {
		p += '<ar:Guess>on</ar:Guess>\n';
	}
	if (document.getElementById('maxanswers5').checked) {
		p += '<ar:MaxAnswers>5</ar:MaxAnswers>\n';
	}
	if (document.getElementById('maxanswers10').checked) {
		p += '<ar:MaxAnswers>10</ar:MaxAnswers>\n';
	}
	if (document.getElementById('maxanswers20').checked) {
		p += '<ar:MaxAnswers>20</ar:MaxAnswers>\n';
	}
	if (document.getElementById('showRules').checked) {
		p += '<ar:RulesOutput>on</ar:RulesOutput>\n';
	}
	if (document.getElementById('showSimpleRules').checked) {
		p += '<ar:SimpleRulesOutput>on</ar:SimpleRulesOutput>\n';
	}
	if (document.getElementById('showAnswerset').checked) {
		p += '<ar:AnswersetOutput>on</ar:AnswersetOutput>\n';
	}
	if (!document.getElementById('showAnswertext').checked) {
		p += '<ar:AnswertextOutput>off</ar:AnswertextOutput>\n';
	}
	if (document.getElementById('showTrace').checked) {
		p += '<ar:TraceOutput>on</ar:TraceOutput>\n';
	}
	if (document.getElementById('showAceTrace').checked) {
		p += '<ar:ACETraceOutput>on</ar:ACETraceOutput>\n';
	}
	if (document.getElementById('lexicon').value.length > 0) {
		p += '<ar:UserLexiconURL>' + document.getElementById('lexicon').value + '</ar:UserLexiconURL>\n';
	}
	
	var soapMessage =
		'<?xml version="1.0" encoding="UTF-8"?>\n\n' +
		'<env:Envelope xmlns:env="http://schemas.xmlsoap.org/soap/envelope/">\n' +
		'<env:Body>\n' +
		'<ar:Request xmlns:ar="http://attempto.ifi.uzh.ch/acerules">\n' +
		'<ar:Program>' + escape(i) + '</ar:Program>\n' +
		p +
		'</ar:Request>\n' +
		'</env:Body>\n' +
		'</env:Envelope>\n';

	if(req) {
	
		req.onreadystatechange = function() {
			if (req.readyState == 4) {
			
				document.getElementById('busyLight').className = "hidden";
	
				if (req.status == 200) {
					var results = document.getElementById('results');
					var result = getResultNode(req, i, soapMessage);
					var newline = document.createTextNode("\n");
					results.insertBefore(newline, results.firstChild);
					results.insertBefore(result, results.firstChild);
				} else {
					alert("Failed to retrieve data from\n" + wsUrl + "\nError: " + req.statusText + " (" + req.status + ")");
				}
			} else {
				document.getElementById('busyLight').className = 's' + req.readyState;
			}
		}
		
		try {
			req.open("POST", wsUrl, true);
	
			req.setRequestHeader('Content-Type', 'application/soap+xml');
	
			req.send(soapMessage);
		}
		catch(e) {
			alert(e);
		}
	}
}


function getResultNode(req, inputtext, soapRequest) {

	var div = document.createElement("div");
	var resultBox = document.createElement("div");

	div.className = "container";

	var infoBox = makeInfoBox(new Date());
	
	if (document.getElementById('showInput').checked) {
		resultBox.appendChild(getOutputNode("input", inputtext, "pre"));
	}
	
	if (document.getElementById('showSoapRequest').checked) {
		soapRequest = soapRequest.replace(/\n+/g, "\n");
		resultBox.appendChild(getOutputNode("soap request", soapRequest, "pre"));
	}
	
	if (document.getElementById('showSoapReply').checked) {
		var soapReply = req.responseText;
		soapReply = soapReply.replace(/\n+/g, "\n");
		resultBox.appendChild(getOutputNode("soap reply", soapReply, "pre"));
	}
	
	// This work-around is needed because IE6 apparently fails to process XML
	// namespaces correcty:
	// (Not yet tested in IE7!)
	var envPrefix = "";
	var arPrefix = "";
	if (req.responseXML.getElementsByTagName("Envelope").item(0) == null) {
		// This is executed in IE6:
		envPrefix = "env:";
		arPrefix = "ar:";
	}
	
	var envelope = req.responseXML.getElementsByTagName(envPrefix + "Envelope").item(0);
	var body = envelope.getElementsByTagName(envPrefix + "Body").item(0);
	
	try {
		var fault = body.getElementsByTagName(envPrefix + "Fault").item(0);
		var reasontext = fault.getElementsByTagName(envPrefix + "faultstring").item(0);
		var s = reasontext.childNodes[0].nodeValue;
		var errorElement = document.createElement("div");
		var errorText = document.createTextNode("ERROR: " + s);
		errorElement.className = "error";
		errorElement.appendChild(errorText);
		resultBox.appendChild(errorElement);
	} catch(e) {}
	
	var reply = envelope.getElementsByTagName(arPrefix + "Reply").item(0);
	
	try {
		var rules = reply.getElementsByTagName(arPrefix + "Rules").item(0);
		var s = rules.childNodes[0].nodeValue;
		var node = getOutputNode("rules", s, "pre");
		resultBox.appendChild(node);
	} catch(e) {}
	
	try {
		var rules = reply.getElementsByTagName(arPrefix + "SimpleRules").item(0);
		var s = rules.childNodes[0].nodeValue;
		var node = getOutputNode("simple rules", s, "pre");
		resultBox.appendChild(node);
	} catch(e) {}

	try {
		var answersets = reply.getElementsByTagName(arPrefix + "Answerset");
		for (var i=0; i < answersets.length; i++) {
			var s = answersets.item(i).childNodes[0].nodeValue;
			var node = getOutputNode("answer set " + (i+1), s, "pre");
			resultBox.appendChild(node);
		}
	} catch(e) {}
	
	try {
		var answertexts = reply.getElementsByTagName(arPrefix + "Answertext");
		for (var i=0; i < answertexts.length; i++) {
			var s = answertexts.item(i).childNodes[0].nodeValue;
			var node = getOutputNode("answer text " + (i+1), s, "pre");
			resultBox.appendChild(node);
		}
	} catch(e) {}
	
	try {
		var trace = reply.getElementsByTagName(arPrefix + "Trace").item(0);
		var steps = trace.getElementsByTagName(arPrefix + "Step");
		var raw = "";
		var del = "";
		var cons = "";
		var s = "";
		var node = getOutputNode("trace", s, "div");
		for (var i = 0; i < steps.length; i++) {
			try { raw = steps.item(i).getElementsByTagName(arPrefix + "Raw").item(0).childNodes[0].nodeValue; } catch(e) {}
			try { del = steps.item(i).getElementsByTagName(arPrefix + "Delete").item(0).childNodes[0].nodeValue; } catch(e) {}
			try { cons = steps.item(i).getElementsByTagName(arPrefix + "Consistent").item(0).childNodes[0].nodeValue; } catch(e) {}
			node.appendChild(getOutputNode("raw", raw, "pre"));
			node.appendChild(getOutputNode("delete", del, "pre"));
			node.appendChild(getOutputNode("consistent", cons, "pre"));
		}
		resultBox.appendChild(node);
	} catch(e) {}
	
	try {
		var acetrace = reply.getElementsByTagName(arPrefix +"ACETrace").item(0);
		var steps = acetrace.getElementsByTagName(arPrefix + "Step");
		var raw = "";
		var cons = "";
		var s = "";
		var node = getOutputNode("ace-trace", s, "div");
		for (var i = 0; i < steps.length; i++) {
			try { raw = steps.item(i).getElementsByTagName(arPrefix + "Raw").item(0).childNodes[0].nodeValue; } catch(e) {}
			try { cons = steps.item(i).getElementsByTagName(arPrefix + "Consistent").item(0).childNodes[0].nodeValue; } catch(e) {}
			node.appendChild(getOutputNode("raw", raw, "pre"));
			node.appendChild(getOutputNode("consistent", cons, "pre"));
		}
		resultBox.appendChild(node);
	} catch(e) {}
	
	resultBox.className = "visible";
	div.appendChild(infoBox);
	div.appendChild(resultBox);
	return div;
}


function getOutputNode(name, content, type) {
	content = content.replace(/\n/g, "\r\n");

	var box = document.createElement("div");
	var label = document.createElement("span");
	var labelText = document.createTextNode(name);

	label.appendChild(labelText);
	label.className = "label";
	box.className = "box";
	box.appendChild(label);

	var containerElement = document.createElement(type);
	var textElement = document.createTextNode(content);
	containerElement.appendChild(textElement);
	box.appendChild(containerElement);

	return box;
}


function makeInfoBox(overallTimeEnd) {
	var infoBox = document.createElement("div");
	var span = document.createElement("span");
	infoBox.className = "infoBox";

	var textTimeBox = document.createTextNode(overallTimeEnd);

	span.appendChild(textTimeBox);
	infoBox.appendChild(span);

	return infoBox;
}


function makeXMLHttpRequestObject() {
	var r;

	if(window.XMLHttpRequest) {
		r = new XMLHttpRequest();
	}
	else if(window.ActiveXObject) {
		r = new ActiveXObject("Microsoft.XMLHTTP");
	}

	if(!r) {
		alert("Failed to create XMLHttpRequest object.");
	}

	return r;
}

function escape(str) {
	return str.replace(/&/g, "&#38;").replace(/</g, "&#60;").replace(/>/g, "&#62;");
}
