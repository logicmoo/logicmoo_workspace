// This file is part of AceRules.
// Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
//
// AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
// Lesser General Public License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with AceRules. If
// not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.acerules;

import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.Namespace;
import org.jdom.input.SAXBuilder;
import org.jdom.output.XMLOutputter;


public class Request implements Runnable {
	
	private final static String WS_URL = "http://attempto.ifi.uzh.ch/ws/acerules/acerulesws.perl";
	
	private final static Namespace ENV_NS = Namespace.getNamespace("env", "http://schemas.xmlsoap.org/soap/envelope/");
	private final static Namespace AR_NS = Namespace.getNamespace("ar", "http://attempto.ifi.uzh.ch/acerules");
	
	private UserInterface userInterface;
	private String program;
	private String mode;
	private boolean trace;
	
	public Request(UserInterface userInterface, String program, String mode, boolean trace) {
		super();
		this.userInterface = userInterface;
		this.program = program;
		this.mode = mode;
		this.trace = trace;
	}

	public void run() {
		Document doc = new Document();
		
		Element envElement = new Element("Envelope", ENV_NS);
		Element bodyElement = new Element("Body", ENV_NS);
		Element requestElement = new Element("Request", AR_NS);
		
		Element programElement = new Element("Program", AR_NS);
		programElement.addContent(program);
		requestElement.addContent(programElement);

		Element modeElement = new Element("Mode", AR_NS);
		modeElement.addContent(mode);
		requestElement.addContent(modeElement);
		
		if (trace) {
			Element traceElement = new Element("ACETraceOutput", AR_NS);
			traceElement.addContent("on");
			requestElement.addContent(traceElement);
		}
		
		if (userInterface.getMenuBar().isSelected("Guess unknown words")) {
			Element guessElement = new Element("Guess", AR_NS);
			guessElement.addContent("on");
			requestElement.addContent(guessElement);
		}
		if (userInterface.getMenuBar().isSelected("5 answers")) {
			Element maxAnswersElement = new Element("MaxAnswers", AR_NS);
			maxAnswersElement.addContent("5");
			requestElement.addContent(maxAnswersElement);
		}
		if (userInterface.getMenuBar().isSelected("10 answers")) {
			Element maxAnswersElement = new Element("MaxAnswers", AR_NS);
			maxAnswersElement.addContent("10");
			requestElement.addContent(maxAnswersElement);
		}
		if (userInterface.getMenuBar().isSelected("15 answers")) {
			Element maxAnswersElement = new Element("MaxAnswers", AR_NS);
			maxAnswersElement.addContent("15");
			requestElement.addContent(maxAnswersElement);
		}
		
		bodyElement.addContent(requestElement);
		envElement.addContent(bodyElement);
		
		doc.setRootElement(envElement);

		try {
			URL wsUrl = new URL(WS_URL);
			URLConnection connection = wsUrl.openConnection();
			connection.setDoInput(true);
			connection.setDoOutput(true);
			connection.setUseCaches(false);
			connection.setRequestProperty("Content-Type", "application/soap+xml");
			
			new XMLOutputter().output(doc, connection.getOutputStream());
			
			SAXBuilder sb = new SAXBuilder();
			sb.setValidation(false);
			Document rDoc = sb.build(connection.getInputStream());
			Element rEnv = rDoc.getRootElement();
			Element rBody = rEnv.getChild("Body", ENV_NS);
			Element rFault = rBody.getChild("Fault", ENV_NS);
			if (rFault == null) {
				Element rReply = rBody.getChild("Reply", AR_NS);
				
				ArrayList<String> answerStrings = new ArrayList<String>();
				for (Object answerObject : rReply.getChildren("Answertext", AR_NS)) {
					if (!(answerObject instanceof Element)) continue;
					
					Element answerElement = (Element) answerObject;
					String s = "";
					if (answerElement.getContentSize() > 0) {
						 s = answerElement.getContent(0).getValue();
					}
					answerStrings.add(s);
				}
				userInterface.getAnswerArea().addAnswers(answerStrings);
				
				if (trace) {
					userInterface.getTraceArea().clear();
					Element rTrace = rReply.getChild("ACETrace", AR_NS);
					for (Object obj : rTrace.getChildren("Step", AR_NS)) {
						if (!(obj instanceof Element)) continue;
						
						Element el = (Element) obj;
						userInterface.getTraceArea().addStep(el.getChild("Consistent", AR_NS).getValue());
					}
				}
				userInterface.showOutputArea();
			} else {
				userInterface.getAnswerArea().clear();
				userInterface.getTraceArea().clear();
				String errorText = rFault.getChild("faultstring", ENV_NS).getContent(0).getValue();
				String errorCode = rFault.getChild("faultcode", ENV_NS).getContent(0).getValue();
				userInterface.showWindow(new ErrorMessage(errorText, null, userInterface, errorCode));
			}
			
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

}
