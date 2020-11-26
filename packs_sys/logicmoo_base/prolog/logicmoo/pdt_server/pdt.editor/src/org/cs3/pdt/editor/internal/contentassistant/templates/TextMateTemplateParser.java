/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.contentassistant.templates;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.cs3.pdt.editor.internal.contentassistant.DefaultCompletion;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class TextMateTemplateParser extends DefaultHandler {

	private ArrayList<DefaultCompletion> completions;
	
	TextMateTemplateParser(ArrayList<DefaultCompletion> completions) {
		this.completions = completions;
	}
	
	private StringBuilder current;
	private String currentKey;
	private String prefix;
	private String completion;
	private String name;
	
	@Override
	public void startDocument() throws SAXException {
		prefix = null;
		completion = null;
		name = null;
	}
	
	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (current == null) {
			current = new StringBuilder();
		}
		current.append(ch, start, length);
	}
	
	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if ("key".equals(localName)) {
			currentKey = current.toString();
		} else if ("string".equals(localName)) {
			if ("content".equals(currentKey)) {
				completion = current.toString();
			} else if ("tabTrigger".equals(currentKey)) {
				prefix = current.toString();
			} else if ("name".equals(currentKey)) {
				name = current.toString();
			}
		}
		current = null;
	}
	
	@Override
	public void endDocument() throws SAXException {
		if (prefix != null && completion != null && name != null) {
			completions.add(new DefaultCompletion(DefaultCompletion.FILE_TYPE_LOGTALK, prefix, adaptCompletion(completion), name.trim()));
		}
	}
	
	private static String adaptCompletion(String input) {
		String s = input.replaceAll("\\$\\{\\d+:", "\\${");
		if (s.endsWith("$0")) {
			s = s.replaceAll("\\$0", "");
		} else {
			s = s.replaceAll("\\$0", "\\${:cursor}");
		}
		Matcher matcher = Pattern.compile("\\$(\\d+)").matcher(s);
		s = matcher.replaceAll("\\${_$1}");
		s = s.replaceAll("\\$TM_FULLNAME", "\\${Author}");
		return s;
	}
}
