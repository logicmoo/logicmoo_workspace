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

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.cs3.pdt.editor.PDTPlugin;
import org.cs3.pdt.editor.internal.contentassistant.DefaultCompletion;
import org.cs3.prolog.connector.common.Debug;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

public class TemplateReader {
	
	public static List<DefaultCompletion> readDefaultCompletions() {
		ArrayList<DefaultCompletion> completions = new ArrayList<DefaultCompletion>();
		readLogtalkTemplates(completions);
		return completions;
	}
	
	private static void readLogtalkTemplates(ArrayList<DefaultCompletion> completions) {
		readLogtalkTextMateTemplates(completions);
	}
	
	private static void readLogtalkTextMateTemplates(ArrayList<DefaultCompletion> completions) {
		XMLReader xmlReader;
		try {
			xmlReader = XMLReaderFactory.createXMLReader();
			xmlReader.setContentHandler(new TextMateTemplateParser(completions));
			ArrayList<URL> urls = getLogtalkTextMateTemplateURLs();
			for (URL url : urls) {
				InputSource inputSource = new InputSource(url.openStream());
				xmlReader.parse(inputSource);
			} 
		} catch (SAXException e) {
			Debug.report(e);
		} catch (IOException e) {
			Debug.report(e);
		}
	}
	
	private static ArrayList<URL> getLogtalkTextMateTemplateURLs() {
		ArrayList<URL> urls = new ArrayList<URL>();
		Enumeration<String> entryPaths = PDTPlugin.getDefault().getBundle().getEntryPaths("/templates/logtalk/textmate/");
		while (entryPaths.hasMoreElements()) {
			String value= entryPaths.nextElement();
			URL url = PDTPlugin.getDefault().getBundle().getEntry(value);
			if (url != null) {
				urls.add(url);
			}
		}
		return urls;
	}


}
