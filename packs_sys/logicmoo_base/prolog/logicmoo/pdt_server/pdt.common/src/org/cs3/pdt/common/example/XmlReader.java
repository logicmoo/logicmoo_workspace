/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.example;

import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;

import org.cs3.pdt.common.PDTCommonUtil;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public class XmlReader extends DefaultHandler {

	private File f;
	private XMLReader xr;
	private ArrayList<ExampleProject> outputList;
	private static final String XML_NAME = "examples.xml";

	public XmlReader(String urlString, ArrayList<ExampleProject> outputList) {
		this.outputList = outputList;
		try {
			File tempDir = new File(System.getProperty("java.io.tmpdir"));
			File examplesDir = new File(tempDir, ExampleProject.relativePath);
			examplesDir.mkdirs();
			f = new File(examplesDir, XML_NAME);
			// Aktuelle Version der XML-Datei laden
			PDTCommonUtil.saveUrlToFile(urlString, f, null);

			xr = XMLReaderFactory.createXMLReader();
			xr.setContentHandler(this);
			xr.setErrorHandler(this);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void parse() {
		try {
			// parse XML-file
			FileReader reader = new FileReader(f);
			xr.parse(new InputSource(reader));
			reader.close();
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
		}
	}

	@Override
	public void startElement(String uri, String name, String qName, Attributes atts) {
		ExampleProject ep = null;

		if (qName.equals("project")) {
			String treePathValue = atts.getValue("treePath");
			String[] treePath;
			if (treePathValue == null) {
				treePath = new String[0];
			} else {
				treePath = treePathValue.split("\\?\\?");
			}
			ep = new ExampleProject(atts.getValue("name"), atts.getValue("projectname"), atts.getValue("filename"), atts.getValue("description"), atts.getValue("url"), treePath);

			outputList.add(ep);
		}

	}

}
