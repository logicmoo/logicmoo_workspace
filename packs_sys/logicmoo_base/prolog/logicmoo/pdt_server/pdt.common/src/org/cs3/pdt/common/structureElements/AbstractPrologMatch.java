/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2015, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.common.structureElements;

import java.util.List;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.Util;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.search.ui.text.Match;

public abstract class AbstractPrologMatch extends Match {

	private List<String> properties;
	private IFile file;
	private int line = -1;
	private boolean isLineLocation= false;
	private String signature;
	
	protected StyledString label;

	private Boolean conversionSuccessful;
	private IDocument document;
	
	public AbstractPrologMatch(Object element, List<String> properties, IFile file, int line, String signature) {
		super(element, UNIT_LINE, line - 1, 1);
		this.properties = properties;
		this.file = file;
		this.line = line;
		isLineLocation = true;
		this.signature = signature;
		initDocument();
	}
	
	public AbstractPrologMatch(Object element, List<String> properties, IFile file, int offset, int length, String signature) {
		super(element, UNIT_CHARACTER, offset, length);
		this.properties = properties;
		this.file = file;
		isLineLocation = false;
		this.signature = signature;
		initDocument();
		convertOffsetAndLength(offset, offset + length);
		parseLineFromProperties();
	}
	
	protected void convertOffsetAndLength(int offset, int end) {
		try {
			long fileLength = EFS.getStore(file.getLocationURI()).fetchInfo().getLength();
			if (fileLength > 1024 * 1024) {
				conversionFailed();
			} else {
				IDocument document = getDocument();
				if (document == null) {
					conversionFailed();
				} else {
					int convertedOffset = UIUtils.logicalToPhysicalOffset(document, offset);
					setOffset(convertedOffset);
					int convertedEnd = UIUtils.logicalToPhysicalOffset(document, end);
					int length = convertedEnd - convertedOffset;
					setLength(length);
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
			conversionFailed();
		}
	}
	
	public void createLabel() {
		StyledString str;
		String labelProperty = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_LABEL, properties);
		if (labelProperty != null) {
			str = new StyledString(Util.unquoteAtom(labelProperty));
		} else {
			str = new StyledString("");
		}
		label = str;
	}
	
	private void conversionFailed() {
		conversionSuccessful = false;
		isLineLocation = true;
	}

	private void parseLineFromProperties() {
		String lineProperty = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_LINE, properties);
		if (lineProperty == null) {
			line = 1;
		} else {
			try {
				line = Integer.parseInt(lineProperty);
				if (line < 1) {
					line = 1;
				}
			} catch (NumberFormatException e) {
				line = 1;
			}
		}
	}

	public int getLine() {
		return line;
	}

	public void setLine(int line) {
		this.isLineLocation=true;
		this.line = line;
	}

	public boolean isLineLocation() {
		return isLineLocation;
	}

	public List<String> getProperties() {
		return properties;
	}
	
	public IFile getFile() {
		return file;
	}
	
	public String getLabel() {
		return "";
	}
	
	public String getSignature() {
		return signature;
	}

	public StyledString getStyledString() {
		return label;
	}
	
	public boolean conversionSuccessful() {
		if (conversionSuccessful != null) {
			return conversionSuccessful;
		} else {
			return isLineLocation;
		}
	}
	
	private void initDocument() {
		try {
			document = UIUtils.getDocument(file);
			if (document.getLength() <= 0) {
				Debug.warning("Empty document for file: " + file.getFullPath());
				document = null;
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
	}
	
	public IDocument getDocument() {
		return document;
	}
	
}
