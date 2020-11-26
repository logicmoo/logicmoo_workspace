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
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.viewers.StyledString;

public class DirectiveMatch extends AbstractPrologMatch {

	private String module;

	public DirectiveMatch(Object element, String module, List<String> properties, IFile file, int offset, int length, String signature) {
		super(element, properties, file, offset, length, signature);
		this.module = module;
	}

	public DirectiveMatch(Object element, String module, List<String> properties, IFile file, int line, String signature) {
		super(element, properties, file, line, signature);
		this.module = module;
	}
	
	public String getModule() {
		return module;
	}

	@Override
	public void createLabel() {
		StyledString str = new StyledString();
		String text = "";
		int line = getLine();
		if (isLineLocation()) {
			IDocument document = getDocument();
			if (document != null) {
				try {
					IRegion lineInformation = document.getLineInformation(line - 1);
					text = document.get(lineInformation.getOffset(), lineInformation.getLength());
				} catch (BadLocationException e) {
					Debug.report(e);
				}
			} else {
				String labelProperty = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_LABEL, getProperties());
				if (labelProperty != null) {
					text = Util.unquoteAtom(labelProperty);
				}
			}
		} else {
			try {
				text = getDocument().get(getOffset(), getLength());
			} catch (BadLocationException e) {
				Debug.report(e);
			}
			text = text.replaceAll("\n|\r", "");
		}
//		String prefix = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_PREFIX, getProperties());
		String suffix = PDTCommonUtil.getProperty(SearchConstants.PROPERTY_SUFFIX, getProperties());
		
//		if (prefix != null && !prefix.isEmpty()) {
//			str.append(Util.unquoteAtom(prefix), StyledString.QUALIFIER_STYLER);
//		}
		str.append(getLine() + ": ", StyledString.QUALIFIER_STYLER);
		str.append(text);
		if (suffix != null && !suffix.isEmpty()) {
			str.append(Util.unquoteAtom(suffix), StyledString.DECORATIONS_STYLER);
		}
		label = str;
	}

}
