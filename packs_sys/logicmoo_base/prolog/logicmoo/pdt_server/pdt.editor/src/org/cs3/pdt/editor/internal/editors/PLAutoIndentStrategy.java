/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 * Created on 20.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.editor.internal.editors;


import org.cs3.prolog.connector.common.Debug;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultIndentLineAutoEditStrategy;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextUtilities;

public class PLAutoIndentStrategy extends DefaultIndentLineAutoEditStrategy implements IAutoEditStrategy{

	private void autoIndentAfterNewLine(IDocument d, DocumentCommand c) {
		
		if (c.offset == -1 || d.getLength() == 0)
			return;
		
		try {
			// find start of line
			int p= (c.offset == d.getLength() ? c.offset  - 1 : c.offset);
			IRegion info= d.getLineInformationOfOffset(p);
			
			int start= info.getOffset();
					
			if ((c.offset == start + info.getLength()) && isPredicateHead(d,info)) 
				predicateIndent(d,c);
			
			// find white spaces
			int end= findEndOfWhiteSpace(d, start, c.offset);
			
			StringBuffer buf= new StringBuffer(c.text);
			if (end > start) {			
				// append to input
				buf.append(d.get(start, end - start));
			}
			
			c.text= buf.toString();
			
		} catch (BadLocationException excp) {
			// stop work
		}	
	}
	
	/**
	 * 
	 */
	private void predicateIndent(IDocument d,DocumentCommand c) {
		String nl = TextUtilities.getDefaultLineDelimiter(d);
		c.text=nl+"\t";
		
	}

	/**
	 * @param info
	 * @return
	 */
	private boolean isPredicateHead(IDocument d, IRegion info) {
		String line;
		try {
			line = d.get(info.getOffset(), info.getLength());
			String removedWS = line.trim();
			return removedWS.endsWith(":-") || removedWS.endsWith("-->");
		} catch (BadLocationException e) {
			Debug.report(e);
		}
		return false;
	}

	/*
	 * @see IAutoIndentStrategy#customizeDocumentCommand
	 */
	@Override
	public void customizeDocumentCommand(IDocument d, DocumentCommand c) {
		if (c.length == 0 && c.text != null && TextUtilities.endsWith(d.getLegalLineDelimiters(), c.text) != -1)
			autoIndentAfterNewLine(d, c);
	}
	
}


