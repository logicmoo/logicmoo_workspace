/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.metadata;

import org.cs3.pdt.common.metadata.Goal;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

public class GoalProvider {

	/**
	 * @param document
	 * @param offset
	 * @return
	 */
	public static Goal getPrologDataFromOffset(String file, IDocument document,
			int offset, int length) throws BadLocationException {
			
		int start; 
		int end;
		
		if (length > 0) {
			start = offset;
			end = offset + length;
		} else {
			start = PredicateReadingUtilities.findBeginOfPredicateName(document, offset);
			end = PredicateReadingUtilities.findEndOfPredicateName(document, offset);

			if (start > end) {
				return null;
			}
		}
		String predicateModuleAndName = document.get(start, end - start);
		
		String functor = PredicateReadingUtilities.extractFunctor(predicateModuleAndName);
		String module= PredicateReadingUtilities.extractModule(predicateModuleAndName);
	
		int endOfWhiteSpace = PredicateReadingUtilities.findEndOfWhiteSpace(document, end, document
				.getLength());
		int endOfTerm = endOfWhiteSpace;
		int arity;
		
		if ((document.getLength() == endOfWhiteSpace)  ||
				 (    (document.getChar(endOfWhiteSpace) != '(')
				   && (document.getChar(endOfWhiteSpace) != '/'))){
			
			arity=0;
			
		} else if (document.getChar(endOfWhiteSpace) == '/') {
			endOfTerm = endOfWhiteSpace + 1;
			String arityBuffer = "";
			while (endOfTerm < document.getLength() 
					&& document.getChar(endOfTerm) >= '0'
					&& document.getChar(endOfTerm) <= '9') {
				arityBuffer += document.getChar(endOfTerm);
				endOfTerm++;
			}
			
			if (arityBuffer.length() == 0)
				return null;
			arity = Integer.parseInt(arityBuffer);
		} else {   // document.getChar(endOfWhiteSpace) == '(')
			endOfTerm = endOfWhiteSpace + 1;
			arity=1;
			whileLoop: while (!PredicateReadingUtilities.isEndOfHead(document, endOfTerm)) {
				char c = document.getChar(endOfTerm);
				switch (c) {
				case '(':
					endOfTerm = PredicateReadingUtilities.consume(document, endOfTerm + 1, ')');
					break;
				case '[':
					endOfTerm = PredicateReadingUtilities.consume(document, endOfTerm + 1, ']');
					break;
				case '"':
					endOfTerm = PredicateReadingUtilities.consumeString(document, endOfTerm + 1, '"');
					break;
				case '\'':
					endOfTerm = PredicateReadingUtilities.consumeString(document, endOfTerm + 1, '\'');
					break;
				case ',':
					arity++;
					endOfTerm++;
					break;
				case ')':
					break whileLoop;
				default:
					endOfTerm++;
				}
			};
			endOfTerm++;
		}
		String term = document.get(start, endOfTerm - start);
		int line = document.getLineOfOffset(offset) +1;

		return new Goal(file, line, start, endOfTerm, module, functor, arity, term);

	}

}


