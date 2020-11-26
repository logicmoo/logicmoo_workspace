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

import org.cs3.pdt.editor.internal.editors.PLPartitionScanner;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.ParserUtils;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITypedRegion;

public class PredicateReadingUtilities {
	

	/**
	 * Returns true, if the character at the postion in the document
	 * does indicate the end of a term head("." or ":-"), else false.
	 * 
	 * @param document
	 * @param pos
	 * @return true if  head ends at pos
	 */
	public static boolean isEndOfHead(IDocument document, int pos) {
		try {
			if (document.getChar(pos) == '.')
				return true;
			if (document.getChar(pos) == ':' && document.getLength() > (pos + 1)
					&& document.getChar(pos + 1) == '-')
				return true;
		} catch (BadLocationException e) {
			Debug.report(e);
		}
		return false;
	}

	public static int findEndOfWhiteSpace(IDocument document, int offset,
			int end) throws BadLocationException {
		while (offset < end) {
			char c = document.getChar(offset);
			if (c != ' ' && c != '\t') {
				return offset;
			}
			offset++;
		}
		return end;
	}

	/**
	 * @param document
	 * @param end
	 * @param c
	 * @return
	 */
	public static int consume(IDocument document, int end, char endChar)
			throws BadLocationException {
		while (!(document.getChar(end) == endChar)) {
			char c = document.getChar(end);
			switch (c) {
			case '\\':
				end = end + 2;
				break;
			case '(':
				end = consume(document, end + 1, ')');
				break;
			case '[':
				end = consume(document, end + 1, ']');
				break;
			case '"':
				end = consumeString(document, end + 1, '"');
				break;
			case '\'':
				end = consumeString(document, end + 1, '\'');
				break;
			default:
				end++;
			}
		}
		return end + 1;
	}

	/**
	 * @param document
	 * @param i
	 * @param c
	 * @return
	 */
	public static int consumeString(IDocument document, int end, char endChar)
			throws BadLocationException {
		while (!(document.getChar(end) == endChar)) {
			char c = document.getChar(end);
			switch (c) {
			case '\\':
				end = end + 2;
				break;
			default:
				end++;
			}
		}
		return end + 1;
	}

	// Set by findBeginOfPredicateName() and findEndOfPredicateName().
	// Used by isPredicateNameChar():
//	private static boolean predicate_name_is_enclosed_in_quotes = false;
	
	public static int findBeginOfPredicateName(IDocument document, int begin)
			throws BadLocationException {
		int start = begin;
		ITypedRegion partition = document.getPartition(begin);
		boolean predicateNameIsEnclosedInQuotes = (partition != null && PLPartitionScanner.PL_SINGLE_QUOTED_STRING.equals(partition.getType()));
		if (predicateNameIsEnclosedInQuotes) {
			return partition.getOffset();
		} else {
			while (start >= 0 && ParserUtils.isNormalPredicateNameChar(document.getChar(start))) {
				start--; // scan left until first non-predicate-name  char
			}
			return start + 1;
		}

//		while (start >= 0 && ParserUtils.isPredicateNameChar(document.getChar(start))) {
//			start--; // scan left until first non-predicate-name  char
//		}
//		start++; // start is now the position of the first predicate char
//		if (document.getChar(start) == '\'') {
//			predicate_name_is_enclosed_in_quotes = true;
//			// start++; // quotes are not part of the name
//		}
//		return start;
	}

	public static int findEndOfPredicateName(IDocument document, int end)
			throws BadLocationException {
		ITypedRegion partition = document.getPartition(end);
		boolean predicateNameIsEnclosedInQuotes = (partition != null && PLPartitionScanner.PL_SINGLE_QUOTED_STRING.equals(partition.getType()));
		if (predicateNameIsEnclosedInQuotes) {
//			// Accept any character up to the next simple quote:
//			while ( document.getChar(end)!='\'' 
//					&& end < document.getLength() ) {
//				end++;
//			}
//			// Include the terminal quote
//			end++;
			return partition.getOffset() + partition.getLength();
		} else {
			// Do not accept special characters that may only occur within quotes:
			while (ParserUtils.isNormalPredicateNameChar(document.getChar(end)) 
					&& end < document.getLength()) {
				end++;
			}
		}

		return end;
	}

	public static String extractFunctor(String predicateModuleAndName) {
		/*if(predicateModuleAndName.endsWith(":"))  //hier war mal was zur Sonderbehandlung, wenn : am ende...
			return predicateModuleAndName;*/
		String[] nameSegments = predicateModuleAndName.split(":");
		int length = nameSegments.length;
		if (length > 0)
			return nameSegments[length-1];
		else
			return predicateModuleAndName;      
	}

	public static String extractModule(String predicateModuleAndName) {
		/*if(predicateModuleAndName.endsWith(":"))  //hier war mal was zur Sonderbehandlung, wenn : am ende...
		return null;*/
	
		String[] nameSegments = predicateModuleAndName.split(":");
		int length2 = nameSegments.length;
		
		if (length2 >= 2) {
			return nameSegments[length2-2];
		} else {
			return null;
		}
	}
}


