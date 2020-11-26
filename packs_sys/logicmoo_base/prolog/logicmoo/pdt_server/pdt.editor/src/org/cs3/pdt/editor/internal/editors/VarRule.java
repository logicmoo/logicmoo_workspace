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

package org.cs3.pdt.editor.internal.editors;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

public class VarRule implements IPredicateRule {

	IToken token;
	PLWhitespaceDetector wsdetector;
	public VarRule(IToken token) {
		this.token =token;
		wsdetector = new PLWhitespaceDetector();
//		super("_", " ", token);
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.IPredicateRule#getSuccessToken()
	 */
	@Override
	public IToken getSuccessToken() {
		return token;
	}
	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.IPredicateRule#evaluate(org.eclipse.jface.text.rules.ICharacterScanner, boolean)
	 */
	@Override
	public IToken evaluate(ICharacterScanner scanner, boolean resume) {
		scanner.unread();
		if (!wsdetector.isWhitespace((char)scanner.read()))
			return Token.UNDEFINED;
		int c = scanner.read();
		if (!(c  == '_' || c >= 'A' && c <= 'Z') || c == ICharacterScanner.EOF) { 
			scanner.unread();
			return Token.UNDEFINED;
		}
		do {
		  c = scanner.read();
		} while (!wsdetector.isWhitespace((char)c) && c != ICharacterScanner.EOF);
		scanner.unread();
		return getSuccessToken();
				
	}
	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.IRule#evaluate(org.eclipse.jface.text.rules.ICharacterScanner)
	 */
	@Override
	public IToken evaluate(ICharacterScanner scanner) {
		return evaluate(scanner, false);
	}
}


