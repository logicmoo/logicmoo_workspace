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
package org.cs3.pdt.editor.internal.editors;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.jface.text.rules.WordRule;

public class CharacterCodeRule extends WordRule implements IPredicateRule {
	
	public CharacterCodeRule(IToken token) {
		super(wordDetector, token, false);
		addWord("0'", token);
	}
	
	private static final IWordDetector wordDetector = new IWordDetector() {
		@Override
		public boolean isWordStart(char c) {
			return c == '0';
		}
		@Override
		public boolean isWordPart(char c) {
			return c == '0' || c == '\'';
		}
	};

	@Override
	public IToken getSuccessToken() {
		return fDefaultToken;
	}

	@Override
	public IToken evaluate(ICharacterScanner scanner, boolean resume) {
		return evaluate(scanner);
	}

}
