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

package org.cs3.pdt.editor.internal.views.lightweightOutline;


/**
 * String matcher that can match two patterns.
 *
 * @since 3.2
 */
class OrStringMatcher extends StringMatcher {

	private StringMatcher fMatcher1;
	private StringMatcher fMatcher2;

	OrStringMatcher(String pattern1, String pattern2, boolean ignoreCase, boolean foo) {
		super("", false, false); //$NON-NLS-1$
		fMatcher1= new StringMatcher(pattern1, ignoreCase, false);
		fMatcher2= new StringMatcher(pattern2, ignoreCase, false);
	}

	@Override
	public boolean match(String text) {
		return fMatcher2.match(text) || fMatcher1.match(text);
	}

}


