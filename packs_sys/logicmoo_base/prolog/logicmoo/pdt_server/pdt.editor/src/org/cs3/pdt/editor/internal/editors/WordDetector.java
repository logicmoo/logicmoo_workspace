/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Tobias Rho
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.editors;

import org.eclipse.jface.text.rules.IWordDetector;

public class WordDetector implements IWordDetector {

	@Override
	public boolean isWordStart(char c) {
		return Character.isJavaIdentifierStart(c);
	}
	
	@Override
	public boolean isWordPart(char c) {
		return Character.isJavaIdentifierPart(c);
	}
} 


