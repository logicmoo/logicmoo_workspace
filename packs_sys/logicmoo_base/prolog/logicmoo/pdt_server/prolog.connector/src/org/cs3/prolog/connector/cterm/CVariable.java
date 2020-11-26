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

package org.cs3.prolog.connector.cterm;

import java.util.Map;
import java.util.Set;

import org.cs3.prolog.connector.internal.cterm.parser.ASTNode;

/**
 * Represents a Prolog variable.
 */
public class CVariable extends CTerm {
	public CVariable(ASTNode node) {
		super(node);
	}
	public String getVariableName() {
		return getFunctorValue();			
	}
	
	@Override
	public void rename(Map<String, String> dictionary) {
		String newName = dictionary.get(getFunctorValue());
		if (newName != null) {
			functorValue = newName;
		} else {
			functorValue = "_";
		}
	}

	@Override
	public void filterAnonymousVariables(Set<String> variableSet) {
		if (!variableSet.contains(getFunctorValue())) {
			functorValue = "_";
		}
	}
}


