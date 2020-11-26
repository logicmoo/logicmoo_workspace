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

import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.internal.cterm.parser.ASTNode;

/**
 * Common superclass for all Prolog elements represented as Java objects.
 */
public class CTerm {
	protected ASTNode node;
	protected String functorValue;
	
	public CTerm(ASTNode node) {
		this.node=node;
	}
	
	@Override
	public String toString() {
		return CTermUtil.renderTerm(this);
	}
		
	public String getFunctorValue() {
		if(functorValue==null){
			functorValue=doGetFunctorValue();	
		}
		return functorValue;
	}

	private  String doGetFunctorValue() {
		String image = getFunctorImage();
		return Util.unquoteStringOrAtom(image);
	}

	public String getFunctorImage() {
		return node.getFunctor();
	}

	public int getArity() {	
		return 0;
	}
	
	public void rename(Map<String, String> dictionary) {
		String newName = dictionary.get(getFunctorValue());
		if (newName != null) {
			functorValue = newName;
		}
	}

	public void filterAnonymousVariables(Set<String> variableSet) {
		
	}

}


