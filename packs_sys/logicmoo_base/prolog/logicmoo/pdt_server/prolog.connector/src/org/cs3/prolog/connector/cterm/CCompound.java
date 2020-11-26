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

import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.cs3.prolog.connector.internal.cterm.parser.ASTAtom;
import org.cs3.prolog.connector.internal.cterm.parser.ASTNode;

/**
 * Represents a compound Prolog term, e.g. a(1, x). Every argument is a {@link CTerm}
 * itself.
 */
public class CCompound extends CTerm implements Iterable<CTerm> {
	private CTerm[] args;

	public CCompound(ASTNode node) {
		super(node);
		args = new CTerm[node.jjtGetNumChildren()-1]; 
	}

	public CTerm getArgument(int i) {
		if(args[i]==null){
			args[i]=CTermFactory.create(node.jjtGetChild(i+1));
		}
		return args[i];
	}

	protected String doGetFunctorImage() {
		return ((ASTAtom)node.jjtGetChild(0)).getString();
	}

	@Override
	public int getArity() {
		return args.length;
	}

	@Override
	public Iterator<CTerm> iterator() {
		for (int i = 0;i<args.length;i++) {
			getArgument(i);
		}
		return Arrays.asList(args).iterator();
	}

	@Override
	public void rename(Map<String, String> dictionary) {
		super.rename(dictionary);
		for (int i = 0;i<args.length;i++) {
			CTerm arg = getArgument(i);
			arg.rename(dictionary);
		}
	}
	
	@Override
	public void filterAnonymousVariables(Set<String> variableSet) {
		for (int i = 0;i<args.length;i++) {
			CTerm arg = getArgument(i);
			arg.filterAnonymousVariables(variableSet);
		}
	}

}


