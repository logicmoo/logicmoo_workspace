/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.console.internal.views.completion;

import org.eclipse.swt.graphics.Image;

public abstract class ComparableCompletionProposal implements IContentProposal, Comparable<ComparableCompletionProposal> {
	
	protected int prefixLength;
	protected boolean addSingleQuote;
	
	public ComparableCompletionProposal(int prefixLength, boolean addSingleQuote) {
		this.prefixLength = prefixLength;
		this.addSingleQuote = addSingleQuote;
	}
	
	public abstract Image getImage(); 
	
	@Override
	public boolean isDeprecated() {
		return false;
	}
	
	@Override
	public int getPrefixLength() {
		return prefixLength;
	}
	
}
