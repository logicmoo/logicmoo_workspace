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

import org.cs3.pdt.console.internal.ImageRepository;
import org.eclipse.swt.graphics.Image;

public class AtomCompletionProposal extends ComparableCompletionProposal {
	
	private String atom;
	private boolean needsQuotes;
	
	public AtomCompletionProposal(String atom, int prefixLength, boolean addSingleQuote, boolean needsQuotes) {
		super(prefixLength, addSingleQuote);
		this.atom = atom;
		this.needsQuotes = needsQuotes;
	}
	
	@Override
	public String getContent(int stateMask) {
		if (addSingleQuote) {
			return atom + "'";
		} else if (needsQuotes) {
			return "'" + atom + "'";
		} else {
			return atom;
		}
	}

	@Override
	public int getCursorPosition() {
		return getContent(-1).length();
	}

	@Override
	public String getLabel() {
		return atom;
	}

	@Override
	public String getDescription() {
		return null;
	}

	@Override
	public Image getImage() {
		return ImageRepository.getImage(ImageRepository.ATOM);
	}

	@Override
	public int compareTo(ComparableCompletionProposal o) {
		if (o instanceof ModuleCompletionProposal || o instanceof PredicateCompletionProposal) {
			return 1;
		} else if (o instanceof AtomCompletionProposal) {
			return atom.compareTo(((AtomCompletionProposal) o).atom);
		} else {
			return -1;
		}
	}
	
}
