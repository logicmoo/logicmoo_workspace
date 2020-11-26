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

package org.cs3.pdt.editor.internal.editors;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

/**
 * Proposal telling that there are no proposals available.
 * <p>
 * Applying this proposal does nothing.
 * </p>
 *
 * @since 3.3
 */
public final class NoCompletionsProposal implements ICompletionProposal {

	/*
	 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#apply(org.eclipse.jface.text.IDocument)
	 */
	@Override
	public void apply(IDocument document) {
		// do nothing
	}

	/*
	 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getAdditionalProposalInfo()
	 */
	@Override
	public String getAdditionalProposalInfo() {
		return null;
	}

	/*
	 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getContextInformation()
	 */
	@Override
	public IContextInformation getContextInformation() {
		return null;
	}

	/*
	 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getDisplayString()
	 */
	@Override
	public String getDisplayString() {
			return "No Prolog Completion available";
		}

	/*
	 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getImage()
	 */
	@Override
	public Image getImage() {
		return null;
	}

	/*
	 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getSelection(org.eclipse.jface.text.IDocument)
	 */
	@Override
	public Point getSelection(IDocument document) {
		return null;
	}

}


