/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.contentassistant;

import org.cs3.pdt.editor.internal.ImageRepository;
import org.eclipse.jface.text.IDocument;


public class SimpleCompletionProposal extends ComparableTemplateCompletionProposal {

	public SimpleCompletionProposal(IDocument document, String name, String description, String pattern, int offset, int length) {
		super(document, name, description, pattern, offset, length, ImageRepository.getImage(ImageRepository.TEMPLATE));
	}
	
	@Override
	public String getAdditionalProposalInfo() {
		return getTemplate().getPattern();
	}
	
	@Override
	protected int getPriority() {
		return PRIORITY_1;
	}
}
