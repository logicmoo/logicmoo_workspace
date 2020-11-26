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

package org.cs3.pdt.editor.internal.contentassistant;

import org.cs3.pdt.editor.internal.ImageRepository;
import org.eclipse.jface.text.IDocument;

public class VariableCompletionProposal extends ComparableTemplateCompletionProposal {

	public VariableCompletionProposal(IDocument document, String variable, int offset, int length) {
		super(document, variable, "", variable, offset, length, ImageRepository.getImage(ImageRepository.PE_PUBLIC));
	}

	@Override
	protected int getPriority() {
		return PRIORITY_2;
	}

}
