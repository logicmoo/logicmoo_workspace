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

public class ModuleCompletionProposal extends ComparableTemplateCompletionProposal {

	public ModuleCompletionProposal(IDocument document, String module, int offset, int length, boolean addQuote) {
		super(document, module, "", createPattern(module, addQuote), offset, length, ImageRepository.getImage(ImageRepository.PACKAGE));
	}

	@Override
	protected int getPriority() {
		return PRIORITY_1;
	}
	
	private static String createPattern(String module, boolean addQuote) {
		if (addQuote) {
			return "'" + module.replace("$", "$$") + "'";
		} else {
			return module.replace("$", "$$");
		}
	}
	}
