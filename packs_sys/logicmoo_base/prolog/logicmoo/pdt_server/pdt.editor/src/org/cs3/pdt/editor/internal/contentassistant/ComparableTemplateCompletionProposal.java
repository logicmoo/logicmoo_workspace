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

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateProposal;
import org.eclipse.swt.graphics.Image;

public abstract class ComparableTemplateCompletionProposal extends TemplateProposal implements Comparable<ComparableTemplateCompletionProposal> {

	protected static final String contextTypeId = "MyContextType";
	
	protected static final int PRIORITY_0 = 0;
	protected static final int PRIORITY_1 = 1;
	protected static final int PRIORITY_2 = 2;
	
	public ComparableTemplateCompletionProposal(IDocument document, String name, String description, String pattern, int offset, int length, Image image) {
		super(new Template(name, description, contextTypeId, pattern, true), new DocumentTemplateContext(new TemplateContextType(contextTypeId), document, offset, length), new Region(offset, length), image);
	}

	public ComparableTemplateCompletionProposal(Template template, TemplateContext context, IRegion region, Image image) {
		super(template, context, region, image);
	}

	@Override
	public String getAdditionalProposalInfo() {
		return null;
	}
	
	@Override
	public int compareTo(ComparableTemplateCompletionProposal p) {
		int c = p.getPriority() - getPriority();
		if (c != 0) {
			return c;
		} else {
			return getCompareText().compareTo(p.getCompareText());
		}
	}
	
	protected abstract int getPriority();
	
	protected String getCompareText() {
		return getTemplate().getName();
	}
	
	@Override
	public String getDisplayString() {
		Template template = getTemplate();
		if (template.getDescription().isEmpty()) {
			return template.getName();
		} else {
			return super.getDisplayString();
		}
	}
	
}
