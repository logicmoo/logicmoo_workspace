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

package org.cs3.pdt.console.internal.views.completion;

import java.util.List;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.console.internal.ImageRepository;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;

public class PredicateCompletionProposal extends ComparableCompletionProposal {

	private String signature;
	private String label;
	
	private String term;
	private String indicator;
	private String functor;
	
	private String visibility;
	private boolean isBuiltin;
	private boolean isDeprecated;
	
	private String docKind;
	private String doc;
	private String description;
	private boolean descriptionSet;
	
	private int lastStateMask = -1;
	
	public PredicateCompletionProposal(String module, String functor, int arity, int prefixLength, String visibility, boolean isBuiltin, boolean isDeprecated, List<String> argNames, String docKind, String doc, boolean addSingleQuote, boolean needsQuotes) {
		super(prefixLength, addSingleQuote);
		this.visibility = visibility;
		this.isBuiltin = isBuiltin;
		this.isDeprecated = isDeprecated;
		signature = functor + "/" + arity;
		if (module == null) {
			label = signature;
		} else {
			label = signature + " - " + module;
		}
		term = getFunctor(functor, addSingleQuote, needsQuotes) + getArglist(arity, argNames);
		indicator = getFunctor(functor, addSingleQuote, needsQuotes) + "/" + arity;
		this.functor = getFunctor(functor, addSingleQuote, needsQuotes);
		this.docKind = docKind;
		this.doc = doc;
	}
	
	private String getFunctor(String functor, boolean addSingleQuote, boolean needsQuotes) {
		if (addSingleQuote) {
			return functor + "'";
		} else if (needsQuotes) {
			return "'" + functor + "'";
		} else {
			return functor;
		}
	}
	
	@Override
	public String getContent(int stateMask) {
		lastStateMask = stateMask;
		if ((stateMask & SWT.CTRL) != 0) {
			return indicator;
		} else if ((stateMask & SWT.SHIFT) != 0) {
			return functor;
		} else {
			return term;
		}
	}

	private String getArglist(int arity, List<String> argNames) {
		if (arity < 1) {
			return "";
		}
		
		StringBuffer buf = new StringBuffer("(");
		char c = 'A';
		int i = 0;
		
		if (argNames == null) {
			while (i < arity) {
				if (i > 0) {
					buf.append(", ");
				}
				buf.append(c);
				if (c == '_' || c == 'Z') {
					c = '_';
				} else {
					c++;
				}
				i++;
			}
		} else {
			for (String argName : argNames) {
				if (i > 0) {
					buf.append(", ");
				}
				buf.append(argName);
				i++;
			}
		}
		buf.append(")");
		return buf.toString();
	}
	
	@Override
	public int getCursorPosition() {
		return getContent(lastStateMask).length();
	}

	@Override
	public String getLabel() {
		return label;
	}

	@Override
	public String getDescription() {
		if (!descriptionSet) {
			description = PDTCommonUtil.getHtmlDocumentation(docKind, doc);
			descriptionSet = true;
		}
		return description;
	}
	
	@Override
	public Image getImage() {
		if (isBuiltin) {
			return ImageRepository.getImage(ImageRepository.PREDICATE_BUILTIN);
		} else {
			if (SearchConstants.VISIBILITY_PUBLIC.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PREDICATE_PUBLIC);
			} else if (SearchConstants.VISIBILITY_PROTECTED.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PREDICATE_PROTECTED);
			} else if (SearchConstants.VISIBILITY_PRIVATE.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PREDICATE_PRIVATE);
			}
		}
		return null;
	}
	
	String getSignature() {
		return signature;
	}

	@Override
	public int compareTo(ComparableCompletionProposal o) {
		if (o instanceof PredicateCompletionProposal) {
			return getSignature().compareTo(((PredicateCompletionProposal) o).getSignature());
		} else if (o instanceof ModuleCompletionProposal) {
			return getSignature().compareTo(o.getLabel());
		} else {
			return -1;
		}
	}
	
	@Override
	public boolean isDeprecated() {
		return isDeprecated;
	}
	
}


