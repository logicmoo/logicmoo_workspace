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

import java.util.List;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.editor.PDT;
import org.cs3.pdt.editor.PDTPlugin;
import org.cs3.pdt.editor.internal.ImageRepository;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.internal.text.html.BrowserInformationControl;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension5;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension6;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateBuffer;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.TextStyle;
import org.eclipse.swt.widgets.Shell;

@SuppressWarnings({ "restriction" })
public class PredicateCompletionProposal extends ComparableTemplateCompletionProposal implements ICompletionProposalExtension5, ICompletionProposalExtension6, IInformationControlCreator {

	public static PredicateCompletionProposal createProposal(IDocument document, int offset, int length, String module, String name, int arity, List<String> argNames, String visibility, boolean isBuiltin, boolean isDeprecated, String docKind, String doc, boolean addQuote) {
		String pattern = createPattern(name, arity, argNames, addQuote);
		StyledString displayString;
		if (module == null) {
			displayString =  new StyledString(name + "/" + arity, isDeprecated ? STRIKETHROUGH : null);
		} else {
			displayString =  new StyledString(name + "/" + arity + " - " + module, isDeprecated ? STRIKETHROUGH : null);
		}
		String signature = name + "/" + arity;
		Image image = getImage(isBuiltin, visibility);
		Template termTemplate = new Template(name, "", contextTypeId, pattern, true);
		Template indicatorTemplate = new Template(name, "", contextTypeId, signature, true);
		DocumentTemplateContext2 documentTemplateContext = new DocumentTemplateContext2(new TemplateContextType(contextTypeId), document, offset, length);
		Region region = new Region(offset, length);
		return new PredicateCompletionProposal(termTemplate, documentTemplateContext, region, image, indicatorTemplate, displayString, signature, docKind, doc);
	}
	
	private static String createPattern(String name, int arity, List<String> argNames, boolean addQuote) {
		if (arity <= 0) {
			if (addQuote) {
				return "'" + name + "'";
			} else {
				return name;
			}
		}
		boolean createArglist = Boolean.parseBoolean(PDTPlugin.getDefault().getPreferenceValue(PDT.PREF_AUTO_COMPLETE_ARGLIST, "true"));
		if (!createArglist) {
			return name;
		}
		StringBuilder buf = new StringBuilder();
		if (addQuote) {
			buf.append('\'');
		}
		buf.append(name.replace("$", "$$"));
		if (addQuote) {
			buf.append('\'');
		}
		buf.append("(");
		if (argNames == null) {
			for (int i = 0; i < arity; i++) {
				if (i == 0) {
					buf.append("${_");
				} else {
					buf.append(", ${_");
				}
				buf.append(i);
				buf.append("}");
			}
		} else {
			boolean first = true;
			for (String argName : argNames) {
				if (first) {
					first = false;
					buf.append("${");
					buf.append(argName);
					buf.append("}");
				} else {
					buf.append(", ${");
					buf.append(argName);
					buf.append("}");
				}
			}
		}
		buf.append(")");
		return buf.toString();
	}
	
	private static Image getImage(boolean isBuiltin, String visibility) {
		if (isBuiltin) {
			return ImageRepository.getImage(ImageRepository.PE_BUILT_IN);
		} else {
			if (SearchConstants.VISIBILITY_PUBLIC.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
			} else if (SearchConstants.VISIBILITY_PROTECTED.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PE_PROTECTED);
			} else if (SearchConstants.VISIBILITY_PRIVATE.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PE_PRIVATE);
			} else if (SearchConstants.VISIBILITY_LOCAL.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PE_LOCAL);
			}
		}
		return null;
	}

	private Template indicatorOnly;
	private StyledString displayString;
	private String signature;
	private String docKind;
	private String doc;
	
	private int currentStateMask = -1;
	
	public PredicateCompletionProposal(Template template, TemplateContext context, IRegion region, Image image, Template indicatorOnly, StyledString displayString, String signature, String docKind, String doc) {
		super(template, context, region, image);
		this.indicatorOnly = indicatorOnly;
		this.displayString = displayString;
		this.signature = signature;
		this.doc = doc;
		this.docKind = docKind;
		
		((DocumentTemplateContext2) getContext()).setProposal(this);
		setInformationControlCreator(this);
	}

	private static class DocumentTemplateContext2 extends DocumentTemplateContext {

		private PredicateCompletionProposal predicateCompletionProposal;

		public DocumentTemplateContext2(TemplateContextType type, IDocument document, int offset, int length) {
			super(type, document, offset, length);
		}

		public void setProposal(PredicateCompletionProposal predicateCompletionProposal) {
			this.predicateCompletionProposal = predicateCompletionProposal;
		}
		
		@Override
		public TemplateBuffer evaluate(Template template) throws BadLocationException, TemplateException {
			if ((predicateCompletionProposal.currentStateMask & SWT.CTRL) != 0) {
				return super.evaluate(predicateCompletionProposal.indicatorOnly);
			} else {
				return super.evaluate(template);
			}
		}
		
	}
	
	@Override
	public void apply(ITextViewer viewer, char trigger, int stateMask, int offset) {
		currentStateMask = stateMask;
		super.apply(viewer, trigger, stateMask, offset);
		currentStateMask = -1;
	}

	@Override
	public Object getAdditionalProposalInfo(IProgressMonitor monitor) {
		return PDTCommonUtil.getHtmlDocumentation(docKind, doc);
	}

	@Override
	public IInformationControl createInformationControl(Shell parent) {
		if (BrowserInformationControl.isAvailable(parent)) {
			return new BrowserInformationControl(parent, JFaceResources.DIALOG_FONT, true) {
				@Override
				public IInformationControlCreator getInformationPresenterControlCreator() {
					return PredicateCompletionProposal.this;
				}
			};
		} else {
			return new DefaultInformationControl(parent);
		}
	}

	@Override
	protected int getPriority() {
		return PRIORITY_1;
	}
	
	@Override
	protected String getCompareText() {
		return signature;
	}
	
	@Override
	public String getDisplayString() {
		return displayString.getString();
	}

	@Override
	public StyledString getStyledDisplayString() {
		return displayString;
	}

	private static Styler STRIKETHROUGH = new Styler() {
		@Override
		public void applyStyles(TextStyle textStyle) {
			textStyle.strikeout = true;
		}
	};
}
