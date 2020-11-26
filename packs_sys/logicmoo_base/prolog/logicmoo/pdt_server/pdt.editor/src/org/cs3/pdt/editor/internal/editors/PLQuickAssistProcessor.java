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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.cs3.pdt.editor.quickfix.PDTMarker;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.quickassist.IQuickAssistProcessor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.TextInvocationContext;
import org.eclipse.ui.texteditor.MarkerAnnotation;

public class PLQuickAssistProcessor implements IQuickAssistProcessor {

	
	private static final ICompletionProposal[] fgNoSuggestionsProposal=  new ICompletionProposal[] { new NoCompletionsProposal() };

	@Override
	public String getErrorMessage() {
		return "error!!";
	}

	@Override
	public boolean canFix(Annotation annotation) {
		return annotation instanceof MarkerAnnotation &&
				((MarkerAnnotation)annotation).getMarker().getAttribute(PDTMarker.QUICKFIX_DESCRIPTION, null) !=null;
	}

	@Override
	public boolean canAssist(IQuickAssistInvocationContext invocationContext) {
		return false;
	}

	@Override
	public ICompletionProposal[] computeQuickAssistProposals(
			IQuickAssistInvocationContext quickAssistContext) {
		ISourceViewer viewer= quickAssistContext.getSourceViewer();
		int documentOffset= quickAssistContext.getOffset();

		int length= viewer != null ? viewer.getSelectedRange().y : -1;
		TextInvocationContext context= new TextInvocationContext(viewer, documentOffset, length);


		IAnnotationModel model= viewer.getAnnotationModel();
		if (model == null)
			return fgNoSuggestionsProposal;

		List<?> proposals= computeProposals(context, model);
		if (proposals.isEmpty())
			return fgNoSuggestionsProposal;

		return (ICompletionProposal[]) proposals.toArray(new ICompletionProposal[proposals.size()]);
	}

	private List<?> computeProposals(IQuickAssistInvocationContext context, IAnnotationModel model) {
		int offset= context.getOffset();
		ArrayList<MarkerAnnotation> annotationList= new ArrayList<MarkerAnnotation>();
		Iterator<?> iter= model.getAnnotationIterator();
		while (iter.hasNext()) {
			Annotation annotation= (Annotation)iter.next();
			if (canFix(annotation)) {
				Position pos= model.getPosition(annotation);
				if (isAtPosition(offset, pos)) {
					collectSpellingProblems((MarkerAnnotation)annotation, annotationList);
				}
			}
		}
		return computeProposals(context, annotationList);
	}

	private boolean isAtPosition(int offset, Position pos) {
		return (pos != null) && (offset >= pos.getOffset() && offset <= (pos.getOffset() +  pos.getLength()));
	}

	private void collectSpellingProblems(MarkerAnnotation annotation, List<MarkerAnnotation> problems) {
		problems.add(annotation);
	}

	private List<ICompletionProposal> computeProposals(IQuickAssistInvocationContext context, List<MarkerAnnotation> markers) {
		List<ICompletionProposal> proposals= new ArrayList<ICompletionProposal>();
		for (int i= 0; i < markers.size(); i++)
			proposals.addAll(Arrays.asList(createProposals(context,markers.get(i))));

		return proposals;
	}

	private ICompletionProposal[] createProposals(IQuickAssistInvocationContext context,
			MarkerAnnotation annotation) {
		return new PrologCompletionProposal[] {
				new PrologCompletionProposal(context, annotation, false),
				new PrologCompletionProposal(context, annotation, true)
		};
	}
}


