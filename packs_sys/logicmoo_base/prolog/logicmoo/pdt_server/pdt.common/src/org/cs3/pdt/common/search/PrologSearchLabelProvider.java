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

package org.cs3.pdt.common.search;

import org.cs3.pdt.common.internal.ImageRepository;
import org.cs3.pdt.common.metadata.PrologElement;
import org.cs3.pdt.common.structureElements.PrologTreeElement;
import org.cs3.pdt.common.structureElements.SearchDirectiveElement;
import org.cs3.pdt.common.structureElements.SearchMatchElement;
import org.cs3.pdt.common.structureElements.SearchModuleElement;
import org.cs3.pdt.common.structureElements.SearchPredicateElement;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.swt.graphics.Image;

public class PrologSearchLabelProvider extends LabelProvider implements IStyledLabelProvider {

	PrologSearchLabelProvider() {
	}

	@Override
	public StyledString getStyledText(Object element) {
		if (element instanceof SearchModuleElement) {
			SearchModuleElement searchModuleElement = (SearchModuleElement) element;
			return getStyledTextWithCount(searchModuleElement.getStyledString(), searchModuleElement.computeContainedMatches());
		} else if (element instanceof SearchPredicateElement){
			SearchPredicateElement pe = ((SearchPredicateElement) element);
			String label = pe.getLabel();
			int count = pe.computeContainedMatches();
			return getStyledTextWithCount(label, count);
		} else if (element instanceof SearchMatchElement) {
			return ((SearchMatchElement) element).getStyledString();
		} if (element instanceof SearchDirectiveElement) {
			return ((SearchDirectiveElement) element).getStyledString();
		}
		return new StyledString(getText(element));
	}
	
	private StyledString getStyledTextWithCount(String label, int count) {
		return getStyledTextWithCount(new StyledString(label), count);
	}
	
	private StyledString getStyledTextWithCount(StyledString str, int count) {
		str.append(" (", StyledString.COUNTER_STYLER);
		str.append(Integer.toString(count), StyledString.COUNTER_STYLER);
		str.append(" match", StyledString.COUNTER_STYLER);
		if (count > 1) {
			str.append("es", StyledString.COUNTER_STYLER);
		}
		str.append(")", StyledString.COUNTER_STYLER);
		return str;
	}

	@Override
	public Image getImage(Object element) {
		if (element instanceof IFile) {
			return ImageRepository.getImage(ImageRepository.FILE);
		} else if (element instanceof SearchMatchElement || element instanceof SearchDirectiveElement) {
			return ImageRepository.getImage(ImageRepository.SEARCH_MATCH);
		} else if(element instanceof SearchModuleElement){
			return ImageRepository.getImage(ImageRepository.ENTITY);
		} else if(element instanceof SearchPredicateElement){
			return setPredicateImage(element);
		} 
		return null;
	}

	private Image setPredicateImage(Object element) {
		PrologElement pe = (PrologElement) element;
		if (pe.isPublic() || "user".equals(pe.getModule())) {
			return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
		} else if (pe.isPrivate()) {
			return ImageRepository.getImage(ImageRepository.PE_PRIVATE);
		} else if (pe.isLocal()) {
			return ImageRepository.getImage(ImageRepository.PE_LOCAL);
		} else if (pe.getProperties().contains("invisible")){
			return ImageRepository.getImage(ImageRepository.PE_INVISIBLE);
		} else {
			return ImageRepository.getImage(ImageRepository.PE_PROTECTED);
		}
	}

	@Override
	public String getText(Object element) {
		if (element instanceof PrologTreeElement){
			return ((PrologTreeElement)element).getLabel();
		} else {
			return super.getText(element);
		}
	}

}

