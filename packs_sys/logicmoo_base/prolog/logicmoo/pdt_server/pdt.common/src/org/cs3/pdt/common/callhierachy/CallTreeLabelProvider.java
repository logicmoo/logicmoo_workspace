/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.common.callhierachy;

import org.cs3.pdt.common.internal.ImageRepository;
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.swt.graphics.Image;

public class CallTreeLabelProvider extends LabelProvider implements IStyledLabelProvider {

	@Override
	public StyledString getStyledText(Object element) {
		if (element instanceof Predicate) {
			return ((Predicate) element).getStyledString();
		} else if (element instanceof PredicateEdge) {
			return ((PredicateEdge) element).getStyledString();
		}
		return new StyledString(getText(element));
	}
	
	@Override
	public Image getImage(Object element) {
		String visibility = null;
		if (element instanceof Predicate) {
			return CallHierarchyUtil.getImageForVisibility(((Predicate) element).getVisibility());
		} else if (element instanceof PredicateEdge) {
			return CallHierarchyUtil.getImageForVisibility(((PredicateEdge) element).getVisibility());
		}
		if ("exported".equals(visibility)) {
			return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
		} else if ("non_exported".equals(visibility)) {
			return ImageRepository.getImage(ImageRepository.PE_PROTECTED);
		} else if ("undefined".equals(visibility)) {
			return ImageRepository.getImage(ImageRepository.PE_UNDEFINED);
		}
		return null;
	}
	
	@Override
	public String getText(Object element) {
		if (element instanceof Predicate) {
			return ((Predicate) element).getLabel();
		} else if (element instanceof PredicateEdge) {
			return ((PredicateEdge) element).getTarget().getLabel();
		}
		return super.getText(element);
	}

}
