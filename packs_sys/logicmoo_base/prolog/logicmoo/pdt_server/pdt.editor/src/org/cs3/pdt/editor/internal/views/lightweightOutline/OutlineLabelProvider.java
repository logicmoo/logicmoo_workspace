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

package org.cs3.pdt.editor.internal.views.lightweightOutline;

import org.cs3.pdt.common.metadata.Predicate;
import org.cs3.pdt.common.structureElements.PrologTreeElement;
import org.cs3.pdt.editor.internal.ImageRepository;
import org.cs3.pdt.editor.internal.structureElements.OutlineClauseElement;
import org.cs3.pdt.editor.internal.structureElements.OutlineFileElement;
import org.cs3.pdt.editor.internal.structureElements.OutlineModuleElement;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PlatformUI;

class OutlineLabelProvider extends LabelProvider implements IColorProvider/*, IStyledLabelProvider*/ {
	@Override
	public String getText(Object element) {
		if(element instanceof PrologTreeElement) {
			return ((PrologTreeElement) element).getLabel();
		} else if (element instanceof String) {
			return (String) element;
		}
		return "";
	}

	@Override
	public Image getImage(Object element) {

		if(element instanceof Predicate) {
			Predicate prologPredicate = (Predicate) element;
			if (prologPredicate.isPublic() || "user".equals(prologPredicate.getModule())){ 
				return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
			} else if (prologPredicate.isPrivate()) {
				return ImageRepository.getImage(ImageRepository.PE_PRIVATE);
			} else if (prologPredicate.isLocal()) {
				return ImageRepository.getImage(ImageRepository.PE_LOCAL);
			}
			return ImageRepository.getImage(ImageRepository.PE_PROTECTED);
		}
		if(element instanceof OutlineModuleElement) {
			if (((OutlineModuleElement) element).fileEqualToEditorFile()) {
				return ImageRepository.getImage(ImageRepository.PACKAGE);
			} else {
				return ImageRepository.getImage(ImageRepository.MODULE_FOREIGN);
			}
		}
		if (element instanceof OutlineClauseElement) {
			return ImageRepository.getImage(ImageRepository.SEARCH_MATCH);
		}
		if (element instanceof OutlineFileElement) {
			return ImageRepository.getImage(ImageRepository.PROLOG_FILE);
		}
		return null;
	}

	@Override
	public Color getForeground(Object element) {
		if(element instanceof Predicate) {
			Predicate predicate = (Predicate) element;
			if(predicate.isDynamic()) {
			return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_BLUE);
			}
		}	
		return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_BLACK);
	}

	@Override
	public Color getBackground(Object element) {
//		OutlinePredicate prologPredicate = (OutlinePredicate) element;
//		if(prologPredicate.isDynamic()) {
//			return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_GRAY);
//		}
		return null;
	}

}


