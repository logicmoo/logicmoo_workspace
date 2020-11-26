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

import org.cs3.pdt.common.structureElements.SearchModuleElement;
import org.cs3.pdt.connector.util.ExternalPrologFilesProjectUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.DecoratingStyledCellLabelProvider;
import org.eclipse.ui.PlatformUI;

public class DecoratingPrologSearchLabelProvider extends DecoratingStyledCellLabelProvider {
	
	public DecoratingPrologSearchLabelProvider(IStyledLabelProvider labelProvider) {
		super(labelProvider, PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator(), null);
	}
	
	@Override
	public String getToolTipText(Object element) {
		if (element instanceof SearchModuleElement) {
			SearchModuleElement moduleElement = (SearchModuleElement) element;
			IFile file = moduleElement.getFile();
			if (file == null) {
				return null;
			}
			if (ExternalPrologFilesProjectUtils.isExternalFile(file)) {
				return file.getRawLocation().toOSString();
			} else {
				return file.getFullPath().toOSString();
			}
		} else {
			return null;
		}
	}
}


