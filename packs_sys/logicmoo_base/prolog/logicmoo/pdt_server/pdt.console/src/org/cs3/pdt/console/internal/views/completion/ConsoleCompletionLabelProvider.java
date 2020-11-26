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

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class ConsoleCompletionLabelProvider extends LabelProvider implements ILabelProvider {

	@Override
	public Image getImage(Object element) {
		if (element instanceof ComparableCompletionProposal) {
			return ((ComparableCompletionProposal) element).getImage();
		} else {
			return null;
		}
	}

	@Override
	public String getText(Object element) {
		if (element instanceof ComparableCompletionProposal) {
			return ((ComparableCompletionProposal) element).getLabel();
		} else {
			return super.getText(element);
		}
	}

}


