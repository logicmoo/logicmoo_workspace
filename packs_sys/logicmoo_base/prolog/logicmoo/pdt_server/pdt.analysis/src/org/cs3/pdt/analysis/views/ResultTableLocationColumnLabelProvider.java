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
package org.cs3.pdt.analysis.views;

import org.cs3.pdt.analysis.model.IResult;
import org.cs3.pdt.analysis.model.IResultElement;
import org.eclipse.jface.viewers.ColumnLabelProvider;

public class ResultTableLocationColumnLabelProvider extends ColumnLabelProvider {
	
	@Override
	public String getText(Object element) {
		if (element instanceof IResult) {
			return "Line: " + Integer.toString(((IResult) element).getLine() + 1);
		}
		if (element instanceof IResultElement) {
			return "";
		}
		return super.getText(element);
	}

}
