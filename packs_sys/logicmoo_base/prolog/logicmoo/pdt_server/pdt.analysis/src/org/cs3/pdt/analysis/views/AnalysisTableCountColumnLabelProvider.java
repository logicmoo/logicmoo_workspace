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

import org.cs3.pdt.analysis.model.IAnalysis;
import org.cs3.pdt.analysis.model.IAnalysisCategory;
import org.cs3.pdt.analysis.model.IFactbase;
import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Control;

public class AnalysisTableCountColumnLabelProvider extends ColumnLabelProvider {
	
	private FontRegistry registry = new FontRegistry();
	private Control control;
	
	@Override
	protected void initialize(ColumnViewer viewer, ViewerColumn column) {
		control = viewer.getControl();
	}
	
	@Override
	public Font getFont(Object element) {
		if (element instanceof IAnalysisCategory) {
			Font font = control.getFont();
			return registry.getBold(font.getFontData()[0].getName());
		}
		return super.getFont(element);
	}
	
	@Override
	public String getText(Object element) {
		if (element instanceof IAnalysisCategory) {
			IAnalysisCategory category = (IAnalysisCategory) element;
			IFactbase factbase = category.getFactbase();
			return Integer.toString(factbase.getNumberOfResults(category));
		} else if (element instanceof IAnalysis) {
			IAnalysis analysis = (IAnalysis) element;
			IFactbase factbase = analysis.getFactbase();
			return Integer.toString(factbase.getNumberOfResults(analysis));
		}
		return super.getText(element);
	}

}
