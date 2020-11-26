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
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class LocationTableLabelProvider extends LabelProvider implements ITableLabelProvider {
	
	private static final int ICON_COLUMN = 0;
	private static final int LINE_COLUMN = 1;
	private static final int CALL_COLUMN = 2;

	@Override
	public Image getColumnImage(Object element, int columnIndex) {
		if (columnIndex == ICON_COLUMN) {
			return ImageRepository.getImage(ImageRepository.SEARCH_MATCH);
		}
		return null;
	}

	@Override
	public String getColumnText(Object element, int columnIndex) {
		if (element instanceof Location) {
			switch (columnIndex) {
			case LINE_COLUMN:
				return Integer.toString(((Location) element).getLine());
			case CALL_COLUMN:
				return ((Location) element).getText();
			}
		}
		return null;
	}

}
