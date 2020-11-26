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

package org.cs3.pdt.graphicalviews.model.labels;

import java.awt.font.FontRenderContext;

import y.geom.YDimension;
import y.view.YLabel;
import y.view.YLabel.Layout;

public class CroppingLabelLayout implements Layout {

	@Override
	public void calculateContentSize(YLabel label, FontRenderContext rndCtx) {
		
		YDimension dimension = (YDimension)label.getUserData();
		
		if (dimension != null)
			label.setContentSize(dimension.getWidth(), dimension.getHeight());
	}

	@Override
	public boolean contains(YLabel label, double x, double y) {
		return false;
	}
}


