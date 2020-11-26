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

import java.awt.FontMetrics;
import java.awt.Graphics2D;

public class PrefixLabel extends CroppingLabelBase {

	@Override
	protected void fillText(String text, String[] lines, int lineWidth,
			Graphics2D gfx, FontMetrics fontmtx) {

		fillLinesFromStart(text, lines, lineWidth, gfx, fontmtx);
		
		String lastLine = lines[lines.length - 1];
		if (lastLine.length() > 0)
			lines[lines.length - 1] = lastLine.substring(0, lastLine.length() - 1) + "...";
	}

}


