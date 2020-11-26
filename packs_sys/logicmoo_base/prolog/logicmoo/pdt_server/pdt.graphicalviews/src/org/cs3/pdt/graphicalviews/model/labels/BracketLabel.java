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

public class BracketLabel extends CroppingLabelBase {

	@Override
	protected void fillText(String text, String[] lines, int lineWidth, Graphics2D gfx, FontMetrics fontmtx) {

		if (text.length() < 3) {
			lines[0] = "...";
			return;
		}
		
		int pl = lines.length / 2;
		int cl = lines.length % 2;
		
		int center = text.length() / 2;
		String leftPart = text.substring(0, center);
		String rightPart = text.substring(center);
		
		String[] part1 = new String[pl];
		String[] part2 = new String[pl];
		
		if (pl > 0) {
			fillLinesFromStart(leftPart, part1, lineWidth, gfx, fontmtx);
			fillLinesFromEnd(rightPart, part2, lineWidth, gfx, fontmtx);
			
			for (int i = 0; i < pl; i++) {
				lines[i] = part1[i];
				lines[pl + i + cl] = part2[i];
			}
			
		}
		if (cl == 1) {
			int leftLength = sumLengths(part1);
			leftPart = leftPart.substring(leftLength);
			
			int rightLength = sumLengths(part2);
			rightPart = rightPart.substring(0, rightPart.length() - rightLength);
			
			String[] centerLine = new String[1];
			
			fillLinesFromStart(leftPart, centerLine, lineWidth / 2, gfx, fontmtx);
			lines[pl] = centerLine[0];
			
			centerLine[0] = null;
			
			fillLinesFromEnd(rightPart, centerLine, lineWidth / 2, gfx, fontmtx);
			lines[pl] += "..." + centerLine[0].substring(1);
		}
		else {
			if (pl > 0) {
				String l = lines[pl - 1]; 
				lines[pl - 1] = l.substring(0, l.length() - 1) + "...";
				
				lines[pl] = "..." + lines[pl].substring(1);
			}
			
		}
		
		
	}

	private int sumLengths(String[] list) {
		int sum = 0;
		for (String l : list) {
			if (l != null)
				sum += l.length();
		}
		return sum;
	}

}


