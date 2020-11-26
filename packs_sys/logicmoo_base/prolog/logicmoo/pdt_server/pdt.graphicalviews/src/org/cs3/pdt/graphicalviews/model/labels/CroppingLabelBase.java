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

import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import y.geom.OrientedRectangle;
import y.view.AbstractCustomLabelPainter;
import y.view.YLabel;

public abstract class CroppingLabelBase extends AbstractCustomLabelPainter {

	public static final int BORDER_PADDING = 6;

	public CroppingLabelBase() {
		super();
	}

	@Override
	public void paintContent(YLabel label, Graphics2D gfx, double x, double y,
			double width, double height) {

		gfx.setColor(Color.BLACK);
		FontMetrics fontmtx = gfx.getFontMetrics(gfx.getFont());

		if (width < 16 || height < 16) {
			return;
		}

		if (width < 32) {
			paintString("...", x, y, width, height, gfx, fontmtx);
			return;
		}

		Rectangle2D bounds = fontmtx.getStringBounds(label.getText(), gfx);

		int lineHeight = fontmtx.getHeight();

		String text = label.getText();

		if (bounds.getWidth() < width - 2 * BORDER_PADDING) {

			paintString(text, x, y, width, height, gfx, fontmtx);
			return;
		}
		
		int maxLineWidth = (int) (width - 2 * BORDER_PADDING) - 24;
		int maxLinesCnt = (int) ((height - 2 * BORDER_PADDING) / lineHeight);
		int linesCnt = (int) Math.ceil(bounds.getWidth() / maxLineWidth);
		int avgLineWidth = (int) (bounds.getWidth() / linesCnt);

		if (maxLineWidth <= 0)
			return;
		
		if (linesCnt <= maxLinesCnt) {
			String[] lines = new String[linesCnt];

			fillLinesFromStart(text, lines, avgLineWidth, gfx, fontmtx);

			paintStrings(lines, x, y, width, height, gfx, fontmtx);
			return;
		} else {
			String[] lines = new String[maxLinesCnt];

			fillText(text, lines, maxLineWidth, gfx, fontmtx);

			paintStrings(lines, x, y, width, height, gfx, fontmtx);
			return;
		}
	}

	protected abstract void fillText(String text, String[] lines, int lineWidth, Graphics2D gfx, FontMetrics fontmtx);

	protected void fillLinesFromStart(String text, String[] lines, int lineWidth, Graphics2D gfx, FontMetrics fontmtx) {
		
		int maxCharWidth = getMaxCharWidth(gfx, fontmtx);
		
		int beginIndex = 0;
		int estCnt = lineWidth / maxCharWidth;

		for (int spIndex = 0; spIndex < lines.length && beginIndex < text.length(); spIndex++) {
			
			int estEnd = beginIndex + estCnt;
			
			for (int i = 0; estEnd + i < text.length(); i++) {
				
				int w = (int) fontmtx.getStringBounds(text, beginIndex, estEnd + i, gfx).getWidth();
				
				if (w > lineWidth) {
					lines[spIndex] = text.substring(beginIndex, estEnd + i);
					beginIndex = estEnd + i;
					break;
				}
			}
			if (lines[spIndex] == null) {
				lines[spIndex] = text.substring(beginIndex);
				beginIndex = text.length();
			}
		}
	}
	
	protected void fillLinesFromEnd(String text, String[] lines, int lineWidth, Graphics2D gfx, FontMetrics fontmtx) {
		
		int maxCharWidth = getMaxCharWidth(gfx, fontmtx);
		
		int endIndex = text.length();
		int estCnt = lineWidth / maxCharWidth;
		
		for (int spIndex = lines.length - 1; spIndex >= 0 && endIndex >= 0; spIndex--) {
			
			int estBegin = endIndex - estCnt;
			
			for (int i = 0; estBegin - i > 0; i++) {
				
				int w = (int) fontmtx.getStringBounds(text, estBegin - i, endIndex, gfx).getWidth();
				
				if (w > lineWidth) {
					lines[spIndex] = text.substring(estBegin - i, endIndex);
					endIndex = estBegin - i;
					break;
				}
			}
			
			if (lines[spIndex] == null) {
				lines[spIndex] = text.substring(0, endIndex);
				endIndex = -1;
			}
		}
	}

	protected int getMaxCharWidth(Graphics2D gfx, FontMetrics fontmtx) {
		return (int) fontmtx.getStringBounds("_", gfx).getWidth();
	}

	protected void paintString(String text, double x, double y, double width,
			double height, Graphics2D gfx, FontMetrics fontmtx) {

		paintStrings(new String[] { text }, x, y, width, height, gfx, fontmtx);
	}

	protected void paintStrings(String[] lines, double x, double y, double width,
			double height, Graphics2D gfx, FontMetrics fontmtx) {

		int lineHeight = fontmtx.getHeight();
		y += height / 2 - lineHeight * (lines.length - 1) / 2 - lineHeight + 4;

		for (String l : lines) {

			y += lineHeight;
			
			if (l == null || "".equals(l)) continue;
			
			Rectangle2D bounds = fontmtx.getStringBounds(l, gfx);
			
			gfx.drawString(l, (int) (x + width / 2 - bounds.getWidth() / 2), (int) y);
		}
	}

	@Override
	public OrientedRectangle getIconBox(YLabel arg0) {
		return null; // code never executes
	}

	@Override
	public OrientedRectangle getTextBox(YLabel arg0) {
		return null; // code never executes
	}

}


