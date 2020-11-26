/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: beckera (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.console.internal.views.completion;

import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;

public class StyledTextContentAdapter implements IControlContentAdapter, IControlContentAdapter2 {
	
	@Override
	public String getControlContents(Control control) {
		return ((StyledText)control).getText();
	}

	@Override
	public int getCursorPosition(Control control) {
		return ((StyledText)control).getCaretOffset();
	}

	@Override
	public Rectangle getInsertionBounds(Control control) {
		StyledText text= (StyledText)control;
		Point caretOrigin= text.getLocationAtOffset(text.getCaretOffset());
		return new Rectangle(caretOrigin.x + text.getClientArea().x, caretOrigin.y + text.getClientArea().y + 3, 1, text.getLineHeight());
	}

	@Override
	public void insertControlContents(Control control, String contents, int cursorPosition) {
		StyledText text= ((StyledText)control);
		text.insert(contents);
		cursorPosition= Math.min(cursorPosition, contents.length());
		text.setCaretOffset(text.getCaretOffset() + cursorPosition);
	}

	@Override
	public void replaceControlContents(Control control, String contents, int prefixLength, int cursorPosition) {
		StyledText styledText = (StyledText)control;
		int caretOffset = styledText.getCaretOffset();
		styledText.replaceTextRange(caretOffset - prefixLength, prefixLength, contents);
		styledText.setCaretOffset(caretOffset - prefixLength + cursorPosition);
	}

	@Override
	public void setCursorPosition(Control control, int index) {
		((StyledText)control).setCaretOffset(index);
	}

	@Override
	public Point getSelection(Control control) {
		return ((StyledText)control).getSelection();
	}

	@Override
	public void setSelection(Control control, Point range) {
		((StyledText)control).setSelection(range);
	}
}