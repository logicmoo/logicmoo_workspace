package org.cs3.pdt.graphicalviews.focusview;

import org.eclipse.jface.window.ToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;

public class NavigationToolTip extends ToolTip {
	private String content;
	
	public NavigationToolTip(Control control) {
		super(control);
	}

	@Override
	protected Composite createToolTipContentArea(Event event, Composite parent) {
		StyledText t = new StyledText(parent, SWT.BALLOON);
		t.setText(content);
		return t;
	}

	public void setContent(String text) {
		this.content = text;
	}

}
