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

package org.cs3.pdt.editor.internal.views.lightweightOutline;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.custom.BusyIndicator;

public class ToggleFilterAction extends Action {
	
	private TreeViewer viewer;
	private ViewerFilter filter;
	private IPreferenceStore store;
	private String key;
	private ImageDescriptor inactive;
	private ImageDescriptor active;

	ToggleFilterAction(String text, ImageDescriptor inactive, ImageDescriptor active, TreeViewer viewer, ViewerFilter filter) {
		this(text, inactive, active, viewer, filter, null, null);
	}
	
	ToggleFilterAction(String text, ImageDescriptor inactive, ImageDescriptor active, TreeViewer viewer, ViewerFilter filter, IPreferenceStore store, String key) {
		super(text, AS_CHECK_BOX);
		
		this.viewer = viewer;
		this.filter = filter;
		this.store = store;
		this.key = key;
		this.inactive = inactive;
		this.active = active;
		
		if (store != null && key != null) {
			boolean checked = store.getBoolean(key);
			setChecked(checked);
			if (checked) {
				viewer.addFilter(filter);
				setImageDescriptor(active);
			} else {
				setImageDescriptor(inactive);
			}
		} else {
			setChecked(false);
			setImageDescriptor(inactive);
		}
	}
	
	@Override
	public void run() {
		if (isChecked()) {
			viewer.getControl().setRedraw(false);
			BusyIndicator.showWhile(viewer.getControl().getDisplay(), new Runnable() {
				@Override
				public void run() {
					viewer.addFilter(filter);
					setImageDescriptor(active);
				}
			});
			viewer.getControl().setRedraw(true);
		} else {
			viewer.getControl().setRedraw(false);
			BusyIndicator.showWhile(viewer.getControl().getDisplay(), new Runnable() {
				@Override
				public void run() {
					viewer.removeFilter(filter);
					setImageDescriptor(inactive);
				}
			});
			viewer.getControl().setRedraw(true);
		}
		if (store != null && key != null) {
			store.setValue(key, isChecked());
		}
	}
	
}


