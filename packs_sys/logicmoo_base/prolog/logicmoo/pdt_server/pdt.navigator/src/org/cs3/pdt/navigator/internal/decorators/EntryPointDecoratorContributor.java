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

package org.cs3.pdt.navigator.internal.decorators;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTDecorator;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.navigator.internal.ImageRepository;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;

public class EntryPointDecoratorContributor implements ILightweightLabelDecorator, PDTDecorator {

	private static final String ENTRY_POINT_SUFFIX = " [entry point]";
	private Vector<ILabelProviderListener> listeners = new Vector<ILabelProviderListener>();

	@Override
	public void addListener(ILabelProviderListener l) {

		synchronized (listeners) {
			if(!listeners.contains(l)){
				listeners.add(l);
			}
		}
		
	}

	@Override
	public void dispose() {
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener l) {
		synchronized (listeners) {
			if(listeners.contains(l)){
				listeners.remove(l);
			}
		}
		
	}

	@Override
	public void decorate(Object element, IDecoration decoration) {
		if(!(element instanceof IFile)){
			return;
		}
		
		PDTCommonPlugin.getDefault().addDecorator(this);
		
		IFile file = (IFile) element;
		if (file.exists()) {
			if (PDTCommonPlugin.getDefault().isEntryPoint(file)) {
				decoration.addOverlay(ImageRepository.getImageDescriptor(ImageRepository.PROLOG_ENTRY_POINT));
				decoration.addSuffix(ENTRY_POINT_SUFFIX);
			}
		}
		
		
	}

	private void fireLabelProviderChanged() {
		final LabelProviderChangedEvent e = new LabelProviderChangedEvent(this);
		Vector<ILabelProviderListener> clone=new Vector<ILabelProviderListener>();
		synchronized(listeners){
			clone.addAll(listeners);
		}
		for (Iterator<ILabelProviderListener> it = clone.iterator(); it.hasNext();) {
			final ILabelProviderListener l = (ILabelProviderListener) it.next();
			UIUtils.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					l.labelProviderChanged(e);
				}
			});
		}
	}

	@Override
	public void updateDecorator() {
		fireLabelProviderChanged();
	}

}


