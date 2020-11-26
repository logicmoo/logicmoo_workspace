/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.queries;

import org.cs3.prolog.connector.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;

public abstract class MarkerCreatingSearchQuery extends PDTSearchQuery {
	
	protected boolean createMarkers;
	private String attribute;
	private String value;
	private String markerType;

	public MarkerCreatingSearchQuery(boolean createMarkers, String markerType) {
		super(null, "");
		this.createMarkers = createMarkers;
		this.markerType = markerType;
	}
	
	public MarkerCreatingSearchQuery(boolean createMarkers, String attribute, String value) {
		super(null, "");
		this.createMarkers = createMarkers;
		this.attribute = attribute;
		this.value = value;
	}
	
	@Override
	public IStatus run(IProgressMonitor monitor) {
		clearMarkers();
		return super.run(monitor);
	}

	protected void clearMarkers() {
		try {
			if (markerType != null) {
				ResourcesPlugin.getWorkspace().getRoot().deleteMarkers(markerType, false, IResource.DEPTH_INFINITE);
			} else {
				IMarker[] markers = ResourcesPlugin.getWorkspace().getRoot().findMarkers(IMarker.PROBLEM, false, IResource.DEPTH_INFINITE);
				for (IMarker marker : markers) {
					if (marker.getAttribute(attribute) != null) {
						marker.delete();
					}
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
	}
	
	protected IMarker createMarker(IFile file, String message, int line) throws CoreException {
		IMarker marker = getMarker(file, message);

		marker.setAttribute(IMarker.LINE_NUMBER, line);
		
		return marker;
	}
	
	protected IMarker createMarker(IFile file, String message, int start, int end) throws CoreException {
		IMarker marker = getMarker(file, message);
		
		marker.setAttribute(IMarker.CHAR_START, start);
		marker.setAttribute(IMarker.CHAR_END, end);
		
		return marker;
	}
	
	private IMarker getMarker(IFile file, String message) throws CoreException {
		IMarker marker;
		if (markerType != null) {
			marker = file.createMarker(markerType);
			marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
		} else {
			marker = file.createMarker(IMarker.PROBLEM);
			marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
			marker.setAttribute(attribute, value);
		}
		marker.setAttribute(IMarker.MESSAGE, message);
		return marker;
	}
	
	
}
