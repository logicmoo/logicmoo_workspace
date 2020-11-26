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
package org.cs3.pdt.analysis;

import org.eclipse.core.resources.IMarker;

public class PDTAnalysis {

	public static final String MARKER_TYPE = "org.cs3.pdt.analysis.marker";
	
	public static final String ANALYSIS_NAME_ATTRIBUTE = "org.cs3.pdt.analyis.name";
	
	public static final String SEVERITY_INFO = "info";
	public static final String SEVERITY_WARNING = "warning";
	public static final String SEVERITY_ERROR = "error";
	
	public static final int getMarkerSeverity(String severity) {
		switch (severity) {
		case SEVERITY_ERROR:
			return IMarker.SEVERITY_ERROR;
		case SEVERITY_WARNING:
			return IMarker.SEVERITY_WARNING;
		case SEVERITY_INFO:
		default:
			return IMarker.SEVERITY_INFO;
		}
	}
	
	public static final String getSeverityText(int severity) {
		switch (severity) {
		case IMarker.SEVERITY_ERROR:
			return SEVERITY_ERROR;
		case IMarker.SEVERITY_WARNING:
			return SEVERITY_WARNING;
		case IMarker.SEVERITY_INFO:
		default:
			return SEVERITY_INFO;
		}
	}
	
}
