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

package org.cs3.pdt.editor.quickfix;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;

public class PDTMarker implements IMarkerResolutionGenerator {

	public static final String SMELL_NAME = "PDT_Quickfix";
	public static final String QUICKFIX_DESCRIPTION = "PDT_QuickfixDescription";
	public static final String QUICKFIX_ACTION = "PDT_QuickfixAction";
	public static final String PDT_MARKER_ID = "PDT_Marker";

	@Override
	public IMarkerResolution[] getResolutions(IMarker mk) {
		try {
			Object smellName = mk.getAttribute(SMELL_NAME);
			Object quickfixDescription = mk.getAttribute(QUICKFIX_DESCRIPTION);

			if (smellName != null && !smellName.equals("") && quickfixDescription != null && !quickfixDescription.equals("")) {
				return new IMarkerResolution[] {
						new PDTQuickFix(quickfixDescription.toString(), false),
						new PDTQuickFix(quickfixDescription.toString(), true)
				};        	   
			} else  {
				return new IMarkerResolution[0];
			}

		}
		catch (CoreException e) {
			return new IMarkerResolution[0];
		}
	}

}


