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

package org.cs3.pdt.graphicalviews.model.realizer.groups;

import java.awt.Color;

import org.cs3.pdt.graphicalviews.model.GraphModel;
import org.cs3.pdt.graphicalviews.preferences.FileAppearancePreferences;

import y.view.NodeLabel;
import y.view.NodeRealizer;


public class FileGroupNodeRealizer extends PrologGroupNodeRealizer {

	public FileGroupNodeRealizer(GraphModel model) {
		super(model);
	}

	public FileGroupNodeRealizer(NodeRealizer nr) {
		super(nr);
	}

	@Override
	protected void createHeaderLabel() {
		NodeLabel label = getLabel();
		label.setAlignment(NodeLabel.LEFT);
		label.setBackgroundColor(FileAppearancePreferences.getFileHeaderColor());
		label.setTextColor(Color.BLACK);
		label.setUnderlinedTextEnabled(true);
		label.setModel(NodeLabel.INTERNAL);
		//label.setConfiguration(LayoutPreferences.getNameCroppingConfiguration());
		
	}

	@Override
	public NodeRealizer createCopy(NodeRealizer nr) {
		return new FileGroupNodeRealizer(nr);
	}
}


