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

import java.util.Map;

import y.view.NodeLabel;
import y.view.YLabel;

public class NodeLabelConfigurationsInitializer {
	public static void initialize() {
		registerLabel(new PrefixLabel());
		registerLabel(new PostfixLabel());
		registerLabel(new BracketLabel());
		registerLabel(new MiddleLabel());
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private static void registerLabel(CroppingLabelBase label) {
		YLabel.Factory factory = NodeLabel.getFactory();

		Map implementationsMap = factory.createDefaultConfigurationMap();
		
		implementationsMap.put(YLabel.Painter.class, label);
		implementationsMap.put(YLabel.Layout.class, new CroppingLabelLayout());

		// Add the first configuration to the factory.
		factory.addConfiguration(label.getClass().getSimpleName(), implementationsMap);
	}
}


