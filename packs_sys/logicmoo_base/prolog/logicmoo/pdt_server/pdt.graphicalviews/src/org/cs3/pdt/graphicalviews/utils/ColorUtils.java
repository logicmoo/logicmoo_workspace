/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Ilshat Aliev
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.graphicalviews.utils;

import java.awt.Color;

import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;

public class ColorUtils {

	public static String getColorString(Color color) {
		RGB rgb = new RGB(color.getRed(), color.getGreen(), color.getBlue());
		return StringConverter.asString(rgb);
	}

}
