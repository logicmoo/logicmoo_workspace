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

import java.net.URL;
import java.util.HashMap;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

public class ImageRepository {
	
	public static final String ICON_RUN = "run.gif";
	public static final String ICON_REMOVE = "delete.gif";
	public static final String ICON_REFRESH = "refresh.gif";
	public static final String ICON_SELECT_ALL = "enabled_co.gif";
	public static final String ICON_SELECT_NONE = "disabled_co.gif";
	public static final String ICON_COLLAPSE_ALL = "collapseall.gif";
	public static final String ICON_EXPAND_ALL = "expandall.gif";
	
	public static final String ICON_MARKER_ERROR = "error.gif";
	public static final String ICON_MARKER_WARNING = "warning.gif";
	public static final String ICON_MARKER_INFO = "information.gif";
	
	private static HashMap<String, Image> cache = new HashMap<String, Image>();
	
    public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PDTAnalysisPlugin.getDefault().getBundle().getEntry("/icons/" + icon);
        return ImageDescriptor.createFromURL(url);
    }

    public static final Image getImage(String icon) {
        Image image = cache.get(icon);
        if (image == null) {
            image = getImageDescriptor(icon).createImage();
            cache.put(icon, image);
        }
        return image;
    }
  
}
