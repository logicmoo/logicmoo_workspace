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

package org.cs3.pdt.graphicalviews.internal;

import java.net.URL;
import java.util.HashMap;

import org.cs3.pdt.graphicalviews.main.PluginActivator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

public class ImageRepository {

	private static HashMap<String, Image> cache = new HashMap<String, Image>();
	
	public static final String HIERARCHY = "hierarchy.ico";
	public static final String ORGANIC = "organic.ico";
	public static final String PREFERENCES = "preferences.png";
	public static final String HELP = "help.gif";
	public static final String REFRESH = "refresh.gif";
	public static final String MOVE = "move.png";
	public static final String M = "m.png";
	public static final String I = "i.png";
	public static final String P = "p.png";
	public static final String S = "s.png";
	
	public static final String INPUT_TYPE_PROJECT = "prj_obj.gif";
	public static final String INPUT_TYPE_LIBRARY = "package_obj.gif";
	public static final String INPUT_TYPE_RECURSIVE_LIBRARY = "packages.gif";
	
	public static final String CONFIGURE_LOGTALK_GRAPH = "configure_graph.png";
	
	public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PluginActivator.getDefault().getBundle().getEntry("/icons/" + icon);
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


