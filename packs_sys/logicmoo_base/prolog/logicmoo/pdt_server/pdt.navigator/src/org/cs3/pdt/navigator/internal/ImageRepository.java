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

package org.cs3.pdt.navigator.internal;

import java.net.URL;
import java.util.HashMap;

import org.cs3.pdt.navigator.PDTNavigatorPlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

public class ImageRepository {
	
	private static HashMap<String, Image> cache = new HashMap<String, Image>();
	
    public static final String PROLOG_ENTRY_POINT = "entry_point.gif";
    
    public static final String PROLOG_FILE_UNCONSULTED = "prolog_file.gif";
    public static final String PROLOG_FILE_CONSULTED = "prolog_file_consulted.png";
    public static final String PROLOG_FILE_CONSULTED_OLD = "prolog_file_consulted_old.png";
    public static final String QLF_FILE_CONSULTED = "qlf_file_consulted.png";
    public static final String QLF_FILE_CONSULTED_OLD = "qlf_file_consulted_old.png";
    public static final String PROLOG_FOLDER_CONSULTED = "prolog_folder_consulted.png";

    public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PDTNavigatorPlugin.getDefault().getBundle().getEntry("/icons/" + icon);
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
