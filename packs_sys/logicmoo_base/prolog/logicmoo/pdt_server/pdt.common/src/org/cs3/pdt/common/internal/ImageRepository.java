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

package org.cs3.pdt.common.internal;

import java.net.URL;
import java.util.HashMap;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

public class ImageRepository {

	private static HashMap<String, Image> cache = new HashMap<String, Image>();
	
    public static final String PE_PUBLIC = "public_co.gif";
    public static final String PE_PROTECTED = "protected_co.gif";
    public static final String PE_PRIVATE = "private_co.gif";
    public static final String PE_LOCAL = "variable.gif";
    public static final String PE_INVISIBLE = "invisible.gif";
    
    public static final String PE_UNDEFINED = "ihigh_obj.gif";

    public static final String FILE = "prolog_file.gif";
    public static final String PROLOG_FILE_TRANSPARENT = "prolog_file_transparent.png";
    public static final String PROLOG_FILE_CONSULTED = "prolog_file_consulted.png";
    public static final String PROLOG_FILE_EXTERNAL = "prolog_file_external.gif";
    
    public static final String ENTITY = "package_obj.gif";
    
	public static final String SEARCH_MATCH = "searchm_obj.gif";
	
	public static final String CH_ROOT = "class_hi.gif";
	public static final String CH_REFRESH = "refresh.gif";
	public static final String CH_CALLERS = "ch_callers.gif";
	public static final String CH_CALLEES = "ch_callees.gif";
	public static final String CH_SCOPE_WORKSPACE = "workspace_obj.gif";
	public static final String CH_SCOPE_PROJECT = "prj_obj.gif";
	public static final String CH_HISTORY_LIST = "history_list.gif";

    public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PDTCommonPlugin.getDefault().getBundle().getEntry("/icons/" + icon);
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
