/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 * Created on 05.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.editor.internal;

import java.net.URL;
import java.util.HashMap;

import org.cs3.pdt.editor.PDTPlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

/**
 * @author xproot
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */
public class ImageRepository {

	public final static String GUITRACER = "guitracer.gif";

	public final static String CLEAR = "clear.gif";

	public final static String NOGUITRACER = "noguitracer.gif";
	
	public final static String FILTER = "filter.gif";
	
	public final static String SORT = "alphab_sort_co.gif";
	
    public final static String PE_PUBLIC = "public_co.gif";

    public final static String PE_BUILT_IN = "built_in.gif";

    public final static String VERIFIED_MATCH = "public_co.gif";

    public final static String PE_PROTECTED = "protected_co.gif";
    public final static String POTENTIAL_MATCH = "protected_co.gif";
    
    public final static String PE_MULTIFILE = "multifile.gif";

    public final static String PE_LOCAL = "variable.gif";

    public final static String PE_MODULE = "module_2.gif";

    public final static String PE_CLAUSE = "clause.gif";

    public final static String PE_TERM = "term.gif";
    
    public final static String PACKAGE = "package_obj.gif";
    
    public static final String MODULE_FOREIGN = "module_foreign.png";
    
    public final static String PROLOG_FILE = "prolog_file_transparent.png";
    
    public final static String PROLOG_FILE_UNCONSULTED = "prolog_file.gif";

    public static final String PROLOG_ENTRY_POINT = "entry_point.gif";
    
    public static final String PROLOG_FILE_CONSULTED = "prolog_file_consulted.png";
    public static final String PROLOG_FILE_CONSULTED_OLD = "prolog_file_consulted_old.png";
    
    public static final String QLF_FILE = "qlf_file.png";
    public static final String QLF_FILE_CONSULTED = "qlf_file_consulted.png";
    public static final String QLF_FILE_CONSULTED_OLD = "qlf_file_consulted_old.png";
    
    public static final String PROLOG_FOLDER_CONSULTED = "prolog_folder_consulted.png";
    
    public final static String PROLOG_FILE_EXTERNAL_NOT_LINKED = "prolog_file_external.gif";
    public final static String PROLOG_FILE_EXTERNAL = "prolog_file_external.png";
    public final static String PROLOG_FILE_EXTERNAL_CONSULTED = "prolog_file_external_consulted.png";
    public final static String PROLOG_FILE_EXTERNAL_CONSULTED_OLD = "prolog_file_external_consulted_old.png";

    private static HashMap<String, Image> cache = new HashMap<String, Image>();
	

    public static final String PE_ATOM = "atom.png";

	public static final String RESTART = "restart.gif";

	public static final String PE_PRIVATE = "private_co.gif";
	
	public static final String REFRESH = "refresh.gif";
	
	public static final String PREFERENCES = "preferences.png";

	public static final String MESSAGE_INFO = "message_info.gif";
	
	public static final String SEARCH_MATCH = "searchm_obj.gif";

    public static final String FILTER_PRIVATE = "protected_co_crossed.gif";
    public static final String FILTER_SYSTEM = "system_filter.png";
    public static final String NO_FILTER_SYSTEM = "system.png";
    
    public static final String TEMPLATE = "template_obj.gif";


    public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PDTPlugin.getDefault().getBundle().getEntry("/icons/" + icon);
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


