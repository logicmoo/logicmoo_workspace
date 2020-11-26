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
package org.cs3.pdt.console.internal;

import java.net.URL;
import java.util.HashMap;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

public class ImageRepository {

	private static HashMap<String, Image> cache = new HashMap<String, Image>();
	
	public final static String GUITRACER = "guitracer.gif";

	public final static String CLEAR = "clear_co.gif";

	public final static String NOGUITRACER = "noguitracer.gif";
	
	public final static String FOLLOW_MODE = "console_following.png";
    
	public final static String MANUAL_MODE = "console.png";
	
	public final static String MANUAL_MODE_FREE = "console_warning.png";
    
	public static final String BREAK = "break.gif";

	public static final String RESTART = "console_restart12.png";
	
	public static final String STOP = "console_stop4.png";
	
	public static final String SELECT_ALL = "selectall.png";
	
	public static final String SELECT_PROCESS = "console.png";
	
	public static final String TRACK_CONTEXT = "synced.gif";

	public static final String PASTE_FILENAME = "paste_filename.gif";

	public static final String NEW_PROCESS = "new_process.png";
	
	public static final String THREAD_MONITOR = "threadmonitor.gif";
	
	public static final String DEBUG_MONITOR = "debugmonitor.gif";

	public static final String ABORT = "abort.gif";

	public static final String TRACE = "suspend_co.gif";
	
	public static final String GEN_LOAD_FILE = "gen_load_file2.gif";

	public static final String HELP = "linkto_help.gif";

	public static final String PREFERENCES = "preferences.png";

	public static final String PREDICATE_PUBLIC = "public_co.gif";

	public static final String PREDICATE_PROTECTED = "protected_co.gif";
	
	public static final String PREDICATE_PRIVATE = "private_co.gif";
	
	public static final String PREDICATE_BUILTIN = "built_in.gif";
	
	public static final String ENTITY = "package_obj.gif";
	
	public static final String ATOM = "atom.png";
	
    public static final ImageDescriptor getImageDescriptor(String icon) {
        URL url = PrologConsolePlugin.getDefault().getBundle().getEntry("/icons/" + icon);
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


