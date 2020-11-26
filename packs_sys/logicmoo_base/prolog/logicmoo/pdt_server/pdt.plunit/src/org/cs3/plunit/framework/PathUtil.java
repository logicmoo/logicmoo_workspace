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

package org.cs3.plunit.framework;


public class PathUtil {

	public static String getProjectPath(Class c){
		String path = stripBinaryPath(getBinPath(c));
		return path.replaceFirst(".test", "");
	}

	public static String getPath(Class c){
		return stripBinaryPath(getBinPath(c));
	}
	
	public static String getBinPath(Class c) {
		String classPath = c.getResource(".").getPath();
		String packagePath = c.getPackage().getName().replace(".", "/");
		String binPath = classPath.replace(packagePath, "");
		return binPath;
	}
	
	private static String stripBinaryPath(String path) {
	    return path.replace("/bin/", "")
                   .replace("/target/classes", "")
                   .replace("/target/test-classes", "");
	}
}


