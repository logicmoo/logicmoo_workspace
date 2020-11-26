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

import com.google.common.base.Joiner;

public class TermUtil {
	public static String list(Object... elements){
		StringBuilder buffer = new StringBuilder();
		buffer.append("[");
		buffer=Joiner.on(", ").appendTo(buffer, elements);
		buffer.append("]");
		return buffer.toString();
	}
	
	public static String atomList(Object... elements) {
		String[] atoms = atoms(elements);
		return list((Object[])atoms);
	}
	
	public static String atom(Object element){
		return new StringBuilder().append("'").
								   append(element).
								   append("'").
								   toString();
	}
	
	public static String[] atoms(Object... elements){
		String[] atoms = new String[elements.length];
		for(int i=0; i<elements.length; i++){
			atoms[i] = atom(elements[i]); 
		}
		return atoms;
	}
}


