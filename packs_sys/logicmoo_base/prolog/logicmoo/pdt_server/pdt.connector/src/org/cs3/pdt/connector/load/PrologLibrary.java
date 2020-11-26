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

package org.cs3.pdt.connector.load;

import java.util.Set;

/**
 * A contribution to the Prolog file lookup path.
 * 
 * An instance moreless represents a clause that can be
 * contributed to the file_search_path/2 predicate.
 * @author lukas
 *
 */
public interface PrologLibrary {
	/**
	 * @return the global unique identifier for this library
	 */
	public String getId();
	
	/**
	 * 
	 * @return The resolved absolute path (the prolog flavour) of the 
	 * directory that contains the library
	 */
	public String getPath();
	
	/**
	 * @return The alias this library uses. (see file_search_path/2)
	 */
	public String getAlias();
	
	/**
	 * @return a list of identifiers of libraries this library depends on.
	 */
	public Set<String> getDependencies();
	
	/**
	 * retrieve application-specific data from the library.
	 * 
	 * Clients may attach additional, string valued attributes to the library that are relevant 
	 * to the respective application.
	 * The pdt core for example adds a flag "hidden" which is set for all and runtime libraries.
	 *  
	 * @param attr the attribute name
	 * @return the attribute value or null if not set.
	 */
	public String getAttributeValue(String attr);
}


