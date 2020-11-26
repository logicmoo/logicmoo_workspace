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

package org.cs3.pdt.common.metadata;

public class PrologSourceLocation {
    private String filePath;
    private int line;
    
    
    public PrologSourceLocation(String filePath, int line){
  	  this.filePath=filePath;
  	  this.line=line;
    }

	public String getFilePath() {
		return filePath;
	}

	public int getLine() {
		return line;
	}


}


