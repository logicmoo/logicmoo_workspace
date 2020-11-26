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

package org.cs3.pdt.console;
import java.util.EventObject;

public class ConsoleModelEvent extends EventObject {
	
	/**
     * Comment for <code>serialVersionUID</code>
     */
    private static final long serialVersionUID = 1L;
    private String oldLineState=null;
    private String newLineState=null;
    private String output = null;
    private boolean errorStream = false;
    private boolean singleCharMode;
    private String commitText;

    
    /**
     * use this constructor upon connect/disconnect.
     */
    public ConsoleModelEvent(Object src) {
        super(src);
    }
    
    /**
	 * use this constructor, when the linebuffer changed
	 */
	public ConsoleModelEvent(Object src, String oldBuffer, String newBuffer) {
		super(src);
		this.oldLineState=oldBuffer;
		this.newLineState=newBuffer;
	}
	
	/**
     * use this constructor when the linebuffer was committed
     */
    public ConsoleModelEvent(Object src, String line) {
        super(src);
        this.commitText=line;
    }

    /**
     * use this constructor when output was read
     */
    public ConsoleModelEvent(Object src, String output, boolean isError) {
        super(src);
        this.output=output;
        this.errorStream=isError;
    }
    
    /**
     * use this constructor when the mode changed
     */
    public ConsoleModelEvent(Object src, boolean newMode) {
        super(src);
        this.singleCharMode=newMode;
    }

    public String getOldLineState(){
		return oldLineState;
	}
	
	public String getNewLineState(){
		return newLineState;
	}

    public void setRecievedOutput(String line) {
        this.output=line;
        
    }

    public void setModeChangedTo(boolean singleCharMode) {
        this.singleCharMode=singleCharMode;
        
    }

    public void setCommitText(String lineBuffer) {
        this.commitText=lineBuffer;        
    }
	
    public String getCommitText() {
        return commitText;
    }
    
	public boolean isErrorStream(){
	    return errorStream;
	}

    public boolean isSingleCharMode() {
        return singleCharMode;
    }
    public String getOutput() {
        return output;
    }
    public void setErrorStream(boolean errorStream) {
        this.errorStream = errorStream;
    }
}


