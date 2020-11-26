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
 */
package org.cs3.prolog.connector.internal.process.socket;

import java.io.PrintStream;

public class SimpleLogBuffer implements LogBuffer {
    private final static int MAX_LENGTH=500000;//this should be enough. 
    private StringBuffer buffer=new StringBuffer();
    private String lastKey=null;
    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, char)
     */
    @Override
	public synchronized void log(String key, char c) {
        log(key,new byte[]{(byte) c});
    }

    private synchronized void cutHead(){
        int cut = buffer.length()-MAX_LENGTH;
        if(cut>0){
            buffer.delete(0,Math.min(cut*2,buffer.length()+1/2));
        }
    }
    
    private synchronized void setKey(String key) {
		if(lastKey!=null){
			buffer.append("</"+lastKey+">\n");
		} else if(key!=null || !lastKey.equals(key)){
			buffer.append("<"+key+">");
            lastKey=key;
        }			
        cutHead();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, byte[], int, int)
     */
    @Override
	public synchronized void log(String key, byte[] buf, int offset, int len) {
        setKey(key);
        if(len>0) {
            String string = new String(buf,offset,len);
			this.buffer.append(string);
        }
        else{
            this.buffer.append("<<EOF>>");
        }
        cutHead();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, java.lang.String)
     */
    @Override
	public synchronized void log(String key, String s) {
        byte[] bytes = s.getBytes();
		log(key,bytes,0,bytes.length);		
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, byte[])
     */
    @Override
	public synchronized void log(String key, byte[] b) {
		log(key,b,0,b.length);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#printLog(java.io.PrintStream)
     */
    @Override
	public synchronized void printLog(PrintStream out) {
       out.println(buffer.toString());        
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
	public synchronized String toString() {     
        cutHead();
        return buffer.toString();
    }
}


