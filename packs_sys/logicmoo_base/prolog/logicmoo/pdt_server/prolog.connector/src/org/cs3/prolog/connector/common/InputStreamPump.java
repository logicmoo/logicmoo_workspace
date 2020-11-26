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

package org.cs3.prolog.connector.common;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;
/**
 * A thread that exhausts InputStream objects.
 *
 */
public class InputStreamPump extends Thread {
	BufferedReader in = null;
	private Writer log;

	public InputStreamPump(InputStream s, Writer writer) {
		super("InputStreamPump");
		this.in = new BufferedReader(new InputStreamReader(s));
		this.log = writer;
	}

	@Override
	public void run() {
		char[] buf = new char[256];
		int read = 0;
		try {
			while(true){
				read = in.read(buf);
				if(read>0){
					dataAvailable(buf,read);
				}
				if(read==-1){
					break;
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
			Debug.report(e);
		}
		finally{
			Debug.info("InputStreamPump stops working!");
		}
	}

	/**
	 * called when data is available on the stream.
	 * @param buffer a char array containing the read data.
	 * @param length the number of <b>new</b> chars available in the buffer. The
	 * newly available data will always be at the beginning of the buffer.  
	 */
	private void dataAvailable(char[] buffer, int length) {
		try {
			log.write(buffer, 0, length);
			log.flush();
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage());
		}
	}
}


