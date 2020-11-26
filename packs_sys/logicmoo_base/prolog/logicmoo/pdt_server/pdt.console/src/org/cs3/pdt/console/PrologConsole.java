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

import org.cs3.prolog.connector.process.PrologProcess;

public interface PrologConsole {
	public ConsoleModel getModel();
	public PrologProcess getPrologProcess();
	public void setPrologProcess(PrologProcess process);
	public boolean isVisible();
	public String getText();
	public int getLineAtOffset(int offset);
	public int getOffsetAtLine(int line);
	public int getLineCount();
	public void clearOutput();
	public int getCaretOffset();
	public void setCaretOffset(int offset);
	public int getStartOfInput();
	public String getTextRange(int offset, int length);
	public void ensureConnectionForCurrentPrologProcess();
	
}


