/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package org.cs3.pdt.common;

import java.util.List;

import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.core.resources.IFile;

public interface ProcessReconsulterListener {
	
	void afterReconsult(PrologProcess process, String reconsultBehaviour, List<IFile> reconsultedFiles);

}
