/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.editor.internal.contentassistant;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.cs3.pdt.editor.internal.contentassistant.templates.TemplateReader;
import org.eclipse.jface.text.IDocument;

public class DefaultCompletion {
	
	private static ArrayList<DefaultCompletion> defaultCompletions;
	
	private static ArrayList<DefaultCompletion> getDefaultCompletions() {
		if (defaultCompletions == null) {
			defaultCompletions = new ArrayList<DefaultCompletion>();
			defaultCompletions.addAll(TemplateReader.readDefaultCompletions());
//			defaultCompletions.add(new DefaultCompletion(FILE_TYPE_LOGTALK, "public", ":- public(${Predicate}).\n"));
//			defaultCompletions.add(new DefaultCompletion(FILE_TYPE_LOGTALK, "protected", ":- protected(${Predicate}).\n"));
//			defaultCompletions.add(new DefaultCompletion(FILE_TYPE_LOGTALK, "private", ":- private(${Predicate}).\n"));
		}
		return defaultCompletions;
	}
	
	public static void addDefaultCompletions(String fileName, IDocument document, int begin, int len, String prefix, List<ComparableTemplateCompletionProposal> proposals) {
		if (fileName.endsWith(".lgt") || fileName.endsWith(".logtalk")) {
			for (DefaultCompletion com : getDefaultCompletions()) {
				if (com.canApply(fileName, prefix)) {
					proposals.add(new SimpleCompletionProposal(document, com.key, com.name, com.completionWithCurrentDate(), begin, len));
				}
			}
		}
	}
	
	public static final int FILE_TYPE_ANY = 0;
	public static final int FILE_TYPE_PROLOG = 1;
	public static final int FILE_TYPE_LOGTALK = 2;
	
	private int fileType;
	private String key;
	private String completion;
	private String name;
	
	public DefaultCompletion(int fileType, String key, String completion, String name) {
		this.fileType = fileType;
		this.key = key;
		this.completion = completion;
		this.name = name;
	}
	
	private boolean canApply(String fileName, String prefix) {
		return canApplyToFile(fileName) && canApplyToPrefix(prefix);
	}
	
	private boolean canApplyToFile(String fileName) {
		switch (fileType) {
		case FILE_TYPE_ANY:
			return true;
		case FILE_TYPE_PROLOG:
			return fileName.endsWith(".pl") || fileName.endsWith(".pro") || fileName.endsWith(".prolog");
		case FILE_TYPE_LOGTALK:
			return fileName.endsWith(".lgt") || fileName.endsWith(".logtalk");
		default:
			return false;
		}
	}
	
	private boolean canApplyToPrefix(String prefix) {
		return key.startsWith(prefix);
	}
	
	private String completionWithCurrentDate() {
		if (completion.contains("`date +%Y/%m/%d`")) {
			return completion.replace("`date +%Y/%m/%d`", Calendar.getInstance().get(Calendar.YEAR) + "/" + (Calendar.getInstance().get(Calendar.MONTH) + 1) + "/" + Calendar.getInstance().get(Calendar.DAY_OF_MONTH));
		} else {
			return completion;
		}
	}

}
