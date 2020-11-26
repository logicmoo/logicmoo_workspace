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

package org.cs3.pdt.editor.internal.editors;

/*
 * contents copied from org.eclipse.ui.texteditor.MarkerAnnotation
 * --lu 
 */

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.texteditor.MarkerAnnotation;

public class AnnotationHover implements IAnnotationHover {

	@Override
    public String getHoverInfo(ISourceViewer sourceViewer, int lineNumber) {
        String[] messages = getMessagesForLine(sourceViewer, lineNumber);

        if (messages.length == 0)
            return null;

        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < messages.length; i++) {
            buffer.append(messages[i]);
            if (i < messages.length - 1)
                buffer.append(System.getProperty("line.separator")); //$NON-NLS-1$
        }
        return buffer.toString();
    }

    private String[] getMessagesForLine(ISourceViewer viewer, int line) {
        IDocument document = viewer.getDocument();
        IAnnotationModel model = viewer.getAnnotationModel();

        if (model == null)
            return new String[0];

        ArrayList<String> messages = new ArrayList<String>();

        Iterator<?> iter = model.getAnnotationIterator();
        while (iter.hasNext()) {
            Object object = iter.next();
            if (object instanceof MarkerAnnotation) {
                MarkerAnnotation annotation = (MarkerAnnotation) object;
                if (compareRulerLine(model.getPosition(annotation),
                    document,
                    line)) {
                    IMarker marker = annotation.getMarker();
                    String message =
                        marker.getAttribute(IMarker.MESSAGE, (String) null);
                    if (message != null && message.trim().length() > 0)
                        messages.add(message);
                }
            }
        }
        return messages.toArray(new String[messages.size()]);
    }

    private boolean compareRulerLine(
        Position position,
        IDocument document,
        int line) {

        try {
            if (position.getOffset() > -1 && position.getLength() > -1) {
                return document.getLineOfOffset(position.getOffset()) == line;
            }
        } catch (BadLocationException e) {
        }
        return false;
    }
}


