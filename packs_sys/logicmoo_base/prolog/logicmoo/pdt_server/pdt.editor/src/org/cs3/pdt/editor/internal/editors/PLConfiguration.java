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

import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.editor.internal.contentassistant.NaivPrologContentAssistProcessor;
import org.cs3.pdt.editor.internal.views.lightweightOutline.PrologOutlineInformationControl;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.AbstractInformationControlManager;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.information.IInformationProvider;
import org.eclipse.jface.text.information.InformationPresenter;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.quickassist.IQuickAssistAssistant;
import org.eclipse.jface.text.quickassist.QuickAssistAssistant;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;

@SuppressWarnings("deprecation")
public class PLConfiguration extends SourceViewerConfiguration {
	private PLDoubleClickStrategy doubleClickStrategy;
	private PLScanner scanner;
	private ColorManager colorManager;
	private IContentAssistant assistant;

	/*
	 * FIXME should not depend on editor
	 * 
	 * 
	 * this was added for because completion, etc. needs some way to associate
	 * the document its working on with a prolog resource - it needs to get a
	 * prolog helper for that project.
	 * 
	 * currently, we assume that completion is only used within the editor. We
	 * can use its input to resolve the connected resource. -> project -> nature ->
	 * plhelper.
	 */
	private PLEditor editor;


	public PLConfiguration(ColorManager colorManager, PLEditor editor) {
		this.colorManager = colorManager;
		this.editor = editor;
	}

	@Override
	public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
		return new String[] { IDocument.DEFAULT_CONTENT_TYPE,
				PLPartitionScanner.PL_COMMENT,
				PLPartitionScanner.PL_MULTI_COMMENT,
				PLPartitionScanner.PL_SINGLE_QUOTED_STRING,
				PLPartitionScanner.PL_DOUBLE_QUOTED_STRING};
	}

	@Override
	public ITextDoubleClickStrategy getDoubleClickStrategy(
			ISourceViewer sourceViewer, String contentType) {
		if (doubleClickStrategy == null)
			doubleClickStrategy = new PLDoubleClickStrategy();
		return doubleClickStrategy;
	}

	protected PLScanner getPLScanner() {
		if (scanner == null) {
			reinitScanner();
		}
		return scanner;
	}


	public void reinitScanner() {
		try {
			scanner = new PLScanner(editor, colorManager);
		} catch (CoreException e) {
			Debug.report(e);
		} catch (PrologProcessException e) {
		}
		scanner.setDefaultReturnToken(new Token(new TextAttribute(colorManager
				.getColor(colorManager.getDefaultColor()))));
	}

	@Override
	public IPresentationReconciler getPresentationReconciler(
			ISourceViewer sourceViewer) {
		PresentationReconciler reconciler = new PresentationReconciler();

		NonRuleBasedDamagerRepairer ndr = new NonRuleBasedDamagerRepairer(
				new TextAttribute(colorManager
						.getColor(colorManager.getCommentColor())));
		reconciler.setDamager(ndr, PLPartitionScanner.PL_MULTI_COMMENT);
		reconciler.setRepairer(ndr, PLPartitionScanner.PL_MULTI_COMMENT);
		
		DefaultDamagerRepairer dr = new DefaultDamagerRepairer(getPLScanner());
		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

		ndr = new NonRuleBasedDamagerRepairer(new TextAttribute(colorManager
				.getColor(colorManager.getCommentColor())));
		reconciler.setDamager(ndr, PLPartitionScanner.PL_COMMENT);
		reconciler.setRepairer(ndr, PLPartitionScanner.PL_COMMENT);

		ndr = new NonRuleBasedDamagerRepairer(new TextAttribute(colorManager
				.getColor(colorManager.getStringColor())));
		reconciler.setDamager(ndr, PLPartitionScanner.PL_SINGLE_QUOTED_STRING);
		reconciler.setRepairer(ndr, PLPartitionScanner.PL_SINGLE_QUOTED_STRING);

		ndr = new NonRuleBasedDamagerRepairer(new TextAttribute(colorManager
				.getColor(colorManager.getStringColor())));
		reconciler.setDamager(ndr, PLPartitionScanner.PL_DOUBLE_QUOTED_STRING);
		reconciler.setRepairer(ndr, PLPartitionScanner.PL_DOUBLE_QUOTED_STRING);
		
		return reconciler;
	}

	@Override
	public IAutoEditStrategy[] getAutoEditStrategies(ISourceViewer sourceViewer, String contentType) {
		return new IAutoEditStrategy[]{new PLAutoIndentStrategy()};
	}

	@Override
	public IAnnotationHover getAnnotationHover(ISourceViewer sourceViewer) {
		return new AnnotationHover();
	}

	@Override
	public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {
		if (assistant != null)
			return assistant;
		ContentAssistant assistant = null;

		assistant = new ContentAssistant();
		NaivPrologContentAssistProcessor processor = new NaivPrologContentAssistProcessor() {
			@Override
			protected IFile getFile() {
				if (editor == null) {
					return null;
				}
				IEditorInput input = editor.getEditorInput();
				if (input == null) {
					return null;
				}
				if (!(input instanceof IFileEditorInput)) {
					return null;
				}
				IFileEditorInput input2 = (IFileEditorInput) input;
				IFile file = input2.getFile();
				return file;
			}
		};
		assistant.setContentAssistProcessor(processor, IDocument.DEFAULT_CONTENT_TYPE);
		assistant.setContentAssistProcessor(processor, PLPartitionScanner.PL_SINGLE_QUOTED_STRING);

		assistant.enableAutoActivation(true);
		assistant.setAutoActivationDelay(500);
		assistant.install(sourceViewer);
		assistant.setInformationControlCreator(new IInformationControlCreator() {
			@Override
			public IInformationControl createInformationControl(Shell parent) {

				return new DefaultInformationControl(parent);
			}
		});
		assistant.enableColoredLabels(true);
		this.assistant = assistant;
		return assistant;
	}

	@Override
	public String[] getDefaultPrefixes(ISourceViewer sourceViewer,
			String contentType) {
		return new String[] { "%", "" };
	}

	public InformationPresenter getOutlinePresenter(ISourceViewer sourceViewer) {
		InformationPresenter presenter;
		presenter= new InformationPresenter(getOutlinePresenterControlCreator(sourceViewer));
		presenter.setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));
		presenter.setAnchor(AbstractInformationControlManager.ANCHOR_GLOBAL);
		IInformationProvider provider= new IInformationProvider() {
			
			@Override
			public IRegion getSubject(ITextViewer textViewer, int offset) {
				Document document = (Document) editor.getDocumentProvider().getDocument(
						editor.getEditorInput());


					TextSelection selection = (TextSelection) editor.getEditorSite()
							.getSelectionProvider().getSelection();
					IRegion info;
					try {
						info = document.getLineInformationOfOffset(selection
								.getOffset());
						return info;
					} catch (BadLocationException e) {
						e.printStackTrace();
					}
					return null;
			}
			
			@Override
			public String getInformation(ITextViewer textViewer, IRegion subject) {
				return UIUtils.getFileNameForEditorInput(editor.getEditorInput());
			}
		};
		presenter.setInformationProvider(provider, IDocument.DEFAULT_CONTENT_TYPE);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_COMMENT);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_DEFAULT);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_DOUBLE_QUOTED_STRING);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_MULTI_COMMENT);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_SINGLE_QUOTED_STRING);
		presenter.setSizeConstraints(50, 20, true, false);
		presenter.setEnabled(true);
		return presenter;
	}
	
	/**
	 * Returns the outline presenter control creator. The creator is a factory creating outline
	 * presenter controls for the given source viewer. This implementation always returns a creator
	 * for <code>JavaOutlineInformationControl</code> instances.
	 *
	 * @param sourceViewer the source viewer to be configured by this configuration
	 * @param commandId the ID of the command that opens this control
	 * @return an information control creator
	 * @since 2.1
	 */
	private IInformationControlCreator getOutlinePresenterControlCreator(ISourceViewer sourceViewer) {
		return new IInformationControlCreator() {
			@Override
			public IInformationControl createInformationControl(Shell parent) {
				int shellStyle= SWT.RESIZE;
				int treeStyle= SWT.V_SCROLL | SWT.H_SCROLL;
				return new PrologOutlineInformationControl(editor.getDocumentProvider().getDocument(editor.getEditorInput()), parent, shellStyle, treeStyle);
			}
		};
	}
	
	@Override
	public IQuickAssistAssistant getQuickAssistAssistant(ISourceViewer sourceViewer) {
		QuickAssistAssistant assist = new QuickAssistAssistant();
		assist.setQuickAssistProcessor(new PLQuickAssistProcessor());
		return assist;
	}

}


