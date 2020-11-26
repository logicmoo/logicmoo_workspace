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

package org.cs3.pdt.console.internal.views;

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.File;
import java.io.FileWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.common.PDTCommon;
import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.search.PrologSearchPage;
import org.cs3.pdt.connector.PDTConnector;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.PrologContextTracker;
import org.cs3.pdt.connector.PrologContextTrackerEvent;
import org.cs3.pdt.connector.registry.PrologProcessRegistry;
import org.cs3.pdt.connector.service.ActivePrologProcessListener;
import org.cs3.pdt.connector.subscription.DefaultSubscription;
import org.cs3.pdt.connector.subscription.Subscription;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.console.ConsoleModel;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PDTConsolePredicates;
import org.cs3.pdt.console.PrologConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.console.internal.DefaultPrologConsoleService;
import org.cs3.pdt.console.internal.ImageRepository;
import org.cs3.pdt.console.internal.loadfile.GenerateLoadFileWizard;
import org.cs3.pdt.console.internal.views.ConsoleViewer.SavedState;
import org.cs3.pdt.console.internal.views.completion.PrologCompletionProvider;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.process.PrologProcessStartException;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IKeyBindingService;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchCommandConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;

public class PrologConsoleView extends ViewPart implements LifeCycleHook, PrologConsole, ActivePrologProcessListener {

	private static final String KILLABLE = "killable";

	private final class ClearAction extends Action {
		private ClearAction(String text, String tooltip, ImageDescriptor image) {
			super(text, image);
			setToolTipText(tooltip);
		}

		@Override
		public void run() {
			getViewer().clearOutput();
		}
	}

	private abstract class PasteAction extends Action {
		public PasteAction(String text, String tooltip, ImageDescriptor icon) {
			super(text, icon);

			setToolTipText(tooltip);
		}

		protected abstract String getTextToInsert();

		@Override
		public void run() {
			try {

				UIJob j = new UIJob(getToolTipText()) {

					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						try {
							PrologConsole c = getConsole();
							int caretOffset = c.getCaretOffset();
							int offsetInLineBuffer = caretOffset
									- c.getStartOfInput();
							ConsoleModel model = c.getModel();
							String lineBuffer = model.getLineBuffer();
							if (offsetInLineBuffer < 0) {
								offsetInLineBuffer=lineBuffer.length();
								caretOffset=c.getStartOfInput()+lineBuffer.length();
							}


							String textToInsert = getTextToInsert();
							if (textToInsert == null) {
								return Status.OK_STATUS;
							}
							lineBuffer = lineBuffer.substring(0,
									offsetInLineBuffer)
									+ textToInsert
									+ lineBuffer.substring(offsetInLineBuffer);

							model.setLineBuffer(lineBuffer);
							c.setCaretOffset(caretOffset
									+ textToInsert.length());

						} catch (Throwable e) {
							Debug.report(e);
							return Status.CANCEL_STATUS;
						} finally {
							monitor.done();
						}
						return Status.OK_STATUS;
					}

					private PrologConsole getConsole() {
						return PrologConsoleView.this;
					}

				};

				j.schedule();
			} catch (Throwable t) {
				Debug.report(t);
			}
		}

	}

	private final class RestartAction extends Action implements IMenuCreator {
		
		private RestartAction() {
			setMenuCreator(this);
		}
		
		@Override
		public void run() {
			restart(null);
		}

		private void restart(final String reconsultStrategy) {
			try {
				Job j = new Job("Restarting the PrologProcess") {
					@Override
					public IStatus run(IProgressMonitor monitor) {
						try {
							monitor.beginTask("initializing...", 2);

							PrologProcess process = getPrologProcess();
							try {
								if (process != null) {
									process.stop();
									monitor.worked(1);
								}
								// setPrologProcess(getEditorPrologProcess());
							} finally {
								if (process != null) {
									if (!process.isDown()){
										process.reset();
										Thread.sleep(1000);
									}
									if (reconsultStrategy != null) {
										process.setAttribute(PDTCommon.PROCESS_SPECIFIC_RECONSULT_STRATEGY, reconsultStrategy);
									}
									process.start();
									Display.getDefault().asyncExec(new Runnable() {
										@Override
										public void run() {
											getDefaultPrologConsoleService().fireConsoleVisibilityChanged(PrologConsoleView.this);
										}
									});
									writeCurrentProcessPortToFile();
								}
							}
						} catch (Throwable e) {
							Debug.report(e);
							return Status.CANCEL_STATUS;
						} finally {
							monitor.done();
						}
						return Status.OK_STATUS;
					}

				};
				j.schedule();
			} catch (Throwable t) {
				Debug.report(t);
			}
		}

		@Override
		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(ImageRepository.RESTART);
		}

		@Override
		public String getToolTipText() {
			return "Restart process";
		}

		@Override
		public String getText() {
			return "Restart";
		}

		private Menu menu;
		
		@Override
		public void dispose() {
			if (menu != null) {
				menu.dispose();
			}
		}

		@Override
		public Menu getMenu(Control parent) {
			if (menu != null) {
				menu.dispose();
			}
			Menu newMenu = new Menu(parent);
			String reconsultStrategyPreference = PDTCommonPlugin.getDefault().getPreferenceStore().getString(PDTCommon.PREF_RECONSULT_ON_RESTART);
			addMenuItem(newMenu, "Restart clean", PDTCommon.RECONSULT_NONE, reconsultStrategyPreference);
			addMenuItem(newMenu, "Restart and reload entry point files", PDTCommon.RECONSULT_ENTRY, reconsultStrategyPreference);
			addMenuItem(newMenu, "Restart and reload all files", PDTCommon.RECONSULT_ALL, reconsultStrategyPreference);
			menu = newMenu;
			return menu;
		}
		
		private void addMenuItem(Menu newMenu, String text, final String reconsultStrategy, String reconsultStrategyPreference) {
			MenuItem item = new MenuItem(newMenu, SWT.NONE);
			item.setText(text);
			item.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					restart(reconsultStrategy);
				}
			});
			if (reconsultStrategyPreference.equals(reconsultStrategy)) {
				item.setImage(ImageRepository.getImage(ImageRepository.RESTART));
			}
		}

		@Override
		public Menu getMenu(Menu parent) {
			return null;
		}
	}

	private final class KillAction extends Action {

		@Override
		public void run() {
			
			boolean answer = MessageDialog.openQuestion(PrologConsoleView.this.getViewSite().getShell(), "Kill process", "Are you sure you want to kill the process? This will remove all breakpoints and will delete the list of consulted files");
			
			if (answer) {
				try {

					Job j = new UIJob("Stopping the PrologProcess") {

						@Override
						public IStatus runInUIThread(IProgressMonitor monitor) {
							try {
								monitor.beginTask("initializing...",
										IProgressMonitor.UNKNOWN);

								PrologProcessRegistry registry = PDTConnectorPlugin.getDefault().getPrologProcessRegistry();


								PrologProcess oldProcess = getPrologProcess();
								if (oldProcess != null) {
									String currentKey = registry.getKey(oldProcess);

									@SuppressWarnings("unchecked")
									List<String> consultedFiles = (List<String>) getPrologProcess().getAttribute(PDTCommon.CONSULTED_FILES);
									if (consultedFiles != null) {
										consultedFiles.clear();
									}
									oldProcess.stop();

									if ("true".equals(oldProcess.getAttribute(KILLABLE))) {
										Set<Subscription> subscriptionsForProcess = registry.getSubscriptionsForProcess(currentKey);
										for (Subscription s : subscriptionsForProcess) {
											registry.removeSubscription(s);
										}
										registry.removePrologProcess(currentKey);
										getDefaultPrologConsoleService().fireConsoleVisibilityChanged(PrologConsoleView.this);
									}

								}
							} catch (Throwable e) {
								Debug.report(e);
								return Status.CANCEL_STATUS;
							} finally {
								monitor.done();
							}
							return Status.OK_STATUS;
						}

					};
					j.schedule();
				} catch (Throwable t) {
					Debug.report(t);
				}
			}
		}



		@Override
		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(ImageRepository.STOP);
		}

		@Override
		public String getToolTipText() {
			return "Kill process";
		}

		@Override
		public String getText() {
			return "kill";
		}
	}


	private final class GenLoadFileAction extends Action {
		@Override
		public void run() {
			try {

				Job j = new UIJob("Generating load file") {

					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						try {
							monitor.beginTask("initializing...",
									IProgressMonitor.UNKNOWN);

							if (getPrologProcess() != null) {
								@SuppressWarnings("unchecked")
								List<String> consultedFiles = (List<String>) getPrologProcess().getAttribute(PDTCommon.CONSULTED_FILES);
								
								// only create load file if there are consulted files
								if (consultedFiles != null && consultedFiles.size() > 0) {
									WizardDialog dialog = new WizardDialog(getViewSite().getShell(), new GenerateLoadFileWizard(consultedFiles));
									dialog.open();
								} else {
									MessageDialog.openWarning(PrologConsoleView.this.getViewSite().getShell(), "Generate Load File", "No need for a load file, since no files are consulted.");
								}
								
							}

						} catch (Throwable e) {
							Debug.report(e);
							return Status.CANCEL_STATUS;
						} finally {
							monitor.done();
						}
						return Status.OK_STATUS;
					}

				};
				j.schedule();
			} catch (Throwable t) {
				Debug.report(t);
			}

		}


		@Override
		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(ImageRepository.GEN_LOAD_FILE);
		}

		@Override
		public String getToolTipText() {
			return "Generate load file from consulted files";
		}

		@Override
		public String getText() {
			return "generateLoadFile";
		}
	}

	private final class CreateNamedProcessAction extends Action implements IMenuCreator {

		private Menu menu;

		private CreateNamedProcessAction() {
			setMenuCreator(this);
		}
		
		@Override
		public void run(){
			askForNameAndCreateProcess(PDTConnectorPlugin.getDefault().getPreferenceStore().getString(PDTConnector.PREF_CONFIGURATION));
		}

		private void askForNameAndCreateProcess(final String configuration) {
			Job j = new UIJob("Creating new Prolog Process")
			{
				@Override
				public IStatus runInUIThread(IProgressMonitor arg0) {
					PrologProcessRegistry registry = PDTConnectorPlugin.getDefault().getPrologProcessRegistry();

					InputDialog dialog = createNewProcessNameDialog(registry);	
					int result = dialog.open();
					if (result == InputDialog.CANCEL)
						return Status.CANCEL_STATUS;
					String processKey = dialog.getValue();

					PrologProcess process = activateNewPrologProcess(registry, processKey, configuration);
					process.setAttribute(KILLABLE, "true");
					return Status.OK_STATUS;
				}

				public InputDialog createNewProcessNameDialog(
						PrologProcessRegistry registry) {
					final Set<String> processKeys = registry.getRegisteredKeys();
					String defaultProcessKey = getNameOfProjectOfActiveEditorInput();
					if (processKeys.contains(defaultProcessKey))
						defaultProcessKey = null;

					IInputValidator validator = new IInputValidator(){
						@Override
						public String isValid(String arg0) {
							if ("".equals(arg0))
								return "Process name must not be empty";
							else if (processKeys.contains(arg0))
								return "Process name already used";
							else
								return null;
						}
					};
					InputDialog dialog = new InputDialog(PrologConsoleView.this.getViewSite().getShell(), "Create Prolog Process (" + configuration + ")", "Enter a new name for your new Prolog process:", defaultProcessKey, validator);
					return dialog;
				}

			};
			j.schedule();
		}

		private String getNameOfProjectOfActiveEditorInput(){
			IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			if (window != null) {
				IWorkbenchPage page = window.getActivePage();
				if (page != null) {
					IEditorPart editor = page.getActiveEditor();
					if (editor == null)
						return null;

					IEditorInput editorInput = editor.getEditorInput();
					if (editorInput == null)
						return null;

					IFile file = (IFile) editorInput.getAdapter(IFile.class);
					if (file != null) {
						return file.getProject().getName();
					}
				}
			}
			return null;
		}

		@Override
		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(ImageRepository.NEW_PROCESS);
		}

		@Override
		public String getToolTipText() {
			return "Create process";
		}

		@Override
		public String getText() {
			return "Create process";
		}

		@Override
		public void dispose() {
			if (menu != null) {
				menu.dispose();
			}
		}

		@Override
		public Menu getMenu(Control parent) {
			if (menu != null) {
				menu.dispose();
			}
			Menu newMenu = new Menu(parent);
			List<String> preferenceConfigurations = PDTConnectorPlugin.getDefault().getPreferenceConfigurations();
			String defaultConfiguration = PDTConnectorPlugin.getDefault().getPreferenceStore().getString(PDTConnector.PREF_CONFIGURATION);
			for (String preferenceConfiguration : preferenceConfigurations) {
				MenuItem item = new MenuItem(newMenu, SWT.NONE);
				item.setText(preferenceConfiguration.replaceAll("&", "&&"));
				if (preferenceConfiguration.equals(defaultConfiguration)) {
					item.setImage(ImageRepository.getImage(ImageRepository.NEW_PROCESS));
				}
				final String configuration = preferenceConfiguration;
				item.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						askForNameAndCreateProcess(configuration);
					}
				});
			}
			menu = newMenu;
			return menu;
		}

		@Override
		public Menu getMenu(Menu parent) {
			return null;
		}
	}

	private class ConsoleQueryAction extends Action {

		private String query;

		public ConsoleQueryAction(String text, ImageDescriptor icon, String query){
			super(text, icon);
			this.query = query.endsWith(".") ? query : query + ".";
			setToolTipText(text);
		}

		protected String getQuery(){
			return query;
		}

		@Override
		public void run(){
			Job j = new Job(getToolTipText()) {
				@Override
				protected IStatus run(IProgressMonitor monitor) {
					try {
						PrologConsole c = getConsole();
						ConsoleModel model = c.getModel();
						model.setLineBuffer(" ");
						model.commitLineBuffer();
						model.setLineBuffer(getQuery());
						model.commitLineBuffer();
					} catch (Throwable e) {
						Debug.report(e);
						return Status.CANCEL_STATUS;
					} finally {
						monitor.done();
					}
					return Status.OK_STATUS;
				}

				private PrologConsole getConsole() {
					return PrologConsoleView.this;
				}

			};
			j.schedule();
		}
	}

	private class ProcessQueryAction extends Action {

		private String query;

		public ProcessQueryAction(String text, ImageDescriptor icon, String query){
			super(text, icon);
			this.query = query.endsWith(".") ? query : query + ".";
			setToolTipText(text);
		}

		protected String getQuery(){
			return query;
		}

		@Override
		public void run(){
			try {
				getPrologProcess().queryOnce(getQuery());
			} catch (PrologProcessException e) {
				Debug.report(e);
			}
		}
	}

	public static final String HOOK_ID = PDTConsole.CONSOLE_VIEW_ID;
	private ConsoleViewer viewer;
	private Composite partControl;
	private PrologProcess currentProcess;
	private Menu contextMenu;
	private Action cutAction;
	private Action copyAction;
	private Action pasteAction;
	private Action selectAllAction;
	private ClearAction clearAction;
	private Action searchAction;
	//	private GuiTracerAction guiTracerAction;
	private PasteAction pasteFileNameAction;
	private RestartAction restartAction;
	private KillAction killAction;
	private GenLoadFileAction genLoadFileAction;
	private CreateNamedProcessAction createProcessAction;
	private HashMap<PrologProcess, PrologSocketConsoleModel> models = new HashMap<PrologProcess, PrologSocketConsoleModel>();
	private Label title;
	private HashMap<PrologProcess, SavedState> viewerStates = new HashMap<PrologProcess, SavedState>();

	private SelectContextProcessAutomatedAction automatedSelector;
	private ConsoleQueryAction activateGuiTracerAction;
	private ConsoleQueryAction deactivateGuiTracerAction;
	private ConsoleQueryAction threadMonitorAction;
	private ConsoleQueryAction debugMonitorAction;

	private ProcessQueryAction abortAction;
	private ProcessQueryAction traceAction;
	private Action helpAction;
	private Action configAction;

	@Override
	public void createPartControl(Composite parent) {

		try {
			createPartControl_impl(parent);
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t.getLocalizedMessage(), t);
		}
	}

	private void createPartControl_impl(Composite parent) {

		this.partControl = parent;

		Listener handler = new Listener() {

			@Override
			public void handleEvent(Event event) {
				switch (event.type) {
				case SWT.Show:
				case SWT.Hide:
					getDefaultPrologConsoleService().fireConsoleVisibilityChanged(PrologConsoleView.this);
					break;
				case SWT.FocusOut:
					getDefaultPrologConsoleService().fireConsoleLostFocus(PrologConsoleView.this);
				}

			}


		};
		parent.getParent().addListener(SWT.Show, handler);
		parent.getParent().addListener(SWT.Hide, handler);
		parent.getParent().addListener(SWT.FocusOut, handler);
		PrologConsolePlugin.getDefault().getPrologConsoleService().registerPrologConsole(this);
		getDefaultPrologConsoleService().fireConsoleVisibilityChanged(PrologConsoleView.this);
		PDTConnectorPlugin.getDefault().getPrologProcessService().registerActivePrologProcessListener(this);
		GridLayout layout = new GridLayout(1, true);
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		parent.setLayout(layout);
		GridData ld = new GridData(GridData.FILL_HORIZONTAL);
		title = new Label(parent, SWT.HORIZONTAL);
		title.setLayoutData(ld);
		viewer = new ConsoleViewer(parent, SWT.BORDER | SWT.MULTI | SWT.WRAP
				| SWT.V_SCROLL);
		viewer.getControl().setEnabled(false);
		ld = new GridData(GridData.FILL_BOTH);
		viewer.getControl().setLayoutData(ld);
		createActions();
		initMenus(parent);
		initToolBars();
		getSite().setSelectionProvider(viewer);

	}

	private DefaultPrologConsoleService getDefaultPrologConsoleService() {
		return ((DefaultPrologConsoleService) PrologConsolePlugin.getDefault().getPrologConsoleService());
	}

	private void createActions() {
		ISharedImages sharedImages = getSite().getWorkbenchWindow().getWorkbench().getSharedImages();
		cutAction = new Action() {
			@Override
			public void run() {
				viewer.cut();
			}
//			@Override
//			public boolean isEnabled() {
//				return ((StyledText)viewer.getControl()).getSelectionRange().y > 0;
//			}
		};
		cutAction.setText("Cut");
		cutAction.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_CUT);
		cutAction.setImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_CUT));
		cutAction.setDisabledImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_CUT_DISABLED));

		copyAction = new Action() {
			@Override
			public void run() {
				viewer.copy();
			}
//			@Override
//			public boolean isEnabled() {
//				return ((StyledText)viewer.getControl()).getSelectionRange().y > 0;
//			}
		};
		copyAction.setText("Copy");
		copyAction.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_COPY);
		copyAction.setImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_COPY));
		copyAction.setDisabledImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_COPY_DISABLED));

		pasteAction = new Action() {
			@Override
			public void run() {
				viewer.paste();
			}
		};
		pasteAction.setText("Paste");
		pasteAction.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_PASTE);
		pasteAction.setImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_PASTE));
		
		selectAllAction = new Action() {
			@Override
			public void run() {
				viewer.selectAll();
			}
		};
		selectAllAction.setText("Select All");
		selectAllAction.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_SELECT_ALL);
		
		clearAction = new ClearAction("Clear", "Clear console output",
				ImageRepository.getImageDescriptor(ImageRepository.CLEAR));
		searchAction = new Action("Open Search Dialog") {
			@Override
			public void run() {
				NewSearchUI.openSearchDialog(getSite().getWorkbenchWindow(), PrologSearchPage.EXTENSION_POINT_ID);
			}
		};
		//		guiTracerAction = new GuiTracerAction(new String[] {"guitracer", "noguitracer"},
		//				new String[] {"activate guitracer", "deactivate guitracer"},  
		//				new String[] {"activate GUI tracer", "deactivate GUI tracer"}, 
		//				new ImageDescriptor[] {
		//				ImageRepository.getImageDescriptor(ImageRepository.GUITRACER),
		//				ImageRepository.getImageDescriptor(ImageRepository.NOGUITRACER)});
		activateGuiTracerAction = new ConsoleQueryAction("Activate GUI tracer", ImageRepository.getImageDescriptor(ImageRepository.GUITRACER), "guitracer");
		deactivateGuiTracerAction = new ConsoleQueryAction("Deactivate GUI tracer", ImageRepository.getImageDescriptor(ImageRepository.NOGUITRACER), "noguitracer");
		threadMonitorAction = new ConsoleQueryAction("Show SWI thread monitor", ImageRepository.getImageDescriptor(ImageRepository.THREAD_MONITOR), "user:prolog_ide(thread_monitor)");
		debugMonitorAction = new ConsoleQueryAction("Show SWI debug message monitor", ImageRepository.getImageDescriptor(ImageRepository.DEBUG_MONITOR), "user:prolog_ide(debug_monitor)");
		abortAction = new ProcessQueryAction("Abort running query", ImageRepository.getImageDescriptor(ImageRepository.ABORT), bT(PDTConsolePredicates.CONSOLE_THREAD_NAME, "ID") + ", catch(thread_signal(ID, abort),_,fail)") {
			@Override
			public void run(){
				super.run();
				if (!getModel().isConnected()) {
					new Thread(new Runnable(){
						@Override
						public void run() {
							try {
								Thread.sleep(500);
							} catch (InterruptedException e) {
							}
							try {
								connect(currentProcess);
							} catch (PrologProcessException e) {
							}
						}
					}).start();
				}
			}
		}; 		
		traceAction = new ProcessQueryAction(
				"Interrupt running query and start tracing",
				ImageRepository.getImageDescriptor(ImageRepository.TRACE),
				bT(PDTConsolePredicates.CONSOLE_THREAD_NAME, "ID") + ", catch(thread_signal(ID, trace),_,fail)");
		
		pasteFileNameAction = new PasteAction("Paste filename",
				"Paste the name of the current editor file", ImageRepository
				.getImageDescriptor(ImageRepository.PASTE_FILENAME)) {

			@Override
			protected String getTextToInsert() {
				String fileName = UIUtils.getFileFromActiveEditor();
				if (fileName == null) {
					return null;
				}
				return QueryUtils.prologFileNameQuoted(new File(fileName));
			}

		};
		pasteFileNameAction.setActionDefinitionId(PDTConsole.COMMAND_PASTE_FILENAME);

		IKeyBindingService keyBindingService = getSite().getKeyBindingService();
		keyBindingService.setScopes(new String[] { PDTConsole.CONTEXT_USING_CONSOLE_VIEW });
		keyBindingService.registerAction(pasteFileNameAction);
		restartAction = new RestartAction();
		killAction = new KillAction();
		genLoadFileAction = new GenLoadFileAction();
		createProcessAction = new CreateNamedProcessAction();
		helpAction = new Action("SWI-Prolog Documentation", ImageRepository.getImageDescriptor(ImageRepository.HELP)) {
			@Override
			public void run() {
				try {
					PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser().openURL(new URL("http://www.swi-prolog.org/pldoc/index.html"));
				} catch (Exception e) {}
			}

		};
		
		configAction = new Action("Console preferences", ImageRepository.getImageDescriptor(ImageRepository.PREFERENCES)) {
			@Override
			public void run() {
				PreferenceDialog dialog = PreferencesUtil.createPreferenceDialogOn(getSite().getShell(), PDTConsole.PREFERNCE_PAGE_ID, null, null);
				if (dialog != null) {
					dialog.open();
				}
			}
		};
	}

	private void initMenus(Control parent) {

		MenuManager manager = new MenuManager();
		manager.setRemoveAllWhenShown(true);
		manager.addMenuListener(new IMenuListener() {

			@Override
			public void menuAboutToShow(IMenuManager manager) {
				manager.add(new Separator("#Clipboard"));
				manager.add(selectAllAction);
				manager.add(cutAction);
				manager.add(copyAction);
				manager.add(pasteAction);
				manager.add(pasteFileNameAction);
				manager.add(clearAction);
				manager.add(new Separator("#Clipboard-end"));
				manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
				manager.add(searchAction);
				manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS + "-end"));
			}

		});
		getSite().registerContextMenu(manager, viewer);
		contextMenu = manager.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(contextMenu);
	}

	private void initToolBars() {
		IActionBars bars = this.getViewSite().getActionBars();

		bars.setGlobalActionHandler(ActionFactory.SELECT_ALL.getId(),
				selectAllAction);
		bars.setGlobalActionHandler(ActionFactory.CUT.getId(), cutAction);
		bars.setGlobalActionHandler(ActionFactory.COPY.getId(), copyAction);
		bars.setGlobalActionHandler(ActionFactory.PASTE.getId(), pasteAction);

		IToolBarManager toolBarManager = bars.getToolBarManager();

		addToolbarContributions(toolBarManager);
		addMenuContributions(bars.getMenuManager());

		//		processSelector.init(getViewSite().getWorkbenchWindow());
		automatedSelector.init(getViewSite().getWorkbenchWindow());

	}

	private void createAutomatedSelector(IToolBarManager toolBarManager) {

		automatedSelector = new SelectContextProcessAutomatedAction(){


			@Override
			protected PrologProcess getPrologProcess() {
				return PrologConsoleView.this.getPrologProcess();
			}


			@Override
			protected void setPrologProcess(PrologProcess prologProcess) {
				PrologConsoleView.this.setPrologProcess(prologProcess);

			}


			@Override
			protected void trackerActivated(PrologContextTracker tracker) {
				setPrologProcess(automatedSelector.getCurrentPrologProcess());

			}


			@Override
			protected void trackerDeactivated(PrologContextTracker tracker) {
				setPrologProcess(automatedSelector.getCurrentPrologProcess());

			}


			@Override
			public void contextChanged(PrologContextTrackerEvent e) {
				PrologContextTracker tracker = (PrologContextTracker) e
						.getSource();
				Debug.info("context changed for tracker " + tracker.getLabel());
				setPrologProcess(e.getPrologProcess());

			}

		};
		toolBarManager.add(automatedSelector);

		//		processSelector = new SelectProcessAction() {
		//
		//			protected void setPrologProcess(PrologProcess prologProcess) {
		//				PrologConsoleView.this.setPrologProcess(prologProcess);
		//
		//			}
		//
		//			protected PrologProcess getPrologProcess() {
		//				return PrologConsoleView.this.getPrologProcess();
		//			}
		//
		//		};
		//		toolBarManager.add(processSelector);

		//		contextSelector = new SelectContextsAction() {
		//
		//			public void contextChanged(PrologContextTrackerEvent e) {
		//				PrologContextTracker tracker = (PrologContextTracker) e
		//						.getSource();
		//				Debug.info("context changed for tracker " + tracker.getLabel());
		//				setPrologProcess(e.getPrologProcess());
		//
		//			}
		//
		//			protected void trackerActivated(PrologContextTracker tracker) {
		//				setPrologProcess(contextSelector.getCurrentPrologProcess());
		//
		//			}
		//
		//			protected void trackerDeactivated(PrologContextTracker tracker) {
		//				setPrologProcess(contextSelector.getCurrentPrologProcess());
		//
		//			}
		//		};

		//		toolBarManager.add(contextSelector);
		//		setPrologProcess(contextSelector.getCurrentPrologProcess());
		Job j = new Job("Initialize Prolog Console") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				PrologConsoleView.this.setPrologProcess(PDTCommonUtil.getActivePrologProcess());
				return Status.OK_STATUS;
			}
		};
		j.setRule(ResourcesPlugin.getWorkspace().getRoot());
		j.schedule();
		automatedSelector.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.MANUAL_MODE));
	}

	public PrologProcess activateNewPrologProcess(PrologProcessRegistry registry, String processKey, String configuration) {
		DefaultSubscription subscription = new DefaultSubscription(processKey + "_indepent", processKey, "Independent prolog process", "Prolog");
		registry.addSubscription(subscription);
		PrologProcess process = PDTConnectorPlugin.getDefault().getPrologProcess(subscription, configuration);

		if (automatedSelector.getActiveTrackers().isEmpty()){
			setPrologProcess(process);
			automatedSelector.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.MANUAL_MODE));
		}
		return process;
	}

	private void addToolbarContributions(IToolBarManager manager) {
		manager.add(new Separator("#Console"));
		createAutomatedSelector(manager);
		manager.add(createProcessAction);
		manager.add(restartAction);
		manager.add(killAction);
		manager.add(clearAction);
		manager.add(new Separator("#Console-end"));
		manager.add(new Separator("#Query"));
		manager.add(traceAction);
		manager.add(abortAction);
		manager.add(new Separator("#Query-end"));
		manager.add(new Separator("#Toolbar-Other"));
		manager.add(genLoadFileAction);
		manager.add(configAction);
		manager.add(helpAction);
		manager.add(new Separator("#Toolbar-End"));
	}

	private void addMenuContributions(IMenuManager manager) {
		manager.add(activateGuiTracerAction);
		manager.add(deactivateGuiTracerAction);
		manager.add(threadMonitorAction);
		manager.add(debugMonitorAction);
	}

	@Override
	public void setFocus() {
		if (viewer == null) {
			Debug
			.warning("PrologConsoleView.setFocus(): View not instantiated yet.");
			return;
		}
		viewer.getControl().setFocus();
		getDefaultPrologConsoleService().fireConsoleRecievedFocus(this);
	}


	@Override
	public void dispose() {
		PrologConsolePlugin.getDefault().getPrologConsoleService().unregisterPrologConsole(this);
		PDTConnectorPlugin.getDefault().getPrologProcessService().unRegisterActivePrologProcessListener(this);
		for (Iterator<PrologProcess> it = models.keySet().iterator(); it.hasNext();) {
			PrologProcess process = it.next();
			try {
				disconnect(process);
				removeHooks(process);
			} catch (Throwable e) {
				Debug.report(e);
			}
		}
		models.clear();
		contextMenu.dispose();
		// viewer.getControl().dispose();
		super.dispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
	 */
	@Override
	public void onInit(PrologProcess process, PrologSession initSession) {
		;
	}

	private void startServer(PrologProcess process, PrologSession session) {
		try {
			String queryString = bT(PDTConsolePredicates.PDT_START_CONSOLE_SERVER,
					"Port",
					QueryUtils.quoteAtom(PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getKey(process)));
			Debug.info("starting console server using: " + queryString);

			Map<String,?> result = session.queryOnce(queryString);
			if (result == null) {
				Debug.info("starting server failed, which may mean that it is actualy running already.");
				result = session.queryOnce(bT(PDTConsolePredicates.PDT_CURRENT_CONSOLE_SERVER, "Port"));
				if(result==null){
					throw new RuntimeException("Failed to connect the Prolog Console to the Prolog process.");
				}
			}

			int port = Integer.parseInt((String) result.get("Port"));
			Debug.debug("A server thread seems to be listinging at port "+port);
		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	@Override
	public void afterInit(PrologProcess process) {
		try {
			connect(process);
		} catch (PrologProcessException e) {
			Debug.report(e);
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	 */
	@Override
	public void beforeShutdown(PrologProcess process, PrologSession session) {
		disconnect(process);
	}

	@Override
	public void onError(PrologProcess process) {
		disconnect(process);
	}

	@Override
	public ConsoleModel getModel() {
		return (getPrologProcess() == null ? null : models
				.get(getPrologProcess()));
	}

	@Override
	public PrologProcess getPrologProcess() {
		return currentProcess;
	}

	@Override
	public void setPrologProcess(PrologProcess newProcess) {
		setPrologProcess(newProcess, true);
	}
	
	private void setPrologProcess(PrologProcess newProcess, boolean updateActiveProcess) {
		if(currentProcess==newProcess){
			return;
		}
		if (currentProcess != null) {
			viewerStates.put(currentProcess, viewer.saveState());
		}
		currentProcess = newProcess;
		if (currentProcess != null) {
			addHooks(currentProcess);
			String errorMessage = null;
			try {
				connect(currentProcess);
			} catch (PrologProcessException e) {
				Debug.report(e);
				String message = e.getMessage();
				StringBuilder buf = new StringBuilder('\n');
				for (String line : message.split("\n")) {
					buf.append("! ");
					buf.append(line);
					buf.append('\n');
				}
				buf.append('\n');
				errorMessage = buf.toString();
			}
			reconfigureViewer(currentProcess);
			if (errorMessage != null) {
				viewer.appendOutput(errorMessage);
			}
			getDefaultPrologConsoleService().fireActivePrologProcessChanged(this);
			if (updateActiveProcess) {
				PDTConnectorPlugin.getDefault().getPrologProcessService().setActivePrologProcess(currentProcess);
			}

		} else {
			Debug.debug("no process (yet).");
		}
		if(automatedSelector != null){
			automatedSelector.update();
		}
		writeCurrentProcessPortToFile();
	}

	public void writeCurrentProcessPortToFile() {
		try {
			int port = (Integer)currentProcess.getClass().getMethod("getPort").invoke(currentProcess);
			File portFile = new File(System.getProperty("java.io.tmpdir")+File.separator + "pdt_console_active_port.txt");
			FileWriter writer = new FileWriter(portFile,false);
			writer.write(""+port+"\n");
			writer.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	@Override
	public void ensureConnectionForCurrentPrologProcess() {
		try {
			connect(currentProcess);
		} catch (PrologProcessException e) {
			Debug.report(e);
		}
	}


	/*
	 * note: implementation should take into account, that this method might be
	 * called several times for the same process, even during one single life cycle.
	 * 
	 * attach means: ensure a model exsists for this process. ensure the model is
	 * connected. console only attach to a process that is in UP state.
	 * 
	 */
	synchronized private void connect(final PrologProcess process)
			throws PrologProcessException {

		PrologSocketConsoleModel model = getConsoleModel(process);
		ensureConnection(process, model);
	}

	private PrologSocketConsoleModel getConsoleModel(final PrologProcess process) {
		PrologSocketConsoleModel model = models.get(process);
		if (model == null) {
			model = new PrologSocketConsoleModel(false);
			models.put(process, model);
		}
		return model;
	}

	private void ensureConnection(final PrologProcess process,
			PrologSocketConsoleModel model) throws PrologProcessException {
		if (model.isConnected()) {
			return;
		}

		PrologSession session = process.getSession(PrologProcess.NONE);

		Map<String,?> result = null;
		try {
//			result = session.queryOnce( "consult(lib_pdt_console_pl(loader)).");
			result = session.queryOnce(bT(PDTConsolePredicates.PDT_START_CONSOLE_SERVER,
					"Port",
					QueryUtils.quoteAtom(PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getKey(process))));
			if (result == null) {
				startServer(process, session);
				result = session.queryOnce(bT(PDTConsolePredicates.PDT_CURRENT_CONSOLE_SERVER, "Port"));
			}
			if (result == null) {
				throw new RuntimeException("Failed to connect the Prolog Console to the Prolog process");
			}
		} 
		catch (Exception e) {
			Debug.info(e.toString());
		}
		finally {
			if (session != null) {
				session.dispose();
			}
		}

		int port = Integer.parseInt(result.get("Port").toString());
		model.setPort(port);
		model.connect();
	}

	private void disconnect(PrologProcess process) {
		PrologSocketConsoleModel model = models
				.get(process);
		if (model == null) {
			return;
		}

		model.disconnect();

	}

	private void addHooks(PrologProcess process) {
		process.addLifeCycleHook(this, HOOK_ID, new String[0]);

	}

	private void removeHooks(PrologProcess process) {

		process.removeLifeCycleHook(HOOK_ID);

	}

	private void reconfigureViewer(final PrologProcess process) {

		if (Display.getCurrent() != viewer.getControl().getDisplay()) {
			viewer.getControl().getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					reconfigureViewer(process);
				}
			});
			return;
		}
		if (process == null ) {

			viewer.setModel(null);
			viewer.setHistory(null);
			viewer.setCompletionProvider(null);
			title.setText("no console available(yet).");
			return;
		}

		ConsoleViewer.SavedState savedState = viewerStates
				.get(process);
		if (savedState == null) {
			viewer.clearOutput();
			ConsoleHistory history = new ConsoleHistory(PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getKey(process));
			viewer.setHistory(history);
			viewer.setModel(models.get(process));
			PrologCompletionProvider completionProvider = new PrologCompletionProvider();
			completionProvider.setPrologProcess(process);
			viewer.setCompletionProvider(completionProvider);
		} else {
			viewer.loadState(savedState);
		}
		PrologProcessRegistry reg = PDTConnectorPlugin.getDefault().getPrologProcessRegistry();
		String key = reg.getKey(process);
		title.setText(SelectContextProcessAutomatedAction.getLabelForProcess(key, reg));
//		Object configuration = process.getAttribute(PrologRuntimeUI.CONFIGURATION_ATTRIBUTE);
//		if (configuration == null) {
//			title.setText(key);
//		} else {
//			title.setText(key + " (" + configuration.toString().replaceAll("&", "&&") + ")");
//		}

		viewer.setEnterSendsSemicolon(false);

	}


	@Override
	public boolean isVisible() {
		return partControl.getVisible();
	}

	public ConsoleViewer getViewer() {
		return viewer;
	}

	@Override
	public String getText() {
		return getViewer().getText();
	}

	@Override
	public int getLineAtOffset(int offset) {
		return getViewer().getLineAtOffset(offset);
	}

	@Override
	public int getOffsetAtLine(int line) {
		return getViewer().getOffsetAtLine(line);
	}

	@Override
	public int getLineCount() {
		return getViewer().getLineCount();
	}

	@Override
	public void clearOutput() {
		getViewer().clearOutput();

	}

	@Override
	public String getTextRange(int offset, int length) {
		return getViewer().getTextRange(offset, length);
	}

	@Override
	public int getCaretOffset() {
		return getViewer().getCaretOffset();
	}

	@Override
	public int getStartOfInput() {

		return getViewer().getStartOfInput();
	}

	@Override
	public void setCaretOffset(int offset) {
		getViewer().setCaretOffset(offset);

	}

	@Override
	public void setData(Object data) {
		;
	}

	@Override
	public void lateInit(PrologProcess process) {
		;
	}

	@Override
	public void activePrologProcessChanged(final PrologProcess process) {
		Display display = getSite().getShell().getDisplay();
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				@Override
				public void run() {
					activePrologProcessChanged(process);
				}
			});
		} else {
			setPrologProcess(process, false);
		}
	}

}


