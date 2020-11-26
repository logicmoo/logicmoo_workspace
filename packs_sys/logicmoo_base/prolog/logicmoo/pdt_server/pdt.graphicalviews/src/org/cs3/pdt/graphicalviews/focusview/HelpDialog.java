package org.cs3.pdt.graphicalviews.focusview;

import org.cs3.pdt.graphicalviews.internal.ImageRepository;
import org.eclipse.jface.window.ApplicationWindow;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

public class HelpDialog extends ApplicationWindow {
	
	public HelpDialog(Shell parentShell) {
		super(parentShell);
	}

	@Override
	protected void configureShell(Shell shell) {
		super.configureShell(shell);

		// Set the title bar text
		shell.setText("PDT Graphical Views Help");
	}
	
	@Override
	protected Control createContents(Composite parent) {

		Composite masterComposite = new Composite(parent, SWT.NONE);
		masterComposite.setLayout(new GridLayout(1,true));
		
//		Composite masterComposite = new Composite(compi , SWT.NONE);
//		masterComposite.setLayout(new GridLayout(1, false));
		/*
		 * Icon Legend
		 */
		
		Group predicateGroup = createGroup(masterComposite, "Predicates", 5);
		addLegendIcon(predicateGroup, "pred_normal_exported.png");
		addLegendIcon(predicateGroup, "pred_meta_exported.png");
		addLegendIcon(predicateGroup, "pred_dynamic_exported.png");
		addLegendIcon(predicateGroup, "pred_modtrans_exported.png");
		addLegendLabel(predicateGroup, "Exported predicates");
		addLegendIcon(predicateGroup, "pred_normal_local.png");
		addLegendIcon(predicateGroup, "pred_meta_local.png");
		addLegendIcon(predicateGroup, "pred_dynamic_local.png");
		addLegendIcon(predicateGroup, "pred_modtrans_local.png");
		addLegendLabel(predicateGroup, "Local predicates\n(not exported)");
		addLegendIcon(predicateGroup, "pred_normal_dead.png");
		addLegendIcon(predicateGroup, "pred_meta_dead.png");
		addLegendIcon(predicateGroup, "pred_dynamic_dead.png");
		addLegendIcon(predicateGroup, "pred_modtrans_dead.png");
		addLegendLabel(predicateGroup, "Dead predicates\n(local and uncalled)");
		addLegendLabel(predicateGroup, "Normal predicate");
		addLegendLabel(predicateGroup, "Meta predicate");
		addLegendLabel(predicateGroup, "Dynamic predicate");
		addLegendLabel(predicateGroup, "Module-transparent\npredicate");
		addLegendLabel(predicateGroup, "");
		
		Group callGroup = createGroup(masterComposite, "Calls", 4);
		addLegendIcon(callGroup, "call_normal.png");
		addLegendIcon(callGroup, "call_meta.png");
		addLegendIcon(callGroup, "call_dynamic.png");
		addLegendLabel(callGroup, "");
		addLegendLabel(callGroup, "Normal call");
		addLegendLabel(callGroup, "Metacall");
		addLegendLabel(callGroup, "Database call\n(assert/retract)");
		addLegendLabel(callGroup, "");

		Button close = new Button(masterComposite, SWT.PUSH);
		close.setText("Close");
		close.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				close();
			}
		});

		return masterComposite;
	}
	
	private Group createGroup(Composite masterComposite, String title, int cols) {
		Group group = new Group(masterComposite,SWT.NONE);
		GridLayout layout = new GridLayout(cols, true);
		GridData data = new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL | GridData.VERTICAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_CENTER | GridData.HORIZONTAL_ALIGN_FILL);
		group.setLayout(layout);
		group.setLayoutData(data);
		group.setText(title);
		return group;
	}

	private void addLegendIcon(Group legendGroup, String iconName) {
		Label iconLabel = new Label(legendGroup, SWT.NONE);
		iconLabel.setImage(ImageRepository.getImageDescriptor("help\\" + iconName).createImage());
		GridData data = new GridData(GridData.CENTER, GridData.CENTER, true, true);
		iconLabel.setLayoutData(data);
	}
	
	private void addLegendLabel(Group legendGroup, String label) {
		Label textLabel = new Label(legendGroup, SWT.NONE);
		textLabel.setText(label);
		textLabel.setAlignment(SWT.CENTER);
		GridData data = new GridData(GridData.CENTER, GridData.CENTER, true, true);
		textLabel.setLayoutData(data);
	}
	
}
