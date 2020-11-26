package org.cs3.pdt.analysis.views.prolog;

import java.util.Set;

import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.connector.registry.PrologProcessRegistryEvent;
import org.cs3.pdt.connector.registry.PrologProcessRegistryListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

public class ProcessSelector extends Composite implements PrologProcessRegistryListener {
	
	private Combo processCombo;
	private Label processLabel;

	public ProcessSelector(Composite parent) {
		super(parent, SWT.FILL | SWT.BEGINNING);
		
		GridData horizontalFillData = new GridData(GridData.FILL_HORIZONTAL);
		horizontalFillData.grabExcessHorizontalSpace = true;
		horizontalFillData.minimumWidth = 120;
		
		setLayout(new FillLayout(SWT.VERTICAL));
		
		Composite processComposite = new Composite(this,SWT.NONE);		
		processComposite.setLayout(new GridLayout(2,false));
		
		processLabel = new Label(processComposite, SWT.LEFT);
		processLabel.setText("Process:");
		
		processCombo = new Combo(processComposite, SWT.READ_ONLY);
		processCombo.setLayoutData(horizontalFillData);
		
		updateProcessList();
		
		PDTConnectorPlugin.getDefault().getPrologProcessRegistry().addPrologProcessRegistryListener(this);
	}

	private void updateProcessList() {
		if (isDisposed()) {
			return;
		}
		getDisplay().asyncExec(new Runnable() {
			@Override
			public void run() {
				if (isDisposed()) {
					return;
				}
				String text = processCombo.getText();
				Set<String> allKeys = PDTConnectorPlugin.getDefault().getPrologProcessRegistry().getAllKeys();
				processCombo.setItems(allKeys.toArray(new String[allKeys.size()]));
				processCombo.setText(text);
			}
		});
	}

	public String getSelectedProcessName() {
		return processCombo.getText();
	}
	
	public void setSelectedProcessName(String name) {
		processCombo.setText(name);
	}
	
	public void addSelectionListener(SelectionListener listener) {
		processCombo.addSelectionListener(listener);
	}
	
	public void removeSelectionListener(SelectionListener listener) {
		processCombo.removeSelectionListener(listener);
	}

	@Override
	public boolean setFocus() {
		return processCombo.setFocus();
	}

	@Override
	public void processAdded(PrologProcessRegistryEvent e) {
		updateProcessList();
	}

	@Override
	public void processRemoved(PrologProcessRegistryEvent e) {
		updateProcessList();
	}

	@Override
	public void subscriptionAdded(PrologProcessRegistryEvent e) {
	}

	@Override
	public void subscriptionRemoved(PrologProcessRegistryEvent e) {
	}
	
	@Override
	public void dispose() {
		PDTConnectorPlugin.getDefault().getPrologProcessRegistry().removePrologProcessRegistryListener(this);
		super.dispose();
	}
}
