package org.cs3.pdt.graphicalviews.focusview;

import org.cs3.pdt.graphicalviews.internal.ImageRepository;
import org.cs3.pdt.graphicalviews.internal.ui.ToolBarAction;
import org.cs3.pdt.graphicalviews.preferences.PredicateVisibilityPreferences;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;

public abstract class CallGraphViewBase extends ViewBase {
	private boolean metapredicateCallsVisible = true;
	private boolean inferredCallsVisible = true;

	public boolean isMetapredicateCallsVisible() {
		return metapredicateCallsVisible;
	}
	
	public boolean isInferredCallsVisible() {
		return inferredCallsVisible;
	}
	
	@Override
	protected void initViewButtons(IToolBarManager toolBarManager) {
		super.initViewButtons(toolBarManager);
		
		toolBarManager.add(new ToolBarAction("Show PDT Predicates",
				ImageRepository.getImageDescriptor(ImageRepository.P)) {
			{
				setChecked(PredicateVisibilityPreferences.showPDTPredicates());
			}

			@Override
			public int getStyle() {
				return IAction.AS_CHECK_BOX;
			}
			
			@Override
			public void performAction() {
				PredicateVisibilityPreferences.setShowPDTPredicates(isChecked());
				updateCurrentFocusView();	
			}
		});
		
		toolBarManager.add(new ToolBarAction("Show SWI Predicates",
				ImageRepository.getImageDescriptor(ImageRepository.S)) {
			{
				setChecked(PredicateVisibilityPreferences.showSWIPredicates());
			}

			@Override
			public int getStyle() {
				return IAction.AS_CHECK_BOX;
			}
			
			@Override
			public void performAction() {
				PredicateVisibilityPreferences.setShowSWIPredicates(isChecked());
				updateCurrentFocusView();	
			}
		});
		
		toolBarManager.add(new ToolBarAction("Show Calls To Metapredicates",
				ImageRepository.getImageDescriptor(ImageRepository.M)) {
				{
					setChecked(metapredicateCallsVisible);
				}
			
				@Override
				public int getStyle() {
					return IAction.AS_CHECK_BOX;
				}
				
				@Override
				public void performAction() {
					metapredicateCallsVisible = !metapredicateCallsVisible;
					updateCurrentFocusView();	
				}
			});
		
		toolBarManager.add(new ToolBarAction("Show Inferred Calls",
				ImageRepository.getImageDescriptor(ImageRepository.I)) {
				{
					setChecked(inferredCallsVisible);
				}
				
				@Override
				public int getStyle() {
					return IAction.AS_CHECK_BOX;
				}
			
				@Override
				public void performAction() {
					inferredCallsVisible = !inferredCallsVisible;
					updateCurrentFocusView();	
				}
			});
	}
}
