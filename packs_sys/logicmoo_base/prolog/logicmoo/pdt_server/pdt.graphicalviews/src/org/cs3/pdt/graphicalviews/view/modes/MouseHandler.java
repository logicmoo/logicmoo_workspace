package org.cs3.pdt.graphicalviews.view.modes;

import org.cs3.pdt.graphicalviews.focusview.NavigationToolTip;
import org.cs3.pdt.graphicalviews.focusview.ViewBase.FocusViewControl;
import org.cs3.pdt.graphicalviews.main.PDTGraphView;
import org.cs3.pdt.graphicalviews.model.realizer.edges.InfoTextProvider;
import org.cs3.pdt.graphicalviews.model.realizer.edges.LoadEdgeRealizer;
import org.cs3.pdt.graphicalviews.model.realizer.nodes.NodeRealizerBase;
import org.cs3.pdt.graphicalviews.preferences.PredicateLayoutPreferences;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.ui.progress.UIJob;

import y.base.Edge;
import y.base.Node;
import y.view.HitInfo;
import y.view.NodeRealizer;
import y.view.ViewMode;

public class MouseHandler extends ViewMode {

	private final ToolTip t;
	private final NavigationToolTip nav;
	private final FocusViewControl focusView;
	
	public MouseHandler(FocusViewControl focusView) {
		this.focusView = focusView;
		this.t = new ToolTip(focusView.getShell(), SWT.NONE);
		this.t.setVisible(false);
		this.nav = new NavigationToolTip(focusView);
	}
	
	@Override
	public void mouseClicked(double x, double y) {
		super.mouseClicked(x, y);	
		
		if (focusView.isNavigationEnabled()) return;
		
		HitInfo hitInfo = getHitInfo(x, y);
		
		if (hitInfo.hasHits())
			showToolTip("Double click to show in editor");
	}
	
	@Override
	public void mouseMoved(final double x, final double y) {
		super.mouseMoved(x, y);
		
		updateStatus(x, y);
	}

	protected void updateStatus(double x, double y) {
		HitInfo hitInfo = getHitInfo(x, y);
		PDTGraphView view = focusView.getPdtGraphView();
		
		String text = "";
		
		if (hitInfo.hasHitNodes()) {
			Node node = hitInfo.getHitNode();
			
			NodeRealizer realizer = view.getGraph2D().getRealizer(node);
			if (realizer instanceof NodeRealizerBase) {
				text = ((NodeRealizerBase)realizer).getInfoText();
			}
		}
		else if (hitInfo.hasHitEdges()) {
			Edge edge = hitInfo.getHitEdge();
			text = view.getDataHolder().getEdgeLabel(edge);
		}
		else if (hitInfo.hasHitEdgeLabels()) {
			Edge edge = hitInfo.getHitEdgeLabel().getEdge();
			InfoTextProvider realizer = (InfoTextProvider)view.getGraph2D().getRealizer(edge);
			text = realizer.getInfoText();
			
			if (realizer instanceof LoadEdgeRealizer)
			{
				final String content = text;
				new UIJob("Updating status") {
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						nav.setContent(content);
						nav.activate();
						return Status.OK_STATUS;
					}
				}.schedule();
			}
		}
		
		if (text == null)
			text = "";
		
		if (text.startsWith("Predicate"))
		{
			text = text.substring(11);
		}
		
		if (!text.equals(focusView.getInfoText()))
		{
			focusView.setInfoText(text);
			showToolTip(text);
		}
	}

	private void showToolTip(final String finalText) {
		new UIJob("Updating status") {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				
				if (PredicateLayoutPreferences.isShowToolTip() && finalText != null && finalText.length() > 0) {
					t.setVisible(false);
					Point location = Display.getCurrent().getCursorLocation();
					location.x += 10;
					location.y += 10;
					t.setLocation(location);
					t.setMessage(finalText);
					t.setVisible(true);
				}
				else {
					t.setVisible(false);
				}
				
				return Status.OK_STATUS;
			}
		}.schedule();
	}
}
