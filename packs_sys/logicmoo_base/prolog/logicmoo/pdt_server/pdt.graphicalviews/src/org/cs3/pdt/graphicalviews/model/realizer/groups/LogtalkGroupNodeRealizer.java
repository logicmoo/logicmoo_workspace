package org.cs3.pdt.graphicalviews.model.realizer.groups;

import java.awt.Color;

import org.cs3.pdt.graphicalviews.model.GraphModel;
import org.cs3.pdt.graphicalviews.utils.LogtalkStyles;

import y.geom.YInsets;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.hierarchy.GroupNodeRealizer;

public class LogtalkGroupNodeRealizer extends GroupNodeRealizer {

	public LogtalkGroupNodeRealizer() {
	}

	public LogtalkGroupNodeRealizer(NodeRealizer noderealizer) {
		super(noderealizer);
	}

	public void init(GraphModel graphModel) {
		setAutoBoundsEnabled(true);
		
		NodeLabel label = getLabel();
		label.setPosition(NodeLabel.BOTTOM_LEFT);
		label.setAlignment(NodeLabel.LEFT);
		label.setModel(NodeLabel.INTERNAL);
		label.setBackgroundColor(new Color(1.0f, 1.0f, 1.0f, 0.0f));
		label.setTextColor(Color.BLACK);
		
		
		String nodeStyle = graphModel.getDataHolder().getNodeStyle(getNode());
		LogtalkStyles logtalkStyles = new LogtalkStyles(nodeStyle);
		
		double m = logtalkStyles.getMargin();
		if (m <= 0) {
			m = 5.0;
		}
		YInsets minInsets = new YInsets(m, m, m, m);
		setMinimalInsets(minInsets);
		
		Color color = logtalkStyles.getColor();
		if (color != null) {
			setFillColor(color);
		}
		
		byte shapeType = logtalkStyles.getShapeType();
		if (shapeType >= 0) {
			setShapeType(shapeType);
		}
	}

}
