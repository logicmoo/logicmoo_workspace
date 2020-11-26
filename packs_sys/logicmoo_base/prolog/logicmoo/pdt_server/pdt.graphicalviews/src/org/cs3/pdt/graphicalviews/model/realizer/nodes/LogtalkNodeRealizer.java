package org.cs3.pdt.graphicalviews.model.realizer.nodes;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;

import org.cs3.pdt.graphicalviews.model.GraphModel;
import org.cs3.pdt.graphicalviews.utils.LogtalkStyles;

import y.base.Node;
import y.view.LineType;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.YLabel;

public class LogtalkNodeRealizer extends NodeRealizerBase {
	
	private NodeLabel contentsLabel;

	public LogtalkNodeRealizer() {
		init();
	}

	public LogtalkNodeRealizer(NodeRealizer r) {
		super(r);
		if (r instanceof LogtalkNodeRealizer) {
			contentsLabel = (NodeLabel) ((LogtalkNodeRealizer) r).contentsLabel.clone();
			contentsLabel.bindRealizer(this);
		} else {
			init();
		}
	}

	public LogtalkNodeRealizer(byte roundRect) {
		super(roundRect);
		init();
	}
	
	private void init() {
		contentsLabel = new NodeLabel();
		contentsLabel.bindRealizer(this);
		contentsLabel.setAlignment(YLabel.ALIGN_CENTER);
		contentsLabel.setModel(NodeLabel.FREE);
	}
	
	public void setContent(String content) {
		if (content != null && !content.isEmpty()) {
			contentsLabel.setText(content);
		}
	}

	public void init(GraphModel graphModel) {
		Node node = getNode();
		
		String nodeStyle = graphModel.getDataHolder().getNodeStyle(node);
		LogtalkStyles logtalkStyles = new LogtalkStyles(nodeStyle);
		
		Color color = logtalkStyles.getColor();
		if (color != null) {
			setFillColor(color);
		}
		LineType lineType = logtalkStyles.getLineType();
		if (lineType != null) {
			setLineType(lineType);
		}
		byte shapeType = logtalkStyles.getShapeType();
		if (shapeType >= 0) {
			setShapeType(shapeType);
		}
		
		String nodeLabel = graphModel.getDataHolder().getNodeLabel(node);
		setLabelText(nodeLabel);
		
		String nodeContent = graphModel.getDataHolder().getNodeContent(node);
		setContent(nodeContent);
		
		NodeLabel label = getLabel();
		label.setFontStyle(Font.BOLD);
		
		double width = label.getWidth() + 10.0;
		double height = 3.0;
		
		height += label.getHeight() + 3.0;
		
		if (!contentsLabel.getText().isEmpty()) {
			width = Math.max(width, contentsLabel.getWidth() + 10.0);
			height += contentsLabel.getHeight() + 3.0;
		}
		setSize(width, height);
	}
	
	@Override
	public void paintText(Graphics2D graphics2d) {
		double yoff = 3.0;

		NodeLabel label = getLabel();
		label.setOffset((getWidth() - label.getWidth()) / 2.0, yoff);
		label.paint(graphics2d);
		yoff += label.getHeight() + 3.0;

		if (!contentsLabel.getText().isEmpty()) {
			yoff += 3.0;
			contentsLabel.setOffset((getWidth() - contentsLabel.getWidth()) / 2.0, yoff);
			contentsLabel.paint(graphics2d);
			yoff += contentsLabel.getHeight() + 3.0;
		}
	}
	
}
