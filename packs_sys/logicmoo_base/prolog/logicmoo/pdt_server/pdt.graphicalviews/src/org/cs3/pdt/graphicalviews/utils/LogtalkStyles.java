package org.cs3.pdt.graphicalviews.utils;

import java.awt.Color;
import java.awt.geom.GeneralPath;
import java.util.HashMap;

import y.view.Arrow;
import y.view.LineType;
import y.view.ShapeNodeRealizer;

public class LogtalkStyles {

	private static final HashMap<String, Color> x11Colors = new HashMap<String, Color>();
	private static final HashMap<String, Arrow> targetArrows = new HashMap<String, Arrow>();
	
	static {
		x11Colors.put("white", Color.WHITE);
		x11Colors.put("snow1", new Color(255, 250, 250));
		x11Colors.put("snow2", new Color(238, 233, 233));
		x11Colors.put("snow3", new Color(205, 201, 201));
		x11Colors.put("beige", new Color(245, 245, 220));
		x11Colors.put("yellow", Color.YELLOW);
		x11Colors.put("aquamarine", new Color(127, 255, 212));
		x11Colors.put("cyan", Color.CYAN);
		x11Colors.put("gainsboro", new Color(64, 220, 220));
		x11Colors.put("turquoise", new Color(220, 224, 208));
		
		targetArrows.put("vee", Arrow.STANDARD);
		targetArrows.put("normal", Arrow.DELTA);
		targetArrows.put("onormal", Arrow.WHITE_DELTA);
		targetArrows.put("dot", Arrow.CIRCLE);
		GeneralPath generalPath;
		generalPath = new GeneralPath(GeneralPath.WIND_NON_ZERO, 6);
		generalPath.moveTo(0.0F, 0.0F);
		generalPath.lineTo(0.0F, 5F);
		generalPath.lineTo(-10F, 5F);
		generalPath.lineTo(-10F, -5F);
		generalPath.lineTo(0.0F, -5F);
		generalPath.closePath();
		targetArrows.put("box", Arrow.addCustomArrow("box", generalPath, Color.BLACK, LineType.LINE_1, Color.BLACK));
		generalPath = new GeneralPath(GeneralPath.WIND_NON_ZERO, 6);
		generalPath.moveTo(0.0F, 0.0F);
		generalPath.lineTo(0.0F, 5F);
		generalPath.lineTo(-10F, 5F);
		generalPath.lineTo(-10F, -5F);
		generalPath.lineTo(0.0F, -5F);
		targetArrows.put("obox", Arrow.addCustomArrow("obox", generalPath, Color.WHITE, LineType.LINE_1, Color.BLACK));
		generalPath = new GeneralPath(1, 6);
		generalPath.moveTo(0.0F, 0.0F);
		generalPath.lineTo(-7F, 5F);
		generalPath.lineTo(-14F, 0.0F);
		generalPath.closePath();
		targetArrows.put("rdiamond", Arrow.addCustomArrow("rdiamond", generalPath, Color.BLACK, LineType.LINE_1, Color.BLACK));
		generalPath = new GeneralPath(1, 6);
		generalPath.moveTo(0.0F, 0.0F);
		generalPath.lineTo(0.0F, 5F);
		generalPath.lineTo(-14F, 0F);
		generalPath.lineTo(0.0F, -5F);
		generalPath.closePath();
		targetArrows.put("inv", Arrow.addCustomArrow("inv", generalPath, Color.BLACK, LineType.LINE_1, Color.BLACK));
	}
	
	private static final String KEY_COLOR = "color";
	private Color color;
	
	private static final String KEY_MARGIN = "margin";
	private double margin = -1.0;
	
	private static final String KEY_STYLE = "style";
	private static final String KEY_SHAPE = "shape";
	private byte shapeType = -1;
	private LineType lineType;
	
	private static final String KEY_TARGET_ARROW = "arrowhead";
	private Arrow sourceArrow;

	public LogtalkStyles(String styles) {
		for (String keyValuePair : styles.split(";")) {
			String[] keyAndValue = keyValuePair.split("=");
			if (keyAndValue.length == 2) {
				setValue(keyAndValue[0], keyAndValue[1]);
			}
		}
	}
	
	private void setValue(String key, String value) {
		if (KEY_COLOR.equals(key)) {
			color = x11Colors.get(value);
		} else if (KEY_MARGIN.equals(key)) {
			try {
				margin = Double.parseDouble(value);
			} catch (NumberFormatException e) {}
		} else if (KEY_STYLE.equals(key)) {
			if ("rounded".equals(value)) {
				shapeType = ShapeNodeRealizer.ROUND_RECT;
			} else if ("\"filled,dashed\"".equals(value)) {
				lineType = LineType.DASHED_1;
			}
		} else if (KEY_SHAPE.equals(key)) {
			// XXX: do something
		} else if (KEY_TARGET_ARROW.equals(key)) {
			sourceArrow = targetArrows.get(value);
		}
	}
	
	public Color getColor() {
		return color;
	}

	public double getMargin() {
		return margin;
	}

	public byte getShapeType() {
		return shapeType;
	}

	public LineType getLineType() {
		return lineType;
	}
	
	public Arrow getSourceArrow() {
		return sourceArrow;
	}

}
