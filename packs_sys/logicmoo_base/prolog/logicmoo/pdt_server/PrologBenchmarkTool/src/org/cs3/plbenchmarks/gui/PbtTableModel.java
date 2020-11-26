package org.cs3.plbenchmarks.gui;

import java.util.ArrayList;
import java.util.List;

import javax.swing.table.DefaultTableModel;

import org.cs3.plbenchmarks.PbtController;
import org.cs3.plbenchmarks.data.FactbaseVersionPair;

public class PbtTableModel extends DefaultTableModel {
	
	private static final long serialVersionUID = 1L;

	private Boolean[][] rowData;
	
	private PbtController controller;

	public PbtTableModel(PbtController controller) {
		this.controller = controller;
		rowData = new Boolean[controller.getFactbaseCount()][controller.getVersionCount()];
		for (int i = 0; i < rowData.length; i++) {
			for (int j = 0; j < rowData[i].length; j++) {
				rowData[i][j] = false;
			}
		}
	}
	
	@Override
	public Class<?> getColumnClass(int i) {
		if (i == 0) {
			return String.class;
		} else {
			return Boolean.class;
		}
	}

	@Override
	public int getColumnCount() {
		if (controller == null) {
			return 0;
		}
		return controller.getVersionCount() + 1;
	}

	@Override
	public String getColumnName(int i) {
		if (i == 0) {
			return "factbase";
		} else {
			return controller.getVersion(i-1).getName();
		}
	}

	@Override
	public int getRowCount() {
		if (controller == null) {
			return 0;
		}
		return controller.getFactbaseCount();
	}

	@Override
	public Object getValueAt(int row, int col) {
		if (col == 0) {
			return controller.getFactbase(row);
		} else {
			return rowData[row][col-1];
		}
	}

	@Override
	public boolean isCellEditable(int row, int col) {
		return (col > 0);
	}

	@Override
	public void setValueAt(Object aValue, int row, int col) {
		if (col > 0 && aValue instanceof Boolean) {
			rowData[row][col-1] = (Boolean) aValue;
		}
	}

	public List<FactbaseVersionPair> getSelectedPairs() {
		List<FactbaseVersionPair> list = new ArrayList<>();
		for(int i=0; i<rowData.length; i++) {
			for(int j=0; j<rowData[i].length; j++) {
				if (rowData[i][j] != null && rowData[i][j]) {
					list.add(new FactbaseVersionPair(controller.getVersion(j).getName(), controller.getFactbase(i)));
				}
			}
		}
		return list;
	}

	public void setSelection(List<FactbaseVersionPair> pairs) {
		for (int i = 0; i < rowData.length; i++) {
			for (int j = 0; j < rowData[i].length; j++) {
				FactbaseVersionPair dummyPair = new FactbaseVersionPair(controller.getVersion(j).getName(), controller.getFactbase(i));
				if (pairs.contains(dummyPair)) {
					rowData[i][j] = true;
				} else {
					rowData[i][j] = false;
				}
			}
		}
	}

	public void selectRow(int rowAtPoint) {
		boolean everythingSelected = true;
		
		for (int i=0; i<rowData[rowAtPoint].length; i++) {
			if(!rowData[rowAtPoint][i]) {
				everythingSelected = false;
				break;
			}
		}
		
		for (int i=0; i<rowData[rowAtPoint].length; i++) {
			rowData[rowAtPoint][i] = !everythingSelected;
		}
		
		fireTableDataChanged();
	}

	public void selectCol(int columnAtPoint) {
		if (columnAtPoint > 0) {
			boolean everythingSelected = true;
			for (int i=0; i<rowData.length; i++) {
				if(!rowData[i][columnAtPoint-1]) {
					everythingSelected = false;
					break;
				}
			}
			
			for (int i=0; i<rowData.length; i++) {
				rowData[i][columnAtPoint-1] = !everythingSelected;
			}
		}
		fireTableDataChanged();
	}

	public void selectAll() {
		boolean everythingSelected = true;
		for (int i=0; i<rowData.length; i++) {
			for (int j=0; j<rowData[i].length; j++) {
				if(!rowData[i][j]) {
					everythingSelected = false;
					break;
				}
			}
		}

		for (int i=0; i<rowData.length; i++) {
			for (int j=0; j<rowData[i].length; j++) {
				rowData[i][j] = !everythingSelected;
			}
		}
		fireTableDataChanged();
	}

}
