package RegulusGUI;

public class treeNode {
	public String Label;
	public int    NodeNo;
	public boolean Cut;
	
	
	public treeNode() {
	}
	public treeNode(int num) {
		NodeNo = num;
	}
	public treeNode(String lab, int num) {
		Label = lab;
		NodeNo = num;
	}
	public treeNode(String lab, int num,boolean cut) {
		Label = lab;
		NodeNo = num;
		Cut = cut;
		
		
	}
	public String toString() {
		// return Label + " (" + NodeNo + ")";
		 return Label; 
	}

}

