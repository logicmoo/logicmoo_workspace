
package RegulusGUI;
import java.awt.*;
import javax.swing.*;
import javax.swing.tree.*;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.Toolkit;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreeSelectionModel;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;

public class DynamicTree extends DefaultTreeCellRenderer   
							implements TreeExpansionListener,MouseListener,ItemListener  {
	
	
    protected DefaultMutableTreeNode rootNode;
    protected DefaultTreeModel treeModel;
    protected JTree tree;
    protected DefaultMutableTreeNode selectedNode = null;
    private DefaultMutableTreeNode node;
   // private TreePath treePath;
    protected Enumeration vEnum = null;
    private Toolkit toolkit = Toolkit.getDefaultToolkit(); 
    public  JScrollPane scrollPane;
    public int testint = 0;
    private String holdSentence = "";
    private String nodeno = "";
    public int Nodeval = 0;
    public int saveInd = 0;
    public String compareString = "(";
    public String Node = ""; 
    public int countDaughters = 0;
    private String Trace = "";
    private String holdDaugher = "";
    private String holdTrace = "";
    private String stringNode = ""; 
    private String[] LeafText = new String[50];
    private String[] NodeText = new String[50];
    private boolean[] cutNodeNoTable = new boolean[50];
    private int textIndex = 0;
    private String LeafLabel = "";
    private String nodeLabel = "";
    private   int holdPos = 0;
    private   int nodeIndex = 0;
    private   int treeIndex = 0;
    private   int saveIndex = 0;
    private String expandNode = "";
    private String holdLabel = "";
    private String compareNode = "";
    private String foundNode = "";
    private String TreeTableString = "";
    private String HoldLeaf = "";
    private boolean leafFound = false;
    private boolean nodeClicked = false;
    private boolean leafIsEmpty = false;
    private boolean treeCollapsed = false;
    private boolean treeExpanded = false;
    private int nodeExistUnderLeafCounter = 0;
    private boolean editButton = false;
    public boolean cutExist; 
    private String catAndcut = "";
    private int index = 0;
    public static final ImageIcon CUT_IMAGE = new ImageIcon("cut.gif");
    private String CutName = "";
    private String AddedCutName = "";
    private String cutNode = ""; 
    private boolean node_updated = false;
    private boolean treebuilt = false;
    private boolean nodeContainsCut = false;
    private String holdNode = "";
    private int cutIndex = 0;
    private int cutNodeNo = 0;
    private boolean iscut = false;
    private int checkNodeNo = 0;
    private partTreeNode parttreenode = null;
    private int[] nodetable = new int[50];
    private int[] nodeNumtable = new int[50];
    private int nodeInd = 0;
    private int nodenum = 0; 
    private int prevcutNodeNo = 0;
    private  int nodeNumIndex = 0;
    private JPopupMenu popup;
    public  boolean rightClick;
    private TreePane treepane = null;
    private  String newLeaf = "";
    //private DefaultTreeCellRenderer renderer = new  DefaultTreeCellRenderer();
    protected  DefaultMutableTreeNode setDefaultMutableTreeNode (DefaultMutableTreeNode tt) {
    	return selectedNode = tt;
    	
     	
}
    
//	 set the pointer to the treePane window
	  public void setTreePane(TreePane window) {
		  treepane = window;
	  }
    
    public JTree getTree() {
    	return tree;
    }
    
    public void populateTreeFromRegulusTree(RegulusTree regulusTree) {
    	populateTreeFromRegulusTree(rootNode, regulusTree);
    }
    
    // Take the information from regulusTree and use it to fill in the subtree rooted in "node"
    public void populateTreeFromRegulusTree(DefaultMutableTreeNode node, RegulusTree regulusTree) {
    	// If this is a lex node, we're done
    	if ( regulusTree.isLex() ) {
        	treebuilt = true;
    		return;
    	}
    	// It wasn't a lex node, so it must have daughters
    	else {
     		// Find the daughters
    		RegulusTree[] daughters = regulusTree.getDaughters();
    		// Find how many of them there are
    		int nDaughters = regulusTree.getNDaughters();
    		// For each daughter, do the following:
    		for ( int i = 0; i < nDaughters ; i++ ) {
    			// Get that daughter in the RegulusTree
    			RegulusTree regulusDaughter = daughters[i];
    			// daughterLabel is the string we're going to put on that daughter in the JTree
    			treeNode daughterLabel = null;
    			treeNode LabelNum = null;
     			// If it's a lex daughter, then use the lex, and use zero to mean no node number
    			if ( regulusDaughter.isLex() ) {
    				daughterLabel = new treeNode(regulusDaughter.getLex(), 0);
    			}
    			// If it's a non-lex daughter, use the cat, and a real node number
    			else {
    				//LabelNum = new treeNode(regulusDaughter.getNodeNumber());
     				//cutExist = regulusDaughter.getCut();
    				//nodenum = regulusDaughter.getNodeNumber();
    				//createTable();
      				//if (editButton && cutExist) {
     					daughterLabel = new treeNode(regulusDaughter.getCat(), regulusDaughter.getNodeNumber());
      					//treeIndex = treeIndex + 1;
      					
    				//}
    				//else
    				//{
     				//daughterLabel = new treeNode(regulusDaughter.getCat(), regulusDaughter.getNodeNumber(),
     				//		regulusDaughter.getCut());
         			//	treeIndex = treeIndex + 1;
    				//}
     			}
    			// Create a new daughter node and add it to the current node
    			
     			DefaultMutableTreeNode daughter = addObject( node,daughterLabel , true );
     			
     			//System.out.println("daughterLabel "+daughterLabel);
       			countDaughters = countDaughters + 1;
       			populateTreeFromRegulusTree( daughter, regulusDaughter );
       			
    		}
    	}
    	
    }
    
    public void createTable() {
    	if (cutExist)
    	{
    		nodetable[nodeInd] = nodenum;
    		//System.out.println("nodetable[nodeInd] "+nodetable[nodeInd]);
    		nodeInd++;
      	}
    }
    public void createTableFromTree() {
      	cutNodeNo = ((treeNode) node.getUserObject()).NodeNo;
    	boolean iscut = ((treeNode) node.getUserObject()).Cut;
    	cutNodeNoTable[cutNodeNo] = iscut;
    	
    }
    public DynamicTree() {
    	
    }

    public DynamicTree(RegulusTree regulusTree, boolean editBtn,TreePane frame) {
    	
       // new GridLayout(1,0);
    	
    	setTreePane(frame);
    	editButton = editBtn;
        rootNode = new DefaultMutableTreeNode( new treeNode(regulusTree.getCat(), regulusTree.getNodeNumber()));
        treeModel = new DefaultTreeModel(rootNode);
        treeModel.addTreeModelListener(new MyTreeModelListener());
        tree = new JTree(treeModel);
        tree.setEditable(true);
        tree.getSelectionModel().setSelectionMode
                (TreeSelectionModel.SINGLE_TREE_SELECTION);
        tree.setShowsRootHandles(true);
        tree.addTreeExpansionListener(this);
        tree.addMouseListener(this);
        //MyRenderer myrenderer = new MyRenderer();
        //tree.setCellRenderer(myrenderer);
        scrollPane = new JScrollPane(tree);
        add(scrollPane);
        
       
     }
 
    public int indexOfUserObject(DefaultMutableTreeNode parent, Object userObject)
    {
        for (Enumeration e = parent.children(); e.hasMoreElements();)
        {
        	
        	DefaultMutableTreeNode node = (DefaultMutableTreeNode) e.nextElement();
        	int cutNodeNo = ((treeNode) node.getUserObject()).NodeNo;
          	cutIndex = cutIndex + 1;
         	//System.out.println("cutIndex "+cutIndex);
        	Object obje = node.getUserObject();
          }
        return -1;
    }
   
    public void checkTable() {
    	if(cutNodeNoTable[cutNodeNo] == false || cutNodeNoTable[cutNodeNo] == true)
    	{
    		//System.out.println("cutNodeNoTable[cutNodeNo] "+cutNodeNoTable[cutNodeNo]);
    	}
    	else
    		cutNodeNoTable[cutNodeNo] = iscut;
    }
   public void iterateTree()
   {
	 
	 Object obj =  tree.getModel().getRoot();
	 TreePath treePath = new TreePath(obj);
	 DefaultMutableTreeNode node = ((DefaultMutableTreeNode)  obj);
	 getRestOfTree(node, treePath);
   }
   
   public void getRestOfTree(DefaultMutableTreeNode node, TreePath treePath){
	   TreePath nextPath = createPath(node, treePath);
	   //System.out.println("node "+node);
   // checkLeaf(node);
	   if (node.isLeaf()) {
		   return;
	   }
	   else if (allChildrenAreLeaves(node)) {
		   //Close the node
		   CloseNode(node, treePath);
	   }
	   else {
		   Enumeration children = node.children();
		   if (children != null) {
			   //countChildren(node,treePath);
			   while (children.hasMoreElements()){
      			   getRestOfTree((DefaultMutableTreeNode) children.nextElement(), nextPath);
			  
			   }
   			}
	   }			   
   }
 
   public void iterateTreeUnderNode()
       {
    	 nodeInd = 0;
    	 Object obj =  tree.getModel().getRoot();
    	 TreePath treePath = new TreePath(obj);
    	 DefaultMutableTreeNode node = ((DefaultMutableTreeNode)  obj);
    	 getRestOfTreeUnderNode(node, treePath);
    
       }
    
    
       public void getRestOfTreeUnderNode(DefaultMutableTreeNode node, TreePath treePath){
       	 TreePath nextPath = createPath(node, treePath);
      		   Enumeration children = node.children();
     		   if (children != null) {
     			   //countChildren(node,treePath);
     			   while (children.hasMoreElements()){
      				 int cutNodeNo = ((treeNode) node.getUserObject()).NodeNo;
     				 //System.out.println("cutNodeNo "+cutNodeNo);
     				 if (cutNodeNo == nodetable[nodeInd] && cutNodeNo !=prevcutNodeNo)
     				 {
     					 nodeNumIndex = 0;
      					prevcutNodeNo = cutNodeNo;
     					getRestOfTreeUntilCut(node, nextPath);
     				 }
           			 getRestOfTreeUnderNode((DefaultMutableTreeNode) children.nextElement(), nextPath);
         		}
     	   }
       }
       
       public void getRestOfTreeUntilCut(DefaultMutableTreeNode node,TreePath treePath) {
    	   TreePath nextPath = createPath(node, treePath);
    	   Enumeration children = node.children();
    	   if (children != null) {
    		   while (children.hasMoreElements()) {
    			   int cutNodeNo = ((treeNode) node.getUserObject()).NodeNo;
    				 //System.out.println("cutNodeNo "+cutNodeNo);
    				 nodeNumtable[nodeNumIndex] = cutNodeNo;
    				 nodeNumIndex = nodeNumIndex + 1;
    				 //System.out.println("nodeNumIndex "+nodeNumIndex);
    			     getRestOfTreeUntilCut((DefaultMutableTreeNode) children.nextElement(), nextPath);
    		   }
    	   }
       }
   public boolean allChildrenAreLeaves(DefaultMutableTreeNode node){
	   boolean result = true;
	   Enumeration children = node.children();
	   if (children != null) {
		   while (children.hasMoreElements()) {
			   DefaultMutableTreeNode nextChild = (DefaultMutableTreeNode) children.nextElement(); 
			   if ( !nextChild.isLeaf() ) {
					   result = false;
				   }
		   }
	   }
	   return result;
	   }
   
   public TreePath createPath(TreeNode node,TreePath treePath)
   {
	   if (node.isLeaf())
	   {
		   // do nothing
		   return null;
	   }
	   else
	   {
//		 create path 
		   if (node.toString().equals(".MAIN"))
		   {
			   return treePath;
			   // do nothing
		   }
		   else
		   {
		 DefaultMutableTreeNode pathNode = ((DefaultMutableTreeNode)  node);
		 TreePath Path = treePath.pathByAddingChild(pathNode);
		 String stringPath = treePath.toString();
 		 //System.out.println("stringPath in createPath"+stringPath);
 		 return Path;
		   }
	   }
   }
   
    public void CloseNode(TreeNode node, TreePath treePath)
    {
    	DefaultMutableTreeNode pathnode = ((DefaultMutableTreeNode)  node);
		TreePath path = treePath.pathByAddingChild(pathnode);
   		// we have the leaf get the node to be able to close node
  		tree.collapsePath(path);
   	 }
    
   
    public void mouseClicked(MouseEvent e)
    {
     	  SwingUtilities.isRightMouseButton(e);
     	    {
     	       // it is right mouse click
      	    	treepane.rightClickOnTree(e);
     	    	
     	    }
    
     }
   
   
    public void itemStateChanged(ItemEvent e) {
    	
    }
    public void mousePressed(MouseEvent e)
    {
  	 
    }
    public void mouseReleased(MouseEvent e)
    {
    }
    public void mouseEntered(MouseEvent e)
    {
    }
    public void mouseExited(MouseEvent e)
    {
    }
    public void keyPressed( KeyEvent e)
    {
     		
    }
    public void keyReleased( KeyEvent e)
    {
  	  // TODO method stub 
  	 // System.out.println("key released"+e.getKeyCode());
    }
    public void keyTyped( KeyEvent e)
    {
  	  // TODO method stub 
  	  //System.out.println("key typed"+e.getKeyCode());
    }
   
    
    public void treeExpanded(TreeExpansionEvent e) {
    	System.out.println("Expand");
       	if (nodeClicked == true)
    	{
    		nodeClicked = false;
    		DefaultMutableTreeNode node;
    		node = (DefaultMutableTreeNode)
    		e.getPath().getLastPathComponent();
     		expandNode = node.toString();
     		reCreateNodeName();
     		int oldIndex = ((treeNode) node.getUserObject()).NodeNo;
           	treeNode newNode = new treeNode(holdLabel, oldIndex);
     	if (node != null)
    	{
  	// put LeafLabel into anode
    	node.setUserObject(newNode);
    	tree.updateUI();
     	}
    	}
     }
    public void treeCollapsed(TreeExpansionEvent e) {
    	System.out.println("Collapsed");
      	DefaultMutableTreeNode node;
    	node = (DefaultMutableTreeNode)
        e.getPath().getLastPathComponent();
    	// put the selected nodes text into a table so we have the old value if user wants to expand tree
    	nodeIndex = ((treeNode) node.getUserObject()).NodeNo;
    	nodeLabel = ((treeNode) node.getUserObject()).Label;
    	NodeText[nodeIndex] = nodeLabel;
     	textIndex = 0;
     	saveIndex = 0;
     	printDescendents(node);
    	createNewLabel();
    	// I have the new label now I want to get the selected node and update it 
    	DefaultMutableTreeNode anode;
    	anode = (DefaultMutableTreeNode)
        e.getPath().getLastPathComponent();
     //  update node with new value
    	int oldIndex = ((treeNode) node.getUserObject()).NodeNo;
    	treeNode newNode = new treeNode(LeafLabel, oldIndex);
    	System.out.println("LeafLabel "+LeafLabel);
     	if (anode != null)
    	{
     		// put LeafLabel into anode
    		anode.setUserObject(newNode);
    		tree.updateUI();
     	}
  	}
    public void printDescendents(TreeNode node){
    	if (node.isLeaf())
    	{
    		stringNode = node.toString();
    		//System.out.println("stringNode "+stringNode);
    		populateTable();
    	}
     	stringNode = node.toString();
    	Enumeration children = node.children();
    	if (children !=null) {
    		while (children.hasMoreElements()){
     			printDescendents((TreeNode) children.nextElement());
    		}
    	}
    }
   
  
    public void reCreateNodeName()
    {
     	 String b = " ";
		  holdPos = 0;
		  	if (expandNode.indexOf(b) != -1) {
			holdPos = expandNode.indexOf(b);
			holdPos = holdPos + 1;
			holdLabel = expandNode.substring(0,holdPos);
		  	}
    }
    public void populateTable()
    {
      	HoldLeaf = stringNode;
    	leafIsEmpty = false;
     	testleaf();
    	// don't include empty leafs in label
    	if (leafIsEmpty == false)
    		{
    			LeafText[textIndex] = HoldLeaf;
    			textIndex = textIndex + 1;
    			saveIndex = textIndex;
    		}
    	else
    		{
    			System.out.println("leaf is empty");
    		}
     }
   
    public void testleaf()
    {
     	 String b = "empt";
		  	if (HoldLeaf.indexOf(b) != -1) {
			leafIsEmpty = true;
		  	}
    }
    public void testLable() {
    	int newPos = 0;
    	 String b = " ";
		  	if (LeafLabel.indexOf(b) != -1) {
		  	newPos =LeafLabel .indexOf(b);
			newLeaf = LeafLabel.substring(0,newPos);
		  	}
    }
    public void createNewLabel()
    {
    	LeafLabel = "";
    	LeafLabel += nodeLabel + " ";
    	testLable();
    	LeafLabel = "";
    	LeafLabel = newLeaf + " ";
       	for ( int i = 0 ; i < saveIndex ; i++ )
    	{
     		LeafLabel += LeafText[i]+ " ";
       	}
    }
   
  
   public void saySomething(String eventDescription, TreeExpansionEvent e) {
    	//System.out.println(eventDescription + "; " + "path = "+e.getPath() );	
    	
    }
   
    /** Add child to the currently selected node. */
    public DefaultMutableTreeNode addObject(Object child) {
        DefaultMutableTreeNode parentNode = null;
        TreePath parentPath = tree.getSelectionPath();

        if (parentPath == null) {
            parentNode = rootNode;
        } else {
            parentNode = (DefaultMutableTreeNode)
                         (parentPath.getLastPathComponent());
        }

        return addObject(parentNode, child, true);
    }

    public DefaultMutableTreeNode addObject(DefaultMutableTreeNode parent,
                                            Object child) {
        return addObject(parent, child, false);
    }

    public DefaultMutableTreeNode addObject(DefaultMutableTreeNode parent,
                                            Object child, 
                                            boolean shouldBeVisible) {
        DefaultMutableTreeNode childNode = 
                new DefaultMutableTreeNode(child);

        if (parent == null) {
            parent = rootNode;
        }

        treeModel.insertNodeInto(childNode, parent, 
                                 parent.getChildCount());

        //Make sure the user can see the lovely new node.
        if (shouldBeVisible) {
            tree.scrollPathToVisible(new TreePath(childNode.getPath()));
        }
        return childNode;
    }
    public void editString()
    {
    	  Trace = holdDaugher;
		  String b = "(";
		  int holdPos = 0;
		  	if (Trace.indexOf(b) != -1) {
			holdPos = Trace.indexOf(b);
			holdTrace = Trace.substring(0,holdPos);
		  	}
    }
  
        public String getNode()
    {
    	 TreePath treePath = tree.getSelectionPath();
         DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) treePath.getLastPathComponent();
         Object obj = treeNode.getUserObject(); 
         treeNode daughterLabel = null;
         daughterLabel = new treeNode();
         daughterLabel = (treeNode)obj;
        // String dLabel = daughterLabel.Label;
          int dnodenum = daughterLabel.NodeNo;
         //System.out.println("dnodenum "+dnodenum);
         // make dnodumun to integer 
         Node = Integer.toString(dnodenum);
         // Node = GetNodeNo();
        return Node;
    }
   
 
   public class MyRenderer extends DefaultTreeCellRenderer   {
	  
   
	        public MyRenderer()
	        {
	            //always call super
	            super();
	        }    
	        
	        /** 
	        * getTreeCellRendererComponent 
	        * This method is overridden to set the node specific icons and tooltips
	        *    
	        * @return The Component object used to render the cell value
	        * @exception 
	        */ 
	        public Component getTreeCellRendererComponent(JTree tree, Object value,
	                                                      boolean selection, boolean expanded,
	                                                      boolean leaf, int row, boolean hasFocus)
	        {
	            super.getTreeCellRendererComponent(tree, value, selection, expanded, 
	                                               leaf, row, hasFocus);
	            
	            //The value object is nothing but the DefaultMutableTreeNode.
	            DefaultMutableTreeNode node = (DefaultMutableTreeNode)value;
	            holdNode = node.toString();
	            //System.out.println("holdNode "+holdNode);
	            boolean isCut = ((treeNode) node.getUserObject()).Cut;
	            checkNodeNo = ((treeNode) node.getUserObject()).NodeNo;
	            nodeContainsCut = false;
	            //nodeContainsCut();
	            nodeExistsInTable();
	        	if (editButton && nodeContainsCut)
	            { 
	        		readNodeTable();
	        		selection = true;
	        		 super.getTreeCellRendererComponent(tree, value, selection, expanded, 
                             leaf, row, hasFocus);

	        		 //The value object is nothing but the DefaultMutableTreeNode.
	        	 DefaultMutableTreeNode anode = (DefaultMutableTreeNode)value;
	        	 Object obj = ((treeNode) node.getUserObject());
	        	 if (checkNodeNo == 1)
	        	 {
	        	 setTextSelectionColor(Color.blue);
	        	 cutIndex = 0;
	        	 indexOfUserObject(anode,obj);
	        	 }
	        	 else
	        	 {
	        		 setTextSelectionColor(Color.yellow);
	        	 cutIndex = 0;
	        	 indexOfUserObject(anode,obj); 
	        	 }
	            }
	            return this;
	        }
	           
   }
   public void readNodeTable() {
	   for (int n = 0 ; n < 8 ; n++ ){
		   boolean showNode = cutNodeNoTable[n];
		   //System.out.println("showNode "+showNode);
	   }
	
	   
   }
   
   public void nodeExistsInTable() {
	  boolean checkNode = cutNodeNoTable[ checkNodeNo];
	  if (checkNode == false || checkNode == true)
	  {
		  nodeContainsCut = true;
	  }
   }
   public void nodeContainsCut(){
	   String bb = "CUT";
		 if (holdNode.indexOf(bb) != -1) {
			 nodeContainsCut = true;
		 }
   }
   public void getChildren()
   {
	   
	
	  
   }
   /** 
    * setIconAndToolTip 
    * This method checks the userobject and appropiately sets the icons and tooltip
    *    
    * @return void
    * @exception 
    */
    private void setIconAndToolTip(Object obj, JTree tree)
    {
    	boolean isCut = ((treeNode) node.getUserObject()).Cut;
    	if (isCut)
        {
    		System.out.println("obj "+obj);
    		 setIcon(CUT_IMAGE);
        }
         
    }
  
  
    class MyTreeModelListener implements TreeModelListener {
        public void treeNodesChanged(TreeModelEvent e) {
        	
            DefaultMutableTreeNode node;
            node = (DefaultMutableTreeNode)
                     (e.getTreePath().getLastPathComponent());
            //System.out.println("node ");

            /*
             * If the event lists children, then the changed
             * node is the child of the node we've already
             * gotten.  Otherwise, the changed node and the
             * specified node are the same.
             */
            try {
                int index = e.getChildIndices()[0];
                node = (DefaultMutableTreeNode)
                       (node.getChildAt(index));
            } catch (NullPointerException exc) {}

            System.out.println("The user has finished editing the node.");
            System.out.println("New value: " + node.getUserObject());
        }
        public void treeNodesInserted(TreeModelEvent e) {
        }
        public void treeNodesRemoved(TreeModelEvent e) {
        }
        public void treeStructureChanged(TreeModelEvent e) {
        }
      
    }
    class PopupListener extends MouseAdapter {
    	public void mousePressed(MouseEvent e) {
    		maybeShowPopup(e);
    	}
    	public void maybeShowPopup(MouseEvent e) {
    		if (e.isPopupTrigger()) {
    			popup.show(e.getComponent(),e.getX(),e.getY());
    		}
    	}
    }
    class PopupTrigger extends MouseAdapter
    {
      public void mouseReleased(MouseEvent e)
      {
        if (e.isPopupTrigger())
        {
          int x = e.getX();
          int y = e.getY();
          TreePath path = tree.getPathForLocation(x, y);
          //if (path != null)
         // {
          
            popup.show(tree, x, y);
            
          //}
        }
      }
    }
}

