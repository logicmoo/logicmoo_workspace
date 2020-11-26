package RegulusGUI;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyVetoException;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.JInternalFrame;
import javax.swing.JInternalFrame.JDesktopIcon;
import java.awt.Graphics;
import javax.swing.tree.DefaultMutableTreeNode;


public class TreePane extends JFrame
								implements ActionListener,InternalFrameListener,MouseListener{
	
	private RegulusGUI regulusWindow = null;
	private Frame2     frame2       = null;
	private JInternalFrame treePane = null;
	private GridBagLayout gblayout;
	private GridBagConstraints gbConstraints;
	public  JButton rule = new JButton("Rule");
	private JButton cut = new JButton("Cut");
	private RegulusSummaryItem regulusSummaryItem; 
	private String cat = "";
	private String tag;
	private String sem = "";
	private String features = "";
	private String form = "";
	public JTextArea text = new JTextArea(4, 15);
	private JScrollPane scroller;
	private String TreeCat = "";
	private int    nodeNumber = 0;
	private String file = "";
	private int startLine = 0;
	private int endLine = 0;
    private String getDaughters = "";
	private DynamicTree treePanel;
	private EditDynamicTree editPanel;
	private String elementNo;
	private String CutcommandPart1 = "CUT ";
	private String CutcommandPart2 = "";
	private String CutcommandPart3 = "";
	private String CutcommandFull = "";
	private String RulecommandPart1 = "RULE ";
	private String RulecommandPart2 = "";
	private String RulecommandPart3 = "";
	private String RulecommandFull = "";
	private String ShowcommandPart1 = "SHOW ";
	private String ShowcommandPart2 = "";
	private String ShowcommandPart3 = "";
	private String ShowcommandFull = "";
	public JInternalFrame[] allFrames = null;
	private int count = 0;
	private String formatString = "";
	private int len = 0;
	private char[] tempCharArray;
	private String[][] myLetterArray;
	private String [] myOutputArray;
	private int result = 0;
	private int charIndex = 0;
	private int columnIndex = 0;
	private int setChar = 0;
	private int rowIndex = 0;
	private String Rule = "";
	private int fromIndex = 0;
	private int toIndex = 0;
	private String output = "";
	private JTextArea outputArea;
	private String summaryString = "";
	private String hold3 = "";
	private boolean editButton = false;
	private boolean editAndCut = false;
	public boolean treebuilt = false;
	private JTextArea RuleText = new JTextArea(4, 15);
	private JButton treebtn =     new JButton("Tree");
	private int treeCount = 0;
	private JInternalFrame[] frames = null;
	private int frameIndex = 0;
    private int holdFrameIndex = 0;
    private String strName = "";
    private boolean   remove_finished = false;
    private Component c = null;
    private String ac = "";
    //private JPopupMenu popup = new JPopupMenu();
   
	 // send name of internal frame 
	  public JInternalFrame getInternalFrame() {
		  return treePane;
	  }
	
//	 get pointer to Regulus window
	  public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	  }
	  
	  // set the pointer to the Regulus window
	  public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	  }
	  
//  get the pointer to frame2 window
	  public Frame2 getFrame2() {
		  return frame2;
	  }
//	 set the pointer to the Frame2 window
	  public void setFrame2(Frame2 window) {
		  frame2 = window;
	  }

	  public TreePane() {
		  
	  }
	  
	public TreePane(Frame2 frame, RegulusGUI regulusgui,String label,String elementno, boolean editBtn,JButton Treebtn, int treeCounter) {
	  String myLabel = label + " " + elementno; 
	  treebtn = Treebtn;
	  treeCount = treeCounter;
	  treebtn.addMouseListener(this);
	  treePane = new JInternalFrame(myLabel,true,true,true,true);
	  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
	  elementNo = elementno;
	  editButton = editBtn;
	  Container c2 = treePane.getContentPane();
	  treePane.addInternalFrameListener(this);
		
	  setFrame2(frame);
	  setRegulusGUI(regulusgui);
	  regulusSummaryItem = regulusWindow.getStepperItem(frame2.Treeval);
	 
	  // set up the layout
	  gblayout = new GridBagLayout();
	 
	  c2.setLayout(gblayout);
	  // instansiate the gridbag constraints
	  gbConstraints = new GridBagConstraints();
	  gbConstraints.fill = GridBagConstraints.HORIZONTAL;
	  
//  Create components
	  treePanel = new DynamicTree(regulusSummaryItem.getTree(),editButton,this);
	
	  
	  //tree.validate();
	 
	  treePanel.countDaughters = treePanel.countDaughters * 4;
	  
	  
	  

//		 Scroll Area
	  JSplitPane vpane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, treePanel.scrollPane, RuleText);
	  text.setToolTipText("Regulus tree Items");
	  gbConstraints.weightx = 1.5;
	  gbConstraints.weighty = 1.5;
	  gbConstraints.gridwidth = GridBagConstraints.REMAINDER;
	  gbConstraints.gridheight = GridBagConstraints.REMAINDER;
	  gbConstraints.gridx = 0;
	  gbConstraints.gridy = 0;
	  gbConstraints.fill = GridBagConstraints.BOTH;
	  //c2.add(vpane,gbConstraints);
	  c2.add(treePanel.scrollPane,gbConstraints);
	 
	 
	  
	  GetregulusSummaryItem();
	  text.append((cat) + "\n");
	  text.append((sem)+ "\n");
	  text.append((features)+ "\n");
	  text.append((form)+ "\n");
	 
	  
	 
	  treePanel.populateTreeFromRegulusTree(regulusSummaryItem.getTree());
	  treePanel.iterateTree();
	  treePane.setSize(450,650);
	 
	}
	
		public void rightClickOnTree(MouseEvent e) {
			
			JPopupMenu popup = new JPopupMenu();
			JMenuItem menuItemOne;
 	        menuItemOne = new JMenuItem("Rule");
 	    	popup.add(menuItemOne);
 	    	menuItemOne.addActionListener(al);
 	    	JMenuItem menuItemTwo;
 	        menuItemTwo = new JMenuItem("Semantic rep");
 	    	popup.add(menuItemTwo);
 	    	menuItemTwo.addActionListener(al);
 	    	JMenuItem menuItemThree;
 	        menuItemThree = new JMenuItem("Cut");
 	    	popup.add(menuItemThree);
 	    	menuItemThree.addActionListener(al);
 	    	treePanel.scrollPane.add( popup);
 	    	 popup.show( e.getComponent(),
                     e.getX(), e.getY() );   
 	    	
 		}
		  // define a MouseListener for the window that displays
	      // a JPopupMenu when the popup trigger event occurs
	    
		ActionListener al = new ActionListener() {
		      public void actionPerformed(ActionEvent e) {  
		    	  
		    	  String command = e.getActionCommand();
		    	  
		    	  if (command.equals("Rule")) {
		  	        // System.out.println("Rule");
		  	         doRuleCommand(); 
		              } else if (command.equals("Semantic rep")) {
		            	  //System.out.println("Semantic rep");
		            	  doSemanticRepCommand(); 
		                  // perform copy operation
		              } else if (command.equals("Cut")) {
		            	  System.out.println("Cut");
		                  // perform paste operation
		            	  doCutCommand();
		              }
		      }
		    };
		    

		public void doRuleCommand() {
			String Element = getelementno();
  			int oldElementNo = Integer.parseInt(Element);
			int newElementNo = regulusWindow.newStepperIdCorrespondingToOldOne(oldElementNo);
			Element = new Integer(newElementNo).toString();
  			String node = getNodeNo();
  			//System.out.println("node in rule "+node);
  			int itemNumber;
  			int nodeNumber;
  			RulecommandPart2 = Element;
  			RulecommandPart3 = node;
  			RulecommandFull = RulecommandPart1 + RulecommandPart2 +" " +RulecommandPart3;
  			//System.out.println("RulecommandFull "+RulecommandFull);
  			regulusWindow.handleCommand(RulecommandFull);
  			String command = regulusWindow.getCommandErrorString();
  			if (regulusWindow.regulus_command_succeeded == false)
  			{
	  				JOptionPane.showMessageDialog(null, command
  						,"Message for Rule",JOptionPane.INFORMATION_MESSAGE);
  			}
  			else
  			{
  				
  			itemNumber = Integer.parseInt(Element);
  			nodeNumber = Integer.parseInt(node);
  			Rule = regulusWindow.getStepperItemRule(itemNumber,nodeNumber);
  			//System.out.println("Rule "+Rule  );
  			formatString = Rule;
//  		 display the Rule
  			JOptionPane.showMessageDialog(null,Rule,"Show Rule",JOptionPane.PLAIN_MESSAGE);
  			
  			if (regulusWindow.regulus_command_succeeded == false)
  				JOptionPane.showMessageDialog(null, command
  						,"Message for Rule",JOptionPane.INFORMATION_MESSAGE);
  			}
  			
  	}
		public void doSemanticRepCommand() {
			String Element = getelementno();
  			int oldElementNo = Integer.parseInt(Element);
			int newElementNo = regulusWindow.newStepperIdCorrespondingToOldOne(oldElementNo);
			Element = new Integer(newElementNo).toString();
  			String node = getNodeNo();
  			int itemNumber;
  			int nodeNumber;
  			RegulusSummaryItem summaryItem;
  			ShowcommandPart2 = Element;
  			ShowcommandPart3 = node;
  			ShowcommandFull = ShowcommandPart1 + ShowcommandPart2 +" " +ShowcommandPart3;
  			System.out.println("ShowcommandFull "+ShowcommandFull);
   			regulusWindow.handleCommand(ShowcommandFull);
   			String command = regulusWindow.getCommandErrorString();
  			if (regulusWindow.regulus_command_succeeded == false)
  			{
  				JOptionPane.showMessageDialog(null, command
  						,"Message for Show",JOptionPane.INFORMATION_MESSAGE);
  			}
  			else
  			{
  			itemNumber = Integer.parseInt(Element);
  			nodeNumber = Integer.parseInt(node);
  			summaryItem = regulusWindow.getStepperItemNode(itemNumber,nodeNumber);
  			summaryString = summaryItem.toStringNoTree();
  			TakeAwayTagInSummaryString();
  			// display the Rule
  			JOptionPane.showMessageDialog(null, hold3 ,"Semantic Representation",JOptionPane.PLAIN_MESSAGE);
  			
  			if (regulusWindow.regulus_command_succeeded == false)
  				JOptionPane.showMessageDialog(null, command
  						,"Message for Show",JOptionPane.INFORMATION_MESSAGE);
  			}
  	}
  		public void doCutCommand() {
  			if (editButton)
  			{
  				System.out.println("editButton true "+editButton);
  				editAndCut = true;
  			}
  			else
  			{
  				System.out.println("editButton false "+editButton);
  				editAndCut = false;
  			}
  			String Element = getelementno();
  			int oldElementNo = Integer.parseInt(Element);
			int newElementNo = regulusWindow.newStepperIdCorrespondingToOldOne(oldElementNo);
			Element = new Integer(newElementNo).toString();
  			String node = getNodeNo();
  			CutcommandPart2 = Element;
  			CutcommandPart3 = node;
  			CutcommandFull = CutcommandPart1 + CutcommandPart2 +" " +CutcommandPart3;
  			regulusWindow.handleCommand(CutcommandFull);
  			String command = regulusWindow.getCommandErrorString();
  			if (regulusWindow.regulus_command_succeeded)
  				JOptionPane.showMessageDialog(null, "Cut was successful "
  						,"Message for Cut",JOptionPane.INFORMATION_MESSAGE);
  			if (regulusWindow.regulus_command_succeeded == false)
  				JOptionPane.showMessageDialog(null, command
  						,"Message for Cut",JOptionPane.INFORMATION_MESSAGE);
  			frame2.getValueTable();
  			frame2.setfield();
  			if (editAndCut)
  			{
  				frame2.CreateAndLinktree("Tree",elementNo);
  			}
  		}
  		
  
   		
 	   public void internalFrameClosing(InternalFrameEvent e) {
		   remove_finished = false;
		   lookForIcon();
   		//System.out.println("Internal frame closing");
   	    }

   	    public void internalFrameClosed(InternalFrameEvent e) {
   		
   	    }

   	    public void internalFrameOpened(InternalFrameEvent e) {
   		
   	    }

   	    public void internalFrameIconified(InternalFrameEvent e) {
   	    	getIconFrame();
	    	remove_finished = false;
	    	lookForIcon();
    	 }

   	    public void internalFrameDeiconified(InternalFrameEvent e) {
   	    	getselectedFrame();
   	    	treebtn =     new JButton(strName);
   	    	regulusWindow.barPanel.add(treebtn);
   	    	treebtn.addMouseListener(this);
   	    	getselectedFrame();
   	    }

   	    public void internalFrameActivated(InternalFrameEvent e) {
   	    	
   	    }

   	    public void internalFrameDeactivated(InternalFrameEvent e) {
   		
   	    }
   	    
   	    public void mouseClicked(MouseEvent e)
   	    {
   	     SwingUtilities.isRightMouseButton(e);
   	   
   	    {
   	       // it is right mouse click
   	      
   	    }
   	    }
   	    public void mousePressed(MouseEvent e)
   	    {
   	    	ac = treebtn.getActionCommand();
 	    	getAllFrames();
   	    }
   	    
   	  public void mouseReleased(MouseEvent e)
	    {
	    }
	    public void mouseEntered(MouseEvent e)
	    {
	    	//System.out.println("mouse entered");
	    	
	    }
	    public void mouseExited(MouseEvent e)
	    {
	    } 
	    
	    public void getIconFrame(){
 	    	 frames = regulusWindow.desktop.getAllFrames();
 	    	 frameIndex = 0;
 	         int countFrames = frames.length;
 	         for (int i = 0; i < countFrames; i++) {
  	          String strFrames = frames[i].toString();
   	         holdFrameIndex = i;
 	         String b = "Tree";
  			 if (strFrames.indexOf(b) != -1) {
  				 
    				if (frames[i].isIcon())
    				{
    					strName = frames[i].getTitle();
     				}
    			 
  			}
 	      }
 	  }	 
	    
		public void lookForIcon() {
			for (int i = 0; remove_finished == false ; i ++)  {
				 c = regulusWindow. barPanel.getComponent(i);
				 String strname = c.toString();
				 String b = strName;
				 b = "Tree";
				 //System.out.println("strName "+strName);
				 if (strname.indexOf(b) != -1) {
					 regulusWindow.barPanel.remove(regulusWindow.barPanel.getComponent(i));	
					 regulusWindow.barPanel.repaint();
					 remove_finished = true;
				 }
				 else
				 {
					
					 
				 }
			 }
		}
		
		  public void getAllFrames(){
 	    	 frames = regulusWindow.desktop.getAllFrames();
 	    	 frameIndex = 0;
 	         int countFrames = frames.length;
 	         for (int i = 0; i < countFrames; i++) {
  	          String strFrames = frames[i].toString();
   	         holdFrameIndex = i;
 	         String b = ac;
  			 if (strFrames.indexOf(b) != -1) {
    				try {
    					frames[i].setSelected(true);
    				
  			  } catch (Exception e) {
  	             // This should not happen
  	             e.printStackTrace();
  	         }
    			 
  			}
 	      }
 		}
		  
		   public void getselectedFrame(){
	   	    	 frames = regulusWindow.desktop.getAllFrames();
	   	    	 frameIndex = 0;
	   	         int countFrames = frames.length;
	   	         for (int i = 0; i < countFrames; i++) {
	    	          String strFrames = frames[i].toString();
	     	         holdFrameIndex = i;
	   	         String b = "Stepper";
	    			 if (strFrames.indexOf(b) != -1) {
	    				 
	      				if (frames[i].isSelected())
	      				{
	      					strName = frames[i].getTitle();
	      					//System.out.println("strName "+strName);
	      				}
	      			 
	    			}
	   	      }
	   	  }	  
   	    
 	public void TakeAwayTagInSummaryString()
	{
		int length = summaryString.length();
		String b = "Tag";
			if (summaryString.indexOf(b) != -1) {
			int startPos = summaryString.indexOf(b);
			int endPos = startPos + 12;
			String Trace = summaryString.substring(startPos,endPos);
			String hold1 = summaryString.substring(0,startPos);
			String hold2 = summaryString.substring(endPos,length);
			hold3 = hold1 + hold2;
			}
	}
	public void createStringTools()
	{
		len = formatString .length();
		result = len / 60;
		myOutputArray = new String[result];
		outputArea = new JTextArea(result,60);
		scroller = new JScrollPane(outputArea);
		toIndex = 60;
		createTableFromSubstring();
		createoutput();
	}
	public void createTableFromSubstring()
	{
		for (rowIndex = 0; rowIndex < result; rowIndex  ++)
		{
			myOutputArray[rowIndex] =  formatString.substring(fromIndex,toIndex);
			fromIndex = fromIndex +60;
			toIndex = toIndex + 60;
		}
	}
	public void createoutput()
	{
		for (rowIndex = 0; rowIndex < result; rowIndex  ++)
		{
			output +=
				"\"" + myOutputArray[rowIndex].toString()+"\n";
			
		}
		outputArea.setText(output);
		JOptionPane.showMessageDialog(null,scroller,"The Rule is",JOptionPane.INFORMATION_MESSAGE);
	}
	
	
	public String getelementno()
	{
		return elementNo;
	}
	
	public String getNodeNo()
	{
		String nodeno = treePanel.getNode(); 
		return nodeno;
		
	}
	public void GetregulusSummaryItem()
	  {
		  cat = regulusSummaryItem.getCat(); 
		  tag = regulusSummaryItem.getTag();
		  sem = regulusSummaryItem.getSem();
		  features = regulusSummaryItem.getFeatures();
		  form = regulusSummaryItem.getForm();
	  }
	  
		 
	  public void populateTree(DynamicTree treePanel){
		  String txtstartLine;
		  String txtendLine;
		  String txtnodeNumber;
		  String p1Name = new String(TreeCat);
		  txtnodeNumber = Integer.toString(nodeNumber);
		  txtstartLine =  Integer.toString(startLine);
		  txtendLine = Integer.toString(endLine);
		  String c1Name = new String("Node " + txtnodeNumber);
		  String c2Name = new String(file);
		  String c3Name = new String("StartLine " + txtstartLine);
		  String c4Name = new String("EndLine " + txtendLine);
		  String c5Name = new String(getDaughters);
		 DefaultMutableTreeNode p1;
		
		p1 = treePanel.addObject(null,p1Name);
		
		treePanel.addObject(p1,c1Name);
		treePanel.addObject(p1,c2Name);
		treePanel.addObject(p1,c3Name);
		treePanel.addObject(p1,c4Name);
		treePanel.addObject(p1,c5Name);
	  }
	  public void actionPerformed(ActionEvent e) {
		  
	  }
}

class MyTreeModelListener implements TreeModelListener {
    public void treeNodesChanged(TreeModelEvent e) {
        DefaultMutableTreeNode node;
        node = (DefaultMutableTreeNode)
                 (e.getTreePath().getLastPathComponent());

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


