package RegulusGUI;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.JInternalFrame;
import javax.swing.JInternalFrame.JDesktopIcon;

import javax.swing.tree.DefaultMutableTreeNode;
import se.sics.prologbeans.PBString;

public class CorpusPane extends JFrame
			implements  VetoableChangeListener{

private RegulusGUI regulusWindow = null;
private Frame3     frame3       = null;
private CorpusPane corpuspane = null;
private JudgePane judgePane = null;
private JInternalFrame corpusPane = null;
private GridBagLayout gblayout;
private GridBagConstraints gbConstraints;

private JTextArea corpustext = new JTextArea(13, 50);
private JTextArea corpusnumbers = new JTextArea(13, 50);
private JTextArea corpusfiles = new JTextArea(13, 50);
private int startind = 0;
private int endind = 1;
private int startpindex = 0;
private int endpindex = 0;
private boolean sign_found = false; 
private int subtractIndexStart = 0;
private int subtractIndexEnd = 0;
int i = 0;
int j = 0;
int length = 0;
private int saveIndex = 0;
private int savefileIndex = 0;
private int startPos = 0;
private int endPos = 0;
private JMenuBar bar = new JMenuBar();  // create menubar
private JMenu viewMenu;
private JPanel inputPanel = new JPanel(new BorderLayout());
private JPanel displayPanel = new JPanel();
private JCheckBoxMenuItem[] DisplayCheckBoxes = new JCheckBoxMenuItem[2];
private int index = 0;
private int temp = 0;
private final String[] DisplayLabelText = {"Corpus Percentage","Corpus Files;"};
private JScrollPane[] DisplayScrollPanes = new JScrollPane[2];
private JLabel percentage = new JLabel("Percentage");
private JLabel corpus  = new JLabel("Corpus Output");
private JLabel files = new JLabel("Corpus Files");
private JLabel DisplayLabels[] = {percentage,corpus,files };
private Font DISPLAY_PANE_FONT = new Font("Monospaced",Font.PLAIN,14);
private JTextArea[] DisplayTextPanes = new JTextArea[2];
public  JMenuItem J_judgeMenu;
public  JMenu 	judgeMenu;
private String HoldStringCorpus = "";
private JButton buttonThree;
		
// send name of internal frame 
public JInternalFrame getInternalFrame() {
	  return corpusPane;
}

//get pointer to Regulus window
public RegulusGUI getRegulusGUI() {
	  return regulusWindow;
}

// set the pointer to the Regulus window
public void setRegulusGUI(RegulusGUI window) {
	  regulusWindow = window;
}
//set the pointer to the Frame2 window
public void setFrame3(Frame3 window) {
	  frame3 = window;
}
public CorpusPane() {
	  
}
public CorpusPane(Frame3 frame, RegulusGUI regulusgui , String CorpusIdent) {
	
	  setJMenuBar( bar );  // set the menubar for the JInternalFrame	
	   
	  corpusPane = new JInternalFrame("Corpus output",true,true,true,true);
	  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
	  Container c2 = corpusPane.getContentPane();
	  setFrame3(frame);
	  setRegulusGUI(regulusgui);
	  HoldStringCorpus = CorpusIdent;
	  
		 
	  // set up the layout
	  gblayout = new GridBagLayout();
	  displayPanel.setLayout(gblayout);
	  
	  // instansiate the gridbag constraints
	  gbConstraints = new GridBagConstraints();
	  gbConstraints.fill = GridBagConstraints.HORIZONTAL;
	  
	  viewMenu = new JMenu("View");
	  viewMenu.setMnemonic('V');
	  bar.add(viewMenu);
	  
	  //  create Judge  menu item and sub menu items
		 
		judgeMenu = new JMenu( "Judge" ); 
		judgeMenu.setMnemonic( 'J');
	    J_judgeMenu = new JMenuItem( "Do Judging" );
	    J_judgeMenu.setToolTipText("Judge Corpus output");
	    J_judgeMenu.addActionListener(
	    		new ActionListener() {
	    			public void actionPerformed( ActionEvent e)
	    			{
	    				//fill in what happens when you click on this
	    				CreateAndLinkjudgePane();
	        			
	      			}
	    		}
	    		);
	    
	    judgeMenu.add(J_judgeMenu);
	    bar.add(judgeMenu);
	  
//	Add labels and textboxes
	  
	  createTestArea();
	  createPercentageList();
	  createCorpusFileListStartandEndIndex();
	  createCorpusFileList();
	  
	  corpusPane.addVetoableChangeListener(this);
	  inputPanel.add(bar,BorderLayout.NORTH);
	  c2.add(inputPanel,BorderLayout.NORTH  );
	  c2.add(displayPanel);
	  
	  corpusPane.pack();
	  //corpusPane.setLocation(430,8);
}
public void CreateAndLinkjudgePane()
{
		regulusWindow.judgeCounter++; 
		buttonThree = new JButton("Judge " +regulusWindow.judgeCounter);
		regulusWindow.barPanel.add( buttonThree);
		
	    JudgePane judgePane = new JudgePane(corpuspane, regulusWindow,HoldStringCorpus,buttonThree,regulusWindow.judgeCounter );
	  	judgePane.setRegulusGUI(regulusWindow );
	  //  display new internal window
	    JInternalFrame JudgePaneInternalFrame = judgePane.getInternalFrame();
	  //add the internal frame to the desktop
		regulusWindow.desktop.add(JudgePaneInternalFrame);
		JudgePaneInternalFrame.setVisible(true); 
}
public void createPercentageList()
{
	// initialize the indexes 
	startind = 0;
	endind = 1;
	length = regulusWindow.getResult.length();
	for (j = 1 ; j < regulusWindow.getResult.length() ; j++ )
	 {
	// reset the boolean switch  
		 sign_found = false; 
	// Put corpus data in a table
	 for (i = 1 ; sign_found == false && i < regulusWindow.getResult.length() ; i++ )
	 {
		 
		 if (regulusWindow.getResult.substring(startind,endind).equals("#"))
		 {
			sign_found = true; 
			// check if you get rid of # signs like this
			startpindex = startind + 2;
			endpindex = startind + 30;
			// add one to startind and end ind so you can se next sign
			addOneToInd();
			// create output
			createPercetageString();
			
		 }
		 else
		 {
			 addOneToInd();
		 }
		 
	 }
	
 }
	
}

public void onlyOutputData()
{
	int tempstartindex = startpindex + 1;
	int secondTempStartIndex = startpindex + 2;
	int tempendindex = endpindex - 1;
	
	if (regulusWindow.getResult.substring(startpindex,tempstartindex).equals("#"))
	{
		startpindex = startpindex + 1;
	}
	if (regulusWindow.getResult.substring(tempstartindex,secondTempStartIndex).equals("#"))
	{
		startpindex = startpindex + 2;
	}
	if (regulusWindow.getResult.substring(tempendindex,endpindex).equals("#"))
	{
		endpindex = endpindex - 1;
	}
	if (regulusWindow.getResult.substring(startpindex,endpindex).contains("Processing"))
	{
		//System.out.println("startpindex "+startpindex);
		//System.out.println("endpindex "+endpindex);
		endpindex = endpindex - 28;
		//System.out.println("endpindex "+endpindex);
	   // System.out.println(regulusWindow.getResult.substring(startpindex,endpindex));
	}
}
public void createPercetageString()
{
	// before you write out to textfield check that this is not a row of #######
	int tempendindex = 27 + startind;
	if (regulusWindow.getResult.substring(startind,tempendindex).equals("###########################"))
	{
		startind = startind +27;
		endind = endind + 27;
	}
	else
	{
		 createPercentageOutput();
	}
}
 
public void createPercentageOutput()
{
	subtractIndexStart = endpindex - 4;
	subtractIndexEnd = subtractIndexStart + 1;	
	checkLastChar();
	// write out to textbox
	onlyOutputData();
	DisplayTextPanes[0].append(regulusWindow.getResult.substring(startpindex,endpindex));
    DisplayTextPanes[0].setCaretPosition(0);
	// add to index so to start reading on the next line
	startind = startind + 26;
	endind = endind + 26;
}

public void addOneToInd()
{
	 if (endind < length)
	 {
		 startind = startind + 1;
		 endind = endind + 1;
	 }
	 else
	 {
		 j = length;
		 i = length;
	 }
}


// check which position is the end postion for each line
public void checkLastChar()
{
	for (int n = 1 ; n < 4 ; n++ )
	{
	if (regulusWindow.getResult.substring(subtractIndexStart,subtractIndexEnd).equals("#"))
	{
		endpindex = subtractIndexEnd - 1;
	}
	else
	{
		subtractIndexStart = subtractIndexStart + 1;
		subtractIndexEnd = subtractIndexEnd + 1;
	}
	}
}
public void createCorpusListEndIndex()
{
	length = regulusWindow.getResult.length();
	startind = 0;
	endind = 1;
	sign_found = false;
	
	for (i = 1 ; sign_found == false && i < regulusWindow.getResult.length() ; i++ )
	 {
		 
		 if (regulusWindow.getResult.substring(startind,endind).equals("#"))
		 {
			sign_found = true; 
			System.out.println("i "+i);
			saveIndex = i;
			saveIndex = saveIndex - 1;
			i = length;
		 }
		 else
		 {
			 addOneToInd();
			
		 }
		 
	 }
	
}
public void createCorpusList()
{
	//DisplayTextPanes[1].append(regulusWindow.getResult.substring(0,saveIndex));
    //DisplayTextPanes[1].setCaretPosition(0);
}

public void createCorpusFileListStartandEndIndex()
{
	String b = "Processing finished";
	endPos = regulusWindow.getResult.length();
	if (regulusWindow.getResult.indexOf(b) != -1) {
		startPos = regulusWindow.getResult.indexOf(b);
		startPos = startPos + 30;
		//System.out.println("startPos "+startPos);
	    //System.out.println("String a contains String b, starting at position " + regulusWindow.getResult.indexOf(b));
	   } else {
	  //  System.out.println("String  does not contain String Processing finished");
	   }
}
public void createCorpusFileList()
{
	DisplayTextPanes[1].append(regulusWindow.getResult.substring(startPos,endPos));
    DisplayTextPanes[1].setCaretPosition(0);
}
public void createTestArea()
{
	  for (index = 0; index < 2; index++ ) 
	  {
		  createTest();
		  createCheckBoxes();
	  }
}
public void createTest()
{
	  // add label
	  temp = 3 + index;
	  gbConstraints.fill = GridBagConstraints.VERTICAL;
	  gbConstraints.gridx = 0;
	  gbConstraints.gridy = temp;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 1;
	  gbConstraints.weightx = 0;
	  gbConstraints.weighty = 0.7;
	  
	  displayPanel.add(DisplayLabels[index] ,gbConstraints);
	  

	  
	  // add textbox
	  DisplayTextPanes[index] = new JTextArea(11,104);
	  DisplayTextPanes[index].setFont(DISPLAY_PANE_FONT);
	  DisplayTextPanes[index].setEditable(false);
	  DisplayScrollPanes[index] = new JScrollPane(DisplayTextPanes[index]);
	  gbConstraints.fill = GridBagConstraints.BOTH;
	  gbConstraints.gridx = 1;
	  gbConstraints.gridy = temp;
	  gbConstraints.gridheight = 1;
	  gbConstraints.gridwidth = 10;
	  gbConstraints.weightx = 10;
	  gbConstraints.weighty = 0.7;
	  setAddArea();
	  
	  }
public void createCheckBoxes()
{
DisplayCheckBoxes[index] = new JCheckBoxMenuItem(DisplayLabelText[index],true);
DisplayCheckBoxes[index].addItemListener(new AnswerCheckBoxHandler());
viewMenu.add(DisplayCheckBoxes[index]);
}
public void setAddArea()
{
  displayPanel.add(DisplayScrollPanes[index],gbConstraints); 
}
public void vetoableChange(PropertyChangeEvent event)
throws PropertyVetoException {
 
JInternalFrame frame2 = (JInternalFrame) event.getSource();
String name = event.getPropertyName();
Object value = event.getNewValue();
int count = 0;

// we only want to check attempts to close a frame

if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
	int result = JOptionPane.showInternalConfirmDialog(frame2,
			" OK to close?");
	// if the user doesn't agree veto the close
	if (result == JOptionPane.CANCEL_OPTION)
		throw new PropertyVetoException("User cancelled close", event);
	else
	//count = aPerformed();
	//System.out.println("count "+count);
	// If in bidirectional mode, essential to call BIDIRECTIONAL_OFF to kill the
	// remote server and free up the port.
	regulusWindow.handleCommand("BIDIRECTIONAL_OFF");
	setVisible(false);
	dispose();
	return;
}

}
private class AnswerCheckBoxHandler implements ItemListener
{
	  public void itemStateChanged(ItemEvent event)
	  {
		  for (index = 0; index < 2; index++ ) {
			  if (event.getSource() == DisplayCheckBoxes[index]){
				  DisplayLabels[index].setVisible(DisplayCheckBoxes[index].isSelected());
				  DisplayScrollPanes[index].setVisible(DisplayCheckBoxes[index].isSelected());
			  }
		  }
			 
	  }
	
}  
}
