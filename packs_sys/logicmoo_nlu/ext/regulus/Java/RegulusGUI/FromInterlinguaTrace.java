package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.FilterReader;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.JInternalFrame;
import javax.swing.JInternalFrame.JDesktopIcon;
import java.io.*;
import java.util.*;
import javax.swing.text.html.*;
import java.applet.*;
import java.net.*;
import javax.swing.text.*;
import javax.swing.text.html.parser.*;
import javax.imageio.*;
import java.util.regex.*;

public class FromInterlinguaTrace extends JFrame implements  MouseListener,VetoableChangeListener,InternalFrameListener{

	private RegulusGUI regulusWindow = null;
	private Frame3     frame3       = null;
	private JInternalFrame frominterlinguatrace = null;
	private JInternalFrame[] allFrames = null;
	private JList summaryList ; 
	private DefaultListModel listModel;
	private String holdTextArea = "";
	private int index = 0;
	private String ruleString = "";
	private int minIndex = 0; 
    private int maxIndex = 0;
    private int showindex = 0;
    private Object thisElement;
    public  String Item = "";
    private int holdNumber = 0;
    private String holdString = "";
    private String selectedItem = "";
    private int ThisIsStartOfDigitPos = 0;
    private  boolean number_found = false;
    private String fileselectedItem = "";
    private String holdTraceFileFirst = "";
    private String holdTraceFileSecond = "";
    private String RealFile = "";
    private String HeaderFile = "";
    private String checkFile = "";
    private String record = null;
    private int startPos = 0;
    private int endPos = 0;
    private int fileindex = 0;
    private int length = 0;
    private String holdWhichRulesString = "";
    private boolean divider_found = false;
    private int ruleNumberOne = 0;
    private int ruleNumberTwo = 0;
    private  String holdFirstRuleNo = "";
    private  String holdSecondRuleNo = "";
    private int holdIntForSecondRuleNumber = 0;
    private JTextArea ruleTextArea;
    private String holdTrace = ""; 
    private int saveSentenceIndex = 0;
    private int holdIndex = 0;
    private String[] TraceSentenceTable = new String[50];
    private String[] TraceRuleTable = new String[50];
    private int twoFilesPos = 0;
    private String fileString = "";
    private String TraceHeaderOne = "";
    private String TraceFileOne = "";
    private String TraceHeaderTwo = "";
    private String TraceFileTwo = "";
    private int saveTableIndex = 0;
    private String[] SentenceTable = new String[50];
    private String[] RuleTable = new String[50];
    private String holdRuleSentence = "";
    private String holdPartOfRule = "";
    private boolean capital_letter_found = false;
    private boolean this_is_a_file = false;
    private int startRulePos = 0;
    private int endRulePos = 0;
    private int len = 0;
    private int ruleIndex = 0;
    private JInternalFrame[] frames = null;
    private int frameIndex = 0;
    private int holdFrameIndex = 0;
    private String strName = "";
    private boolean   remove_finished = false;
    private Component c = null;
    private String ac = "";
    private JButton fromInterlinguabtn =     new JButton("To interlingua Trace");
    private int fromInterlinguaCounter = 0;
    private boolean this_is_window_button = false;
    
	 // send name of internal frame 
	  public JInternalFrame getInternalFrame() {
		  return frominterlinguatrace;
	  }
	
//	 get pointer to Regulus window
	  public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	  }
	  
	  // set the pointer to the Regulus window
	  public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	  }
//	 set the pointer to the Frame3 window
	  public void setFrame3(Frame3 window) {
		  frame3 = window;
	  }
	  
public  FromInterlinguaTrace() {
		  
	  }

public FromInterlinguaTrace(Frame3 frame,CreateFrame3 createframe, RegulusGUI regulusgui,
		  String WhichTraceFile,int saveSentence, String[] TraceSentence,  String[] TraceRule,JButton Treebtn, int treeCounter ) {
	  
	  holdTrace = WhichTraceFile;
	  saveSentenceIndex = saveSentence;
	  fromInterlinguabtn = Treebtn;
	  fromInterlinguaCounter = treeCounter;
	  fromInterlinguabtn.addMouseListener(this);
	 
	  // move tables
	  
	 
	  for (int i = 0;TraceRule[i]!= null  ; i++)
	  {
		  TraceRuleTable[i] = TraceRule[i];
	  }
	  
	  createEmptyTable();
	  
	  for (int i = 0;TraceSentence[i]!= null  ; i++)
	  {
		  TraceSentenceTable[i] = TraceSentence[i];
		  saveTableIndex = i;
		
	  }
	  // create the table which holds info to be showed to user 
	  
	  ruleIndex = 0;
	  createOutput();
	  
	  frominterlinguatrace = new JInternalFrame("From Interlingua Trace",true,true,true,true);
	  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
	  
	  Container c2 = frominterlinguatrace.getContentPane();
	  setFrame3(frame);
	  setRegulusGUI(regulusgui);
	  frominterlinguatrace.addInternalFrameListener(this);
	  
//	 create the listmodel
	  listModel = new DefaultListModel();
	  listModel.addElement("Rule Window containing Rule No");
	  summaryList = new JList(listModel);
	  summaryList.addMouseListener(this);
	  countRulesTable();
	  createListModel();
	  readRulesTable();
	  listModel.addElement("                         ");
	  c2.add(summaryList,BorderLayout.CENTER );
	 
	  frominterlinguatrace.addVetoableChangeListener(this);
	  frominterlinguatrace.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	  frominterlinguatrace.setLocation(0,250);
	  setSize(30,30);
	  frominterlinguatrace.pack();
	  
}
public void createEmptyTable()
{
	  for (int i = 0;i < 20  ; i++) {
		  TraceSentenceTable[i] = null; 
	  }
}
public void createOutput() {
	  for (int i = 0;TraceSentenceTable[i]!= null  ; i++)
	  {
		  holdRuleSentence = TraceSentenceTable[i];
		  checkIfFile();
		  if (this_is_a_file == false)
		  {
		  checkCapitalLetter();
		  }
		  else
		  {
			  getFileFromTable();
			  getFileForOutput();
		  }
	  } 
}
public void checkCapitalLetter()
{
	  	//System.out.println("holdRuleSentence "+holdRuleSentence);
	  	len = holdRuleSentence.length();
		String[] tempStringArray = new String[len];
//		 put original string in an array of strings
		capital_letter_found = false;
		
		for (int ind = 0; capital_letter_found == false ; ind++) {
			 int ind2 = ind + 1;
			 tempStringArray[ind] = holdRuleSentence.substring(ind,ind2);
			 String holdChar = tempStringArray[ind];
		       if (holdChar.equals("[")) {
		    	  
		       }
		       else
		       {
		    	   if (holdChar.equals("A") || (holdChar.equals("B"))
		   		 ||  (holdChar.equals("C")) || (holdChar.equals("D"))
		   		 ||  (holdChar.equals("E")) || (holdChar.equals("F"))
		   		 ||  (holdChar.equals("G")) || (holdChar.equals("H"))
		   		 ||  (holdChar.equals("I")) || (holdChar.equals("J"))
		   		 ||  (holdChar.equals("K")) || (holdChar.equals("L"))
		   		 ||  (holdChar.equals("M")) || (holdChar.equals("N"))
		   		 ||  (holdChar.equals("O")) || (holdChar.equals("P"))
		   		 ||  (holdChar.equals("Q")) || (holdChar.equals("R"))
		   		 ||  (holdChar.equals("S")) || (holdChar.equals("T"))
		   		 ||  (holdChar.equals("U")) || (holdChar.equals("V"))
		   		 ||  (holdChar.equals("X")) || (holdChar.equals("Y"))
		   		 ||  (holdChar.equals("Z"))) {
		    	  endRulePos = ind;
		    	  endRulePos = endRulePos - 1;
		    	  capital_letter_found = true;
		    	  holdPartOfRule = holdRuleSentence.substring(0,endRulePos);
		    	  ruleIndex = ruleIndex + 1;
		    	  RuleTable[ruleIndex] = holdPartOfRule;
		    	 
		    	 }
		    	 
		  }
	 }
}


public void checkIfFile()
{
	  this_is_a_file = false;
	  String b = "c:/";
	  	if (holdRuleSentence.indexOf(b) != -1) {
	  		this_is_a_file = true;
	  	}
}
public void getItem()
{
	  minIndex = summaryList.getMinSelectionIndex();
    maxIndex = summaryList.getMaxSelectionIndex();
    for (int i = minIndex; i <= maxIndex; i++) {
      	if (summaryList.isSelectedIndex(i)) {
  		 showindex = i;
        	}
     	thisElement = listModel.get(showindex); 
      String holdSentence = thisElement.toString();
      Item = String.valueOf(showindex);
    }
}
public void getSelectedIndex()
{
	  holdNumber = Integer.parseInt(Item);
	  holdNumber = holdNumber - 1;
}
public void getSelectedItem(){
	  int length = TraceRuleTable[holdNumber].length();
	  length = length - 1;
	  holdString =  TraceRuleTable[holdNumber].substring(0,length);
	 // System.out.println("holdString "+holdString);
	  selectedItem = holdString; 
}
public void getFileNameFromRulesTable()
{
	  ThisIsStartOfDigitPos = 0;
	  number_found = false;
	  
	  for (int i = 0; number_found == false; i++)
	  {
		  //System.out.println("selectedItem "+selectedItem);  
		  if (Character.isDigit(selectedItem.charAt(i))){
			  ThisIsStartOfDigitPos = i;
			  i = i - 1;
			  fileselectedItem = selectedItem.substring(0,i);
			  //System.out.println("fileselectedItem "+fileselectedItem);
			  number_found = true;
		  }
	  }
	
}
public void getFileFromTable()
{
	  for (int index = 0;TraceSentenceTable[index] !=null  ; index++)
	  {
	  String b = "c:/";
	  
	  	if (TraceSentenceTable[index].indexOf(b) != -1) {
	  		holdTraceFileFirst = TraceSentenceTable[index];
	  		startPos = holdTraceFileFirst.indexOf(b);
	  		splitFileNameString();
	  		checkInterlinguaorDiscource();
	  		fileindex = index;
	  		
	  	}
	  }
}

public void splitFileNameString(){
	 // get the first file
		TraceHeaderOne = holdTraceFileFirst.substring(0,startPos); 
		 String c = ".pl";
			if (holdTraceFileFirst.indexOf(c) != -1) {
				twoFilesPos =holdTraceFileFirst.indexOf(c);
				twoFilesPos = twoFilesPos + 3;
				length = holdTraceFileFirst.length();
				String holdTrace = holdTraceFileFirst.substring(0,twoFilesPos);
				TraceFileOne = holdTraceFileFirst.substring(startPos,twoFilesPos);
				//System.out.println("TraceFileOne "+TraceFileOne);
				int lengthTraceWhole =holdTraceFileFirst.length(); 
				int lengthTraceOne = holdTrace.length();
			
				// Check if there are two files to be read
				if (lengthTraceWhole > lengthTraceOne)
				{
					int startFileTwo = twoFilesPos + 1;
					String holdTraceFileSecond = holdTraceFileFirst.substring(startFileTwo,lengthTraceWhole);
					  String b = "c:/";
					  	if (holdTraceFileSecond.indexOf(b) != -1) {
					  		startPos = holdTraceFileSecond.indexOf(b);
					  		int lenthTraceTwo = holdTraceFileSecond.length();
					  		TraceFileTwo = holdTraceFileSecond.substring(startPos,lenthTraceTwo); 
					  	}
							 
						b = ":";
						if (holdTraceFileSecond.indexOf(b) != -1) {
							endPos = holdTraceFileSecond.indexOf(b);
							TraceHeaderTwo = holdTraceFileSecond.substring(0,endPos ); 
						}
				}
			}
}
public void checkInterlinguaorDiscource()
{
	  if (holdTrace.equals("Discourse") );
	  {
		  checkfileDiscourse();
	  }
	  if (holdTrace.equals("Interlingua") );
	  {
		  checkfileInterlingua();
	  }
}
public void checkfileInterlingua()
{
	 String b = "IN";
	 if (HeaderFile.indexOf(b) != -1) {
		 startPos = HeaderFile.indexOf(b);
		 int Headerlength = HeaderFile.length();
		 Headerlength = Headerlength - 2;
		
		 checkFile =  HeaderFile.substring(startPos,Headerlength);
	 }
	
}

public void checkfileDiscourse()
{
	 // System.out.println("checkfileDiscourse");
	 String b = "SOURCE";
	 if (HeaderFile.indexOf(b) != -1) {
		 startPos = HeaderFile.indexOf(b);
		 startPos = startPos - 11;
		 int Headerlength = HeaderFile.length();
		 Headerlength = Headerlength - 2;
		 checkFile =  HeaderFile.substring(startPos,Headerlength);
	 }
	
}

public void getRuleNumbers()
{
	 int holdLength = selectedItem.length();
	 holdWhichRulesString =  selectedItem.substring(ThisIsStartOfDigitPos,holdLength);
	 divider_found = false;  
	 getFromNumber();
	 getToNumber ();
//	 Integer i = Integer.valueOf("13245");
	  ruleNumberOne = Integer.valueOf(holdFirstRuleNo);
	  ruleNumberTwo = Integer.valueOf(holdSecondRuleNo);
	 
}
public void getFromNumber()
{
	  for (int i = 0; divider_found == false; i++)
	  {
		  if (Character.isDigit(holdWhichRulesString.charAt(i))){
			  divider_found = false;  
		  }
		  else
		  {
			  holdIntForSecondRuleNumber = i;
			  holdFirstRuleNo = holdWhichRulesString.substring(0,i);
			  divider_found = true;
		  }
	  }
}
public void getToNumber ()
{
	  int holdRuleLength = holdWhichRulesString.length();
	  holdIntForSecondRuleNumber = holdIntForSecondRuleNumber + 1;
	  holdSecondRuleNo = holdWhichRulesString.substring(holdIntForSecondRuleNumber,holdRuleLength);
}
public void getRightFile()
{
	  // check which file to read
	
	  if (TraceHeaderTwo.equals(fileselectedItem))
	  {
		  RealFile = TraceFileTwo;
		  ReadRulesFile();
		  JOptionPane.showMessageDialog(null,ruleTextArea,"Show Rule",JOptionPane.PLAIN_MESSAGE);
	  }
	  else 
	  {
		  //System.out.println("TraceFileOne "+TraceFileOne);
		  RealFile = TraceFileOne;
		  ReadRulesFile();
		  JOptionPane.showMessageDialog(null,ruleTextArea,"Show Rule",JOptionPane.PLAIN_MESSAGE);
	  
	  }
}

public void getFileForOutput()
{
	  // check which file to read
		
		
		int len2 = TraceFileTwo.length();
		int len1 = TraceFileOne.length();
	  if (len2 > 0)
	  {
		  System.out.println("TraceFileTwo get "+TraceFileTwo);
		  RealFile = TraceFileTwo;
	  }
	  else
	  {
		  System.out.println("TraceFileOne get "+TraceFileOne);
		  RealFile = TraceFileOne; 
	  }
}
public void ReadRulesFile()
{
	int result =  ruleNumberTwo -  ruleNumberOne + 1;
	ruleTextArea = new JTextArea(result,40);
	int recCount = 1;
	//checkRealFile();
	try {
		FileReader fr     = new FileReader("/"+RealFile);
		BufferedReader br = new  BufferedReader(fr);
		//indexCount++;
		record = new String();
		ruleTextArea.append(RealFile + "\n");
		checkIfSecondRuleNoSameAsFirst();
		//ruleTextArea.append(ruleNumberOne + "  to "+ruleNumberTwo +  "\n");
		while ((record = br.readLine())!= null) {
			if ( recCount >= ruleNumberOne && recCount <= ruleNumberTwo)
			{
			System.out.println("recCount "+recCount);
			ruleTextArea.append(record + "\n");
			//System.out.println("record "+record);
			}
			recCount++;
		}
	}catch (IOException e){
		// catch possible io errors from readline()
		System.out.println("got an IO exception error");
		e.printStackTrace();
	}
}
public void checkIfSecondRuleNoSameAsFirst()
{
	  if (ruleNumberOne == ruleNumberTwo) {
		  ruleTextArea.append(ruleNumberOne +  "\n");
	  }
	  else {
		  ruleTextArea.append(ruleNumberOne + "  to "+ruleNumberTwo +  "\n");
	  }
}
public void checkRealFile(){
	  String c = ".pl";
		if (RealFile.indexOf(c) != -1) {
			twoFilesPos = RealFile.indexOf(c);
			twoFilesPos = twoFilesPos + 3;
			fileString =  RealFile.substring(0,twoFilesPos);
			RealFile = fileString;
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
    	fromInterlinguabtn =     new JButton(strName);
    	regulusWindow.barPanel.add(fromInterlinguabtn);
    	fromInterlinguabtn.addMouseListener(this);
    	getselectedFrame();
    }

    public void internalFrameActivated(InternalFrameEvent e) {
    	
    }

    public void internalFrameDeactivated(InternalFrameEvent e) {
	
    }
    public void getIconFrame(){
    	 frames = regulusWindow.desktop.getAllFrames();
    	 frameIndex = 0;
         int countFrames = frames.length;
         for (int i = 0; i < countFrames; i++) {
	          String strFrames = frames[i].toString();
	         holdFrameIndex = i;
         String b = "From";
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
			 b = "From";
			 System.out.println("strName "+strName);
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
         b = "From";
			 if (strFrames.indexOf(b) != -1) {
				try {
					//System.out.println("frames[i] "+frames[i]);
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
  	         String b = "From";
   			 if (strFrames.indexOf(b) != -1) {
   				 
     				if (frames[i].isSelected())
     				{
     					strName = frames[i].getTitle();
     					//System.out.println("strName "+strName);
     				}
     			 
   			}
  	      }
  	  }	  
public void checkWhichButton()
 {
	this_is_window_button = false;
	String b = "inter";
	if (ac.indexOf(b) != -1) {
		this_is_window_button	= true;
	 }
 }	   
	    
public void mouseClicked(MouseEvent e)
{
}
public void mousePressed(MouseEvent e)
{
		ac = fromInterlinguabtn.getActionCommand();
		//System.out.println("ac "+ac );
		checkWhichButton();
		if (this_is_window_button)
		{
			ac = fromInterlinguabtn.getActionCommand();
			//System.out.println("ac "+ac);
			getAllFrames();
		}
		else
		{
			getItem();
			getSelectedIndex();
			getSelectedItem();
			getFileNameFromRulesTable();
			getFileFromTable();
			getRuleNumbers();
			getRightFile();
		}
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
public void countRulesTable()
{
	  for (index = 0 ;index < saveSentenceIndex;  index++ ) 
	  {
		  holdIndex = index;
	  }
}
public void createListModel()
{
	 // create a list with a start item
	  
	  summaryList.setVisibleRowCount(holdIndex);
}
public void readRulesTable()
{
	  listModel.removeAllElements();
	  listModel.addElement("                         ");
	 // addFileToOutput();  
	for (index = 1 ;RuleTable[index]!= null;  index++ ) 
	  {
		  checkHowManyFilesExist();
	  }
}
public void addFileToOutput()
{
	  listModel.addElement(  RealFile); 
}
public void checkHowManyFilesExist()
{
	    ruleString = RuleTable[index];
		listModel.addElement( ruleString);
}
public void checkEndOfTable() {
	  index++;
	  if (RuleTable[index]!= null)
	  {
		  RuleTable[index] = null;
		  checkEndOfTable();
	  }
}
public void closeFrame(int count){
	  // Iterate ower the frames, closing them if they are open
	 		count = count - 1;
		  for (int j = 0; j < count; j++ ){
		  JInternalFrame f = allFrames[j];
		  if((f.isClosed()== false) || (f.isIcon()== true))
		  try{
		  f.setIcon(true);
		  f.dispose();
		  }
		  catch (PropertyVetoException ex) {}
		  }
		}
			
		  public void vetoableChange(PropertyChangeEvent event)
			throws PropertyVetoException {
			 
			JInternalFrame rulespane = (JInternalFrame) event.getSource();
			String name = event.getPropertyName();
			Object value = event.getNewValue();
			int count = 0;
			
			// we only want to check attempts to close a frame
			
			if (name.equals("closed")&& value.equals(Boolean.TRUE)) { // ask user
				int result = JOptionPane.showConfirmDialog(rulespane,
						"         OK to close?",
						"Close Confirm Pane",
						JOptionPane.OK_CANCEL_OPTION); 
						
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
		  
		
}
