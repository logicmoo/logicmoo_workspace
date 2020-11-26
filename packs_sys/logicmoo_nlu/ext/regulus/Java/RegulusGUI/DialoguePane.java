package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
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

public class DialoguePane extends JFrame{
	private RegulusGUI regulusWindow = null;
	private Frame4     frame4       = null;
	private JInternalFrame dialoguepane = null;
	private JTextArea dialogueTextArea;
	private String  holdSentence = "";
	private String  holdString = "";
	private int length = 0;
	private boolean end_of_string = false;
	private int numNewLines = 0;
	private int holdPos = 0;
	
	 // send name of internal frame 
	  public JInternalFrame getInternalFrame() {
		  return dialoguepane;
	  }
	
//	 get pointer to Regulus window
	  public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	  }
	  
	  // set the pointer to the Regulus window
	  public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	  }
//	 set the pointer to the Frame4 window
	  public void setFrame4(Frame4 window) {
		  frame4 = window;
	  }
	  
public DialoguePane() {
		  
	  }

public DialoguePane(Frame4 frame,RegulusGUI regulusgui,String frame4String) {
	
	 dialoguepane = new JInternalFrame("Processing Dialogue",true,true,true,true);
	  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
	  holdString = frame4String;
	  
	  Container c2 = dialoguepane.getContentPane();
	  setFrame4(frame);
	  setRegulusGUI(regulusgui);
	  
	  // create outputdata
	  length = holdString.length();
	  holdSentence = holdString;
	  checkString();
	  handleDisplayData();
	  
	  c2.add(dialogueTextArea,BorderLayout.CENTER );
		 
	  dialoguepane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	  dialoguepane.setLocation(250,200);
	  setSize(30,30);
	  dialoguepane.pack();
	}
public void handleDisplayData()
{
	numNewLines = numNewLines + 2;
	dialogueTextArea = new JTextArea(numNewLines,60);
	 dialogueTextArea.setText(holdString);
}

public void checkString(){
	
	String tempString;

	 for (int ind = 0; ind < length; ind++) {
		 int ind2 = ind +1;
	       tempString = holdSentence.substring(ind,ind2 );
	       if (tempString.equals("\n"))
	       {
	    	   numNewLines++;
	    	   System.out.println("numNewLines "+numNewLines);
	       }
	 	} 
	}
}
