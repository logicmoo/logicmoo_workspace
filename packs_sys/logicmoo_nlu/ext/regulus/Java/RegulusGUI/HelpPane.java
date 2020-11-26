package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyVetoException;
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


public class HelpPane extends JFrame{
	private RegulusGUI regulusWindow = null;
	private Frame2     frame2       = null;
	private JudgePane  judgepane    = null;
	private TrainingPane  trainingpane    = null;
	private JInternalFrame helpPane = null;
	private JTextArea HelptextArea = new JTextArea(25,30);
	private String record = null;
	private JEditorPane htmlPane;
	private JScrollPane scrollPane;
	
	 // send name of internal frame 
	  public JInternalFrame getInternalFrame() {
		  return helpPane;
	  }
	
//	 get pointer to Regulus window
	  public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	  }
	  
	  // set the pointer to the Regulus window
	  public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	  }
//	 set the pointer to the Frame2 window
	  public void setFrame2(Frame2 window) {
		  frame2 = window;
	  }
	  
//		 set the pointer to the JudgePane window
	  public void setJudgePane(JudgePane judgewindow) {
		  judgepane = judgewindow;
	  }
	  
//		 set the pointer to the TrainingPane window
	  public void setTrainingPane(TrainingPane trainingwindow) {
		  trainingpane = trainingwindow;
	  }  
  public HelpPane() {
		  
	  }
	  
	public HelpPane(Frame2 frame, RegulusGUI regulusgui) {
	 
	  helpPane = new JInternalFrame("Help",true,true,true,true);
	  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
	  
	  Container c2 = helpPane.getContentPane();
	  setFrame2(frame);
	  setRegulusGUI(regulusgui);
	  HtmlFileReadHelp();
	  //TextFileReadHelp();   // This reads a textfile
	  c2.add(scrollPane,BorderLayout.CENTER );
	  //c2.add(new JScrollPane(HelptextArea), BorderLayout.CENTER);   // This outputs a textfile
	  helpPane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	  helpPane.pack();
	  
	}
	public HelpPane(JudgePane judge, RegulusGUI regulusgui) {
		 
		  helpPane = new JInternalFrame("Help",true,true,true,true);
		  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
		  
		  Container c2 = helpPane.getContentPane();
		  setJudgePane(judge);
		  setRegulusGUI(regulusgui);
		  HtmlFileReadHelp();
		  //TextFileReadHelp();   // This reads a textfile
		  c2.add(scrollPane,BorderLayout.CENTER );
		  //c2.add(new JScrollPane(HelptextArea), BorderLayout.CENTER);   // This outputs a textfile
		  helpPane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		  helpPane.pack();
		  
		}
	public HelpPane(TrainingPane trainingpane, RegulusGUI regulusgui) {
		 
		  helpPane = new JInternalFrame("Help",true,true,true,true);
		  setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
		  
		  Container c2 = helpPane.getContentPane();
		  setTrainingPane(trainingpane);
		  setRegulusGUI(regulusgui);
		  HtmlFileReadHelp();
		  //TextFileReadHelp();   // This reads a textfile
		  c2.add(scrollPane,BorderLayout.CENTER );
		  helpPane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		  helpPane.pack();
		  
		}
	  public void TextFileReadHelp()
	  {
		
		int recCount = 0;
		try {
			FileReader fr     = new FileReader("/C:/Regulus GUI/testdoc.txt");
			BufferedReader br = new  BufferedReader(fr);
			
			record = new String();
			while ((record = br.readLine())!= null) {
				HelptextArea.append(record + "\n");
				System.out.println("record "+record);
				recCount++;
			}
		}catch (IOException e){
			// catch possible io errors from readline()
			System.out.println("got an IO exception error");
			e.printStackTrace();
		}
	}
	  public void HtmlFileReadHelp()
	  {
		 try{
			 htmlPane = new JEditorPane("file:///C:/Regulus GUI/documentation.html");
			 htmlPane.setEditable(false);
			 scrollPane = new JScrollPane(htmlPane);
			 scrollPane .setPreferredSize(new Dimension(700,600) );
		 }catch (IOException e){
				// catch possible io errors from readline()
				System.out.println("got an IO exception error");
				e.printStackTrace();
		 }
	  }
	  public void HtmlFileReadHelp2()
	  //throws Exception
	  {
		try{
			FileReader reader = new FileReader("/C:/Regulus GUI/testdoc.html");
		
			JEditorPane editor = new JEditorPane();
			editor.setContentType("text/html" );
			editor.setEditable(false);
			editor.read(reader , null);
			scrollPane = new JScrollPane(editor);
			scrollPane .setPreferredSize(new Dimension(300,200) );
			getContentPane().add(scrollPane);
		}catch (IOException e){
			// catch possible io errors from readline()
			System.out.println("got an IO exception error");
			e.printStackTrace();
		}
	  }
	 
}
