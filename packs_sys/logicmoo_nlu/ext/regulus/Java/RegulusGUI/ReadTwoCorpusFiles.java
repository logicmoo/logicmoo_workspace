package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import se.sics.prologbeans.*;
import java.io.*;
import javax.swing.*;

import java.util.regex.*;

public class ReadTwoCorpusFiles extends Thread{

	private RegulusGUI regulusWindow = null;
	private Frame3 frame3 = null;
	private CreateCorpusMeny createcorpusmeny = null;
	private String CorpusString = "";
	private String record = null;
	private Integer recCount = 0;
	private Integer strrecCount = 0;
	private int  nLines = 0;
	private int  sumLines = 0;
	private String progressFile = "";
	private String REGEX;
	private JProgressBar progressbarfull;
	private JProgressBar progressbarpartial;
	private JTextArea out;
	private JTextField input = new JTextField(25);
	private int partialmax = 0;
	private int sumRecCount = 0;
	
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
//	set the pointer to the Frame3 window
	public void setFrame3(Frame3 window) {
		  frame3 = window;
	}
//	get pointer to Frame3 window	
	public Frame3 getFrame3() {
		  return frame3;
	}
	// set the pointer to the createcorpusMeny
	public void setCreateCorpusMeny(CreateCorpusMeny menuwindow) {
		createcorpusmeny = menuwindow;
	}
// get the pointer to the createCorpusMeny
	public CreateCorpusMeny getCreateCorpusMeny() {
		return createcorpusmeny;
	}
	
	public ReadTwoCorpusFiles()
	{
		
	}
	
	public ReadTwoCorpusFiles(CreateCorpusMeny createcorpus,Frame3 frame, RegulusGUI regulusgui, String strCorpus)
	{
		setFrame3(frame);
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
		frame3 = frame;
		setCreateCorpusMeny(createcorpus);
		createcorpusmeny = createcorpus;
		CorpusString = strCorpus;
		
		RegulusGUI.ProgressFileInfo[] infoArray = regulusWindow.getProgressFileInfo(CorpusString);
		progressFile = infoArray[0].getFile();
		System.out.println("progressFile "+progressFile);
		nLines = infoArray[0].getNumberOfLines();
		int holdLines = infoArray[0].getNumberOfLines();
		sumLines = nLines + holdLines;
		System.out.println("sumLines "+sumLines);
		System.out.println("nLines "+nLines);
		partialmax = nLines;
		REGEX = infoArray[0].getSearchPattern(); 
		System.out.println("REGEX  "+REGEX );
	}

	
public void run(){
	try{
		CreateAndLinkshowprogress();
		out.append("Creating and Reading SpeechFiles please be patient"+"\n" );
  		 sleep(10000);
  	 }
  	 catch (InterruptedException e){
  	 }
	 try {
	 FileInputStream fileIn = new FileInputStream(progressFile);
	 InputStreamReader inReader = new InputStreamReader(fileIn);
	 BufferedReader in = new BufferedReader(inReader);
	   	while ((record = in.readLine())!= null) {
	   		search();
	   		recCount++;
        	 try{
       		 sleep(200);
       	 }
       	 catch (InterruptedException e){
       	 }
       	
         }
		in.close();
		regulusWindow.deleteRegulusFile(progressFile);
		new readSpeechFile().start();
	 	} catch (IOException e ) {
		 }
		System.out.println("done first file");
		}

	


public void sendMessageCorpus(){
	 if (regulusWindow.regulus_command_succeeded)
		{ 
		regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
		createcorpusmeny.CreateAndLinkcorpusPane(CorpusString);
		}
		else
		{
		
		String command = regulusWindow.getCommandErrorString();
		regulusWindow.InputText = command;
		regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
		}
regulusWindow.handleCommand("TRANSLATE_TRACE_ON");
regulusWindow.availablemenus.check_available_menus();
regulusWindow.unavailablecommands.check_unavailable_menus();
	 
}
public void search()
{
	if (record.indexOf(REGEX) != -1)
	{
		strrecCount++;
		sumRecCount++;
		new Thread(new thread1()).start(); // start the first thread 
		new Thread(new thread2()).start(); // start the second thread 
		//System.out.println("I have found record "+record);
	}
}
public void CreateAndLinkshowprogress()
{
	 showProgressPane showprogresspane = new showProgressPane();
	 JInternalFrame showprogressPaneInternalFrame = showprogresspane.getInternalFrame();
	 regulusWindow.desktop.add(showprogressPaneInternalFrame);
	 showprogressPaneInternalFrame.setVisible(true);
}
//The Thread class handling progressbar for both files
public class thread1 implements Runnable{
	public void run(){
		
		progressbarfull.setValue(sumRecCount);
		progressbarfull.repaint();
		try { Thread.sleep(10); }
			catch (InterruptedException err){}
		}
	
	}
//The Thread2 class handling progressbar for each individul heavy job
public class thread2 implements Runnable{
	public void run(){
		progressbarpartial.setValue(strrecCount);
		progressbarpartial.repaint();
		//System.out.println("strrecCount "+strrecCount);
		try { Thread.sleep(10); }
			catch (InterruptedException err){}
		}
	
	}
class readSpeechFile extends Thread {
	public readSpeechFile()
	{
		RegulusGUI.ProgressFileInfo[] infoArray = regulusWindow.getProgressFileInfo(CorpusString);
		progressFile = infoArray[1].getFile();
		System.out.println("progressFile "+progressFile);
		nLines = infoArray[1].getNumberOfLines();
		System.out.println("nLines "+nLines);
		partialmax = nLines;
		strrecCount = 0;
	}
	public void run(){
		 try {
		 FileInputStream fileIn = new FileInputStream(progressFile);
		 InputStreamReader inReader = new InputStreamReader(fileIn);
		 BufferedReader in = new BufferedReader(inReader);
		   	while ((record = in.readLine())!= null) {
		   		//System.out.println("record "+record);
		   		//System.out.println("strrecCount "+strrecCount);
		   		System.out.println("sumOfRecords in secind file "+sumRecCount);
		   		strrecCount++;
		   		sumRecCount++;
		   		new Thread(new thread1()).start(); // start the first thread 
		   		new Thread(new thread2()).start(); // start the second thread 
	        	 try{
	       		 sleep(100);
	       		
	       	 }
	       	 catch (InterruptedException e){
	       	 }
	       	
	         }
			in.close();
			regulusWindow.deleteRegulusFile(progressFile);
			EventQueue.invokeLater( new Runnable () {
			public void run () {
				if (CorpusString.equals("TRANSLATE_SPEECH_CORPUS"))
					{
					regulusWindow.InputText = "Translate Speech Corpus command succeeded";
					System.out.println("Translate Corpus command succeeded");
					sendMessageCorpus();
					}
				else
				{
					regulusWindow.InputText = CorpusString + " command succeeded";
					System.out.println("Translate Corpus arg command succeeded");
					sendMessageCorpus();
				}
				
				}
			});
		 }
		 catch (IOException e ) {
		 }
		System.out.println("done speechFile");
		}

	}
class showProgressPane extends Thread {
	private JInternalFrame showprogresspane = null;
	 // send name of internal frame 
	  public JInternalFrame getInternalFrame() {
		  return showprogresspane;
	  }
	public showProgressPane() {
		showprogresspane = new JInternalFrame("Progress",true,true,true,true);
		showprogresspane.setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
		
		
		progressbarpartial = new JProgressBar(0,partialmax);
		progressbarpartial.setValue(0);
		
		progressbarfull = new JProgressBar(0,sumLines);
		
		progressbarfull.setStringPainted(true);
		progressbarpartial.setStringPainted(true);
		
		Container c2 = showprogresspane.getContentPane();
		
		out = new JTextArea(4,28);
		out.setMargin(new Insets(5,5,5,5));
		out.setEditable(false);
			
		c2.add(progressbarfull, BorderLayout.PAGE_START);
		//c2.add(input,BorderLayout.LINE_START);
		c2.add(progressbarpartial,BorderLayout.PAGE_END);
		c2.add(out,BorderLayout.CENTER );
		
		showprogresspane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		showprogresspane.setLocation(250,250);
		showprogresspane.setSize(40,40);
		showprogresspane.pack();
	 }
}
}


