package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import se.sics.prologbeans.*;
import java.io.*;

import javax.swing.*;
public class ReadOneCorpusFile extends Thread {
	
	private RegulusGUI regulusWindow = null;
	private Frame3 frame3 = null;
	private CreateCorpusMeny createcorpusmeny = null;
	private String CorpusString = "";
	private String CorpusMenuString = "";
	private String record = null;
	private Integer recCount = 1;
	private int  nLines = 0;
	private String progressFile = "";
	private ProgressMonitor monitor;
	private String TranslateCorpusString = "TRANSLATE_CORPUS";
    private String TranslateSpeechCorpusString = "TRANSLATE_SPEECH_CORPUS";
    private String TranslateSpeechCorpusAgainString = "TRANSLATE_SPEECH_CORPUS_AGAIN";
    private int length = 0;
	
	
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
	
	public ReadOneCorpusFile()
	{
		
	}
	
	public ReadOneCorpusFile(CreateCorpusMeny createcorpus,Frame3 frame, RegulusGUI regulusgui, String strCorpus,String menuCorpus)
	{
		setFrame3(frame);
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
		frame3 = frame;
		setCreateCorpusMeny(createcorpus);
		createcorpusmeny = createcorpus;
		CorpusString = strCorpus;
		System.out.println("CorpusString "+CorpusString);
		CorpusMenuString = menuCorpus;
		
		RegulusGUI.ProgressFileInfo[] infoArray = regulusWindow.getProgressFileInfo(CorpusString);
		progressFile = infoArray[0].getFile();
		System.out.println("progressFile "+progressFile);
		nLines = infoArray[0].getNumberOfLines();
		System.out.println("nLines "+nLines);
	}
	
	 public void run(){
		 try{
		  		 sleep(10000);
		  	 }
		 catch (InterruptedException e){
	  	 }
		 try {
		 FileInputStream fileIn = new FileInputStream(progressFile);
		 InputStreamReader inReader = new InputStreamReader(fileIn);
		 BufferedReader in = new BufferedReader(inReader);
		 monitor = new ProgressMonitor(null,"monitoring progress",null,0,nLines);
		   	while ((record = in.readLine())!= null) {
		   		recCount++;
		   		System.out.println("recCount "+recCount);
		        monitor.setProgress(recCount);
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
				decideWhichCorpus();
				}
			});
		 }
		 catch (IOException e ) {
		 }
		System.out.println("done");
		
	}
	 
	public void decideWhichCorpus(){
		length = CorpusString.length();
		if (CorpusString.lastIndexOf(TranslateSpeechCorpusAgainString) != -1)
		 {
			TranslateSpeechCorpusAgain();
		 }
		
		else if (CorpusString.lastIndexOf(TranslateSpeechCorpusString) != -1)
		 {
			TranslateSpeechCorpus();
		 }
		
		else if (CorpusString.lastIndexOf(TranslateCorpusString) != -1)
		 {
			TranslateCorpus();
		 }
	}
	public void TranslateSpeechCorpusAgain()
	{
		 if (length > 29){
			regulusWindow.InputText =CorpusString+ " command succeeded";
			sendMessageCorpus(); 
		 }
		 else
		 {
			regulusWindow.InputText = "Translate Speech Corpus Again command succeeded";
			sendMessageCorpus();
		 }
	}
	public void TranslateCorpus()
	{
		 if (length > 16){
			 regulusWindow.InputText =CorpusString+ " command succeeded";
			 sendMessageCorpus(); 
		 }
		 else
		 {
			regulusWindow.InputText = "Translate Corpus command succeeded";
			sendMessageCorpus();
		 }
	}
	public void TranslateSpeechCorpus()
	{
		 if (length > 23){
			regulusWindow.InputText =CorpusString+ " command succeeded";
			sendMessageCorpus();  
		 }
		 else
		 {
			regulusWindow.InputText = "Translate Speech Corpus command succeeded";
			sendMessageCorpus();
		 }
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
	
}
