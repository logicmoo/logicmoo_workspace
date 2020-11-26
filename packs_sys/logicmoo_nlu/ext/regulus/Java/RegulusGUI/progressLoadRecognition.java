package RegulusGUI;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

import javax.swing.*;

import javax.swing.BorderFactory;
import javax.swing.border.Border;


public class progressLoadRecognition extends Thread{
	private RegulusGUI regulusWindow = null;
	private SpeechCommandInBackGround speechcommand = null;
	private String CommandString = "";
	private JProgressBar progressBar;
	private int counter = 0;
	private JTextArea out;
	public JInternalFrame[] allFrames = null;
	private int count = 0;
	private static int acount = 0;
	private JInternalFrame[] frames = null;
	private int frameIndex = 0;
	private int holdFrameIndex = 0;
	
//	get pointer to Regulus window
	public RegulusGUI getRegulusGUI() {
		  return regulusWindow;
	}

//	 set the pointer to the Regulus window
	public void setRegulusGUI(RegulusGUI window) {
		  regulusWindow = window;
	}
	
	public progressLoadRecognition()
	{
		
	}
	
	public progressLoadRecognition(RegulusGUI regulusgui, String strCommand,SpeechCommandInBackGround speech)
	{
		
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
		CommandString = strCommand;
		speechcommand = speech;
	}
	
			public void run() {
				try
				{
				 CreateAndLinkprogress();
				}
				catch (Exception e) {
						e.printStackTrace();
				}
			
			EventQueue.invokeLater( new Runnable () {
				public void run () {
					if (speechcommand.jobIsFinished)
					{
						System.out.println(" this has now finished");
						getAllFrames();
					
					}
					
					
					}
				});
			}
			
			public void getAllFrames(){
		    	 frames = regulusWindow.desktop.getAllFrames();
		    	 frameIndex = 0;
		         int countFrames = frames.length;
		         for (int i = 0; i < countFrames; i++) {
		          String strFrames = frames[i].toString();
		         holdFrameIndex = i;
		         String b = "Progress";
				 if (strFrames.indexOf(b) != -1) {
	 				try {
	 					//frames[i].setSelected(true);
	 					System.out.println(" I have selected something");
	 				
				  } catch (Exception e) {
		             // This should not happen
		             e.printStackTrace();
		         }
	 			 
				}
		      }
			}

	 public void CreateAndLinkprogress()
	 {
		
		 progressListPane progresslistpane = new progressListPane();
		 JInternalFrame progressListPaneInternalFrame = progresslistpane.getInternalFrame();
		 regulusWindow.desktop.add(progressListPaneInternalFrame);
		 progressListPaneInternalFrame.setVisible(true);
	 }
		public void executeCommandFromMenuInBackGround()
		{
			 if (CommandString.equals("LOAD"))
				{
				regulusWindow.InputText = "Load Recognition command succeeded";
				regulusWindow.regulus_command_succeeded = true;
				sendMessageLoad();
				}
		}
		
		 public void sendMessageLoad(){
				
				if (regulusWindow.regulus_command_succeeded)
				{ 
					regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
					
				}
				else
				{
					String command = regulusWindow.getCommandErrorString();	
					regulusWindow.InputText = command;
					regulusWindow.txtBoxDisplayNegative(regulusWindow.InputText);
				}
					checkMenuStatus(); 
		 }
		 public void checkMenuStatus()
		  {
			regulusWindow.updateCommandStatus();
			regulusWindow.availablemenus.check_available_menus();
			regulusWindow.unavailablecommands.check_unavailable_menus();
		  }
		 class progressListPane extends Thread {
				private JInternalFrame progresslistpane = null;
				 // send name of internal frame 
				
				  public JInternalFrame getInternalFrame() {
					  return progresslistpane;
				  }
				
		 public progressListPane() {
			  JTextArea text = new JTextArea(20, 20);
			  JTextField field;
			  field = new JTextField();
		      field.setEditable(false);
		   
			  String strText = "Loading Speech Recognition Data please be patient";
			  field.setText(strText);
			
			  progresslistpane = new JInternalFrame("Progress",true,true,true,true);
			  progresslistpane.setDefaultCloseOperation( JInternalFrame.DISPOSE_ON_CLOSE );
			  Container c2 = progresslistpane.getContentPane();
			  progresslistpane.add(field);
			 
			  progresslistpane.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
			  progresslistpane.setLocation(250,250);
				
			  progresslistpane.setSize(400,100);
			
			  
		 		}
		 public void closeTheFrame() {
			 progresslistpane.dispose(); 
		 }
		 }
}
		 
		 
		
		


		 
	
