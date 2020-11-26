package RegulusGUI;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;

import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;

import RegulusGUI.progressLoadRecognition.progressListPane;

public class SpeechCommandInBackGround  extends Thread{

	private RegulusGUI regulusWindow = null;
	private progressLoadRecognition progressloadrecognition = null;
	private String CommandString = "";
	private boolean thisIsRemoteJob = false;
	public boolean jobIsFinished = false;
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
	
	public SpeechCommandInBackGround()
	{
		
	}
	
	public SpeechCommandInBackGround(RegulusGUI regulusgui,String strCommand)
	{
		setRegulusGUI(regulusgui);
		regulusWindow = regulusgui;
		CommandString = strCommand;
		
		
	}
		
		public SpeechCommandInBackGround(RegulusGUI regulusgui,String strCommand,boolean jobIsRemote)
		{
			setRegulusGUI(regulusgui);
			regulusWindow = regulusgui;
			CommandString = strCommand;
			thisIsRemoteJob = jobIsRemote;
		}
			
			public void run() {
				try
				{
					regulusWindow.handleCommand(CommandString,thisIsRemoteJob);
				}
				catch (Exception e) {
					e.printStackTrace();
				}
				EventQueue.invokeLater( new Runnable () {
					public void run () {
					jobIsFinished = true;
					getAllFrames();	
					regulusWindow.InputText = "Load Speech Recognition command succeeded";
					sendMessageLoad();
					//JOptionPane.showMessageDialog(null, "Loading of Speech data finished"
					//				,"Load Information",JOptionPane.INFORMATION_MESSAGE);
						checkMenuStatus(); 
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
	 					frames[i].dispose();
	 				
				  } catch (Exception e) {
		             // This should not happen
		             e.printStackTrace();
		         }
	 			 
				}
		      }
			}
			
			
			
			 public void sendMessageLoad(){
					
					if (regulusWindow.regulus_command_succeeded)
					{ 
						JOptionPane.showMessageDialog(null, "Loading of Speech data finished"
								,"Load Information",JOptionPane.INFORMATION_MESSAGE);
						//regulusWindow.txtBoxDisplayPositive(regulusWindow.InputText);
						
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
			}
		
			
		
	

