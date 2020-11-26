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

import java.io.*;

public class ProgressDisplay {
	
private RegulusGUI regulusWindow = null;
private Frame3 frame3 = null;
private JInternalFrame progressDisplay = null;

private String inputFile;
private String outputFile;
private int outputRecordsPerInputRecord;
private int nRecordsInInputFile;
private int estimatedNRecordsInOutputFile;

//send name of internal frame 
public JInternalFrame getInternalFrame() {
	  return progressDisplay;
}
//get pointer to Regulus window
public RegulusGUI getRegulusGUI() {
	  return regulusWindow;
}

// set the pointer to the Regulus window
public void setRegulusGUI(RegulusGUI window) {
	  regulusWindow = window;
}
//set the pointer to the Frame3 window
public void setFrame3(Frame3 window) {
	  frame3 = window;
}

public ProgressDisplay() {
	  
}

/*
 * inFileKey is the "key" we need to look up the input file from the Regulus process, e.g. "translation_corpus"
 * 
 * outFileKey is the "key" we need to look up the output file from the Regulus process, e.g. "batchrec_trace"
 * 
 * ratio is the number of output lines we expect for each input line. This depends on the type of file. For
 * example, a speech translation output file has 12 output lines for each input line.
 * 
 */

public ProgressDisplay(Frame3 frame, RegulusGUI regulusgui, String inFileKey, String outFileKey, int ratio) {
	
	setFrame3(frame);
	setRegulusGUI(regulusgui);
	regulusWindow = regulusgui;
	
	outputRecordsPerInputRecord = ratio;
	inputFile = regulusWindow.getRegulusFile(inFileKey, "input");
	outputFile = regulusWindow.getRegulusFile(outFileKey, "output");
	
	File actualOutputFile = new File(outputFile);
	if ( actualOutputFile.exists() ) {
		System.out.println("\nDeleting old file preparatory to monitoring: " + outputFile); 
		actualOutputFile.delete();
	}
	
	CountRecordsInInputFile();
	EstimateNumberOfRecordsInOutputFile();
	}

public void MonitorOutputFile()
{
	System.out.println("\nMonitoring output file " + outputFile + ", expecting " + estimatedNRecordsInOutputFile + " records\n"); 
	// start a new thread to read the file which is being written
	OutputFileMonitor fileMonitor = new OutputFileMonitor(outputFile, estimatedNRecordsInOutputFile, regulusWindow);
	fileMonitor.start();
}


public void CountRecordsInInputFile()
{
	nRecordsInInputFile = 0;
	
	try
	{
		// Open the file 
		BufferedReader stream = new BufferedReader(new FileReader(inputFile));
				
		// Continue to read lines while 
		// there are still some left to read
		
		while ( stream.readLine() != null )
			{
            // count the records
			nRecordsInInputFile++; 
			System.out.println("nRecordsInInputFile "+nRecordsInInputFile);
			}

			stream.close();
			} 
	catch (Exception e)
		{
			System.err.println("File input error");
		}
	}

private void EstimateNumberOfRecordsInOutputFile() 
{
	estimatedNRecordsInOutputFile = nRecordsInInputFile * outputRecordsPerInputRecord;
}

private class OutputFileMonitor extends Thread  
{
	
	File outputFile;
	int estimatedNRecordsInOutputFile;
	RegulusGUI frame;
	ProgressMonitor progressMonitor;
	
	OutputFileMonitor() {};
	
	OutputFileMonitor(String outputFile, int estimatedNRecordsInOutputFile, RegulusGUI frame) {
		 this.outputFile = new File(outputFile);
		 this.estimatedNRecordsInOutputFile = estimatedNRecordsInOutputFile;
		 this.frame = frame;
	}
	
	public void run() {
		try
		{
			int linesReadSoFar = 0;
			int lastLinesReadSoFar = 0;
			int numberOfTimesCheckedWithNoProgress = 0;
			BufferedReader stream = null;
			String lastLine = "";
			
			progressMonitor = new ProgressMonitor(frame, "Running corpus task", "", 0, estimatedNRecordsInOutputFile);
			//progressMonitor.setMillisToDecideToPopup(100);
			
			// If we check three times in a row and nothing has happened, 
			// and we also have at least 90% of the records we expected, say we're done
			while ( numberOfTimesCheckedWithNoProgress < 3 && linesReadSoFar < 0.9 * estimatedNRecordsInOutputFile ) {
				// Wait for the file to exist, then open it
				System.out.println("\nOpening monitored file " + outputFile.toString() + " ... "); 
				while ( !outputFile.exists() ) {
					System.out.println("File doesn't exist - waiting");
					sleep(1000);
				}
			
				linesReadSoFar = countLinesInFile(outputFile);
				System.out.println("Read " + linesReadSoFar + " lines"); 
			
				// Did we make any progress?
				if ( linesReadSoFar > lastLinesReadSoFar ) {
					// If we did, update lastLinesReadSoFar and numberOfTimesCheckedWithNoProgress
//					
					lastLinesReadSoFar = linesReadSoFar;
					numberOfTimesCheckedWithNoProgress = 0;
					// and also the progress monitor
					progressMonitor.setNote(linesReadSoFar + " lines in output file");
					progressMonitor.setProgress(linesReadSoFar);
				} else {
					numberOfTimesCheckedWithNoProgress++;
				}
				// Wait 10 seconds and then try reading again to see if new stuff has turned up
				sleep(10000);
			}
		
			System.out.println("Finished monitoring file " + outputFile);
		}
		catch (Exception e) {
			e.printStackTrace();
			}
		
		}
	
	
	public int countLinesInFile(File file)
	{
		int count = 0;
		
		try
		{
			// Open the file 
			BufferedReader stream = new BufferedReader(new FileReader(file));
					
			// Continue to read lines while 
			// there are still some left to read
			
			while ( stream.readLine() != null )
				{
	            // count the records
				count++; 	
				}

				stream.close();
				return count;
				} 
		catch (Exception e)
			{
				System.err.println("File input error");
				return -1;
			}
	}
	
	
}


          
}



