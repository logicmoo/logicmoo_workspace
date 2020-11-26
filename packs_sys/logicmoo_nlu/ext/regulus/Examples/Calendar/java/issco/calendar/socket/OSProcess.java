package issco.calendar.socket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Class for interfacing with the environment in order to execute 
 * a specified string command in a separate process runtime command.
  */
public class OSProcess{ 
//extends Thread{
    public static Logger logger = Logger.getLogger(OSProcess.class.getPackage().getName());
    public Process process = null;
    String command = null;
    String[] environmentVars; // env vars when executing the external command
    java.io.File workingDir; // working directory when executing the external command 

    public StreamPrinter outputPrinter = null;
    public StreamPrinter errorPrinter = null;
    
    public String processOutput = "";
    
    public boolean stillStarting = true;
    
    /**
     *  
     * @param command to be executed as a command in a separate process 
     */
    public OSProcess(String command){
        init(command, null, null);
        
    }

    /**
     *  
     * @param command to be executed as a command in a separate process 
     */
    public OSProcess(String command, String[] envVar, java.io.File wDir){
        init(command, envVar, wDir);
        
    }

       
    private void init(String command, String[] envVar, java.io.File wDir) {
        this.command = command;
        this.environmentVars= envVar;
        this.workingDir = wDir;
    }
    
    public void destroy(){
        process.destroy();
        outputPrinter = null;
        errorPrinter = null;
    }
    
    public int waitFor() throws InterruptedException{
        return process.waitFor();
    }

    /**
     * starts the process
     */
    public void run() {
        try {
            process = Runtime.getRuntime().exec(command, environmentVars, workingDir);
            
            InputStream stdOutStream = process.getInputStream();
            InputStream stdErrStream = process.getErrorStream();
            outputPrinter = new StreamPrinter(stdOutStream, "OSProcess STDOUT: ", this);
            errorPrinter = new StreamPrinter(stdErrStream, "OSProcess STDERR: ", this);
                       
            outputPrinter.start();
            errorPrinter.start();
            
         } 
        catch (IOException ioe) {
        	logger.log(Level.SEVERE, "Failed creating runtime process '" + command + "'", ioe);
        }        
    }
    
    public boolean isStillStarting(){
    	return stillStarting;
    }
}

class StreamPrinter extends Thread
{
	public static Logger logger = Logger.getLogger(StreamPrinter.class.getPackage().getName());
    InputStream is;
    String type;
          
    StreamPrinter(InputStream is, String type, OSProcess parentThread)
    {
        this.is = is;
        this.type = type;        
    }
    
    public void run()
    {    	
        try
        {
        	final InputStreamReader isr = new InputStreamReader(is);
        		BufferedReader br = new BufferedReader(isr);
        		String line=br.readLine();
        		while ( line != null){
        			System.out.println(type + ">" + line);
        			/*
        			if ( (line.indexOf("ready") > -1) || line.equals("")){
        				line=null;
        				// notifyAll();
        				parentThread.stillStarting = false;
        			}        			
        			else
        			*/
        				line=br.readLine();
        		}
        		// parentThread.stillStarting = false;        		            
           }
        catch (IOException ioe) {
                logger.log(Level.WARNING, "Problem when getting the input stream of the process runtime command.", ioe);  
            }
        
    }
        
}