package issco.calendar.socket;

import issco.calendar.gui.*;

import java.io.File;
import java.util.logging.Logger;

import net.sf.regulus.RegClient;
import net.sf.regulus.RegResultNBest;

/**
 * When running this thread: the client connects to the regserver.
 */
public class RunnableRecognition implements Runnable {
	
	private final static Logger logger = Logger.getLogger(RunnableRecognition.class.getName());
	
	private final Calendar calendarRootComponent;
	private final RegClient regClient ;
	
	private final Language currentLanguage = null;
	private static boolean lastWavFileRetrieved ;
	
	private final String grammar;
	
	//private final boolean computeXCPUTime ; 
	private final String lastWavFileRecorded ;
	
	private final CalendarConfiguration calendarConf;
	
	private final static float xCPURT = 0.0f;
	
	private String stateVarRecognitionResult;
    private final String stateVarHelpSeed ;
    private String regResultInterpretation ;
    
    private String interpret4DialogueServer = null; // this attribute is set up inside the processRecognitionResult method

	RunnableRecognition( RegClient p_regClient, 
			boolean p_lastWavFileRetrieved, boolean p_computeXCPUTime, 
			String p_lastWavFileRecorded, CalendarConfiguration p_calendarConf, 
			final Calendar p_calendarRootComponent, 
			String p_stateVarRecognitionResult,
			String p_stateVarHelpSeed,
			String p_regResultInterpretation,
			String p_grammar)
	{
		regClient = p_regClient;  
		lastWavFileRetrieved = p_lastWavFileRetrieved;	
		//computeXCPUTime = p_computeXCPUTime;
		lastWavFileRecorded = p_lastWavFileRecorded;
		
		calendarConf= p_calendarConf;
		calendarRootComponent = p_calendarRootComponent;
		
		stateVarRecognitionResult= p_stateVarRecognitionResult;
		stateVarHelpSeed = p_stateVarHelpSeed;
		regResultInterpretation = p_regResultInterpretation;
		
		grammar = p_grammar;
	}
	
	/**
	 * Invokes the regserver with the grammar specified in the constructor
	 * Gets the response received by regClient and 
	 * calls Client methods in order to update the interface  
	 */
	public void run() {
		net.sf.regulus.RegResultNBest glmRegResult = null;      
        lastWavFileRetrieved = false;

        try {
        	logger.info("recognize with the grammar '" + grammar + "'");
            glmRegResult = regClient.recognizeNBestMode(grammar);
                
             logger.info("got result from regserver : '" + glmRegResult.getRecognition() + "'");
        }
        catch (Exception e) {
            // JOptionPane.showMessageDialog(calendarRootComponent, e.toString(), "Recognition Error", JOptionPane.ERROR_MESSAGE);
        	logger.info("Recognition Error");
        	processRecognitionResult(null);
            return;
        }        
        processRecognitionResult(glmRegResult);
    }
	
    /**
     * Takes the regResult and eliminates some characters like '[', etc.
     * Calls the "processRecognitionResult" method from Client class  in order to update the interface. 
     * 
     * @param regResult
     */
	private void processRecognitionResult(RegResultNBest regResult){
        logger.entering(this.getClass().getName(), "processRecognitionResult");
 
        String result = null;
        
        if(regResult != null ){
        	
        	if (regResult.getStatus() == RegResultNBest.ACTION_SUCCEEDED) {
        		// calendarRootComponent.setRecognitionTimings(xCPURT);
        	}	
        	else{
        		// calendarRootComponent.setRecognitionTimings(xCPURT);
        	}
        	
        	if (regResult.getRecognition() != null) {
        		result = new String(regResult.getRecognition());

        		// XXX 
            if(regResult.getInterpretation() != null){
                stateVarRecognitionResult = result;
                regResultInterpretation = regResult.getInterpretation();
                regResultInterpretation = nlValueToRegulusInterpretation(regResult.getInterpretation()); //  Retrieves the "[ 'value' =" string from the nlValue.
                regResultInterpretation = regResultInterpretation.replaceAll("\'", "");
                regResultInterpretation = regResultInterpretation.replaceAll(" ", "");
            }
            
            this.interpret4DialogueServer = regResult.getInterpret4DialogueServer() + ".";
            calendarRootComponent.processRecResultAndInvokeDialogueServer(result, this.interpret4DialogueServer); // Calls the "processRecognitionResult" method from Client class  in order to update the interface.
            }
         logger.exiting(this.getClass().getName(), "processRecognitionResult");
        }
    }
     
    /*
     * 
     */
    public String getStateVarRecognitionResult(){
    	return stateVarRecognitionResult;
    }
    
    public String getStateVarHelpSeed(){
    	return stateVarHelpSeed;
    }
    
    public String getRegResultInterpretation(){
    	return regResultInterpretation;
    }
    
    public boolean getLastWavFileRetrieved (){
    	return lastWavFileRetrieved;
    }

	/**
	 * Retrieves the "[ 'value' =" string from the nlValue. 
	 * @param nlValue
	 * @return
	 */
    public static String nlValueToRegulusInterpretation(String nlValue){
        String result = null;
        
        //
        // retrieve the value string
        //
        if (nlValue.startsWith("[ 'value' = ")) {
            result = nlValue.substring("[ 'value' = ".length());
            result = result.replaceAll("\\]$", "");
        }
        else{
            result = nlValue;
        }

        return result;
    }
    
    /*
     * gets the N-best result in the format required by the dialogue server  
     */
    public String getInterpret4DialogueServer(){
    	return interpret4DialogueServer;
    }

}
