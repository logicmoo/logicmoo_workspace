package gui.transact;

import socket.OSProcess;

import java.util.logging.Level;
import java.util.logging.Logger;

/*
 * Class usefull for starting general processes like nlm, vocaliser, recserver  
 */
public class TrInitGlobalProcesses extends TrState {
	private static Logger logger = Logger.getLogger(TrInitGlobalProcesses.class.getName());

	private OSProcess []processMap;
	
	private static int STARTING_TIME_INTERVAL = 40000;

	public TrInitGlobalProcesses() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * Starts up processes that are required by all languages (e.g. nlm, translation server).
	 * @throws Exception 
	 */
	private void startup(Transaction tr) throws Exception{		
		if(processMap != null){
			String[] processes = tr.getConfig().getGlobalProcesses();
			STARTING_TIME_INTERVAL = tr.getConfig().getDialogueServerStartingTime();
			
			long currentTime = System.currentTimeMillis();
			for(int i=0 ; i<processMap.length ; i++){
				logger.info( (System.currentTimeMillis() - currentTime) + " miliseconds passed since the previous process started. ");
				currentTime = System.currentTimeMillis();
				try {
					logger.config("Starting process: '" + processes[i] + "'");
					processMap[i] = new OSProcess(processes[i]);
					processMap[i].run();
					// TODO: wait until the process started
					/*
				     * and pipes streams (in child threads) through to the corresponding
				     * streams (e.g., the process System.err to this System.err) also finished.
				     * Should implement a way to get notice when the process completes,
				     * (but this is difficult when using the redirect command).
				     */					
				}
				catch (Exception e) {
					throw new Exception("Could not start '" + processes[i] + "'", e);
				}
			}
			logger.info( (System.currentTimeMillis() - currentTime) + " miliseconds passed since the previous process started. ");
			// Here just wait for an approximate period of time so that the processes start. 	
			// This is a temporary solution until a nice solution will be implemented in order 
			// to catch when the external process started:  				      
			try {				
		          Thread.sleep(STARTING_TIME_INTERVAL ); // wait for around one minute: 50484
		       }
		       catch (InterruptedException e) {
		    	   logger.severe("Inside InitializationProcessHandler class: The thread couldn't sleep because of the noisy exception : " + e);
		       } 
		}
		
	}

	/**
	 * Stops all general processes (e.g. nlm, vocalizer, recserver) 
	 */
	private boolean shutdown(){
		// boolean returnValue = true;
		try{
		if(processMap != null){
			for(int i=0 ; i<processMap.length ; i++){
				processMap[i].destroy();
			}
		}
		}
		catch(Exception ex){
			logger.severe("Exception when stopping processes : ");
			ex.printStackTrace();
			return false; 
		}
			
		return true;
	}

	
	@Override
	public void runTasks(Transaction tr) {	
		System.out.println(this.status());
		tr.addVisitedState(this);
		int numProcesses = tr.getConfig().getGlobalProcesses().length;
		if(numProcesses > 0){
			processMap = new OSProcess[numProcesses];
		}

		try {
			this.startup(tr);
			this.setTasksCompleted(true);
			//TODO: to wait for a while here in order to have the dialogue server started ?

		} catch (Exception e) {
			String errorMessage = "The following error occured while initiating the system:\n";
			errorMessage += e.getMessage();
			errorMessage += "\nPlease check your configuration and try again.";

			logger.log(Level.SEVERE, "Could not start all required processes. Error occuring when creating Calendar Main App",
					e);
			// this.tasksCompleted = false, i.e. the value of this attribute remains false (as false is the default value)
			;			
		}
		
	}

	@Override
	public void nextStep(Transaction tr) {
		if (this.isTasksCompleted()){
			tr.setState(TrConstants.INIT_SPEECH_HANDLER.getState());
		}
		else{
			tr.setState(TrConstants.EXIT_APP.getState());
		}
	}
	
    /**
     * Stops the processes started by tasks of TrInitGlobalProcess state.
     */
	@Override
	public void stopStateProcs(Transaction tr){		
	   this.shutdown();
	};
	
}

