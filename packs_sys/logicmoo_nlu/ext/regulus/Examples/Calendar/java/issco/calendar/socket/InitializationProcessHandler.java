package issco.calendar.socket;

import issco.calendar.gui.*;

import java.util.logging.Level;
import java.util.logging.Logger;

/*
 * Class usefull for starting general processes like nlm, vocaliser, recserver  
 */
public class InitializationProcessHandler implements IProcessHandler {
	public static Logger logger = Logger.getLogger(InitializationProcessHandler.class.getName());

	OSProcess []processMap;
	//long []lProcessMap; // library map 
	ProcessHandler ph;
	CalendarConfiguration calendarConfig;
	
	private static int dialServStartingTime = 40000;

	public InitializationProcessHandler(CalendarConfiguration calendarConfig) {
		logger.setLevel(Level.CONFIG);
		this.calendarConfig = calendarConfig;
		// ph = new ProcessHandler(calendarConfig.getProcessHandlerLibrary());
		
		int numProcesses = calendarConfig.getGlobalProcesses().length;
		if(numProcesses > 0){
			processMap = new OSProcess[numProcesses];
			// lProcessMap = new long[numProcesses];
		}
	}
	
	/**
	 * Starts up processes that are required by all languages (e.g. nlm, translation server).
	 * @throws Exception 
	 */
	public void startup() throws Exception{
		
		if(processMap != null){
			String[] processes = calendarConfig.getGlobalProcesses();
			dialServStartingTime = calendarConfig.getDialogueServerStartingTime();
			
			long currentTime = System.currentTimeMillis();
			for(int i=0 ; i<processMap.length ; i++){
				logger.info( (System.currentTimeMillis() - currentTime) + " miliseconds passed since the previous process started. ");
				currentTime = System.currentTimeMillis();
				try {
					logger.config("Starting process: '" + processes[i] + "'");
					processMap[i] = new OSProcess(processes[i]);
					processMap[i].run();
					/*TODO: wait until the process started
				     * and pipes streams (in child threads) through to the corresponding
				     * streams (e.g., the process System.err to this System.err) also finished.
				     * Should implement a way to get notice when the process completes,
				     * (but this is difficult when using the redirect comand).
				     */					
				}
				catch (Exception e) {
					throw new Exception("Could not start '" + processes[i] + "'", e);
				}
			}
			logger.info( (System.currentTimeMillis() - currentTime) + " miliseconds passed since the previous process started. ");
			// here just wait for a while so that the last process started
			System.out.println(" Please wait for a few seconds until the dialogue server starts: " );
			long startTime = System.currentTimeMillis();
			
			// Until a nice solution will be implemented in order 
			// to catch when the external process started:
			// just wait for a fixed period of time  
			
	      
			try {				
		          Thread.sleep(dialServStartingTime ); // wait for around one minute: 50484
		       }
		       catch (InterruptedException e) {
		          System.out.print("Couldn't sleep because of the noisy exception : " + e);
		       }
//			System.out.println(" After Waiting : " + (System.currentTimeMillis() - startTime) + " millisec. " );
  
		}
		
	}

	/**
	 * Stops all general processes (e.g. nlm, vocalizer, recserver) 
	 */
	public boolean shutdown(){
		// boolean returnValue = true;
		try{
		if(processMap != null){
			for(int i=0 ; i<processMap.length ; i++){
				processMap[i].destroy();
			}
		}
		}
		catch(Exception ex){
			System.out.println("Exception when stopping processes : ");
			ex.printStackTrace();
			return false; 
		}
		
	/*
		if(lProcessMap != null){
			for(int i=0 ; i<lProcessMap.length ; i++){
				ph.stopProcess(lProcessMap[i]);
			}
		}
		*/
		return true;
	}
}

