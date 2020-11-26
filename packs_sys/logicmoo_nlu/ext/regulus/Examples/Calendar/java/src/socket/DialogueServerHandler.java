package socket;

	import gui.CalendarConfiguration;
	import gui.ApplicationState;

	import java.util.logging.Level;

	
import java.util.logging.Logger;


	/**
	 * 
	 */
	public class DialogueServerHandler { 
	    private static Logger logger = Logger.getLogger(DialogueServerHandler.class.getName());
	    
	    private final CalendarConfiguration calendarConf;	    
	    private ApplicationState appState = null;
	    
	    private static OSProcess dialogServerProcess = null;
	       
	    /**
	     * Constructor 
	     * @param calendarRootComponent
	     * @param calendarConf
	     * @param appState
	     */
	    public DialogueServerHandler(CalendarConfiguration calendarConf, ApplicationState appState) {
	        logger.setLevel(Level.ALL);
	        this.calendarConf = calendarConf;
	        this.appState = appState;        
	    }
	    
	    public ApplicationState getAppState(){
	    	return appState;
	    }
	         

	    /**
	     * Runs the external command that starts dialogue server.
	     * @throws Exception
	     */
	    public void startup() throws Exception {	        
	        String dialogStartupCommand = calendarConf.getDialogServerCommand();
	        String currentDialogPackage = appState.getLanguage().getDialogPackage();

	        String dServerCommand = dialogStartupCommand + " " + currentDialogPackage;
	        logger.config("Starting process: '" + dServerCommand + "'");
	        dialogServerProcess = new OSProcess(dServerCommand);
	        dialogServerProcess.run();

	        try {
	            Thread.sleep(calendarConf.getStartupDialogServerWaitTime());
	            logger.info("Was waiting for " + calendarConf.getStartupDialogServerWaitTime() + " milisec to launch dialog server ");
	        }
	        catch (InterruptedException e) {
	        	logger.severe("DialogueServerHandler error during startup(): "  + e.getMessage());
	        }
	    }
	    
	    public boolean shutdown() throws Exception {	
	        // kill the server process
	        if (dialogServerProcess != null) {
	        	dialogServerProcess.destroy();
	        	dialogServerProcess = null;	        	
	        }
	        return true;
	    }
	    
	    /**
	     * @param newAppState
	     */
	    protected void reconfigure(ApplicationState newAppState) throws Exception {
	        	logger.info("Stopping dialogue server services.");
	        	shutdown();

		        logger.info("Old language = " + this.appState.getLanguage().getLanguageName());
	            this.appState = new ApplicationState(newAppState);
		        logger.info("New language = " + this.appState.getLanguage().getLanguageName());

	            logger.info("Restarting dialogue server services.");            	            
	            startup();
	    }
       

}


