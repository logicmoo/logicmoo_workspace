package issco.calendar.socket;

	import issco.calendar.gui.CalendarConfiguration;
	import issco.calendar.gui.ApplicationState;

	import java.io.IOException;

	import java.util.logging.Level;

	
import java.util.logging.Logger;


	/**
	 * 
	 */
	public class DialogueServerHandler implements IProcessHandler { 
	    private static Logger logger = Logger.getLogger(DialogueServerHandler.class.getName());
	    //private String logDirectory = null;
	    
	    private final CalendarConfiguration calendarConf;	    
	    private ApplicationState appState = null;
	    
	    // private static Process dialogServerProcess = null;
	    private static OSProcess dialogServerProcess = null;
	       
	    /**
	     * Constructor 
	     * @param calendarRootComponent
	     * @param calendarConf
	     * @param appState
	     */
	    public DialogueServerHandler(CalendarConfiguration calendarConf, ApplicationState appState) {
	        logger.setLevel(Level.ALL);
//	        logDirectory = appState.getLoggingDirectory();	        	        

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
	        logger.entering(this.getClass().getName(), "startupDialogueServer");
	        
	        String dialogStartupCommand = calendarConf.getDialogServerCommand();
	        String currentDialogPackage = appState.getLanguage().getDialogPackage();
	        
	        //try {
	        	String dServerCommand = dialogStartupCommand + " " + currentDialogPackage;
	        /*	
	            logger.finest("Launching '" + dServerCommand + "'");
	            System.out.println("Launching '" + dServerCommand + "'");
	            dialogServerProcess = Runtime.getRuntime().exec(dServerCommand);
	            */
	        	
	            //
				logger.config("Starting process: '" + dServerCommand + "'");
				dialogServerProcess = new OSProcess(dServerCommand);
				dialogServerProcess.run();

	        /*    
	        }
	        catch (IOException e) {
	            throw new Exception("Failed to start dialog server");
	        }
	        */
	        try {
	            Thread.sleep(calendarConf.getStartupDialogServerWaitTime());
	            System.out.println("Wait " + calendarConf.getStartupDialogServerWaitTime() + " milisec to launch dialog server ");
	        }
	        catch (InterruptedException e) {
	        }
	        logger.exiting(this.getClass().getName(), "startup dialog server.");	        
	    }
	    
	    public boolean shutdown() throws Exception {	
	        //
	        // kill the server process
	        //
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
	//        }
	    }
       
	    public static void main(String[] args){
	    	String configFile = "D:/Workspace/Calendar/calendar.prop";
	    	
	    	CalendarConfiguration calendarConfig = new CalendarConfiguration(configFile);
			// debug only 
			System.out.println("Dialog server command: " + calendarConfig.getDialogServerCommand());
			
			ApplicationState calendarApplicationState = new ApplicationState(calendarConfig);

			DialogueServerHandler dialogServer = new DialogueServerHandler(calendarConfig, calendarApplicationState);			
			try{
				dialogServer.startup();
			}			
			catch(Exception e){
				logger.log(Level.SEVERE, "Couldn't create dialog server process! ");
			}
			
	        try{
	        	ApplicationState newApplicationState = new ApplicationState(calendarApplicationState);
	        	newApplicationState.setLanguageIndex(1);	               
	        	try{
	        		dialogServer.reconfigure(newApplicationState);
	        	}
	        	catch(Exception e){
	        		System.out.println("Dialog server was NOT reconfigured ! ");
	        	}
	        }
	        catch(Exception ex){
	        	ex.printStackTrace();
	        };
/*
			try{
				dialogServer.shutdown();
			}
			catch(Exception e){
				System.out.println("Could shutdown dialog server process! ");
			}
*/
	    }

}


