package gui;

import gui.transact.TrConstants;
import gui.transact.TrInitGlobalProcesses;
import gui.transact.Transaction;

import java.io.File;
import java.io.IOException;

import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger; // supposing that we have JRE 1.4 or later version 
import java.util.logging.SimpleFormatter;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.Box;
import javax.swing.JFrame;
import javax.swing.UIManager;

import socket.SpeechHandler;

/**
 * 
 * Main class for launching the GUI. The Calendar is a speech-enabled application 
 * for accessing a meeting database.
 * This Calendar class is a mediator and contains event-handling methods. 
 * 
 */
public class Calendar {
	
	private boolean updateRecPanelControls = true;

	public static Logger logger = Logger.getLogger(Calendar.class.getPackage()
			.getName());

	/** configuration object shared throughout the app */
	private CalendarConfiguration calendarConfig ;

	private ApplicationState appState ;

	//
	// visual components
	//	
	CalendarFrame calFrame ;
	private CalendarPanel calPanel = null;
	private StatusBar calStatusBar ;

	//
	// objects that interface to the outside world
	//
	private SpeechHandler calRecHandler = null;
	private TrInitGlobalProcesses globalProcs = null;
	// private DialogueServerHandler dialServHandler = null;
	private Dialogue observableDialog; 
	
	Object syncObject = new Object();

	protected Transaction getTransact() {
		return transact;
	}

	private Transaction transact;
	
	/*
	 * Public constructor for Calendar application
	 */
	public Calendar(String configFile) {
		observableDialog = new Dialogue();
		// Create configuration
		calendarConfig = new CalendarConfiguration(configFile);

		appState = new ApplicationState(calendarConfig);
		// this.setNewLoggingDirectory(); //sets logging directory to the value of: calendarConfig.getWaveformLoggingDirectory()
		
		this.transact = new Transaction(calendarConfig, appState, observableDialog);
		transact.runTasks(); // this should run TrAvailable
		transact.nextStep();
		System.out.println(transact.status());
		
		transact.runTasks(); // this should run TrInitGlobalProceses
		transact.nextStep();
		System.out.println(transact.status());

		// Create visual components while the previous processes initialise
		initSwingComponents();

		initServers();
	}


	/**
	 * 
	 */
	private void initServers() {
		transact.runTasks(); // this should run TrInitSpeech
		transact.nextStep();
		// System.out.println(transact.status());
		
		transact.runTasks(); // this should run TrInitDialServ
		transact.nextStep();
		System.out.println(transact.status());
		
		transact.runTasks(); // this should run TrInitDialClient
		transact.nextStep();
		System.out.println(transact.status());
		
		transact.runTasks(); // this should run TrReady
		transact.nextStep();
		System.out.println(transact.status());

	}


	private void initSwingComponents(){
		// createSwingComponents();
		calFrame = new CalendarFrame(this, "Calendar Application");
		calPanel = new CalendarPanel(this);

		calStatusBar = new StatusBar();

		//
		// Glue up visual components
		//
		Box boxCenter = Box.createVerticalBox();
		boxCenter.add(calPanel, null);
		boxCenter.add(Box.createVerticalGlue(), null);

		Box debugBox = Box.createHorizontalBox();
		Box textComponents = Box.createVerticalBox();
		debugBox.add(textComponents);
		boxCenter.add(debugBox);

		calFrame.getContentPane().setLayout(new BorderLayout());
		calFrame.getContentPane().add(boxCenter, BorderLayout.CENTER);	
		calFrame.getContentPane().add(calStatusBar, BorderLayout.SOUTH);
		calFrame.pack();
		//
		// resize the window
		//
		//Dimension currentSize = calFrame.getSize();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		calFrame.setSize((int) (screenSize.getWidth() * 0.8), (int)(screenSize.getHeight()*0.9) );

		calFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		calFrame.addWindowListener(new WindowAdapter() {
			public void windowClosed(WindowEvent arg0) {
				exitApp();
			}
		});
	}

	public Dialogue getDialogue() {
		return observableDialog;
	}

	
	public void processRecognitionAbortRequest() {
		calRecHandler.abortRecognition();
	}

	
	protected void repaintCalendarPanel(){
		calPanel.repaint();
	}

	protected void validateCalendarPanel(){
		calPanel.validate();
	}


	/**
	 * @return
	 */
	protected CalendarConfiguration getCalendarConfiguration() {
		return calendarConfig;
	}

	/**
	 * @return ApplicationState
	 */
	protected ApplicationState getApplicationState() {
		return appState;
	}
	
	/**
	 * @param args
	 * @param i
	 */
	private static void createLogFile(String[] args, int i) {
		try {

			//
			// create the directory & the logStream file
			//
			boolean success = false;
			File loggingDir = new File(args[i + 1]);

			if (!loggingDir.exists()) {
				success = loggingDir.mkdirs();
			}
			if (!success) {
				System.err
				.println("Unable to create logging directory '"
						+ args[i + 1] + "'");
			}

			FileHandler fh = null;
			fh = new FileHandler(args[i + 1] + "/calendarGUI-%g.txt",
					1000000, 5, false);
			fh.setFormatter(new SimpleFormatter());
			Logger.getLogger("").addHandler(fh);
		} catch (SecurityException e1) {
			e1.printStackTrace();
		} catch (IOException e1) {
			e1.printStackTrace();
		}
	}


	/**
	 * Reconfigures in order to change the language (e.g. from english to japanese).
	 * 
	 */
	public void reconfigureToChangeLanguage(int languageIndex){
		calStatusBar.setStatusMessage("Stopping recognition and dialogue processes...");

		transact.setState(TrConstants.EXIT_APP.getState());
		transact.runTasks();
		transact.removeVisitedStates();
		
		// TODO: More detailed testing for this method		
		/*
		if (dialServHandler != null )    	
			// 	disconnect client from dialogue server and then kill dialogue server 
			disconnectFromDialogueServer(dialClient);   	 
		if (calRecHandler != null) 
			calRecHandler.shutdown();
*/
		
		calStatusBar.setStatusMessage("Recognition and dialogue processes stoped.");
		appState.setLanguageIndex(languageIndex);	               
		calStatusBar.setStatusMessage("Restart recognition and dialogue processes.");

		this.transact = new Transaction(calendarConfig, appState, observableDialog);
		transact.runTasks(); // this should run TrAvailable
		transact.nextStep();
		System.out.println(transact.status());
		
		transact.runTasks(); // this should run TrInitGlobalProceses
		transact.nextStep();
		System.out.println(transact.status());

		initServers();

		/*
		try {

			// Initialise the recognition server (i.e. vocalizer, regserver and recserver)
			calRecHandler = new SpeechHandler(calendarConfig, appState);			
			calRecHandler.startup();
			calRecHandler.setParam("client.WriteWaveforms", "1");
			calRecHandler.setParam("client.RecordDirectory", appState.getLoggingDirectory());

			// Initialize dialogue server 			
			dialServHandler = new DialogueServerHandler(calendarConfig, appState);			
			try{
				dialServHandler.startup();
			}			
			catch(Exception e){
				logger.log(Level.SEVERE, "Couldn't create dialog server process! ");
			}

			//////////////////////
			// Connect to dialogue server
			// System.out.println("Connecting to dialogue server" + calendarConfig.getDialogueServerHost() + "  " + calendarConfig.getDialogueServerPort() + " " + calendarConfig.getDialogueServerCharset());
			boolean connection_refused = true;
			int noTry = 0;
			while (connection_refused && (noTry++ < 6) ){
				this.dialClient = this.connectToDialogueServer(calendarConfig.getDialogueServerHost(), calendarConfig.getDialogueServerPort(), calendarConfig.getDialogueServerCharset());
				if (this.dialClient != null){
					connection_refused = false;	
				}

			}

			processPlayRequest("SAY_LIST -tts_text: Ready to start");
		} catch (Exception e) {
			String errorMessage = "The following error occured while initiating the system:\n";
			errorMessage += e.getMessage();
			errorMessage += "\nPlease check your configuration and try again.";

			logger.log(Level.SEVERE, "Could not start all required processes.",
					e);
			exitApp();
		}
		*/

	}
	
	public void exitApp() {
		calStatusBar.setStatusMessage("Stopping auxilary processes...");
		transact.setState(TrConstants.EXIT_APP.getState());
		transact.runTasks();
		calStatusBar.setStatusMessage("Auxilary processes have been stoped.");
	
		calFrame.setVisible(false);
		calFrame.dispose();
		System.exit(0);
	}
	

	protected void setHistoryShowing(boolean show){
		calPanel.getJScrollPaneDialogueContext().setVisible(show);
	}

	protected void setNBestShowing(boolean show){
		calPanel.getJScrollPaneRawRecResult().setVisible(show);
	}


	/**
	 * 
	 */
	public static void main(String[] args) {
		String configFile = null;
	
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-logdir") || args[i].equals("-ld")
					&& (i + 1 <= args.length)) {
				createLogFile(args, i);
			} else if ((args[i].equals("-config") || args[i].equals("-conf"))
					&& (i + 1 <= args.length)) {
				configFile = args[i + 1];
			}
		}
		//
		// Set up the logger:
		// The root logger's handlers default to ALL. We have to
		// crank them up. We could crank up only some of them
		// if we wanted, but we will turn them all up.
		Handler[] handlers = Logger.getLogger("").getHandlers();
		for (int index = 0; index < handlers.length; index++) {
			handlers[index].setLevel(Level.ALL);
		}
		logger.setLevel(Level.SEVERE);
	
		//
		// Set the platform's native look and feel
		//
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			e.printStackTrace();
		}
	
		//
		// Create and make visible the application Swing components 
		//
		Calendar mainCalendar = new Calendar(configFile);
		mainCalendar.calFrame.setVisible(true);
	
	}
	
}
