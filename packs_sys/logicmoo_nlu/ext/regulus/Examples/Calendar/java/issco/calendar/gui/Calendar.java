package issco.calendar.gui;

import issco.calendar.socket.*;

import net.sf.regulus.RegResultNBest;
import net.sf.regulus.NBestBean;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import javax.swing.Box;
import javax.swing.JFrame;
import javax.swing.UIManager;



/**
 * GUI for Calendar - Reglus Application
 */
public class Calendar {
	private static final long serialVersionUID = -5496761026898207499L;

	private boolean updateRecognitionPanelControls = true;

	public static Logger logger = Logger.getLogger(Calendar.class.getPackage()
			.getName());

	
	/** configuration object shared throughout the app */
	private CalendarConfiguration calendarConfig = null;

	private ApplicationState calendarApplicationState = null;

	//
	// visual components
	//	
	private CalendarFrame calendarFrame = null;
	private CalendarPanel calendarPanel = null;
	private StatusBar calendarStatusBar = null;

	//
	// objects that interface to the outside world
	//
	private RecognitionHandler calendarRecognitionHandler = null;
	private InitializationProcessHandler calendarInitializationProcessHandler = null;

	//
	// objects that handle application logging
	//	
	//private LogEntryDTO logEntry = null;
	private String logDir = null;

	private String msg4DialogueServer = ""; // ex: "action(process_nbest_list([nbest_hyp(30,when is manny),.."
	ClientCom dialogueClient = null;
	
	private Object syncObject = null;


	/**
	 * @return ApplicationState
	 */
	protected ApplicationState getCalendarApplicationState() {
		return calendarApplicationState;
	}

	/*
	 * Public constructor for Calnder application
	 */
	public Calendar(String configFile) {
		
		syncObject = new Object();

		// Create configuration
		calendarConfig = new CalendarConfiguration(configFile);
		calendarApplicationState = new ApplicationState(calendarConfig);
		// Set logging directory to the value of: calendarConfig.getWaveformLoggingDirectory()
		setNewLoggingDirectory(calendarApplicationState, calendarConfig.getWaveformLoggingDirectory()); 

		//
		// Start up any processes defined in global.process.N (N = 1, 2,
		calendarInitializationProcessHandler = new InitializationProcessHandler(calendarConfig);
		
		try {
			calendarInitializationProcessHandler.startup();
			// wait for a while here in order to have the dialogue server started
			
		} catch (Exception e) {
			String errorMessage = "The following error occured while initiating the system:\n";
			errorMessage += e.getMessage();
			errorMessage += "\nPlease check your configuration and try again.";

			logger.log(Level.SEVERE, "Could not start all required processes. Error occuring when creating Calendar Main App",
					e);
			exitApp();			
		}


		// Create visual components while the previous processes initialize
		initSwingComponents();

		try {
			// Initialize the recognition server (i.e. both regserver and recserver)
			calendarRecognitionHandler = new RecognitionHandler(this,
					calendarConfig, calendarApplicationState);
			
			calendarRecognitionHandler.startup();
			calendarRecognitionHandler.setParam("client.WriteWaveforms", "1");
			// calendarRecognitionHandler.setParam("client.RecordDirectory", "D:/Regulus/Examples/Calendar/corpora/speech/");
			calendarRecognitionHandler.setParam("client.RecordDirectory", logDir);

			// Connect to dialogue server
			// System.out.println("Connecting to dialogue server" + calendarConfig.getDialogueServerHost() + "  " + calendarConfig.getDialogueServerPort() + " " + calendarConfig.getDialogueServerCharset());
			boolean connection_refused = true;
			int noTry = 0;
			while (connection_refused && (noTry++ < 6) ){
				this.dialogueClient = this.connectToDialogueServer(calendarConfig.getDialogueServerHost(), calendarConfig.getDialogueServerPort(), calendarConfig.getDialogueServerCharset());
				if (this.dialogueClient != null){
					// System.out.println("Connection to the dialogue server, attempt no " + noTry);								
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
	
	}


	private void initSwingComponents(){
		// createSwingComponents();
		calendarFrame = new CalendarFrame(this, "Calendar Application");
		calendarPanel = new CalendarPanel(this,
				calendarConfig);

		calendarStatusBar = new StatusBar();

		//
		// Glue up visual components
		//
		Box boxCenter = Box.createVerticalBox();
		boxCenter.add(calendarPanel, null);
		boxCenter.add(Box.createVerticalGlue(), null);

		Box debugBox = Box.createHorizontalBox();
		Box textComponents = Box.createVerticalBox();
		debugBox.add(textComponents);
		boxCenter.add(debugBox);

		calendarFrame.getContentPane().setLayout(new BorderLayout());
		calendarFrame.getContentPane().add(boxCenter, BorderLayout.CENTER);	
		calendarFrame.getContentPane().add(calendarStatusBar, BorderLayout.SOUTH);
		calendarFrame.pack();
		//
		// resize the window
		//
		Dimension currentSize = calendarFrame.getSize();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		calendarFrame.setSize((int) (screenSize.getWidth() * 0.8), (int)(screenSize.getHeight()*0.9) );

		calendarFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		calendarFrame.addWindowListener(new WindowAdapter() {
			public void windowClosed(WindowEvent arg0) {
				exitApp();
			}
		});
	}

	/**
	 * Invokes the dialogue server to process the N-best list (previously received from the regServer) 
	 * @param msg4DialogueServer message to be send to the dialogue server
	 * @return reply from the dialogue server
	 */
	protected String sendRequestToDialogueServer(String msg4DialogueServer){
		String reply = null;
		
		if (dialogueClient != null){
			try {
				reply = this.dialogueClient.sendRequest(msg4DialogueServer);
			}
			catch (ServerConnectionException e) {
				logger.log(Level.WARNING, "", e);
				System.out.println("WARNING: ");
				e.printStackTrace();
			}
		}
		else{
			System.out.println("dialogue client not connected");
		}
		return reply;
	}
	
	/**
	 * Creates a client of the dialogue server and send a client connection request.  
	 */
	private ClientCom connectToDialogueServer(String host, int serverPort, java.nio.charset.Charset charSet){		
		try {
			// CLIENT OF THE DIALOGUE SERVER:
			//this.dialogueClient = new ClientCom("localhost", 1985, DEFAULT_CHARSET);
			this.dialogueClient = new ClientCom(host, serverPort, charSet);
			
			String clientRequest1 = "client('localhost').";
			this.dialogueClient.sendRequest(clientRequest1);
			return this.dialogueClient;
		}
		catch (ServerConnectionException e) {
			logger.log(Level.WARNING, "", e);
			System.out.println("WARNING: ");
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Sends to the dialogue server the message "dissconnect"
	 * and closes the clientCom connection. 
	 * @param ClientCom cc 
	 */
	private void disconnectFromDialogueServer(ClientCom cc){		
		try {
			//String clientRequest = "disconnect.";
			//cc.sendRequest(clientRequest);		
			
			String clientRequest2 = "client_shutdown.";
			cc.sendRequest(clientRequest2);
			
			cc.close();
		}
		catch (ServerConnectionException e) {
			logger.log(Level.WARNING, "", e);
			e.printStackTrace();
		}
		catch (IOException e) {
			logger.log(Level.SEVERE, "IO Exception when closing connection ");
			e.printStackTrace();
		}		
	}
	
	
	/**
	 * @param recResult
	 */
public void processRecResultAndInvokeDialogueServer(String recResult, String interpret4DialogueServer) {
	logger.entering(this.getClass().getName(), "processRecognitionResult");
	logger
		.warning(" entering processRecognitionResult -- updateRecognitionPanelControls is "
						+ updateRecognitionPanelControls);

	synchronized (syncObject) {
		// logEntry.setPrimaryRecognitionResult(recResult);
		/*
		 * logEntry.setSecondaryRecognitionResult(calendarRecognitionHandler.getSecondaryRecognitionResult()); 
		 */
		// logEntry.setPrimaryGrammar(calendarRecognitionHandler.getPrimaryGrammar());
		// logEntry.setSecondaryGrammar(calendarRecognitionHandler.getSecondaryGrammar());
		// logEntry.setWavfile(calendarRecognitionHandler.getLastWavFileRecorded());
		// logEntry.setBackTranslationResult(talkbackResult);

		// temporary hack for displaying question marks
		String recResultShow = recResult;
		// String talkbackResultShow = talkbackResult;

		if (!Utils.isStringEmpty(recResultShow)) {
			recResultShow = recResultShow + "?";			
		}
		// process the recResult to clean it up
		java.util.ArrayList<NBestBean> speechResponseArray = RegResultNBest.formatNBestRegResponse(recResultShow);
		calendarPanel.setRawResultTable(speechResponseArray);
		
		String dialogueServerReply;
/*			
			if (recResult.indexOf("recognition_failed(") > -1){
				// DON'T Send msg to dialogue server
				calendarRecognitionPanel.setRecognitionButtonEnabled(true);
				calendarRecognitionPanel.setRecognitionText("?");
//				try{
					//
//					processPlayRequest("SAY_LIST -tts_text: Recognition Failed ");
//				}
//				catch(Exception ex){					
//				}
				
				// return;
//				dialogueServerReply = "";
			}
			else{
*/				
		// Send msg to dialogue server				
		dialogueServerReply = this.sendRequestToDialogueServer(interpret4DialogueServer);
		//}
		// show to the user the selected interpretation, 
		// i.e. to which question the system is answering
		int idx1 = dialogueServerReply.indexOf("selected");
		int idx2 = dialogueServerReply.indexOf("action");
		if ((idx1 !=-1) && (idx2 != -1)){
			String selectedInterpretation = dialogueServerReply.substring(idx1 + 9, idx2-2);
			if (selectedInterpretation.indexOf("'") == 0){
				selectedInterpretation = selectedInterpretation.substring(1, selectedInterpretation.length());
			}
			if (selectedInterpretation.lastIndexOf("'") == selectedInterpretation.length() ){
				selectedInterpretation = selectedInterpretation.substring(0, selectedInterpretation.length()-1);
			}
			calendarPanel.setTextAreaSelectedInterpretation(selectedInterpretation);
			calendarPanel.addTextToDialogueHistoryTextArea("User   : " + selectedInterpretation);
						
		}
		else{
			if (dialogueServerReply.indexOf("unknown_request") > -1)
				calendarPanel.setTextAreaSelectedInterpretation("");			
			else
				if (dialogueServerReply.indexOf("Sorry, something went wrong") > -1)
					calendarPanel.setTextAreaSelectedInterpretation("");				
				else
					// tts('Sorry, something went wrong').
					calendarPanel.setTextAreaSelectedInterpretation(dialogueServerReply);
		}
			
		// Take the response from the dialogue server and send it to the TTS engine		
		if (dialogueServerReply.equalsIgnoreCase("ok")){}
		else{
			if (dialogueServerReply.indexOf("unknown_request") > -1){
				dialogueServerReply = "Sorry\\, unknown request.";
			}
			else{
				// dialogueServerReply = dialogueServerReply.replace("tts(", "");
				int indexTTS = dialogueServerReply.indexOf("tts(");
			
				dialogueServerReply = dialogueServerReply.replace(")", "");
				dialogueServerReply = dialogueServerReply.replace("]", "");
				dialogueServerReply = dialogueServerReply.replace(".", "");
				
				dialogueServerReply = dialogueServerReply.substring(indexTTS + 4, dialogueServerReply.length());
				if (dialogueServerReply.indexOf("'") == 0){
					dialogueServerReply = dialogueServerReply.substring(1, dialogueServerReply.length());
				}
				if (dialogueServerReply.lastIndexOf("'") == dialogueServerReply.length()-1){
					dialogueServerReply = dialogueServerReply.substring(0, dialogueServerReply.length()-1);					
				}

				dialogueServerReply = dialogueServerReply.replace("\\'", "'");
				dialogueServerReply = dialogueServerReply.replace(",", "\\,");
			}
			try{
				long currentTime = System.currentTimeMillis();
				processPlayRequest("SAY_LIST -tts_text: " + dialogueServerReply);			
			}
			catch(Exception e){
				logger.log(Level.SEVERE, "Problems when communicating with the TTS engine.");
				e.printStackTrace();
			}
			String dialogueServerReplyToShow = dialogueServerReply.replace("\\", "");
			long currentTime = System.currentTimeMillis();
			
			calendarPanel.setMsgFromDialServTextArea(dialogueServerReplyToShow);
			calendarPanel.addTextToDialogueHistoryTextArea("System : " + dialogueServerReplyToShow);
			calendarPanel.addTextToDialogueHistoryTextArea("------------");

			/* Communicate with the regserver and the dialogue server in order to obtain the help sentences
			 	First : query the regserver to get the most recent recorded file
		 		Second: Take the SLM recognition result, format it into a help request
				and send that to the dialogue server
				receive the help response
		
		 * Example:
		 * --- Sending message to Regserver: "GET_PARAMETER client.FilenameRecorded
		 * --- Received message from Regserver: "parameter(string,'client.FilenameRecorded','..\\corpora\\speech\\2007-10-19_15-44-10\\utt01.wav')"
		 * --- Current wavfile: ..\corpora\speech\2007-10-19_15-44-10/utt01.wav
		 * --- Sending message to Regserver: "RECOGNISE_FILE d:/regulus/examples/calendar/corpora/speech/2007-10-19_15-44-10/utt01.wav .MAIN_SLM
		 * --- Received message from Regserver: "recognition_succeeded([rec_result(49,'for zero ten',[])])"
		 	* --- Sending message to dialogue server: action(get_help_examples(for zero ten,5))
		 	*/

			// Object oResponse = getParameterFromRegServer("client.FilenameRecorded");
			String lastWavFileRecorded = calendarRecognitionHandler.getLastWavFileRecorded();
			// calendarRecognitionHandler.getParamWriteWaveForms(); // debug only
			
			lastWavFileRecorded = lastWavFileRecorded.replace("\\", "/");
			lastWavFileRecorded = lastWavFileRecorded.toLowerCase();
			System.out.println("Current wavfile: " + lastWavFileRecorded);
			// --- Sending message to Regserver. Eg: "RECOGNISE_FILE d:/regulus/examples/calendar/corpora/speech/2007-10-19_15-44-10/utt01.wav .MAIN_SLM
			try{
				// calendarRecognitionHandler.sendMessageToRegClient("RECOGNISE_FILE " + lastWavFileRecorded + " .MAIN_SLM");
				String SLMresult = calendarRecognitionHandler.recognizeFile(lastWavFileRecorded, ".MAIN_SLM");
				// --- Sending message to dialogue server: action(get_help_examples(for zero ten,5))
				String action = "action(get_help_examples(" + SLMresult + "," + calendarConfig.getDialogueNoHelpSentences() + "))."; 
				String helpSent = this.sendRequestToDialogueServer(action);
				//	System.out.println("Help sentences: " + helpSent);
				// PROCESS DIALOGUE RESPONSE AND SHOW IT IN CALENDAR PANEL
				DialogueHelpResult dhr = new DialogueHelpResult(helpSent);
				//calendarPanel.addTextToHelpJList(dhr.getHelpSentences());
				calendarPanel.setTextForHelpJList(dhr.getHelpSentences());				
			}
			catch(Exception ex){
				System.out.println("ERR when communication with regserver in order to do rec with SLM grammar");
			}
			
			if (updateRecognitionPanelControls) {
				calendarPanel.setRecognitionInProgress(false); 
				//	calendarRecognitionPanel.setRecognitionTipHelpSentence(helpSeed);
			}

		}// end else
		logger.warning("XXXX exiting processRecognitionResult");
		logger.exiting(this.getClass().getName(), "processRecognitionResult");
		}// end synchronised
	}

	
	/**
	 * 
*/
	public void processRecognitionRequest() {
		logger.entering(this.getClass().getName(), "processRecognitionRequest");
		try{
			
		synchronized (syncObject) {
			/*
			if (logEntry == null) {
				logEntry = new LogEntryDTO();
			} 
			logEntry.clear();
			*/
			calendarPanel.setRecognitionInProgress(true); // change the text on the button into "Abort Recognition"  
			calendarRecognitionHandler.startRecognition();
		}
		
		}
		catch(Exception ex){
			System.out.println("Exception because calendarRecognitionHandler.startRecognition()");
			ex.printStackTrace();
		}
		logger.exiting(this.getClass().getName(), "processRecognitionRequest");
	}

	public void processRecognitionAbortRequest() {
		// logEntry.clear();
		calendarRecognitionHandler.abortRecognition();
	}

	protected void repaintCalendarPanel(){
		calendarPanel.repaint();
	}
	
	protected void validateCalendarPanel(){
		calendarPanel.validate();
	}
	
	/*
	private void writeLogEntry() {
		try {
			String wav = logEntry.getWavfile();
			boolean writeLog = true;

			if (!Utils.isStringEmpty(wav)) {
				if (!wav.endsWith(".wav")) {
					writeLog = false;
				}
			}

			if (writeLog) {
				logStream.write(logEntry.toXMLString().getBytes());
				logStream.flush();
			}
		} catch (IOException e) {
			logger.log(Level.WARNING, "", e);
		}
	}
	*/
		
	  public void processPlayRequest(String msg) throws Exception{
		  calendarRecognitionHandler.sendMessageToRegClient(msg);
	  }
	
	/**
	 * Sets logDir attribute to a new value
	 * @param ApplicationState
	 * @param String newLogDir provide the value of the new logging directory
	 */
	private void setNewLoggingDirectory(ApplicationState newAppState, String newLogDir) {
		//
		// close 'old' logStream
		//
		/*
		try {
			if (logStream == null) {
			} else {
				logStream.write("\n</log>\n".getBytes());
				logStream.close();
			}
		} catch (IOException e) {
			logger.log(Level.WARNING, "Could not close logStream", e);
		}
*/
		//
		// Get new logging directory
		//
		java.text.SimpleDateFormat date = new SimpleDateFormat("yyyy-MM-dd");
		SimpleDateFormat time = new SimpleDateFormat("HH.mm.ss.SSS");
		Date now = new Date();

		StringBuffer tmp = new StringBuffer();
		tmp.append(newLogDir);

		if (!(newLogDir.endsWith("/") || newLogDir.endsWith("\\"))) {
			tmp.append("/");
		}
		tmp.append(date.format(now));
		// tmp.append("/");
		// tmp.append(newAppState.getLanguage().getLanguageName());
		tmp.append("/");
		tmp.append(time.format(now));

		logDir = tmp.toString();

		//
		// create the directory & re-create the logStream file
		//
		boolean success = false;
		success = (new File(logDir)).mkdirs();

		if (!success) {
			logger.warning("Could not create logging directory.");
			newAppState.setLoggingDirectory(null);
			/*
			logStream = new BufferedOutputStream(System.out);
			*/
		} else {
			newAppState.setLoggingDirectory(logDir);
			
			/*
			String logFileName = logDir + "/calendar_log.xml";
			String xsltFile = "log2transcription.xsl";

			Utils.createXSLTFile(logDir + "/" + xsltFile);

			try {
				OutputStream os = new FileOutputStream(logFileName);
				logStream = new BufferedOutputStream(os);
				logStream
						.write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>\n"
								.getBytes());
				logStream.write(("<?xml-stylesheet type=\"text/xsl\" href=\""
						+ xsltFile + "\"?>\n").getBytes());
				logStream
						.write(("<!-- "
								+ CalendarConfiguration.getReleaseInformation() + " -->\n")
								.getBytes());
				logStream.write("<log>\n".getBytes());
			} catch (FileNotFoundException e) {
				logger.log(Level.WARNING, "Could not open '" + logFileName
						+ "'", e);
				logStream = new BufferedOutputStream(System.out);
			} catch (IOException e) {
				logger.log(Level.WARNING, "Could not write to '" + logFileName
						+ "'", e);
			}
			*/
		}
	}

	/**
	 * @return
	 */
	protected CalendarConfiguration getCalendarConfiguration() {
		return calendarConfig;
	}

	/**
	 * @return
	 */
	/*
	protected LogEntryDTO getLogEntry() {
		return logEntry;
	}
*/
	/**
	 * @return
	 */
	protected ApplicationState getApplicationState() {
		return calendarApplicationState;
	}

	/**
	 * 
	 */
	/*
	public void showAboutDlg() {
		JDialog aboutDialog = new AboutDlg(this, calendarConfig);
		aboutDialog.setVisible(true);
	}
*/
		public static void main(String[] args) {
		String configFile = null;

		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-logdir") || args[i].equals("-ld")
					&& (i + 1 <= args.length)) {
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
			} else if ((args[i].equals("-config") || args[i].equals("-conf"))
					&& (i + 1 <= args.length)) {
				configFile = args[i + 1];
			}
		}

		//
		// Set up the logger
		//
		// The root logger's handlers default to ALL. We have to
		// crank them up. We could crank up only some of them
		// if we wanted, but we will turn them all up.
		Handler[] handlers = Logger.getLogger("").getHandlers();
		for (int index = 0; index < handlers.length; index++) {
			handlers[index].setLevel(Level.ALL);
		}
		logger.setLevel(Level.ALL);

		//
		// Set the platform's native look and feel
		//
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			e.printStackTrace();
		}

		//
		// Create the app
		//
		Calendar mainCalendar = new Calendar(configFile);

		mainCalendar.calendarFrame.setVisible(true);
		
	}

	/**
	 * @param b
	 */
	public void setCPUTimeShowing(boolean b) {
		calendarRecognitionHandler.setComputeCPUTimeEnabled(b);
	}

    public void exitApp() {
    	// undo this comment : for debug only
   	 disconnectFromDialogueServer(dialogueClient);
        calendarStatusBar.setStatusMessage("Stopping auxilary processes...");
  
        if (calendarRecognitionHandler != null) {
        	calendarRecognitionHandler.shutdown();
        }

        if (calendarInitializationProcessHandler != null) {
        	calendarInitializationProcessHandler.shutdown();
        }

        calendarStatusBar.setStatusMessage("Stopping auxilary processes... done.");
        
        /*
        try {
            logStream.write("\n</log>".getBytes());
            logStream.close();
        }
        catch (IOException e1) {
        }
*/
        
        calendarFrame.hide();
        System.exit(0);
    }

    
    protected void setHistoryShowing(boolean show){
    	calendarPanel.getJScrollPaneDialogueContext().setVisible(show);
    }
    
    protected void setNBestShowing(boolean show){
    	calendarPanel.getJScrollPaneRawRecResult().setVisible(show);
    }
}
/*
 * Example of communication with the dialogue server and and regserver:
 * --- Sending message to Regserver: "RECOGNISE .MAIN
"
--- Received message from Regserver: "recognition_succeeded([rec_result(40,'were maria at this eleventh meeting on march twenty five',[value=[[ynq,[[v
erb_type,pp],[tense,past],[verb,be],[subj,[[spec,name],[head,maria]]],[subcat_pp,[[at_meeting,[[spec,[ordinal,this,11]],[head,meeting],[on_date,[[spec
ial_np,date],[month,3],[day,25]]]]]]]]]]]),rec_result(39,'were marianne at this eleventh meeting on march twenty five',[value=[[ynq,[[verb_type,pp],[t
ense,past],[verb,be],[subj,[[spec,name],[head,marianne]]],[subcat_pp,[[at_meeting,[[spec,[ordinal,this,11]],[head,meeting],[on_date,[[special_np,date]
,[month,3],[day,25]]]]]]]]]]]),rec_result(39,'were maria at that eleventh meeting on march twenty five',[value=[[ynq,[[verb_type,pp],[tense,past],[ver
b,be],[subj,[[spec,name],[head,maria]]],[subcat_pp,[[at_meeting,[[spec,[ordinal,that,11]],[head,meeting],[on_date,[[special_np,date],[month,3],[day,25
]]]]]]]]]]]),rec_result(39,'one is my address of the eleventh meeting on march twenty five',[value=[[whq,[[subj,[[spec,1],[head,null]]],[verb_type,tra
ns],[tense,present],[verb,be],[obj,[[spec,null],[possessive,[[[spec,pro],[head,i]]]],[head,address],[of,[[spec,[ordinal,the_sing,11]],[head,meeting],[
on_date,[[special_np,date],[month,3],[day,25]]]]]]]]]]]),rec_result(38,'one is my address of that eleventh meeting on march twenty five',[value=[[whq,
[[subj,[[spec,1],[head,null]]],[verb_type,trans],[tense,present],[verb,be],[obj,[[spec,null],[possessive,[[[spec,pro],[head,i]]]],[head,address],[of,[
[spec,[ordinal,that,11]],[head,meeting],[on_date,[[special_np,date],[month,3],[day,25]]]]]]]]]]]),rec_result(38,'will marianne santaholma attend the m
eeting on march twenty five',[value=[[ynq,[[tense,future],[subj,[[spec,name],[head,marianne_santaholma]]],[verb_type,trans],[verb,attend],[obj,[[spec,
the_sing],[head,meeting],[on_date,[[special_np,date],[month,3],[day,25]]]]]]]]])])"

Received from recogniser:

recognition_succeeded([rec_result(40, were maria at this eleventh meeting on march twenty five,
 * --- Sending message to dialogue server: action(process_nbest_list([nbest_hyp(40,were maria at this eleventh meeting on march twenty five),nbest_hyp(39
,were marianne at this eleventh meeting on march twenty five),nbest_hyp(39,were maria at that eleventh meeting on march twenty five),nbest_hyp(39,one
is my address of the eleventh meeting on march twenty five),nbest_hyp(38,one is my address of that eleventh meeting on march twenty five),nbest_hyp(38
,will marianne santaholma attend the meeting on march twenty five)]))
 * --- Received reply from dialogue server: [selected=were maria at this eleventh meeting on march twenty five,action=tts(no)]

Interpreted as: "were maria at this eleventh meeting on march twenty five"
--- Sending message to Regserver: "SAY_LIST -tts_text: no
"
--- Sending message to Regserver: "GET_PARAMETER client.FilenameRecorded
"
--- Received message from Regserver: "parameter(string,'client.FilenameRecorded','..\\corpora\\speech\\2007-11-08_14-41-17\\utt06.wav')"

--- Current wavfile: ..\corpora\speech\2007-11-08_14-41-17\\utt06.wav

--- Sending message to Regserver: "RECOGNISE_FILE d:/regulus/examples/calendar/corpora/speech/2007-11-08_14-41-17/utt06.wav .MAIN_SLM
"
--- Received message from Regserver: "recognition_succeeded([rec_result(43,'were idiap at this eleventh meeting on march ninety five',[])])"

--- Sending message to dialogue server: action(get_help_examples(were idiap at this eleventh meeting on march ninety five,5))

--- Received reply from dialogue server: help(will it be a meeting on july nine
was there a meeting on september nine
was there a meeting on july seven
was there a meeting on july nine
is there a meeting on september ten)

HELP EXAMPLES FOR STRING "were idiap at this eleventh meeting on march ninety five":

will it be a meeting on july nine
was there a meeting on september nine
was there a meeting on july seven
was there a meeting on july nine
is there a meeting on september ten
 *
 */

