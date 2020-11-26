package issco.calendar.socket;

import issco.calendar.gui.Calendar;
import issco.calendar.gui.CalendarConfiguration;
import issco.calendar.gui.Language;
import issco.calendar.gui.ApplicationState;

import java.io.IOException;

import java.util.logging.Level;
import java.util.logging.Logger;

import net.sf.regulus.RegClient;
import net.sf.regulus.RegResultNBest;


/**
 * 
 * The method startupRecognitionServer runs the external "recserver" command.
 *
 */
public class RecognitionHandler implements IProcessHandler { 
    private static Logger logger = Logger.getLogger(RecognitionHandler.class.getName());
 
    private final Calendar calendarRootComponent;
    private final CalendarConfiguration calendarConf;
    private Language currentLanguage = null;
    
    private static RegClient regClient = null;

    private Process recServerProcess = null;
    private Process ttsProcess = null;

    private String promptPrefix = "";
    private String grammar = null;
    private String currentRecognitionPackage = null;
    private String lastWavFileRecorded = null;
    private String logDirectory = null;

    private String interpret4DialogueServer = null ; // this attribute is set up after receiving the recognition result, i.e. inside the startRecognition() method

    private boolean lastWavFileRetrieved = false;
    static private boolean computeXCPUTime = false; 

    String backupPromptPath[] = null;
    
    
    
    /*
     * State variables to hold results during
     * different processes that are going on.
     * This has to go.
     */   
    private String stateVarRecognitionResult = null;
    private String stateVarHelpSeed = null;
    private String stateVarSourceRepresentation = null;

//    private NGramHelpGenerator nGramHelpGenerator = null;

    /**
     * Constructor 
     * @param calendarRootComponent
     * @param calendarConf
     * @param appState
     */
    public RecognitionHandler(final Calendar calendarRootComponent, CalendarConfiguration calendarConf, ApplicationState appState) {
        logger.setLevel(Level.ALL);
        this.calendarRootComponent = calendarRootComponent;
        this.calendarConf = calendarConf;
        currentLanguage = appState.getLanguage();
        
        logDirectory = appState.getLoggingDirectory();
        
        computeXCPUTime = calendarConf.computeXCPURT();
        
    }
    
    public void startup() throws Exception {
        startupRecognition();
        startupPlayback();
    }

     
    /**
     * Run vocaliser with tts parameters.  
     */
    private void startupPlayback() {
        if (currentLanguage.isUsingTTS()) {
            promptPrefix = "";
            String ttsParams = currentLanguage.getPlaybackParameters();
            try {

                ttsProcess = Runtime.getRuntime().exec("vocalizer " + ttsParams);
            }
            catch (IOException e) {
                new Exception("Failed to start tts server");
            }
        }
        else {
            try {
                regClient.abortPlayback();
            }
            catch (IOException e) {
                logger.log(Level.WARNING, "Failed to issue abort playback command.", e);
            }
            promptPrefix = currentLanguage.getPlaybackParameters();
        }
    }
    /**
     * stops any TTS processes running
     */
    private void shutdownPlayback() {
        if ((currentLanguage != null) && currentLanguage.isUsingTTS() && (ttsProcess != null)) {
            ttsProcess.destroy();
            try {
                Thread.sleep(calendarConf.getVocalizerKillWaitTime());
            } catch (InterruptedException e) {
            }
            ttsProcess = null;
        }
    }

    public boolean shutdown() {
        shutdownRecognition();
        shutdownPlayback();
        return true;
    }

    /**
     * Invoke Regserver with GLM 
     * @throws Exception
     */
    private void startupRegServer() throws Exception {
        String recognitionPackage = currentLanguage.getRecognitionPackage();
        String[] globalClientParams = currentLanguage.getRecognitionClientParameters();
        
        grammar = currentLanguage.getGLMGrammar();
        logger.finest("Grammar set to '" + grammar + "'");

        StringBuffer regClientConfig = new StringBuffer();
        String[] configParams = new String[4 + globalClientParams.length];
        
        configParams[0] = "-package";
        regClientConfig.append(configParams[0]);
        regClientConfig.append(" ");
               
        configParams[1] = recognitionPackage;
        regClientConfig.append(configParams[1]);
        regClientConfig.append(" ");
        
        configParams[2] = "-port";
        regClientConfig.append(configParams[2]);
        regClientConfig.append(" ");
        
        configParams[3] = new Integer(calendarConf.getRegulusServerPort()).toString();
        regClientConfig.append(configParams[3]);
        regClientConfig.append(" ");

        for (int i = 0; i < globalClientParams.length; i++) {
            configParams[i+4] = globalClientParams[i];
            regClientConfig.append(globalClientParams[i]);
            regClientConfig.append(" ");
        }

        // Create the SpeechChannel object
        regClient = new RegClient(calendarConf.getRegulusServerHost(), calendarConf.getRegulusServerPort(), regClientConfig.toString(), 
        		calendarConf.getRegulusServerCommand(), calendarConf.getRegulusServerTimeOut());

        // Set logging directory
        if (logDirectory != null) {
            regClient.setParameter("client.WriteWaveforms", "TRUE");
            regClient.setParameter("client.RecordDirectory", logDirectory);
        }
        else {
            regClient.setParameter("client.WriteWaveforms", "FALSE");
        }
    }

    /**
     * This simply runs the external "recserver" command
     * @throws Exception
     */
    private void startupRecognitionServer() throws Exception {
        logger.entering(this.getClass().getName(), "startupRecognitionServer");
        currentRecognitionPackage = currentLanguage.getRecognitionPackage();

        try {
            String recserverCommand = "recserver -package " + currentRecognitionPackage;
            logger.finest("Launching '" + recserverCommand + "'");
            recServerProcess = Runtime.getRuntime().exec(recserverCommand);
        }
        catch (IOException e) {
            throw new Exception("failed to start recognition server");
        }
        logger.exiting(this.getClass().getName(), "startupRecognitionServer");
    }
    
    /**
     * Starts recognition server and client
     * 
     * @throws FatalException
     * @throws NuanceConfigException
     */
    private void startupRecognition() throws Exception {
        startupRecognitionServer(); // run the external recserver.exe program 

        // 
        // allow recserver to startup
        //
        try {
            Thread.sleep(calendarConf.getStartupRecognitionServerWaitTime());
        }
        catch (InterruptedException e) {
        }

        startupRegServer(); // run the external program regserver.exe
        
    }

    /**
     * Start a recognition request in a new thread and 
     * @return true if the recognition was successful 
     * and the interpretation (message to be sent to the dialogue server)
     * was created   
     *  
     */
    public boolean startRecognition() {
    	RunnableRecognition rr = new RunnableRecognition(regClient, lastWavFileRetrieved, computeXCPUTime, lastWavFileRecorded, calendarConf, 
    			calendarRootComponent,stateVarRecognitionResult, stateVarHelpSeed, stateVarSourceRepresentation, grammar	);
    	new Thread(rr).start();
    	stateVarSourceRepresentation = rr.getRegResultInterpretation();
    	stateVarHelpSeed = rr.getStateVarHelpSeed(); 
    	stateVarRecognitionResult = rr.getStateVarRecognitionResult();
		lastWavFileRetrieved = rr.getLastWavFileRetrieved();
		
	    interpret4DialogueServer = rr.getInterpret4DialogueServer();
	    if (interpret4DialogueServer != null)
	    	return true;
	    else 
	    	return false;		
    }

      
    /**
     * Abort an ongoing recognition
     *  
     */
    public void abortRecognition() {
        try {
            regClient.abortRecognition();
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Gets the primaryGrammar used in recognition
     *  
     */
    public String getPrimaryGrammar() {
        return grammar;
    }

    private void shutdownRecognition() {
        //
        // disconnect the client
        //
        if (regClient != null) {
            try {
                regClient.shutdown();
                regClient = null;
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }

        //
        // kill the server process
        //
        if (recServerProcess != null) {
            recServerProcess.destroy();
            recServerProcess = null;
        }
    }

    
    /**
     * @param newAppState
     */
    protected void reconfigure(ApplicationState newAppState) throws Exception {

        Language inputLanguage = newAppState.getLanguage();
        logger.info("inputLang = " + inputLanguage.getLanguageName());

        logDirectory = newAppState.getLoggingDirectory();
        logger.finest("logDirectory = " + logDirectory);

        if (!inputLanguage.isImplementedLanguage()) {
            throw new Exception("'" + inputLanguage.getLanguageName() + "' is not a valid input language.");
        }

        if (!currentLanguage.equals(inputLanguage)) {
            currentLanguage = inputLanguage;
            logger.info("Stopping recognition services.");

            shutdownRecognition();
            logger.info("Restarting recognition services.");            
            
            startupRecognition();

            if(inputLanguage.isSuportingTalkback()){
	            logger.info("Reconfiguring talkback engine.");
            }

        }
                
       	     grammar = inputLanguage.getGLMGrammar();

        Language outputLanguage = newAppState.getLanguage();
        if (!currentLanguage.equals(outputLanguage)) {
            if (currentLanguage.isUsingTTS()) {
            	logger.info("Stopping TTS server");

                shutdownPlayback();
                logger.info("Starting TTS server.");
            }
            else {
            	logger.info("Initiating playback..");
            }

            currentLanguage = outputLanguage;
            startupPlayback();

        }        
        logger.info("Playback initialized.");
    }

    /**
     * @param string
     */
    public void startPlayback(String playback_string) throws Exception {
        abortPlayback();        
        String playback = playback_string.trim();        
        
        logger.info(currentLanguage.getLanguageName() + " is using TTS: " + currentLanguage.isUsingTTS());

        if (playback.startsWith("+wavfile_tts")) {
        	playback = playback.replace("+wavfile_tts", "");
        	playback = playback.replaceFirst("'", "");
        	playback = playback.substring(0, playback.length()-1);
        	
        	playback = playback.replaceAll("\\?", "");
            logger.info("___ playback request = '" + playback_string + "'");
            
            String[] prompts = playback.split(" ");
            String str = prompts[0];
            boolean isEmpty = ((str == null) || (str.trim().equals(""))); 
            if (!((prompts.length == 1) && (isEmpty))) {
                for (int i = 0; i < prompts.length; i++) {
                    prompts[i] = promptPrefix + "/" + prompts[i] + ".wav";
                    System.out.println("Playing file" + prompts[i]);
                }
                
                regClient.playList(prompts);
            }
        }

        if (playback.startsWith("+text")) {
           	logger.info("Text only output since one or more wavfiles are missing.");
            }        

        if (currentLanguage.isUsingTTS()) {
            if (playback.startsWith("+tts")) {
                playback = playback.replaceFirst("\\tts \"", "-tts_text:");
                playback = playback.replaceAll("\"$", "");
                logger.info("___ playback TTS request = '" + playback + "'");
                regClient.play(playback);
            }
        }
    }

    
    public void abortPlayback() {
        try {
            regClient.abortPlayback();
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * @return Returns the lastWavFileRecorded.
     * 
     */
    public String getLastWavFileRecorded() {
        if (!lastWavFileRetrieved) {
            try {
                lastWavFileRecorded = regClient.getStringParameter("client.FilenameRecorded");
            }
            catch (Exception e) {
                logger.log(Level.WARNING, "Could not retrieve last filename", e);
                lastWavFileRecorded = null;
            }
        }

        return lastWavFileRecorded;
    }
    
    /**
     * @return Returns the lastWavFileRecorded.
     * 
     */
    public int getParamWriteWaveForms() {
        if (!lastWavFileRetrieved) {
            try {
                // lastWavFileRecorded = regClient.getLastFilenameRecorded();
                int val = regClient.getIntParameter("client.WriteWaveforms");
                return val;
            }
            catch (Exception e) {
                logger.log(Level.WARNING, "Could not retrieve last filename", e);
                return -1;
            }
        }
        
        return -1;
    }

    /**
     * @return Returns the lastWavFileRecorded.
     * 
     */
    public boolean setParam(String param, String value) {
            try {
                boolean succeded = regClient.setParameter(param, value);
                return succeded;
                
            }
            catch (Exception e) {
                logger.log(Level.WARNING, "Could not retrieve last filename", e);
                lastWavFileRecorded = null;
                return false;
            }
        }    
    
   
    public void setWaitDialogMessage(int progress, String message){
        logger.info(message);
    }
   
    /**
     * @param b
     */
    public void setComputeCPUTimeEnabled(boolean enabled) {
        computeXCPUTime = enabled;
    }

    /**
     * @param updateRecognitionControls The updateRecognitionControls to set.
     */
    /*
    public void setUpdateRecognitionControls(boolean updateRecognitionControls) {
        this.updateRecognitionControls = updateRecognitionControls;
    }
    */
    
    public void sendMessageToRegClient(String msg) throws Exception{
    		regClient.send(msg);
    }
    
    /*
     * gets the N-best result in the format required by the dialogue server  
     */
    public String getInterpret4DialogueServer(){
    	return interpret4DialogueServer;
    }

    public RegClient getRegClient(){
    	return regClient;
    }
    
    public String recognizeFile(String file, String grammar) throws IOException{
    	RegResultNBest res = regClient.recognizeFileNBestMode(file, grammar);
    	return res.getRecognition();
    	
    }
}

