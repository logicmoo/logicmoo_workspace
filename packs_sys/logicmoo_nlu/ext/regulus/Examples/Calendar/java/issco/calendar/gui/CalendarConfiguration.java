package issco.calendar.gui;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Global parameter settings for the application. 
 * (These parameters are taken from the file "calendar.prop". )  
 *
 */
public class CalendarConfiguration {

	private static Logger logger = Logger.getLogger(CalendarConfiguration.class.getPackage().getName());

    private static final String buildVersion = "$Revision: 1.2 $"; 
    private static final String buildDate = "$Date: 2007/12/07 14:32:06 $";

    // This string gets replaced by the build date.
    // This is done by the script that triggers the nightly build  
    private static final String releaseInformation = "123_RELEASE_INFO_123";
    
    private Properties configuration = null;
	private String []languageNames = null;
	private Language []appLanguages = null;

	private String []globalProcesses = null;

	//private Language defaultInputLanguage = null;
	private Language appLanguage = null;
	// private Language defaultOutputLanguage = null;
	
	private String dialogueServerHost = null;
	private int dialogueServerPort ;
	private java.nio.charset.Charset dialogueServerCharset = null;
	
	private int dialogueNoHelpSentences = 2; 
	
	/**
	 * 
	 * @return
	 */
    public final static String getReleaseInformation(){
        String result = releaseInformation;
        
        return result;
    }
    
/**
 * 
 * @param configFile
 */
	public CalendarConfiguration(String configFile){
		logger.setLevel(Level.ALL);
		logger.setFilter(null);

		configuration = new Properties();
		InputStream is = null;
		
		String configFileName = null;
		
		if(configFile == null){
		    configFileName = "calendar.prop";
		}
		else{
		    configFileName = configFile;
		}

		//
		// load configuration file
		//
		try{
			is = new FileInputStream(configFileName);
			logger.config("Loading configuration from file '" + configFileName + "'");
		}
		catch(FileNotFoundException e){
		}

		//
		// if no file is found, load default configuration from classpath
		//
		if(is == null){
			is = this.getClass().getClassLoader().getResourceAsStream(configFileName);
			logger.config("Loading configuration '" + configFileName + "' from application package.");
		}

		if(is != null){
			try {
				configuration.load(is);
				is.close();
				logger.config("Configuration file loaded.");
			}
			catch (IOException e1) {
				e1.printStackTrace();
			}
		}

		// languageNames = initAvailableLanguages();
		languageNames = initAvailableLanguages();


		//
		// initialize languages and inputLanguages arrays
		//
		
		if(languageNames != null){
			this.appLanguages = new Language[languageNames.length];
			// Vector appLangTmp = new Vector();

			for(int i=0 ; i<languageNames.length ; i++){
				this.appLanguages[i] = new Language(languageNames[i], configuration);
				/*
				if(appLanguages[i].isInputLanguage()){
					inputLangTmp.add(languages[i]);
				}*/
			}
			
			/*
			inputLanguages = new Language[inputLangTmp.size()];
			for(int i=0 ; i<inputLanguages.length ; i++){
				inputLanguages[i] = (Language) inputLangTmp.get(i);
			}
			*/
		}

		//
		// Set default input/output language and domain
		//
		//defaultInputLanguage = languages[0];
		appLanguage = new Language(languageNames[0], configuration);
		logger.config("Application language set to " + appLanguage.getLanguageName());

		initGlobalProcesses();
		
		// 

		dialogueServerHost = configuration.getProperty("dialogue.server.host");
		dialogueServerPort = new Integer(configuration.getProperty("dialogue.server.port")).intValue();
		dialogueServerCharset = Charset.forName(configuration.getProperty("dialogue.server.charset"));
		
		dialogueNoHelpSentences = new Integer(configuration.getProperty("dialogue.noHelpSent")).intValue();
		
	}

	/**
	 * Extract names of languages from config file
	 */
	private String [] initAvailableLanguages(){
		String []result = null;

		int i=1;
		String val = "";
		while(val != null){
			val = configuration.getProperty("global.lang." + i + ".name");
			if(val == null) break;
			i++;
		}

		if(i-1>0){
			result = new String[i-1];
		}
		else{
			Logger.global.warning("The application language is not specified in 'calendar.prop file'.");
		}

		if(result != null){
			for(i=1 ; i<=result.length ; i++){
				result[i-1] = configuration.getProperty("global.lang." + i + ".name").trim();
			}
		}

		return result;
	}

	
	Language getLanguageByName(String langName) throws Exception {
		Language result = null;

		for(int i=0 ; i<appLanguages.length ; i++){
			if(appLanguages[i].getLanguageName().equals(langName)){
				result = appLanguages[i];
				break;
			}
		}

		if(result == null) {
			throw new Exception("No language with name '" + langName + "' was defined");
		}

		return result;
	}
	
	public Language[] getAvailableLanguages() {
		return appLanguages;
	}

	public String getProcessHandlerLibrary(){
		return configuration.getProperty("global.processHandler.nativeLibrary", "native/Debug/native");
	}


	public int getDialogueNoHelpSentences(){
		return new Integer(configuration.getProperty("dialogue.noHelpSent")).intValue(); 		
	}

	public void initGlobalProcesses(){
		if(globalProcesses == null){
			int i=1;
			String val = "";
			while(val != null){
				val = configuration.getProperty("global.process." + i);
				if(val == null) break;
				i++;
			}

			if(i-1>0){
				globalProcesses = new String[i-1];

				for(int j=1 ; j<globalProcesses.length+1 ; j++){
					globalProcesses[j-1] = configuration.getProperty("global.process." + j).trim();
				}
			}
			else{
				Logger.global.warning("xxx.");
			}
		}
	}

	public String[] getGlobalProcesses(){
		return globalProcesses;
	}

	public String getWaveformLoggingDirectory(){
		return configuration.getProperty("global.recognition.wavFileDir", ".");	    
	}
	
	public int getRecognitionNBest(){
        int result = 1;

        String setting = configuration.getProperty("global.recognition.nbest", Integer.toString(result));
        try{
            result = Integer.parseInt(setting);
        }
        catch(NumberFormatException nfe){
            Logger.global.log(Level.WARNING, "Could not get the number corresponding to NBest recognition " + result + " ", nfe);
        }

        return result;
	    
	}
	
 
    public long getVocalizerKillWaitTime() {
        long result = 1500;
        
        String setting = configuration.getProperty("global.vocalizer.killWaitTime", Long.toString(result));
        try{
            result = Long.parseLong(setting);
        }
        catch(NumberFormatException nfe){
            Logger.global.log(Level.WARNING, "Could not get vocalizer kill waiting time. Assuming " + result + " msec.", nfe);
        }
        Logger.global.info("returning " + result);
        return result;

    }
    
	/**
	 * 
	 * @return
	 */
    public long getStartupRecognitionServerWaitTime() {
		long result = 2500;

		String setting = configuration.getProperty("global.startup.recServer.waitTimeMsecs", Long.toString(result));
		try{
			result = Long.parseLong(setting);
		}
		catch(NumberFormatException nfe){
			Logger.global.log(Level.WARNING, "Could not get recognition server startup time. Assuming " + result + " msec.", nfe);
		}

		return result;
	}

	/**
	 *
	 */
	public int getRegulusServerPort() {
		int result = 1974;

		String setting = configuration.getProperty("global.startup.regulusServer.Port", Integer.toString(result));
		try{
			result = Integer.parseInt(setting);
		}
		catch(NumberFormatException nfe){
			Logger.global.log(Level.WARNING, "Could not get regulus server port. Assuming " + result + " ", nfe);
		}

		return result;
	}

	/**
	 *
	 */
	public String getRegulusServerHost() {
		return configuration.getProperty("global.startup.regulusServer.Host", "127.0.0.1");
	}


	/**
	 *
	 */
	public long getRegulusServerTimeOut() {
		long result = 7000;

		String setting = configuration.getProperty("global.startup.regulusServer.waitTimeMsecs", Long.toString(result));
		try{
			result = Long.parseLong(setting);
		}
		catch(NumberFormatException nfe){
			Logger.global.log(Level.WARNING, "Could not get regulus server startup time. Assuming " + result + " msec.", nfe);
		}

		return result;
	}

	/**
	 *
	 */
	public String getRegulusServerCommand() {
		return configuration.getProperty("global.startup.regulusServer.command", "regserver");
	}

	/**
	 * @return
	 */
	public boolean showInlineHelp() {
		boolean result;

		String setting = configuration.getProperty("global.gui.startup.showInlineHelp", "true");
		result = Boolean.valueOf(setting).booleanValue();

		return result;
	}
	
	/**
	 * @return
	 */
	public boolean computeXCPURT(){
		boolean result;

		String setting = configuration.getProperty("global.gui.startup.computeXCPUTime", "true");
		result = Boolean.valueOf(setting).booleanValue();

		return result;	    
	}

    /**
     * @return
     */
    public boolean showRawRecognitionResult() {
		boolean result;

		String setting = configuration.getProperty("global.gui.startup.showRawRecognitionResult", "true");
		result = Boolean.valueOf(setting).booleanValue();

		return result;
    }

    public int getDialogueServerStartingTime() {
		int result;

		String setting = configuration.getProperty("dialogue.server.starting.time");
		result = Integer.valueOf(setting).intValue();

		return result;
    }
    
       
    /**
     * @return
     */
    public String getBatchrecOutputFile() {
        String result = null;
        
        result = configuration.getProperty("global.batchrecOutputFile", null);
        if(result == null){
            result = getWaveformLoggingDirectory() + "/batchrec-output.txt";
        }

        return result;
    }
    
    
    public String getDefaultFontName(){
        String result = null;
        
        result = configuration.getProperty("global.gui.font.name", "\"SansSerif\"");
        if(result == null){
            result = "\"SansSerif\",Font.PLAIN,14";
        }

        return result;
    }
    
    public int getDefaultFontSize(){
        int result = 14;

        String setting = configuration.getProperty("global.gui.font.size", Integer.toString(result));
        try{
            result = Integer.parseInt(setting);
        }
        catch(NumberFormatException nfe){
            Logger.global.log(Level.WARNING, "Could not get font size. Assuming " + result + " ", nfe);
        }

        return result;
    }

    /*
     * 
     */
    public String getBatchrecTestSetFile() {
        String result = null;
        
        result = configuration.getProperty("global.batchrecTestSetFile", null);
        if(result == null){
            result = getWaveformLoggingDirectory() + "/batchrec-testset.txt";
        }

        return result;
    }

    
    public String getDialogueServerHost(){
    	return dialogueServerHost;
    }
    
	public int getDialogueServerPort(){
		return dialogueServerPort;
	}
	
	public java.nio.charset.Charset getDialogueServerCharset(){
		return dialogueServerCharset;
	}
	
}
