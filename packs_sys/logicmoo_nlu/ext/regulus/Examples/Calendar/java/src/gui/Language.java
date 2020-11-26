package gui;

import java.util.Properties;
import java.util.logging.Logger;

/**
 * Application parameter settings. 
 * (Rather application parameter settings, which are specific for communicating with the regserver)
 */
public class Language {
	private static Logger logger = Logger.getLogger(Language.class.getName());
	private String languageName = null;
	private int languageIndex = -1;
		
	private String recognitionPackage = null;	
	private boolean isSLMSelected = true;    
	private String recognitionStringEncoding = null;	
	private boolean isUsingTTS = false;
	private String playbackParameters = null;
	
	private String dialogueServerPackage = null;

	private final Properties config;

	public Language(String languageName, Properties pConfig) {
		this.languageName = languageName;
		this.config = pConfig;

		logger.config("Loading settings for '" + languageName + "'");

		recognitionPackage = this.config.getProperty(languageName + ".recognition.package");
		recognitionStringEncoding = this.config.getProperty(languageName + ".recognition.characterEncoding", "iso-8859-1");        	
		dialogueServerPackage = this.config.getProperty(languageName + ".dialogueServer.package");
		try{
			String langIdx = this.config.getProperty(languageName + ".index");
			if (langIdx != null)
				this.languageIndex = new Integer(langIdx);
		}
		catch(NumberFormatException e){
			e.printStackTrace();
		}
		
		String outputMode = this.config.getProperty(languageName + ".outputMode");
		if(outputMode != null){			
			if(outputMode.equals("files"))
			{
				//isUsingTTS = false;
			}			
			else
				if(outputMode.equals("vocalizer")) 
					isUsingTTS = true;
				else {
					logger.warning("Output mode " + languageName + ".outputMode cannot be '" + outputMode + "' for this application. Valid output modes are: 'vocalizer' and 'files'. ");
					isUsingTTS = false;
				}

			playbackParameters = config.getProperty(languageName + ".outputMode.parameters");
			if(playbackParameters == null){
				logger.severe("Undefined TTS parameters for '" + languageName + "'");
			}
		}
		else{
			logger.finest("'" + languageName + "' is not an output langauge defined for this application.");
		}
	}
	
	public String getRecognitionPackage(){
		return recognitionPackage;
	}
	
	public String getDialogServerPackage(){
		return dialogueServerPackage;
	}
	/**
	 *
	 */
	public String getDialogPackage() {
		return config.getProperty(languageName + ".dialogueServer.package");
	}
	

	public String[] getRecognitionClientParameters() {
		String[] result = null;
		String tmp = config.getProperty("global.recognition.clientParameters");
		// tmp =  tmp + " " + pConfig.getProperty(languageName + ".recognition" + glmOrSlm + ".clientParameters");

		if (tmp != null) {
			tmp = tmp + " ";
			result = tmp.split(" ");

			for (int i = 0; i < result.length; i++) {
				result[i] = result[i].trim();
			}
		}
		return result;
	}


	public String getSLMGrammar() {
		String result = null;
		String key = languageName + ".slm.grammar";
		result = config.getProperty(key, "");		
		// logger.info("key = " + key);
		
		if ((result == null) || (result.trim().equals(""))) {
			logger.warning("No configuration entry defined for '" + key + "'");
		}

		return result;
	}

	public String[] getSLMRecognitionClientParameters() {
		String []result = null;
		
		String key = languageName + ".slm.clientParameters";
	
		String tmp = config.getProperty(key);
		tmp =  tmp + " " + config.getProperty("global.recognition.clientParameters");

		if (tmp != null) {
			tmp.trim();
			tmp = tmp + " ";
			result = tmp.split(" ");

			for (int i = 0; i < result.length; i++) {
				result[i] = result[i].trim();
			}
		}

		return result;
	}

	public String[] getGLMRecognitionClientParameters() {
		String []result = null;
		
		String key = languageName + ".glm.clientParameters";
	
		String tmp = config.getProperty(key);
		tmp =  tmp + " " + config.getProperty("global.recognition.clientParameters");

		if (tmp != null) {
			tmp.trim();
			tmp = tmp + " ";
			result = tmp.split(" ");

			for (int i = 0; i < result.length; i++) {
				result[i] = result[i].trim();
			}
		}

		return result;
	}


	public String getLanguageName() {
		return languageName;
	}

	public int getLanguageIndex() {
		return languageIndex;
	}
	
	public int getLanguageIndex(String languageName) {
		try{
			this.languageIndex = new Integer(this.config.getProperty(languageName + ".index")).intValue();
		}
		catch(NumberFormatException e){
			e.printStackTrace();
		}
		return languageIndex;
	}
	
	
	
	public String getRecognitionStringEncoding() {
		return recognitionStringEncoding;
	}
    
	public boolean isImplementedLanguage() {
		String str = getRecognitionPackage();
		if ((str == null) || (str.trim().equals(""))){
			return false;
		}
		else {
			return true;
		}
	}
	
	public boolean isUsingTTS(){
		return isUsingTTS;	
	}

	public String getGLMGrammar() {
		String result = null;
		String key = languageName + ".glm.grammar";
		result = config.getProperty(key, "");
		
		logger.info("key = " + key);

		if ((result == null) || (result.trim().equals(""))){
			logger.warning("No configuration entry defined for '" + key + "'");
		}
		return result;
	}
	
	/**
	 * Returns true if the two languages have the same name.
	 * @param lang
	 * @return
	 */
	public boolean equals(Language lang){
		boolean result = false;
		
		if(this.languageName.equals(lang.languageName)){
			result = true;
		}
		
		return result;
	}

	public boolean hasGLMGrammar() throws Exception{
		boolean result = false;
		String str = getGLMGrammar();
		if (!((str == null) || (str.trim().equals("")))){
			result = true;
		}
		
		return result;
	}

	public boolean hasSLMGrammar() {
		boolean result = false;
		String str = getSLMGrammar();
		if (!((str == null) || (str.trim().equals("")))){	
			result = true;
		}
		
		return result;
	}

	public boolean isSLMSelected(){
		return isSLMSelected;
	}
	
	public void setSLMSelected(boolean selected){
			isSLMSelected = selected;
	}

	/**
	 * @return
	 */
	public boolean isSuportingTalkback() {
		boolean result;

		String setting = config.getProperty(languageName + ".supportsTalkback", "false");
		result = Boolean.valueOf(setting).booleanValue();

		return result;
	}

	
	/**
	 * @return
	 */
	public String getPlaybackParameters() {
		return playbackParameters;
	}
	
}