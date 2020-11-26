package issco.calendar.gui;

import java.util.Properties;
import java.util.logging.Logger;

/**
 * Application parameter settings. 
 * (Rather application parameter settings, which are specific for communicating with the regserver)
 */
public class RegSettings {
	private static Logger logger = Logger.getLogger(RegSettings.class.getName());
	private String languageName = null;
		
	private String recognitionPackage = null;
	
	private String slmGrammar = null;
	private String[] slmParameters = null;

	private String glmGrammar = null;
	private String[] glmParameters = null;

	private boolean isSLMSelected = true;
    
	private String recognitionStringEncoding = null;
	
	private boolean isOutputLanguage = false;
	private boolean isUsingTTS = false;
	private String playbackParameters = null;

	private final Properties config;

	public RegSettings(String languageName, Properties pConfig) {
		this.languageName = languageName;
		this.config = pConfig;

		logger.config("Loading settings for '" + languageName + "'");

		recognitionPackage = pConfig.getProperty(languageName + ".recognition.package");

		recognitionStringEncoding = pConfig.getProperty(languageName + ".recognition.characterEncoding", "iso-8859-1");
        	
		String outputMode = config.getProperty(languageName + ".outputMode");
		if( outputMode != null){
			if(outputMode.equals("files")){
				isOutputLanguage = true;
				isUsingTTS = false;
				playbackParameters = config.getProperty(languageName + ".outputMode.parameters");
				if(playbackParameters == null){
					logger.severe("No prompt paths defined for '" + languageName + "'.");
				}			
			}
			else if(outputMode.equals("vocalizer")) {
				isOutputLanguage = true;
				isUsingTTS = true;
				playbackParameters = config.getProperty(languageName + ".outputMode.parameters");
				if(playbackParameters == null){
					logger.severe("Undefined TTS parameters for '" + languageName + "'");
				}
			}
			else if(outputMode.equals("text")){
				isOutputLanguage = true;
				isUsingTTS = false;				
			}
			else {
				logger.warning("Output mode " + languageName + ".outputMode cannot be '" + outputMode + "'");
				// set those anyway, so the language will appear as an output language
				isOutputLanguage = true;
				isUsingTTS = false;
			}
		}
		else{
			isOutputLanguage = true;
			logger.finest("'" + languageName + "' is not an output langauge");
		}
	}
	
	public String getRecognitionPackage(){
		return recognitionPackage;
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

	public String getRecognitionStringEncoding() {
		return recognitionStringEncoding;
	}
    
	public boolean isInputLanguage() {
		boolean result = false;
		String str = getRecognitionPackage();
		if ((str == null) || (str.trim().equals(""))){ 
			result = false;
		}
		else {
			result = true;
		}
		return result;
	}
	
	public boolean isOutputLangauge(){
		return isOutputLanguage;
	}

	public boolean isUsingTTS(){
		return isUsingTTS;	
	}

	public String getGLMGrammar() {
		String result = null;
		String key = languageName + ".glm.grammar";
		result = config.getProperty(key, "");
		
		logger.info("key = " + key);

		if ((result == null) || (result.trim().equals(""))) {
			logger.warning("No configuration entry defined for '" + key + "'");
		}
		return result;
	}
	
	/**
	 * Returns true if the two langauges have the same name.
	 * @param lang
	 * @return
	 */
	public boolean equals(RegSettings lang){
		boolean result = false;
		
		if(this.languageName.equals(lang.languageName)){
			result = true;
		}
		
		return result;
	}

	public boolean hasGLMGrammar() throws Exception{
		boolean result = false;		
		String str = getGLMGrammar();			
		if ((str == null) || (str.trim().equals(""))) {
			result = true;
		}
		
		return result;
	}

	public boolean hasSLMGrammar() {
		boolean result = false;
		String str =getSLMGrammar(); 			
		if ((str == null) || (str.trim().equals(""))) {
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