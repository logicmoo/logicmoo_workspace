package gui;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Logger;

/**
 *  Application parameter settings. (Parameters' values taken from the file "calendar.prop". ) 
 * 	@Author(
 *    name = "Maria Georgescul",
 *     date = "4/11/2008"
 *   )
 *
 */
public class ApplicationState {
	public static final Logger logger = Logger.getLogger(ApplicationState.class.getPackage()
			.getName());

	private CalendarConfiguration calendarConfig ;

	private int langIndex = 0;
	private Language lang = null;
	
	private String loggingDirectory = null;
	
	public ApplicationState(CalendarConfiguration p_calendarConfig) {
		try {
			init(p_calendarConfig, 0);
		} catch (Exception e) {
			// this will ideally never happen
			e.printStackTrace();
		}
	}
	
	public ApplicationState(ApplicationState appState)  throws Exception {
	    init(appState.calendarConfig, appState.getlangIndex());
	}

	public ApplicationState(CalendarConfiguration p_calendarConfig, int langIndex) throws Exception {
		init(p_calendarConfig, langIndex);
	}
	
	private void init(CalendarConfiguration p_calendarConfig, int langIndex) throws Exception{
		this.calendarConfig = p_calendarConfig;
 
		this.langIndex = langIndex;
		
		Language[] appLanguages = calendarConfig.getAvailableLanguages();
		try{
			this.lang = appLanguages[langIndex];
		}
		catch(java.lang.IndexOutOfBoundsException except ){
			throw (new java.lang.IndexOutOfBoundsException("This language index does not exist !! "));
		}
		
		setNewLoggingDirectory();
	}

	/**
	 * Sets logDir attribute to a new value
	 * @param ApplicationState
	 * @param String newLogDir provides the value of the new logging directory
	 */
	private void setNewLoggingDirectory() {
		//
		// Get new logging directory
		//
		String newLogDir = calendarConfig.getWaveformLoggingDirectory();
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

		String waveLogDir = tmp.toString();

		//
		// create the directory & re-create the logStream file
		//
		boolean success = false;
		success = (new File(waveLogDir)).mkdirs();

		if (!success) {
			logger.warning("Could not create logging directory.");
			this.setLoggingDirectory(null);
			/*
			logStream = new BufferedOutputStream(System.out);
			 */
		} 
		else {
			this.setLoggingDirectory(waveLogDir);
		}
	}

	public void setLanguageIndex(int langIdx) throws java.lang.IndexOutOfBoundsException{
		Language[] appLanguages = calendarConfig.getAvailableLanguages();
		try{
			this.lang = appLanguages[langIdx];
		}
		catch(java.lang.IndexOutOfBoundsException except ){
			throw (new java.lang.IndexOutOfBoundsException("This language index does not exist !! "));
		}
		this.langIndex = langIdx;
		
	}

	
	public boolean equals(ApplicationState s) {
		boolean result = false;

		if (langIndex == s.langIndex) {
			result = true;
		}

		return result;
	}

	
	/**
	 * @return
	 */
	public int getlangIndex() {
		return langIndex;
	}

	
	/**
	 * @return
	 */
	public Language getLanguage() {
		return lang;
	}

	
	
	/**
	 * 
	 */
	public String toString(){
		StringBuffer sb = new StringBuffer();
		
		sb.append("langIndex = " + langIndex + "\n");
		
		return sb.toString();
	}
	
    /**
     * @return Returns the loggingDirectory.
     */
    public String getLoggingDirectory() {
        return loggingDirectory;
    }
    /**
     * @param loggingDirectory The loggingDirectory to set.
     */
    public void setLoggingDirectory(String logDirectory) {
        this.loggingDirectory = logDirectory;
    }
    
}
