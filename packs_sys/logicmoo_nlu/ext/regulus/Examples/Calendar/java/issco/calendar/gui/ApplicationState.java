package issco.calendar.gui;

import java.io.BufferedOutputStream;

/**
 *  Application parameter settings. 
 * (These parameters are taken from the file "calendar.prop". )  
 *
 */
public class ApplicationState {
	private CalendarConfiguration calendarConfig ;

	private int langIndex = 0;
	private boolean isSLMSelected = false;
	private Language lang = null;
	
	private String loggingDirectory = null;
	
	private static BufferedOutputStream feedbackLoggingStream = null;
	
	
	public ApplicationState(CalendarConfiguration p_calendarConfig) {
		try {
			init(p_calendarConfig, 0, false);
		} catch (Exception e) {
			// this will ideally never happen
			e.printStackTrace();
		}
	}
	
	public ApplicationState(ApplicationState appState)  throws Exception {
	    init(appState.calendarConfig, appState.getlangIndex(), appState.isSLMSelected() );
	}

	public ApplicationState(CalendarConfiguration p_calendarConfig, int langIndex, boolean isSLMSelected, int domainIndex) throws Exception {
		init(p_calendarConfig, langIndex, isSLMSelected);
	}
	
	private void init(CalendarConfiguration p_calendarConfig, int langIndex, boolean isSLMSelected) throws Exception{
		this.calendarConfig = p_calendarConfig;
 
		this.langIndex = langIndex;
		
		Language[] appLanguages = calendarConfig.getAvailableLanguages();
		this.lang = appLanguages[langIndex];
		
		this.isSLMSelected = isSLMSelected;
		
		loggingDirectory = "";
	}

	public boolean equals(ApplicationState s) {
		boolean result = false;

		if ((langIndex == s.langIndex) && (isSLMSelected == s.isSLMSelected()) ) {
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

	

	public boolean isSLMSelected(){
		return isSLMSelected;
	}
	
	
	/**
	 * 
	 */
	public String toString(){
		StringBuffer sb = new StringBuffer();
		
		sb.append("langIndex = " + langIndex + "\n");
		sb.append("isSLMSelected = " + isSLMSelected + "\n");
		
		return sb.toString();
	}
	

    /**
     * @return BufferedOutputStream
     */
    public BufferedOutputStream getFeedbackLoggingStream() {
        return feedbackLoggingStream;
    }
    /**
     * @param loggingStream The loggingStream to set.
     */
    public void setFeedbackLoggingStream(BufferedOutputStream loggingStream) {
        feedbackLoggingStream = loggingStream;
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
    public void setLoggingDirectory(String loggingDirectory) {
        this.loggingDirectory = loggingDirectory;
    }
    
}
