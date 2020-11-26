package gui.transact;

import java.util.logging.Level;
import java.util.logging.Logger;

public class TrInitSpeech extends TrState {
	public static final Logger logger = Logger.getLogger(TrInitSpeech.class.getPackage()
			.getName());

	@Override
	public void nextStep(Transaction tr) {
		if (this.isTasksCompleted()){
			tr.setState(TrConstants.INIT_DIAL_SERV.getState());
		}
		else{
			tr.setState(TrConstants.EXIT_APP.getState());
		}
	}

	@Override
	public void runTasks(Transaction tr) {
		System.out.println(this.status());
		tr.addVisitedState(this);
		try {
			// Initialise the recognition server (i.e. vocalizer, regserver and recserver)
			tr.getCalRecHandler().startup();
			tr.getCalRecHandler().setParam("client.WriteWaveforms", "1");
			// debug only: tr.getCalRecHandler().setParam("client.RecordDirectory",...
			// calendarRecognitionHandler.setParam("client.RecordDirectory", "D:/Regulus/Examples/Calendar/corpora/speech/");
			tr.getCalRecHandler().setParam("client.RecordDirectory", tr.getAppState().getLoggingDirectory());
			this.setTasksCompleted(true);			
		} catch (Exception e) {
			String errorMessage = "The following error occured while initiating the system:\n";
			errorMessage += e.getMessage();
			errorMessage += "\nPlease check your app configuration file and try again.";

			logger.log(Level.SEVERE, "Could not start all required processes.",
					e);
		}
	}
	
	public void stopStateProcs(Transaction tr){
		if (tr.getCalRecHandler() != null){ 
			tr.getCalRecHandler().shutdown();
		}	
	}
	
	

}
