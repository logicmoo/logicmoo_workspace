package gui.transact;

import java.util.logging.Logger;

public class TrPlayBack extends TrState {
	public static Logger logger = Logger.getLogger(TrPlayBack.class.getName());

	@Override
	public void runTasks(Transaction tr) {
		System.out.println(this.status());
		tr.addVisitedState(this);
		playback("Ready to start.", tr);
		
	}
	@Override
	public void nextStep(Transaction tr) {
		// TODO Auto-generated method stub
	}

	private void playback(String strToPlay, Transaction tr) {

		try {
			tr.getCalRecHandler().playback("+" + strToPlay);
			
		} catch (Exception e) {
			logger.severe("Could not playback." + e);
		}
	}


	


}
