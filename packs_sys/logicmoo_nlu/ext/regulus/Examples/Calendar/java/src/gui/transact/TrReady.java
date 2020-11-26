/**
 * 
 */
package gui.transact;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author GEORGESC
 *
 */
public class TrReady extends TrState {

	private static final Logger logger = Logger.getLogger(TrReady.class.getName());

	/*
	 * @see issco.calendar.gui.transact.TrState#nextStep(issco.calendar.gui.transact.Transaction)
	 */
	@Override
	public void nextStep(Transaction tr) {
		if (this.isTasksCompleted()){
				tr.setState(TrConstants.RECOGNISE.getState());		
		}
		else
			tr.setState(TrConstants.EXIT_APP.getState()); 

	}

	/* 
	 * @see issco.calendar.gui.transact.TrState#runTasks(issco.calendar.gui.transact.Transaction)
	 */
	@Override
	public void runTasks(Transaction tr) {
		System.out.println(this.status());
		try{
			if (! tr.isVisitedReadyState())
				tr.getCalRecHandler().sendMsgToRegServer("SAY_LIST -tts_text: Ready to start");
			// processPlayRequest("SAY_LIST -tts_text: Ready to start", tr);
			this.setTasksCompleted(true); 
		}
		catch(Exception e){
			logger.log(Level.SEVERE, "The regserver didn't respond to the play request : " +
					" 'SAY_LIST -tts_text: Ready to start'");
		}

	}
	/*
	private void processPlayRequest(String msg, Transaction tr) throws Exception{
		tr.getCalRecHandler().sendMsgToRegServer(msg);	
	}
	*/


}
