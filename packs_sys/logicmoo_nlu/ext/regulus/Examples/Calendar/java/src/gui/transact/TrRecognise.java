package gui.transact;

import socket.DialogueHelpResult;
import socket.ServerConnectionException;

import java.util.logging.Level;
import java.util.logging.Logger;

import net.sf.regulus.NBestRegResult;

public class TrRecognise extends TrState {

	public static Logger logger = Logger.getLogger(TrRecognise.class.getName());

	@Override
	public void runTasks(Transaction tr) {
		System.out.println(this.status());
		tr.addVisitedState(this);		 
		processRecRequest(tr);		
	}

	@Override
	public void nextStep(Transaction tr) {
		if (this.isTasksCompleted()){
			tr.setState(TrConstants.READY_TO_RECOGNISE.getState());
		}
		else{
			tr.setState(TrConstants.EXIT_APP.getState());
		}
		
	}
	
	/**
	 * Communicates with the Regulus server in order to process the recognition request.
	 */
	private synchronized void processRecRequest(Transaction tr) {
		logger.entering(this.getClass().getName(), "processRecognitionRequest");
		try{

			// synchronized (syncObject) {
				// calPanel.setRecognitionInProgress(true); // change the text on the button into "Abort Recognition"  
				NBestRegResult nbest = tr.getCalRecHandler().startRecognition();
				if ( (nbest != null) && (nbest.getNBestResults() != null) && (nbest.getNBestResults().size() > 0) ){
					// observableDialog.setRecognizedText(nbest.getNBestResults().get(0).getRecognition());
					tr.getObservableDialog().setRawRecResult(nbest);
					this.invokeDialServ(nbest, tr);
					//TODO: Create a new state TrInvokeDialServ and then  
					//TODO replace invokeDialServ(nbest) with the execution of the tasks of TrInvokeDialServ.   
				}
				else{
					tr.getObservableDialog().setRecognizedText("?");
				}
				/*
				if (updateRecPanelControls) {
					calPanel.setRecognitionInProgress(false); 
				}
				*/
		}
		catch(Exception ex){
			System.out.println("Exception because calendarRecognitionHandler.startRecognition()");
			ex.printStackTrace();
		}
		logger.exiting(this.getClass().getName(), "processRecognitionRequest");
	}
	
	/**
	 * Communicates with the dialogue server (prolog implem) in order to :
	 * 1) get the selected interpretation (from the initial n-best list provided by the recognizer);
	 * 2) and then to obtain help sentences.
	 * @param nbest - the list of n-best recognition hypotheses 
	 */
	private void invokeDialServ(NBestRegResult nbest, Transaction tr) {
		 // 1) Communicates with the dialogue server (prolog implem) in order to
 		 //     get the selected interpretation (from the initial n-best list provided by the recognizer):
		String textToSend = DialServResponseParser.getActionProcessNbestList(nbest); 
		System.out.println("text to send: " + textToSend);
			// textToSend contains the text to be sent to dialogue server; e.g. textToSend = "action(process_nbest_list([nbest_hyp(55,'were there meetings that monday'),nbest_hyp(55,'were there meetings my monday'),nbest_hyp(48,'were there meetings her monday'),nbest_hyp(48,'were there meetings after monday'),nbest_hyp(48,'were there meetings last monday'),nbest_hyp(48,'were there meetings for monday')])).";
		String dialogueServerReply = tr.sendRequestToDialogueServer(textToSend);
		logger.info("Received reply from dial serv: " + dialogueServerReply);
		if (dialogueServerReply == null){
			return;
		}
		DialServResponseParser.Response dsr = DialServResponseParser.parseResponse(dialogueServerReply);
		tr.getObservableDialog().setRecognizedText(dsr.getSelected());
		tr.getObservableDialog().setAnswer(dsr.getAction());
		try {
			tr.getCalRecHandler().playback("+" + dsr.getAction());
			
		} catch (Exception e) {
			logger.severe("Could not playback." + e);
		}

		// 2) Communicates with the dialogue server (i.e. the prolog implementation) 
		// in order to get help sentences.
		String helpSent = this.getHelpFromDialServ(tr);
		DialogueHelpResult dhr = new DialogueHelpResult(helpSent);
		tr.getObservableDialog().setHelp(dhr.getHelpSentences());
		logger.exiting(this.getClass().getName(), "processRecognitionResult");
	}


	
	/* 
	 * Communicate with the regserver and the dialogue server in order to 
	 * obtain the help sentences. <br/>
	 * First : query the regserver to get the most recent recorded file; <br/>
	 * Second: Take the SLM recognition result, format it into a help request
	 * and send it to the dialogue server;<br/>
	 * Third: receive the help response. <br/>
	 * Example: <br/>
	 * --- Sending message to Regserver: "GET_PARAMETER client.FilenameRecorded <br/>
	 * --- Received message from Regserver: "parameter(string,'client.FilenameRecorded','..\\corpora\\speech\\2007-10-19_15-44-10\\utt01.wav')" <br/>
	 * --- Current wavfile: ..\corpora\speech\2007-10-19_15-44-10/utt01.wav <br/>
	 * --- Sending message to Regserver: "RECOGNISE_FILE d:/regulus/examples/calendar/corpora/speech/2007-10-19_15-44-10/utt01.wav .MAIN_SLM <br/>
	 * --- Received message from Regserver: "recognition_succeeded([rec_result(49,'for zero ten',[])])" <br/>
	 * --- Sending message to dialogue server: action(get_help_examples(for zero ten,5)) <br/>
	 */
	private String getHelpFromDialServ(Transaction tr) {
		String lastWavFileRecorded = tr.getCalRecHandler().getLastWavFileRecorded();
		lastWavFileRecorded = lastWavFileRecorded.replace("\\", "/");
		lastWavFileRecorded = lastWavFileRecorded.toLowerCase();
		// --- Sending message to Regserver. Eg: "RECOGNISE_FILE d:/regulus/examples/calendar/corpora/speech/2007-10-19_15-44-10/utt01.wav .MAIN_SLM
		try{
			// calendarRecognitionHandler.sendMessageToRegClient("RECOGNISE_FILE " + lastWavFileRecorded + " .MAIN_SLM");
			logger.info("_______________SEND MSG TO DIAL SERV: grammar=" + ".MAIN_SLM" + " ;file=" + lastWavFileRecorded );
			String SLMresult = tr.getCalRecHandler().recognizeFile(lastWavFileRecorded, ".MAIN_SLM");
			logger.info("_______________GOT MSG FROM DIAL SERV: " + SLMresult);
			// --- Sending message to dialogue server: action(get_help_examples(for zero ten,5))
			//return "";
			
			String action = "action(get_help_examples(" + SLMresult + "," + tr.getConfig().getDialogueNoHelpSentences() + "))."; 
			String helpSent = tr.sendRequestToDialogueServer(action);
			return helpSent;
			
		}
		catch(Exception ex){
			logger.severe("ERR when communication with regserver in order to do rec with SLM grammar for getting help");
			return "";
		}
	}


    /**
     * This class overrides this method in order to
     * perform recognition.
     */
	/*
	@Override 
    public void runRecognizeTask(Transaction tr){
		processRecRequest(tr); 
	};
*/

}
