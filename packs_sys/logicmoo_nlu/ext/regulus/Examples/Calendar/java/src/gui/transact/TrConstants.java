package gui.transact;

/**
 * State constants
 * @author GEORGESC
 *
 */
public enum TrConstants {
	AVAILABLE(new TrAvailable()), 
	INIT_GLOBAL_PROCS(new TrInitGlobalProcesses()),
	INIT_SPEECH_HANDLER(new TrInitSpeech()),
	INIT_DIAL_SERV(new TrInitDialServ()),
	INIT_DIAL_CLIENT(new TrInitDialClient()),
	READY_TO_RECOGNISE(new TrReady()),
	RECOGNISE(new TrRecognise()),  
	// NBEST_SELECT(new TrNbestSelect()),  
	PLAY_BACK(new TrPlayBack()),
	EXIT_APP(new TrExitApp());
	
	private final TrState state;
	
    TrConstants(TrState state) {
        this.state = state;
    }
    
    public TrState getState(){
    	return state;
    }

}
