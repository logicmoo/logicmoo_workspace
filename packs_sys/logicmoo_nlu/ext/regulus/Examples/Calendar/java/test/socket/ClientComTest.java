/**
 * 
 */
package socket;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author GEORGESC
 *
 */
public class ClientComTest {
	
	public static Logger logger = Logger.getLogger(ClientCom.class.getName());
	final Object syncObject = new Object();
	private OSProcess dialogueServer;

	/**
	 */
	@Before
	public final void testStartDialogServer() {
		String fo_name = "D:/Regulus/Examples/Calendar/scripts/logs/dialogue_server_out.txt";
		File fo = new File(fo_name);
		fo.delete();
		dialogueServer = new OSProcess("redirect.exe -e D:/Regulus/Examples/Calendar/scripts/logs/dialogue_server_err.txt " +
				"-o " + fo_name + " sicstus -l D:/Regulus/Examples/Calendar/scripts/load_and_run_server_regulus_parser.pl");
		dialogueServer.run();	

		// Until a nice solution will be implemented in order 
		// to catch when the external process started:
		// just wait until the size of the output file is higher than a certain value. 		
		while (fo.length() < 2000){
			// just wait
		}
		logger.info(" Dialogue server should be started by now. ");		
	}

	/**
	 * Test method for {@link issco.calendar.socket.ClientCom#sendNBestListToDialogueServer(java.lang.String)}.
	 */
	@Test
	public final void testSendNBestListToDialogueServer() {
		synchronized(syncObject){
			ClientCom cc;
			try {
				// CLIENT OF THE DIALOGUE SERVER:
				cc = new ClientCom("localhost", 1985, ClientCom.DEFAULT_CHARSET);

				String clientRequest1 = "client('localhost').";
				String r = sendNBestListToDialogueServer(clientRequest1, cc);
				System.out.println("Reply = " + r);
				
//				String clientRequest2 = "action(process_nbest_list([nbest_hyp(55,'were there meetings that monday'),nbest_hyp(55,'were there meetings my monday'),nbest_hyp(48,'were there meetings her monday'),nbest_hyp(48,'were there meetings after monday'),nbest_hyp(48,'were there meetings last monday'),nbest_hyp(48,'were there meetings for monday')])).";
				String clientRequest2 = "action(process_nbest_list([nbest_hyp(55,were there meetings that monday),nbest_hyp(55,were there meetings my monday),nbest_hyp(48,were there meetings her monday),nbest_hyp(48,were there meetings after monday),nbest_hyp(48,were there meetings last monday),nbest_hyp(48,were there meetings for monday)])).";
				sendNBestListToDialogueServer(clientRequest2, cc);

				String clientRequest = "disconnect.";
				sendNBestListToDialogueServer(clientRequest, cc);
								
				cc.close();
			}
			catch (ServerConnectionException e) {
				logger.log(Level.WARNING, "", e);
				// e.printStackTrace();
			}
			catch (IOException e) {
				logger.log(Level.WARNING, "IO Exception when closing connection ",e);
				// e.printStackTrace();
			}

		}
	}
	
	/**
	 * Invokes the dialogue server to process the N-best list (previously received from the regServer) 
	 * @param msg4DialogueServer message to be send to the dialogue server
	 * @return reply from the dialogue server
	 */
	private String sendNBestListToDialogueServer(String msg4DialogueServer, ClientCom cc){
		String reply = null;		
		try {
			reply = cc.sendRequest(msg4DialogueServer);			
		}
		catch (ServerConnectionException e) {
			logger.log(Level.SEVERE, e.getMessage());
			e.printStackTrace();
		}
		return reply;
	}


	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		dialogueServer.destroy();
		logger.info("The dialogue server process should have been distroyed. Otherwise kill manually the sicstus external process!! ");		
	}

}
