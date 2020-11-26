
import java.io.*;
import java.net.*;
import java.util.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;

import net.sf.regulus.RegClient;
import net.sf.regulus.RegResult;

/**
 *  Regulus sample application.
 * 
 * This application illustrates how to use the Regulus java client library.
 * 
 * It creates an application window, with a 'recognition' button.
 * When the button is pressed the user can say 'one cat' or 'two dogs'.
 * 
 * The application then displays the animal requested, and plays a
 * corresponding sound.
 *  
 */
public class Toy1app extends JFrame {
    JButton jButtonStartRecognition = null;
    JLabel statusBar = null;

    RegClient regulusClient = null;

    String appGrammar = ".MAIN";
    
    final int regServerPort = 1975;
    
    final String pathToRecognitionPackage = "../Generated/recogniser";
    
    final String nuanceParameters = "audio.OutputVolume=50";

    ArrayList<Object> inState = null;
    InputManager inputManager = null;
    ArrayList<Object> dialogueMove = null;
    DialogueManager dialogueManager = null;
    ArrayList<Object> actionStateList = null;
    OutputManager outputManager = null;
    String action = null;

    /**
     * Initial Constructor
     * @throws Exception
     */
    public Toy1app() throws Exception {
        super("Toy 1 Java Demo");

        //
        // initialize recognition client
        //
        regulusClient = new RegClient(regServerPort, pathToRecognitionPackage, nuanceParameters);

        //
        // initialize display components
        //

        statusBar = new JLabel();
        statusBar.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
        statusBar.setText("Ready.");

        JPanel appPanel = new JPanel();
        BoxLayout bl = new BoxLayout(appPanel, BoxLayout.Y_AXIS);

        appPanel.setLayout(bl);
        appPanel.add(new JPanel().add(getJButtonStartRecognition()));
        appPanel.add(Box.createHorizontalStrut(3), null);
        appPanel.add(Box.createHorizontalStrut(3), null);

        getContentPane().add(appPanel, BorderLayout.NORTH);
        getContentPane().add(statusBar, BorderLayout.SOUTH);


        //
        // properly shut down the speech server and
        //
        this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        this.addWindowListener(new WindowAdapter() {
            public void windowClosed(WindowEvent arg0) {
                try {
                    regulusClient.shutdown();
		    
		    
                }
                catch (IOException e) {
                    e.printStackTrace(System.out);
                }
            }
        });

        pack();

	// play start-up message
	regulusClient.play("-tts_text: ready to start.");
    }

    /**
     * 
     * @return Returns the recognition button. When pressed, the
     *         #recognitionButtonPressed() method is called
     */
    private JButton getJButtonStartRecognition() {
        if (jButtonStartRecognition == null) {
            jButtonStartRecognition = new javax.swing.JButton();
            jButtonStartRecognition.setText("Start Recognition");

            jButtonStartRecognition.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    recognitionButtonPressed(e);
                }
            });
        }
        return jButtonStartRecognition;
    }

    private void performAction (String action)
	throws Exception {
	if (action.startsWith("-tts_text:")) {
	    String ttsString = action;
	    regulusClient.play(action);
	} else {
	    regulusClient.play("-tts_text: Sorry something went wrong.");
	}
	
    }
    /**
     * This method: 
     *  - initiates a recognition action and gets the result and LF
     *
     *  - passes resulting lf and currentState to inputManager; 
     *    returns dialogueMove
     *  - passes resulting dialogueMove and currentState to dialogueManager
     *    returns abstractAction and outState
     *  - passes abstractAction to outputManager
     *    returns concreteAction
     *
     ***
     *
     * and updates the picture shown. -cn for later implementation
     * 
     * @param e
     */

    private void recognitionButtonPressed(ActionEvent e) {
        try {
            jButtonStartRecognition.setEnabled(false);
            RegResult regResult = regulusClient.recognize(appGrammar);
            
            if (regResult.isActionSuccesfull()) {

		System.err.println("*********START RECOGNITION TRACE**********");
		System.out.println("*********RECOGNITION EVENT **********");

                /**
                // print out recognition and interpretation strings.
                //
                // this contains the regulus interpretation string.
                // Example: Turn on the light in the kitchen
                // Interpretation: [ 'value' = [ [ 'utterance_type', 'command' ], 
		//                               [ 'action', 'switch' ], 
		//                               [ 'onoff', 'on' ], 
		//                               [ 'device', 'light' ], 
		//                               [ 'location', 'kitchen' ] ] ]
		**/

                System.err.println("'Recognition: " + regResult.getRecognition() + "'");
                //System.err.println("'Interpretation: " + regResult.getInterpretation() + "'");

                //
                // retrieve intepretation object
                //
                // the interpretation object in our example contains a hashtable
                // whith one key/value pair. The key in our example is "value",
                // and the value is an arraylist, containing other arraylists.
                //
                Object interpretationObject = regResult.getInterpretationObject();
                System.err.println("'InterpretationObject: " + interpretationObject + "'");
		
                //
                // retrieve the 'value' Hashtable
                //
                Hashtable h = (Hashtable) ((ArrayList) interpretationObject).get(0);
		
                
                //
                // retrieve the arraylist
                // the 'a' object will contain 5 arraylists: 
		// [[ 'utterance_type', 'command' ], [ 'action', 'switch' ], 
		//  [ 'onoff', 'on' ], [ 'device', 'light' ], 
		//  [ 'location', 'kitchen' ]]'
		//
                //
                ArrayList a = (ArrayList) h.get("value");	
                System.out.println("App: LF input: " + a);
	
                //
		// create the InputManager
		// call lfToDialogueMove(LF), which returns dialogueMove
		//
		inputManager = new InputManager();
		dialogueMove = new ArrayList<Object>(inputManager.lfToDialogueMove(a));

                System.out.println("App: dialogue move: " + dialogueMove);

		//
		// create the dialogueManager;
		// initialize current state (inState) if required;
		// call dialogueManager.updateDialogueState(dialogueMove, inState)
		//   return absAction-outputState pair (in a list format).
		//
		dialogueManager = new DialogueManager();

		//** if the state doesn't currently exist, create one.
		inState = this.inState;
		if (inState == null) {
		    inState = new ArrayList<Object>(dialogueManager.initialDialogueState());
		}

		System.out.println("App: inState: " + inState);
		
		actionStateList = new ArrayList<Object>(dialogueManager.updateDialogueState(dialogueMove, inState));

		//** devide up actionStateList into abstract action and output state.
		ArrayList<Object> absAction = (ArrayList<Object>)  actionStateList.get(0);
		ArrayList<Object> outState = (ArrayList<Object>) actionStateList.get(1);
		//** reset the current state
		this.inState = outState;
		
		System.out.println("App: absAction: " + absAction);
		System.out.println("App: outState: " + outState);

		//
		// create the outputManager;
		// call outputManager.abstractActionToAction(absAction);
		//   return concrete action (action), aka TTS string
		//
		outputManager = new OutputManager();
		action = (String) outputManager.abstractActionToAction(absAction);
		System.out.println("App: action " + action);
		
		//
		// realize the TTS String by calling Vocalizer
		//
		performAction(action);
                statusBar.setText(regResult.getRecognition());
                
            }
            else {

                statusBar.setText("No recognition");
		
		//** What the prolog versions says...
		//regulusClient.play("-tts_text: Sorry something went wrong.");
		
		regulusClient.play("-tts_text: Sorry I didn't understand you.");

            }
            pack();
            jButtonStartRecognition.setEnabled(true);
        }
        catch (Exception err) {
            err.printStackTrace(System.out);
        }
    }


    public static void main(String[] args) {
        try {
	    //
	    // If there is no option given, or the option is not -d or --debug,
	    // then redirect all the standard output to stderr.log
	    //

	    String pathToJavaAppDirectory = "../java";

	    if ((args.length >= 1)) {
		if (!(args[0].equals("-d") || (args[0].equals("--debug")))) {
		    System.out.println("Debugging trace can be turned on using the -d or --debug option.  If you are using Make, call $ make run_toy1_debug.");
		    // redirect Standard Out messages to log
		    PrintStream out =
			new PrintStream(
			    new BufferedOutputStream(
				new FileOutputStream(pathToJavaAppDirectory+"/stderr.log")));
		    System.setErr(out);
		}
	    } else {
		System.out.println("Debugging trace can be turned on using the -d or --debug option.  If you are using Make, call $ make run_toy1_debug.");
		// redirect Standard Out messages to log
		PrintStream out =
		    new PrintStream(
			new BufferedOutputStream(
			      new FileOutputStream(pathToJavaAppDirectory+"/stderr.log")));
		System.setErr(out);
	    }

            Toy1app app = new Toy1app();
            app.show();
        }
        catch (Exception ex) {
            System.out.println("Exception:" + ex);
        }

    }

}
