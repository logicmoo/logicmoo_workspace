
import java.io.*;
import java.net.*;
import java.util.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;

import net.sf.regulus.RegClient;
import net.sf.regulus.RegResult;
import net.sf.regulus.NBestRegResult;

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
public class JDemo extends JFrame {
    JButton jButtonStartRecognition = null;
    ImageIcon appImage = null;
    JLabel statusBar = null;
    JLabel imageLabel = null;

    RegClient regulusClient = null;

    String appGrammar = "<file:../Nuance/main.gsl>";
    
    final int regServerPort = 1975;
    
    final String pathToRecognitionPackage = "../Generated/recogniser_dynamic";
    
    final String nuanceParameters = "audio.OutputVolume=120";
    final String nbestnuanceParameters = "audio.OutputVolume=120 rec.DoNBest=TRUE rec.NumNBest=3";
    
    String pathToAudioFilesDirectory = "../java/audio";
    
    boolean doNBest = false;

    /**
     * 
     * @throws Exception
     */
    public JDemo(boolean nbest) throws Exception {
        super("Regulus Demo");

        doNBest = nbest;
        
        //
        // initialize recognition client
        //
        if (nbest == false){
        	regulusClient = new RegClient(regServerPort, pathToRecognitionPackage, nuanceParameters);
        }
        else{
           	regulusClient = new RegClient(regServerPort, pathToRecognitionPackage, nbestnuanceParameters);
        }

        //
        // initialize display components
        //
        appImage = JDemo.createImageIcon("img/unk.jpg", "unknown");
        imageLabel = new JLabel();
        imageLabel.setIcon(appImage);

        statusBar = new JLabel();
        statusBar.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
        statusBar.setText("Ready.");

        JPanel appPanel = new JPanel();
        BoxLayout bl = new BoxLayout(appPanel, BoxLayout.Y_AXIS);

        appPanel.setLayout(bl);
        appPanel.add(new JPanel().add(getJButtonStartRecognition()));
        appPanel.add(Box.createHorizontalStrut(3), null);

        appPanel.add(imageLabel);
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
                    e.printStackTrace();
                }
            }
        });

        pack();
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
    
    /**
     * This method initiates a nbest recognition action, gets the result and updates
     * the picture shown.
     * 
     * @param e
     */
    private void recognizeNBest(){
    	try {
    	
    		NBestRegResult regResults = regulusClient.recognizeNBest(appGrammar);
    		    		
    		ArrayList<RegResult> resultsArray = regResults.getNBestResults();
    		
    		if (resultsArray.size() > 0)
    		{
    			regResults.printResults();
    		
    			java.util.Iterator it = resultsArray.iterator();
    		
    			RegResult regResult = (RegResult)it.next();
    			    		
    			if (regResult.isActionSuccesfull()) {
        	
        			//
        			// print out recognition and interpretation strings.
        			//
        			// this contains the regulus interpretation string.
        			// Example: one dog
        			// Interpretation: ['value'=[['spec', 1], ['noun', 'canis_domesticus']]]
        			System.out.println("'" + regResult.getRecognition() + "'");
        			System.out.println("'" + regResult.getInterpretation() + "'");

        			//
        			// retrieve intepretation object
        			//
        			// the interpretation object in our example contains a hashtable
        			// whith one key/value pair. The key in our example is "value",
        			// and the value is an arraylist, containing 2 arraylists.
        			//
        			Object interpretationObject = regResult.getInterpretationObject();
        			System.out.println("'" + interpretationObject + "'");

        			//
        			// retrieve the 'value' Hashtable
        			//
        			Hashtable h = (Hashtable) ((ArrayList) interpretationObject).get(0);
            
        			//
        			// retrieve the arraylist
        			// the 'a' object will contain  two arraylist: [['spec', 1], ['noun', 'canis_domesticus']]
        			//
        			ArrayList a = (ArrayList) h.get("value");
            
        			//
        			// retrieve the objects we are interested in
        			//
        			ArrayList specList = (ArrayList)a.get(0);
        			ArrayList nounList = (ArrayList)a.get(1);
            
        			// retrieve the value of "spec" as String and integer.
        			String spec = (String) specList.get(1);
        			int iSpec = Integer.parseInt(spec);
            
        			// retrieve "noun" value
        			String noun = (String) nounList.get(1);
           
        			//
        			// Show the image of what has been said
        			//
        			String imageToShow = "img/" + noun + "-" + spec + ".jpg";
        			String imageDescription = spec + " " + noun;

        			appImage = JDemo.createImageIcon(imageToShow, imageDescription);
        			imageLabel.setIcon(appImage);

        			// display recognition result in statusbar
        			statusBar.setText(regResult.getRecognition());

        			//
        			// play sound of the animals
        			//
        			String[] playbackFiles = new String[iSpec];
        			
        			for( int i=0 ; i < iSpec ; i ++ ){
        				playbackFiles[i] = pathToAudioFilesDirectory + "/" + noun + ".wav";
        			}
        			
        			regulusClient.playList(playbackFiles);
            
        		}
        		else {
        			appImage = JDemo.createImageIcon("img/unk.jpg", "???");
        			imageLabel.setIcon(appImage);
            
        			statusBar.setText("No recognition");
        		}    		
    		}
    		else {
    			appImage = JDemo.createImageIcon("img/unk.jpg", "???");
    			imageLabel.setIcon(appImage);
        
    			statusBar.setText("No recognition");
    		}
    		
    		System.out.println("\n------------------------------\n");
       	}
    	catch (Exception err) {
    		err.printStackTrace();
    	}
    }
    
    /**
     * This method initiates a recognition action, gets the result and updates
     * the picture shown.
     * 
     * @param e
     */
    private void recognize(){
    	try {
    	
    		RegResult regResult = regulusClient.recognize(appGrammar);
    		    
    		if (regResult.isActionSuccesfull()) {
    	     			    	
    			//
    			// print out recognition and interpretation strings.
    			//
    			// this contains the regulus interpretation string.
    			// Example: one dog
    			// Interpretation: ['value'=[['spec', 1], ['noun', 'canis_domesticus']]]
    			System.out.println("'" + regResult.getRecognition() + "'");
    			System.out.println("'" + regResult.getInterpretation() + "'");

    			//
    			// retrieve intepretation object
    			//
    			// the interpretation object in our example contains a hashtable
    			// whith one key/value pair. The key in our example is "value",
    			// and the value is an arraylist, containing 2 arraylists.
    			//
    			Object interpretationObject = regResult.getInterpretationObject();
    			System.out.println("'" + interpretationObject + "'");

    			//
    			// retrieve the 'value' Hashtable
    			//
    			Hashtable h = (Hashtable) ((ArrayList) interpretationObject).get(0);
        
    			//
    			// retrieve the arraylist
    			// the 'a' object will contain  two arraylist: [['spec', 1], ['noun', 'canis_domesticus']]
    			//
    			ArrayList a = (ArrayList) h.get("value");
        
    			//
    			// retrieve the objects we are interested in
    			//
    			ArrayList specList = (ArrayList)a.get(0);
    			ArrayList nounList = (ArrayList)a.get(1);
        
    			// retrieve the value of "spec" as String and integer.
    			String spec = (String) specList.get(1);
    			int iSpec = Integer.parseInt(spec);
        
    			// retrieve "noun" value
    			String noun = (String) nounList.get(1);
       
    			//
    			// Show the image of what has been said
    			//
    			String imageToShow = "img/" + noun + "-" + spec + ".jpg";
    			String imageDescription = spec + " " + noun;

    			appImage = JDemo.createImageIcon(imageToShow, imageDescription);
    			imageLabel.setIcon(appImage);

    			// display recognition result in statusbar
    			statusBar.setText(regResult.getRecognition());

    			//
    			// play sound of the animals
    			//
    			String[] playbackFiles = new String[iSpec];
    			
    			for( int i=0 ; i < iSpec ; i ++ ){
    				playbackFiles[i] = pathToAudioFilesDirectory + "/" + noun + ".wav";
    			}
    			
    			regulusClient.playList(playbackFiles);
        
    		}
    		else {
    			appImage = JDemo.createImageIcon("img/unk.jpg", "???");
    			imageLabel.setIcon(appImage);
        
    			statusBar.setText("No recognition");
    		}
    		
    		System.out.println("\n------------------------------\n");
    	}
    	catch (Exception err) {
    		err.printStackTrace();
    	}
    }
    
    /**
     * This method initiates a recognition action, gets the result and updates
     * the picture shown.
     * 
     * @param e
     */
    private void recognitionButtonPressed(ActionEvent e) {
        try {
            jButtonStartRecognition.setEnabled(false);
            
            if (doNBest == true) {
            	recognizeNBest();
            }
            else {
            	recognize();            
            }
            
            pack();
            jButtonStartRecognition.setEnabled(true);
        }
        catch (Exception err) {
            err.printStackTrace();
        }
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    public final static ImageIcon createImageIcon(String path, String description) {
        URL imgURL = null;
        ImageIcon result = null;

        try {
            imgURL = JDemo.class.getClassLoader().getResource(path);

            if (imgURL == null) {
                imgURL = (new File(path)).toURL();
            }

            if (imgURL != null) {
                result = new ImageIcon(imgURL, description);
            }
            else {
                result = null;
            }
        }
        catch (MalformedURLException e) {
        }

        return result;
    }

    public static void main(String[] args) {
        try {
        	if (args.length != 0) {
        		if (args[0].equals("1")) {
        			JDemo app = new JDemo(true);
        			app.show();
        		}
        		else {
        			JDemo app = new JDemo(false);
        			app.show();
        		}
        	}
        	else {
        		JDemo app = new JDemo(false);
        		app.show();
        	}
        }
        catch (Exception ex) {
            System.err.println("Exception:" + ex);
        }

    }

}
