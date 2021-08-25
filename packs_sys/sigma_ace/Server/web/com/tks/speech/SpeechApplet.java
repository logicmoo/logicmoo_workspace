package com.tks.speech;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.Locale;
import java.io.*;
import java.net.*;
import java.util.*;



import javax.speech.*;
import javax.speech.recognition.*;
import javax.speech.synthesis.*;

import org.jdom.*;
import org.jdom.input.*;

/**
 * <p>Title: SpeechApplet</p>
 * <p>Description: SpeechApplet is implementation of of java speech api to support SR & TTS functionality. This applet needs java speech api jar file to perform speech reorganization and speech synthesis. </p>
 * <p>Copyright: Teknowledge Corp Copyright (c) 2002</p>
 * <p>Company: Teknowledge Corpration</p>
 * @author bvasired@teknowledge.com
 * @version 1.0
 */

public class SpeechApplet extends Applet {

  /**
  *	Static field to download speech dll file into client machine.
  */
  public static final String SPEECH_DLL_FILE = "cgjsapi141.dll";

  /**
  *usefull to redirect log information to various streams.
  */
  public PrintStream logStream = System.out;
  
  /**
  *specifies the applet is standalone
  */
  boolean isStandalone = false;
  
  /**
  *	Recognizer to recognize user speech. 
  */
  static Recognizer rec;
  
  /**
  *	Synthesizer to speak response of server. 
  */
  static Synthesizer synth;
  
  /**
  *	TextArea Pannel to keep TextArea component.
  */
  private Panel textAreaPanel = new Panel();
  
  /**
  *	TextArea to display user speech before submitting to server.
  */
  private java.awt.TextArea textArea;

  /**
  *	Button Pannel to keep button component.
  */
  private Panel buttonPanel = new Panel();

  /**
  *	Button to clear TextArea text.
  */
  private java.awt.Button  clearButton;
  
  /**
  *	Button to submit TextArea text to server.
  */  
  private java.awt.Button  submitButton;


  /**
  *	Gets parameter information from applet tags.
  */
  public String getParameter(String key, String def) {
    return isStandalone ? System.getProperty(key, def) :
      (getParameter(key) != null ? getParameter(key) : def);
  }

  /**
  *	Constructor of Applet.
  */
  public SpeechApplet() {
  }

  /**
  *Initialize method of the applet. This method will create all panels and required comopnents to display on screen.
  */
  public void init() {

            textArea = new java.awt.TextArea();
            clearButton = new java.awt.Button("Reset");
            submitButton = new java.awt.Button("Submit");


        FlowLayout layout1 = new FlowLayout(FlowLayout.CENTER, 10, 10);
         textAreaPanel.setLayout(layout1);
         textAreaPanel.add(textArea);
         add(textAreaPanel);


         FlowLayout layout2 = new FlowLayout(FlowLayout.CENTER, 10, 10);
         buttonPanel.setLayout(layout2);

         buttonPanel.add(clearButton);
         buttonPanel.add(submitButton);
         add(buttonPanel);


          clearButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clearButtonActionPerformed(evt);
            }
          });


          submitButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                submitButtonActionPerformed(evt);
            }
          });

          try {
              // logStream = new PrintStream(new FileOutputStream("speech.log"));

               jbInit();
          }
          catch(Exception e) {
                e.printStackTrace(logStream);
          }

  }

	/**
	*	Clears the textArea when user presses clear button on applet.
	*/
    private void clearButtonActionPerformed(java.awt.event.ActionEvent evt) {
        textArea.setText("");
    }

	/**
	*	Submits user input to server. before submitting it cleans user input.
	*/
    private void submitButtonActionPerformed(java.awt.event.ActionEvent evt) {
       // textArea.setText(getCodeBase().toString());
        String line = "";
        StringBuffer buffer = new StringBuffer();

        try{
            URL url = new URL(getCodeBase().toString() + "/celtqueryhandler");
            URLConnection connect = url.openConnection();
            connect.setDoOutput(true);
            connect.setDoInput(true);
            connect.setUseCaches(false);
            //connect.addRequestProperty("query","aaaa");
            String query = "query="+ textArea.getText();
			//cleaning the query
			query = query.replace('\t',' ');
			query = query.replace('\n',' ');
			query = query.replace('\r',' ');
			query = query.replace('\f',' ');
			query = query.trim();

            PrintWriter out = new PrintWriter(connect.getOutputStream());
            logStream.println(query);
            out.write(query);
            out.flush();
            out.close();



            BufferedReader reader = new BufferedReader(new InputStreamReader(connect.getInputStream()));

            while( (line = reader.readLine()) != null ){

              buffer.append(line);
            }

        }catch (Exception e)
        {
          logStream.println("exception happened " + e);
          repaint();
          return;
        }

        textArea.setText(buffer.toString());

        try{

        	String warning = getWarning(buffer.toString().replaceAll("<->", ":"));
        	logStream.println("warning in result " + warning);
        	synth.speakPlainText(warning,null);

		}catch(Exception e)
        {
          logStream.println("exception happened while getting warning " + e);
          repaint();
          return;
        }

    }

	/**
	*	takes server response and parses using SaXParser. It gets warning message from xml data and pass back to called method.
	*/
    public String getWarning(String result) throws Exception{

		Document document = new SAXBuilder().build(new StringReader(result));

		Element rootElement = document.getRootElement();

		java.util.List resultElements = rootElement.getChildren("result");

		if(resultElements.size() == 0){
			return "";
		}


		for(int i =0; i < resultElements.size(); i++){

			Element resultElement  = (Element) resultElements.get(i);

			java.util.List varElements = resultElement.getChildren("var");

			for(int j = 0; j < varElements.size(); j++){

				Element varElement = (Element) varElements.get(j);

				Attribute warningAttribute = varElement.getAttribute("name");

				if(warningAttribute != null && warningAttribute.getValue().equalsIgnoreCase("warning" )){

					return varElement.getText();
				}

			}


		}

		return "";


	}

	/**
	*	This method checks required dll file in client machine and downloads it, if it is not available.
	*/
    private void jbInit() throws Exception {

    	URL url = new URL(getCodeBase().toString()  + SpeechApplet.SPEECH_DLL_FILE);
        logStream.println("url to download dll file " + url.toString());
        InputStream in = url.openStream();
        String javaLibraryPath = System.getProperty("java.library.path");
        StringTokenizer st = new StringTokenizer(javaLibraryPath,";");
        String path = null;

        while(st.hasMoreTokens()){

          path = st.nextToken();
          if(path.toUpperCase().endsWith("SYSTEM32") || path.toUpperCase().endsWith("SYSTEM") || path.toUpperCase().endsWith("WINNT") || path.toUpperCase().endsWith("WINDOWS") || path.toUpperCase().endsWith("WIN")){
            break;
          }
       }

          File file = new File(path + java.io.File.separator + SpeechApplet.SPEECH_DLL_FILE);
          logStream.println("place to download dll file " + file.getAbsolutePath() );
          if(!file.exists()){
              FileOutputStream fin = new FileOutputStream(file);

              int readByte;

              while( (readByte = in.read()) != -1){
                fin.write(readByte);
              }

              in.close();
              fin.flush();
              fin.close();


              System.load(file.getAbsolutePath());
              logStream.println(file + " downloaded and loaded as library");
          }else{
            System.load(file.getAbsolutePath());
            logStream.println(file + " is available in local drive, so loaded as library");
            }





  }

	/**
	*This method will be called while applet is executed on browser. responsible to start speech reorganization and synthesis engines on client machine. 
	*/
  	public void start() {

      try{

             logStream.println("about to start engines");
              if(rec == null){
                   initRecognizer();
                }

              if(synth == null){
                  initSynthesizer();
              }


          }catch(Exception e){
            logStream.println(e);
          }

  	}

  /**
  *	Starts Recognizer in client machine.
  */
  public void initRecognizer() throws Exception{

	try{
        // Create a recognizer that supports English.
  		rec = Central.createRecognizer(new EngineModeDesc(Locale.ENGLISH));
        rec.addEngineListener(new EngineListener());

  		// Start up the recognizer
  		rec.allocate();
        rec.waitEngineState(Recognizer.ALLOCATED);
        DictationGrammar dictation = rec.getDictationGrammar(null);
        dictation.setEnabled(true);
        rec.suspend();
        rec.commitChanges();

        rec.waitEngineState(rec.LISTENING);

        // Request focus and start listening
  		rec.requestFocus();
  		rec.resume();
        rec.addResultListener(new ResultListener(rec));
    }catch (Exception e){
		logStream.println("recognizer init failure " + e);
		logStream.flush();
		throw e;
	}

  }

  /**
  *	Starts Synthesizer in client machine.
  */
  public void initSynthesizer() throws Exception{

        synth = Central.createSynthesizer(null);
        if(synth == null) {
          return;
        }

        synth.allocate();
        synth.resume();
        synth.waitEngineState(Synthesizer.ALLOCATED);
        SynthesizerProperties props = synth.getSynthesizerProperties();
        Voice v = new Voice("Microsoft Mary",Voice.GENDER_FEMALE, Voice.AGE_DONT_CARE, null);
        props.setVoice(v);
  }


	/**
	*	This methood will be executed when appleet is stoped in browser. This method deallocate Recognizer and Synthesizer.
	*/
  public void stop() {

    try{
      //synth cleaning.
      synth.cancel();
      synth.waitEngineState(Synthesizer.QUEUE_EMPTY);
      synth.deallocate();
      synth.waitEngineState(Synthesizer.DEALLOCATED);

      //recog cleaning
      if(rec.testEngineState(rec.ALLOCATED)) rec.pause();
      if(rec.testEngineState(rec.PROCESSING))
        rec.waitEngineState(rec.LISTENING);
      rec.deallocate();
      rec.waitEngineState(Recognizer.DEALLOCATED);
    }catch(Exception e){
      logStream.println(e);
    }

  }
  
  /*
  *Destroy the applet when browser is closed in client machine.
  */
  public void destroy() {


  }

  /**
  *Get Applet information
  */
  public String getAppletInfo() {
    return "Applet Information";
  }


  /**
  *	Get parameter information from applet tags.
  */
  public String[][] getParameterInfo() {
    return null;
  }
  
  /**
  *	Inner class to keep track of Recognizer status and Synthesizer status.
  */
  public class EngineListener implements SynthesizerListener,
					   RecognizerListener {
    	public void  engineAllocated(EngineEvent e) {
			logStream.println(e.getSource()+" engineAllocated");
    	}
    
    	public void  engineAllocatingResources(EngineEvent e) {
			logStream.println(e.getSource()+" engineAllocatingResources");
    	}
    
    	public void  engineDeallocated(EngineEvent e) {
			logStream.println(e.getSource()+" engineDeallocated");
    	}
    	
    	public void  engineDeallocatingResources(EngineEvent e) {
			logStream.println(e.getSource()+" engineDeallocatingResources");
    	}
    	
    	public void  engineError(EngineErrorEvent e) {
			logStream.println(e.getSource()+" engineError");
    	}
    	public void  enginePaused(EngineEvent e) {
			logStream.println(e.getSource()+" enginePaused");
    	}
    
    	public void  engineResumed(EngineEvent e) {
			logStream.println(e.getSource()+" engineResumed...");
    	}
    
    	public void  recognizerProcessing(RecognizerEvent e) {
			logStream.println(e.getSource()+" recognizerProcessing");
    	}
    
    	public void  recognizerListening(RecognizerEvent e) {
			logStream.println(e.getSource()+" recognizerListening");
    	}
    
    	public void recognizerSuspended(RecognizerEvent e) {
			logStream.println(e.getSource()+" recognizerSuspended");
    	}
    	
    	public void changesCommitted(RecognizerEvent e) {
			logStream.println(e.getSource()+" changesCommitted");
    	}
    
    	public void focusGained(RecognizerEvent e) {
			logStream.println(e.getSource()+" focusGained");
    	}
    
    	public void focusLost(RecognizerEvent e) {
			logStream.println(e.getSource()+" focusLost");
    	}
    	
    	public void  queueEmptied(SynthesizerEvent e) {
			logStream.println(e.getSource()+" queueEmptied");
    	}
    
    	public void  queueUpdated(SynthesizerEvent e) {
			logStream.println(e.getSource()+" queueUpdated");
		}

	}


	/**
	*	Inner class to recognize user speech and announce  server warning response.
	*/
	public class ResultListener extends ResultAdapter {
    	private int nRecs = 0;
    	private Recognizer rec;
    	private boolean playAudio;


    	public ResultListener() {
    	}

   		public ResultListener(Recognizer rec) {
        	this.rec = rec;

    	}

    	public ResultListener(Recognizer rec, int nRecs, boolean playAudio) {
        	this.rec = rec;
        	this.nRecs = nRecs;
        	this.playAudio = playAudio;
    	}

    	public void resultRejected(ResultEvent e) {
        	Result r = (Result)(e.getSource());
        	logStream.println("Result Rejected ");
   	 	}
    
    	public void resultCreated(ResultEvent e) {
        	Result r = (Result)(e.getSource());
        	logStream.println("Result Created ");
    	}
    	
    	public void resultUpdated(ResultEvent e) {
        	Result r = (Result)(e.getSource());
        	logStream.println("Result Updated... "+r);
    	}

    	public  void resultAccepted(ResultEvent e) {
        	final FinalResult r = (FinalResult)(e.getSource());
        	Runnable lt = new Runnable() {
            	public void run() {
                	try {
                    	logStream.print("Result Accepted: "+r);

                    	ResultToken tokens[]  = r.getBestTokens();
                    	logStream.println("number of tokens " + tokens.length);
                     	
                     	for(int i=0;i<tokens.length;i++){
                      		String token = tokens[i].getSpokenText();
                      		textArea.append(token + " ");
                      		logStream.println(token);
                      	}


                	} catch(Exception e1) {
                    e1.printStackTrace(logStream);

                } catch(ResultStateError er) {
                    er.printStackTrace(logStream);
                }
                
                	nRecs--;
                	if(nRecs == 0) {
                    	try {
                        	rec.deallocate();
                    	} catch(Exception e2) {
                        	e2.printStackTrace(logStream);
                    }
                }
            }
        };
        (new Thread(lt)).start();

    }
}


}
