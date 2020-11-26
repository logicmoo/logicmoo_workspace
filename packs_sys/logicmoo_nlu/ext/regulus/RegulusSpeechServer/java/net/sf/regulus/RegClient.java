
package net.sf.regulus;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Class that interfaces to the regserver process.<br>
 * The RegClient class offers basic functionality for recognition, interpretation and playback.
 * 
 * <p>
 * On object creation a new <i>regserver</i> instance is started, which initiates the recognition engine.
 * After the <i>regserver</i> has initialized it listens for incoming requests.
 * </p>
 * <p>
 * The RegClient object connects to the regserver. After that recognition/playback related functions are available.   
 * </p>
 * <p>
 * The library assumes that the 'regulus' variable is set (i.e. on the java command line -Djava=${REGULUS}).
 * Default configuration values for the regserver process are defined in ${REGULUS}/RegulusSpeechServer/runtime/regulus.properties
 * </p>
 *<p>
 * Example of cmd command to run the regerver:
	#"d:/regulus/regulusspeechserver/runtime/win32/regserver.exe 
	#    -package d:/regulus/examples/calendar/generated/recogniser -p 1975 
	#     client.TTSAddresses=localhost audio.OutputVolume=200 
	#     rec.ConfidenceRejectionThreshold=0 rec.Pruning=1600 
	#     rec.DoNBest=TRUE rec.NumNBest=6 
	#     client.RecordDirectory=../corpora/speech/"
	</p> 
 */
public class RegClient {
	public static Logger logger = Logger.getLogger(RegClient.class.getPackage().getName());
	
	public int PARAM_TYPE_INT = 1;
	public int PARAM_TYPE_FLOAT = 2;
	public int PARAM_TYPE_STRING = 3;
		
    String regulusBaseDirectory = null;;
	String regulusServerHost = "127.0.0.1";
	String regulusServerCommand ="regserver" ; // = "D:/Regulus/RegulusSpeechServer/runtime/win32/regserver";
    String regClientConfig = "";
    
    final static String regRuntimeDir = "/RegulusSpeechServer/runtime/";

    int regulusServerPort = 1974; 
	long regServerStartupTimeout = 5000;
	String encoding = "iso-8859-1";

	Process regServerProcess = null;	
	Socket regClientSocket; 
	OutputStream out ;
	BufferedReader in;

	Object rcClientLock ;

	/**
	 * 
	 * @param serverHost The name (or IP address) of the host where regserver is running
	 * @param serverPort The port the  regserver is waiting for connections
	 * @param regClientConfig Parameters that get passed to the regserver (Nuance recognition client parameters)
	 * @param regulusServerCommand Path to the regserver executable 
	 * @param regulusServerTimeout How long to wait for regserver to start up
	 * @throws Exception Connection to the <i>regserver</i> failed.
     * @deprecated use Regclient(String) instead
	 */
	public RegClient(String serverHost, int serverPort, String regClientConfig, String regulusServerCommand, long regulusServerTimeout) throws Exception, RegClientException {
		init(serverHost, serverPort, regClientConfig, regulusServerCommand, regulusServerTimeout);
	}

	/**
	 * 
	 * @param serverPort The name (or IP address) of the host where regserver is running
	 * @param regClientConfig Parameters that get passed to the regserver (Nuance recognition client parameters)
	 * @param regulusServerCommand Path to the regserver executable 
	 * @param regulusServerTimeout How long (in msecs) to wait for regserver to start up
	 * @throws Exception Connection to the <i>regserver</i> failed.
     * @deprecated use Regclient(String) instead
	 */
	public RegClient(int serverPort, String regClientConfig, String regulusServerCommand, long regulusServerTimeout) throws Exception {
		init(regulusServerHost, serverPort, regClientConfig, regulusServerCommand, regulusServerTimeout);
	}

    /**
     * Sets the encoding to be used when communicating with the server.
     * When the object is created the encoding is set to "iso-8859-1"
     * @param newEncoding
     */
    public void setEncoding(String newEncoding) {
        encoding = newEncoding;
    }
    
    /**
     * Starts up a regulus client library along with the regserver
     * @param port - portnumber of the regserver process. 
     * @param recognitionPackage - Recognition package to load.
     * @param configParams - one or more Nuance configuration parameters.
	 * @author georgesc
     */
    public RegClient(int port, String recognitionPackage, String configParams) throws Exception {
        if(port <= 0){
            throw new RegClientException("Illegal port number");
        }

        try{
            loadConfiguration();
            this.regulusServerPort = port;
            String regClientParams = this.regClientConfig;

            if( (recognitionPackage!= null) && (! recognitionPackage.trim().equals(""))){
            	if (configParams != null)
	                regClientParams = regClientParams + " -package " + recognitionPackage + " " + configParams;
                else
   	                regClientParams = regClientParams + " -package " + recognitionPackage;
            }
            else{
                logger.severe("No or empty recognition package");
            }

            init(regulusServerHost, this.regulusServerPort, regClientParams, regulusServerCommand, this.regServerStartupTimeout);
        }
        catch(Exception e){
            logger.log(Level.SEVERE, "Couldn't initialize regserver", e);
            throw e;
        }
    }
    
    /**
     * Starts up a regulus client library along with the regserver
     * @param port - portnumber of the regserver process. 
     * @param recognitionPackageAndConfigParams - Starts with the recognition package to load. May be followed by one or more nuance configuration parameters.
     * May be followed by one or more nuance configuration parameters
     */
    public RegClient(int port, String recognitionPackageAndConfigParams) throws Exception {
        if(port <= 0){
            throw new RegClientException("Illegal port number");
        }

        try{
            loadConfiguration();
            this.regulusServerPort = port;
            String regClientParams = this.regClientConfig;
            if( (recognitionPackageAndConfigParams!= null) && (! recognitionPackageAndConfigParams.trim().equals(""))){
                regClientParams = regClientParams + " -package " + recognitionPackageAndConfigParams;
            }
            else{
                // warn - recognition package is empty
            }

            init(regulusServerHost, this.regulusServerPort, regClientParams, regulusServerCommand, this.regServerStartupTimeout);
        }
        catch(Exception e){
            logger.log(Level.SEVERE, "Couldn't initialize regserver", e);
            throw e;
        }
    }
    
    /**
     * Starts up a regulus client library along with the regserver
     * @param recognitionPackageAndConfigParams - Starts with the recognition package to load. May be followed by one or more nuance configuration parameters.
     * May be followed by one or more nuance configuration parameters
     */
    public RegClient(String recognitionPackageAndConfigParams) throws RegClientException, IOException {
        try{
            loadConfiguration();
            String regClientParams = this.regClientConfig;
            if( (recognitionPackageAndConfigParams!= null) && (! recognitionPackageAndConfigParams.trim().equals(""))){
                regClientParams = regClientParams + " -package " + recognitionPackageAndConfigParams;
            }
            else{
                // warn - recognition package is empty
            	throw new RegClientException("Recognition package is empty");
            }

		    init(regulusServerHost, this.regulusServerPort, regClientParams, regulusServerCommand, this.regServerStartupTimeout);
        }
        catch(RegClientException e){
            logger.log(Level.SEVERE, "Couldn't initialize regserver", e);
            throw e;
        }
    }
    
    private void loadConfiguration() throws IOException, RegClientException {
        // String regulusEnv = System.getProperty("regulus", "D:/Regulus");
    	String regulusEnv = System.getProperty("regulus");
    	if ((regulusEnv == null) && (regulusBaseDirectory==null)){
    		logger.log(Level.SEVERE, "Please check if the REGULUS environment variable exist. ");    		
    		 throw new net.sf.regulus.RegClientException("The REGULUS environment variable should be setup."); 
    	}
    		
   		regulusBaseDirectory = regulusEnv;

        String regulusBinDir = regulusBaseDirectory + regRuntimeDir;
        final String regulusPropertyFile = regulusBinDir + "regulus.properties";
        
        logger.log(Level.INFO, "Regulus base directory set to '" + regulusBaseDirectory + "'");            
        Properties regulusProperties = new Properties();
    
        InputStream is = null;
        
        try {
            is = new FileInputStream(regulusPropertyFile);
        }
        catch(FileNotFoundException fno){
            logger.log(Level.SEVERE, "Failed to open regulus config file '" + regulusPropertyFile + "'");
            throw new RegClientException("couldn't read regulus configuration file.");
        }

        if(is == null){
            is = getClass().getResourceAsStream(regulusPropertyFile);
        }
        
        if( is != null) {
            logger.log(Level.INFO, "Loading configuration from '" + regulusPropertyFile + "'");
            regulusProperties.load(is);
        }
        else{
            logger.log(Level.WARNING, "Failed to load default regulus properties file '" + regulusPropertyFile+ "'");
        }

        this.regulusServerHost = regulusProperties.getProperty("regulus.regServer.Host", this.regulusServerHost);
        try{
            this.regulusServerPort = Integer.parseInt(regulusProperties.getProperty("regulus.regServer.Port", Integer.toString(this.regulusServerPort)));
        }
        catch(NumberFormatException nfe){
            logger.log(Level.WARNING, "Failed to set regulus server timeout value from configuration.");
        }
        
        try{
            this.regServerStartupTimeout = Long.parseLong(regulusProperties.getProperty("regulus.regClient.ServerTimeout", Long.toString(this.regServerStartupTimeout)));
        }
        catch(NumberFormatException nfe){
            logger.log(Level.WARNING, "Failed to set regulus server timeout value from configuration.");
        }
        
        //
        // set the regserver binary
        //
        String osName = System.getProperty("os.name").toLowerCase();
        String osArch = System.getProperty("os.arch").toLowerCase();
        
        if(osName.startsWith("windows")){
            this.regulusServerCommand = regulusBinDir + "regserver";
        }
        else if(osName.startsWith("sunos") && osArch.equals("sparc")){
            this.regulusServerCommand = regulusBinDir + "sparc-solaris/regserver";
        }
        
        //
        // read config parameters
        //
        
        this.regClientConfig = regulusProperties.getProperty("regulus.regServer.ConfigParams", "");
        logger.log(Level.INFO, "Regulus properties '" + this.regClientConfig + "'");
    }


    /**
     * Initializes a new RegClient object. 
     * This methos is called from the object's constructor.
     * It actually execute the cmd for getting started the regulus server (i.e. 'regserver'). 
     * @param regulusServerHost The name (or IP address) of the host where regserver is running
     * @param regulusServerPort The port the  regserver is waiting for connections
     * @param regClientConfigParams Parameters that get passed to the regserver (Nuance recognition client parameters)
     * @param regulusServerCommand Path to the regserver executable 
     * @param regulusServerTimeout How long (in msecs) to wait for regserver to start up
     * @throws Exception
     */
    private void init(String regulusServerHost, int regulusServerPort, String regClientConfigParams, String regulusServerCommand, long regulusServerTimeout) throws RegClientException {
		rcClientLock = new Object();
		encoding = "iso-8859-1";
		this.regulusServerHost = regulusServerHost;
		this.regulusServerPort = regulusServerPort;
		regServerStartupTimeout = regulusServerTimeout;
		Exception exception = null;

		try {
			// launh the regserver
			String command = regulusServerCommand + " -port " + regulusServerPort + " " + regClientConfigParams;
			// String command = regulusServerCommand + " " + regClientConfigParams;
            command = command.trim();
			logger.info("Launching '" + command + "'");
			regServerProcess = Runtime.getRuntime().exec(command);

			long timeOutIncrements = regServerStartupTimeout/4;

			// connecting to regserver
			for(int i=0 ; i<4 ; i++){
				try{
					try{
                        logger.log(Level.INFO, "Waiting " + timeOutIncrements + " msecs for regulus server to start up");
						Thread.sleep(timeOutIncrements);
					}
					catch(InterruptedException e){
					}

                    logger.log(Level.INFO, "Connecting to " + regulusServerHost + ":" + regulusServerPort + " (attemt no. " + i + ")");
					regClientSocket = new Socket(regulusServerHost, regulusServerPort);
                    logger.log(Level.INFO, "Connected to port " + regClientSocket.getPort());
					break;
				}
				catch(Exception se){
				    exception = se;
				}
			}

            if(regClientSocket != null){
                // encoding is set to iso8859-1 by default. better to get the language's encoding and set this instead. 
				in = new BufferedReader(new InputStreamReader(regClientSocket.getInputStream(), encoding));
                out = regClientSocket.getOutputStream();
			}
			else{
			    if (exception == null){
			        throw new Exception("Could not connect to server socket");
			    }
			    else{
			        throw exception;
			    }
			}
		}
		catch (Exception e) {
			regClientSocket = null;
			if(out != null){
                try{
                    out.close();
                }
                catch (IOException e1) {
                }
			}
			if(in != null){
				try {
					in.close();
				}
				catch (IOException e1) {
				}
			}
			logger.log(Level.SEVERE, "Connection to '"+ regulusServerHost + ":" + regulusServerPort + "' failed" , e);
			throw new RegClientException("Could not connect to " + regulusServerHost + ":" + regulusServerPort + "\nCause: '" + e + "'");
		}
	}

    /**
     * Exit <code>regserver</code> process, and release held resources.
     * @throws IOException
     */
	public void shutdown() throws IOException{
		send("CLEAN_UP");

		out.close();

		try {
			in.close();
		}
		catch (IOException e) {
			System.out.println("IOException when trying to close: the BufferedReader corresponding to regClient! ");
		}

		try {
			regClientSocket.close(); // closes the regclient socket
		}
		catch (IOException e1) {
			System.out.println("The regClient socket can not be closed! ");
		}
	}

	/**
	 * Sets a Nuance parameter. 
	 * @param param
	 * @param value
	 * @return Returns true if the set parameter command was succesfull.
	 * @throws IOException
	 */
	public boolean setParameter(String param, String value) throws IOException {
	    logger.entering(this.getClass().getName(), "setParameter");

	    boolean returnValue = false;

	        String command = "SET_PARAMETER " + param.trim() + " " + value;
	    synchronized (rcClientLock) {
			send(command);

			String reply = in.readLine();

			if(! reply.equals("set_parameter_ok.")){
			    returnValue = false;
			}
			else{
			    returnValue = true;
			}
	    }

	    logger.exiting(this.getClass().getName(), "setParameter");
	    return returnValue;
	}

	/**
	 * Initiate a playback action. This function returns after the playback request has been submitted to the underlying platform. 
	 *  
	 * @param fileOrTTS If the parameter is a path to a file, it is played directly, 
	 *  otherwise if the string starts with +TTS it is submitted to a speech synthesis engine.
	 * @throws IOException
	 */
	public void play(String fileOrTTS) throws IOException {
		send("SAY_LIST " + fileOrTTS);
	}

	/**
	 * Initiate a playback action. This function returns after the playback request has been submitted to the underlying platform.
	 *  
	 * @param fileOrTTSList If the item is a path to a file, it is played directly, 
	 * otherwise if the string starts with +TTS it is submitted to a speech synthesis engine.
	 * @throws IOException
	 */
	public void playList(String []fileOrTTSList) throws IOException{
		StringBuffer command = new StringBuffer();

		for(int i=0 ; i<fileOrTTSList.length-1; i++){
			command.append(fileOrTTSList[i]);
			command.append(",");
		}
		command.append(fileOrTTSList[fileOrTTSList.length-1]);

		send("SAY_LIST " + command.toString());
	}

	public float getFloatParameter(String paramName) throws IOException, RegClientException{
	    Object result = getParameter(paramName);
	    
	    if(! (result instanceof Float)){
	        throw new RegClientException("Parameter '" + paramName + "' is not of type 'float'");    
	    }

	    return ((Float)result).floatValue();
	}

	public int getIntParameter(String paramName) throws IOException, RegClientException{
	    Object result = getParameter(paramName);
	    
	    if(! (result instanceof Integer) ){
	        throw new RegClientException("Parameter '" + paramName + "' is not of type 'int'");
	    }

	    return ((Integer)result).intValue();
	}

	public String getStringParameter(String paramName) throws IOException, RegClientException{
	    Object result = getParameter(paramName);
	    
	    if(! (result instanceof String) ){
	        throw new RegClientException("Parameter '" + paramName + "' is not of type 'String'");
	    }

	    return (String)result;

	}

	public Object getParameter(String paramName) throws IOException, RegClientException {
		logger.entering(this.getClass().getName(), "getParameter");
		String serverResponse = null;
		Object result = null;

		//
		// retrieve parameter
		//
		synchronized (rcClientLock) {
			send("GET_PARAMETER " + paramName);

			serverResponse = in.readLine();			
			rcClientLock.notifyAll();
		}
		
		//
		// server reply looks like this: 'parameter(int, param.Name, 5).'
		// w/o the single quotes
		//
		if(serverResponse.startsWith("parameter(error, ")){
		    throw new RegClientException(serverResponse.substring("parameter(error, ".length()+1, serverResponse.length()-2));
		}
		else if(serverResponse.startsWith("parameter(int, ")){
		    String intString = serverResponse.substring("parameter(int, ".length() + paramName.length() + 2, serverResponse.length()-2);
		    
		    try{
		        result = new Integer(intString);
		    }
		    catch(NumberFormatException nfe){
		        throw(new RegClientException(nfe.toString()));
		    }
		}
		else if(serverResponse.startsWith("parameter(float, ")){
		    String floatString = serverResponse.substring("parameter(float, ".length() + paramName.length() + 2, serverResponse.length()-2);
		    
		    try{
		        result = new Float(floatString);
		    }
		    catch(NumberFormatException nfe){
		        throw(new RegClientException(nfe.toString()));
		    }
		}

		else if(serverResponse.startsWith("parameter(string, '")){
			  result = serverResponse.substring("parameter(string, '".length() + paramName.length() + 4, serverResponse.length()-3);
		}
		
	    // this shouldn't happen
		if(result == null){
		    throw new RegClientException("Error when getting or when interpretting the msg received from regserver. ");
		}


		logger.exiting(this.getClass().getName(), "getParameter");
		return result;
	}


	/**
	 * Perform a recognition action. This function blocks untill recognition returns.
	 * It does however not mean that the value returned will contain a valid recognition result.
	 * The result might for example contain an error indication or a timeout.   
	 *  
	 * @param grammar Recognition grammar used.
	 * @return Recognition result
	 * @throws IOException
	 */
	public RegResult recognize(String grammar) throws IOException{
		logger.entering(this.getClass().getName(), "recognize");
		String reply = null;
		RegResult  result = null;

		synchronized (rcClientLock) {
			
			send("RECOGNISE " + grammar);
			reply = new String(in.readLine().getBytes(), encoding);

			result = new RegResult(reply);
			rcClientLock.notifyAll();
		}
		logger.exiting(this.getClass().getName(), "recognize");
		return result;
	}

	/**
	 * Perform a recognition action in N-best mode. 
	 * This function blocks untill recognition returns.
	 * It does however not mean that the value returned will contain a valid recognition result.
	 * The result might for example contain an error indication or a timeout.   
	 * @author GEORGESC
	 * @param grammar Recognition grammar used.
	 * @return Recognition result
	 * @throws IOException
	 */
	public RegResultNBest recognizeNBestMode(String grammar) throws IOException{
		logger.entering(this.getClass().getName(), "recognizeNBestMode");
		String reply = null;
		RegResultNBest  result = null;

		synchronized (rcClientLock) {

			send("RECOGNISE " + grammar);
			reply = new String(in.readLine().getBytes(), encoding);

			result = new RegResultNBest(reply);

			rcClientLock.notifyAll();
		}
		logger.exiting(this.getClass().getName(), "recognizeNBestMode");
		return result;
	}
	
	/**
	 * Perform a recognition action in N-best. 
	 * This function blocks untill recognition returns.
	 * It does however not mean that the value returned will contain a valid recognition result.
	 * The result might for example contain an error indication or a timeout.   
	 * @author TSOURAKI
	 * @param grammar Recognition grammar used.
	 * @return Recognition result
	 * @throws IOException
	 */
	public NBestRegResult recognizeNBest(String grammar) throws IOException{
		logger.entering(this.getClass().getName(), "recognizeNBest");
		String reply = null;
		NBestRegResult  result = null;

		synchronized (rcClientLock) {

			send("RECOGNISE " + grammar);
			reply = new String(in.readLine().getBytes(), encoding);

			result = new NBestRegResult(reply);

			rcClientLock.notifyAll();
		}
		logger.exiting(this.getClass().getName(), "recognizeNBest");
		return result;
	}
	
	/**
	 * Perform Natural Language (NL) interpretation. This function blocks untill interpretation returns.
	 * @param grammar The grammar context the input text will be parsed with 
	 * @param text Input text we want to get interpreted 
	 * @return The RegResult object returned will contain a valid interpretation if no error was encountered. Check the result's status before using this object.  
	 * @throws IOException
	 */
	public RegResult interpret(String grammar, String text) throws IOException{
		logger.entering(this.getClass().getName(), "interpret");
		String reply = null;
		RegResult  result = null;

		synchronized (rcClientLock) {

			send("INTERPRET " + grammar + " " + text);
			reply = in.readLine();

			result = new RegResult(reply);

			rcClientLock.notifyAll();
		}

		logger.exiting(this.getClass().getName(), "interpret");
		return result;
	}

	/**
	 * Perform Natural Language (NL) interpretation. This function blocks untill interpretation returns.
	 * @param grammar The grammar context the input text will be parsed with 
	 * @param text Input text we want to get interpreted 
	 * @return The RegResult object returned will contain a valid interpretation if no error was encountered. Check the result's status before using this object.  
	 * @throws IOException
	 */
	public RegResult interpretNBestMode(String grammar, String text) throws IOException{
		logger.entering(this.getClass().getName(), "interpret");
		String reply = null;
		RegResultNBest  result = null;

		synchronized (rcClientLock) {

			send("INTERPRET " + grammar + " " + text);
			reply = in.readLine();

			result = new RegResultNBest(reply);

			rcClientLock.notifyAll();
		}

		logger.exiting(this.getClass().getName(), "interpret");
		return result;
	}
	
	/**
	 * Perform a recognition action on a prerecorded file. This function blocks untill recognition returns.
	 * @see #recognize(String)
	 * @param file Prerecorded file
	 * @param grammar
	 * @return The RegResult object returned will contain a valid interpretation if no error was encountered. Check the result's status before using this object.
	 * @throws IOException
	 */
	public RegResult recognizeFile(String file, String grammar) throws IOException{
		logger.entering(this.getClass().getName(), "recognizeFile");
		RegResult result = null;
		String reply = null;

		synchronized (rcClientLock) {
			if( (file == null) || (file.trim().equals(""))){
				result = new RegResult(RegResult.ACTION_ERROR);
			}
			else{
				send("RECOGNISE_FILE " +  file.trim() + " " + grammar.trim());

				String tmp = in.readLine();
				logger.finest("XXX Server returned: '" + tmp + "'");

				reply = new String(tmp.getBytes(), encoding);
				logger.finest("XXX encoded reply: '" + reply + "'");
				
				result = new RegResult(reply);
				
				logger.finest("XXX result string: '" + result.getRecognition() + "'");
				logger.finest("XXX result interpretation: '" + result.getInterpretation() + "'");
			}
			rcClientLock.notifyAll();
		}

		logger.exiting(this.getClass().getName(), "recognizeFile");
		return result;
	}


	/**
	 * Perform a recognition action on a prerecorded file. 
	 * This function blocks untill recognition returns.
	 * @see #recognize(String)
	 * @param file Prerecorded file
	 * @param grammar
	 * @return The RegResultNBest object returned will contain a valid interpretation if no error was encountered. 
	 * Check the result's status before using this object.
	 * @throws IOException
	 * @author GEORGESC
	 */
	public RegResultNBest recognizeFileNBestMode(String file, String grammar) throws IOException{
		logger.entering(this.getClass().getName(), "recognizeFileNBestMode");
		RegResultNBest result = null;
		String reply = null;

		synchronized (rcClientLock) {
			if( (file == null) || (file.trim().equals(""))){
				result = new RegResultNBest(RegResult.ACTION_ERROR);
			}
			else{
				send("RECOGNISE_FILE " +  file.trim() + " " + grammar.trim());

				String tmp = in.readLine();
				logger.finest("XXX Server returned: '" + tmp + "'");

				reply = new String(tmp.getBytes(), encoding);
				logger.finest("XXX encoded reply: '" + reply + "'");				
				// System.out.println("reply from regserver : " + reply);

				result = new RegResultNBest(reply);
				
				logger.finest("XXX result string: '" + result.getRecognition() + "'");
				logger.finest("XXX result interpretation: '" + result.getInterpretation() + "'");
			}
			rcClientLock.notifyAll();
		}

		logger.exiting(this.getClass().getName(), "recognizeFileNBestMode");
		return result;
	}

	/**
	 * Abort an ongoing recognition action
	 */
	public void abortRecognition() throws IOException {
		logger.entering(this.getClass().getName(), "recognize");
		send("ABORT_RECOGNITION");
		logger.exiting(this.getClass().getName(), "recognize");
	}

	/**
	 * Abort an ongoing playback action 
	 */
	public void abortPlayback() throws IOException{
		send("ABORT_PLAYBACK");
	}

	/**
	 * Sends the message to the server via the "out" attribute of type OutputStream.
	 * @param message
	 * @throws IOException
	 */
	public synchronized void send(String message) throws IOException {
	    logger.finest("sending : '" + message.trim() + "'");
	    
	    String strToSend = message.trim() + "\n";
		
        out.write(strToSend.getBytes(encoding));
	    out.flush();
	}

    /**
     * @return Returns the regulusBaseDirectory.
     */
    public String getRegulusBaseDirectory() {
        return regulusBaseDirectory;
    }

}
