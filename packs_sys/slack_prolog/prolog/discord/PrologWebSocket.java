import javax.websocket.ClientEndpoint;
import javax.websocket.*;
import javax.websocket.ContainerProvider;
import javax.websocket.DeploymentException;
import javax.websocket.OnClose;
import javax.websocket.OnError;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.WebSocketContainer;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Integer;
import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;


import java.io.IOException;
import java.net.URI;

/*
     * @author Douglas Miles
*/
@ClientEndpoint
public class PrologWebSocket {

    public int DEBUG = 1;

    public static PrologWebSocket new_ws_client(String wsurl, String callback, int bufsize) throws IOException, DeploymentException {
        return new PrologWebSocket(wsurl, callback, bufsize);
    }

    Session userSession = null;
    String messageHandler = "websocket_event";
    WebSocketContainer container = null;//
    boolean DEAD = false;

    public void close() throws IOException {
        DEAD = true;
        if (userSession!=null)
            userSession.close();
        userSession = null;
    }

    public PrologWebSocket(String wsurl, String callback) throws IOException, DeploymentException {
        this(URI.create(wsurl), callback, -1);
    }

    public PrologWebSocket() {
    }
    public PrologWebSocket(String wsurl, String callback, int bufsize) throws IOException, DeploymentException {
        this(URI.create(wsurl), callback, bufsize);
    }

    public PrologWebSocket(URI endpointURI, String callback, int bufsize) throws IOException, DeploymentException {
        messageHandler = callback;
        try {
            int size = bufsize;
            if (size==-1) {
                // 64MB
                size = 1024 * 1024 * 64;
            }
            container = ContainerProvider.getWebSocketContainer();
            container.setDefaultMaxBinaryMessageBufferSize(size);
            container.setDefaultMaxTextMessageBufferSize(size);
            //container.setDefaultInboundMessageSizeLimit(size);
            //((ClientEndpoint)this).setInboundMessageSizeLimit(java.lang.Integer.MAX_VALUE);
            userSession=container.connectToServer(this, endpointURI);
	    callback("onCreateSuccess", this);
        } catch ( Error t ) {
            if (DEBUG>0) t.printStackTrace();
	    System.exit(1);
	    throw t;
        } catch ( Throwable t ) {
            if (DEBUG>0) t.printStackTrace();
            callback("onCreateError", t);
	    System.exit(1);
        }
    }

    /**
     * Callback hook for Connection open events.
     *
     * @param userSession the userSession which is opened.
     */
    @OnOpen
    public void onOpen(Session userSession) {
        if (DEAD) return;
		if (userSession==null) userSession = this.userSession;
        this.userSession = userSession;
        callback("onOpen", userSession, null); // maybeJRef(userSession)
    }

    public static Term maybeJRef(Object o) {
        if (o instanceof Term) return(Term)o;
        if (o==null) return JPL.JNULL;
        if (o==(Object)true) return JPL.JTRUE;
        if (o==(Object)false) return JPL.JFALSE;
        if (o==void.class) return JPL.JVOID;
        if (o instanceof String) return new Atom((String)o);
        return Term.objectToJRef(o);
    }
    /**
     * Callback hook for Connection close events.
     *
     * @param userSession the userSession which is getting closed.
     * @param reason the reason for connection close
     */
    @OnClose
    public void onClose(Session userSession, CloseReason reason) {
        if (userSession==null) userSession = this.userSession;
        callback("onClose", new Compound("closeReason", new Term[]{new Atom(""+userSession), 
                                             new Atom(""+reason)}));
        DEAD = true;
    }

    /**
     * Callback hook for Connection close events.
     *
     * @param userSession the userSession which is getting closed.
     * @param reason the reason for connection close
     */
    @OnError
    public void onError(Session userSession, Throwable reason) {
        if (DEAD) return;
        callback("onError", reason, null);
        //DEAD = true;
    }

    /**
     * Callback hook for Message Events. This method will be invoked when a client send a message.
     *
     * @param message The text message
     */
    @OnMessage
    public void onMessage(String message) {
        if (DEAD) return;
        callback("onMessage", message, new Atom(message, "string"));
    }

    public void callback(String type, Object message) {
        callback(type, message, null);
    }

    public void callback(String type, Object message, Term m) {
        if (DEAD) return;
        java.io.PrintStream ps = null;
        if (this.messageHandler != null) {
            try {
                if (m==null) m = new Atom(""+message);
                new Query(messageHandler, new Term[]{new Atom(type), m}).oneSolution();
            } catch ( Throwable t ) {
                t.printStackTrace();
            }
        } 
	if (DEBUG>1) {
            if (ps==null) ps = System.err;
            if (ps==null) ps = System.out;
            if (ps!=null) {
                ps.println("callback " + type + " " + message );
            }
        }

        if (DEBUG>1 && message instanceof Throwable) {
            if (ps==null) ps = System.err;
            if (ps==null) ps = System.out;
            if (ps!=null) {
                ((Throwable)message).printStackTrace(ps);

            } else {
                ((Throwable)message).printStackTrace();
            } 
        }
    }

    public void set_callback(String msgHandler) {
        this.messageHandler = msgHandler;
    } 

    /**
     * Send a message to the server.
     *
     * @param message
     */
    public void send_message(Object message) {
        if (DEAD) return;
        String m = ""+message;
        if (DEBUG>1) System.err.println("sending: " + m);
        try {
            this.userSession.getAsyncRemote().sendText(m);

	} catch ( NullPointerException t ) {
	    t.printStackTrace();
	    callback("sendTextError", t);
	    if (!DEAD) {
		throw t;
	    }
	} catch ( Throwable t ) {
	    if (DEBUG>0) t.printStackTrace();
	    callback("sendTextError", t);
	}
    }

    private static Object waitLock = new Object();

    private static void wait4TerminateSignal()
    {
        synchronized(waitLock)
        {try {
                waitLock.wait();
            } catch (InterruptedException e) {
            }}}

    public static void main(String[] args) {
        String ws = "wss://gateway.discord.gg";
        PrologWebSocket prologWebSocket = null;
        try {

            prologWebSocket = new PrologWebSocket(ws, null);
            wait4TerminateSignal();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (prologWebSocket!=null) {
                try {
                    prologWebSocket.close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } 
    } 
}


