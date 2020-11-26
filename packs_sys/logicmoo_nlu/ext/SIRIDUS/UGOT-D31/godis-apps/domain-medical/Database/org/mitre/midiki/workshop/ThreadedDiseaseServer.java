package org.mitre.midiki.workshop;

import java.net.*;
import java.io.*;

/** This is just a Thread with a field to store a
 *  Socket object. Used as a thread-safe means to pass
 *  the Socket from handleConnection to run.
 */

class Connection extends Thread {
  protected Socket serverSocket;

  public Connection(Runnable serverObject,
                    Socket serverSocket) {
    super(serverObject);
    this.serverSocket = serverSocket;
  }
}

/**
 * A multi-threaded variation of DiseaseServer.<br>
 * Adapted from code that appears in Core Web Programming
 * from Prentice Hall Publishers, and may be freely used
 * or adapted. 1997 Marty Hall, hall@apl.jhu.edu.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 * @see DiseaseServer
 * @see Runnable
 */
public class ThreadedDiseaseServer extends DiseaseServer
                                implements Runnable {
  public static void main(String[] args) {
    int port = 5555;
    if (args.length < 1) {
        System.out.println("ThreadedDiseaseServer <database> [<port>]");
        return;
    }
    if (!loadDTD(defaultDTD)) return;
    if (!loadDatabase(args[0])) return;
    if (args.length > 1)
      port = Integer.parseInt(args[1]);
    ThreadedDiseaseServer diseaseServer =
      new ThreadedDiseaseServer(port, 0);
    diseaseServer.serverName =
        "MITRE Dialogue Workshop Disease Database Server 1.0";
    diseaseServer.listen();
  }

  public ThreadedDiseaseServer(int port, int connections) {
    super(port, connections);
  }

  //----------------------------------------------------
  /** The new version of handleConnection starts
   *  a thread. This new thread will call back to the
   *  <I>old</I> version of handleConnection, keeping
   *  the same server behavior in a multi-threaded
   *  version. The thread stores the Socket instance
   *  since run doesn't take any arguments, and since
   *  storing the socket in an instance variable risks
   *  having it overwritten if the next thread starts
   *  before the run method gets a chance to
   *  copy the socket reference.
   */
                                  
  public void handleConnection(Socket server) {
    Connection connectionThread
      = new Connection(this, server);
    connectionThread.start();
  }
    
  public void run() {
    Connection currentThread
      = (Connection)Thread.currentThread();
    try {
      super.handleConnection(currentThread.serverSocket);
    } catch(IOException ioe) {
      System.out.println("IOException: " + ioe);
      ioe.printStackTrace();
    }
  }
}
