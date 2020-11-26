package org.mitre.midiki.workshop;

import java.net.*;
import java.io.*;

/** A starting point for network servers. You'll need to
 *  override handleConnection, but in many cases
 *  listen can remain unchanged. It uses
 *  SocketUtil to simplify the creation of the
 *  PrintStream and DataInputStream.
 *
 * This appears in Core Web Programming from
 * Prentice Hall Publishers, and may be freely used
 * or adapted. 1997 Marty Hall, hall@apl.jhu.edu.
 *
 * @see SocketUtil
 */

public class NetworkServer {
  protected int port, maxConnections;

  //----------------------------------------------------
  /** Build a server on specified port. It will continue
   *  to accept connections (passing each to
   *  handleConnection) until an explicit exit
   *  command is sent (e.g. System.exit) or the
   *  maximum number of connections is reached. Specify
   *  0 for maxConnections if you want the server
   *  to run indefinitely.
   */
  
  public NetworkServer(int port, int maxConnections) {
    this.port = port;
    this.maxConnections = maxConnections;
  }

  //----------------------------------------------------
  /** Monitor a port for connections. Each time one
   *  is established, pass resulting Socket to
   *  handleConnection.
   */
  
  public void listen() {
    int i=0;
    try {
      ServerSocket listener = new ServerSocket(port);
      Socket server;
      while((i++ < maxConnections) ||
            (maxConnections == 0)) {
        server = listener.accept();
        handleConnection(server);
      }
    } catch (IOException ioe) {
      System.out.println("IOException: " + ioe);
      ioe.printStackTrace();
    }
  }

  //----------------------------------------------------
  /** This is the method that provides the behavior
   *  to the server, since it determines what is
   *  done with the resulting socket. <B>Override this
   *  method in servers you write.</B>
   *  <P>
   *  This generic version simply reports the host
   *  that made the connection, shows the first line
   *  the client sent, and sends a single line
   *  in response.
   */

  protected void handleConnection(Socket server)
      throws IOException{
    SocketUtil s = new SocketUtil(server);
    DataInputStream in = s.getDataStream();
    PrintStream out = s.getPrintStream();
    System.out.println
      ("Generic Network Server:\n" +
       "got connection from " +
       server.getInetAddress().getHostName() + "\n" +
       "with first line '" +
       in.readLine() + "'");
    out.println("Generic Network Server");
    server.close();
  }
}
