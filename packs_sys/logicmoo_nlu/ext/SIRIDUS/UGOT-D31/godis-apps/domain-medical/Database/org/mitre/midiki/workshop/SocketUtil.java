package org.mitre.midiki.workshop;

import java.net.*;
import java.io.*;

/** A shorthand way to create PrintStreams and
 *  buffered/unbuffered DataInputStreams associated
 *  with a socket.
 *
 * This appears in Core Web Programming from
 * Prentice Hall Publishers, and may be freely used
 * or adapted. 1997 Marty Hall, hall@apl.jhu.edu.
 *
 * @author <a href="mailto:cburke@mitre.org">Carl Burke</a>
 * @version 1.0
 * @since 1.0
 */
public class SocketUtil {
  private Socket s;

  public SocketUtil(Socket s) {
    this.s = s;
  }
  
  public DataInputStream getDataStream()
      throws IOException {
    return(new DataInputStream(
                 new BufferedInputStream(
                       s.getInputStream())));
  }

  public DataInputStream getUnbufferedDataStream()
      throws IOException {
    return(new DataInputStream(s.getInputStream()));
  }

  public PrintStream getPrintStream()
      throws IOException {
    return(new PrintStream(s.getOutputStream()));
  }
}
