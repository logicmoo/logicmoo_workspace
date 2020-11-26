package org.mitre.midiki.workshop;

import java.net.*;
import java.io.*;
import java.util.*;
import java.util.StringTokenizer;

/** Based on a simple HTTP server, generates XML
 *  based on a disease db query in an encoded URL.
 *  The form of the URL is identical to that of the
 *  command line test tool <code>WorkshopDatabaseTest</code>,
 *  except that arguments are separated by an asterisk (*),
 *  and whitespace is replace by %20 encoding.<br>
 *
 * Adapted from code that appears in Core Web Programming
 * from Prentice Hall Publishers, and may be freely used
 * or adapted. 1997 Marty Hall, hall@apl.jhu.edu.
 */

public class DiseaseServer extends NetworkServer {
  protected int maxInputLines = 25;
  protected String serverName = "DiseaseServer 1.0";
  static protected String defaultDTD = "diseasequeryresponse.dtd";
  static protected String defaultDatabaseName = "diseasedb.ser";
  static protected WorkshopDatabase wd = null;
    static protected LinkedList dtdData = new LinkedList();
    static protected int nextId = 0;
  protected StringBuffer buffer;
  protected StringBuffer cvtbuf;
    protected int id;
    protected UrlConverter uc;

    static protected boolean loadDatabase(String filename)
    {
        try {
            FileInputStream fos = new FileInputStream(filename);
            ObjectInputStream oos = new ObjectInputStream(fos);
            wd = (WorkshopDatabase)oos.readObject();
            fos.close();
            System.out.println("Read database from "+filename);
        } catch (Exception e) {
            System.out.println("*** Couldn't read disease database ***");
            e.printStackTrace();
            return false;
        }
        return true;
    }

    static protected boolean loadDTD(String filename)
    {
        try {
            FileReader fos = new FileReader(filename);
            BufferedReader oos = new BufferedReader(fos);
            for (String line=oos.readLine(); line!=null; line=oos.readLine()) {
                dtdData.add(line);
            }
            fos.close();
            System.out.println("Read dtd from "+filename);
        } catch (Exception e) {
            System.out.println("*** Couldn't read disease query response dtd ***");
            e.printStackTrace();
            return false;
        }
        return true;
    }

  //----------------------------------------------------
  /** Supply a port number as a command-line
   *  argument. Otherwise port 5555 will be used.
   */
  
  public static void main(String[] args) {
    int port = 5555;
    if (args.length < 1) {
        System.out.println("DiseaseServer <database> [<port>]");
        return;
    }
    if (!loadDTD(defaultDTD)) return;
    if (!loadDatabase(args[0])) return;
    if (args.length > 1)
      port = Integer.parseInt(args[1]);
    DiseaseServer echoServer = new DiseaseServer(port, 0);
    echoServer.listen();
  }

  public DiseaseServer(int port, int maxConnections) {
    super(port, maxConnections);
    if (wd==null) loadDatabase(defaultDatabaseName);
    buffer = new StringBuffer(2048);
    cvtbuf = new StringBuffer(256);
    id = nextId++;
    uc = new UrlConverter();
  }

  //----------------------------------------------------
  /** Overrides the NetworkServer handleConnection
   *  to read each line of data received, save it
   *  into an array of strings, then send it
   *  back embedded inside a PRE element in an
   *  HTML page.
   */
  
  public void handleConnection(Socket server)
      throws IOException{
    System.out.println
        (serverName + ": got connection from " +
         server.getInetAddress().getHostName());
      SocketUtil s = new SocketUtil(server);
    DataInputStream in = s.getDataStream();
    PrintStream out = s.getPrintStream();
    String[] inputLines = new String[maxInputLines];
    int i;
    for (i=0; i<maxInputLines; i++) {
      inputLines[i] = in.readLine();
      if (inputLines[i] == null)
        break;
      if (inputLines[i].length() == 0) {
        if (usingPost(inputLines)) {
          readPostData(inputLines, i, in);
          i = i + 2;
        }
        break;
      }
    }
    out.println(processRequest(inputLines));
    server.close();
  }

  //----------------------------------------------------
  // Normal Web page requests use GET, so this
  // server can simply read a line at a time.
  // However, CGI programs can use POST, in which
  // case we have to determine the number of POST bytes
  // that are sent so we know how much extra data
  // to read after the standard HTTP headers.
  
  private boolean usingPost(String[] inputs) {
    return(inputs[0].toUpperCase().startsWith("POST"));
  }

  private void readPostData(String[] inputs, int i,
                            DataInputStream in)
      throws IOException {
    int contentLength = contentLength(inputs);
    byte[] postData = new byte[contentLength];
    in.read(postData);
    inputs[++i] = new String(postData, 0);
  }

  //----------------------------------------------------
  // Given a line that starts with CONTENT-LENGTH,
  // this returns the integer value specified.
  
  private int contentLength(String[] inputs) {
    String input;
    for (int i=0; i<inputs.length; i++) {
      if (inputs[i].length() == 0)
        break;
      input = inputs[i].toUpperCase();
      if (input.startsWith("CONTENT-LENGTH"))
        return(getLength(input));
    }
    return(0);
  }

    private int getLength(String length) {
        StringTokenizer tok = new StringTokenizer(length);
        tok.nextToken();
        return(Integer.parseInt(tok.nextToken()));
    }

    /**
     * Accepts the input from an HTTP GET operation (stored as multiple
     * lines of input), executes a query against the database, and returns
     * a string containing the XML-encoded result. The XML document is built
     * according to the diseasequeryresult DTD, which this server will also
     * return upon request.<br>
     * Future versions of this server may accept POST operations, but I don't
     * have any tests for that ready to hand.
     *
     * @param rqst a <code>String[]</code> value
     * @return a <code>String</code> value
     */
    private String processRequest(String[] rqst)
    {
        String result = "<disease-query-result><error>Could not understand your request</error></disease-query-result>";
        List l;
        Iterator it;
        String[] cmd;
        Symptom symp;
        DiagnosticTest test;
        MedicalHistory hist;
        if (rqst==null)
            return result;
        buffer.setLength(0);
        buffer.append("<?xml version=\"1.0\" ?>\n<!DOCTYPE diseasequeryresponse SYSTEM \"diseasequeryresponse.dtd\">");

        for (int line=0; line<rqst.length; line++) {
            if (rqst[line]==null) continue;
            if (!rqst[line].startsWith("GET")) continue;
            rqst[line] = rqst[line].substring(5);
            int space = rqst[line].indexOf(" ");
            if (space > -1) {
                rqst[line] = rqst[line].substring(0,space);
            }
            if (rqst[line].equals("diseasequeryresponse.dtd")) {
                buffer.setLength(0);
                it = dtdData.iterator();
                while (it.hasNext()) {
                    String dtdline = (String)it.next();
                    buffer.append(dtdline);
                    buffer.append("\n");
                }
                return buffer.toString();
            }
            System.out.println("Processing query: "+rqst[line]);
            switch (rqst[line].charAt(0)) {
                case 'd':
                    buffer.append("<disease-query-result>\n");
                    l = wd.getAllDiseases();
                    it = l.iterator();
                    while (it.hasNext()) {
                        String diseaseName = (String)it.next();
                        buffer.append("<disease name=\"");
                        buffer.append(diseaseName);
                        buffer.append("\"/>\n");
                    }
                    buffer.append("</disease-query-result>\n");
                    result = buffer.toString();
                    break;
                case 's':
                    buffer.append("<disease-query-result>\n");
                    l = wd.getAllSymptoms();
                    it = l.iterator();
                    while (it.hasNext()) {
                        String symptomName = (String)it.next();
                        buffer.append("<symptom name=\"");
                        buffer.append(symptomName);
                        buffer.append("\"/>\n");
                    }
                    buffer.append("</disease-query-result>\n");
                    result = buffer.toString();
                    break;
                case 't':
                    buffer.append("<disease-query-result>\n");
                    l = wd.getAllTests();
                    it = l.iterator();
                    while (it.hasNext()) {
                        test = (DiagnosticTest)it.next();
                        buffer.append("<test name=\"");
                        buffer.append(test.name);
                        buffer.append("\" testname=\"");
                        buffer.append(test.test);
                        buffer.append("\"/>\n");
                    }
                    buffer.append("</disease-query-result>\n");
                    result = buffer.toString();
                    break;
                case 'h':
                    buffer.append("<disease-query-result>\n");
                    l = wd.getAllHistoryItems();
                    it = l.iterator();
                    while (it.hasNext()) {
                        String historyName = (String)it.next();
                        buffer.append("<history-item name=\"");
                        buffer.append(historyName);
                        buffer.append("\"/>\n");
                    }
                    buffer.append("</disease-query-result>\n");
                    result = buffer.toString();
                    break;
                case 'n':
                    buffer.append("<disease-query-result>\n");
                    l = wd.getAllSymptomSynonyms();
                    it = l.iterator();
                    while (it.hasNext()) {
                        Synonym syn = (Synonym)it.next();
                        buffer.append("<synonyms>\n");
                        Iterator it2 = syn.iterator();
                        while (it2.hasNext()) {
                            String symptomName = (String)it2.next();
                            buffer.append("<synonym name=\"");
                            buffer.append(symptomName);
                            buffer.append("\"/>\n");
                        }
                        buffer.append("</synonyms>\n");
                    }
                    buffer.append("</disease-query-result>\n");
                    result = buffer.toString();
                    break;
                case 'r':
                    cmd = parseCommand(rqst[line]);
                    buffer.append("<disease-query-result>\n");
                    if (cmd.length==1) {
                        buffer.append("<error>\n");
                        buffer.append("No disease name provided");
                        buffer.append("</error>\n");
                        break;
                    }
                    Disease descr = wd.getDiseaseDescription(cmd[1]);
                    if (descr==null) {
                        buffer.append("<error>\n");
                        buffer.append(cmd[1]).append(" not stored in database");
                        buffer.append("</error>\n");
                    } else {
                        buffer.append("<disease name=\"");
                        buffer.append(descr.name);
                        buffer.append("\">\n");

                        buffer.append("<description>\n");
                        buffer.append(descr.description);
                        buffer.append("</description>\n");

                        buffer.append("<regions>\n");
                        buffer.append(descr.regions);
                        buffer.append("</regions>\n");

                        buffer.append("<symptoms>\n");
                        Iterator sit = descr.symptoms.iterator();
                        while (sit.hasNext()) {
                            symp = (Symptom)sit.next();
                            buffer.append("<symptom name=\"");
                            buffer.append(symp.name);
                            buffer.append("\" required=\"");
                            buffer.append(symp.required ? "yes" : "no");
                            buffer.append("\" present=\"");
                            buffer.append(symp.present ? "yes" : "no");
                            buffer.append("\"/>\n");
                        }
                        buffer.append("</symptoms>\n");

                        buffer.append("<tests>\n");
                        sit = descr.tests.iterator();
                        while (sit.hasNext()) {
                            test = (DiagnosticTest)sit.next();
                            buffer.append("<test name=\"");
                            buffer.append(test.name);
                            buffer.append("\" testname=\"");
                            buffer.append(test.test);
                            buffer.append("\" result=\"");
                            buffer.append(test.result);
                            buffer.append("\" required=\"");
                            buffer.append(test.required ? "yes" : "no");
                            buffer.append("\" present=\"");
                            buffer.append(test.present ? "yes" : "no");
                            buffer.append("\"/>\n");
                        }
                        buffer.append("</tests>\n");

                        buffer.append("<history>\n");
                        sit = descr.history.iterator();
                        while (sit.hasNext()) {
                            hist = (MedicalHistory)sit.next();
                            buffer.append("<history-item name=\"");
                            buffer.append(hist.name);
                            buffer.append("\" required=\"");
                            buffer.append(hist.required ? "yes" : "no");
                            buffer.append("\" present=\"");
                            buffer.append(hist.present ? "yes" : "no");
                            buffer.append("\"/>\n");
                        }
                        buffer.append("</history>\n");

                        buffer.append("<treatment>\n");
                        buffer.append(descr.treatment);
                        buffer.append("</treatment>\n");

                        buffer.append("<prevention>\n");
                        buffer.append(descr.prevention);
                        buffer.append("</prevention>\n");

                        buffer.append("</disease>\n");
                    }
                    buffer.append("</disease-query-result>\n");
                    result = buffer.toString();
                    break;
                case 'g':
                    cmd = parseCommand(rqst[line]);
                    buffer.append("<disease-query-result>\n");
                    if (cmd.length == 1) {
                        buffer.append("<error>\n");
                        buffer.append("No symptoms provided");
                        buffer.append("</error>\n");
                        break;
                    }
                    LinkedList symps = new LinkedList();
                    LinkedList tests = new LinkedList();
                    LinkedList hists = new LinkedList();
                    for (int i=1; i<cmd.length; i++) {
                        boolean foundSymptom = false;
                        Symptom newSymptom = parseSymptom(fromUrl(cmd[i]),wd);
                        if (newSymptom != null) {
                            System.out.println(newSymptom);
                            symps.add(newSymptom);
                            foundSymptom = true;
                        }
                        if (!foundSymptom) {
                            DiagnosticTest newTest = parseTest(fromUrl(cmd[i]),wd);
                            if (newTest != null) {
                                System.out.println(newTest);
                                tests.add(newTest);
                                foundSymptom = true;
                            }
                        }
                        if (!foundSymptom) {
                            MedicalHistory newHistory = parseHistory(fromUrl(cmd[i]),wd);
                            if (newHistory != null) {
                                System.out.println(newHistory);
                                hists.add(newHistory);
                                foundSymptom = true;
                            }
                        }
                        if (!foundSymptom) {
                            buffer.append("<error>\n");
                            buffer.append(fromUrl(cmd[i]));
                            buffer.append(" not recognized; ignored for diagnosis");
                            buffer.append("</error>\n");
                        }
                    }
                    
                    QueryResponse qr = wd.getApplicableDiseases(symps,tests,hists);
                    it = qr.possibleDiagnoses.iterator();
                    while (it.hasNext()) {
                        String dname = (String)it.next();
                        buffer.append("<disease name=\"");
                        buffer.append(dname);
                        buffer.append("\"/>\n");
                    }
                    buffer.append("<discriminating-symptoms>\n");
                    it = qr.discriminatingSymptoms.iterator();
                    while (it.hasNext()) {
                        symp = (Symptom)it.next();
                        buffer.append("<symptom name=\"");
                        buffer.append(symp.name);
                        buffer.append("\" required=\"");
                        buffer.append(symp.required ? "yes" : "no");
                        buffer.append("\" present=\"");
                        buffer.append(symp.present ? "yes" : "no");
                        buffer.append("\"/>\n");
                    }
                    buffer.append("</discriminating-symptoms>\n");
                    buffer.append("<diagnostic-tests>\n");
                    it = qr.discriminatingTests.iterator();
                    while (it.hasNext()) {
                        test = (DiagnosticTest)it.next();
                        buffer.append("<test name=\"");
                        buffer.append(test.name);
                        buffer.append("\" testname=\"");
                        buffer.append(test.test);
                        buffer.append("\" result=\"");
                        buffer.append(test.result);
                        buffer.append("\" required=\"");
                        buffer.append(test.required ? "yes" : "no");
                        buffer.append("\" present=\"");
                        buffer.append(test.present ? "yes" : "no");
                        buffer.append("\"/>\n");
                    }
                    buffer.append("</diagnostic-tests>\n");
                    buffer.append("<relevant-history>\n");
                    it = qr.discriminatingHistory.iterator();
                    while (it.hasNext()) {
                        hist = (MedicalHistory)it.next();
                        buffer.append("<history-item name=\"");
                        buffer.append(hist.name);
                        buffer.append("\" required=\"");
                        buffer.append(hist.required ? "yes" : "no");
                        buffer.append("\" present=\"");
                        buffer.append(hist.present ? "yes" : "no");
                        buffer.append("\"/>\n");
                    }
                    buffer.append("</relevant-history>\n");
                    buffer.append("</disease-query-result>\n");
                    result = buffer.toString();
                    break;
                default:
                    System.out.println("Error: unrecognized option "+rqst[line]);
                    break;
            }
        }
        return result;
    }

    /**
     * Decodes %20 hexadecimal codes into spaces and returns the
     * converted string. Used for decoding query strings passed in URLs.
     *
     * @param cvt a <code>String</code> value
     * @return a <code>String</code> value
     */
    public String fromUrl(String cvt)
    {
        return uc.decode(cvt);
    }

    /**
     * Encodes spaces in a string into %20 hexadecimal encoding,
     * so that the string can be passed as part of a URL. 
     * Uses a UrlConverter to do the job.
     *
     * @param cvt a <code>String</code> value
     * @return a <code>String</code> value
     */
    public String toUrl(String cvt)
    {
        return uc.encode(cvt);
    }

    /**
     * Parses the argument to an HTTP GET into a series of arguments
     * separated by *.
     *
     * @param cmd a <code>String</code> value
     * @return a <code>String[]</code> value
     */
    public String[] parseCommand(String cmd)
    {
        StringTokenizer st = new StringTokenizer(cmd,"*");
        String[] result = new String[st.countTokens()];
        int i=0;
        while (st.hasMoreTokens()) {
            result[i++] = st.nextToken();
        }
        return result;
    }

    /**
     * Checks the container for a copy of this string, ignoring case.
     * If the item is found, the canonical version is returned.
     *
     * @param c a <code>Container</code> value
     * @param s a <code>String</code> value
     * @return a <code>String</code> value
     */
    static public String containsIgnoreCase(Collection c, String s)
    {
        Iterator it = c.iterator();
        while (it.hasNext()) {
            String sit = (String)it.next();
            if (sit.equalsIgnoreCase(s)) return sit;
        }
        return null;
    }

    /**
     * Parses a symptom specification of the form "symptom"/<flag> into
     * a Symptom object. The test component of the symptom is not specified.
     * If the <flag> component is not specified, it is assumed to be "yes".<br>
     * <br>parseSymptom("headache/yes") yields Symptom(headache,"",yes)
     * <br>parseSymptom("\"headache\"/no") yields Symptom(headache,"",no)
     * <br>parseSymptom("headache") yields Symptom(headache,"",yes)
     *
     * @param s a <code>String</code> value
     * @param wd a <code>WorkshopDatabase</code> in which to search for the symptom name
     * @return a <code>Symptom</code> value
     */
    static public Symptom parseSymptom(String s,
                                       WorkshopDatabase wd)
    {
        int slash = s.lastIndexOf("/");
        String presence = null;
        if (slash==-1) {
            presence = "yes";
        } else {
            presence = s.substring(slash+1);
            s = s.substring(0,slash);
        }
        if (s.startsWith("\"")) {
            s = s.substring(1,s.length()-1);
        }
        s = containsIgnoreCase(wd.symptomList,wd.resolveSynonyms(s));
        if (s==null) return null;
        return new Symptom(s,true,presence.equals("yes"));
    }

    /**
     * Parses a symptom specification of the form "symptom"/<flag> into
     * a DiagnosticTest object. The test and result components of the test
     * are not specified.
     * If the <flag> component is not specified, it is assumed to be "yes".<br>
     *@see parseSymptom
     *
     * @param s a <code>String</code> value
     * @param wd a <code>WorkshopDatabase</code> in which to search for the test name
     * @return a <code>DiagnosticTest</code> value
     */
    static public DiagnosticTest parseTest(String s,
                                           WorkshopDatabase wd)
    {
        int slash = s.lastIndexOf("/");
        String presence = null;
        if (slash==-1) {
            presence = "yes";
        } else {
            presence = s.substring(slash+1);
            s = s.substring(0,slash);
        }
        if (s.startsWith("\"")) {
            s = s.substring(1,s.length()-1);
        }
        s = containsIgnoreCase(wd.testList,wd.resolveSynonyms(s));
        if (s==null) return null;
        return new DiagnosticTest(s,"","",true,presence.equals("yes"));
    }

    /**
     * Parses a symptom specification of the form "symptom"/<flag> into
     * a MedicalHistory object. 
     * If the <flag> component is not specified, it is assumed to be "yes".<br>
     *@see parseSymptom
     *
     * @param s a <code>String</code> value
     * @param wd a <code>WorkshopDatabase</code> in which to search for the history item name
     * @return a <code>MedicalHistory</code> value
     */
    static public MedicalHistory parseHistory(String s,
                                              WorkshopDatabase wd)
    {
        int slash = s.lastIndexOf("/");
        String presence = null;
        if (slash==-1) {
            presence = "yes";
        } else {
            presence = s.substring(slash+1);
            s = s.substring(0,slash);
        }
        if (s.startsWith("\"")) {
            s = s.substring(1,s.length()-1);
        }
        s = containsIgnoreCase(wd.historyList,wd.resolveSynonyms(s));
        if (s==null) return null;
        return new MedicalHistory(s,true,presence.equals("yes"));
    }
}
