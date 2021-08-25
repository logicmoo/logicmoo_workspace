/*This code is copyrighted by Teknowledge (c) 2001.  It is released under
the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.  Users of
this code also consent, by use of this code, to credit Teknowledge, in any writings, briefings,
publications, presentations, or other representations of any software which
incorporates, builds on, or uses this code.*/
package com.tks.celt;

import com.tks.daml.dispatcher.*;
/**
 * <p>Title: PrologHandler</p>
 * <p>Description:PrologHandler class takes <b>CELT</b> queries from users and executes on Prolog server </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: Teknowledge Corp</p>
 * @author bvasired
 * @version 1.0
 */

public class PrologHandler {

  /**
   * holds "LogicExpr"
   */
  public static final String LOGIC_EXP = "LogicExpr";

   /**
   * holds "ParseTree"
   */
  public static final String PARSE_TR = "ParseTree";

   /**
   * holds "Action"
   */
  public static final String ACTION = "Action";

   /**
   * holds "Warning"
   */
  public static final String WARNING = "Warning";

  /**
   * Constructor of PrologHandler
   */
  public PrologHandler() {
  }

  /**
   * main method of PrologHandler
   * @param args
   */
  public static void main(String[] args) {
    PrologHandler prologHandler = new PrologHandler();
    try{
        prologHandler.sendRequest(args[0],300,"user1");
    }catch(Exception e){
          System.out.println(e);
    }
  }

  /**
   * sends request to prologserver and gets response back
   */
   public String sendRequest(String query, int timeLimit, String userID) throws Exception{

      if(query.startsWith("\"")){
          query = query.substring(1);
      }

      if(query.endsWith("\"")){
          query = query.substring(0,query.length()-1);
      }

      String wholeQuery =  "call_with_time_limit((eng2log(\"" + query + "\", " + PrologHandler.LOGIC_EXP +" , " + PrologHandler.PARSE_TR + " , " + PrologHandler.ACTION + " , " + PrologHandler.WARNING + "))," + timeLimit + ")";
      System.out.println(wholeQuery);

      UserSession session=new UserSession(userID,"",true);
      String responseString = session.send(wholeQuery);
      System.out.println("response " + responseString);
      return responseString;
   }

}