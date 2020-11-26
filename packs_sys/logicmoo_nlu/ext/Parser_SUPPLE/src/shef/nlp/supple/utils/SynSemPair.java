package shef.nlp.supple.utils;

/**
 *
 * <p>Title: SynSemPair</p>
 * <p>Description: Pair of lists to contain syntactic and semantic output</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: U.of.Sheffield</p>
 * @author Horacio Saggion
 * @version 1.0
 */


//java stuff
import java.util.ArrayList;

public class SynSemPair {

  public ArrayList syntax;
  public ArrayList semantics;

  public SynSemPair(ArrayList syn, ArrayList sem) {
    syntax=syn;
    semantics=sem;
  }
  public ArrayList getSyntax() { return syntax;}
  public ArrayList getSemnatics() { return semantics;}

}