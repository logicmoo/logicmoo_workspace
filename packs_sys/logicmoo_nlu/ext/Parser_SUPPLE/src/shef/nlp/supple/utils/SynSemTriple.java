package shef.nlp.supple.utils;

/**
 *
 * <p>Title: SynSemTriple</p>
 * <p>Description: Triple of lists to contain syntactic, semantic, best parse output</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: U.of.Sheffield</p>
 * @author Horacio Saggion
 * @version 1.0
 */


//java stuff
import java.util.ArrayList;

public class SynSemTriple {

  public ArrayList syntax;
  public ArrayList semantics;
  public ArrayList bestparse;

  public SynSemTriple(ArrayList syn, ArrayList sem, ArrayList best) {
    syntax=syn;
    semantics=sem;
    bestparse=best;
  }
  public ArrayList getSyntax() { return syntax;}
  public ArrayList getSemnatics() { return semantics;}
  public ArrayList getBestParse() { return bestparse;}
}
