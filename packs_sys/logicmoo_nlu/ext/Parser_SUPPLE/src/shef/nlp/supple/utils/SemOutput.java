package shef.nlp.supple.utils;

import java.util.ArrayList;

public class SemOutput {

  Long start;
  Long end;
  ArrayList semantics;

  public SemOutput() { super();}
  public SemOutput(Long s, Long e, ArrayList sem) {
    start=s;end=e;semantics=new ArrayList(sem);
  }
  public Long getStart() { return start;}
  public Long getEnd() { return end;}
  public ArrayList getSemantics() { return semantics;}
  public void setStart(Long s) { start=s;}
  public void setEnd(Long e) { end=e;}
  public void setSemantics(ArrayList s) { semantics=new ArrayList(s);}
}