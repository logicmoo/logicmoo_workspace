package shef.nlp.supple.utils;

public class SynOutput {



  public String start;

  public String end;

  public String category;

  public String constituents;

  public int level;



  public SynOutput(String a, String b, String c, String d, int e) {

      start=a;

      end=b;

      category=c;

      constituents=d;

      level=e;



  }



  public String getStart() {return start; }

  public String getEnd() {return end; }

  public String getCategory() {return category; }

  public String getConstituens() {return constituents;}

  public int getLevel() {return level;}

}
