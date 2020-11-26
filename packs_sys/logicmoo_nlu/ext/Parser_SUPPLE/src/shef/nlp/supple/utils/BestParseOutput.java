package shef.nlp.supple.utils;

public class BestParseOutput {

 public String start;

 public String end;

 public String parse;

 public BestParseOutput(String s, String e, String p) {
   start=s;
   end=e;
   parse=p;
 }

 public String  getStart() { return start;}
 public String getEnd() { return end;}
 public String getBestParse() { return parse;}

}