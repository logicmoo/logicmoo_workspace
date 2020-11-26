package net.sf.regulus;

import java.io.StringReader;

import net.sf.regulus.interpretation.InterpretationParser;
import net.sf.regulus.interpretation.ParseException;


public class Interpretation {
    
    Object interpretation = null;
    
    public Interpretation(String regulusInterpretationString){
        init(regulusInterpretationString, false);
    }

    public Interpretation(String regulusInterpretationString, boolean tracingEnabled){
        init(regulusInterpretationString, tracingEnabled);
    }

    
    private void init(String regulusInterpretationString, boolean tracingEnabled){
        interpretation = null;

        StringReader sr = new StringReader(regulusInterpretationString); 
        InterpretationParser iParser = new InterpretationParser(sr);
        if(tracingEnabled){
            iParser.enable_tracing();
        }
        else{
            iParser.disable_tracing();
        }

        try {
            interpretation = iParser.createInterpretation();
        }
        catch (ParseException e) {
            System.out.println("Got Exception:");
            e.printStackTrace();
            interpretation = null;            
        }        
    }
    
    public Object getInterpretation(){
        return interpretation;
    }

}
