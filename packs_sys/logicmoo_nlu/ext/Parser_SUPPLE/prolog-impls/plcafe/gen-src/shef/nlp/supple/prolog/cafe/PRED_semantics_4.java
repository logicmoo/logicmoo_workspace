package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_semantics_4.java
 * @procedure semantics/4 in semantics.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_semantics_4 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("sem");

    public Term arg1, arg2, arg3, arg4;

    public PRED_semantics_4(Term a1, Term a2, Term a3, Term a4, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        this.cont = cont;
    }

    public PRED_semantics_4(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9;
        Predicate p1, p2, p3;
        a1 = arg1.dereference();
        a2 = arg2.dereference();
        a3 = arg3.dereference();
        a4 = arg4.dereference();

        a5 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a6 = new ListTerm(a1, a7);
        a9 = new VariableTerm(engine);
        Term[] h3 = {s2, a9};
        a8 = new StructureTerm(f1, h3);
        p1 = new PRED_clean_semantics_2(a9, a4, cont);
        p2 = new PRED_member_2(a8, a7, p1);
        p3 = new PRED_$614646_2(a5, a6, p2);
        return new PRED_$dummy_semantics46pl_0_16(a2, a3, new VariableTerm(engine), new VariableTerm(engine), a5, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), p3);
    }

    public int arity() { return 4; }

    public String toString() {
        return "semantics(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ")";
    }
}

