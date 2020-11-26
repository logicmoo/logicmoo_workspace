package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_string_to_number_2.java
 * @procedure string_to_number/2 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_string_to_number_2 extends Predicate {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("0");

    public Term arg1, arg2;

    public PRED_string_to_number_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_string_to_number_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5, a6;
        Predicate p1, p2;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        a4 = new VariableTerm(engine);
        a3 = new ListTerm(a4, s1);
        a6 = new VariableTerm(engine);
        a5 = new ListTerm(a6, s1);
        p1 = new PRED_$minus_3(a4, a6, a2, cont);
        p2 = new PRED_name_2(s2, a5, p1);
        return new PRED_name_2(a1, a3, p2);
    }

    public int arity() { return 2; }

    public String toString() {
        return "string_to_number(" + arg1 + ", " + arg2 + ")";
    }
}

