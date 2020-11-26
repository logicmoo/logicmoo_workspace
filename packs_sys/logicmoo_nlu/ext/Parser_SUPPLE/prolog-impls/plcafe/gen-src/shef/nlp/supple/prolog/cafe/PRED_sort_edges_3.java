package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_sort_edges_3.java
 * @procedure sort_edges/3 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_sort_edges_3 extends Predicate {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static IntegerTerm s2 = new IntegerTerm(0);

    public Term arg1, arg2, arg3;

    public PRED_sort_edges_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_sort_edges_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3;
        a1 = arg1.dereference();
        a2 = arg2.dereference();
        a3 = arg3.dereference();

        return new PRED_sort_edges_5(a1, s1, a2, s2, a3, cont);
    }

    public int arity() { return 3; }

    public String toString() {
        return "sort_edges(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

