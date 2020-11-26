package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_get_best_parse_edges_2.java
 * @procedure get_best_parse_edges/2 in best_parse.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_get_best_parse_edges_2 extends Predicate {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Term arg1, arg2;

    public PRED_get_best_parse_edges_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_get_best_parse_edges_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        return new PRED_get_best_parse_edges_3(a1, s1, a2, cont);
    }

    public int arity() { return 2; }

    public String toString() {
        return "get_best_parse_edges(" + arg1 + ", " + arg2 + ")";
    }
}

