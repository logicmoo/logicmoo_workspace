package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_best_parse_vertex_3.java
 * @procedure best_parse_vertex/3 in best_parse.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_best_parse_vertex_3 extends Predicate {
    static IntegerTerm s1 = new IntegerTerm(1);
    static SymbolTerm f2 = SymbolTerm.makeSymbol("vertex_best_parse", 3);

    public Term arg1, arg2, arg3;

    public PRED_best_parse_vertex_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_best_parse_vertex_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2;
        a1 = arg1.dereference();
        a2 = arg2.dereference();
        a3 = arg3.dereference();

        a4 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        Term[] h3 = {a1, a5, a6};
        a7 = new StructureTerm(f2, h3);
        p1 = new PRED_assert_1(a7, cont);
        p2 = new PRED_best_parse_vertex_6(a4, a1, a2, a5, a6, a3, p1);
        return new PRED_$minus_3(a1, s1, a4, p2);
    }

    public int arity() { return 3; }

    public String toString() {
        return "best_parse_vertex(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

