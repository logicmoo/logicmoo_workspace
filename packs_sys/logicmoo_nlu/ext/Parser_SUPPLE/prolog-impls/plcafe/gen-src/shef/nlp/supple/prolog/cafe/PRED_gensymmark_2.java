package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_gensymmark_2.java
 * @procedure gensymmark/2 in dynamic.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_gensymmark_2 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("gensymmark", 2);
    static SymbolTerm f3 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s4 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");

    public Term arg1, arg2;

    public PRED_gensymmark_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_gensymmark_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5;
        Predicate p1;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        Term[] h2 = {a1, a2};
        a3 = new StructureTerm(f1, h2);
        a4 = new VariableTerm(engine);
        Term[] h5 = {s4, a4};
        a5 = new StructureTerm(f3, h5);
        p1 = new PRED_translated_goal_1(a5, cont);
        return new PRED_clause_2(a3, a4, p1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "gensymmark(" + arg1 + ", " + arg2 + ")";
    }
}

