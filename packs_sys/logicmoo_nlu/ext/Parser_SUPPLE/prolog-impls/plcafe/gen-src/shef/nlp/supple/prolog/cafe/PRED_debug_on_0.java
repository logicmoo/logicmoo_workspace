package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_debug_on_0.java
 * @procedure debug_on/0 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_debug_on_0 extends Predicate {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("debug_on");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");

    public PRED_debug_on_0(Predicate cont) {
        this.cont = cont;
    }

    public PRED_debug_on_0(){}
    public void setArgument(Term[] args, Predicate cont) {
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2;
        Predicate p1;

        a1 = new VariableTerm(engine);
        Term[] h4 = {s3, a1};
        a2 = new StructureTerm(f2, h4);
        p1 = new PRED_translated_goal_1(a2, cont);
        return new PRED_clause_2(s1, a1, p1);
    }

    public int arity() { return 0; }

    public String toString() {
        return "debug_on";
    }
}

