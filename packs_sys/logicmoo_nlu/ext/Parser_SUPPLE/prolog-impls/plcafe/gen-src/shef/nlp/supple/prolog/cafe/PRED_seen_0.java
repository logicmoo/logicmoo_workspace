package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_seen_0.java
 * @procedure seen/0 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_seen_0 extends Predicate {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("user_input");

    public PRED_seen_0(Predicate cont) {
        this.cont = cont;
    }

    public PRED_seen_0(){}
    public void setArgument(Term[] args, Predicate cont) {
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1;
        Predicate p1, p2;

        a1 = new VariableTerm(engine);
        p1 = new PRED_set_input_1(s1, cont);
        p2 = new PRED_close_1(a1, p1);
        return new PRED_current_input_1(a1, p2);
    }

    public int arity() { return 0; }

    public String toString() {
        return "seen";
    }
}

