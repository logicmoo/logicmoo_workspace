package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_upper_2.java
 * @procedure upper/2 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_upper_2 extends Predicate {

    public Term arg1, arg2;

    public PRED_upper_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_upper_2(){}
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

        return new PRED_$dummy_supple_utils46pl_0_4(a1, a2, new VariableTerm(engine), new VariableTerm(engine), cont);
    }

    public int arity() { return 2; }

    public String toString() {
        return "upper(" + arg1 + ", " + arg2 + ")";
    }
}

