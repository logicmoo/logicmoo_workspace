package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_clean_predicate_2.java
 * @procedure clean_predicate/2 in semantics.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_clean_predicate_2 extends Predicate {

    public Term arg1, arg2;

    public PRED_clean_predicate_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_clean_predicate_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5;
        Predicate p1, p2, p3;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        a3 = new VariableTerm(engine);
        a4 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        p1 = new PRED_atom_chars_2(a2, a5, cont);
        p2 = new PRED_replace_whitespace_2(a4, a5, p1);
        p3 = new PRED_atom_chars_2(a3, a4, p2);
        return new PRED_lower_2(a1, a3, p3);
    }

    public int arity() { return 2; }

    public String toString() {
        return "clean_predicate(" + arg1 + ", " + arg2 + ")";
    }
}

