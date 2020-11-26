package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_compile_grammar2_1.java
 * @procedure compile_grammar2/1 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_compile_grammar2_1 extends Predicate {

    public Term arg1;

    public PRED_compile_grammar2_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_compile_grammar2_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1;
        Predicate p1;
        a1 = arg1.dereference();

        p1 = new PRED_compile_lexicon_1(a1, cont);
        return new PRED_compile_grammar3_1(a1, p1);
    }

    public int arity() { return 1; }

    public String toString() {
        return "compile_grammar2(" + arg1 + ")";
    }
}

