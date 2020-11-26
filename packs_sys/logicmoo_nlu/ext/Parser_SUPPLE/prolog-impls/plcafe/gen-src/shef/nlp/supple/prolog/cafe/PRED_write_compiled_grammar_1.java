package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_write_compiled_grammar_1.java
 * @procedure write_compiled_grammar/1 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_write_compiled_grammar_1 extends Predicate {

    public Term arg1;

    public PRED_write_compiled_grammar_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_write_compiled_grammar_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8;
        a1 = arg1.dereference();

        a2 = new VariableTerm(engine);
        p1 = new PRED_tell_1(a2, cont);
        p2 = new PRED_told_0(p1);
        p3 = new PRED_write_compiled_lexicon_0(p2);
        p4 = new PRED_write_compiled_grammar_0(p3);
        p5 = new PRED_write_best_parse_cats_0(p4);
        p6 = new PRED_tell_1(a1, p5);
        p7 = new PRED_told_0(p6);
        p8 = new PRED_tell_1(a1, p7);
        return new PRED_telling_1(a2, p8);
    }

    public int arity() { return 1; }

    public String toString() {
        return "write_compiled_grammar(" + arg1 + ")";
    }
}

