package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_parse_file_2.java
 * @procedure parse_file/2 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_parse_file_2 extends Predicate {

    public Term arg1, arg2;

    public PRED_parse_file_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_parse_file_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3;
        Predicate p1;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        a3 = new VariableTerm(engine);
        p1 = new PRED_parse_file_3(a1, a2, a3, cont);
        return new PRED_$dummy_parse_file46pl_16_1(a3, p1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "parse_file(" + arg1 + ", " + arg2 + ")";
    }
}

