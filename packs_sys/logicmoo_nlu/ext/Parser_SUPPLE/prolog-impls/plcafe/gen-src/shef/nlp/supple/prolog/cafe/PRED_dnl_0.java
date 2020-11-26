package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_dnl_0.java
 * @procedure dnl/0 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_dnl_0 extends Predicate {
    static Predicate dnl_0_1 = new PRED_dnl_0_1();
    static Predicate dnl_0_2 = new PRED_dnl_0_2();
    static Predicate dnl_0_sub_1 = new PRED_dnl_0_sub_1();

    public PRED_dnl_0(Predicate cont) {
        this.cont = cont;
    }

    public PRED_dnl_0(){}
    public void setArgument(Term[] args, Predicate cont) {
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(dnl_0_1, dnl_0_sub_1);
    }

    public int arity() { return 0; }

    public String toString() {
        return "dnl";
    }
}

class PRED_dnl_0_sub_1 extends PRED_dnl_0 {

    public Predicate exec(Prolog engine) {
        return engine.trust(dnl_0_2);
    }
}

class PRED_dnl_0_1 extends PRED_dnl_0 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1, p2, p3, p4, p5, p6, p7;
        Predicate cont = engine.cont;

        a1 = new VariableTerm(engine);
        a2 = new VariableTerm(engine);
        a3 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a1, cont);
        p2 = new PRED_tell_1(a2, p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_tell_1(a3, p3);
        p5 = new PRED_verbose_output_1(a3, p4);
        p6 = new PRED_telling_1(a2, p5);
        p7 = new PRED_debug_on_0(p6);
        return new PRED_$get_level_1(a1, p7);
    }
}

class PRED_dnl_0_2 extends PRED_dnl_0 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

