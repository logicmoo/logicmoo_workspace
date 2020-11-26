package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_vwrite_1.java
 * @procedure vwrite/1 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_vwrite_1 extends Predicate {
    static Predicate vwrite_1_1 = new PRED_vwrite_1_1();
    static Predicate vwrite_1_2 = new PRED_vwrite_1_2();
    static Predicate vwrite_1_sub_1 = new PRED_vwrite_1_sub_1();

    public Term arg1;

    public PRED_vwrite_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_vwrite_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(vwrite_1_1, vwrite_1_sub_1);
    }

    public int arity() { return 1; }

    public String toString() {
        return "vwrite(" + arg1 + ")";
    }
}

class PRED_vwrite_1_sub_1 extends PRED_vwrite_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(vwrite_1_2);
    }
}

class PRED_vwrite_1_1 extends PRED_vwrite_1 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3, p4, p5, p6, p7;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        a2 = new VariableTerm(engine);
        a3 = new VariableTerm(engine);
        a4 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a2, cont);
        p2 = new PRED_tell_1(a3, p1);
        p3 = new PRED_write_1(a1, p2);
        p4 = new PRED_tell_1(a4, p3);
        p5 = new PRED_verbose_output_1(a4, p4);
        p6 = new PRED_telling_1(a3, p5);
        p7 = new PRED_verbose_0(p6);
        return new PRED_$get_level_1(a2, p7);
    }
}

class PRED_vwrite_1_2 extends PRED_vwrite_1 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

