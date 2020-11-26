package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_to_lower_2.java
 * @procedure to_lower/2 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_to_lower_2 extends Predicate {
    static Predicate to_lower_2_1 = new PRED_to_lower_2_1();
    static Predicate to_lower_2_2 = new PRED_to_lower_2_2();
    static Predicate to_lower_2_sub_1 = new PRED_to_lower_2_sub_1();

    public Term arg1, arg2;

    public PRED_to_lower_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_to_lower_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(to_lower_2_1, to_lower_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "to_lower(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_to_lower_2_sub_1 extends PRED_to_lower_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(to_lower_2_2);
    }
}

class PRED_to_lower_2_1 extends PRED_to_lower_2 {
    static IntegerTerm s1 = new IntegerTerm(64);
    static IntegerTerm s2 = new IntegerTerm(91);
    static IntegerTerm s3 = new IntegerTerm(32);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        a3 = new VariableTerm(engine);
        p1 = new PRED_$plus_3(a1, s3, a2, cont);
        p2 = new PRED_$cut_1(a3, p1);
        p3 = new PRED_$less_than_2(a1, s2, p2);
        p4 = new PRED_$greater_than_2(a1, s1, p3);
        return new PRED_$get_level_1(a3, p4);
    }
}

class PRED_to_lower_2_2 extends PRED_to_lower_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !a1.unify(a2, engine.trail) ) return engine.fail();
        return cont;
    }
}

