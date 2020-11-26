package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_supple46pl_3_2.java
 * @procedure $dummy_supple.pl_3/2 in supple.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_supple46pl_3_2 extends Predicate {
    static Predicate $dummy_supple46pl_3_2_1 = new PRED_$dummy_supple46pl_3_2_1();
    static Predicate $dummy_supple46pl_3_2_2 = new PRED_$dummy_supple46pl_3_2_2();
    static Predicate $dummy_supple46pl_3_2_sub_1 = new PRED_$dummy_supple46pl_3_2_sub_1();

    public Term arg1, arg2;

    public PRED_$dummy_supple46pl_3_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_$dummy_supple46pl_3_2(){}
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
        return engine.jtry($dummy_supple46pl_3_2_1, $dummy_supple46pl_3_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "$dummy_supple.pl_3(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_$dummy_supple46pl_3_2_sub_1 extends PRED_$dummy_supple46pl_3_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_supple46pl_3_2_2);
    }
}

class PRED_$dummy_supple46pl_3_2_1 extends PRED_$dummy_supple46pl_3_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("top");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        a3 = new VariableTerm(engine);
        a4 = new ListTerm(s1, a2);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_$cut_1(a3, p1);
        p3 = new PRED_$614646_2(a1, a4, p2);
        return new PRED_$get_level_1(a3, p3);
    }
}

class PRED_$dummy_supple46pl_3_2_2 extends PRED_$dummy_supple46pl_3_2 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

