package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_parse_file46pl_12_4.java
 * @procedure $dummy_parse_file.pl_12/4 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_parse_file46pl_12_4 extends Predicate {
    static Predicate $dummy_parse_file46pl_12_4_1 = new PRED_$dummy_parse_file46pl_12_4_1();
    static Predicate $dummy_parse_file46pl_12_4_2 = new PRED_$dummy_parse_file46pl_12_4_2();
    static Predicate $dummy_parse_file46pl_12_4_sub_1 = new PRED_$dummy_parse_file46pl_12_4_sub_1();

    public Term arg1, arg2, arg3, arg4;

    public PRED_$dummy_parse_file46pl_12_4(Term a1, Term a2, Term a3, Term a4, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        this.cont = cont;
    }

    public PRED_$dummy_parse_file46pl_12_4(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_parse_file46pl_12_4_1, $dummy_parse_file46pl_12_4_sub_1);
    }

    public int arity() { return 4; }

    public String toString() {
        return "$dummy_parse_file.pl_12(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ")";
    }
}

class PRED_$dummy_parse_file46pl_12_4_sub_1 extends PRED_$dummy_parse_file46pl_12_4 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_parse_file46pl_12_4_2);
    }
}

class PRED_$dummy_parse_file46pl_12_4_1 extends PRED_$dummy_parse_file46pl_12_4 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        Predicate cont = engine.cont;

        p1 = new PRED_display_best_parse_2(a2, a3, cont);
        p2 = new PRED_assert_edge_list_1(a1, p1);
        return new PRED_best_parse_file_1(a4, p2);
    }
}

class PRED_$dummy_parse_file46pl_12_4_2 extends PRED_$dummy_parse_file46pl_12_4 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

