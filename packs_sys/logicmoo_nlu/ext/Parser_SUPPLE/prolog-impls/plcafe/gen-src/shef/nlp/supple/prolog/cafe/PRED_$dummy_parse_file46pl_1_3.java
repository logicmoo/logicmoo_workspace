package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_parse_file46pl_1_3.java
 * @procedure $dummy_parse_file.pl_1/3 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_parse_file46pl_1_3 extends Predicate {
    static Predicate $dummy_parse_file46pl_1_3_1 = new PRED_$dummy_parse_file46pl_1_3_1();
    static Predicate $dummy_parse_file46pl_1_3_2 = new PRED_$dummy_parse_file46pl_1_3_2();
    static Predicate $dummy_parse_file46pl_1_3_sub_1 = new PRED_$dummy_parse_file46pl_1_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_$dummy_parse_file46pl_1_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_$dummy_parse_file46pl_1_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_parse_file46pl_1_3_1, $dummy_parse_file46pl_1_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "$dummy_parse_file.pl_1(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_$dummy_parse_file46pl_1_3_sub_1 extends PRED_$dummy_parse_file46pl_1_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_parse_file46pl_1_3_2);
    }
}

class PRED_$dummy_parse_file46pl_1_3_1 extends PRED_$dummy_parse_file46pl_1_3 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        return new PRED_$dummy_parse_file46pl_2_2(a1, a2, cont);
    }
}

class PRED_$dummy_parse_file46pl_1_3_2 extends PRED_$dummy_parse_file46pl_1_3 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("best_parse_file", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        Term[] h2 = {a3};
        a4 = new StructureTerm(f1, h2);
        p1 = new PRED_assert_1(a4, cont);
        return new PRED_telling_1(a3, p1);
    }
}

