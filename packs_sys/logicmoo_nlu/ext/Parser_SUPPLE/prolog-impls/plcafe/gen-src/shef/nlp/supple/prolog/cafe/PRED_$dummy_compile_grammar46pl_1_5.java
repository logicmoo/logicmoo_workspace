package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_compile_grammar46pl_1_5.java
 * @procedure $dummy_compile_grammar.pl_1/5 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_compile_grammar46pl_1_5 extends Predicate {
    static Predicate $dummy_compile_grammar46pl_1_5_1 = new PRED_$dummy_compile_grammar46pl_1_5_1();
    static Predicate $dummy_compile_grammar46pl_1_5_2 = new PRED_$dummy_compile_grammar46pl_1_5_2();
    static Predicate $dummy_compile_grammar46pl_1_5_sub_1 = new PRED_$dummy_compile_grammar46pl_1_5_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5;

    public PRED_$dummy_compile_grammar46pl_1_5(Term a1, Term a2, Term a3, Term a4, Term a5, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        this.cont = cont;
    }

    public PRED_$dummy_compile_grammar46pl_1_5(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        arg5 = args[4]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.aregs[5] = arg5;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry($dummy_compile_grammar46pl_1_5_1, $dummy_compile_grammar46pl_1_5_sub_1);
    }

    public int arity() { return 5; }

    public String toString() {
        return "$dummy_compile_grammar.pl_1(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ")";
    }
}

class PRED_$dummy_compile_grammar46pl_1_5_sub_1 extends PRED_$dummy_compile_grammar46pl_1_5 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_compile_grammar46pl_1_5_2);
    }
}

class PRED_$dummy_compile_grammar46pl_1_5_1 extends PRED_$dummy_compile_grammar46pl_1_5 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        Predicate cont = engine.cont;

        a6 = new VariableTerm(engine);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_$cut_1(a6, p1);
        p3 = new PRED_compiled_rule_5(a3, a4, a2, a5, a1, p2);
        return new PRED_$get_level_1(a6, p3);
    }
}

class PRED_$dummy_compile_grammar46pl_1_5_2 extends PRED_$dummy_compile_grammar46pl_1_5 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

