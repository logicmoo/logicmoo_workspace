package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_combine_edge_8.java
 * @procedure combine_edge/8 in supple.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_combine_edge_8 extends Predicate {
    static Predicate combine_edge_8_1 = new PRED_combine_edge_8_1();
    static Predicate combine_edge_8_2 = new PRED_combine_edge_8_2();
    static Predicate combine_edge_8_sub_1 = new PRED_combine_edge_8_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8;

    public PRED_combine_edge_8(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Term a7, Term a8, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        arg6 = a6; 
        arg7 = a7; 
        arg8 = a8; 
        this.cont = cont;
    }

    public PRED_combine_edge_8(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        arg5 = args[4]; 
        arg6 = args[5]; 
        arg7 = args[6]; 
        arg8 = args[7]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.aregs[5] = arg5;
        engine.aregs[6] = arg6;
        engine.aregs[7] = arg7;
        engine.aregs[8] = arg8;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(combine_edge_8_1, combine_edge_8_sub_1);
    }

    public int arity() { return 8; }

    public String toString() {
        return "combine_edge(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ", " + arg7 + ", " + arg8 + ")";
    }
}

class PRED_combine_edge_8_sub_1 extends PRED_combine_edge_8 {

    public Predicate exec(Prolog engine) {
        return engine.trust(combine_edge_8_2);
    }
}

class PRED_combine_edge_8_1 extends PRED_combine_edge_8 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        a7 = engine.aregs[7].dereference();
        a8 = engine.aregs[8].dereference();
        Predicate cont = engine.cont;

        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        a11 = new VariableTerm(engine);
        a12 = new VariableTerm(engine);
        a13 = new VariableTerm(engine);
        a14 = new VariableTerm(engine);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_$dummy_supple46pl_1_15(a1, a2, a3, a4, a6, a7, a8, a10, a11, a12, a13, a14, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), p1);
        p3 = new PRED_find_daughter_edges_6(a1, a12, a5, a13, a9, a14, p2);
        return new PRED_compiled_rule_5(a3, a9, a10, a11, a8, p3);
    }
}

class PRED_combine_edge_8_2 extends PRED_combine_edge_8 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

