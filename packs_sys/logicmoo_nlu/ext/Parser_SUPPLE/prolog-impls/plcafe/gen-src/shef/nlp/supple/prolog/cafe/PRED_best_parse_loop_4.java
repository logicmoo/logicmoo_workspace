package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_best_parse_loop_4.java
 * @procedure best_parse_loop/4 in best_parse.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_best_parse_loop_4 extends Predicate {
    static Predicate best_parse_loop_4_1 = new PRED_best_parse_loop_4_1();
    static Predicate best_parse_loop_4_2 = new PRED_best_parse_loop_4_2();
    static Predicate best_parse_loop_4_sub_1 = new PRED_best_parse_loop_4_sub_1();

    public Term arg1, arg2, arg3, arg4;

    public PRED_best_parse_loop_4(Term a1, Term a2, Term a3, Term a4, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        this.cont = cont;
    }

    public PRED_best_parse_loop_4(){}
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
        return engine.jtry(best_parse_loop_4_1, best_parse_loop_4_sub_1);
    }

    public int arity() { return 4; }

    public String toString() {
        return "best_parse_loop(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ")";
    }
}

class PRED_best_parse_loop_4_sub_1 extends PRED_best_parse_loop_4 {

    public Predicate exec(Prolog engine) {
        return engine.trust(best_parse_loop_4_2);
    }
}

class PRED_best_parse_loop_4_1 extends PRED_best_parse_loop_4 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        Predicate cont = engine.cont;

        a5 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_$greater_than_2(a1, a2, p1);
        return new PRED_$get_level_1(a5, p2);
    }
}

class PRED_best_parse_loop_4_2 extends PRED_best_parse_loop_4 {
    static IntegerTerm s1 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        Predicate cont = engine.cont;

        a5 = new VariableTerm(engine);
        p1 = new PRED_best_parse_loop_4(a5, a2, a3, a4, cont);
        p2 = new PRED_$plus_3(a1, s1, a5, p1);
        return new PRED_best_parse_vertex_3(a1, a3, a4, p2);
    }
}

