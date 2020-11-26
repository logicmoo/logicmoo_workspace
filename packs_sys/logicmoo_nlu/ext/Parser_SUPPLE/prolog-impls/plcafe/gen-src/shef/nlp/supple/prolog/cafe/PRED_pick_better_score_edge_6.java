package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_pick_better_score_edge_6.java
 * @procedure pick_better_score_edge/6 in best_parse.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_pick_better_score_edge_6 extends Predicate {
    static Predicate pick_better_score_edge_6_1 = new PRED_pick_better_score_edge_6_1();
    static Predicate pick_better_score_edge_6_2 = new PRED_pick_better_score_edge_6_2();
    static Predicate pick_better_score_edge_6_sub_1 = new PRED_pick_better_score_edge_6_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6;

    public PRED_pick_better_score_edge_6(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        arg6 = a6; 
        this.cont = cont;
    }

    public PRED_pick_better_score_edge_6(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        arg5 = args[4]; 
        arg6 = args[5]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.aregs[5] = arg5;
        engine.aregs[6] = arg6;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(pick_better_score_edge_6_1, pick_better_score_edge_6_sub_1);
    }

    public int arity() { return 6; }

    public String toString() {
        return "pick_better_score_edge(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ")";
    }
}

class PRED_pick_better_score_edge_6_sub_1 extends PRED_pick_better_score_edge_6 {

    public Predicate exec(Prolog engine) {
        return engine.trust(pick_better_score_edge_6_2);
    }
}

class PRED_pick_better_score_edge_6_1 extends PRED_pick_better_score_edge_6 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        Predicate cont = engine.cont;

        if ( !a1.unify(a5, engine.trail) ) return engine.fail();
        if ( !a2.unify(a6, engine.trail) ) return engine.fail();
        a7 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a7, cont);
        p2 = new PRED_is_better_than_score_2(a1, a3, p1);
        return new PRED_$get_level_1(a7, p2);
    }
}

class PRED_pick_better_score_edge_6_2 extends PRED_pick_better_score_edge_6 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        Predicate cont = engine.cont;

        if ( !a3.unify(a5, engine.trail) ) return engine.fail();
        if ( !a4.unify(a6, engine.trail) ) return engine.fail();
        return cont;
    }
}

