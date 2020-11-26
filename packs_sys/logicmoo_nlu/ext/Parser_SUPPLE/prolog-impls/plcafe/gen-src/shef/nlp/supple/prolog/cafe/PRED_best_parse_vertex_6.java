package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_best_parse_vertex_6.java
 * @procedure best_parse_vertex/6 in best_parse.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_best_parse_vertex_6 extends Predicate {
    static Predicate best_parse_vertex_6_1 = new PRED_best_parse_vertex_6_1();
    static Predicate best_parse_vertex_6_2 = new PRED_best_parse_vertex_6_2();
    static Predicate best_parse_vertex_6_3 = new PRED_best_parse_vertex_6_3();
    static Predicate best_parse_vertex_6_sub_1 = new PRED_best_parse_vertex_6_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6;

    public PRED_best_parse_vertex_6(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        arg6 = a6; 
        this.cont = cont;
    }

    public PRED_best_parse_vertex_6(){}
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
        return engine.jtry(best_parse_vertex_6_1, best_parse_vertex_6_sub_1);
    }

    public int arity() { return 6; }

    public String toString() {
        return "best_parse_vertex(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ")";
    }
}

class PRED_best_parse_vertex_6_sub_1 extends PRED_best_parse_vertex_6 {
    static Predicate best_parse_vertex_6_sub_2 = new PRED_best_parse_vertex_6_sub_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(best_parse_vertex_6_2, best_parse_vertex_6_sub_2);
    }
}

class PRED_best_parse_vertex_6_sub_2 extends PRED_best_parse_vertex_6 {

    public Predicate exec(Prolog engine) {
        return engine.trust(best_parse_vertex_6_3);
    }
}

class PRED_best_parse_vertex_6_1 extends PRED_best_parse_vertex_6 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("-", 2);
    static IntegerTerm s2 = new IntegerTerm(0);
    static Term[] h5 = {s2, s2};
    static StructureTerm s3 = new StructureTerm(f1, h5);
    static SymbolTerm s4 = SymbolTerm.makeSymbol("nil");

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

        if ( !s3.unify(a4, engine.trail) ) return engine.fail();
        if ( !s4.unify(a5, engine.trail) ) return engine.fail();
        a7 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a7, cont);
        p2 = new PRED_$less_than_2(a1, s2, p1);
        return new PRED_$get_level_1(a7, p2);
    }
}

class PRED_best_parse_vertex_6_2 extends PRED_best_parse_vertex_6 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("-", 2);
    static IntegerTerm s3 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        Predicate cont = engine.cont;

        a7 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        Term[] h2 = {a9, a10};
        a8 = new StructureTerm(f1, h2);
        a11 = new VariableTerm(engine);
        a12 = new VariableTerm(engine);
        a13 = new VariableTerm(engine);
        a14 = new VariableTerm(engine);
        Term[] h4 = {a12, a13};
        a15 = new StructureTerm(f1, h4);
        a16 = new VariableTerm(engine);
        a17 = new VariableTerm(engine);
        a18 = new VariableTerm(engine);
        p1 = new PRED_pick_better_score_edge_6(a14, a7, a17, a18, a4, a5, cont);
        p2 = new PRED_best_parse_vertex_6(a16, a2, a3, a17, a18, a6, p1);
        p3 = new PRED_$minus_3(a1, s3, a16, p2);
        p4 = new PRED_$unify_2(a14, a15, p3);
        p5 = new PRED_$plus_3(a10, s3, a13, p4);
        p6 = new PRED_$plus_3(a9, a11, a12, p5);
        p7 = new PRED_$minus_3(a2, a1, a11, p6);
        p8 = new PRED_vertex_best_parse_3(a1, a8, new VariableTerm(engine), p7);
        return new PRED_best_parse_highest_edge_5(a1, a2, a3, a7, a6, p8);
    }
}

class PRED_best_parse_vertex_6_3 extends PRED_best_parse_vertex_6 {
    static IntegerTerm s1 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        Predicate cont = engine.cont;

        a7 = new VariableTerm(engine);
        p1 = new PRED_best_parse_vertex_6(a7, a2, a3, a4, a5, a6, cont);
        return new PRED_$minus_3(a1, s1, a7, p1);
    }
}

