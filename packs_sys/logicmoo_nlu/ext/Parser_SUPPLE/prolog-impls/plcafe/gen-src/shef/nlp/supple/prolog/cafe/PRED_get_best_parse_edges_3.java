package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_get_best_parse_edges_3.java
 * @procedure get_best_parse_edges/3 in best_parse.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_get_best_parse_edges_3 extends Predicate {
    static Predicate get_best_parse_edges_3_1 = new PRED_get_best_parse_edges_3_1();
    static Predicate get_best_parse_edges_3_2 = new PRED_get_best_parse_edges_3_2();
    static Predicate get_best_parse_edges_3_3 = new PRED_get_best_parse_edges_3_3();
    static Predicate get_best_parse_edges_3_sub_1 = new PRED_get_best_parse_edges_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_get_best_parse_edges_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_get_best_parse_edges_3(){}
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
        return engine.jtry(get_best_parse_edges_3_1, get_best_parse_edges_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "get_best_parse_edges(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_get_best_parse_edges_3_sub_1 extends PRED_get_best_parse_edges_3 {
    static Predicate get_best_parse_edges_3_sub_2 = new PRED_get_best_parse_edges_3_sub_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(get_best_parse_edges_3_2, get_best_parse_edges_3_sub_2);
    }
}

class PRED_get_best_parse_edges_3_sub_2 extends PRED_get_best_parse_edges_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(get_best_parse_edges_3_3);
    }
}

class PRED_get_best_parse_edges_3_1 extends PRED_get_best_parse_edges_3 {
    static IntegerTerm s1 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !a2.unify(a3, engine.trail) ) return engine.fail();
        a4 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a4, cont);
        p2 = new PRED_$less_than_2(a1, s1, p1);
        return new PRED_$get_level_1(a4, p2);
    }
}

class PRED_get_best_parse_edges_3_2 extends PRED_get_best_parse_edges_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("nil");
    static IntegerTerm s2 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        a4 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        p1 = new PRED_get_best_parse_edges_3(a5, a2, a3, cont);
        p2 = new PRED_$minus_3(a1, s2, a5, p1);
        p3 = new PRED_$cut_1(a4, p2);
        p4 = new PRED_vertex_best_parse_3(a1, new VariableTerm(engine), s1, p3);
        return new PRED_$get_level_1(a4, p4);
    }
}

class PRED_get_best_parse_edges_3_3 extends PRED_get_best_parse_edges_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("edge", 10);
    static IntegerTerm s3 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        a4 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        Term[] h4 = {a5, a1, a6, s1, s1, a7, s3, a8, a9, a4};
        a11 = new StructureTerm(f2, h4);
        a12 = new ListTerm(a10, a2);
        p1 = new PRED_get_best_parse_edges_3(a5, a12, a3, cont);
        p2 = new PRED_$unify_2(a10, a11, p1);
        p3 = new PRED_edge_10(a5, a1, a6, s1, new VariableTerm(engine), a7, new VariableTerm(engine), a8, a9, a4, p2);
        return new PRED_vertex_best_parse_3(a1, new VariableTerm(engine), a4, p3);
    }
}

