package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_best_parse_highest_edge_5.java
 * @procedure best_parse_highest_edge/5 in best_parse.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_best_parse_highest_edge_5 extends Predicate {
    static Predicate best_parse_highest_edge_5_1 = new PRED_best_parse_highest_edge_5_1();
    static Predicate best_parse_highest_edge_5_2 = new PRED_best_parse_highest_edge_5_2();
    static Predicate best_parse_highest_edge_5_3 = new PRED_best_parse_highest_edge_5_3();
    static Predicate best_parse_highest_edge_5_sub_1 = new PRED_best_parse_highest_edge_5_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5;

    public PRED_best_parse_highest_edge_5(Term a1, Term a2, Term a3, Term a4, Term a5, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        this.cont = cont;
    }

    public PRED_best_parse_highest_edge_5(){}
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
        return engine.jtry(best_parse_highest_edge_5_1, best_parse_highest_edge_5_sub_1);
    }

    public int arity() { return 5; }

    public String toString() {
        return "best_parse_highest_edge(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ")";
    }
}

class PRED_best_parse_highest_edge_5_sub_1 extends PRED_best_parse_highest_edge_5 {
    static Predicate best_parse_highest_edge_5_sub_2 = new PRED_best_parse_highest_edge_5_sub_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(best_parse_highest_edge_5_2, best_parse_highest_edge_5_sub_2);
    }
}

class PRED_best_parse_highest_edge_5_sub_2 extends PRED_best_parse_highest_edge_5 {

    public Predicate exec(Prolog engine) {
        return engine.trust(best_parse_highest_edge_5_3);
    }
}

class PRED_best_parse_highest_edge_5_1 extends PRED_best_parse_highest_edge_5 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("grammar1");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f3 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s4 = SymbolTerm.makeSymbol("source");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("list");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
        Predicate p1, p2, p3, p4, p5, p6, p7;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a5, engine.trail) ) return engine.fail();
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        a8 = new ListTerm(a9, a10);
        a12 = new VariableTerm(engine);
        Term[] h5 = {s4, a12};
        a11 = new StructureTerm(f3, h5);
        p1 = new PRED_$cut_1(a6, cont);
        p2 = new PRED_$unify_2(a12, s6, p1);
        p3 = new PRED_nonvar_1(a12, p2);
        p4 = new PRED_memberchk_2(a11, a10, p3);
        p5 = new PRED_memberchk_2(a9, a3, p4);
        p6 = new PRED_$614646_2(a7, a8, p5);
        p7 = new PRED_edge_10(a1, a2, a7, s2, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), a4, p6);
        return new PRED_$get_level_1(a6, p7);
    }
}

class PRED_best_parse_highest_edge_5_2 extends PRED_best_parse_highest_edge_5 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        Predicate cont = engine.cont;

        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a8 = new ListTerm(a9, new VariableTerm(engine));
        p1 = new PRED_$cut_1(a6, cont);
        p2 = new PRED_memberchk_2(a9, a3, p1);
        p3 = new PRED_$614646_2(a7, a8, p2);
        p4 = new PRED_edge_10(a1, a2, a7, s1, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), a4, p3);
        return new PRED_$get_level_1(a6, p4);
    }
}

class PRED_best_parse_highest_edge_5_3 extends PRED_best_parse_highest_edge_5 {
    static IntegerTerm s1 = new IntegerTerm(1);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s3 = SymbolTerm.makeSymbol("list_np");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        Predicate cont = engine.cont;

        a6 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a7 = new ListTerm(a8, new VariableTerm(engine));
        p1 = new PRED_$9261_2(a8, s3, cont);
        p2 = new PRED_$614646_2(a6, a7, p1);
        p3 = new PRED_edge_10(a1, a2, a6, s2, new VariableTerm(engine), new VariableTerm(engine), s1, new VariableTerm(engine), new VariableTerm(engine), a4, p2);
        p4 = new PRED_$plus_3(a1, s1, a2, p3);
        return new PRED_filter_grammar_1(a5, p4);
    }
}

