package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_buchart_gensym_2.java
 * @procedure buchart_gensym/2 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_buchart_gensym_2 extends Predicate {
    static Predicate buchart_gensym_2_1 = new PRED_buchart_gensym_2_1();
    static Predicate buchart_gensym_2_2 = new PRED_buchart_gensym_2_2();
    static Predicate buchart_gensym_2_sub_1 = new PRED_buchart_gensym_2_sub_1();

    public Term arg1, arg2;

    public PRED_buchart_gensym_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_buchart_gensym_2(){}
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
        return engine.jtry(buchart_gensym_2_1, buchart_gensym_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "buchart_gensym(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_buchart_gensym_2_sub_1 extends PRED_buchart_gensym_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(buchart_gensym_2_2);
    }
}

class PRED_buchart_gensym_2_1 extends PRED_buchart_gensym_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("gensymmark", 2);
    static IntegerTerm s3 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        a3 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        Term[] h2 = {a1, a5};
        a4 = new StructureTerm(f1, h2);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        Term[] h4 = {a1, a6};
        a10 = new StructureTerm(f1, h4);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_assert_1(a10, p1);
        p3 = new PRED_name_2(a2, a9, p2);
        p4 = new PRED_append_3(a8, a7, a9, p3);
        p5 = new PRED_name_2(a1, a8, p4);
        p6 = new PRED_name_2(a6, a7, p5);
        p7 = new PRED_$plus_3(a5, s3, a6, p6);
        p8 = new PRED_retract_1(a4, p7);
        return new PRED_$get_level_1(a3, p8);
    }
}

class PRED_buchart_gensym_2_2 extends PRED_buchart_gensym_2 {
    static IntegerTerm s1 = new IntegerTerm(1);
    static SymbolTerm f2 = SymbolTerm.makeSymbol("gensymmark", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        a3 = new VariableTerm(engine);
        a4 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        Term[] h3 = {a1, s1};
        a6 = new StructureTerm(f2, h3);
        p1 = new PRED_assert_1(a6, cont);
        p2 = new PRED_name_2(a2, a5, p1);
        p3 = new PRED_append_3(a4, a3, a5, p2);
        p4 = new PRED_name_2(a1, a4, p3);
        return new PRED_name_2(s1, a3, p4);
    }
}

