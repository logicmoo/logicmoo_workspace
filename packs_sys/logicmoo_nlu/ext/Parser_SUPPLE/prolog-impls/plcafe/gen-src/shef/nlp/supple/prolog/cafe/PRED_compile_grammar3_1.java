package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_compile_grammar3_1.java
 * @procedure compile_grammar3/1 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_compile_grammar3_1 extends Predicate {
    static Predicate compile_grammar3_1_1 = new PRED_compile_grammar3_1_1();
    static Predicate compile_grammar3_1_2 = new PRED_compile_grammar3_1_2();
    static Predicate compile_grammar3_1_sub_1 = new PRED_compile_grammar3_1_sub_1();

    public Term arg1;

    public PRED_compile_grammar3_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_compile_grammar3_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(compile_grammar3_1_1, compile_grammar3_1_sub_1);
    }

    public int arity() { return 1; }

    public String toString() {
        return "compile_grammar3(" + arg1 + ")";
    }
}

class PRED_compile_grammar3_1_sub_1 extends PRED_compile_grammar3_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(compile_grammar3_1_2);
    }
}

class PRED_compile_grammar3_1_1 extends PRED_compile_grammar3_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("rule");
    static SymbolTerm f3 = SymbolTerm.makeSymbol("compiled_rule", 5);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8, p9;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        a2 = new VariableTerm(engine);
        a3 = new VariableTerm(engine);
        a4 = new ListTerm(a2, s1);
        a6 = new VariableTerm(engine);
        a5 = new ListTerm(a6, s1);
        a7 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        a8 = new ListTerm(a9, a10);
        a11 = new VariableTerm(engine);
        a16 = new VariableTerm(engine);
        a15 = new ListTerm(new VariableTerm(engine), a16);
        a14 = new ListTerm(new VariableTerm(engine), a15);
        a13 = new ListTerm(new VariableTerm(engine), a14);
        a12 = new ListTerm(new VariableTerm(engine), a13);
        a17 = new VariableTerm(engine);
        Term[] h4 = {a9, a10, a6, a17, a1};
        a18 = new StructureTerm(f3, h4);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_assert_1(a18, p1);
        p3 = new PRED_number_chars_2(a17, a16, p2);
        p4 = new PRED_atom_chars_2(a11, a12, p3);
        p5 = new PRED_buchart_gensym_2(s2, a11, p4);
        p6 = new PRED_$dummy_compile_grammar46pl_1_5(a1, a6, a9, a10, new VariableTerm(engine), p5);
        p7 = new PRED_reverse_2(a7, a8, p6);
        p8 = new PRED_compile_features_2(a3, a7, p7);
        p9 = new PRED_compile_features_2(a4, a5, p8);
        return new PRED_rule_2(a2, a3, p9);
    }
}

class PRED_compile_grammar3_1_2 extends PRED_compile_grammar3_1 {

    public Predicate exec(Prolog engine) {
        Term a1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        return new PRED_$neck_cut_0(cont);
    }
}

