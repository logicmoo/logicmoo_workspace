package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_parse_charts_2.java
 * @procedure parse_charts/2 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_parse_charts_2 extends Predicate {
    static Predicate parse_charts_2_1 = new PRED_parse_charts_2_1();
    static Predicate parse_charts_2_2 = new PRED_parse_charts_2_2();
    static Predicate parse_charts_2_sub_1 = new PRED_parse_charts_2_sub_1();

    public Term arg1, arg2;

    public PRED_parse_charts_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_parse_charts_2(){}
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
        return engine.jtry(parse_charts_2_1, parse_charts_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "parse_charts(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_parse_charts_2_sub_1 extends PRED_parse_charts_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(parse_charts_2_2);
    }
}

class PRED_parse_charts_2_1 extends PRED_parse_charts_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");
    static SymbolTerm f4 = SymbolTerm.makeSymbol("best_parse_cats", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        a3 = new VariableTerm(engine);
        Term[] h5 = {a3, new VariableTerm(engine)};
        a5 = new StructureTerm(f4, h5);
        Term[] h6 = {s3, a5};
        a4 = new StructureTerm(f2, h6);
        a6 = new VariableTerm(engine);
        p1 = new PRED_parse_charts2_2(a1, a6, cont);
        p2 = new PRED_findall_3(a3, a4, a6, p1);
        return new PRED_$neck_cut_0(p2);
    }
}

class PRED_parse_charts_2_2 extends PRED_parse_charts_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("best_parse_cats", 2);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("filter_grammar", 1);
    static SymbolTerm s5 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        Term[] h2 = {new VariableTerm(engine), new VariableTerm(engine)};
        a3 = new StructureTerm(f1, h2);
        Term[] h4 = {new VariableTerm(engine)};
        a4 = new StructureTerm(f3, h4);
        a5 = new ListTerm(a2, s5);
        a6 = new VariableTerm(engine);
        a7 = new ListTerm(a6, s5);
        p1 = new PRED_parse_charts2_2(a1, a7, cont);
        p2 = new PRED_compile_grammars_2(a5, a6, p1);
        p3 = new PRED_retractall_1(a4, p2);
        return new PRED_retractall_1(a3, p3);
    }
}

