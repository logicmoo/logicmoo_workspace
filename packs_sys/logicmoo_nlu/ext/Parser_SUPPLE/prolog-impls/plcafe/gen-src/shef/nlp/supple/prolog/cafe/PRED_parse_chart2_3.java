package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_parse_chart2_3.java
 * @procedure parse_chart2/3 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_parse_chart2_3 extends Predicate {
    static Predicate parse_chart2_3_1 = new PRED_parse_chart2_3_1();
    static Predicate parse_chart2_3_2 = new PRED_parse_chart2_3_2();
    static Predicate parse_chart2_3_3 = new PRED_parse_chart2_3_3();
    static Predicate parse_chart2_3_sub_1 = new PRED_parse_chart2_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_parse_chart2_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_parse_chart2_3(){}
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
        return engine.jtry(parse_chart2_3_1, parse_chart2_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "parse_chart2(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_parse_chart2_3_sub_1 extends PRED_parse_chart2_3 {
    static Predicate parse_chart2_3_sub_2 = new PRED_parse_chart2_3_sub_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(parse_chart2_3_2, parse_chart2_3_sub_2);
    }
}

class PRED_parse_chart2_3_sub_2 extends PRED_parse_chart2_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(parse_chart2_3_3);
    }
}

class PRED_parse_chart2_3_1 extends PRED_parse_chart2_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("edge", 10);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16;
        Predicate p1, p2, p3, p4, p5, p6;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a2.isList() ){
            a4 = ((ListTerm)a2).car();
            if ( !s1.unify(((ListTerm)a2).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a2.isVariable() ){
            a4 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a4, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        Term[] h3 = {new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a5 = new StructureTerm(f2, h3);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        a11 = new VariableTerm(engine);
        a12 = new VariableTerm(engine);
        a13 = new VariableTerm(engine);
        a14 = new VariableTerm(engine);
        a15 = new VariableTerm(engine);
        a16 = new VariableTerm(engine);
        p1 = new PRED_$dummy_parse_file46pl_12_4(a1, a3, a16, new VariableTerm(engine), cont);
        p2 = new PRED_$dummy_parse_file46pl_11_17(a1, a4, a3, a8, a9, a10, a11, a12, a13, a14, a15, a16, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), p1);
        p3 = new PRED_display_tdm_attributes_3(a3, a16, a1, p2);
        p4 = new PRED_$dummy_parse_file46pl_10_11(a4, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, p3);
        p5 = new PRED_parse_edges_2(a6, a4, p4);
        p6 = new PRED_sort_edges_3(a1, a6, a7, p5);
        return new PRED_retractall_1(a5, p6);
    }
}

class PRED_parse_chart2_3_2 extends PRED_parse_chart2_3 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
        Predicate p1, p2, p3, p4, p5, p6;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a2.isList() ){
            a4 = ((ListTerm)a2).car();
            a5 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a4, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        Term[] h2 = {new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a6 = new StructureTerm(f1, h2);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        p1 = new PRED_parse_chart2_3(a10, a5, a3, cont);
        p2 = new PRED_next_edges_3(a9, a1, a10, p1);
        p3 = new PRED_best_parse_3(a4, a9, a8, p2);
        p4 = new PRED_parse_edges_2(a7, a4, p3);
        p5 = new PRED_sort_edges_3(a1, a7, a8, p4);
        p6 = new PRED_retractall_1(a6, p5);
        return new PRED_filter_grammar_1(a4, p6);
    }
}

class PRED_parse_chart2_3_3 extends PRED_parse_chart2_3 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("[]");
    static IntegerTerm s4 = new IntegerTerm(0);
    static IntegerTerm s5 = new IntegerTerm(1);
    static SymbolTerm f7 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s8 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");
    static IntegerTerm s9 = new IntegerTerm(2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19;
        Predicate p1, p2, p3, p4, p5;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a2.isList() ){
            a4 = ((ListTerm)a2).car();
            a5 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a4, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        Term[] h2 = {new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a6 = new StructureTerm(f1, h2);
        a7 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        a11 = new VariableTerm(engine);
        a12 = new VariableTerm(engine);
        a13 = new VariableTerm(engine);
        a14 = new VariableTerm(engine);
        a15 = new VariableTerm(engine);
        Term[] h6 = {a9, a10, a11, s3, s4, a12, s5, a13, a14, a15};
        a8 = new StructureTerm(f1, h6);
        Term[] h10 = {a9, a10, a11, s3, new VariableTerm(engine), a12, s9, a13, a14, a15};
        a17 = new StructureTerm(f1, h10);
        Term[] h11 = {s8, a17};
        a16 = new StructureTerm(f7, h11);
        a18 = new VariableTerm(engine);
        a19 = new VariableTerm(engine);
        p1 = new PRED_parse_chart2_3(a19, a5, a3, cont);
        p2 = new PRED_append_3(a1, a18, a19, p1);
        p3 = new PRED_findall_3(a8, a16, a18, p2);
        p4 = new PRED_parse_edges_2(a7, a4, p3);
        p5 = new PRED_sort_edges_3(a1, a7, new VariableTerm(engine), p4);
        return new PRED_retractall_1(a6, p5);
    }
}

