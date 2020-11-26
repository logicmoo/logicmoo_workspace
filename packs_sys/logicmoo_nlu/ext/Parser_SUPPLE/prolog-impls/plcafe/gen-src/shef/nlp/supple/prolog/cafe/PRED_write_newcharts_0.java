package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_write_newcharts_0.java
 * @procedure write_newcharts/0 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_write_newcharts_0 extends Predicate {
    static Predicate write_newcharts_0_1 = new PRED_write_newcharts_0_1();
    static Predicate write_newcharts_0_2 = new PRED_write_newcharts_0_2();
    static Predicate write_newcharts_0_sub_1 = new PRED_write_newcharts_0_sub_1();

    public PRED_write_newcharts_0(Predicate cont) {
        this.cont = cont;
    }

    public PRED_write_newcharts_0(){}
    public void setArgument(Term[] args, Predicate cont) {
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(write_newcharts_0_1, write_newcharts_0_sub_1);
    }

    public int arity() { return 0; }

    public String toString() {
        return "write_newcharts";
    }
}

class PRED_write_newcharts_0_sub_1 extends PRED_write_newcharts_0 {

    public Predicate exec(Prolog engine) {
        return engine.trust(write_newcharts_0_2);
    }
}

class PRED_write_newcharts_0_1 extends PRED_write_newcharts_0 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("edges");
    static SymbolTerm s4 = SymbolTerm.makeSymbol("chart(");
    static SymbolTerm s5 = SymbolTerm.makeSymbol(", ");
    static SymbolTerm s6 = SymbolTerm.makeSymbol("edges : [");
    static SymbolTerm s7 = SymbolTerm.makeSymbol("], ");
    static SymbolTerm s8 = SymbolTerm.makeSymbol(").");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14;
        Predicate cont = engine.cont;

        a1 = new VariableTerm(engine);
        a3 = new VariableTerm(engine);
        Term[] h3 = {s2, a3};
        a2 = new StructureTerm(f1, h3);
        a4 = new VariableTerm(engine);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_nl_0(p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_write_1(s8, p3);
        p5 = new PRED_nl_0(p4);
        p6 = new PRED_writeq_1(a4, p5);
        p7 = new PRED_write_1(s7, p6);
        p8 = new PRED_write_edges_1(a3, p7);
        p9 = new PRED_nl_0(p8);
        p10 = new PRED_write_1(s6, p9);
        p11 = new PRED_write_1(s5, p10);
        p12 = new PRED_writeq_1(a1, p11);
        p13 = new PRED_nl_0(p12);
        p14 = new PRED_write_1(s4, p13);
        return new PRED_chart_3(a1, a2, a4, p14);
    }
}

class PRED_write_newcharts_0_2 extends PRED_write_newcharts_0 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

