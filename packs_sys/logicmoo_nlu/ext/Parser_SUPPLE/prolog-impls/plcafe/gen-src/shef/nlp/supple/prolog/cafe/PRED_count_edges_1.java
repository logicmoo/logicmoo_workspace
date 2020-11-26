package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_count_edges_1.java
 * @procedure count_edges/1 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_count_edges_1 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm f3 = SymbolTerm.makeSymbol(",", 2);
    static SymbolTerm f4 = SymbolTerm.makeSymbol(";", 2);
    static SymbolTerm f5 = SymbolTerm.makeSymbol("retract", 1);
    static SymbolTerm f6 = SymbolTerm.makeSymbol("count_edges_number", 1);
    static SymbolTerm f9 = SymbolTerm.makeSymbol("is", 2);
    static IntegerTerm s10 = new IntegerTerm(0);
    static SymbolTerm f13 = SymbolTerm.makeSymbol("+", 2);
    static IntegerTerm s14 = new IntegerTerm(1);
    static SymbolTerm f17 = SymbolTerm.makeSymbol("assert", 1);

    public Term arg1;

    public PRED_count_edges_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_count_edges_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
        Predicate p1;
        a1 = arg1.dereference();

        Term[] h2 = {new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a2 = new StructureTerm(f1, h2);
        a7 = new VariableTerm(engine);
        Term[] h7 = {a7};
        a6 = new StructureTerm(f6, h7);
        Term[] h8 = {a6};
        a5 = new StructureTerm(f5, h8);
        Term[] h11 = {a7, s10};
        a8 = new StructureTerm(f9, h11);
        Term[] h12 = {a5, a8};
        a4 = new StructureTerm(f4, h12);
        a11 = new VariableTerm(engine);
        Term[] h15 = {a7, s14};
        a12 = new StructureTerm(f13, h15);
        Term[] h16 = {a11, a12};
        a10 = new StructureTerm(f9, h16);
        Term[] h18 = {a11};
        a14 = new StructureTerm(f6, h18);
        Term[] h19 = {a14};
        a13 = new StructureTerm(f17, h19);
        Term[] h20 = {a10, a13};
        a9 = new StructureTerm(f3, h20);
        Term[] h21 = {a4, a9};
        a3 = new StructureTerm(f3, h21);
        Term[] h22 = {a1};
        a15 = new StructureTerm(f6, h22);
        p1 = new PRED_retract_1(a15, cont);
        return new PRED_foreach_2(a2, a3, p1);
    }

    public int arity() { return 1; }

    public String toString() {
        return "count_edges(" + arg1 + ")";
    }
}

